set mem 2000m
cd  D:\Home\Downloads\TITG2020\zr2006_2018_EF_Stata11
use "zr2006_2018_EF_Stata11.dta"

//Only include the industries of interest
//drop if ind2 <= 60
//drop if ind2 >= 64

xtset id year, y


//Calculate Hall markup - note this includes services which are a seperate line item 
// in AJPES data, but is an expense that should be included in the calculation with 
// payroll and material cost
gen SalesRevenue = aop110
gen InventoryVariation = aop121+aop122
gen MaterialCost = aop130
gen Payroll = aop139
gen Service = aop134
gen MarkupHall = ( SalesRevenue + InventoryVariation ) / ( Payroll + MaterialCost + Service )


//Calculate finiacial metrics

//Combine positive and negative revenue
gen OperatingRevenue = aop151 - aop152
gen OperatingMargin = OperatingRevenue / aop110
gen AssetTurnover = aop110 / aop001

//Calculate Cash Flow since AJPES data doesn't include cash flow statements
gen NTotalProft = aop182
gen OTotalLoss = aop183
gen ProfitsMinusLoss = aop182 - aop183
gen OperatingExpenses = aop127
gen WriteOffs = aop144
gen FianacialRevenue = aop153
gen LongTermReceivables = aop027 + aop029
gen ShortTermReceivables = aop048 +aop050
gen ChangesInInvestment =  aop040 + aop042 
gen GainLossSalesAssets = aop010 + aop018 + aop020
gen DeferedTaxAssets = aop031 + aop033
gen OperatingReceivables = aop163
gen ShortTermDeferredRevenue = aop053 + aop055
gen DeferredCosts = aop003
gen ChangesInInventories = aop121 - aop122
gen GainLossSale = aop010
gen CashFlow = ProfitsMinusLoss + OperatingExpenses + FianacialRevenue + LongTermReceivables + ShortTermReceivables + ChangesInInvestment + GainLossSalesAssets + DeferedTaxAssets + OperatingReceivables + ShortTermDeferredRevenue + DeferredCosts + ChangesInInventories + GainLossSale - WriteOffs
gen CashFlowRatio = CashFlow / (aop085 + aop086)
gen CriticalCash = (CashFlow + aop155) / (aop169 + aop085 + aop086)
//Still have companies with 0 expenses - this confirms that they seem to be still paying dividends
//Ended up dropping these companies from the dataset later
gen RetainedEarnings = aop056 + aop058
gen Dividends = L.RetainedEarnings - RetainedEarnings + ProfitsMinusLoss

//Get rid of the non-functioning companies
drop if Payroll==.
drop if aop188 == 0
drop if aop110 == 0

 
//Find exporter status, this is known to impact the markup. Will control for it in regression
gen exporter = 0
// 115 = EU revenue, 118 outside EU revenue 
replace exporter = 1 if aop115 > 0 | aop118 > 0

//Using log(sales) as a proxy for size. Not using assets because some of these firms don't have a 
// lot of fixed assets (not needed in IT). Should correlate directly with other, unobserved metrics,
// such as number of employees and market value.
gen log_sales = log(aop110)
gen log_payroll = log(Payroll)
gen log_capital = log(aop001)
gen log_materials = log(aop128)

//Figure out if companies are exiting or entering the market
gen export_entry = 0
replace export_entry = 1 if L.exporter == 0 & exporter == 1
gen export_exit = 0
replace export_exit = 1 if L.exporter == 1 & exporter == 0

gen entry = 0
replace entry = 1 if L.log_sales == . & log_sales != .
gen exit = 0 
replace exit = 1 if log_sales != . & F.log_sales == .

//aop001 == Capital (All assets), aop128 == COGS
//We are only going to second order here - in DeLocker they go to the third
// can come back and fix this if we find the estimates are way off
gen log_materials2 = log_materials^2
gen log_payroll2 = log_payroll^2
gen log_capital2 = log_capital^2
gen log_labourMaterials = log_payroll * log_materials
gen log_labourCapital = log_capital * log_payroll
gen log_capitalMaterials = log_capital * log_materials
gen log_labourCapitalMaterials = log_payroll * log_capital * log_materials

//Generating Shortnames for the interaction terms 
// Todo: fix this to proper names in the final version, but for now the debug output is hard to 
//       read if they are full names

gen k = log_capital
gen l = log_payroll
gen m = log_materials //and services

//Second order
gen m2 = log_materials2 
gen l2 = log_payroll2
gen k2 = log_capital2
gen kl = k * l
gen km = k * m 
gen ml = l * m 

//third order

gen k3 = k^3
gen l3 = l^3
gen m3 = m^3

gen k2l = k * k * l 
gen k2m = k * k * m 
gen kl2 = k * l * l
gen km2 = k * m * m
gen m2l = m * m * l 
gen ml2 = m * l * l
gen klm = k * l * m 

log using output.log
foreach x in 25 41 43 45 46 47 49 56 62 68 69 70 71 72 73 74 75{
	preserve
	drop if ind2 != `x'

	prodest log_sales, free(log_payroll) state(log_capital) proxy(log_materials) fsresidual(epsilon) met(lp) reps(50) id(id) t(year)

		gen labour_elasticity = _b[log_payroll]
		gen capital_elasticity = _b[log_capital]
		gen material_elasticity = _b[log_materials]


		//bmols == material demand elasticity, aop128 == materials cost 
		gen alpha = aop128 / exp(log_sales - epsilon)
		gen theta = material_elasticity
		gen MarkupDamijan = theta / alpha 

		//Trim outliers - these are companies that we missed in the clean up earlier
		drop if MarkupDamijan > 100 
		drop if MarkupHall > 100

		//Generate graphs for the presentation 
		hist MarkupDam , xtitle("Markup") title("Markup Distribution - Damijan")
		graph export `x'DamijanMarkup.png
		hist MarkupHall , xtitle("Markup") title("Markup Distribution - Hall")
	graph export `x'HallMarkup.png

	//Run regression for Hall method of estimating markup 
	xtreg MarkupHall AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupHall , fe
	eststo Hall 

	//Find the values, and show that fixed effects is the right choice
	// These are going to be biased because this is a dynamic panel, but still interesting to run 
	xtreg MarkupDamijan AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupDamijan, fe
	estimates store fixed
	xtreg MarkupDamijan AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupDamijan, re
	estimates store random
	hausman fixed random 

	//This is dynamic panel data - so use a dynamic panel data estimator. 
	//ivstyle is strictly exogenous variables (firm size and year) 
	//gmmstyle is for endogenous or partially endogenous variables
	//Used AR(1) process - the output will show this was a valid choice
	// They used estimates in levels, not just first differences (think this is Blundel Bond - need to look that up )
	xtabond2 MarkupDamijan AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupDamijan, ivstyle(log_sales i.year) gmmstyle( AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter L.MarkupDamijan)
	//Suggestion from presentation feedback - move log_sales to ivstyle 
	//xtabond2 MarkupDamijan AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupDamijan, ivstyle(i.year) gmmstyle( log_sales AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter L.MarkupDamijan)
	eststo dynamicDamFull

	xtabond2 MarkupHall AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupHall, ivstyle( i.year) gmmstyle(log_sales AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter L.MarkupHall)
	eststo dynamicHall
	//Get that output 
	esttab dynamicHall dynamicDamFull

	//Trimmed graphs so that it is easier to see the distribution without outliers 
	histogram CashFlowRatio if CashFlowRatio <100 & CashFlowRatio > -100, ytitle(Density) xtitle(Cash Flow Ratio) title(Cash Flow Ratio Distribution)
	graph export `x'CashFlowRatio.png
	histogram CriticalCash if CriticalCash <100 & CriticalCash > -100, ytitle(Density) xtitle(Critical Cash Ratio) title(Critical Cash Ratio Distribution)
	graph export `x'CriticalCash.png
	histogram OperatingMargin if OperatingMargin <1 & OperatingMargin > -1, ytitle(Density) xtitle(Operating Margin) title(Operating Margin Distribution)
	graph export `x'OperatingMargin.png
	histogram AssetTurnover if AssetTurnover <100 & AssetTurnover > -100, ytitle(Density) xtitle(AssetTurnover) title(Asset Turnover Distribution) 
	graph export `x'AssetTurnover.png

	//This section was added after the presentation - was from the feedback to add the metrics one at a time
	// to see what the behaviour of the coefficient estimates are 
	// Note: These move log_sales to ivstyle per feedback, thus will not match the powerpoint slides
	xtabond2 MarkupDamijan CriticalCash exporter log_sales L.MarkupDamijan, ivstyle( i.year) gmmstyle(CriticalCash exporter L.MarkupDamijan log_sales)
	eststo dynamicDamCCR

	xtabond2 MarkupDamijan CriticalCash AssetTurnover exporter log_sales L.MarkupDamijan, ivstyle(i.year) gmmstyle(CriticalCash AssetTurnover exporter L.MarkupDamijan log_sales)
	eststo dynamicDamCCR_AT

	xtabond2 MarkupDamijan CriticalCash AssetTurnover CashFlowRatio exporter log_sales L.MarkupDamijan, ivstyle(i.year) gmmstyle(CriticalCash AssetTurnover CashFlowRatio exporter L.MarkupDamijan log_sales)
	eststo dynamicDamCCR_AT_CF

	xtabond2 MarkupDamijan CriticalCash AssetTurnover CashFlowRatio OperatingMargin exporter log_sales L.MarkupDamijan, ivstyle(i.year) gmmstyle(CriticalCash AssetTurnover CashFlowRatio OperatingMargin exporter L.MarkupDamijan log_sales)
	eststo dynamicDamCCR_AT_CF_OM

	esttab dynamicDamCCR dynamicDamCCR_AT dynamicDamCCR_AT_CF dynamicDamCCR_AT_CF_OM 
	 restore  
}
log close

//Have a nice day 

