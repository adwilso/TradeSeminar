set mem 2000m
use "zr2006_2018_EF_Stata11.dta"
//Only include the industries of interest
drop if ind2 <= 60
drop if ind2 >= 64

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



//aop001 == Capital (All assets), aop128 == COGS
gen log_materials2 = log_materials^2
gen log_payroll2 = log_payroll^2
gen log_capital2 = log_capital^2
gen log_labourMaterials = log_payroll * log_materials
gen log_labourCapital = log_capital * log_payroll
gen log_capitalMaterials = log_capital * log_materials
gen log_labourCapitalMaterials = log_payroll * log_capital * log_materials

xi: reg log_sales log_payroll log_capital log_materials log_materials2 log_payroll2 log_capital2 log_labourMaterials log_labourCapital log_capitalMaterials log_labourCapitalMaterials i.year
gen blols = _b[log_payroll]
gen bkols = _b[log_capital]
gen bmols = _b[log_materials]
gen bmmols = _b[log_materials2]
gen blmols = _b[log_labourMaterials]
gen bkmols = _b[log_capitalMaterials]
gen blkmols = _b[log_labourCapitalMaterials]


//bmols == material demand elasticity, aop128 == COGS
gen theta = bmols + (2* bmmols * log_materials) + (bkmols * log_capital) + (blkmols * log_payroll * log_capital)
gen MarkupDamijan = theta * log_sales / log_materials

//Trim outliers - these are companies that we missed in the clean up earlier
drop if MarkupDamijan > 100 
drop if MarkupHall > 100

//Generate graphs for the presentation 
hist MarkupDam , xtitle("Markup") title("Markup Distribution - Damijan")
hist MarkupHall , xtitle("Markup") title("Markup Distribution - Hall")

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
xtabond2 MarkupDamijan AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupDamijan, ivstyle(log_sales i.year) gmmstyle( AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter L.MarkupDamijan)
//Suggestion from presentation feedback - move log_sales to ivstyle 
//xtabond2 MarkupDamijan AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupDamijan, ivstyle(i.year) gmmstyle( log_sales AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter L.MarkupDamijan)
eststo dynamicDamFull

xtabond2 MarkupHall AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.MarkupHall, ivstyle(log_sales i.year) gmmstyle( AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter L.MarkupHall)
eststo dynamicHall
//Get that output 
esttab dynamicHall dynamicDamFull

//Trimmed graphs so that it is easier to see the distribution without outliers 
histogram CashFlowRatio if CashFlowRatio <100 & CashFlowRatio > -100, ytitle(Density) xtitle(Cash Flow Ratio) title(Cash Flow Ratio Distribution)
histogram CriticalCash if CriticalCash <100 & CriticalCash > -100, ytitle(Density) xtitle(Critical Cash Ratio) title(Critical Cash Ratio Distribution)
histogram OperatingMargin if OperatingMargin <1 & OperatingMargin > -1, ytitle(Density) xtitle(Operating Margin) title(Operating Margin Distribution)
histogram AssetTurnover if AssetTurnover <100 & AssetTurnover > -100, ytitle(Density) xtitle(AssetTurnover) title(Asset Turnover Distribution) 

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

esttab dynamicDamCCR dynamicDamCCR_AT dynamicDamCCR_AT_CF dynamicDamCCR_AT_CF_OM dynamicDamFull
