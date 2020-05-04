set mem 2000m
use "zr2006_2018_EF_Stata11.dta"
//Only include the industries of interest
drop if ind2 <= 60
drop if ind2 >= 64

xtset id year, y
/*
* Calcuating the markup
*/
//Caluclate sales revenue 
gen SalesRevenue = aop110

//Calcuate Invetory variation 
 gen InventoryVariation = aop121+aop122

// calculate payroll and materialcost
gen MaterialCost = aop130

gen Payroll = aop139

//Calculate service because every firm is putting lot of money in it, which biased the payroll 

 gen Service = aop134

//Calculate Markup 
gen Markup = ( SalesRevenue + InventoryVariation ) / ( Payroll + MaterialCost + Service )


/*
 * Calcuate the financial values 
 */
//Combine positive and negative revenue
gen OperatingRevenue = aop151 - aop152
//Calcluate profitability
gen OperatingMargin = OperatingRevenue / aop110
//Cacluate efficiency 
gen AssetTurnover = aop110 / aop001

//Calculate Cash Flow 
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
//Trying to find what is with the 0 expenses companies
 gen RetainedEarnings = aop056 + aop058
 gen Dividends = L.RetainedEarnings - RetainedEarnings + ProfitsMinusLoss

/*
 * Calcuate the metrics to control with that are known to impact markups
 */ 
gen exporter = 0
// 115 = EU revenue, 118 outside EU revenue 
replace exporter = 1 if aop115 > 0 | aop118 > 0

//Using log(sales) as a proxy for size. Not using assets because some of these firms don't have a 
// lot of fixed assets (not needed in IT). Should correlate directly with other, unobserved metrics,
// such as number of employees and market value.
gen log_sales = log(aop110)

//Calculating the log(TFP) using Y = A * L^alpha * K^(1 - alpha)
// thus log(TFP) = a = y - alpha * l - k * (1 - alpha)
// Y = gross sales
// L = labour + services spend
// K = capital spend (assets + materials)
// Try to avoid using this in the same regression as the markup calculated the old way. It includes 
//  a lot of the same information and you'll get a biased result
//gen log_labour = log(Payroll + Service)
//gen log_capital = log()

 xtreg Markup AssetTurnover OperatingMargin CashFlowRatio CriticalCash exporter log_sales L.Markup , fe

//Helpful other commands 
//inspect <variable name> 
//summarize <variable name> 
//scatter <var1> <var2) 