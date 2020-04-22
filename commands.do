set mem 2000m
use "D:\Home\Downloads\TITG2020\zr2006_2018_EF_Stata11\zr2006_2018_EF_Stata11.dta"
//Only include the industries of interest
drop if ind2 <= 60
drop if ind2 >= 64

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
replace Markup = ( SalesRevenue + InventoryVariation ) / ( Payroll + MaterialCost + Service )


/*
 * Calcuate the financial values 
 */
//Combine positive and negative revenue
gen OperatingRevenue = aop151 - aop152
//Calcluate profitability
gen OperatingMargin = OperatingRevenue / aop110
//Cacluate efficiency 
gen AssetTurnover = aop110 / aop001


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


//Helpful other commands 
//inspect <variable name> 
//summarize <variable name> 
//scatter <var1> <var2) 