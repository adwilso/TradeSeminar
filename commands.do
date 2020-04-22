set mem 2000m
use "D:\Home\Downloads\TITG2020\zr2006_2018_EF_Stata11\zr2006_2018_EF_Stata11.dta"
//Only include the industries of interest
drop if ind2 <= 60
drop if ind2 >= 64

/*
* Calcuating the markup
*/
//Caluclate sales revenue 
gen Salesrevenue = aop110

//Calcuate Invetory variation 
 gen Inventoryvariation = aop121+aop122

// calculate payroll and materialcost
gen materialcost = aop130

gen Payroll = aop139

//Calculate service because every firm is putting lot of money in it, which biased the payroll 

 gen Service = aop134

//Calculate Markup 
replace Markup = ( Salesrevenue + Invertoryvariation ) / ( Payroll + materialcost + Service )


/*
 * Calcuate the financial values 
 */
//Combine positive and negative revenue
gen OperatingRevenue = aop151 - aop152
//Calcluate profitability
gen OperatingMargin = OperatingRevenue / aop110
//Cacluate efficiency 
gen AssetTurnover = aop110 / aop001


//Helpful other commands 
//inspect <variable name> 
//summarize <variable name> 
