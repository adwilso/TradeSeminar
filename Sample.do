*****
*Lecture Intro to Stata
*****
*use "/Users/crtkostevc/Documents/Documents/Projekti/imports-innovation-exports/spain-all.dta", clear
cd "//Users/crt/Documents/Documents/Projekti/imports-innovation-exports/"
cd "//EMTEC/"
use "spain-all.dta", clear
===================
*Looping
===================
forval x=1/4 {
	gen imp_dum_`x'=imp_dum[_n-`x'] if year==year[n-`x']+`x'
	}

query memory
===================
*Data desctriptives
===================
bysort inov_pc: table (inov_pr ex_dum)
summarize (l va_emp no_prodinn no_procinn)
summarize (l va_emp no_prodinn no_procinn), detail
table year, by(imp_dum ex_dum) c(count id sum inov_pr sum inov_pc) f(%9.2f)
tabulate year
***********************************************
* Are importers more productive than non importers
***********************************************
gen imp_status= "non-importer" if imp_dum==0
replace imp_status= "importer" if imp_dum==1

gen exp_status= "non-exporter" if ex_dum==0
replace exp_status= "exporter" if ex_dum==1

*Bar graph
graph bar lva_emp, over(imp_status) ytitle ("Log value added per employee") title("Effect of import status on productivity") legend( label(1 "non-importer") label(2 "importer") )note("Source:  Survey of business strategies ESEE")
 
*Graph both import and export status
graph bar lva_emp, over(imp_status) over(exp_status) yscale(range(6/10) titlegap(3)) ytitle ("Log value added per employee") title("effect of import status on productivity") subtitle("Average log value added per employee")note("Source:  Survey of business strategies ESEE")

gen inovator="innovator" 
replace inovator="non-innovator" if inov_pr==0&inov_pc==0

*import export + innovator status
graph bar lva_emp, over(inovator) over(imp_status) over(exp_status)  ytitle ("Log value added per employee") title("Effect of import status on productivity") subtitle("Average log value added per employee") note("Source:  Survey of business strategies ESEE")

graph bar lva_emp, by(inovator exp_status) over(imp_status) ytitle ("Log value added per employee") title("Effect of import status on productivity") note("Source:  Survey of business strategies ESEE")

*import export status bar chart
graph bar (median) lva_emp, over(imp_status) over(exp_status) over(inovator) linten(*1.6) ytitle ("Log value added per employee") title("Effect of import status on productivity") subtitle("Median log value added per employee") note("Source:  Survey of business strategies ESEE")

*Economist scheme
graph bar (median) lva_emp, over(imp_status) over(exp_status) over(inovator) linten(*1.6) scheme(economist) ytitle ("Log value added per employee") title("Effect of import status on productivity") subtitle("Median log value added per employee") note("Source:  Survey of business strategies ESEE")
graph bar (median) lva_emp, over(imp_status) over(exp_status) over(inovator) linten(*1.6) scheme(sj) ytitle ("Log value added per employee") title("Effect of import status on productivity") subtitle("Median log value added per employee") note("Source:  Survey of business strategies ESEE")

graph query, schemes
==============================
*correlation matrices
==============================
pwcorr (ex_dum imp_dum inov_pr inov_pc)
pwcorr (ex_dum imp_dum inov_pr inov_pc), star(0.01)

pwcorrs (ex_dum imp_dum inov_pr inov_pc), sig 
pwcorrs (ex_dum imp_dum inov_pr inov_pc), sp sig 
 
==============================
*Complete distribution by value added
==============================
twoway (kdensity lva_emp if imp_dum==0&ex_dum==0, gauss n(500) width(0.45) clpat(solid)) (kdensity lva_emp if imp_dum==1&ex_dum==0, gauss n(500) width(0.45) clpat(vshortdash))(kdensity lva_emp if imp_dum==1&ex_dum==1, gauss n(500) width(0.45) lcolor(red) clpat(solid)), ytitle(density) xtitle(Logarithm of value added per employee) legend(on order(1 "non importers, non exporters" 2 "importers, non exporters" 3 "importers and exporters")) scheme (s1mono) 

ksmirnov lva_emp, by(imp_dum)
kwallis lva_emp, by(ex_dum)
===============================
*regressions
===============================
*no lags
probit inov_pc inov_pr ex_dum imp_dum lva_emp lemp

*w/ lags
probit inov_pc ex_dum_1 imp_dum_1 lva_emp_1 lemp_1 

*w/ lags, dummies
xi: probit inov_pc ex_dum_1 imp_dum_1 lva_emp_1 lemp_1 i.sector i.year, robust 

*w/ lags, interaction dummies
xi: probit inov_pc ex_dum_1 imp_dum_1 lva_emp_1 lemp_1 i.sector*i.year, robust 
*marginal effects
xi: dprobit inov_pc ex_dum_1 imp_dum_1 lva_emp_1 lemp_1 i.sector*i.year, robust 

gen rd_share=total_rd/y

xi: xtprobit inov_pc ex_dum_1 imp_dum_1 lva_emp_1 lemp_1 i.sector i.year, i(id) 
outreg using uposmazt.xls, title("Panel probit regression of trade exposure effect on innovativeness") bdec(3) 3aster replace

==============================
*matching
==============================
drop score_imp imp_score
xi: pscore imp_noexp va_emp_1 k_emp_1 emp_1 fdi_1 i.sector i.year, pscore(score_imp) comsup

gen imp_score=100*sector+score_imp


bysort id (year): gen imp_noexp_1=imp_noexp[_n-1] if year==year[_n-1]+1

*overall
attnd inov_starter imp_noexp_1, pscore(imp_score) comsup   
attnd inov_pr_starter imp_noexp_1, pscore(imp_score) comsup
attnd inov_pc_starter imp_noexp_1, pscore(imp_score) comsup 

