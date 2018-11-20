/////////////////////////////
*** 1st STEP REGRESSION (1960-2016)
*----------------------------*
***** betaGS_PW (government-spending cyclicality by Prais-Winsten)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.

forvalues i=1/196 {
	capture noisily {
               qui prais dlRGS dlRGDP if iso1==`i'
			   replace coef =_b[dlRGDP] if iso1==`i'
			   replace se =_se[dlRGDP] if iso1==`i'
			   replace t =_b[dlRGDP]/_se[dlRGDP] if iso1==`i'
			   replace p = 2*ttail(e(df_r),abs((_b[dlRGDP]/_se[dlRGDP]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
keep if n>=24		//check: all countries with n=24 have 25 yrs of obs.
save betaGS_PW, replace
*------------------*
***** betaGS_PW_g (government-spending cyclicality by Prais-Winsten at good times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196 {
	capture noisily {
               qui prais dlRGS dlRGDP if iso1==`i' & D==0
			   replace coef=_b[dlRGDP] if iso1==`i'
			   replace se=_se[dlRGDP] if iso1==`i'
			   replace t=_b[dlRGDP]/_se[dlRGDP] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP]/_se[dlRGDP]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaGS_PW_g, replace	//will be merged with betaGS_PW later
*------------------*
***** betaGS_PW_b (government-spending cyclicality by Prais-Winsten at bad times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196 {
	capture noisily {
               qui prais dlRGS dlRGDP if iso1==`i' & D==1
			   replace coef=_b[dlRGDP] if iso1==`i'
			   replace se=_se[dlRGDP] if iso1==`i'
			   replace t=_b[dlRGDP]/_se[dlRGDP] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP]/_se[dlRGDP]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaGS_PW_b, replace		//will be merged with betaGS_PW later
*------------------**------------------*
***** betaGS_OLS (government-spending cyclicality by OLS)
clear all
use DATA60
sort iso year
parmby "qui reg dlRGS dlRGDP, vce(robust)", by(iso) label saving(betaGS_OLS,replace)

use betaGS_OLS, clear
keep if parm=="dlRGDP" & dof>=22	//dof=N-2 --> need N>=24, so dof>=22
keep iso estimate stderr t p dof
tab iso if estimate==0
save betaGS_OLS, replace
*------------------*
***** betaGS_OLS_g (government-spending cyclicality by OLS at good times)
clear all
use DATA60
sort iso year
parmby "qui reg dlRGS dlRGDP if D==0, vce(robust)", by(iso) label saving(betaGS_OLS_g,replace)

use betaGS_OLS_g, clear
keep if parm=="dlRGDP"
keep iso estimate stderr t p dof
tab iso if estimate==0
save betaGS_OLS_g, replace		//will be merged with betaGS_OLS later
*------------------*
***** betaGS_OLS_b (government-spending cyclicality by OLS at bad times)
clear all
use DATA60
sort iso year
parmby "qui reg dlRGS dlRGDP if D==1, vce(robust)", by(iso) label saving(betaGS_OLS_b,replace)

use betaGS_OLS_b, clear
keep if parm=="dlRGDP"
keep iso estimate stderr t p dof
tab iso if estimate==0
save betaGS_OLS_b, replace		//will be merged with betaGS_OLS later
*------------------* *------------------*
***** betaVAT_PW (VAT cyclicality by Prais-Winsten)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais VAT dlRGDP100 if iso1==`i'
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
keep if n>=25 & n!=.	//drop CZE with 24obs for VAT
save betaVAT_PW, replace
*------------------*
***** betaVAT_PW_g (VAT cyclicality by Prais-Winsten at good times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais VAT dlRGDP100 if iso1==`i' & D==0
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaVAT_PW_g, replace		//will be merged with betaVAT_PW later
*------------------*
***** betaVAT_PW_b (VAT cyclicality by Prais-Winsten at bad times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais VAT dlRGDP100 if iso1==`i' & D==1
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaVAT_PW_b, replace		//will be merged with betaVAT_PW later
*------------------**------------------*
***** betaVAT_OLS (VAT cyclicality by OLS)
clear all
use DATA60
sort iso1 year
parmby "qui reg VAT dlRGDP100, vce(robust)", by(iso) label saving(betaVAT_OLS,replace)

use betaVAT_OLS, clear
keep if parm=="dlRGDP100" & dof >=23
keep iso estimate stderr t p dof
tab iso if estimate==0 //KOR, PRY: bcoz their VAT rates dont change
replace estimate=. if estimate==0
save betaVAT_OLS, replace
*------------------*
***** betaVAT_OLS_g (VAT cyclicality by OLS at good times)
clear all
use DATA60
sort iso1 year
parmby "qui reg VAT dlRGDP100 if D==0, vce(robust)", by(iso) label saving(betaVAT_OLS_g,replace)

use betaVAT_OLS_g, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p dof
tab iso if estimate==0 //KOR, PRY, THA, ZAF (in the sample size with at least 25 yrs of obs): bcoz their VAT rates dont change
replace estimate=. if estimate==0
save betaVAT_OLS_g, replace		//will be merged with betaVAT_OLS later
*------------------*
***** betaVAT_OLS_b (VAT cyclicality by OLS at bad times)
clear all
use DATA
sort iso1 year
parmby "qui reg VAT dlRGDP100 if D==1, vce(robust)", by(iso) label saving(betaVAT_OLS_b,replace)

use betaVAT_OLS_b, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p dof
tab iso if estimate==0 //JPN, KOR, PHL, PRY: bcoz their VAT rates dont change
replace estimate=. if estimate==0
save betaVAT_OLS_b, replace		//will be merged with betaVAT_OLS later
*------------------**------------------*
***** betaPIT_PW (PIT cyclicality by Prais-Winsten)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais PIT dlRGDP100 if iso1==`i'
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
keep if n>=25 & n!=.
save betaPIT_PW, replace
*------------------*
***** betaPIT_PW_g (PIT cyclicality by Prais-Winsten at good times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais PIT dlRGDP100 if iso1==`i' & D==0
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaPIT_PW_g, replace		//will be merged with betaPIT_PW
*------------------*
***** betaPIT_PW_b (PIT cyclicality by Prais-Winsten at bad times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais PIT dlRGDP100 if iso1==`i' & D==1
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaPIT_PW_b, replace		//will be merged with betaPIT_PW
*------------------**------------------*
***** betaPIT_OLS (PIT cyclicality by OLS)
clear all
use DATA60
sort iso1 year
parmby "qui reg PIT dlRGDP100, vce(robust)", by(iso) label saving(betaPIT_OLS,replace)

use betaPIT_OLS, clear
keep if parm=="dlRGDP100" & dof>=23
keep iso estimate stderr t p dof
tab iso if estimate==0 //ARE, BHR, KWT, OMN, QAT, SAU: bcoz their PIT rates do not change
replace estimate=. if estimate==0
save betaPIT_OLS, replace
*------------------*
***** betaPIT_OLS_g (PIT cyclicality by OLS at good times)
clear all
use DATA60
sort iso1 year
parmby "qui reg PIT dlRGDP100 if D==0, vce(robust)", by(iso) label saving(betaPIT_OLS_g,replace)

use betaPIT_OLS_g, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p dof
tab iso if estimate==0
save betaPIT_OLS_g, replace			//will be merged with betaPIT_OLS
*------------------*
***** betaPIT_OLS_b (PIT cyclicality by OLS at bad times)
clear all
use DATA60
sort iso1 year
parmby "qui reg PIT dlRGDP100 if D==1, vce(robust)", by(iso) label saving(betaPIT_OLS_b,replace)

use betaPIT_OLS_b, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p dof
tab iso if estimate==0 //THA, URY: bcoz their PIT rates dont change
replace estimate=. if estimate==0
save betaPIT_OLS_b, replace
*------------------**------------------*
***** betaCIT_PW (CIT cyclicality by Prais-Winsten)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais CIT dlRGDP100 if iso1==`i'
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
keep if n>=25 & n!=.
save betaCIT_PW, replace
*------------------*
***** betaCIT_PW_g (CIT cyclicality by Prais-Winsten at good times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais CIT dlRGDP100 if iso1==`i' & D==0
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaCIT_PW_g, replace		//will be merged with betaCIT_PW
*------------------*
***** betaCIT_PW_b (CIT cyclicality by Prais-Winsten at bad times)
clear all
use DATA60
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.
forvalues i=1/196{
	capture noisily {
               qui prais CIT dlRGDP100 if iso1==`i' & D==1
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
save betaCIT_PW_b, replace		//will be merged with betaCIT_PW
*------------------**------------------*
***** betaCIT_OLS (CIT cyclicality by OLS)
clear all
use DATA60
sort iso1 year
parmby "qui reg CIT dlRGDP100, vce(robust)", by(iso) label saving(betaCIT_OLS,replace)

use betaCIT_OLS, clear
keep if parm=="dlRGDP100" & dof>=23
keep iso estimate stderr t p dof
tab iso if estimate==0 //BHR: bcoz their CIT rates dont change
replace estimate=. if estimate==0
save betaCIT_OLS, replace
*------------------*
***** betaCIT_OLS_g (CIT cyclicality by OLS at good times)
clear all
use DATA60
sort iso1 year
parmby "qui reg CIT dlRGDP100 if D==0, vce(robust)", by(iso) label saving(betaCIT_OLS_g,replace)

use betaCIT_OLS_g, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p dof
tab iso if estimate==0
save betaCIT_OLS_g, replace
*------------------*
***** betaCIT_OLS_b (CIT cyclicality by OLS at bad times)
clear all
use DATA60
sort iso1 year
parmby "qui reg CIT dlRGDP100 if D==1, vce(robust)", by(iso) label saving(betaCIT_OLS_b,replace)

use betaCIT_OLS_b, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p dof
tab iso if estimate==0 //THA: bcoz their VAT rates dont change
replace estimate=. if estimate==0
save betaCIT_OLS_b, replace
*------------------**------------------**------------------*


*** merge all betas into fiscyl.dta
clear all
use DATA60
sort iso1 year
keep countryname iso
duplicates drop
*
joinby iso using betaGS_PW, unm(both)
keep countryname iso coef
rename coef betaGS_PW
label var betaGS_PW "estimated GS cyclicality by Prais-Winsten"
*
joinby iso using betaGS_PW_g, unm(both)
keep countryname iso betaGS_PW coef
rename coef betaGS_PW_g
label var betaGS_PW_g "estimated GS cyclicality by Prais-Winsten at good times"
*
joinby iso using betaGS_PW_b, unm(both)
keep countryname iso betaGS_PW betaGS_PW_g coef
rename coef betaGS_PW_b
label var betaGS_PW_b "estimated GS cyclicality by Prais-Winsten at bad times"
*
joinby iso using betaGS_OLS, unm(both)
keep countryname iso betaGS_PW betaGS_PW_g betaGS_PW_b estimate
rename estimate betaGS_OLS
label var betaGS_OLS "estimated GS cyclicality by OLS"
*
joinby iso using betaGS_OLS_g, unm(both)
keep countryname iso betaGS_PW betaGS_PW_g betaGS_PW_b betaGS_OLS estimate
rename estimate betaGS_OLS_g
label var betaGS_OLS_g "estimated GS cyclicality by OLS at good times"
*
joinby iso using betaGS_OLS_b, unm(both)
keep countryname iso betaGS_PW betaGS_PW_g betaGS_PW_b betaGS_OLS betaGS_OLS_g estimate
rename estimate betaGS_OLS_b
label var betaGS_OLS_b "estimated GS cyclicality by OLS at bad times"
*
joinby iso using betaVAT_PW, unm(both)
drop _merge se t p n
rename coef betaVAT_PW
label var betaVAT_PW "estimated VAT cyclicality by Prais-Winsten"
*
joinby iso using betaVAT_OLS, unm(both)
drop _merge stderr t p dof
rename estimate betaVAT_OLS
label var betaVAT_OLS "estimated VAT cyclicality by OLS"
*
joinby iso using betaPIT_PW, unm(both)
drop _merge se t p n
rename coef betaPIT_PW
label var betaPIT_PW "estimated PIT cyclicality by Prais-Winsten"
*
joinby iso using betaPIT_OLS, unm(both)
drop _merge stderr t p dof
rename estimate betaPIT_OLS
label var betaPIT_OLS "estimated PIT cyclicality by OLS"
*
joinby iso using betaCIT_PW, unm(both)
drop _merge se t p n
rename coef betaCIT_PW
label var betaCIT_PW "estimated CIT cyclicality by Prais-Winsten"
*
joinby iso using betaCIT_OLS, unm(both)
drop _merge stderr t p dof
rename estimate betaCIT_OLS
label var betaCIT_OLS "estimated CIT cyclicality by OLS"
*
joinby iso using betaVAT_PW_g, unm(both)
drop _merge se t p n
rename coef betaVAT_PW_g
label var betaVAT_PW_g "estimated VAT cyclicality by Prais-Winsten at good times"
*
joinby iso using betaVAT_PW_b, unm(both)
drop _merge se t p n
rename coef betaVAT_PW_b
label var betaVAT_PW_b "estimated VAT cyclicality by Prais-Winsten at bad times"
*
joinby iso using betaVAT_OLS_g, unm(both)
drop _merge stderr t p dof
rename estimate betaVAT_OLS_g
label var betaVAT_OLS_g "estimated VAT cyclicality by OLS at good times"
*
joinby iso using betaVAT_OLS_b, unm(both)
drop _merge stderr t p dof
rename estimate betaVAT_OLS_b
label var betaVAT_OLS_b "estimated VAT cyclicality by OLS at bad times"
*
joinby iso using betaPIT_PW_g, unm(both)
drop _merge se t p n
rename coef betaPIT_PW_g
label var betaPIT_PW_g "estimated PIT cyclicality by Prais-Winsten at good times"
*
joinby iso using betaPIT_PW_b, unm(both)
drop _merge se t p n
rename coef betaPIT_PW_b
label var betaPIT_PW_b "estimated PIT cyclicality by Prais-Winsten at bad times"
*
joinby iso using betaPIT_OLS_g, unm(both)
drop _merge stderr t p dof
rename estimate betaPIT_OLS_g
label var betaPIT_OLS_g "estimated PIT cyclicality by OLS at good times"
*
joinby iso using betaPIT_OLS_b, unm(both)
drop _merge stderr t p dof
rename estimate betaPIT_OLS_b
label var betaPIT_OLS_b "estimated PIT cyclicality by OLS at bad times"
*
joinby iso using betaCIT_PW_g, unm(both)
drop _merge se t p n
rename coef betaCIT_PW_g
label var betaCIT_PW_g "estimated CIT cyclicality by Prais-Winsten at good times"
*
joinby iso using betaCIT_PW_b, unm(both)
drop _merge se t p n 
rename coef betaCIT_PW_b
label var betaCIT_PW_b "estimated CIT cyclicality by Prais-Winsten at bad times"
*
joinby iso using betaCIT_OLS_g, unm(both)
drop _merge stderr t p dof
rename estimate betaCIT_OLS_g
label var betaCIT_OLS_g "estimated CIT cyclicality by OLS at good times"
*
joinby iso using betaCIT_OLS_b, unm(both)
drop _merge stderr t p dof
rename estimate betaCIT_OLS_b
label var betaCIT_OLS_b "estimated CIT cyclicality by OLS at bad times"
*
replace betaGS_PW_g=. if betaGS_PW==.
replace betaGS_PW_b=. if betaGS_PW==.
replace betaGS_OLS_g=. if betaGS_OLS==.
replace betaGS_OLS_b=. if betaGS_OLS==.

replace betaVAT_PW_g=. if betaVAT_PW==.
replace betaVAT_PW_b=. if betaVAT_PW==.
replace betaVAT_OLS_g=. if betaVAT_OLS==.
replace betaVAT_OLS_b=. if betaVAT_OLS==.

replace betaPIT_PW_g=. if betaPIT_PW==.
replace betaPIT_PW_b=. if betaPIT_PW==.
replace betaPIT_OLS_g=. if betaPIT_OLS==.
replace betaPIT_OLS_b=. if betaPIT_OLS==.

replace betaCIT_PW_g=. if betaCIT_PW==.
replace betaCIT_PW_b=. if betaCIT_PW==.
replace betaCIT_OLS_g=. if betaCIT_OLS==.
replace betaCIT_OLS_b=. if betaCIT_OLS==.

save fiscyl, replace
*------------------*

* MERGE fiscyl.dta with mean of other variables (from DATA60.dta) --> fiscyl60.dta: consisting all betas and all mean variables for cross-country regression
clear all
use DATA60
bysort iso: egen debt=mean(DEBT)
bysort iso: egen debt_vol=sd(DEBT)

bysort iso: egen fiscap=mean(FISCAP)
bysort iso: egen fiscap_vol=sd(FISCAP)
bysort iso: egen lfiscap=mean(LFISCAP)
bysort iso: egen lfiscap_vol=sd(LFISCAP)

bysort iso: egen fiscap_e=mean(FISCAP_E)
bysort iso: egen fiscap_e_vol=sd(FISCAP_E)
bysort iso: egen lfiscap_e=mean(LFISCAP_E)
bysort iso: egen lfiscap_e_vol=sd(LFISCAP_E)

bysort iso: egen polcon=mean(POLCON)
bysort iso: egen trade=mean(TRADE)
bysort iso: egen inf=mean(INF)
bysort iso: egen GDP=mean(GDPR)
bysort iso: egen nare=mean(NARE)
bysort iso: egen manu=mean(MANU)
bysort iso: egen TAL=mean(TAL_GDP)
bysort iso: egen gs=mean(GS)

bysort iso: egen CRI=mean(CRIR)
bysort iso: egen ERI=mean(ERIR)
bysort iso: egen FRI=mean(FRIR)
bysort iso: egen PRI=mean(PRIR)

bysort iso: egen govstab=mean(GOVSTAB)
bysort iso: egen socecon=mean(SOCECON)
bysort iso: egen invest=mean(INVEST)
bysort iso: egen inconflict=mean(INCONFLICT)
bysort iso: egen exconflict=mean(EXCONFLICT)
bysort iso: egen corrupt=mean(CORRUPT)
bysort iso: egen military=mean(MILITARY)
bysort iso: egen religious=mean(RELIGIOUS)
bysort iso: egen law=mean(LAW)
bysort iso: egen ethnic=mean(ETHNIC)
bysort iso: egen democracy=mean(DEMOCRACY)
bysort iso: egen bureau=mean(BUREAU)

bysort iso: egen arGDP=mean(rGDP)
bysort iso: egen SWFm=max(SWF)

keep iso debt debt_vol fiscap fiscap_vol lfiscap lfiscap_vol fiscap_e fiscap_e_vol lfiscap_e lfiscap_e_vol polcon trade inf GDP nare manu TAL gs CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau arGDP SWFm region regioncode incomegroup OECDgroup
rename SWFm SWF
duplicates drop

joinby iso using fiscyl, unm(both)
drop if betaGS_PW==.
drop _m
save fiscyl60, replace
*----------------------------**----------------------------*





/////////////////////////////
*** 1st STEP REGRESSION (1980-2016)
*----------------------------*
***** betaGS_PW (government-spending cyclicality by Prais-Winsten)
clear all
use DATA80		//panel data with GDP, GS and some other variables from WEO
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.
gen n=.

forvalues i=1/193 {
	capture noisily {
               qui prais dlRGS dlRGDP if iso1==`i'
			   replace coef =_b[dlRGDP] if iso1==`i'
			   replace se =_se[dlRGDP] if iso1==`i'
			   replace t =_b[dlRGDP]/_se[dlRGDP] if iso1==`i'
			   replace p = 2*ttail(e(df_r),abs((_b[dlRGDP]/_se[dlRGDP]))) if iso1==`i'
			   replace n = e(N) if iso1==`i'
			     }
				 }
keep iso coef se t p n
duplicates drop
keep if n>=24 & n!=.		//check: all countries with n=24 have 25 yrs of obs.
save betaGS80_PW, replace
*------------------*
***** betaGS_OLS (government-spending cyclicality by OLS)
clear all
use DATA80
sort iso year
parmby "qui reg dlRGS dlRGDP, vce(robust)", by(iso) label saving(betaGS80_OLS,replace)

use betaGS80_OLS, clear
keep if parm=="dlRGDP" & dof>=22	//dof=N-2 --> need N>=24, so dof>=22
keep iso estimate stderr t p dof
tab iso if estimate==0
save betaGS80_OLS, replace
*------------------**------------------*

* Merge to create fiscyl80.dta
clear all
use DATA80
bysort iso: egen debt=mean(DEBT)
bysort iso: egen debt_vol=sd(DEBT)

bysort iso: egen fiscap=mean(FISCAP)
bysort iso: egen fiscap_vol=sd(FISCAP)
bysort iso: egen lfiscap=mean(LFISCAP)
bysort iso: egen lfiscap_vol=sd(LFISCAP)

bysort iso: egen polcon=mean(POLCON)
bysort iso: egen trade=mean(TRADE)
bysort iso: egen inf=mean(INF)
bysort iso: egen GDP=mean(GDPR)
bysort iso: egen nare=mean(NARE)
bysort iso: egen manu=mean(MANU)
bysort iso: egen TAL=mean(TAL_GDP)
bysort iso: egen gs=mean(GS)

bysort iso: egen CRI=mean(CRIR)
bysort iso: egen ERI=mean(ERIR)
bysort iso: egen FRI=mean(FRIR)
bysort iso: egen PRI=mean(PRIR)

bysort iso: egen govstab=mean(GOVSTAB)
bysort iso: egen socecon=mean(SOCECON)
bysort iso: egen invest=mean(INVEST)
bysort iso: egen inconflict=mean(INCONFLICT)
bysort iso: egen exconflict=mean(EXCONFLICT)
bysort iso: egen corrupt=mean(CORRUPT)
bysort iso: egen military=mean(MILITARY)
bysort iso: egen religious=mean(RELIGIOUS)
bysort iso: egen law=mean(LAW)
bysort iso: egen ethnic=mean(ETHNIC)
bysort iso: egen democracy=mean(DEMOCRACY)
bysort iso: egen bureau=mean(BUREAU)

keep iso countryname debt debt_vol fiscap fiscap_vol lfiscap lfiscap_vol polcon trade inf GDP nare manu TAL gs CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau region incomegroup OECDgroup
duplicates drop

joinby iso using betaGS80_PW, unm(both)
keep if _m==3
rename coef betaGS_PW
drop se t p n _merge

joinby iso using betaGS80_OLS, unm(both)
rename estimate betaGS_OLS
drop stderr dof t p _merge
save fiscyl80, replace
*----------------------------**----------------------------*






/////////////////////////////
*** 2nd STEP: CROSS REGRESSION (1960-2016)
*----------------------------*
* Pairwise correlation matrix, sample period 1960-2016
use fiscyl60, clear
logout, save("table2a-Corrmatrix") excel dec(2) replace: pwcorr betaGS_PW betaVAT_PW betaPIT_PW betaCIT_PW polcon inf trade TAL gs GDP debt debt_vol fiscap fiscap_vol lfiscap lfiscap_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau, star(0.5) 
*----------------------------*
* Pairwise correlation matrix, sample period 1980-2016
use fiscyl80, clear
logout, save("table2b-Corrmatrix") excel dec(2) replace: pwcorr betaGS_PW betaGS_OLS polcon inf trade TAL gs GDP debt debt_vol fiscap fiscap_vol lfiscap lfiscap_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau, star(0.5) 
*----------------------------*



* Summary statistics (the left part), sample period 1960-2016
use fiscyl60, clear
sum2docx betaGS_PW betaVAT_PW betaPIT_PW betaCIT_PW polcon inf trade TAL gs GDP debt debt_vol fiscap fiscap_vol lfiscap lfiscap_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau using table3l-sumstat.docx, replace obs mean sd min max
*----------------------------*
* Summary statistics (the right part), sample period 1980-2016
use fiscyl80, clear
sum2docx betaGS_PW betaGS_OLS polcon inf trade TAL gs GDP debt debt_vol fiscap fiscap_vol lfiscap lfiscap_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau using table3r-sumstat.docx, replace obs mean sd min max
*----------------------------*



* Summary betaGS by region/income level/OECDgroup (the left part), sample period 1960-2016
use fiscyl60, clear
local bygroup region incomegroup OECDgroup
foreach var in `bygroup' {
estpost tabstat betaGS_PW, by(`var') stat(mean sd min max)
esttab . using table4l.doc, append cells("mean(fmt(a3)) sd min max")
}
*----------------------------*
* Summary betaGS by region/income level/OECDgroup (the right part), sample period 1980-2016
use fiscyl80, clear
local bygroup region incomegroup OECDgroup
foreach var in `bygroup' {
estpost tabstat betaGS_PW, by(`var') stat(mean sd min max)
esttab . using table4r.doc, append cells("mean(fmt(a3)) sd min max")
}
*----------------------------*


*** Table 5
* Determinants of betaGS_PW, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_PW polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_PW polcon inf trade TAL gs `var', vce(robust)
}
esttab *, se(3) stats(N r2 p), using table5a.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*
* Determinants of betaGS_OLS, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_OLS polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_OLS polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table5b.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------**----------------------------*
/// PLUS: Determinants of betaGS_PW when tax base excludes social contributions 'fiscap_e' 
clear all
use fiscyl60
local indepvars fiscap_e fiscap_e_vol lfiscap_e lfiscap_e_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_PW polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_PW polcon inf trade TAL gs `var', vce(robust)
}
esttab *, se(3) stats(N r2 p), using table5aPLUS.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------**----------------------------*
* Determinants of betaGS_PW, sample period 1980-2016
clear all
use fiscyl80
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_PW polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_PW polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table5c.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*
* Determinants of betaGS_OLS, sample period 1980-2016
clear all
use fiscyl80
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_OLS polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_OLS polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table5d.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------**----------------------------*
* Plus Good/Bad times: Determinants of betaGS_PW_g (betaGS_OLS_g) at good times [similarly for bad times but variables are not sig.]
// gen dummy variable for good times (positive growth rate D==0) and bad times (negative growth rate D==1)
// calculate average variables during good times/ bad times respectively (construct new dataset)
// run cross-country regression using betaGS_PW_g (betaGS_OLS_g) in fiscyl60.dta
* GOOD TIMES
* Create mean data for good times
clear all
use DATA60
replace D =. if dlRGDP ==.
keep if D == 0
fillin iso year
local varlist GS DEBT FISCAP LFISCAP POLCON TRADE INF GDPR MANU NARE TAL_GDP ERIR FRIR PRIR CRIR GOVSTAB SOCECON INVEST INCONFLICT EXCONFLICT CORRUPT MILITARY RELIGIOUS LAW ETHNIC DEMOCRACY BUREAU
foreach var in `varlist' {
	egen `var'1 = mean(`var'), by(iso)
	}
bysort iso: egen DEBT_VOL1 = sd(DEBT)
bysort iso: egen FISCAP_VOL1 = sd(FISCAP)
bysort iso: egen LFISCAP_VOL1 = sd(LFISCAP)

keep *1 iso
drop iso1
duplicates drop
rename *, lower
joinby iso using fiscyl60, unm(both)
keep if _m==3

keep iso countryname *1 betaGS_PW_g betaGS_OLS_g
rename (gs1 debt1 fiscap1 lfiscap1 polcon1 trade1 inf1 gdpr1 manu1 nare1 tal_gdp1 erir1 frir1 prir1 crir1 govstab1 socecon1 invest1 inconflict1 exconflict1 corrupt1 military1 religious1 law1 ethnic1 democracy1 bureau1 debt_vol1 fiscap_vol1 lfiscap_vol1)(gs debt fiscap lfiscap polcon trade inf GDP manu nare TAL ERI FRI PRI CRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau debt_vol fiscap_vol lfiscap_vol)
//Determinants of tableGS_PW_g at good times
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_PW_g polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_PW_g polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N p r2), using tableGS_PW_g.csv, append star(* 0.10 ** 0.05 *** 0.01)
//Determinants of tableGS_OLS_g at good times
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_OLS_g polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_OLS_g polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N p r2), using tableGS_OLS_g.csv, append star(* 0.10 ** 0.05 *** 0.01)
*--------------------------------------*
* BAD TIMES
* Create mean data for bad times
clear all
use DATA60
replace D =. if dlRGDP ==.
keep if D == 1
fillin iso year
local varlist GS DEBT FISCAP LFISCAP POLCON TRADE INF GDPR MANU NARE TAL_GDP ERIR FRIR PRIR CRIR GOVSTAB SOCECON INVEST INCONFLICT EXCONFLICT CORRUPT MILITARY RELIGIOUS LAW ETHNIC DEMOCRACY BUREAU
foreach var in `varlist' {
	egen `var'1 = mean(`var'), by(iso)
	}
bysort iso: egen DEBT_VOL1 = sd(DEBT)
bysort iso: egen FISCAP_VOL1 = sd(FISCAP)
bysort iso: egen LFISCAP_VOL1 = sd(LFISCAP)

keep *1 iso
drop iso1
duplicates drop
rename *, lower
joinby iso using fiscyl60, unm(both)
keep if _m==3

keep iso countryname *1 betaGS_OLS_b
rename (gs1 debt1 fiscap1 lfiscap1 polcon1 trade1 inf1 gdpr1 manu1 nare1 tal_gdp1 erir1 frir1 prir1 crir1 govstab1 socecon1 invest1 inconflict1 exconflict1 corrupt1 military1 religious1 law1 ethnic1 democracy1 bureau1 debt_vol1 fiscap_vol1 lfiscap_vol1)(gs debt fiscap lfiscap polcon trade inf GDP manu nare TAL ERI FRI PRI CRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau debt_vol fiscap_vol lfiscap_vol)
//Determinants of betaGS_OLS_b
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_OLS_b polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_OLS_b polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N p r2), using tableGS_OLS_b.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------**----------------------------*

* Determinants of betaVAT_PW, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaVAT_PW inf, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaVAT_PW inf `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table6a.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*
* Determinants of betaVAT_OLS, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaVAT_OLS inf, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaVAT_OLS inf `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table6b.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*


* Determinants of betaPIT_PW, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaPIT_PW gs GDP, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaPIT_PW gs GDP `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table7a.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*
* Determinants of betaPIT_OLS, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaPIT_OLS gs GDP trade inf, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaPIT_OLS gs GDP trade inf `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table7b.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*


* Determinants of betaCIT_PW, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaCIT_PW gs GDP, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaCIT_PW gs GDP `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table8a.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*
* Determinants of betaCIT_OLS, sample period 1960-2016
clear all
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaCIT_OLS gs GDP trade, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaCIT_OLS gs GDP trade `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table8b.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*


* SWF and CRI impacts on betaGS_PW
// AT ALL TIMES
clear all
use fiscyl60
eststo: qui reg betaGS_PW polcon inf trade TAL SWF fiscap c.SWF#c.fiscap CRI c.SWF#c.CRI [aw=arGDP], vce(robust)
eststo: qui reg betaGS_PW polcon inf trade TAL SWF lfiscap c.SWF#c.lfiscap CRI c.SWF#c.CRI [aw=arGDP], vce(robust)
esttab *, b(3) se(3) stats(N p r2), using table12l.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*
// AT GOOD TIMES
clear all
use DATA60
replace D =. if dlRGDP ==.
keep if D == 0 // good times
joinby iso year using realGDP2010US, unm(both)
keep if _m==3
bysort iso: egen arGDP_g=mean(rGDP)

local varlist GS DEBT FISCAP LFISCAP POLCON TRADE INF GDPR MANU NARE TAL_GDP ERIR FRIR PRIR CRIR GOVSTAB SOCECON INVEST INCONFLICT EXCONFLICT CORRUPT MILITARY RELIGIOUS LAW ETHNIC DEMOCRACY BUREAU
foreach var in `varlist' {
	egen `var'1 = mean(`var'), by(iso)
	}
bysort iso: egen DEBT_VOL1 = sd(DEBT)
bysort iso: egen FISCAP_VOL1 = sd(FISCAP)
bysort iso: egen LFISCAP_VOL1 = sd(LFISCAP)

keep *1 iso arGDP_g
drop iso1
duplicates drop
rename *, lower
joinby iso using fiscyl60, unm(both)

keep if _m==3
keep iso countryname *1 betaGS_PW_g argdp_g SWF
rename (gs1 debt1 fiscap1 lfiscap1 polcon1 trade1 inf1 gdpr1 manu1 nare1 tal_gdp1 erir1 frir1 prir1 crir1 govstab1 socecon1 invest1 inconflict1 exconflict1 corrupt1 military1 religious1 law1 ethnic1 democracy1 bureau1 debt_vol1 fiscap_vol1 lfiscap_vol1)(gs debt fiscap lfiscap polcon trade inf GDP manu nare TAL ERI FRI PRI CRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau debt_vol fiscap_vol lfiscap_vol)

eststo: qui reg betaGS_PW_g polcon inf trade TAL SWF fiscap c.SWF#c.fiscap CRI c.SWF#c.CRI [aw=argdp_g], vce(robust)
eststo: qui reg betaGS_PW_g polcon inf trade TAL SWF lfiscap c.SWF#c.lfiscap CRI c.SWF#c.CRI [aw=argdp_g], vce(robust)
esttab *, b(3) se(3) stats(N p r2), using table12r.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------**----------------------------*




/////////////////////////////
*** PANEL REGRESSION
*----------------------------*
* Spending cyclicality OECD vs. non-OECD countries (the left part), sample period 1960-2016
clear all
use DATA60
eststo: qui reg dlRGS dlRGDP if OECDgroup=="OECD", robust
eststo: qui xtreg dlRGS dlRGDP if OECDgroup=="OECD", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if OECDgroup=="OECD", fe vce(robust)
eststo: qui reg dlRGS dlRGDP if OECDgroup=="non-OECD", robust
eststo: qui xtreg dlRGS dlRGDP if OECDgroup=="non-OECD", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if OECDgroup=="non-OECD", fe vce(robust)
esttab, b(3) se(3) stats(N p), using table9l_govspOECD.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)
*----------------------------*
* Spending cyclicality OECD vs. non-OECD countries (the right part), sample period 1980-2016
clear all
use DATA80
eststo: qui reg dlRGS dlRGDP if OECDgroup=="OECD", robust
eststo: qui xtreg dlRGS dlRGDP if OECDgroup=="OECD", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if OECDgroup=="OECD", fe vce(robust)
eststo: qui reg dlRGS dlRGDP if OECDgroup=="non-OECD", robust
eststo: qui xtreg dlRGS dlRGDP if OECDgroup=="non-OECD", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if OECDgroup=="non-OECD", fe vce(robust)
esttab, b(3) se(3) stats(N p), using table9r_govspOECD.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)
*----------------------------*
* Tax cyclicality OECD vs. non-OECD countries
clear all
use DATA60
local indepvars VAT PIT CIT
foreach var in `indepvars' {
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="OECD", fe vce(robust)
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="OECD", re vce(robust)
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="non-OECD", fe vce(robust)
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="non-OECD", re vce(robust)
	}
esttab, b(3) se(3) stats(N p), using table9b_taxOECD.csv, append star(* 0.10 ** 0.05 *** 0.01)
*----------------------------*


* Spending cyclicality by income level, sample period 1960-2016
clear all
use DATA60
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="High income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="High income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Upper middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Upper middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Lower middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Lower middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Low income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Low income", fe vce(robust)
esttab, b(3) se(3) stats(N p r2_w), using table10a_govspbyincome.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)
*----------------------------*
* Spending cyclicality by income level, sample period 1980-2016
clear all
use DATA80
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="High income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="High income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Upper middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Upper middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Lower middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Lower middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Low income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Low income", fe vce(robust)
esttab, b(3) se(3)stats(N p r2_w), using table10b__govspbyincome.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)
*----------------------------*


* Spending cyclicality by sub-period, sample period 1960-2016
clear all
use DATA60
eststo: qui xtreg dlRGS dlRGDP if year<=1971, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year<=1971, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=1972&year<=1980, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=1972&year<=1980, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=1981&year<=1989, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=1981&year<=1989, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=1990&year<=1998, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=1990&year<=1998, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=1999&year<=2007, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=1999&year<=2007, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=2008, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=2008, fe vce(robust)
esttab, b(3) se(3) stats(N p r2_w), using table11a_govspbytime.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)
*----------------------------*
* Spending cyclicality by sub-period, sample period 1980-2016
clear all
use DATA80
eststo: qui xtreg dlRGS dlRGDP if year<=1989, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year<=1989, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=1990&year<=1998, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=1990&year<=1998, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=1999&year<=2007, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=1999&year<=2007, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year<=2007, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year<=2007, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if year>=2008, fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if year>=2008, fe vce(robust)
esttab, b(3) se(3) stats(N p r2_w), using table11b_govspbytime.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)
*----------------------------*



