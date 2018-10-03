***** betaGS_PW (government-spending cyclicality by Prais-Winsten)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170 {
	capture noisily {
               qui prais dlRGS dlRGDP if iso1==`i'
			   replace coef=_b[dlRGDP] if iso1==`i'
			   replace se=_se[dlRGDP] if iso1==`i'
			   replace t=_b[dlRGDP]/_se[dlRGDP] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP]/_se[dlRGDP]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaGS_PW, replace
*------------------*
***** betaGS_PW_g (government-spending cyclicality by Prais-Winsten at good times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170 {
	capture noisily {
               qui prais dlRGS dlRGDP if iso1==`i' & D==0
			   replace coef=_b[dlRGDP] if iso1==`i'
			   replace se=_se[dlRGDP] if iso1==`i'
			   replace t=_b[dlRGDP]/_se[dlRGDP] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP]/_se[dlRGDP]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaGS_PW_g, replace
*------------------*
***** betaGS_PW_b (government-spending cyclicality by Prais-Winsten at bad times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170 {
	capture noisily {
               qui prais dlRGS dlRGDP if iso1==`i' & D==1
			   replace coef=_b[dlRGDP] if iso1==`i'
			   replace se=_se[dlRGDP] if iso1==`i'
			   replace t=_b[dlRGDP]/_se[dlRGDP] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP]/_se[dlRGDP]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaGS_PW_b, replace
*------------------**------------------*
***** betaGS_OLS (government-spending cyclicality by OLS)
clear all
use data
sort iso year
parmby "qui reg dlRGS dlRGDP, vce(robust)", by(iso) label saving(betaGS_OLS,replace)

use betaGS_OLS, clear
keep if parm=="dlRGDP"
keep iso estimate stderr t p
save betaGS_OLS, replace
*------------------*
***** betaGS_OLS_g (government-spending cyclicality by OLS at good times)
clear all
use data
sort iso year
parmby "qui reg dlRGS dlRGDP if D==0, vce(robust)", by(iso) label saving(betaGS_OLS_g,replace)

use betaGS_OLS_g, clear
keep if parm=="dlRGDP"
keep iso estimate stderr t p
save betaGS_OLS_g, replace
*------------------*
***** betaGS_OLS_b (government-spending cyclicality by OLS at bad times)
clear all
use data
sort iso year
parmby "qui reg dlRGS dlRGDP if D==1, vce(robust)", by(iso) label saving(betaGS_OLS_b,replace)

use betaGS_OLS_b, clear
keep if parm=="dlRGDP"
keep iso estimate stderr t p
save betaGS_OLS_b, replace
*------------------* *------------------*
***** betaVAT_PW (VAT cyclicality by Prais-Winsten)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais VAT dlRGDP100 if iso1==`i'
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaVAT_PW, replace
*------------------*
***** betaVAT_PW_g (VAT cyclicality by Prais-Winsten at good times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais VAT dlRGDP100 if iso1==`i' & D==0
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaVAT_PW_g, replace
*------------------*
***** betaVAT_PW_b (VAT cyclicality by Prais-Winsten at bad times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais VAT dlRGDP100 if iso1==`i' & D==1
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaVAT_PW_b, replace
*------------------**------------------*

***** betaVAT_OLS (VAT cyclicality by OLS)
clear all
use data
sort iso1 year
parmby "qui reg VAT dlRGDP100, vce(robust)", by(iso) label saving(betaVAT_OLS,replace)

use betaVAT_OLS, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaVAT_OLS, replace
*------------------*
***** betaVAT_OLS_g (VAT cyclicality by OLS at good times)
clear all
use data
sort iso1 year
parmby "qui reg VAT dlRGDP100 if D==0, vce(robust)", by(iso) label saving(betaVAT_OLS_g,replace)

use betaVAT_OLS_g, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaVAT_OLS_g, replace
*------------------*
***** betaVAT_OLS_b (VAT cyclicality by OLS at bad times)
clear all
use data
sort iso1 year
parmby "qui reg VAT dlRGDP100 if D==1, vce(robust)", by(iso) label saving(betaVAT_OLS_b,replace)

use betaVAT_OLS_b, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaVAT_OLS_b, replace
*------------------**------------------*
***** betaPIT_PW (PIT cyclicality by Prais-Winsten)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais PIT dlRGDP100 if iso1==`i'
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaPIT_PW, replace
*------------------*
***** betaPIT_PW_g (PIT cyclicality by Prais-Winsten at good times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais PIT dlRGDP100 if iso1==`i' & D==0
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaPIT_PW_g, replace
*------------------*
***** betaPIT_PW_b (PIT cyclicality by Prais-Winsten at bad times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais PIT dlRGDP100 if iso1==`i' & D==1
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaPIT_PW_b, replace
*------------------**------------------*
***** betaPIT_OLS (PIT cyclicality by OLS)
clear all
use data
sort iso1 year
parmby "qui reg PIT dlRGDP100, vce(robust)", by(iso) label saving(betaPIT_OLS,replace)

use betaPIT_OLS, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaPIT_OLS, replace
*------------------*
***** betaPIT_OLS_g (PIT cyclicality by OLS at good times)
clear all
use data
sort iso1 year
parmby "qui reg PIT dlRGDP100 if D==0, vce(robust)", by(iso) label saving(betaPIT_OLS_g,replace)

use betaPIT_OLS_g, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaPIT_OLS_g, replace
*------------------*
***** betaPIT_OLS_b (PIT cyclicality by OLS at bad times)
clear all
use data
sort iso1 year
parmby "qui reg PIT dlRGDP100 if D==1, vce(robust)", by(iso) label saving(betaPIT_OLS_b,replace)

use betaPIT_OLS_b, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaPIT_OLS_b, replace
*------------------**------------------*
***** betaCIT_PW (CIT cyclicality by Prais-Winsten)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais CIT dlRGDP100 if iso1==`i'
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaCIT_PW, replace
*------------------*
***** betaCIT_PW_g (CIT cyclicality by Prais-Winsten at good times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais CIT dlRGDP100 if iso1==`i' & D==0
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaCIT_PW_g, replace
*------------------*
***** betaCIT_PW_b (CIT cyclicality by Prais-Winsten at bad times)
clear all
use data
sort iso1 year
gen coef=.
gen se=.
gen t=.
gen p=.

forvalues i=1/170{
	capture noisily {
               qui prais CIT dlRGDP100 if iso1==`i' & D==1
			   replace coef=_b[dlRGDP100] if iso1==`i'
			   replace se=_se[dlRGDP100] if iso1==`i'
			   replace t=_b[dlRGDP100]/_se[dlRGDP100] if iso1==`i'
			   replace p=2*ttail(e(df_r),abs((_b[dlRGDP100]/_se[dlRGDP100]))) if iso1==`i'
			     }
				 }
keep iso coef se t p
duplicates drop
save betaCIT_PW_b, replace
*------------------**------------------*
***** betaCIT_OLS (CIT cyclicality by OLS)
clear all
use data
sort iso1 year
parmby "qui reg CIT dlRGDP100, vce(robust)", by(iso) label saving(betaCIT_OLS,replace)

use betaCIT_OLS, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaCIT_OLS, replace
*------------------*
***** betaCIT_OLS_g (CIT cyclicality by OLS at good times)
clear all
use data
sort iso1 year
parmby "qui reg CIT dlRGDP100 if D==0, vce(robust)", by(iso) label saving(betaCIT_OLS_g,replace)

use betaCIT_OLS_g, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaCIT_OLS_g, replace
*------------------*
***** betaCIT_OLS_g (CIT cyclicality by OLS at bad times)
clear all
use data
sort iso1 year
parmby "qui reg CIT dlRGDP100 if D==1, vce(robust)", by(iso) label saving(betaCIT_OLS_b,replace)

use betaCIT_OLS_b, clear
keep if parm=="dlRGDP100"
keep iso estimate stderr t p
save betaCIT_OLS_b, replace
*------------------**------------------**------------------*


*** merge all into allbeta.dta
clear all
use data
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
drop _merge se t p
rename coef betaVAT_PW
label var betaVAT_PW "estimated VAT cyclicality by Prais-Winsten"
*
joinby iso using betaVAT_OLS, unm(both)
drop _merge stderr t p
rename estimate betaVAT_OLS
label var betaVAT_OLS "estimated VAT cyclicality by OLS"
*
joinby iso using betaPIT_PW, unm(both)
drop _merge se t p
rename coef betaPIT_PW
label var betaPIT_PW "estimated PIT cyclicality by Prais-Winsten"
*
joinby iso using betaPIT_OLS, unm(both)
drop _merge stderr t p
rename estimate betaPIT_OLS
label var betaPIT_OLS "estimated PIT cyclicality by OLS"
*
joinby iso using betaCIT_PW, unm(both)
drop _merge se t p
rename coef betaCIT_PW
label var betaCIT_PW "estimated CIT cyclicality by Prais-Winsten"
*
joinby iso using betaCIT_OLS, unm(both)
drop _merge stderr t p
rename estimate betaCIT_OLS
label var betaCIT_OLS "estimated CIT cyclicality by OLS"
*
joinby iso using betaVAT_PW_g, unm(both)
drop _merge se t p
rename coef betaVAT_PW_g
label var betaVAT_PW_g "estimated VAT cyclicality by Prais-Winsten at good times"
*
joinby iso using betaVAT_PW_b, unm(both)
drop _merge se t p
rename coef betaVAT_PW_b
label var betaVAT_PW_b "estimated VAT cyclicality by Prais-Winsten at bad times"
*
joinby iso using betaVAT_OLS_g, unm(both)
drop _merge stderr t p
rename estimate betaVAT_OLS_g
label var betaVAT_OLS_g "estimated VAT cyclicality by OLS at good times"
*
joinby iso using betaVAT_OLS_b, unm(both)
drop _merge stderr t p
rename estimate betaVAT_OLS_b
label var betaVAT_OLS_b "estimated VAT cyclicality by OLS at bad times"
*
joinby iso using betaPIT_PW_g, unm(both)
drop _merge se t p
rename coef betaPIT_PW_g
label var betaPIT_PW_g "estimated PIT cyclicality by Prais-Winsten at good times"
*
joinby iso using betaPIT_PW_b, unm(both)
drop _merge se t p
rename coef betaPIT_PW_b
label var betaPIT_PW_b "estimated PIT cyclicality by Prais-Winsten at bad times"
*
joinby iso using betaPIT_OLS_g, unm(both)
drop _merge stderr t p
rename estimate betaPIT_OLS_g
label var betaPIT_OLS_g "estimated PIT cyclicality by OLS at good times"
*
joinby iso using betaPIT_OLS_b, unm(both)
drop _merge stderr t p
rename estimate betaPIT_OLS_b
label var betaPIT_OLS_b "estimated PIT cyclicality by OLS at bad times"
*
joinby iso using betaCIT_PW_g, unm(both)
drop _merge se t p
rename coef betaCIT_PW_g
label var betaCIT_PW_g "estimated CIT cyclicality by Prais-Winsten at good times"
*
joinby iso using betaCIT_PW_b, unm(both)
drop _merge se t p
rename coef betaCIT_PW_b
label var betaCIT_PW_b "estimated CIT cyclicality by Prais-Winsten at bad times"
*
joinby iso using betaCIT_OLS_g, unm(both)
drop _merge stderr t p
rename estimate betaCIT_OLS_g
label var betaCIT_OLS_g "estimated CIT cyclicality by OLS at good times"
*
joinby iso using betaCIT_OLS_b, unm(both)
drop _merge stderr t p
rename estimate betaCIT_OLS_b
label var betaCIT_OLS_b "estimated CIT cyclicality by OLS at bad times"
*
save fiscyl, replace
