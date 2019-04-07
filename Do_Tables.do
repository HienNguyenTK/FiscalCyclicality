************ DATA ON INTERNATIONAL COUNTRY RISK GUIDE REFERRED IN THE CODES BELOW ARE NOT AVAILABLE BECAUSE OF COPYRIGHT





************ PANEL REGRESSION
******** Table 2: Fiscal behaviour of government spending of OECD and non-OECD, 1960–2016
use DATA60
eststo: qui reg dlRGS dlRGDP if OECDgroup=="OECD", robust
eststo: qui xtreg dlRGS dlRGDP if OECDgroup=="OECD", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if OECDgroup=="OECD", fe vce(robust)
eststo: qui reg dlRGS dlRGDP if OECDgroup=="non-OECD", robust
eststo: qui xtreg dlRGS dlRGDP if OECDgroup=="non-OECD", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if OECDgroup=="non-OECD", fe vce(robust)
esttab, b(3) se(3) stats(N p), using table2.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)


******** Table 3: Fiscal behaviour of tax rates of OECD and non-OECD, 1960–2016
use DATA60
local indepvars VAT PIT CIT
foreach var in `indepvars' {
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="OECD", fe vce(robust)
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="OECD", re vce(robust)
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="non-OECD", fe vce(robust)
	eststo: qui xtreg `var' dlRGDP100 if OECDgroup=="non-OECD", re vce(robust)
	}
esttab, b(3) se(3) stats(N p), using table3.csv, append star(* 0.10 ** 0.05 *** 0.01)


******** Table 4: Fiscal behaviour of government spending by income level, 1960–2016
use DATA60
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="High income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="High income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Upper middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Upper middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Lower middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Lower middle income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP if incomegroup=="Low income", fe vce(robust)
eststo: qui xtreg dlRGS dlRGDP i.year if incomegroup=="Low income", fe vce(robust)
esttab, b(3) se(3) stats(N p r2_w), using table4.csv, append star(* 0.10 ** 0.05 *** 0.01) keep(dlRGDP _cons)



************ CROSS-COUNTRY REGRESSION, 1960-2016
************ (apply similarly for data 1980-2016 by using fiscyl80)
******** Table 6: Cross-country regression of government-spending cyclicality using Prais-Winsten estimates, 1960–2016
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_PW polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_PW polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table6.csv, append star(* 0.10 ** 0.05 *** 0.01)


******** Table 7 : Cross-country regression of government-spending cyclicality using Prais-Winsten estimates, 1960–2016
******** (Apply similarly for Table 10 1980-2016 by using fiscyl80)
******** Model 1: 1st step by Prais–Winsten, 2nd step by WLS
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_PW polcon inf trade TAL gs [aw=betaGS_PW_rse], vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_PW polcon inf trade TAL gs `var' [aw=betaGS_PW_rse], vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table71.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Model 2: 1st step and 2nd step by OLS with robust standard errors.
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_OLS polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_OLS polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table72.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Model 3: 1st step by OLS with Newey-West standard errors and AR(1), 2nd step by WLS
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_NWSE1 polcon inf trade TAL gs [aw=betaGS_NWSE1_rse], vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_NWSE1 polcon inf trade TAL gs `var' [aw=betaGS_NWSE1_rse], vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table73.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Model 4: 1st step by OLS with Newey-West standard errors and AR(2), 2nd step by WLS
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_NWSE2 polcon inf trade TAL gs [aw=betaGS_NWSE2_rse], vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_NWSE2 polcon inf trade TAL gs `var' [aw=betaGS_NWSE2_rse], vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table74.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Model 5: 1st step by TSLS (excluded instrument is lag.SHOCKGL), 2nd step by WLS
******** (apply similarly for models 6-8 using the instruments stated in the paper)
use fiscyl60
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_2SLS_1 polcon inf trade TAL gs [aw=betaGS_2SLS_rse_1] if iso!="USA", vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_2SLS_1 polcon inf trade TAL gs `var' [aw=betaGS_2SLS_rse_1] if iso!="USA", vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table75.csv, append star(* 0.10 ** 0.05 *** 0.01)


******** Table 8: Cross-country regressions on government-spending cyclicality at good times versus bad times, 1960–2016
****** Create mean data for good times
use DATA60
replace D =. if dlRGDP ==.
keep if D == 0	//good times
fillin iso year
local varlist GS DEBT FISCAP LFISCAP POLCON TRADE INF MANU NARE TAL_GDP ERIR FRIR PRIR CRIR GOVSTAB SOCECON INVEST INCONFLICT EXCONFLICT CORRUPT MILITARY RELIGIOUS LAW ETHNIC DEMOCRACY BUREAU
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
keep iso countryname *1 betaGS_PW_g betaGS_PW_g_rse betaGS_OLS_g betaGS_OLS_g_rse
rename (gs1 debt1 fiscap1 lfiscap1 polcon1 trade1 inf1 manu1 nare1 tal_gdp1 erir1 frir1 prir1 crir1 govstab1 socecon1 invest1 inconflict1 exconflict1 corrupt1 military1 religious1 law1 ethnic1 democracy1 bureau1 debt_vol1 fiscap_vol1 lfiscap_vol1)(gs debt fiscap lfiscap polcon trade inf manu nare TAL ERI FRI PRI CRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau debt_vol fiscap_vol lfiscap_vol)
******** Column 1: 1st step by Prais–Winsten, 2nd step by OLS with robust standard errors.
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_PW_g polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_PW_g polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table81.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Column 2: 1st step & 2nd step by OLS with robust standard errors.
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_OLS_g polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_OLS_g polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N r2 p), using table82.csv, append star(* 0.10 ** 0.05 *** 0.01)

****** Create mean data for bad times
use DATA60
replace D =. if dlRGDP ==.
keep if D == 1
fillin iso year
local varlist GS DEBT FISCAP LFISCAP POLCON TRADE INF MANU NARE TAL_GDP ERIR FRIR PRIR CRIR GOVSTAB SOCECON INVEST INCONFLICT EXCONFLICT CORRUPT MILITARY RELIGIOUS LAW ETHNIC DEMOCRACY BUREAU
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
keep iso countryname *1 betaGS_OLS_b betaGS_OLS_b_rse
rename (gs1 debt1 fiscap1 lfiscap1 polcon1 trade1 inf1 manu1 nare1 tal_gdp1 erir1 frir1 prir1 crir1 govstab1 socecon1 invest1 inconflict1 exconflict1 corrupt1 military1 religious1 law1 ethnic1 democracy1 bureau1 debt_vol1 fiscap_vol1 lfiscap_vol1)(gs debt fiscap lfiscap polcon trade inf manu nare TAL ERI FRI PRI CRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau debt_vol fiscap_vol lfiscap_vol)
******** Column 3: 1st step & 2nd step by OLS with robust standard errors.
local indepvars fiscap fiscap_vol lfiscap lfiscap_vol debt debt_vol nare manu CRI ERI FRI PRI govstab socecon invest inconflict exconflict corrupt military religious law ethnic democracy bureau
eststo: qui reg betaGS_OLS_b polcon inf trade TAL gs, vce(robust)
foreach var in `indepvars'{
eststo: qui reg betaGS_OLS_b polcon inf trade TAL gs `var', vce(robust)
}
esttab *, b(3) se(3) stats(N p r2), using table83.csv, append star(* 0.10 ** 0.05 *** 0.01)


******** Table 9: SWF and government-spending cyclicality.
******** Column 1: Full sample
use fiscyl60
eststo: qui reg betaGS_PW polcon inf trade TAL SWF fiscap c.SWF#c.fiscap CRI c.SWF#c.CRI [aw=arGDP], vce(robust)
eststo: qui reg betaGS_PW polcon inf trade TAL SWF lfiscap c.SWF#c.lfiscap CRI c.SWF#c.CRI [aw=arGDP], vce(robust)
esttab *, b(3) se(3) stats(N p r2), using table91.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Column 1: Full sample
use fiscyl60
eststo: qui reg betaGS_PW polcon inf trade TAL SWF fiscap c.SWF#c.fiscap CRI c.SWF#c.CRI [aw=betaGS_PW_rse], vce(robust)
eststo: qui reg betaGS_PW polcon inf trade TAL SWF lfiscap c.SWF#c.lfiscap CRI c.SWF#c.CRI [aw=betaGS_PW_rse], vce(robust)
esttab *, b(3) se(3) stats(N p r2), using table92_WLS.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Good-times mean variables
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
******** Column 3: Good-times sample
eststo: qui reg betaGS_PW_g polcon inf trade TAL SWF fiscap c.SWF#c.fiscap CRI c.SWF#c.CRI [aw=argdp_g], vce(robust)
eststo: qui reg betaGS_PW_g polcon inf trade TAL SWF lfiscap c.SWF#c.lfiscap CRI c.SWF#c.CRI [aw=argdp_g], vce(robust)
esttab *, b(3) se(3) stats(N p r2), using table93.csv, append star(* 0.10 ** 0.05 *** 0.01)
******** Column 4: Good-times sample
eststo: qui reg betaGS_PW_g polcon inf trade TAL SWF fiscap c.SWF#c.fiscap CRI c.SWF#c.CRI [aw=betaGS_PW_g_rse], vce(robust)
eststo: qui reg betaGS_PW_g polcon inf trade TAL SWF lfiscap c.SWF#c.lfiscap CRI c.SWF#c.CRI [aw=betaGS_PW_g_rse], vce(robust)
esttab *, b(3) se(3) stats(N p r2), using table94_WLS.csv, append star(* 0.10 ** 0.05 *** 0.01)
**********************EndOfCodes************************
****** NOTE: Use corresponding variables in fiscyl60 to estimate the determinants of tax-rate cyclicality
