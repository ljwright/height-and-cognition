*main analyses file for bann et al cognition height paper
*appends 1946, 1958, 1970, 2001 (MCS) cohort data and analyses/outputs relevant results

*set directories
*C
global data "c:\users\dab\onedrive - university college london\stats_cls\cohorts"
global output "C:\Users\DAB\onedrive - university college london\cross-cohort\cog\analysis\output"
*S:
global data "S:\IOEQSS_Main\Bann\crosscinequality"
global output  "S:\IOEQSS_Main\Bann\crosscinequality\cognition"


**appends and analyses data - unimputed
*46c
use "$output\46c_cog_anthro_cleaned.dta", clear
*58c
append using  "$output\58c_cog_anthro_cleaned.dta"
*70c
append using  "$output\bcs70_cog_anthro_cleaned.dta"

*mcs
append using "$output\mcs_cog_anthro_cleaned.dta" 

*create ridit scores/z scores
foreach var of varlist vsim10  vsim10aa  maths10 maths10aa fsc10 voc16 voc16aa maths16 maths16aa medb medy ht43 {
cap drop `var'_ridit
by cohort, sort: egen `var'_ridit = ridit(`var')
cap drop z`var'
by cohort, sort: egen z`var'= std(`var')
}

**additional restrictions to aid cross-cohort comparability
*drop if eth ==1  //drop 'other', but could actually explain cohort difference by generation so use in sens analyses
*drop if ideath==1 & !missing(ideath) //drop if died in infancy; 4%

*tab bmulti cohort //singleton births, for now not dropping + need identify variable
*tab core sid if age==10, mi
*drop if bmulti >0 & !missing(bmulti) //drop multi births, yet 4%, and they do exist in older cohorts
*drop if core==1 & !missing(core) //drop if not part original cohort; 4% and again may help attrition in older cohorts

*response outcomes, seperately and for both 
gen outcome10 = .
replace outcome10 = 1 if !missing(zht10)
gen outcome16 = . 
replace outcome16 = 1 if !missing(zht16)

*save pooled data
save "$output\cog_ht_ses_pht_46_mcs.dta", replace
  

*a word to stop the syntax running through if one mistakenly clicks ctrl+d !
*stop

*check ns by cohort
*for those with valid height data
*ideally do same for cognition - but those with missing cognition (exposure) data can be imputed subsequently 
tab outcome10 cohort, mi col //crude response rates range from 73% (46 cohort) to 61% (MCS) - so not much difference really
tab outcome16 cohort, mi col //crude response rates range from 68% (46 cohort) to 52% (MCS)

*use imputed data [eventually once analyses finalised]	
*use "$output/cog_ht_bmi_clean_imputed_v01.dta", clear

*table 1
*ns for table 1
tab outcome10 cohort
tab outcome16 cohort

*Table 1  //ssc install table1_mc
tab fsc10
tab fsc10, nolab
cap drop fsc10b
recode fsc10 (0/2=0 "manual") (3/5=1 "non manual"), gen(fsc10b)
tab fsc10b fsc10, mi

gen mhtcm = mht*100
gen phtcm = pht*100

cap erase "$output/table 1z.xlsx"
table1_mc , by(cohort) ///
vars( ///
sex bin %4.1f \ ///
zht11  contn %4.1f \ ///
zht16  contn %4.1f \ ///
vsim10 contn   %4.1f \ ///
maths10 contn %4.1f \ ///
voc16 contn  %4.1f \ ///
maths16  contn %4.1f \ ///
fsc10b bin %4.1f \ ///
medb bin %4.1f \ ///
mhtcm contn %4.1f \ ///
phtcm contn %4.1f \ ///
) ///
nospace percent  ///
saving("$output/table 1z.xlsx", replace)

*table 2
*additional stats for cognition measures (sadly need to copy/paste!)
tabstat vsim10, by(cohort) stats(mean, min,  max, sd, cv)
tabstat maths10, by(cohort) stats(mean, min,  max, sd, cv)
tabstat voc16, by(cohort) stats(mean, min,  max, sd, cv)
tabstat maths16, by(cohort) stats(mean, min,  max, sd, cv)


*correlations
*Supplementary table 1; output matrix
cap erase "$output/correlation_matrix.xlsx"

foreach cohort in 0 1 2 3 {
putexcel set  "$output\correlation_matrix.xlsx", modify sheet(`cohort')
spearman zht11 zht16 vsim10aa maths10aa   voc16aa  maths16aa fsc10 medb mht pht if cohort==`cohort', pw
return list
matrix list r(Rho)
putexcel A1=matrix(r(Rho)), names nformat(number_d2)
putexcel save
}

*sensitivity analysis assuming more missing data in older cohorts to see if weakens assocaitions 
pwcorr vsim10aa maths10aa  voc16aa  maths16aa if cohort==0
pwcorr vsim10aa maths10aa  voc16aa  maths16aa if cohort==0 & zvsim10aa >0.1 //correlations are weaker, but this is with an unfeasible amount of additional missing data 
																			//+still higher than MCS

pwcorr vsim10aa maths10aa  voc16aa  maths16aa if cohort==3 

*scatter matrix
graph matrix  vsim10 maths10 voc16  maths16 if cohort==0
graph export "$output/matrix_1946c.tif", replace  

graph matrix  vsim10 maths10 voc16  maths16 if cohort==3
graph export "$output/matrix_2001c.tif", replace  




*fig 1 - regressions of cognition->height
*10y --note throughout that fsc0 more strongly assocaited
*check works both cohorts reg zbmi10 zht10 i.cohort vsim10aa_ridit fsc10_ridit
cap erase "$output/10y_16y_estimates.csv"
*zht10
foreach outcome of varlist  zht10   {
foreach exposure of varlist vsim10aa_ridit  maths10aa_ridit {
foreach cohort in 0 1 2 3 {
eststo: mi estimate, post : reg `outcome' `exposure' i.sex if cohort==`cohort' & outcome10 == 1 
est store `outcome'_`exposure'_`cohort'
esttab  `outcome'_`exposure'_`cohort'  using "$output/10y_16y_estimates.csv", ///
cells("b ci_l ci_u")  stats() modelwidth(20) ///
plain nolabel nogaps varwidth (15) nolines  compress append   nolabel  ///
mlabels("`outcome'") collabels("") nocons keep(`exposure') noobs 
}
}
}

*16y
foreach outcome of varlist  zht16  {
foreach exposure of varlist  voc16aa_ridit maths16aa_ridit  {
foreach cohort in 0 1 2 3 {
eststo: mi estimate, post : reg `outcome' `exposure' i.sex if cohort==`cohort' & outcome16 == 1 
est store `outcome'_`exposure'_`cohort'
esttab  `outcome'_`exposure'_`cohort'   using "$output/10y_16y_estimates.csv", ///
cells("b ci_l ci_u")  stats() modelwidth(20) ///
plain nolabel nogaps varwidth (15) nolines  compress append   nolabel  ///
mlabels(`outcome') collabels("") nocons keep(`exposure') noobs 
}
}
}			

	
***adjusted for SES
*10y --note throughout that fsc0 more strongly assocaited
*check works both cohorts reg zbmi10 zht10 i.cohort vsim10aa_ridit fsc10_ridit
cap erase "$output/10y_16y_estimates_fsc.csv"
*zht10
foreach outcome of varlist  zht10   {
foreach exposure of varlist vsim10aa_ridit  maths10aa_ridit  {
foreach cohort in 0 1 2 3 {
reg `outcome' `exposure' i.sex i.fsc10 i.medb if cohort==`cohort' & !missing(fsc10_ridit, vsim10aa_ridit) // no association
est store `outcome'_`exposure'_`cohort'
esttab    using "$output/10y_16y_estimates_fsc.csv", ///
cells("b ci_l ci_u")  stats() modelwidth(20) ///
plain nolabel nogaps varwidth (15) nolines  compress append   nolabel  ///
mlabels("`outcome'") collabels("") nocons keep(`exposure') noobs 
}
}
}

*16y

 foreach outcome of varlist  zht16  {
foreach exposure of varlist  voc16aa_ridit maths16aa_ridit  {
foreach cohort in 0 1 2 3 {
reg `outcome' `exposure' i.sex i.fsc10 i.medb if cohort==`cohort' & !missing(fsc10_ridit, voc16aa_ridit ) // no association
est store `outcome'_`exposure'_`cohort'
esttab    using "$output/10y_16y_estimates_fsc.csv", ///
cells("b ci_l ci_u")  stats() modelwidth(20) ///
plain nolabel nogaps varwidth (15) nolines  compress append   nolabel  ///
mlabels(`outcome') collabels("") nocons keep(`exposure') noobs 
}
}
}	


*10y --note throughout that fsc0 more strongly assocaited
*check works both cohorts reg zbmi10 zht10 i.cohort vsim10aa_ridit fsc10_ridit
cap erase "$output/10y_16y_estimates_fsc_pht.csv"
*zht10
foreach outcome of varlist  zht10   {
foreach exposure of varlist vsim10aa_ridit  maths10aa_ridit  {
foreach cohort in 0 1 2 3 {
reg `outcome' `exposure' i.sex i.fsc10 mht pht if cohort==`cohort' & !missing(fsc10_ridit, vsim10aa_ridit) // no association
est store `outcome'_`exposure'_`cohort'
esttab    using "$output/10y_16y_estimates_fsc_pht.csv", ///
cells("b ci_l ci_u")  stats() modelwidth(20) ///
plain nolabel nogaps varwidth (15) nolines  compress append   nolabel  ///
mlabels("`outcome'") collabels("") nocons keep(`exposure') noobs 
}
}
}

*16y

 foreach outcome of varlist  zht16  {
foreach exposure of varlist  voc16aa_ridit maths16aa_ridit  {
foreach cohort in 0 1 2 3 {
reg `outcome' `exposure' i.sex i.fsc10  mht pht if cohort==`cohort' & !missing(fsc10_ridit, voc16aa_ridit ) // no association
est store `outcome'_`exposure'_`cohort'
esttab    using "$output/10y_16y_estimates_fsc_pht.csv", ///
cells("b ci_l ci_u")  stats() modelwidth(20) ///
plain nolabel nogaps varwidth (15) nolines  compress append   nolabel  ///
mlabels(`outcome') collabels("") nocons keep(`exposure') noobs 
}
}
}	

***quantile regression results 
**10y
*ht
foreach x in 0 1 2 3 {
local quantiles 5 10 25 50 75 90 95 // K quantiles that you care about
local models ""                             // names of K quantile models for coefplot to graph 
local xlabel ""                             // for x-axis labels
local j=1                                   // counter for quantiles
foreach q of numlist `quantiles' {
    qreg zht10 vsim10aa_ridit sex if cohort==`x' , quantile(`q')
    estimates store me_tu`q'
    local models `"`models' me_tu`q' || "'
    local xlabel `"`xlabel' `j++' "Q{sub:`q'}""'
}
di "`models'
di `"`xlabel'"'
coefplot `models' /// 
, vertical bycoefs  ///
xlab(none) xlabel(`xlabel', add) ///
ytitle("SDS height difference") ylabel(-.4(.2)1, angle(horiz)) drop(_cons sex *cohort*) ///
ciopts(recast(rcap))  graphregion(color(white) ) yline(0, lcolor(gs8) lpattern(dash)) title("")

graph export "$output/qreg_ht10_cohort_`x'.tif", replace  
}

**16y
*ht
foreach x in 0 1 2 3 {
local quantiles 5 10 25 50 75 90 95 // K quantiles that you care about
local models ""                             // names of K quantile models for coefplot to graph 
local xlabel ""                             // for x-axis labels
local j=1                                   // counter for quantiles
foreach q of numlist `quantiles' {
    qreg zht16 voc16aa_ridit sex if cohort==`x' , quantile(`q')
    estimates store me_tu`q'
    local models `"`models' me_tu`q' || "'
    local xlabel `"`xlabel' `j++' "Q{sub:`q'}""'
}
di "`models'
di `"`xlabel'"'
coefplot `models' /// 
, vertical bycoefs  ///
xlab(none) xlabel(`xlabel', add) ///
ytitle("SDS height difference") ylabel(-.4(.2)1, angle(horiz)) drop(_cons sex *cohort*) ///
ciopts(recast(rcap))  graphregion(color(white) ) yline(0, lcolor(gs8) lpattern(dash)) title("")

graph export "$output/qreg_ht16_cohort_`x'.tif", replace  
}

***



