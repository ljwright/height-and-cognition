**imputation models

global data "c:\users\dab\onedrive - university college london\stats_cls\cohorts"
global output "C:\Users\DAB\onedrive - university college london\cross-cohort\cog\analysis\output"

*46c
use "$output\46c_cog_anthro_cleaned.dta", clear

*58c
append using  "$output\58c_cog_anthro_cleaned.dta"

*70c
append using  "$output\bcs70_cog_anthro_cleaned.dta"

*mcs
append using "$output\mcs_cog_anthro_cleaned.dta" 

tab cohort

*which variables?
tabstat zht10 zbmi10 sex fsc10 mbmi pbmi mht pht vsim10aa voc16aa, by(cohort)

*run multiple imputation

**considerations - cohort-specific or cohort-pooled
**minor points - put interaction terms in model if they are being included (they likely aren't in exported models)
**modelling ridit terms as continious - imputed values fall out of range
***this may or may not affect the main paremeter of interest: unbiased main parameter estimates
**missing outcome data: results appear similar smed->BMI for complete case and multiply imputed 

cap mi unset //code has drafting purpose only
cap drop *_*_ //code has drafting purpose only (variables not present in source data)
set seed 1234
mi set wide

drop if missing(sex, cohort) //needed as otherwise missing ind vars - and stata requires some to have full data; of course don't impute these vars

*create relevant interaction terms
*cohort * ses
gen fsc10_cohort = fsc10*cohort
gen fsc10_sex = fsc10*sex

*cognition and cohort
foreach var of varlist  vsim10aa maths10aa voc16aa maths16aa {
gen `var'_cohort= `var'*cohort
}

sum *_cohort
				
*register variables to be imputed - ie, confounders, exposures (yes, per sterne: https://www.bmj.com/content/338/bmj.b2393)
mi register imputed  zht10 zbmi10  zht16 zbmi16 ht43 /// outcomes 
		sex fsc10 mbmi pbmi mht pht vsim10aa voc16aa maths10aa maths16aa  medb medy  fedb fedy /// exposures
						fsc10_cohort fsc10_sex vsim10aa_cohort  maths10aa_cohort  voc16aa_cohort  maths16aa_cohort //interaction terms 

							
			

							
*run imputation		
*[note perfect prediction error, when added parental vars and maternal ed]		
*redo again with continious maternal ed, then seperate (non imputed) with education attainment at a stretch
mi impute chained ///
(regress) zht10 zbmi10  zht16 zbmi16 mbmi pbmi ht43 mht pht vsim10aa maths10aa voc16aa maths16aa fsc10_cohort fsc10_sex vsim10aa_cohort  maths10aa_cohort  voc16aa_cohort  maths16aa_cohort ///
(ologit) fsc10  medy  fedy ///
(logit) medb fedb ///
= i.sex i.cohort, add(10) rseed(1234) replace // force augment
		//note these outcomes are included in the model, per sterne et al recommendations as they can be informative
		//but they are not imputed (ie, not imputing the dead back to life), mi analyses are restricted

***another consideration (found to not affect findings in BP BMI work, or to 2nd dp)
***is whether more optimal to include cohort interaction terms in imputation model; vastly slows creation of imputed dataset 
		
**create ridit scores in each imputed dataset...by cohort thus interpretation is from v bottom to v top in each
foreach outcome in fsc10 medb medy fedb fedy vsim10aa voc16aa maths16aa  maths10aa {
cap drop `outcome'_ridit*
mi  passive:  egen `outcome'_ridit0 = ridit(`outcome')	if cohort==0
mi  passive:  egen `outcome'_ridit1 = ridit(`outcome')	if cohort==1
mi  passive:  egen `outcome'_ridit2 = ridit(`outcome')	if cohort==2
mi  passive:  egen `outcome'_ridit3 = ridit(`outcome')	if cohort==3

mi  passive:  gen `outcome'_ridit = `outcome'_ridit0 if cohort==0
mi  passive:  replace `outcome'_ridit = `outcome'_ridit1 if cohort==1
mi  passive:  replace `outcome'_ridit = `outcome'_ridit2 if cohort==2
mi  passive:  replace `outcome'_ridit = `outcome'_ridit3 if cohort==3
}

*and, for good measure, z scores
foreach outcome in fsc10 medb medy fedb fedy vsim10aa voc16aa maths16aa  maths10aa {
	cap drop z`outcome'
cap drop `outcome'_z*
mi  passive:  egen `outcome'_z0 = std(`outcome')	if cohort==0
mi  passive:  egen `outcome'_z1 = std(`outcome')	if cohort==1
mi  passive:  egen `outcome'_z2 = std(`outcome')	if cohort==2
mi  passive:  egen `outcome'_z3 = std(`outcome')	if cohort==3

mi  passive:  gen z`outcome'_z0 = `outcome'_z0 if cohort==0
mi  passive:  replace z`outcome' = `outcome'_z1 if cohort==1
mi  passive:  replace z`outcome' = `outcome'_z2 if cohort==2
mi  passive:  replace z`outcome' = `outcome'_z3 if cohort==3
}
sum *z*

*response outcomes, seperately and for both 
gen outcome10 = .
replace outcome10 = 1 if !missing(zht10)
gen outcome16 = . 
replace outcome16 = 1 if !missing(zht16)
		
*generate complete outcome indicator, so mi analyses only for those with valid outcome data
gen outcome10_16 	   = 1 if !missing(zbmi16, zht16)
replace outcome10_16 = 0 if missing(zbmi16, zht16)

save "$output/cog_ht_bmi_clean_imputed_v01.dta", replace
		
use "$output/cog_ht_bmi_clean_imputed_v01.dta"

*generate complete outcome indicator, so mi analyses only for those with valid outcome data
gen coutcome = 1 if !missing(sbpt, dbpt, hyper)
replace coutcome = 0 if missing(sbpt, dbpt, hyper)

mi register regular coutcome 
tab cohort coutcome

		

********************************************************************************
*check imputed dataset
use "$output/cog_ht_bmi_clean_imputed_v01.dta", clear 

*check imputed data ranges are sensible [unclear if important if they are not - key is having unbiased assoc estimates]
**imputed data have _1 etc prefix  - distributions appear similar, and key point is association inference not distributions of variables
mi describe
sum *_ridit //distributions are not realistic when modelled linearly
				// https://www.theanalysisfactor.com/multiple-imputation-5-recent-findings-that-change-how-to-use-it/
				//You actually get better results by leaving the imputed values at impossible values, even though it’s counter-intuitive.

foreach var of varlist 	fsc smed sc40s /// exposures
						sbpt dbpt /// outcomes
						hyper bmi  {
						sum `var' _*_`var'
						}
						
						
						

*check results

mi estimate: reg zht16 voc16aa_z sex if cohort==0 & !missing(mht, pht)
mi estimate: reg zht16 voc16aa_z sex if cohort==1 & !missing(mht, pht)
mi estimate: reg zht16 voc16aa_z sex if cohort==2 & !missing(mht, pht)


reg zht16 voc16aa_z sex if cohort==0
reg zht16 voc16aa_z sex if cohort==1 
reg zht16 voc16aa_z sex if cohort==2 


reg zht10 voc16aa_z sex if cohort==0 & outcome10==1
reg zht10 voc16aa_z sex if cohort==1 & outcome10==1
reg zht10 voc16aa_z sex if cohort==2 & outcome10==1


mi estimate: reg zht10 voc16aa_ridit sex if cohort==0 & outcome10==1
mi estimate: reg zht10 voc16aa_ridit sex if cohort==1 & outcome10==1
mi estimate: reg zht10 voc16aa_ridit sex if cohort==2 & outcome10==1

tabstat voc16aa  , by(cohort) stats(min max mean )
tabstat voc16aa_z  , by(cohort) stats(min max mean )
tabstat voc16aa_ridit , by(cohort) stats(min max mean )

corr voc16aa_z voc16aa_ridit 
scatter voc16aa_z voc16aa_ridit 
scatter voc16aa_z voc16aa_ridit , by(cohort)

*check vars

tab medb
tab medb cohort

tab medy
tab medy cohort