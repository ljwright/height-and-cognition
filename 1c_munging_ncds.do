/*
1958 Cohort (NCDS)
*/
use "${raw}/NCDS/0y-16y/ncds0123.dta", clear

merge 1:1 ncdsid using ///
	"${bann}/derived/closer_education/ncds_ParentEducationDerived.dta", ///
	nogen keepusing(n16med n16fed n016nmed n716daded)
	

// Age
gen_age age_anth_11 n910 n911 `=ym(1958, 3)'
gen_age age_anth_16 n2925 n2927 `=ym(1958, 3)'

gen age_cog_11 = age_anth_11
gen age_cog_16 = age_anth_16


// Sex
gen male = 2 - n622  if inrange(n622, 1, 2)

	
// Anthropometrics
foreach age in 11 16{
    gen height_`age' = dvht`age' * 100 if dvht`age' >= 0
	gen weight_`age' = dvwt`age' if dvwt`age' >= 0
}

gen_bmi, a(11 16)
gen_anthro, a(11 16)


// Cognition
* Maths
gen maths_11 = n926 if n926 >= 0
gen maths_16 = n2930 if n2930 >= 0

* Verbal
gen verbal_11 = n914 if n914 >= 0 // nvdsvabil

* Reading
gen vocab_16 = n2928 if n2928 >= 0 // ncds_rcomp

* Age Adjusted
gen_residuals maths_11 maths_16 verbal_11 vocab_16


// Parental Anthropometrics
gen mother_height = n1205 * 2.54 if n1205 >= 0
gen father_height = n1199 * 2.54 if n1199 >= 0
gen parent_height = mother_height < 140 | father_height < 140
replace mother_height = . if mother_height < 140
replace father_height = . if father_height < 140

foreach var of varlist n1202 n1196{
	local parent = cond("`var'" == "n1202", "mother", "father")
	
	gen `parent'_weight = (6 * 6.35029) + ((`var' - 12) * 0.5 * 6.35029) ///
		if `var' >= 0
}

gen mother_bmi = mother_weight / (mother_height ^ 2)
gen father_bmi = father_weight / (father_height ^ 2)


// Parental Education
gen mother_edu_years = n16med if inrange(n16med, 1, 10)
gen father_edu_years = n16fed if inrange(n16fed, 1, 10)

gen mother_edu_level = n016nmed if inrange(n016nmed, 0, 1)
gen father_edu_level = n716daded if inrange(n716daded, 0, 1)
label define parent_edu_level 0 "Low" 1 "High"
label values *_edu_level parent_edu_level


// Father's Social Class
gen father_class_alt = 8 - n236 if inrange(n236, 2, 7)
gen father_class = 7 - n1687 if inrange(n1687, 1, 6)

label_class


// Unit Tests


			
// Format Dataset
keep if n1811 == 0 // SINGLETON BIRTHS

rename ncdsid id
gen cohort = "1958c"
gen survey_weight = 1
keep id cohort survey_weight male ///
	age_* height_* bmi_* ///
	maths_* verbal_* vocab_* ///
	father_* mother_* parent_height
compress
save "${clean}/1958c_cleaned.dta", replace