/*
1946 Cohort (NSHD)
*/
use "${bann}/46c/46c_ht_cog.dta", clear
merge 1:1 nshdid_db1120  using "${bann}/46c/magels_fagels.dta", nogen

// Age
gen age_11 = date57c/12 if inrange(date57c, 128, 137)
gen age_15 = date61c/12 if inrange(date61c, 172, 182)

foreach age in 11 15{
	gen age_anth_`age' = age_`age'
	gen age_cog_`age' = age_`age'
}
drop age_??


// Sex
gen male = 2 - sex


// Anthropometrics
foreach var of varlist ht57u ht61u bmi57u bmi61u{
	local year = substr("`var'", -3, 2)
	local age = `year' - 46	
	local stub = cond(substr("`var'", 1, 2) == "ht", "height", "bmi")
	
	recode `var' (7777/9999 = .), gen(`stub'_`age')
}

gen_anthro, a(11 15)


// Cognition
* Maths
gen maths_11 = a11r if a11r != 99
gen maths_15 = m15r if m15r != 99

* Verbal
gen verbal_11 = v11r57 if v11r57 >= 0

* Reading
gen vocab_15 = r15r if r15r != 99

* Age Adjusted
gen_residuals maths_11 maths_15 verbal_11 vocab_15


// Parental Anthropometrics
gen mother_height = mht52 * 2.54 if mht52 >= 0
gen father_height = fht52 * 2.54 if fht52 >= 0
gen parent_height = mother_height < 140 | father_height < 140
replace mother_height = . if mother_height < 140
replace father_height = . if father_height < 140


// Parental Education
foreach parent in mother father{
	local p = substr("`parent'", 1, 1)
	
	gen `parent'_edu_years = .
	replace `parent'_edu_years = `p'agels - 9 if inrange(`p'agels, 10, 19)
	replace `parent'_edu_years = 10 if inrange(`p'agels, 20, 30)
	
	gen `parent'_edu_level = inrange(`p'agels, 15, 30) ///
		if inrange(`p'agels, 10, 30)
}
label define parent_edu_level 0 "Low" 1 "High"
label values *_edu_level parent_edu_level


// Father's Social Class
foreach var of varlist fsc50 fsc57{
	local suffix = cond("`var'" == "fsc50", "_alt", "")
	
	recode `var' 	(50 = 1 "V Unskilled") (40 = 2 "IV Partly Skilled") ///
					(35 = 3 "III Skilled Manual") (30 = 4 "III Skilled Non-Manual") ///
					(20 = 5 "II Intermediate") (10 = 6 "I Professional") ///
					(2 = .) (60 = .), gen(father_class`suffix')
}


// Unit Tests



// Format Dataset
egen total_weight = total(inf)
gen survey_weight = inf * _N / total_weight
tostring nshdid_db1120, gen(id)
gen cohort = "1946c"
keep id cohort survey_weight male ///
	age_* height_* bmi_* ///
	maths_* verbal_* vocab_* ///
	father_* mother_* parent_height
compress
save "${clean}/1946c_cleaned.dta", replace