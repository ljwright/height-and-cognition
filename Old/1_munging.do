/*
Data Munging for
	Weakening of the cognition and height association from 1957 to 2018: 
	Findings from four British birth cohort studies 
Bann et al. (2021)

Cleans and derives variables for the 1946, 1958, 1970 and 2001 (MCS) cohorts, separately.
Files are appended together in 2_mice.R, where variables are also renamed to account for different measurement ages

Questions:
	Can't audit code if built on derived variables with no code trail
	1946c Verbal and Maths and Age are anti-correlated at age 11
	Minimum mother weight in 1970c is 29kg (4.5 stone)!
	Why changed between natural and all mothers for education variables in 1970c?
	Parental qualification type not labelled correctly for 1970c
	Check coding of social_class_42 in 1970c
	Why own SEP measures in 1970c?
	What are the age 10s in 1970c?
	Mistake in age16_cof (1970c) is constructed
	Mistake in for first father SEP coded for 1970c
	comp_16 (1970c) incorrect in original because rowtotal includes missing
	Do we want to drop Northern Ireland in 1970c?
	Why no 2 person households in CLOSER MCS file?
	Are ?CNUM00 in MCS consistent?
	parent_edu_type not consistent
	I get different numbers of observations for mcs!
	Need to add at same time to get same variables (recoding of sex mcs weird)
	Check social class variables in correct direction
Notes:
	Add variable that is conjuction of father_class 00 and 10 for 1970c
*/

clear
cls
set linesize 180

* Directories
global bann 	"S:/IOEQSS_Main/Bann/crosscinequality"
global raw		"D:"
global clean 	"D:/Projects/Cognition and Height/Data"

* Programs
// ssc install egenmore
// ssc install wridit
// net install dm0004_1.pkg // zanthro

/*
CLOSER BMI, Sex, and Height Data
*/
use "${raw}/CLOSER/ncds_closer_wp1.dta", clear
append using "${raw}/CLOSER/bcs70_closer_wp1.dta"
append using "${raw}/CLOSER/mcs_closer_wp1.dta"

gen id = "", before(closerid)
replace id = ncdsid if stid == "NCDS"
replace id = bcsid if stid == "BCS70"
replace id = mcsid if stid == "MCS"

rename stid study
rename xage 	age_int
rename visitage	age_wave
rename ht		height
rename wt		weight
keep id study sex age_* height weight bmi

// CHECK I HAVE CORRECT AGES FOR Z SCORES; SEE IF AGE CAN DIFFER OVER Z SCORE
gen height_cm = height * 100
gen male = 2 - sex
gen age_z = cond(age_int >= 23, 23, age_int)

egen height_z = zanthro(height, ha, UK), ///
		xvar(age_z) gender(male) gencode(male = 1, female = 0) 
egen bmi_z = zanthro(bmi, ba, UK), ///
		xvar(age_z) gender(male) gencode(male = 1, female = 0)
drop height_cm
		
compress
save "${clean}/closer_anthropometrics.dta", replace

/*
Combine Cohorts
	TO DO: 	AGE ADJUSTED COGNITION BY COHORT
			RESHAPE ALL TO LONG
*/
* Age Adjusted
foreach var of varlist maths_11 maths_16 verbal_11 vocab_16{
	local age = substr("`var'", -2, .)
	local test = cond(`age' == 16, "i.home_test", "") // FOR BCS70
	
	regress `var' age_`age'
	predict `var'_resid, residuals
}


/*
1946 Cohort (NSHD)
*/


/*
1958 Cohort (NCDS)
	* Parental Education: n16med, n16fed, n016nmed, n716daded
*/
use "${raw}/NCDS/0y-16y/ncds0123.dta", clear

// Parental Anthropometrics
gen mother_height = n1205 * 0.0254 if n1205 >= 0
replace mother_height = . if mother_height < 1.4

gen father_height = n1199 * 0.0254 if n1199 >= 0
replace father_height = . if father_height < 1.4

foreach var of varlist n1202 n1196{
	local parent = cond("`var'" == "n1202", "mother", "father")
	
	gen `parent'_weight = (6 * 6.35029) + ((`var' - 12) * 0.5 * 6.35029) ///
		if `var' >= 0
}

gen mother_bmi = mother_weight / (mother_height^2)
gen father_bmi = father_weight / (father_height^2)

// Father's Social Class
recode n236 	(7 = 1 "V Unskilled") (6 = 2 "IV Partly Skilled") ///
				(5 = 3 "III Skilled Manual") (4 = 4 "III Skilled Non-Manual") ///
				(3 = 5 "II Intermediate") (2 = 6 "I Professional") ///
				(1 = .) (-1 = .), gen(father_class_00)

recode n1687 	(6 = 1 "V Unskilled") (5 = 2 "IV Partly Skilled") ///
				(4 = 3 "III Skilled Manual") (3 = 4 "III Skilled Non-Manual") ///
				(2 = 5 "II Intermediate") (1 = 6 "I Professional") ///
				(-1 = .) (7 = .), gen(father_class_11)	

// Parental Education
// gen mother_edu_years = n16med if inrange(n16med, 1, 10)
// gen father_edu_years = n16fed if inrange(n16fed, 1, 10)
//
// gen mother_edu_level = n016nmed if inrange(n016nmed, 0, 1)
// gen father_edu_level = n716daded if inrange(n716daded, 0, 1)
// label define parent_edu_level 0 "Low" 1 "High"
// label values *_edu_level parent_edu_level

// Cognition
* Maths
gen maths_11 = n926 if n926 >= 0
gen maths_16 = n2930 if n2930 >= 0

// * Verbal
// gen verbal_11 = nvdsvabil
//
// * Reading
// gen vocab_16 = ncds_rcomp
 
// Format Dataset
// keep id cohort male age_* height_* bmi_* maths_* verbal_* vocab_* father_* mother_*
// compress
// save "${clean}/58c_cleaned.dta", replace


/*
1970 Cohort (BCS70)
*/
use BCSID BD3AGE BD3MATHS using "${raw}/BCS70/10y/bcs3derived.dta" ///
	if !missing(BCSID, BD3MATHS), clear
merge 1:1 BCSID using "${raw}/BCS70/0y/bcs1derived.dta", ///
	nogen keepusing(BD1CNTRY)
rename BCSID bcsid
merge 1:1 bcsid using "${raw}/BCS70/0y/bcs7072a.dta", ///
	nogen keepusing(a0014)
merge 1:1 bcsid using "${raw}/BCS70/10y/sn3723.dta", ///
	nogen keepusing(e1_1 e1_2 e2_1 e2_2 c3_4)
merge 1:1 bcsid using "${raw}/BCS70/16y/bcs70_16-year_arithmetic_data.dta", ///
	nogen keepusing(mathscore)
merge 1:1 bcsid using "${raw}/BCS70/16y/bcs7016x.dta", ///
	nogen keepusing(bversion e1-e13 *doc_mt *doc_yr)

// Parental Anthropometrics
gen mother_height = e1_1 / 100 if e1_1 >= 140
gen father_height = e2_1 / 100 if e2_1 >= 140

gen mother_weight =  e1_2 if e1_2 >= 0 // Minimum is 29kg!
gen father_weight =  e2_2 if e2_2 >= 0

gen mother_bmi = mother_weight / (mother_height^2)
gen father_bmi = father_weight / (mother_height^2)

// Father's Social Class
recode a0014 	(1 = 1 "V Unskilled") (2 = 2 "IV Partly Skilled") ///
				(4 = 3 "III Skilled Manual") (3 = 4 "III Skilled Non-Manual") ///
				(5 = 5 "II Intermediate") (6 = 6 "I Professional") ///
				(min/-1 = .) (7/max = .), gen(father_class_00)

recode c3_4 	(1 = 1 "V Unskilled") (2 = 2 "IV Partly Skilled") ///
				(4 = 3 "III Skilled Manual") (3 = 4 "III Skilled Non-Manual") ///
				(5 = 5 "II Intermediate") (6 = 6 "I Professional") ///
				(min/-1 = .) (7/max = .), gen(father_class_10)	

// Parental Education
// gen mother_edu_years = b016mmed if inrange(b016mmed, 1, 10)
// gen father_edu_years = b016ffed if inrange(b016ffed, 1, 10)
//
// gen mother_edu_level = b05med if inrange(b05med, 0, 1) // WHY NATURAL?
// gen father_edu_level = b5fed2 if inrange(b5fed2, 0, 1)
// label define parent_edu_level 0 "Low" 1 "High"
// label values *_edu_level parent_edu_level

// Cognition
// gen age_10_cog = BCSage10/12
//
gen age_cog_16 = .
foreach x in f g h l p q r t  {
	replace age_cog_16 = ym(`x'doc_yr + 1900, `x'doc_mt) ///
		if `x'doc_yr > 0 & `x'doc_mt > 0 & missing(age_cog_16)
}
replace age_cog_16 = (age_cog_16 - ym(1970, 4))/12

* Maths
gen maths_10 = BD3MATHS if BD3MATHS >= 0
gen maths_16 = mathscore if mathscore >= 0

// * Verbal
// gen verbal_10 = sim10_G if sim10_G >= 0
//
// * Reading
// gen vocab_16 = voc16_20 if voc16_20 >= 0

gen comp_16 = 0 // WHY JUST QUESTIONS 1-13?
foreach x of numlist 1/13{
	replace comp_16 = comp_16 + 1 if e`x' == 1
	replace comp_16 = . if !inlist(e`x', -2, 1, 2)
}
 
// Format Dataset
drop if  BD1CNTRY == 4 // Drop Northern Ireland
// keep id cohort male age_* height_* bmi_* ///
// 	maths_* verbal_* vocab_* comp_* home_test ///
// 	father_* mother_* family_income_10 social_class_42 education_34 // MISSING COG_5; LOTS OF BMI VARIABLES
// compress 
// save "${clean}/70c_cleaned.dta", replace 


/*
2001 Cohort (MCS)
	* To Add: Father's Class @ 0. APDBMIA0, APHGTM00
*/
use MCSID ADMHGT00 ADMBMI00 using "${raw}/mcs/0y/mcs1_family_derived.dta", clear

use MCSID DCNUM00 DAGEDY000 DCMATHS7SC "D:\MCS\7y\mcs4_cm_cognitive_assessment.dta"

use MCSID GCNUM00 GCMCS7AG using "${raw}/mcs/17y/mcs7_cm_derived.dta"
use MCSID GCNUM00 GCNAAS0A-GCNAAS0J

use MCSID GOVWT1 "${raw}/mcs/17y/mcs_longitudinal_family_file.dta"


// Parental Anthropometrics
clonevar father_household = ADFINH00

gen mother_height = ADMHGT00 if ADMHGT00 >= 1.4
// gen father_height = APHGTM00 if APHGTM00 >= 1.4

gen mother_bmi = ADMBMI00 if ADMBMI00 > 0
// gen father_bmi = APDBMIA0 if APDBMIA0 > 0

// Parental Education
// gen mother_edu_years = mmled if inrange(mmled, 1, 10)
// gen father_edu_years = mfled if inrange(mfled, 1, 10)
//
// gen mother_edu_level = mcs_mledg - 1 if inrange(mcs_mledg, 1, 2)
// gen father_edu_level = mcs_fledg - 1 if inrange(mcs_fledg, 1, 2)
// label define parent_edu_level 0 "Low" 1 "High"
// label values *_edu_level parent_edu_level

// Father's Social Class
// recode csep 	(5 = 1 "V Unskilled") (4 = 2 "IV Partly Skilled") ///
// 				(3 = 3 "III Skilled Manual") (2 = 4 "III Skilled Non-Manual") ///
// 				(1 = 5 "II Intermediate") (0 = 6 "I Professional"), ///
// 				gen(father_class_10)

// Cognition
gen age_7 = DAGEDY000 / 365.25
// gen age_10 = MCSage10 / 12  // EMCS5AGE
// gen age_14 = MCSage14 / 12
gen age_17 = GCMCS7AG if GCMCS7AG >= 0

* Maths
gen maths_7 = DCMATHS7SC if DCMATHS7SC >= 0

gen maths_17 = 0
local correct 5 1 3 4 1 5 4 4 2 5
forval i = 1/10{
	local answer: word `i' of `correct'
	local J: word `i' of `c(ALPHA)'
	
	replace maths_17 = maths_17 + 1 if GCNAAS0`J' == `answer'
	replace maths_17 = . if !inrange(GCNAAS0`J', 1, 6)
} 

// * Verbal
gen verbal_10 = EVSABIL

// * Reading
// gen vocab_14 = voc14_20
 
// Format Dataset 
gen survey_weight_17 = GOVWT1
// drop if country == 4
// keep id cohort survey_weight_00 male age_* height_* bmi_* ///
// 	maths_* verbal_* vocab_* comp_* home_test ///
// 	father_* mother_* // MISSING FATHER SOCIAL CLASS
// compress 
// save "${clean}/mcs_cleaned.dta", replace 
