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
	Why not father's social class from CLOSER?
Notes:
	Add variable that is conjuction of father_class 00 and 10 for 1970c
*/

clear
cls
set linesize 120

* Directories
global raw 		"S:/IOEQSS_Main/Bann/crosscinequality"
global clean 	"D:/Projects/Cognition and Height/Data"

* Programs
// ssc install egenmore
// ssc install wridit
// net install dm0004_1.pkg // zanthro


/*
1946 Cohort (NSHD)
*/
use "${raw}/46c/46c_ht_cog.dta", clear
merge 1:1 nshdid_db1120  using "${raw}/46c/magels_fagels.dta", nogen

rename nshdid_db1120 id

// Parental Anthropometrics
foreach parent in mother father{
	local p = substr("`parent'", 1, 1)
	
	gen `parent'_height = `p'ht52 * 0.0254 if `p'ht52 >= 0
	replace `parent'_height = . if `parent'_height < 1.4
}

// Parental Education
foreach parent in mother father{
	local p = substr("`parent'", 1, 1)
	
	gen `parent'_edu_years = .
	replace `parent'_edu_years = `p'agels - 9 if inrange(`p'agels, 10, 19)
	replace `parent'_edu_years = 10 if inrange(`p'agels, 20, 30)
	
	gen `parent'_edu_level = inrange(`p'agels, 15, 30) ///
		if inrange(`p'agels, 10, 30)
		
	gen `parent'_edu_type = `p'ed if inrange(`p'ed, 1, 8)
}
label define parent_edu_level 0 "Low" 1 "High"
label values *_edu_level parent_edu_level

// Father's Social Class
foreach var of varlist fsc50 fsc57{
	local age = cond("`var'" == "fsc50", "04", "11")
	
	recode `var' 	(50 = 1 "V Unskilled") (40 = 2 "IV Partly Skilled") ///
					(35 = 3 "III Skilled Manual") (30 = 4 "III Skilled Non-Manual") ///
					(20 = 5 "II Intermediate") (10 = 6 "I Professional") ///
					(2 = .) (60 = .), gen(father_class_`age')
}

// Sex, Cohort, Age
gen male = 2 - sex

gen survey_weight_00 = inf

gen cohort = 1

gen age_11 = date57c/12 if inrange(date57c, 128, 137)
gen age_15 = date61c/12 if inrange(date61c, 172, 182)

// Anthropometrics
foreach var of varlist ht57u ht61u ht89u bmi57u bmi61u bmi89u{
	local year = substr("`var'", -3, 2)
	local age = `year' - 46	
	local stub = cond(substr("`var'", 1, 2) == "ht", "height", "bmi")
	
	recode `var' (7777/9999 = .), gen(`stub'_`age')
}

foreach age in 11 15{
	egen height_`age'_z = zanthro(height_`age', ha, UK), ///
		xvar(age_`age') gender(male)  gencode(male = 1, female = 0) 
		
	egen bmi_`age'_z = zanthro(bmi_`age', ba, UK), ///
		xvar(age_`age') gender(male)  gencode(male = 1, female = 0) 
}

// Cognition
* Maths
gen maths_11 = a11r if a11r != 99
gen maths_15 = m15r if m15r != 99

* Verbal
gen verbal_11 = v11r57 if v11r57 >= 0

* Reading
gen vocab_15 = r15r if r15r != 99

* Age Adjusted
foreach var of varlist maths_11 maths_15 verbal_11 vocab_15{
	local age = substr("`var'", -2, .)
		
	regress `var' age_`age' // Verbal and Maths and Age are anti-correlated at age 11
	predict `var'_resid, residuals	
}

// Format Dataset
keep id cohort survey_weight_00 male ///
	age_* height_* bmi_* maths_* verbal_* vocab_* father_* mother_*
compress
save "${clean}/46c_cleaned.dta", replace


/*
1958 Cohort (NCDS)
*/
use "${raw}/58c/derived/bmi_7_50_closer.dta", clear
merge m:1 ncdsid using "${raw}/58c/0, 7, 11, 16y/ncds0123.dta", ///
	keepusing(n236 n190 n1687 n2928 n1196 n1199 n1202 n1205 n926 n2930) ///
	nogen
merge m:1 ncdsid using "${raw}/58c/derived/NCDS age 11 to 16.dta", nogen
merge m:1 ncdsid using ///
	"${raw}/derived/closer_education/ncds_ParentEducationDerived.dta", nogen
	
rename ncdsid id
 
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

// Parental Education
gen mother_edu_years = n16med if inrange(n16med, 1, 10)
gen father_edu_years = n16fed if inrange(n16fed, 1, 10)

gen mother_edu_level = n016nmed if inrange(n016nmed, 0, 1)
gen father_edu_level = n716daded if inrange(n716daded, 0, 1)
label define parent_edu_level 0 "Low" 1 "High"
label values *_edu_level parent_edu_level

// Father's Social Class
recode n236 	(7 = 1 "V Unskilled") (6 = 2 "IV Partly Skilled") ///
				(5 = 3 "III Skilled Manual") (4 = 4 "III Skilled Non-Manual") ///
				(3 = 5 "II Intermediate") (2 = 6 "I Professional") ///
				(1 = .) (-1 = .), gen(father_class_00)

recode n1687 	(6 = 1 "V Unskilled") (5 = 2 "IV Partly Skilled") ///
				(4 = 3 "III Skilled Manual") (3 = 4 "III Skilled Non-Manual") ///
				(2 = 5 "II Intermediate") (1 = 6 "I Professional") ///
				(-1 = .) (7 = .), gen(father_class_11)


// Sex, Cohort, Age
gen male = 1 - sex

gen cohort = 2

foreach var of varlist xage*{
	local age = substr("`var'", 5, .)
	if `age' < 10	local age 0`age'
	
	gen age_`age' = `var'
}

// Anthropometrics
foreach var of varlist ht11 ht16 ht42 ht44 bmi11 bmi16 bmi42 bmi44{
	local age = substr("`var'", -2, .)
	local stub = cond(substr("`var'", 1, 2) == "ht", "height", "bmi")
	
	gen `stub'_`age' = `var'
	if "`stub'" == "height" replace `stub'_`age' = `stub'_`age' * 100
}

foreach age in 11 16 42 44{
	local age_c = cond(`age' >= 23, "age_23", "age_`age'")
	
	egen height_`age'_z = zanthro(height_`age', ha, UK), ///
		xvar(`age_c') gender(male) gencode(male = 1, female = 0) 
		
	egen bmi_`age'_z = zanthro(bmi_`age', ba, UK), ///
		xvar(`age_c') gender(male) gencode(male = 1, female = 0) 
}


// Cognition
* Maths
gen maths_11 = n926 if n926 >= 0
gen maths_16 = n2930 if n2930 >= 0

* Verbal
gen verbal_11 = nvdsvabil // WHERE ARE THESE FROM?

* Reading
gen vocab_16 = ncds_rcomp

* Age Adjusted
foreach var of varlist maths_11 maths_16 verbal_11 vocab_16{
	local age = substr("`var'", -2, .)
		
	regress `var' age_`age'
	predict `var'_resid, residuals	
}
 
// Format Dataset
keep id cohort male age_* height_* bmi_* maths_* verbal_* vocab_* father_* mother_*
compress
save "${clean}/58c_cleaned.dta", replace 

 
/*
1970 Cohort (BCS70)
*/
capture program drop merge_bcs
program define merge_bcs
	args id file vlist
	
	qui replace bcsid = BCSID if missing(bcsid)
	qui replace BCSID = bcsid if missing(BCSID)
	merge 1:1 `id' using "${raw}/70c/`file'", ///
		nogen keepusing(`vlist')
	
end

use "${raw}/70c/10y/bcs3derived.dta" if !missing(BD3PSOC), clear
gen bcsid = BCSID
merge_bcs BCSID "birth and 22m subsample/bcs1derived.dta"
merge_bcs BCSID "birth and 22m subsample/bcs7072a.dta" ///
	"A0014 A0018"
merge_bcs BCSID "5y/bcs2derived.dta"
merge_bcs bcsid "5y/f699b.dta" ///
	"e131 e190 e220 e228a e228b e228c e228d e228e e229 e230 e231 e232 e233 e234 e235"
merge_bcs bcsid "10y/sn3723.dta" ///
	"examdata examdatb examdatc doba10 dobb10 dobc10 e1_1 e1_2 e2_1 e2_2 c3_4 c3_11"
merge_bcs BCSID "derived/bmi_wt_ht0_42.dta"
merge_bcs bcsid "16y/bcs7016x.dta" "*_mt *_yr bversion e1-e13"	
merge_bcs bcsid "derived/bcs70_16-year_arithmetic_data.dta" ///
	"mathscore"
merge_bcs BCSID "34y/bcs7derived.dta"
merge_bcs BCSID "46y/ukds/bcs_age46_main.dta" ///
	"BD10BMI BD10MBMI B10BFPC B10HEIGHTCM"
merge_bcs bcsid "derived/bcs cognition 5 to 16.dta"
merge 1:1 bcsid using ///
	"${raw}\derived\closer_education\bcs70_ParentEducationDerived.dta", nogen
	
qui replace bcsid = BCSID if missing(bcsid)
qui replace BCSID = bcsid if missing(BCSID)
rename bcsid id

 
// Parental Anthropometrics
gen mother_height = e1_1 / 100 if e1_1 >= 140
gen father_height = e2_1 / 100 if e2_1 >= 140

gen mother_weight =  e1_2 if e1_2 >= 0 // Minimum is 29kg!
gen father_weight =  e2_2 if e2_2 >= 0

gen mother_bmi = mother_weight / (mother_height^2)
gen father_bmi = father_weight / (mother_height^2)

// Parental Education
gen mother_edu_years = b016mmed if inrange(b016mmed, 1, 10)
gen father_edu_years = b016ffed if inrange(b016ffed, 1, 10)

gen mother_edu_level = b05med if inrange(b05med, 0, 1) // WHY NATURAL?
gen father_edu_level = b5fed2 if inrange(b5fed2, 0, 1)
label define parent_edu_level 0 "Low" 1 "High"
label values *_edu_level parent_edu_level

// Father's Social Class
recode A0014 	(1 = 1 "V Unskilled") (2 = 2 "IV Partly Skilled") ///
				(4 = 3 "III Skilled Manual") (3 = 4 "III Skilled Non-Manual") ///
				(5 = 5 "II Intermediate") (6 = 6 "I Professional") ///
				(min/-1 = .) (7/max = .), gen(father_class_00)

recode c3_4 	(1 = 1 "V Unskilled") (2 = 2 "IV Partly Skilled") ///
				(4 = 3 "III Skilled Manual") (3 = 4 "III Skilled Non-Manual") ///
				(5 = 5 "II Intermediate") (6 = 6 "I Professional") ///
				(min/-1 = .) (7/max = .), gen(father_class_10)		
				
// Sex, Cohort, Age
gen male = 1 - sex

gen cohort = 3

gen age_10 = xage10
gen age_16 = xage16
gen age_23 = 23

// Anthropometrics
foreach age in 10 16 42{
	gen height_`age' = ht`age' * 100
	gen bmi_`age' = bmi`age'
}

foreach age in 10 16 42{
	local age_c = cond(`age' >= 23, "age_23", "age_`age'")
	
	egen height_`age'_z = zanthro(height_`age', ha, UK), ///
		xvar(`age_c') gender(male) gencode(male = 1, female = 0) 
		
	egen bmi_`age'_z = zanthro(bmi_`age', ba, UK), ///
		xvar(`age_c') gender(male) gencode(male = 1, female = 0) 
}

gen age_10_cog = BCSage10/12

gen age_16_cog = .
foreach x in f g h l p q r t  {
	replace age_16_cog = ym(`x'doc_yr + 1900, `x'doc_mt) ///
		if `x'doc_yr > 0 & `x'doc_mt > 0 & missing(age_16_cog)
}
replace age_16_cog = (age_16_cog - ym(1970, 4))/12

// Cognition
* Maths
gen maths_10 = BD3MATHS if BD3MATHS >= 0
gen maths_16 = mathscore if mathscore >= 0

* Verbal
gen verbal_10 = sim10_G if sim10_G >= 0

* Reading
gen vocab_16 = voc16_20 if voc16_20 >= 0

gen comp_16 = 0
foreach x of numlist 1/13{
	replace comp_16 = comp_16 + 1 if e`x' == 1
	replace comp_16 = . if !inlist(e`x', -2, 1, 2)
}

* Age Adjusted
gen home_test = bversion if bversion >= 0
foreach var of varlist maths_10 maths_16 verbal_10 vocab_16 comp_16{
	local age = substr("`var'", -2, .)
	local test = cond(`age' == 16, "i.home_test", "")
		
	regress `var' age_`age'_cog `test'
	predict `var'_resid, residuals	
}
 
// Format Dataset
drop if  BD1CNTRY == 4 // Drop Northern Ireland
keep id cohort male age_* height_* bmi_* ///
	maths_* verbal_* vocab_* comp_* home_test father_* mother_*
compress 
save "${clean}/70c_cleaned.dta", replace 

 
/*
2001 Cohort (MCS)
*/
capture program drop fix_cnum
program define fix_cnum
	qui foreach x in A B C D E F G{
		replace cnum00 = `x'CNUM00 if !missing(`x'CNUM00) & missing(cnum00)
	}
	qui foreach x in A B C D E F G{
		replace `x'CNUM00 = cnum00 if missing(`x'CNUM00) & !missing(cnum00)
	}
end

capture program drop merge_mcs
program define merge_mcs
	args join file vlist
	
	merge `join' using "${raw}/mcs/`file'", nogen keepusing(`vlist')
	fix_cnum
end


use "${raw}/mcs/derived/mcs_bmi_ses_closer v001.dta", clear
gen MCSID = subinstr(mcsid, ".", "",.)
drop mcsid

gen cnum00 = 1
foreach x in A B C D E F G{
	gen `x'CNUM00 = 1
}
fix_cnum

merge_mcs "1:1 MCSID BCNUM00" "3y/mcs2_cm_derived.dta" "BDC06E00"
merge_mcs "1:1 MCSID DCNUM00" "7y/mcs4_cm_cognitive_assessment.dta" "DCMATHS7SC DAGEDY000"
merge_mcs "1:1 MCSID GCNUM00" "17y/W2/mcs7_cm_derived.dta" "GCMCS7AG GCBMIN7"
merge_mcs "1:1 MCSID GCNUM00" "17y/W3/mcs7_cm_interview.dta" "GCHTCM00"
merge_mcs "1:1 MCSID GCNUM00" "17y/W4/mcs7_cm_cognitive_assessment.dta" "GCNAAS0A-GCNAAS0J"

merge_mcs "m:1 MCSID" "17y/mcs_longitudinal_family_file.dta" "GOVWT2"
merge_mcs "m:1 MCSID" "0y/mcs1_derived_variables.dta"

rename MCSID, lower
gen cnum = cnum00
merge_mcs "1:1 mcsid cnum" "derived/MCS cognition 5 to 14.dta"
merge m:1 mcsid using "${raw}/derived/closer_education/mcs_ParentEducationDerived.dta" 
merge_mcs "m:1 mcsid" "0y/mcs_longitudinal_family_file.dta" "country"

rename mcsid id
 
// Parental Anthropometrics
clonevar father_household = ADFINH00

gen mother_height = ADMHGT00 if ADMHGT00 >= 1.4
gen father_height = APHGTM00 if APHGTM00 >= 1.4

gen mother_bmi = ADMBMI00 if ADMBMI00 > 0
gen father_bmi = APDBMIA0 if APDBMIA0 > 0

// Parental Education
gen mother_edu_years = mmled if inrange(mmled, 1, 10)
gen father_edu_years = mfled if inrange(mfled, 1, 10)

gen mother_edu_level = mcs_mledg - 1 if inrange(mcs_mledg, 1, 2)
gen father_edu_level = mcs_fledg - 1 if inrange(mcs_fledg, 1, 2)
label define parent_edu_level 0 "Low" 1 "High"
label values *_edu_level parent_edu_level

// Father's Social Class
recode csep 	(5 = 1 "V Unskilled") (4 = 2 "IV Partly Skilled") ///
				(3 = 3 "III Skilled Manual") (2 = 4 "III Skilled Non-Manual") ///
				(1 = 5 "II Intermediate") (0 = 6 "I Professional"), ///
				gen(father_class_10)

// Sex, Cohort, Age
gen male = sex

gen survey_weight_17 = GOVWT2

gen cohort = 4  

gen age_07 = DAGEDY000 / 365.25
gen age_10 = MCSage10 / 12
gen age_14 = MCSage14 / 12
gen age_17 = GCMCS7AG if GCMCS7AG >= 0

// Anthropometrics
gen height_10 = ht11 * 100
gen height_14 = ht14 * 100
gen height_17 = GCHTCM00 if GCHTCM00 > 0

gen bmi_10 = bmi11
gen bmi_14 = bmi14
gen bmi_17 = GCBMIN7 if GCBMIN7 > 0

foreach age in 10 14 17{
	egen height_`age'_z = zanthro(height_`age', ha, UK), ///
		xvar(age_`age') gender(male) gencode(male = 1, female = 0) 
		
	egen bmi_`age'_z = zanthro(bmi_`age', ba, UK), ///
		xvar(age_`age') gender(male) gencode(male = 1, female = 0) 
}

// Cognition
* Maths
gen maths_07 = DCMATHS7SC if DCMATHS7SC >= 0

gen maths_17 = 0
local correct 5 1 3 4 1 5 4 4 2 5
forval i = 1/10{
	local answer: word `i' of `correct'
	local J: word `i' of `c(ALPHA)'
	
	replace maths_17 = maths_17 + 1 if GCNAAS0`J' == `answer'
	replace maths_17 = . if !inrange(GCNAAS0`J', 1, 6)
} 

* Verbal
gen verbal_10 = EVSABIL

* Reading
gen vocab_14 = voc14_20

* Age Adjusted
foreach var of varlist maths_07 maths_17 verbal_10 vocab_14{
	local age = substr("`var'", -2, .)
		
	regress `var' age_`age'
	predict `var'_resid, residuals	
}
 
// Format Dataset 
drop if country == 4
keep id cnum00 cohort survey_weight_17 male age_* height_* bmi_* ///
	maths_* verbal_* vocab_*
compress 
save "${clean}/mcs_cleaned.dta", replace


