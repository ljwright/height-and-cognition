/*
1970 Cohort (BCS70)
*/
use BCSID BD3AGE BD3MATHS using "${raw}/BCS70/10y/bcs3derived.dta" ///
	if !missing(BCSID, BD3MATHS), clear
	
merge 1:1 BCSID using "${raw}/BCS70/xwave/bcs70_response_1970-2016.dta", ///
	nogen keepusing(SEX COB MULTIPNO)

merge 1:1 BCSID using "${raw}/BCS70/0y/bcs1derived.dta", ///
	nogen keepusing(BD1CNTRY)

rename BCSID bcsid
merge 1:1 bcsid using "${raw}/BCS70/0y/bcs7072a.dta", ///
	nogen keepusing(a0014 a0248 a0197)

merge 1:1 bcsid using "${raw}/BCS70/10y/sn3723.dta", ///
	nogen keepusing(e1_1 e1_2 e2_1 e2_2 c3_4 ///
					meb17 meb19_1 examdatb examdatc ///
					i2503m i2503y i3576-i3616 i3503m i3503y)

merge 1:1 bcsid using "${raw}/BCS70/16y/bcs70_16-year_arithmetic_data.dta", ///
	nogen keepusing(mathscore)

merge 1:1 bcsid using "${raw}/BCS70/16y/bcs7016x.dta", ///
	nogen keepusing(bversion e1-e13 *doc_mt *doc_yr cvo* rd2_1 rd4_1 ha1_1 ha1_2)
	
merge 1:1 bcsid using ///
	"${bann}\derived\closer_education\bcs70_ParentEducationDerived.dta", ///
	nogen keepusing(b016mmed b016ffed b05med b5fed2)
	

// Age
* Medical Exam
gen_age age_anth_10 examdatb examdatc `=ym(1970, 4)'

gen_age age_anth_16 		rdoc_mt rdoc_yr `=ym(1970, 4)'
gen_age age_anth_16_self 	hdoc_mt hdoc_mt `=ym(1970, 4)'
replace age_anth_16 = age_anth_16_self if rd2_1 < 0 | missing(rd2_1)
drop age_anth_16_self

* Cognitive Assessments
gen_age age_cog_10_maths 	i2503m i2503y `=ym(1970, 4)'
gen_age age_cog_10_verbal 	i3503m i3503y `=ym(1970, 4)'

gen age_cog_16 = .	// NOT RIGHT
foreach x in f g h l p q r t  {
    gen_age xx `x'doc_mt `x'doc_yr  `=ym(1970, 4)'
	replace age_cog_16 = xx if missing(age_cog_16)
	drop xx
}


// Sex
gen male = 2 - SEX if inrange(SEX, 1, 2)


// Anthropometrics
gen height_10 = meb17 / 10 if meb17 >= 0
gen weight_10 = meb19_1 / 10 if meb19_1 >= 0

gen height_16 = rd2_1 * 100 if rd2_1 >= 0
replace height_16 = ha1_2 * 100 if missing(height_16) & ha1_2 >= 0

gen weight_16 = rd4_1 if rd4_1 >= 0
replace weight_16 = ha1_1 if missing(height_16) & ha1_1 >= 0

gen height_16_report = 0 if rd2_1 >= 0 & !missing(rd2_1)
replace height_16_report = 1 if !missing(height_16) & missing(height_16_report)

gen weight_16_report = 0 if rd4_1 >= 0 & !missing(rd4_1)
replace weight_16_report = 1 if !missing(weight_16) & missing(weight_16_report)

gen_bmi, a(10 16)
gen_anthro, a(10 16)


// Cognition
gen home_test = bversion if bversion >= 0

* Maths
gen maths_10 = BD3MATHS if BD3MATHS >= 0
gen maths_16 = mathscore if mathscore >= 0

* Verbal
gen verbal_10 = 0 // sim10_G if sim10_G >= 0
forval q = 3576(2)3616 {
    replace verbal_10 = verbal_10 + 1 if i`q' == 1
	replace verbal_10 = . if !inrange(i`q', 1, 9)
}

* Reading
local vocab_vars 	4 12 17 27 28 33 37 39 41 43 45 46 50 52 53 55 59 72 73 75
local correct		5 5  2  2  5  3  1  5  5  4  2  2  5  4  3  5  1  1  3  3  

gen vocab_16 = 0 // voc16_20 if voc16_20 >= 0
gen miss = 0
forval i = 1/20{
    local q: word `i' of `vocab_vars'
	local a: word `i' of `correct'
	
	replace vocab_16 = vocab_16 + 1 if cvo`q' == `a'
	replace miss = miss + 1 if !inrange(cvo`q', 1, 5)
}
replace vocab_16 = . if miss == 20

gen comp_16 = 0
foreach x of numlist 1/13{
	replace comp_16 = comp_16 + 1 if e`x' == 1
	replace comp_16 = . if !inlist(e`x', -2, 1, 2)
}

* Age Adjusted
gen age_cog_10 = age_cog_10_maths
gen_residuals maths_10

replace age_cog_10 = age_cog_10_verbal
gen_residuals verbal_10

drop age_cog_10

gen_residuals maths_16 vocab_16 comp_16, covars(bversion)


// Parental Anthropometrics
gen mother_height = e1_1 if e1_1 >= 140 // a0197 as alternative. Not 100% correlated.
gen father_height = e2_1 if e2_1 >= 140
gen parent_height = inrange(e1_1, 0, 139) | inrange(e2_1, 0, 139)

gen mother_weight =  e1_2 if e1_2 >= 0 // Minimum is 29kg!
gen father_weight =  e2_2 if e2_2 >= 0

gen mother_bmi = mother_weight / ( (mother_height / 100) ^ 2)
gen father_bmi = father_weight / ( (father_height / 100) ^ 2)

				
// Parental Education
gen mother_edu_years = b016mmed if inrange(b016mmed, 1, 10)
gen father_edu_years = b016ffed if inrange(b016ffed, 1, 10)

gen mother_edu_level = b05med if inrange(b05med, 0, 1) // WHY NATURAL?
gen father_edu_level = b5fed2 if inrange(b5fed2, 0, 1)
label define parent_edu_level 0 "Low" 1 "High"
label values *_edu_level parent_edu_level


// Father's Social Class
gen father_class_alt = 7 - a0014 if inrange(a0014, 1, 6)
gen father_class = 7 - c3_4 if inrange(c3_4, 1, 6)

label_class
 

// Unit Tests 
 

// Format Dataset
drop if  BD1CNTRY == 4 // Drop Northern Ireland
keep if MULTIPNO == -1 | a0248 == 1 // Singleton births

rename bcsid id
gen cohort = "BCS70"
gen survey_weight = 1
keep id cohort survey_weight male ///
	age_* height_* ///
	maths_* verbal_* vocab_* comp_* home_test ///
	father_* mother_* parent_height
compress 
save "${clean}/70c_cleaned.dta", replace 