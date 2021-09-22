/*
Data Munging for
	Weakening of the cognition and height association from 1957 to 2018: 
	Findings from four British birth cohort studies 
Bann et al. (2021)

Cleans and derives variables for the 1946, 1958, 1970 and 2001 (MCS) cohorts, separately.
Files are appended together in 2_mice.R, where variables are also renamed to account for different measurement ages

To Do + Questions:
	All:
		Sort out ages: MCS (all), BCS (age_cog_16), NCDS (all), NSHD (all)
		Check Mother Education is dichotomised at correct age!
		Add Z-Scores and Residuals: NSHD
		Check Social Class: NSHD
		Add Unit Tests: MCS, BCS, NCDS, NSHD
		Check if need more cognitive tests: MCS, BCS, NCDS, NSHD
		Unit Test: Do people grow across time? Check correlation between height and weight and # with decreasing height
		Ensure education level variables (years and level) are consistent
		Add cleaning to remove outliers!
	MCS:
		Can I get access to NSHD CLOSER data?
		Why not highest RGSC rather than father?
		Get raw RGSC code and find out why dropping 261 participants.	
	BCS70:
		What to do about low heights and weights?
		Why just questions 1-13 for comp_16?
		Age for anthropometrics
		I've cleaned sim10_G differently - missing if any missing
		Should it not be COB rather than BD1CNTRY
		weight_10 not actually in grams (check CLOSER)
		Sort out cognitive assessment ages properly! May differ by test.	
		Lots of missingness for mathscore
	NCDS:
		Get OG parental education code
	NSHD:
	
	Can I get access to NSHD CLOSER data?
	Why not highest RGSC rather than father?
	Get raw RGSC code and find out why dropping 261 participants.
*/

/*
Set-Up
*/

clear
cls
set linesize 140

global bann 	"S:/IOEQSS_Main/Bann/crosscinequality"
global raw		"D:"
global clean 	"D:/Projects/Cognition and Height/Data"
global code		"D:/Projects/Cognition and Height/Code"

// ssc install egenmore
// ssc install wridit
// net install dm0004_1.pkg // zanthro


/*
Make Programmes
*/
capture program drop gen_bmi
program define gen_bmi
	syntax, Ages(string)
	
	foreach age of local ages{
	    gen bmi_`age' = weight_`age' / ( (height_`age' / 100) ^ 2)
	}
end

capture program drop gen_age
program define gen_age
	args new_var int_m int_y birth_my
	
	tempvar date
	gen `date' = ym(1900 + `int_y', `int_m') if `int_y' >= 0 & `int_m' >= 0
	gen `new_var' = (`date' - `birth_my') / 12
	drop `date'
end

capture program drop gen_anthro
program define gen_anthro
	syntax, Ages(string)
	
	foreach age of local ages{
	    egen height_chart_`age' = zanthro(height_`age', ha, UK), ///
			xvar(age_anth_`age') gender(male) gencode(male = 1, female = 0) 
		
		egen bmi_chart_`age' = zanthro(bmi_`age', ba, UK), ///
			xvar(age_anth_`age') gender(male) gencode(male = 1, female = 0) 
	}	
end
	
capture program drop gen_residuals
program define gen_residuals
	syntax varlist [, covars(varlist)]
	
	foreach var of local varlist{
		local age = substr("`var'", -2, .)
		local stub = substr("`var'", 1, strpos("`var'", "_") - 1)
		
		regress `var' age_cog_`age' `covars'
		predict `stub'_resid_`age', residuals
	}
end

capture program drop label_class
program define label_class
	label define father_class 	///
		1 "V Unskilled" 2 "IV Partly Skilled" ///
		3 "III Skilled Manual" 4 "III Skilled Non-Manual" ///
		5 "II Intermediate" 6 "I Professional"
	label values father_class* father_class
end


/*
Run Do Files
*/
do "${code}/1a_munging_mcs.do"
do "${code}/1b_munging_bcs70.do"
do "${code}/1c_munging_ncds.do"
do "${code}/1d_munging_nshd.do"



