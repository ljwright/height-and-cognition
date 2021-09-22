*Cleaning file for bann et al cognition - height paper
*cleans and derives variables for the 1946, 1958, 1970, and 2001 (MCS)
*saves minimum file in each cohort
*these are then appended in the analyses file to conduct cross-cohort analyses

*set directories
*C
*global data "S:\IOEQSS_Main\Bann\crosscinequality"
global data "c:\users\dab\onedrive - university college london\stats_cls\cohorts"
global output "C:\Users\DAB\onedrive - university college london\cross-cohort\cog\analysis\output"

*S
*global data "S:\IOEQSS_Main\Bann\crosscinequality"
*global output  "S:\IOEQSS_Main\Bann\crosscinequality\cognition"

*clean up 1946c
use "$data\46c\46c_ht_cog.dta", clear
save "$data\46c\46c_ht_wt_cog_pared.dta", replace

*merge additional parental education variable (years)
merge 1:1 nshdid_db1120  using "$data\46c\magels_fagels.dta"

*parental height
lookfor height
sum mht52 fht52
mvdecode mht52 fht52, mv(-9999/-9000=.)
sum mht52 fht52

*change units from inches
gen mht = mht52 * 0.0254
gen pht = fht52 * 0.0254

*drop minority of observations with very low height (severe outliers in regression models) 147cm = dwarfism; rounded down to 1.4 given shorter oldest cohort
sum mht pht
replace mht = . if mht <1.4
replace pht = . if pht <1.4

corr mht pht

*med
desc magels
mvdecode magels , mv(-9999=. \ 88/99=. \ 0/9=.)
tab magels

*convert years to 1-10 to match other cohorts 
tab magels , nolab
gen medy = magels -9
tab medy
replace  medy = 10 if medy>10 & !missing(medy)
tab medy

mvdecode fagels , mv(-9999=. \ 88/99=. \ 0/9=.)
tab fagels

tab fagels , nolab
gen fedy = fagels -9

replace  fedy = 10 if fedy>10 & !missing(fedy)
tab fedy

*gen binary 
**based on # years, consistent with the 58 + 70 cohorts 
tab magels //largely concordant 14y age in sam's documentation
tab magels , nolab
recode magels (10/14= 0 "low") (15/23 = 1 "high, stayed on") (0/9=.) (99=.) (-9999=.) (88=.), gen(medb) //DON: 0-9 deemed missing as their labels are not year values
																							//DON: recoded 0/1 values flipped for consistency with NCDS & BCS70
tab magels medb, mi
tab medb
label variable medb "Mother's education level (stayed in school or left)"

recode fagels (10/14= 0 "low") (15/30 = 1 "high, stayed on") (0/9=.) (99=.) (-9999=.) (88=.), gen(fedb) //DON: 0-9 deemed missing as their labels are not year values
tab fagels fedb  

*qualifications
mvdecode med fed , mv(-9899=.)
tab med , nolab
tab med
tab fed

*qualifications
*clonevar  medq = med
*drop med

*clonevar fedq = fed
*drop fed


*tab med medb 

*for now (given can't seem to merge w closer data *sigh*)
*make binary var for this

*fsc
desc fsc57 fsc50
sum fsc50 fsc57

tab  fsc50 
tab  fsc50 , nolab
tab  fsc57 
tab  fsc57 , nolab

cap drop fsc11
recode fsc57  (50=0 "v unskilled") (40=1 ) (35=2 ) (30=3 )  (20=4 )   (10=5 "i professional") (60=.), gen(fsc11)
tab fsc11 fsc57 , mi

cap drop fsc4
recode fsc50  (50=0 "v unskilled") (40=1 ) (35=2 ) (30=3 )  (20=4 )   (10=5 "i professional") (60=.), gen(fsc4)
tab fsc4


clonevar fsc10 = fsc11

*sex
tab sex
tab sex, nolab
rename sex x1
recode x1 (1=1 "men") (2=0 "women"), gen(sex)
tab sex x1, mi
tab sex

*cohort
gen cohort =0

*ht
desc ht57 ht61 ht89 
sum ht57 ht61 ht89 
mvdecode ht57 ht61 ht89 , mv(7777/9999=.)

*bmi
desc bmi57 bmi61 bmi89 
sum bmi57 bmi61 bmi89 
mvdecode bmi57 bmi61 bmi89 , mv(7777/9999=.)

*ages
lookfor age
desc date57c date61c 
sum date57c date61c 
mvdecode date57c date61c , mv(888=.)

gen xage11= date57c/12
gen xage16= date61c/12

*ht in cm

cap drop z*
egen zht10 = zanthro(ht57u, ha, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zht11 = zanthro(ht57u, ha, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zht16 = zanthro(ht61u, ha, UK) , xvar(xage16) gender(sex)  gencode(male=1, female=0) 
sum z* //all negative, thus shorter than anticipated as expected

*adult height in m
sum ht89 
gen ht43 = ht89 /100
sum ht43 

gen ht42 = ht89 /100


*bmi
egen zbmi10 = zanthro(bmi57u, ba, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zbmi11 = zanthro(bmi57u, ba, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zbmi16 = zanthro(bmi61u, ba, UK) , xvar(xage16) gender(sex)  gencode(male=1, female=0) 
sum z* 
sum sex  date57c ht57u

*cognition tests
*11y

*maths11
desc a11r
sum a11r
tab a11r
tab a11r, nolab
mvdecode a11r  , mv(99=.)

clonevar maths10 = a11r  

cap drop maths10aa 
reg a11r xage11
predict maths10aa , residuals 
corr maths10aa  a11r 

*maths16
sum m15r 
tab m15r
tab m15r, nolab
mvdecode m15r  , mv(99=.)

clonevar maths16 = m15r  

cap drop maths16aa
reg maths16 xage16
predict maths16aa , residuals 
sum maths16aa 


*verbal etc
desc v11r57
tab v11r57
sum v11r57
mvdecode v11r57, mv(-9999/-9699=.)
clonevar vsim10 = v11r57 //vanessa - 'perfect'

reg vsim10 xage11 
predict vsim10aa , residuals 

corr vsim10 vsim10aa

*15y
*june 2021: vanessa suggested use Watts Vernon r15r  note unedited (presume uncleaned) version is r15r61; alternative is v15r which is more normally distributed as less ceiling effect
desc r15r 
tab r15r 
mvdecode r15r , mv(99=.)

cap drop voc16
gen voc16 = r15r

*age adjusted
sum voc16 xage16
reg voc16 xage16
predict voc16aa , residuals 

corr voc16 voc16aa 

*other test = reading, don't use for now [march 2021]
desc r15r61 //the Watts-Vernon Reading Test (WVRT) was administered in the 1946c 
sum r15r61 

mvdecode r15r61, mv(-9999/-9699=.)
sum r15r61

drop *dxa*
keep *ht* *bmi* sex vsim10* voc16*   fsc* cohort  mht pht maths16* maths10* inf med* fed*
save  "$output\46c_cog_anthro_cleaned.dta", replace

*58c
use "$data\58c\derived\bmi_7_50_closer.dta", clear

*add in fsc at 7, 0y
cap drop _merge
merge m:1 ncdsid using "$data\58c\0, 7, 11, 16y\ncds0123.dta", keepusing(n236 n190 n1687 n2928 n1196     n1199   n1202   n1205 n926 n2930)

*merge in cognition variables
cap drop _merge
merge m:1 ncdsid using "$data\58c\derived\NCDS age 11 to 16.dta"

*parental ed
cap drop _merge
merge m:1 ncdsid using "$data\derived\closer_education\ncds_ParentEducationDerived.dta"

*maternal ed 
desc n16med n16fed
clonevar medy = n16med 
clonevar fedy = n16fed 
mvdecode medy fedy , mv( 11=. ) 

tab medy
tab fedy

*binary
*note, fathers education measured slightly diff age

desc n016nmed n716daded 
sum n016nmed n716daded 
 
tab n016nmed 
tab n716daded

*[here]*
desc n016nmed n716daded
clonevar medb=  n016nmed
clonevar fedb= n716daded
tab medb
tab fedb


*reorder sex as per other analyses where women = 0 men = 1
tab sex
tab sex, nolab
rename sex x1
recode x1 (0=1 "men") (1=0 "women"), gen(sex)
tab sex x1, mi
tab sex

*parental wt ht

sum n1196     n1199   n1202   n1205 
mvdecode n1196     n1199   n1202   n1205 , mv( -9/-1=. )
gen mwt = n1202 * 6.35029 //stone to kg
gen pwt = n1196 * 6.35029 //stone to kg

*ht
gen mht = n1205 * 0.0254
gen pht = n1199 * 0.0254

replace mht = . if mht <1.4
replace pht = . if pht <1.4

*bmi
gen mbmi = mwt / mht^2
gen pbmi = pwt / pht^2


sum mht

*fsc 
desc n236
capture drop fsc
clonevar fsc = n236
mvdecode fsc , mv( -1=. \ 1=. \ 9=. \ 10 =. \ 11=. \ 12 = . ) //codes missing to. , aslso unemployed, students etc

tab  fsc 
recode fsc  (7=0 "v unskilled") (6=1 ) (5=2 ) (4=3 )  (3=4 )   (2=5 "i professional"), gen(fsc0)
tab fsc0 n236


*fsc11
desc n1687
tab n1687

tab n1687, nolab
recode n1687 (6=0 "v unskilled") (5=1 ) (4=2 ) (3=3 )  (2=4 )   (1=5 "i professional") (7=.) (-1=.), gen(fsc11)
tab n1687 fsc11

tab n1687 fsc11, mi

clonevar fsc10 = fsc11

clonevar fsc10_0 = fsc10
replace fsc10_0 = fsc0 if missing(fsc10_0 )
tab fsc10 fsc10_0 , mi

***bmi z scores

*ht in cm
foreach var of varlist ht11 ht16 ht44 {
    gen `var'cm = `var' *100
}

gen ht43 = ht44
egen zht10 = zanthro(ht11cm, ha, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zht11 = zanthro(ht11cm, ha, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zht16 = zanthro(ht16cm, ha, UK) , xvar(xage16) gender(sex)  gencode(male=1, female=0) 

sum *zht*

egen zbmi10 = zanthro(bmi11, ba, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zbmi11 = zanthro(bmi11, ba, UK) , xvar(xage11) gender(sex)  gencode(male=1, female=0) 
egen zbmi16 = zanthro(bmi16, ba, UK) , xvar(xage16) gender(sex)  gencode(male=1, female=0) 


gen age23=23 //max range of chart, so puts in comparable z score to prior measures ie, in ref to ref population
egen zbmi42 = zanthro(bmi42, ba, UK) , xvar(age23) gender(sex)  gencode(male=1, female=0) 
sum zbmi42 bmi42 
sum zbmi42 

egen zbmi46 = zanthro(bmi44, ba, UK) , xvar(age23) gender(sex)  gencode(male=1, female=0) 
sum zbmi46 bmi44 

**cognition, age adjusted
*11y

*maths
clonevar maths10 = n926
mvdecode maths10, mv( -1=. )
tab maths10 

reg maths10 ncdsage11 //older =higher score [expected]

predict maths10aa , residuals 

*vsim
desc nvdsvabil  //derived and source variables

clonevar vsim10 = nvdsvabil 
cap drop vsim10aa

reg vsim10 ncdsage11 //older =higher score [expected]

predict vsim10aa , residuals 

*16y
*maths
desc n2930
tab n2930, nolab

clonevar maths16 = n2930

mvdecode maths16, mv( -1=. )
tab maths16 

reg maths16 ncdsage16 //very small diff in age , but negative 

predict maths16aa , residuals 
corr maths16aa maths16 //in any case, extremly highly correlated 

*voc
desc ncds_rcomp n2928 ncdsage16
sum ncds_rcomp n2928 ncdsage16 

clonevar voc16 = ncds_rcomp

reg ncds_rcomp ncdsage16 
predict voc16aa, residuals 
sum voc16aa

*nvdsvabil age 11 originally n914
*ncds_rcomp age 16 originally n2928

*cohort variable
gen cohort =1

*save dataset
keep *ht* *bmi* sex vsim10* voc16*   fsc* cohort  mbmi pbmi mht pht maths16* maths10* med* fed*
save  "$output\58c_cog_anthro_cleaned.dta", replace


*70c
*10y
use "$data\70c\10y\bcs3derived.dta", clear

*syntax to remove duplicates in this dataset
*quietly by BCSID:  gen dup = cond(_N==1,0,_n)
*tab dup
*br if dup>0 

drop if missing(BD3PSOC)
tab BD3PSOC

sort BCSID
merge m:m BCSID using "$data/70c/birth and 22m subsample/bcs1derived.dta"

*birth fsc
capture drop _merge
merge m:m BCSID using "$data/70c/birth and 22m subsample/\bcs7072a.dta", keepusing(A0014 A0018)

//N=17196
*5y
capture drop _merge
sort BCSID 
merge 1:1 BCSID using "$data/70c/5y\bcs2derived.dta"  
tab _merge  //most in both as expected, some new though as expected due to imigration

*parent education
		rename BCSID, lower
capture drop _merge

merge  1:1 bcsid using "$data/70c/5y\f699b.dta" , keepusing(e131 e190 e220 e228a e228b e228c e228d e228e e229 e230 e231 e232 e233 e234 e235)  // N=17588, came into the study after birth? 
tab _merge  //most in both as expected, some new though as expected due to imigration

*10y variables to obtain dates, and social class 
rename bcsid, lower
cap drop _merge
merge  1:1 bcsid using "$data/70c/10y\sn3723.dta" , keepusing(examdata examdatb examdatc doba10 dobb10      dobc10         e1_1  e1_2 e2_1 e2_2    c3_4   c3_11                 )

*closer data
rename bcsid, upper
cap drop _merge
merge 1:1 BCSID using "$data\70c\derived\bmi_wt_ht0_42.dta" //data on bmi ht wt and closer ses -fathers social class and mat education
tab _merge

rename *, lower

*16y source date variables
cap drop _merge
merge 1:1 bcsid using "$data\70c\16y\bcs7016x.dta" , keepusing(*_mt *_yr bversion     e1-e13 ) //e1-e13 comprehension sub scale to use in sens analysis w 58c 

*16y maths variable
cap drop _merge
merge 1:1 bcsid using "$data\70c\derived\bcs70_16-year_arithmetic_data.dta", keepusing(mathscore)

*34y education
cap drop _merge
rename  bcsid BCSID
merge 1:1 BCSID using "$data\70c\34y\bcs7derived.dta"
cap drop _merge

*46y
cap drop _merge
merge 1:1 BCSID using "$data\70c\46y\ukds\bcs_age46_main.dta" , keepusing(BD10BMI BD10MBMI B10BFPC B10HEIGHTCM)

*merge in vanessa's cognition file
rename *, lower
cap drop _merge
merge 1:1 bcsid using "$data\70c\derived\bcs cognition 5 to 16.dta" 

*parental ed
cap drop _merge
merge m:1 bcsid using "$data\derived\closer_education\bcs70_ParentEducationDerived.dta"

*maternal ed - number of years in school
desc b016mmed //contin medy
tab b016mmed 
clonevar medy = b016mmed 
desc b016ffed //fedy
clonevar fedy = b016ffed 

gen rmed = .
replace rmed = b016nmmed //note strange 11 variable (presume code to missing or lowest group)
tab b016nmmed

vreverse rmed, gen(med)
tab rmed med
tab med, 
tab med, nolab

*post compulsary mat ed var
desc b05nmed  
sum b05nmed  
clonevar  medb = b05nmed 
tab medb b05nmed 

desc b5fed2
sum b5fed2
clonevar fedb = b5fed2

*education qualification
clonevar medq = b516mq
clonevar fedq = b516fq

***clean data
rename *, lower

*reorder sex as per other analyses where women = 0 men = 1
tab sex
tab sex, nolab
rename sex x1
recode x1 (0=1 "men") (1=0 "women"), gen(sex)
tab sex x1, mi
tab sex

*parental ht bmi
desc e1_1  e1_2 e2_1 e2_2
sum e1_1  e1_2 e2_1 e2_2
mvdecode e1_1  e1_2 e2_1 e2_2, mv( -9/-1=. )


gen mht =  e1_1 / 100
gen pht =  e2_1 / 100

replace mht = . if mht <1.4
replace pht = . if pht <1.4

gen mwt =  e1_2
gen pwt =  e2_2

*bmi 
gen mbmi = mwt / mht^2
gen pbmi = pwt / pht^2

sum mbmi pbmi
corr mbmi pbmi //0.17

corr mht pht //0.22


**0y fsc
desc bd1psoc a0014    // a0014  sc father 1970       
tab bd1psoc a0014    
capture drop fsc0
tab a0014 

recode a0014 (1=0 "v unskilled")  (2=1 )  (3=2 ) (4=3 ) (5=4 ) (6=5 "i professional") (-2/-1 =.) (7=.) (8=.), gen(fsc0)
tab fsc0
tab a0014 fsc0, mi

*fsc 10
desc c3_4 c3_11 bd3psoc
tab c3_11, nolab

cap drop fsc10 
recode c3_4 (6=0 "v unskilled")  (5=1 )  (4=2 ) (3=3 ) (2=4 ) (1=5 "i professional")  (-1=.), gen(fsc10) 
tab fsc10 c3_4, nolab

tab fsc0 fsc10
tab fsc0 fsc10, nolab

clonevar fsc10_0 = fsc10
replace fsc10_0 = fsc0 if missing(fsc10_0)
tab fsc10_0 //17k

*maternal education
tab e190
tab e190, nolab
clonevar pedx = e190
mvdecode  pedx, mv(-3=. \ -2=. \ -1=.  ) 
tab pedx, nolab
replace pedx=. if pedx==8
tab pedx

*income 10y
clonevar income10 = bd3inc 
tab income10 
tab income10 , nolab
mvdecode  income10 , mv(-1=. \ 8 = .a)
tab income10 
vreverse income10, gen(income10r)
tab income10 income10r

*sc
gen sc42 = b9csc 
recode sc42  6 =.
cap drop asep
recode sc42(1=0 " I Professional") (2=1 " II Managerial-technical") (3.1=2 "3.1") (3.2=3 "3.2") (4=4 "IV Partly skilled") (5=5 "Unskilled"), gen(asep)
tab asep b9csc, mi

*own education - at 34y
desc bd7hachq
tab bd7hachq
tab bd7hachq, nolab

cap drop smed
recode bd7hachq (7/8=0 "degree/higher") (4/6=1 "a levels or diploma") (1/3=2 "o levels, gcses, vocational") (0=3 "none") , gen(smed)
tab smed
tab bd7hachq smed 
tab bd7hachq smed , mi


*ht in cm
foreach var of varlist ht10 ht16 ht42 {
    gen `var'cm = `var' *100
}

*make age-adjusted measures
*zscores for anthropometrics; should be comparable as with respect to reference population (not internally such that 1 SD means something different)

*dates and ages...
desc dob* 
sum dob* //all born 1970 april!
*make dob variable
gen born = ym(dobc10+1900,dobb10)
di born /12
sum born

*make anthro date variable
mvdecode  examdata examdatb examdatc, mv(-8/-1= .)
mvdecode  bd3age , mv(-8/-1= .) 

desc  examdata examdatb examdatc
sum examdata examdatb examdatc

gen interviewed = ym(examdatc +1900, examdatb )
*format interviewed %tm
sum born interviewed

*gen age

gen age10anthro = interviewed - born
sum age10anthro 

gen age10anthroy = age10anthro/12
sum age10anthroy 

corr age10anthroy xage10 
*scatter age10anthroy xage10 //will's documented age variable derived correctly
 
*diff age for assessment
gen bcsage10y = bcsage10/12

sum age10anthroy  bcsage10y if !missing(age10anthroy  , bcsage10y ) //medical interviews occured a number of weeks after assessment interview, explaining discrepency 
 
sum  xage10 bcsage10 bcsage10y // the source variable  BD3AGE has n=5620 'not computed' in N=14875

desc bd3age xage10  bcsage10y
sum bd3age xage10  bcsage10y
sum bd3age xage10 bcsage10y if !missing(bd3age )
sum xage10 bd3age bcsage10y if !missing(bd3age, xage10 , bcsage10y)

*corr xage10 bd3age bcsage10y if !missing(bd3age, xage10 , bcsage10y)
*scatter bd3age bcsage10y if !missing(bd3age, xage10 , bcsage10y)




*46y measures
mvdecode  bd10mbmi b10heightcm b10bfpc b10bfpc b10heightcm, mv(-9/-1= .)
desc b10bfpc b10bfpc 
sum b10bfpc b10bfpc 
clonevar fm46 = b10bfpc
clonevar bmi46 = bd10mbmi
sum bmi46 

gen ht46 = b10heightcm /100
sum ht46 

*same name as 46c
gen ht43 = b10heightcm /100
sum ht43 

clonevar ht46cm = b10heightcm
sum ht46cm 


*age adjusted z-scores
*egen bmi7iotf = zbmicat(bmi7) , xvar(age7y) gender(sex)  gencode(male=1, female=0) 

egen zbmi10 = zanthro(bmi10, ba, UK) , xvar(xage10) gender(sex)  gencode(male=1, female=0) 
sum zbmi10 bmi10

egen zbmi16 = zanthro(bmi16, ba, UK) , xvar(xage16) gender(sex)  gencode(male=1, female=0) 

sum bmi42 bmi46
gen age23=23 //max range of chart, so puts in comparable z score to prior measures ie, in ref to ref population
egen zbmi42 = zanthro(bmi42, ba, UK) , xvar(age23) gender(sex)  gencode(male=1, female=0) 
sum zbmi42 bmi42 
sum zbmi42 

egen zbmi46 = zanthro(bmi46, ba, UK) , xvar(age23) gender(sex)  gencode(male=1, female=0) 
sum zbmi46 bmi46  




*ht
egen zht10 = zanthro(ht10cm, ha, UK) , xvar(xage10) gender(sex)  gencode(male=1, female=0) 
egen zht11 = zanthro(ht10cm, ha, UK) , xvar(xage10) gender(sex)  gencode(male=1, female=0) 

egen zht16 = zanthro(ht16cm, ha, UK) , xvar(xage16) gender(sex)  gencode(male=1, female=0) 
sum zht* zht10 ht16


*age-adjusted cognition measures 


*10y

*maths
desc bd3maths 
tab bd3maths , nolab

clonevar maths10 =  bd3maths
mvdecode  maths10, mv(-1=. )

reg maths10 bcsage10y //older =higher score
predict maths10aa , residuals 



*vsim

sum bcsage10y //ranges from 9-11.5, so massive variance
reg sim10_g bcsage10y //older =higher score

sum sim10_g bcsage10y xage10 //some missing age variable despite valid cog measure
sum sim10_g bcsage10y xage10 if !missing(sim10_g)

corr bcsage10y xage10


clonevar vsim10 = sim10_g 
cap drop vsim10aa
reg sim10_g bcsage10y //older =higher score

predict vsim10aa , residuals 

sum vsim10aa
corr vsim10aa  bcsage10y
corr vsim10aa  bcsage10y xage10
sum xage10 vsim10aa bcsage10y if !missing(xage10, vsim10) & missing(vsim10aa)


*16y
*lots of missing in age 16y variable , much more than actual cognition tests!
*source cog variables around 5-6k eg, cvo4
**so creating age using multiple age source variables *_yr and *_mt variables - ie, date X document completed
**document C appears to contain vocab tests https://cls.ucl.ac.uk/wp-content/uploads/2017/07/Guide_to_the_10-year_data-Part_1.pdf
**but I don't see a date for this specific test

mvdecode *_yr , mv(-8/-1= .)
mvdecode *_mt , mv(-8/-1= .)

sum *_yr
sum *_mt
cap drop date16*

foreach x in f g h l p q r t {
cap drop 	testdate16_`x'
cap drop 	age16`x'
gen testdate16_`x' = ym(`x'doc_yr +1900, `x'doc_mt)
gen age16`x' = (testdate16_`x' - born) /12
}
sum bcsage16 testdate16_*  age16*

foreach x in g h l p q r t {
cap drop age16 
gen age16 = age16f
replace age16 = age16`x' if missing(age16)
}
sum bcsage16 age16 //N=7,117

sum voc16_20 age16 bcsage16
sum voc16_20 age16 bcsage16 xage16 if !missing(voc16_20)

*age adjusted measure
clonevar voc16 = voc16_20


mvdecode  bversion, mv(-1=. )

reg voc16_20 age16 i.bversion, base 
predict voc16aa, residuals 
sum voc16aa
corr voc16aa age16 bversion

*maths16
corr mathscore voc16aa //positive as expected

clonevar maths16 = mathscore
reg maths16 age16 i.bversion
predict maths16aa, residuals 


*comprehension sens analysis variable
mvdecode  e1-e13, mv(-1= .)  //no q to missing 'not stated' assume incorrect
sum e1-e13 //Ns looks comparable to other scales
tab e6 
tab e6, nolab //1 = correct , 2 and -2 not
foreach x in 1 2 3 4 5 6 7 8 9 10 11 12 13 {
replace e`x'=0 if e`x' ==2
replace e`x'=0 if e`x' ==-2
}
cap drop comp16
egen comp16 =rowtotal(e1-e13 )
sum comp16

reg comp16 age16 bversion
predict comp16aa, residuals 
sum comp16aa
corr comp16aa age16 bversion

*reg voc16_20 bcsage16 //n~3k!
*note will's closer dataset created a derived age measure, but focused on when age at BMI measured, and then presumably imputed others
*reg voc16_20 xage16 //magnitude doesn't appear right

*drop northern ireland to make fair denominator for response
tab bd1cntry
tab bd1cntry, nolab
drop if bd1cntry==4 //n=628

*cohort variable
gen cohort =2

*save dataset 
keep *ht* *bmi* sex vsim10* voc16* sid  fsc* cohort bversion *ped* comp16* cog_5 mbmi pbmi mht pht maths16* maths10* med* fed*
save  "$output\bcs70_cog_anthro_cleaned.dta", replace

***MCS
*svyset sptn00 [pweight=aweight17], strata(pttype2) fpc(nh2)
use "$data\mcs\derived\mcs_bmi_ses_closer v001.dta", clear //data on bmi ht wt and closer ses -fathers social class and mat education
replace mcsid = subinstr(mcsid, ".", "",.)  // [unclear why pid [mcsid] has a . at the end of it for each case, unlike other mcs datafiles

rename mcsid, upper
merge m:m MCSID using "$data\mcs\17y\W2\mcs7_cm_derived.dta", keepusing(GCNUM00 GCMCS7AG GCBMIN7)
cap drop _merge
merge 1:1 MCSID GCNUM00  using "$data\mcs\17y\W3\mcs7_cm_interview.dta", keepusing(GCHTCM00)

*analytical weights
cap drop _merge
merge m:1 MCSID    using "$data\mcs\17y\mcs_longitudinal_family_file.dta" , keepusing(GOVWT2)  //10,952 matched, 8409 not matched 

*17y cog source 
cap drop _merge
merge m:1 MCSID  GCNUM00  using "$data\mcs\17y\W4\mcs7_cm_cognitive_assessment.dta", keepusing(GCNAAS0A- GCNAAS0J )

*parental bmi ht
cap drop _merge
merge m:1 MCSID using "$data\mcs\0y\mcs1_derived_variables.dta"

*ethnicity - ?

*3y
cap drop _merge
merge m:m MCSID  using "$data\mcs\3y\mcs2_cm_derived.dta" , keepusing(BDC06E00)

*sex [already in dataset]


*drop those with no cohort member number (ie, retain cohort member sample)
sum

*maths 7y
cap drop _merge
merge m:m MCSID using "$data\mcs\7y\mcs4_cm_cognitive_assessment.dta" , keepusing(DCMATHS7SC  DAGEDY000)

*derived cog variables
cap drop _merge
rename MCSID, lower
clonevar cnum = GCNUM00
merge m:m mcsid using "$data\mcs\derived\MCS cognition 5 to 14.dta" 

*mat education
*derived cog variables
cap drop _merge
merge m:m mcsid using "$data\derived\closer_education\mcs_ParentEducationDerived.dta" 

*country variable
cap drop _merge
merge m:m mcsid using "$data\mcs\0y\mcs_longitudinal_family_file.dta", keepusing(country)
tab country //

*check dropping

*mat education
*already in dataset, thus checking alongside source maternal ed data 
*maternal ed - number of years in school


*banded years
lookfor education
sum mmled  mfled
clonevar medy = mmled  
clonevar fedy = mfled
sum medy fedy

*binary
tab mmled
*original value labels of mcs_mledg in mcs 1= compulsory ed [left by 16] ;  2 = post-compulsory
cap drop medb
recode mcs_mledg (1=0 "low") (2=1 "high (stayed on)"), gen(medb)
tab medb

tab medy medb //correct

*fed
tab fedy
tab fedy, nola
recode fedy (0/6=0 "low") (6/max=1 "high (stayed on)"), gen(fedb)
tab fedy fedb

*qualification
clonevar medq = mcs_mhq
clonevar fedq = mcs_fhq

tab mcs_nmhq mcs_mhq , mi //almost all same
tab mcs_nfhq mcs_fhq , mi //almost all same

rename *, lower


*analytic weight at 17y
clonevar aweight17 = govwt2  

*svyset sptn00 [pweight=aweight17], strata(pttype2) fpc(nh2)


*eth



*sex
tab sex

*recode so male = 1 female ==0
recode sex 2=0
tab sex
label define sex 0 "female" 1 "male", replace
label values sex sex
tab sex
replace sex =. if sex==-1


*reverse code SEP
vreverse csep, gen(fsc11)
vreverse medb, gen(medx)
tab medx

tab medb

*ages

gen age11 = mcsage10/12
sum age11 

gen age14 = mcsage14/12
sum age14 

clonevar age17 = gcmcs7ag
mvdecode age17 , mv(-1 = .)
sum age17

*cog

*

*10y evasbil 
sum evsabil //10-179

sum evsabil  mcsage10 //thankfully similar N!
sum evsabil  

clonevar vsim10 = evsabil  


reg evsabil  mcsage10 
predict vsim10aa , residual
sum vsim10aa  evsabil   mcsage10 
corr vsim10aa  evsabil   mcsage10 //appears to have worked

*14y voc16
sum voc14_20  mcsage14 //ranges 0-19
desc voc14_20 mcsage14


clonevar voc16 = voc14_20

reg voc14_20 mcsage14 //older= higher score
predict voc16aa, residuals
sum voc16aa voc14_20 mcsage14 
corr voc16aa voc14_20 mcsage14 //appears to have worked 

*maths 10
desc  dagedy000 dcmaths7sc 
sum  dagedy000 dcmaths7sc 
mvdecode dcmaths7sc , mv(-1 = .)

clonevar maths10 = dcmaths7sc

reg dcmaths7sc dagedy000 //older = higher score
predict maths10aa , residuals 

*maths16

desc  gcnaas0a-gcnaas0j 
sum gcnaas0a-gcnaas0j 

cap drop score17 
gen score17 = 0 if !missing(gcnaas0a)
sum score17 gcnaas0a if gcnaas0a==5

replace score17 = score17 +1 if gcnaas0a==5
replace score17 = score17 +1 if gcnaas0b==1
replace score17 = score17 +1 if gcnaas0c==3
replace score17 = score17 +1 if gcnaas0d==4
replace score17 = score17 +1 if gcnaas0e==1
replace score17 = score17 +1 if gcnaas0f==5
replace score17 = score17 +1 if gcnaas0g==4
replace score17 = score17 +1 if gcnaas0h==4
replace score17 = score17 +1 if gcnaas0i==2
replace score17 = score17 +1 if gcnaas0j==5

tab score17

clonevar maths16 = score17

reg maths16 age17 //older = higher score
predict maths16aa , residuals 


*17y
cap drop bmi17 
clonevar bmi17= gcbmin7 
mvdecode bmi17 , mv(-99/0 = .) //replace negative coded values to .

mvdecode gchtcm00  , mv(-1 = .) //replace negative coded values to .
clonevar ht17cm = gchtcm00  
replace ht17cm = . if ht17cm==-5
*ht in cm
foreach var of varlist ht11 ht14 {
    gen `var'cm = `var' *100
}

tab sex
sum age*
sum bmi* ht*

*zscores
egen zbmi11 = zanthro(bmi11, ba, UK) , xvar(age11) gender(sex)  gencode(male=1, female=0) 
egen zbmi14 = zanthro(bmi14, ba, UK) , xvar(age14) gender(sex)  gencode(male=1, female=0) 
egen zbmi17 = zanthro(bmi17, ba, UK) , xvar(age17) gender(sex)  gencode(male=1, female=0) 

cap drop zht*
egen zht11 = zanthro(ht11cm, ha, UK) , xvar(age11) gender(sex)  gencode(male=1, female=0) 
egen zht14 = zanthro(ht14cm, ha, UK) , xvar(age14) gender(sex)  gencode(male=1, female=0) 
egen zht17 = zanthro(ht17cm, ha, UK) , xvar(age17) gender(sex)  gencode(male=1, female=0) 


sum zht14 ht14

*make comparable variables names with bcs 
clonevar zbmi10 = zbmi11 
clonevar zbmi16 = zbmi14

clonevar zht10 = zht11 
clonevar zht16 = zht14

clonevar fsc10 = fsc11


reg zbmi10 fsc10 
*br if missing(sid) & !missing(vsim10aa ) // [why?]

*COHORT VARIABLE
gen cohort =3

*parental ht bmi
*ht
desc admhgt00 amhgtm00 aphgtm00
sum admhgt00 amhgtm00 aphgtm00

mvdecode admhgt00 amhgtm00 aphgtm00 , mv( -1=. )

cap drop mht pht
clonevar mht = admhgt00 
clonevar pht = aphgtm00

*checking
list mht admhgt00 if admhgt00 <1.4 
sum  mht admhgt00 if admhgt00 <1.4  //n=33

*remove outliers
replace mht = . if mht <1.4 
replace pht = . if pht <1.4

clonevar nfather = adfinh00 

sum mht pht
sum mht pht if nfather==1
sum mht pht if nfather==1 & !missing(mht, pht) // very closely matches  https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0054186

corr mht pht if nfather==1 & !missing(mht, pht) // 0.17 correlation of height


*bmi
lookfor bmi
di  r(varlist)

desc admbmi00 admbmb00 amdbmia0 amdbmib0 apdbmia0 apdbmib0
sum admbmi00 admbmb00 amdbmia0 amdbmib0 apdbmia0 apdbmib0
mvdecode admbmi00 admbmb00 amdbmia0 amdbmib0 apdbmia0 apdbmib0 , mv( -9/-1=. )

clonevar mbmi = admbmi00  //using before born, more relevant to assortative mating. they correlate at 0.86
clonevar pbmi = apdbmia0 

desc mbmi pbmi
sum  mbmi pbmi

corr mbmi pbmi 
corr mbmi pbmi if nfather==1 //correlated at 0.17
*scatter mbmi pbmi if nfather==1

*drop northern ireland to retain comparability with other cohort and give correct denominator
tab country
tab country, nolab
drop if country==4

keep  *bmi* *ht* sex vsim10* voc16* sid medx fsc* cohort sptn00 aweight17  pttype2 nh2 cog_5 mbmi pbmi mht pht nfather maths16* eth maths10* med* country fed*
save  "$output\mcs_cog_anthro_cleaned.dta", replace