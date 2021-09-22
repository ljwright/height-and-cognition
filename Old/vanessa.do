/*
Here are the relevant cognition variables:
•	NCDS: nvdsvabil, ncds_rcomp 
ncds0123.dta/
*nvdsvabil age 11 originally n914
*ncds_rcomp age 16 originally n2928
I think david took the n* variables and made -1 =.
•	BCS70: mathscore, sim10_G, voc16_20
bcs70_16-year_arithmetic_data.dta
replace mathscore =. if mathscore==-1

below in file attached:
•	BCS70: sim10_G, voc16_20
•	MCS: voc14_20
*/





***BAS*****
use "....\sn3723.dta"


****BAS Similarities (word)****

*group name only
gen BCS10simG1=1 if i3576==1
replace BCS10simG1=0 if inrange(i3576,2,9)
gen BCS10simG2=1 if i3578==1
replace BCS10simG2=0 if  inrange(i3578,2,9)
gen BCS10simG3=1 if i3580==1
replace BCS10simG3=0 if inrange(i3580,2,9)
gen BCS10simG4=1 if i3582==1 
replace BCS10simG4=0 if inrange(i3582,2,9)
gen BCS10simG5=1 if i3584==1
replace BCS10simG5=0 if inrange(i3584,2,9)
gen BCS10simG6=1 if i3586==1
replace BCS10simG6=0 if inrange(i3586,2,9)
gen BCS10simG7=1 if i3588==1
replace BCS10simG7=0 if inrange(i3588,2,9)
gen BCS10simG8=1 if i3590==1
replace BCS10simG8=0 if inrange(i3590,2,9)
gen BCS10simG09=1 if i3592==1
replace BCS10simG09=0 if inrange(i3592,2,9)
gen BCS10simG10=1 if i3594==1
replace BCS10simG10=0 if inrange(i3594,2,9)
gen BCS10simG11=1 if i3596==1
replace BCS10simG11=0 if inrange(i3596,2,9)
gen BCS10simG12=1 if i3598==1
replace BCS10simG12=0 if  inrange(i3598,2,9)
gen BCS10simG13=1 if i3600==1
replace BCS10simG13=0 if inrange(i3600,2,9)
gen BCS10simG14=1 if i3602==1
replace BCS10simG14=0 if inrange(i3602,2,9)
gen BCS10simG15=1 if i3604==1
replace BCS10simG15=0 if inrange(i3604,2,9)
gen BCS10simG16=1 if i3606==1
replace BCS10simG16=0 if inrange(i3606,2,9)
gen BCS10simG17=1 if i3608==1
replace BCS10simG17=0 if inrange(i3608,2,9)
gen BCS10simG18=1 if i3610==1
replace BCS10simG18=0 if inrange(i3610,2,9)
gen BCS10simG19=1 if i3612==1
replace BCS10simG19=0 if inrange(i3612,2,9)
gen BCS10simG20=1 if i3614==1
replace BCS10simG20=0 if inrange(i3614,2,9)
gen BCS10simG21=1 if i3616==1
replace BCS10simG21=0 if inrange(i3616,2,9)
egen sim10_G=rowtotal(BCS10simG1-BCS10simG21)
replace sim10_G=90 if i3579==. | inrange(i3579, -6, -3)
replace sim10_G=. if sim10_G==90

egen sim10_std = std(sim10_G)



*****AGE 16/14*********
*age 16/14 APU vocabulary - BCS 75 items MCS 20 (a subset of the 75) - the only measures where we can tests for measurement invariance
*OK so I'll give you the total 20 for each and total for the 12 items that Sam found were MI, I have yet to do this myself, but will. (So depending on what we want to do - compare means and/or coefficients )

***BCS***
**20 items**
bcs7016x.dta/

foreach x of varlist cvo1-cvo75{
replace `x'=. if inrange(`x',-2,-1)
}
foreach x of varlist  cvo37   cvo59  cvo72{
replace `x'=0 if inrange(`x',2,5)
replace `x'=1 if `x'==1
}
foreach x of varlist   cvo17 cvo27  cvo45 cvo46    {
replace `x'=0 if `x'==1 | inrange(`x',3,5)
replace `x'=1 if `x'==2
}
foreach x of varlist  cvo33   cvo53  cvo73 cvo75  {
replace `x'=0 if inrange(`x',1,2) | inrange(`x',4,5)
replace `x'=1 if `x'==3
}
foreach x of varlist   cvo43 cvo52   {
replace `x'=0 if inrange(`x',1,3) | `x'==5
replace `x'=1 if `x'==4
}
foreach x of varlist cvo4 cvo12 cvo28  cvo39  cvo41 cvo50 cvo55   {
replace `x'=0 if inrange(`x',1,4) 
replace `x'=1 if `x'==5
}
egen voc16_20=rowtotal(cvo4 cvo12 cvo17 cvo27 cvo28 cvo33 cvo37 cvo39 cvo41 cvo43 cvo45 cvo46 cvo50 cvo52 cvo53 cvo55 cvo59 cvo72 cvo73 cvo75)
egen nmiss = rowmiss(cvo4 cvo12 cvo17 cvo27 cvo28 cvo33 cvo37 cvo39 cvo41 cvo43 cvo45 cvo46 cvo50 cvo52 cvo53 cvo55 cvo59 cvo72 cvo73 cvo75)
replace voc16_20=. if nmiss==20 
drop nmiss
sum voc16_20, detail
hist voc16_20, freq



*****MCS****
**NB the structure of the MCS files changed earlier this year from wide to long so CHECK - there may be some file name changes
mcs6_cm_derived/
rename FCWRDSC voc14_20
replace voc14_20=. if inrange(voc14_20,-9,-1)
sum voc14_20, detail
hist voc14_20, freq

