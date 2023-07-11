***********************************************************************************************************************
*** Replication Files for Chowdhury, Sutter, and Zimmermann ***
*** "Economic preferences across generations and family clusters: A large-scale experiment in a developing country" ***
***********************************************************************************************************************
***********************************************************************************************************************


***********************************************************************************************************************
*** DATA CLEANING AND VARIABLE CONSTRUCTION: This file takes the raw data and constructs the outcome variables 		***
*** used for analysis ***
*** It takes data sets in the Raw Data subfolder, and saves the constructed sets in the ConstructedData subfolder 	***
***********************************************************************************************************************

*** Set directory 

capture cd "~/Data Archive"


************************************************
*   Set Globals for Directories                 *
************************************************
	global constructdata 	"ConstructedData"
	global rawdata 			"RawData"
	global figuresapp 		"FiguresAppend"
	global tablesapp  		"TablesAppend"

	*** Set Stata version
	capture version 16.0
	capture log close
	set more off
	
***********************************************************************************************************************
** Raw Data (input files)
	** indentity_2014.dta //Comes from HH Survey conducted in 2014
	** Section_1_1 //Comes from HH Survey conducted in 2014
	** Section_5_1 //Comes from HH Survey conducted in 2014
	** cognitive_data_children_2015 // cognitive data collected in 2015
	** cognitive_data_adult_2015 // cognitive data collected in 2015
	** ExperimentChildren // economic preference experiment conducted in 2016 
	** ExperimentAdult // economic preference experiment conducted in 2016 
	** parenting_style // parenting style data collected in 2018
	** income2016 // household income collected in 2016
	** village // village survey conducted in 2014
	
** Constructed Data (output files):
	** children // one of the main data sets constructed to conduct the analyis 
	** parents // one of the main data sets constructed to conduct the analyis 
	** table10 // constructed to produce Table 10
	** table11 // constructed to produce Table 11
	** table12 // constructed to produce Table 12
	** table13 // constructed to produce Table 13
	** appendB // constructed to produce Appendix B tables
	** tableA6 // constructed to produce Table A.6
	** tableA8 // constructed to produce Table A.8
	** data_with_different_samples // constructed to produce Table A.1
	** data_for_attrition_analysis // constructed to produce Table A.3


******************************************************************
**********	Experiment indivdual characteristics - Adults ********
******************************************************************
	use "$rawdata\ExperimentAdult.dta", clear
	keep slno respondent_id dis_code upz_code uni_code village_code age_adult sex_adult general_q1-general_q4
	rename respondent_id 		mid
	rename age_adult 			age_at_exp
	rename sex_adult 			sex_at_exp
	rename general_q1			elder_bro
	rename general_q2			elder_sis
	rename general_q3			younger_bro
	rename general_q4			younger_sis
	sort slno mid
	save "$constructdata\temporary\exp_ind_charc_adult.dta", replace


********************************************************************
**********	Experiment indivdual characteristics - Children ********
********************************************************************
	use "$rawdata\ExperimentChildren.dta", clear
	keep slno respondent_id dis_code upz_code uni_code village_code age_child sex_child general_q1-general_q4
	rename respondent_id 		mid
	rename age_child 			age_at_exp
	rename sex_child 			sex_at_exp
	rename general_q1			elder_bro
	rename general_q2			elder_sis
	rename general_q3			younger_bro
	rename general_q4			younger_sis
	save "$constructdata\temporary\exp_ind_charc_child.dta", replace
	append using "$constructdata\temporary\exp_ind_charc_adult.dta"	
	sort slno mid
	save "$constructdata\temporary\exp_individual_characteristics.dta", replace 
	
**********************************************
**********	Risk Preference - Adults	********
**********************************************
	use "$rawdata\ExperimentAdult.dta", clear

	keep slno respondent_id sex_adult age_adult /*
	*/ risk_q1 risk_q5

	rename respondent_id mid
	clonevar binswanger=risk_q5
	la var binswanger "Lottery number picked"
	
/*Understood the risk preference game or not*/
	ta risk_q1, nol
	gen ustood_risk=0 if risk_q1==4
	replace ustood_risk=1 if risk_q1~=4
	replace ustood_risk=. if risk_q1==.
	la var ustood_risk "Respondent understood risk preference game"

	keep slno mid binswanger ustood_risk 
	sort slno mid
	save "$constructdata\temporary\risk_preference_adult.dta", replace


**********************************************
**********	Time Preference	- Adults ********
**********************************************
	use "$rawdata\ExperimentAdult.dta", clear
	
	rename respondent_id mid
	
	egen byte patient_choices = anycount(time_set_1_q1- time_set_3_q6), values(2)
	gen v=1 if time_set_1_q1- time_set_3_q6~=.
	replace patient_choices=. if v==.
	la var patient_choices "number of patient choices made"

	gen ustood_time=0 if time_q2==4| time_q3==4| time_q4==4
	replace ustood_time=1 if time_q2~=4 & time_q3~=4 & time_q4~=4
	replace ustood_time=. if time_q2==.| time_q3==.| time_q4==.
	la var ustood_time "Respondent understood time preference game"
	
	keep slno mid patient_choices ustood_time
	sort slno mid
	save "$constructdata\temporary\time_number_of_patient_choices_adult.dta", replace
	

**********************************************
**********	Social Preference - Adults	********
**********************************************
	use "$rawdata\ExperimentAdult.dta", clear	

	rename respondent_id mid
	rename others_chioce_q2 ds1 
	rename others_chioce_q3 ds2
	rename others_chioce_q4 ds3
	rename others_chioce_q5 ds4
	
/*social preferences*/
	gen 	spiteful=0 		if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace spiteful=1 		if ds1==2 & ds2==2 & ds3==1 & ds4==1
	
	gen 	egalitarian=0 	if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace egalitarian=1 	if ds1==1 & ds2==1 & ds3==1 & ds4==1

	gen 	altruistic=0 	if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace altruistic=1 	if ds1==1 & ds2==1 & ds3==2 & ds4==2

	gen 	selfish=0 		if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace selfish=1 		if ds1==2		   			& ds4==2

/*Understood the social preference game or not*/
	ta others_chioce_q1, nol
	gen ustood_social=0 if others_chioce_q1==4
	replace ustood_social=1 if others_chioce_q1~=4
	replace ustood_social=. if others_chioce_q1==.
	la var ustood_social "Respondent understood social preference game"

keep slno mid spiteful egalitarian altruistic selfish ustood_social
sort slno mid
save "$constructdata\temporary\social_preference_adult.dta", replace

	
**********************************************
**********	Big Five - Adults	********
**********************************************
use "$rawdata\ExperimentAdult.dta", clear
	rename respondent_id mid
	
	foreach var of varlist big_five_17_q1-big_five_17_q15 {
    replace `var' =. if `var' ==0
} 
		

foreach x of varlist big_five_17_q3 big_five_17_q7 big_five_17_q12 big_five_17_q15 {
  replace `x' = (8 - `x')
}


****** GENERATE BIG FIVE MEASURES BY ADDING UP SCORES ******

*conscientiousness 
gen count=.
replace count = 3

foreach var of varlist big_five_17_q1 big_five_17_q7 big_five_17_q11 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q1 big_five_17_q7 big_five_17_q11), missing
gen conscientiousness = v1/count
drop v1

*extraversion 
replace count = 3
foreach var of varlist big_five_17_q2 big_five_17_q8 big_five_17_q12 {
    replace count = count - 1 if `var' == .
} 
egen v1 = rowtotal(big_five_17_q2 big_five_17_q8 big_five_17_q12), missing
gen extraversion = v1/count
drop v1

*agreeableness 
replace count = 3
foreach var of varlist big_five_17_q3 big_five_17_q6 big_five_17_q13 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q3 big_five_17_q6 big_five_17_q13), missing
gen agreeableness = v1/count
drop v1

*openness 
replace count = 3
foreach var of varlist big_five_17_q4 big_five_17_q9 big_five_17_q14 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q4 big_five_17_q9 big_five_17_q14), missing
gen openness = v1/count
drop v1

*neuroticism 
replace count = 3
foreach var of varlist big_five_17_q5 big_five_17_q10 big_five_17_q15 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q5 big_five_17_q10 big_five_17_q15), missing
gen neuroticism = v1/count
drop v1
drop count

/*Standardizing big five measures*/
foreach var of varlist conscientiousness extraversion agreeableness openness neuroticism {
	egen `var'_std=std(`var')
}

keep slno mid conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std

sort slno mid
save "$constructdata\temporary\big_five_adult.dta", replace



**********************************************
**********	Locus of control - Adults	********
**********************************************
	use "$rawdata\ExperimentAdult.dta", clear

	rename respondent_id mid
	
	foreach var of varlist locus_q1-locus_q28 { //this is to rescale it
    replace `var' =`var' + 1 if `var' ~=.
} 
	

	gen loc_external = locus_q1 +locus_q2 + locus_q4 + locus_q6 + locus_q8 + /*
	*/ locus_q11 + locus_q14 + locus_q17 + locus_q19 + locus_q22 + locus_q23 + /*
	*/ locus_q24 + locus_q26 + locus_q27 if locus_q1-locus_q28~=.

	gen loc_internal = locus_q3 +locus_q5 + locus_q7 + locus_q9 + locus_q10 + /*
	*/ locus_q12 + locus_q13 + locus_q15 + locus_q16 + locus_q18 + locus_q20 + /*
	*/ locus_q21 + locus_q25 + locus_q28 if locus_q1-locus_q28~=.

	gen loc_index = loc_external - loc_internal if loc_internal ~=. & 	loc_external ~=.
	la var loc_index "Locus of Control Index"
	egen loc_std=std(loc_index)

/*Locus of Control*/
	keep slno mid loc_index loc_std
	sort slno mid
	save "$constructdata\temporary\locus_of_control_adult.dta", replace


**********************************************
**********	Risk Preference - Children	********
**********************************************
use "$rawdata\ExperimentChildren.dta", clear
	
	keep slno respondent_id age_child sex_child /*
	*/ risk_2_q1 risk_2_q5 
	
	rename respondent_id mid
	clonevar binswanger=risk_2_q5
	la var binswanger "Lottery number picked"

	/*Understood the social preference game or not*/
	gen ustood_risk=0 if risk_2_q1==4
	replace ustood_risk=1 if risk_2_q1~=4
	replace ustood_risk=. if risk_2_q1==.
	la var ustood_risk "Respondent understood risk preference game"
	
	keep slno mid binswanger ustood_risk  
	sort slno mid
	save "$constructdata\temporary\risk_preference_children.dta", replace	
	

**********************************************
**********	Time Preference	- Children ********
**********************************************
	use "$rawdata\ExperimentChildren.dta", clear
	
	rename respondent_id mid

	egen byte patient_choices = anycount(time_q6-time_q11), values(2)
	gen v=1 if time_q6-time_q11~=.
	replace patient_choices=. if v==.
	la var patient_choices "number of patient choices made"

	gen ustood_time=0 if time_q2==4| time_q3==4| time_q4==4| time_q5==4
	replace ustood_time=1 if time_q2~=4 & time_q3~=4 & time_q4~=4 & time_q5~=4
	replace ustood_time=. if time_q2==.| time_q3==.| time_q4==.| time_q5==.
	la var ustood_time "Child understood time preference game"

	keep slno mid patient_choices ustood_time
	sort slno mid
	save "$constructdata\temporary\time_number_of_patient_choices_children.dta", replace


**********************************************
**********	Soical Preference - Children	********
**********************************************
	use "$rawdata\ExperimentChildren.dta", clear
	
	rename respondent_id mid
	rename others_chioce_q2 ds1 
	rename others_chioce_q3 ds2
	rename others_chioce_q4 ds3
	rename others_chioce_q5 ds4

/*social preferences*/
	gen 	spiteful=0 		if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace spiteful=1 		if ds1==2 & ds2==2 & ds3==1 & ds4==1
	
	gen 	egalitarian=0 	if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace egalitarian=1 	if ds1==1 & ds2==1 & ds3==1 & ds4==1

	gen 	altruistic=0 	if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace altruistic=1 	if ds1==1 & ds2==1 & ds3==2 & ds4==2

	gen 	selfish=0 		if ds1~=. & ds2~=. & ds3~=. & ds4~=.
	replace selfish=1 		if ds1==2		   			& ds4==2

	
	/*Understood the social preference game or not*/
	ta others_chioce_q1, nol /*there is a problem with one id*/
	gen ustood_social=0 if others_chioce_q1==4
	replace ustood_social=1 if others_chioce_q1~=4
	replace ustood_social=. if others_chioce_q1==.
	la var ustood_social "Respondent understood social preference game"

	keep slno mid spiteful egalitarian altruistic selfish ustood_social
	sort slno mid
	save "$constructdata\temporary\social_preference_children.dta", replace
	
	
	
**********************************************
**********	Big Five - Children	********
**********************************************
	use "$rawdata\ExperimentChildren.dta", clear

	//Big five modules are slightly different for children below 12 and 12 and above
	//10 items asked to mothers for younger kids. For the rest, items are similar to adults
	
	foreach var of varlist big_five_6_11_q1-big_five_6_11_q10 {
    replace `var' =. if `var' ==0
	}
	
	rename respondent_id mid
	
/* GENERATE BIG FIVE MEASURES BY ADDING UP SCORES FROM SINGLE ITEMS */

*GENERATE BIG FIVE MEASURES 

g extraversion_1 = big_five_6_11_q1  
g extraversion_2 = big_five_6_11_q6 
g extraversion_ma = .

g conscientiousness_1 = big_five_6_11_q2
g conscientiousness_2 = big_five_6_11_q7
g conscientiousness_ma = .

g openness_1 = big_five_6_11_q4
g openness_2 = big_five_6_11_q9
g openness_ma = .

g agreeableness_1 = big_five_6_11_q3
g agreeableness_2 = big_five_6_11_q8
g agreeableness_ma = .

g neuroticism_1 = big_five_6_11_q5
g neuroticism_2 = big_five_6_11_q10
g neuroticism_ma = .

*Reverse scale for items which are considered negative
foreach var of varlist extraversion_1 conscientiousness_2 openness_2 agreeableness_1 ///
neuroticism_2 {
	replace `var' =  (12 - `var') 
}


*ADDING UP MEASURES

*** Extraversion ***
replace extraversion_ma = (extraversion_1 + extraversion_2)/2 if extraversion_1 != . & extraversion_2 != .
replace extraversion_ma = extraversion_1 if extraversion_1 != . & extraversion_2 == .
replace extraversion_ma = extraversion_2 if extraversion_1 == . & extraversion_2 != .
egen 	extra_ma_std = std(extraversion_ma)
replace extra_ma_std =. if extraversion_ma==.


*** conscientiousness ***
replace conscientiousness_ma = (conscientiousness_1 + conscientiousness_2)/2 if conscientiousness_1 != . & conscientiousness_2 != .
replace conscientiousness_ma = conscientiousness_1 if conscientiousness_1 != . & conscientiousness_2 == .
replace conscientiousness_ma = conscientiousness_2 if conscientiousness_1 == . & conscientiousness_2 != .
egen 	con_ma_std = std(conscientiousness_ma)
replace con_ma_std=. if conscientiousness_ma==. 

*** Openness ***
replace openness_ma = (openness_1 + openness_2)/2 if openness_1 != . & openness_2 != .
replace openness_ma = openness_1 if openness_1 != . & openness_2 == .
replace openness_ma = openness_2 if openness_1 == . & openness_2 != .
egen 	open_ma_std = std(openness_ma)
replace open_ma_std =.  if openness_ma==.

***agreeableness***
replace agreeableness_ma = (agreeableness_1 + agreeableness_2)/2 if agreeableness_1 != . & agreeableness_2 != .
replace agreeableness_ma = agreeableness_1 if agreeableness_1 != . & agreeableness_2 == .
replace agreeableness_ma = agreeableness_2 if agreeableness_1 == . & agreeableness_2 != .
egen 	agree_ma_std = std(agreeableness_ma)
replace agree_ma_std =. if agreeableness_ma==.

***neuroticism***
replace neuroticism_ma = (neuroticism_1 + neuroticism_2)/2 if neuroticism_1 != . & neuroticism_2 != .
replace neuroticism_ma = neuroticism_1 if neuroticism_1 != . & neuroticism_2 == .
replace neuroticism_ma = neuroticism_2 if neuroticism_1 == . & neuroticism_2 != .
egen 	neuro_ma_std = std(neuroticism_ma)
replace neuro_ma_std =. if neuroticism_ma==.


*LABELING
la var extraversion_ma "extraversion of child evaluated by mother"
la var conscientiousness_ma "conscientiousness of child evaluated by mother"
la var openness_ma "openness of the child evaluated by mother"
la var agreeableness_ma "agreeableness of the child evaluated by mother"
la var neuroticism_ma "neuroticism of the child evaluated by mother"

foreach x of varlist big_five_12_17_q3 big_five_12_17_q7 big_five_12_17_q12 big_five_12_17_q15 {
  replace `x' = (8 - `x')
}


****** GENERATE BIG FIVE MEASURES BY ADDING UP SCORES ******

*conscientiousness 
gen count=.
replace count = 3

foreach var of varlist big_five_12_17_q1 big_five_12_17_q7 big_five_12_17_q11 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q1 big_five_12_17_q7 big_five_12_17_q11), missing
gen conscientiousness = v1/count
drop v1

*extraversion 
replace count = 3
foreach var of varlist big_five_12_17_q2 big_five_12_17_q8 big_five_12_17_q12 {
    replace count = count - 1 if `var' == .
} 
egen v1 = rowtotal(big_five_12_17_q2 big_five_12_17_q8 big_five_12_17_q12), missing
gen extraversion = v1/count
drop v1

*agreeableness 
replace count = 3
foreach var of varlist big_five_12_17_q3 big_five_12_17_q6 big_five_12_17_q13 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q3 big_five_12_17_q6 big_five_12_17_q13), missing
gen agreeableness = v1/count
drop v1

*openness 
replace count = 3
foreach var of varlist big_five_12_17_q4 big_five_12_17_q9 big_five_12_17_q14 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q4 big_five_12_17_q9 big_five_12_17_q14), missing
gen openness = v1/count
drop v1

*neuroticism 
replace count = 3
foreach var of varlist big_five_12_17_q5 big_five_12_17_q10 big_five_12_17_q15 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q5 big_five_12_17_q10 big_five_12_17_q15), missing
gen neuroticism = v1/count
drop v1
drop count

foreach var of varlist conscientiousness extraversion agreeableness openness neuroticism {
	egen `var'_std=std(`var')
}
	
replace conscientiousness_std = con_ma_std 		if conscientiousness_std==. & conscientiousness_ma~=.
replace extraversion_std 	  = extra_ma_std 	if extraversion_std	 ==. & extraversion_ma~=.
replace openness_std 		  = open_ma_std 	if openness_std		 ==. & openness_ma~=.
replace neuroticism_std 	  = neuro_ma_std 	if neuroticism_std		 ==. & neuroticism_ma~=.
replace agreeableness_std	  = agree_ma_std	if agreeableness_std	 ==. & agreeableness_ma~=.
	
	keep slno mid conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std
	
	sort slno mid
	save "$constructdata\temporary\big_five_children.dta", replace


**********************************************
**********	Locus of control - Children	********
**********************************************
	use "$rawdata\ExperimentChildren.dta", clear
	rename respondent_id mid
	sum locus_q1-locus_q5/* 1812 observations*/

	gen loc_external = locus_q2 +locus_q3 + locus_q4 + locus_q5 
	gen loc_internal = locus_q1 

	gen loc_index = loc_external - loc_internal if loc_internal ~=. & 	loc_external ~=.
	la var loc_index "Locus of Control Index"
	egen loc_std=std(loc_index)
			
/*Locus of Control*/
	keep slno mid loc_index loc_std
	sort slno mid
	save "$constructdata\temporary\locus_of_control_children.dta", replace


**********************************************
**********	Full Scale IQ measure - Adults	********
**********************************************
	use "$rawdata\cognitive_data_adult_2015.dta", clear
	gen FSIQ=VCI + PRI + WMI + PSI
	replace FSIQ=236 if FSIQ>236 & FSIQ~=.
	label var FSIQ "Full Scale IQ" 
	egen FSIQ_std=std(FSIQ)
	la var FSIQ_std "Standardized FSIQ measure"
	keep slno mid FSIQ_std 
	sort slno mid 
	save "$constructdata\temporary\cognitive_adult.dta", replace


**********************************************
**********	Full Scale IQ measure - Children	********
**********************************************
	use "$rawdata\cognitive_data_children_2015.dta", clear
	gen FSIQ=VCI + PRI + WMI + PSI
	replace FSIQ=226 if FSIQ>226 & FSIQ~=.
	label var FSIQ "Full Scale IQ" 
	egen FSIQ_std=std(FSIQ)
	la var FSIQ_std "Standardized FSIQ measure"
	keep slno mid FSIQ_std 
	sort slno mid 
	save "$constructdata\temporary\cognitive_children.dta", replace
	

	
**********************************************
**********	Baseline (2014) Individual Characteristics	********
**********************************************
	use "$rawdata\Section_1_1.dta", clear
	rename s1_1_q1 mid
	gen gender=s1_1_q3==1
	drop s1_1_q3
	la var gender 		"female=0, male=1"
	la var slno 		"household serial number"
	la var mid  		"member serial number"	
	la var s1_1_q6_1    "Age (Year) in 2014"
	la var s1_1_q6_2  	"Age (Month) in 2014"
	la var s1_1_q7      "Can read or write (2014)?" 
	la var s1_1_q8      "Currently studying (2014)"
	la var s1_1_q9_1    "Highest grade attained (2014)"
	la var s1_1_q13_1	"Occupation -primary (2014)"
		
	sort slno mid
	save "$constructdata\temporary\baseline_individual_characteristics.dta", replace

	
**********************************************
**********	Baseline (2014) Household Characteristics ********
**********************************************
	//Household size
	use "$rawdata\Section_1_1.dta", clear
	bysort slno: generate hhsize=_N
	bysort slno: gen n1=_n
	keep if n1==1
	keep slno hhsize 
	la var hhsize		"Household size (2014)"
	sort slno 
	save "$constructdata\temporary\hhsize.dta", replace

	//Grand parents
	use "$rawdata\Section_1_1.dta", clear
	rename s1_1_q1 mid
	sort slno mid
	keep if s1_1_q5==5| s1_1_q5==6 //based on the relationship with the household head
	gen grand_parents=1
	bysort slno: gen n=_n
	keep if n==1
	keep slno grand_parents
	la var grand_parents "Grand parents are present"
	sort slno
	save "$constructdata\temporary\grandparents.dta", replace

	//Land ownership
	use "$rawdata\Section_5_1.dta", clear
	bysort slno: egen land=sum(s5_1_q3 )
	bysort slno: gen n1=_n
	keep if n1==1
	keep slno land 
	la var land		"Amount of land owned (in decims) (in 2014)"
	sort slno 
	save "$constructdata\temporary\land.dta", replace

	//merge them to create baseline household characteristics
	use  "$constructdata\temporary\hhsize.dta", clear
	merge 1:1 slno using "$constructdata\temporary\grandparents.dta"
	replace grand_parents=0 if grand_parents==.
	drop _m
	
	merge 1:1 slno using "$constructdata\temporary\land.dta"
	drop _m
	sort slno
	save "$constructdata\temporary\baseline_household_characteristics.dta", replace
	
************************************************
********** Combining all the variables	********
************************************************
	//time preference - patient choices
	use 			"$constructdata\temporary\time_number_of_patient_choices_children.dta", clear
	append using  	"$constructdata\temporary\time_number_of_patient_choices_adult.dta"
	sort slno mid
	save 			"$constructdata\temporary\time_patient_choices.dta", replace 
	
	//risk preference
	use 			"$constructdata\temporary\risk_preference_children.dta", clear
	append using  	"$constructdata\temporary\risk_preference_adult.dta"
	sort slno mid
	save 			"$constructdata\temporary\risk.dta", replace 

	//social preferences
	use 			"$constructdata\temporary\social_preference_children.dta", clear
	append using  	"$constructdata\temporary\social_preference_adult.dta"
	sort slno mid
	save 			"$constructdata\temporary\social.dta", replace 
	
	//big-five 
	use 			"$constructdata\temporary\big_five_children.dta", clear
	append using  	"$constructdata\temporary\big_five_adult.dta"
	sort slno mid
	save 			"$constructdata\temporary\bigfive.dta", replace 
	
	//locus of control
	use 			"$constructdata\temporary\locus_of_control_children.dta", clear
	append using  	"$constructdata\temporary\locus_of_control_adult.dta"
	sort slno mid
	save 			"$constructdata\temporary\locus.dta", replace 

	//cognitive ability
	use 			"$constructdata\temporary\cognitive_children.dta", clear
	append using  	"$constructdata\temporary\cognitive_adult.dta"
	sort slno mid
	save 			"$constructdata\temporary\cognitive.dta", replace 
	
	//Baseline (2014) individual characteristics
	use "$constructdata\temporary\baseline_individual_characteristics.dta", clear 
	merge m:m slno mid using "$constructdata\temporary\exp_individual_characteristics.dta" //merging with experiment (2016) individual characteristics
	rename _m m1
	
	//Merging with time preferences
	merge m:m slno mid using "$constructdata\temporary\time_patient_choices.dta" 
	rename _m m3
	
	//we drop for whom no preferences data were collected 
	drop if m1==1 & m3==1
	drop m1 m3 
	//Merging with risk preferences
	merge m:m slno mid using "$constructdata\temporary\risk.dta"
	drop _m 
	
	//Merging with social preferences
	merge m:m slno mid using "$constructdata\temporary\social.dta"
	drop _m 
	//Merging with congnitive ability
	merge m:m slno mid using "$constructdata\temporary\cognitive.dta"
	keep if _m==3 //we drop if cognitive data were not collected
	drop _m 
	
	//Merging with big-five
	merge m:m slno mid using "$constructdata\temporary\bigfive.dta"
	drop _m
	
	//Merging with locus of control
	merge m:m slno mid using "$constructdata\temporary\locus.dta"
	drop _m
	
	//Merging baseline household characteristics
	merge m:1 slno using "$constructdata\temporary\baseline_household_characteristics.dta"
	drop _m
	
	//We add income data collected in 2016
	merge m:1 slno using "$rawdata\income2016.dta"
	drop _m
	
	//Baseline village characteristics
	merge m:1 village_code using "$rawdata\village.dta"
	drop if _m==2
	drop _m

	order slno mid village_code uni_code upz_code dis_code
	save "$constructdata\temporary\data_temporary.dta", replace 
	
************************************************
********** Recoding and labelling of some variables	********
************************************************
	//Schooling 
	clonevar v1=s1_1_q9_1 
	replace v1=0 if v1==99  //replacing never attended by 0
	replace v1=0 if v1==22|v1==22|v1==23 |v1==24  //replacing pre-primary, mosque, temple, hafizia madrassa by 0
	replace v1=12 if v1==20|v1==21 //replacing vocational, and diploma by 12
	gen schooling = v1
	la var schooling "years of schooling"
	drop v1  
	
	//Currently attending school
	clonevar school_attending =s1_1_q8
	replace school_attending=0 if school_attending==2
	la var school_attending "currently attending school=1, 0 otherwise"
	drop s1_1_q8
	
///Getting the parents' age, education, & profession
	//Age
	bysort slno: gen v1=s1_1_q6_1 + 2 if gender==1 & s1_1_q5<=2 
	egen age_father = max(v1), by(slno)
	replace age_father=. if  s1_1_q5<=2
	drop v1
	
	bysort slno: gen v1=s1_1_q6_1 + 2 if gender==0 & s1_1_q5<=2 
	egen age_mother = max(v1), by(slno)
	replace age_mother=. if  s1_1_q5<=2
	drop v1
	
	//Education
	bysort slno: gen v1=s1_1_q9_1 if gender==1 & s1_1_q5<=2
	replace v1=0 if v1==99 /* replacing never attended by 0*/
	replace v1=0 if v1==22|v1==22|v1==23 | v1==24
	/* replacing pre-primary, mosque, temple, hafizia madrassa by 0*/
	replace v1=12 if v1==20|v1==21 /* replacing vocational, and diploma by 12*/
	egen schooling_father = max(v1), by(slno)
	replace schooling_father =. if  s1_1_q5<=2
	drop v1

	bysort slno: gen v1=s1_1_q9_1 if gender==0 & s1_1_q5<=2
	replace v1=0 if v1==99 /* replacing never attended by 0*/
	replace v1=0 if v1==22|v1==22|v1==23 |v1==24
	/* replacing pre-primary, mosque, temple, hafizia madrassa by 0*/
	replace v1=12 if v1==20|v1==21 /* replacing vocational, and diploma by 12*/
	egen schooling_mother = max(v1), by(slno)
	replace schooling_mother =. if  s1_1_q5<=2
	drop v1

	//Profession
	clonevar v1=s1_1_q13_1
	gen v2= 0 if v1>=1 & v1<=10 /*agriculture=1 to 10*/
	replace v2=1 if v1>=11 & v1<=32|v1>=35 & v1<=37| v1==47 |v1==52
	/*self-employed non-ag 11-32, 35-37, 47, 52*/
	replace v2=2 if v1==33 |v1>=38 & v1<=43| v1==57
	/*non-ag workers (construction) 33, 38-43, 57*/
	replace v2=3 if v1==44 |v1==50 | v1==54| v1==55 | v1==56
	/*professional 44 50, 54, 55, 56*/
	replace v2=4 if v1==64 |v1==65 /*unemployed 64, 65*/
	replace v2=5 if v1==61 |v1==67 /*housewife, household works 61, 67*/
	replace v2=6 if v1==62 |v1==63 /*students, children 63, 62*/
	replace v2=7 if v1==66 |v1==68 /*others 68, 66*/

	bysort slno: gen v3=v2 if gender==1 & s1_1_q5<=2
	egen prof_father = max(v3), by(slno)
	replace prof_father =. if  s1_1_q5<=2
	drop v3
	
	bysort slno: gen v3=v2 if gender==0 & s1_1_q5<=2
	egen prof_mother = max(v3), by(slno)
	replace prof_mother =. if  s1_1_q5<=2
	drop v1 v2 v3

	
//Getting the parents cognitive ability and preferences
	foreach V in FSIQ_std patient_choices binswanger spiteful egalitarian altruistic selfish{
	bysort slno: gen v1 = `V' if gender==1 & s1_1_q5<=2 
	egen `V'_father=max(v1), by(slno)
	replace `V'_father=. if  s1_1_q5<=2
	drop v1
	}
	
	foreach V in FSIQ_std patient_choices binswanger spiteful egalitarian altruistic selfish{
	bysort slno: gen v1 = `V' if gender==0 & s1_1_q5<=2 
	egen `V'_mother=max(v1), by(slno)
	replace `V'_mother=. if  s1_1_q5<=2
	drop v1
	}

	//missing parents observations need to be dropped 
gen v1=1
foreach var in  age_father age_mother schooling_father schooling_mother prof_father prof_mother FSIQ_std_father patient_choices_father binswanger_father spiteful_father egalitarian_father altruistic_father selfish_father FSIQ_std_mother patient_choices_mother binswanger_mother spiteful_mother egalitarian_mother altruistic_mother selfish_mother {
 replace v1=0 if `var'==.
}

keep if s1_1_q5==3 & age_at_exp<=17 // we retrict the sample to children only
keep if age_mother>=22 & age_mother<=72 // parents restricted to 22 to 72 years of age 
keep if v1==1 // non-missing preference data 
drop v1
//missing experimental observations need to be dropped 
gen sample_full_info=1
foreach var in gender age_at_exp sex_at_exp elder_bro elder_sis younger_bro younger_sis patient_choices ustood_time spiteful egalitarian altruistic selfish ustood_social FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
	replace sample_full_info=0 if `var'==.
}
keep if sample_full_info==1

save "$constructdata\temporary\children_temporary.dta", replace 


***********************************************************************
*********** Parents data **********************************************
***********************************************************************
	use "$constructdata\temporary\children_temporary.dta", clear
	keep slno sample_full_info
	bysort slno: gen n1=_n
	sum n1
	keep if n1==1
	drop n1
	sort slno
	merge 1:m slno using "$constructdata\temporary\data_temporary.dta"
	keep if _m==3 //to match with the children data 
	drop _m
	//number of children
	bysort slno: egen v1=count(mid) if mid>2 & s1_1_q5==3 
	egen children=max(v1), by(slno)
	drop v1
	la var children "Number of children"
	
	keep if mid<=2 // we keep parents only 
	drop if age_at_exp>72
	
	clonevar relationship=s1_1_q5
	
	//Schooling 
	clonevar v1=s1_1_q9_1 
	replace v1=0 if v1==99  //replacing never attended by 0
	replace v1=0 if v1==22|v1==22|v1==23 |v1==24  //replacing pre-primary, mosque, temple, hafizia madrassa by 0
	replace v1=12 if v1==20|v1==21 //replacing vocational, and diploma by 12
	gen schooling = v1
	la var schooling "years of schooling"
	drop v1  
	
	//Differences in age and schooling
	foreach V in age_at_exp schooling { 
	bysort slno: gen v1 = `V' if relationship==1 & gender==1 
	egen `V'_husband=max(v1), by(slno)
	bysort slno: gen v2 = `V' if relationship==2 & gender==0
	egen `V'_wife=max(v2), by(slno)
	gen `V'_diff=abs(`V'_husband-`V'_wife)
	replace `V'_husband=. if  relationship==1
	replace `V'_wife=. if  relationship==2
	drop v1 v2 
	}
	rename age_at_exp_husband age_husband
	rename age_at_exp_wife age_wife
	rename age_at_exp_diff age_diff
	rename schooling_diff schol_diff
	
	//Profession
	clonevar v1=s1_1_q13_1
	gen v2= 0 if v1>=1 & v1<=10 /*agriculture=1 to 10*/
	replace v2=1 if v1>=11 & v1<=32|v1>=35 & v1<=37| v1==47 |v1==52
	/*self-employed non-ag 11-32, 35-37, 47, 52*/
	replace v2=2 if v1==33 |v1>=38 & v1<=43| v1==57
	/*non-ag workers (construction) 33, 38-43, 57*/
	replace v2=3 if v1==44 |v1==50 | v1==54| v1==55 | v1==56
	/*professional 44 50, 54, 55, 56*/
	replace v2=4 if v1==64 |v1==65 /*unemployed 64, 65*/
	replace v2=5 if v1==61 |v1==67 /*housewife, household works 61, 67*/
	replace v2=6 if v1==62 |v1==63 /*students, children 63, 62*/
	replace v2=7 if v1==66 |v1==68 /*others 68, 66*/
	rename v2 profession 
	la var profession "1 ag, 2 self-empl, 3 professional, 4 unemployed, 5 housewife, 6 students, 7 others"
	drop v1 
	
	//preferences
	foreach V in patient_choices binswanger spiteful egalitarian altruistic selfish { 
	bysort slno: gen v1 = `V' if relationship==1 & gender==1 
	egen `V'_husband=max(v1), by(slno)
	replace `V'_husband=. if  relationship==1
	drop v1
	}
	
	foreach V in patient_choices binswanger spiteful egalitarian altruistic selfish { 
	bysort slno: gen v1 = `V' if relationship==2 & gender==0
	egen `V'_wife=max(v1), by(slno)
	replace `V'_wife=. if  relationship==2
	drop v1
	}
		
	
	gen 	patient_choices_spouse=patient_choices_wife if relationship==1 & gender==1
	replace patient_choices_spouse=patient_choices_husband if relationship==2 & gender==0
	
	gen 	binswanger_spouse=binswanger_wife if relationship==1 & gender==1
	replace binswanger_spouse=binswanger_husband if relationship==2 & gender==0
	
	gen 	spiteful_spouse=spiteful_wife if relationship==1 & gender==1
	replace spiteful_spouse=spiteful_husband if relationship==2 & gender==0
	gen 	egalitarian_spouse=egalitarian_wife if relationship==1 & gender==1
	replace egalitarian_spouse=egalitarian_husband if relationship==2 & gender==0
	gen 	altruistic_spouse=altruistic_wife if relationship==1 & gender==1
	replace altruistic_spouse=altruistic_husband if relationship==2 & gender==0
	gen 	selfish_spouse=selfish_wife if relationship==1 & gender==1
	replace selfish_spouse=selfish_husband if relationship==2 & gender==0
	
	duplicates drop slno mid village_code uni_code upz_code dis_code, force //there are four duplicates
	bysort slno: gen n2=_N 
	ta n2 // there are three household only with one parent
	drop if n2==1
	drop n2
	
	save "$constructdata\parents.dta", replace 
	
	//we go back to the children data 
	
	keep slno sample_full_info
	bysort slno: gen n1=_n
	sum n1
	keep if n1==1
	drop n1
	sort slno
	merge 1:m slno using "$constructdata\temporary\children_temporary.dta"
	keep if _m==3
	drop _m
	save "$constructdata\children.dta", replace 
	
	
*************************************************************************
**** Table 10. Construct Variables used in Table 10 *********
*************************************************************************	
	use "$constructdata\children.dta", clear 
	sort slno
	merge m:1 slno using "$rawdata\parenting_style.dta"
	*unique slno if _m==3
	keep if _m==3

	gl styles style_ew_std style_mon_std style_neg_std style_pc_std style_sc_std
	pca $styles, comp(2) // here we stick with 2 becuase thats what is suggested according parallel analysis 
	rotate, varimax blanks (.3)
	rotate, varimax
	rotate, clear 
	// when the loadings are less than .3 they are considered too small to make up the component. 

	***** predicting component score ******
	predict pc1 pc2, score
	// the components are named based on the characteristics of the vars they contain. In our case negative parenting includes strict traits, and positive includes warm traits. This is based on the classic Baumrind classifications 
	rename pc1 negative_parenting 
	rename pc2 positive_parenting 

	la var negative_parenting "negative_parenting_pca"
	la var positive_parenting "positive_parenting_pca"
	
	save "$constructdata\table10.dta", replace
	

*************************************************************************
**** Table 11. Construct Variables used in Table 11 *********
*************************************************************************
	use "$constructdata\parents.dta", clear

	/*Controls*/
	loc background 			"gender age_at_exp schooling i.profession children"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc household 		  	"hhsize inc_per_cap_per_month_2016"
	loc con_land			"land hhsize foodexp nonfood_month"
	
	loc cognitive			"FSIQ"
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	loc vill				"population"
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	/*cluster*/
	loc unit slno //clustering unit, we are using households
	loc f_effect "i.district_code"
	
	/*Social preferences Variables*/
					
	xi: probit spiteful  `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	predict pr_spit
	xi: probit egalitarian  `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	predict pr_egal
	xi: probit altruistic  `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	predict pr_altru
	xi: probit selfish  `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	predict pr_slfsh

foreach V in pr_spit pr_egal pr_altru pr_slfsh {
	bysort slno: gen v1=`V' if mid==1 & gender==1 
	egen `V'_f=max(v1), by(slno)
	
	bysort slno: gen v2=`V' if mid==2 & gender==0 
	egen `V'_m=max(v2), by(slno)
	drop v1 v2
	}
	
	keep slno mid gender pr_* 
	keep if mid==1
	drop mid gender pr_spit pr_egal pr_altru pr_slfsh
	
	sort slno 
	merge 1:m slno using "$constructdata\children.dta"

	
//patient choices	
	gen v3 =abs(patient_choices_father-patient_choices_mother) if patient_choices_father~=.& patient_choices_mother~=.
	egen v4=sd(v3)
	gen pc_homo=1 if v3<v4 & patient_choices_father~=.& patient_choices_mother~=.
	replace pc_homo=0 if v3>=v4 & patient_choices_father~=.& patient_choices_mother~=.
	drop v3 v4
	
//risk -binswanger	
	gen v3 =abs(binswanger_father-binswanger_mother) if binswanger_father~=.& binswanger_mother~=.
	egen v4=sd(v3)
	gen bins_homo=1 if v3<v4 & binswanger_father~=.& binswanger_mother~=.
	replace bins_homo=0 if v3>=v4 & binswanger_father~=.& binswanger_mother~=.
	drop v3 v4

//spiteful	
	gen v3 =abs(pr_spit_f-pr_spit_m) if pr_spit_f~=.& pr_spit_m~=.
	egen v4=sd(v3)
	gen spit_homo=1 if v3<v4 & pr_spit_f~=.& pr_spit_m~=.
	replace spit_homo=0 if v3>=v4 & pr_spit_f~=.& pr_spit_m~=.
	drop v3 v4

//egalitarian	
	gen v3 =abs(pr_egal_f-pr_egal_m) if pr_egal_f~=.& pr_egal_m~=.
	egen v4=sd(v3)
	gen egal_homo=1 if v3<v4 & pr_egal_f~=.& pr_egal_m~=.
	replace egal_homo=0 if v3>=v4 & pr_egal_f~=.& pr_egal_m~=.
	drop v3 v4

//altruistic	
	gen v3 =abs(pr_altru_f-pr_egal_m) if pr_altru_f~=.& pr_altru_m~=.
	egen v4=sd(v3)
	gen altru_homo=1 if v3<v4 & pr_altru_f~=.& pr_altru_m~=.
	replace altru_homo=0 if v3>=v4 & pr_altru_f~=.& pr_altru_m~=.
	drop v3 v4

//selfish	
	gen v3 =abs(pr_slfsh_f-pr_slfsh_m) if pr_slfsh_f~=.& pr_slfsh_m~=.
	egen v4=sd(v3)
	gen slfsh_homo=1 if v3<v4 & pr_slfsh_f~=.& pr_slfsh_m~=.
	replace slfsh_homo=0 if v3>=v4 & pr_slfsh_f~=.& pr_slfsh_m~=.
	drop v3 v4

	drop  pr_spit_f pr_spit_m pr_egal_f pr_egal_m pr_altru_f pr_altru_m pr_slfsh_f pr_slfsh_m

	
	gen pc_homo_f=pc_homo*patient_choices_father
	gen pc_homo_m=pc_homo*patient_choices_mother
	
	gen bins_homo_f=bins_homo*binswanger_father
	gen bins_homo_m=bins_homo*binswanger_mother
	
	gen spit_homo_f=spit_homo*spiteful_father
	gen spit_homo_m=spit_homo*spiteful_mother
	
	gen egal_homo_f=egal_homo*egalitarian_father
	gen egal_homo_m=egal_homo*egalitarian_mother
	
	gen altru_homo_f=altru_homo*altruistic_father
	gen altru_homo_m=altru_homo*altruistic_mother
	
	gen slfsh_homo_f=slfsh_homo*selfish_father
	gen slfsh_homo_m=slfsh_homo*selfish_mother

	save "$constructdata/table11.dta", replace

****************************************************************************************************************
*** Table 12: construct variables used in Table 12 ***
****************************************************************************************************************	
// The estimation involves two stepts: 
// In the first step, we regress elder child's preference only on parents preference and predict the residulas
// In the second step, we use the residuls to predict younger sibling's preference on other elder child's preference

	use "$constructdata\children.dta", clear 
	
	//prefernce residual calculation

	// Patient choices
	reg patient_choices patient_choices_father patient_choices_mother  if s1_1_q5==3  & ustood_time==1, cl(`unit')
	predict pat_cho_res, residuals 
	
	// Risk - Lottery number picked
	reg binswanger  binswanger_father binswanger_mother if s1_1_q5==3  & ustood_risk==1, cl(`unit')
	predict binsw_res, residuals

	// Spiteful
	xi: dprobit spiteful  spiteful_father spiteful_mother  if s1_1_q5==3 &  ustood_social==1, cl(`unit')
	predict spit_res, dev			

	// Egalitarian
	xi: dprobit egalitarian  egalitarian_father egalitarian_mother  if s1_1_q5==3 & ustood_social==1, cl(`unit')
	predict egal_res, dev	
	
	// Altruistic
	xi: dprobit altruistic  altruistic_father altruistic_mother if s1_1_q5==3 & ustood_social==1, cl(`unit')
	predict altru_res, dev
	
	// Selfish
	xi: dprobit selfish  selfish_father selfish_mother  if s1_1_q5==3 & ustood_social==1, cl(`unit')
	predict self_res, dev
	
	// adding siblings prefernce residual
	egen v1=max(age_at_exp), by(slno) 	
	egen v2=count(mid), by (slno)
	gen elder=1 if age_at_exp>=v1 & age_at_exp~=. & v2>1
	replace elder=0 if elder==. & age_at_exp~=. & v2>1
	la var elder "elder sibling =1, younger sibling=0" // we have 129 children who do not have elder siblings in our data
	drop v1 v2
	
	foreach V in pat_cho_res binsw_res spit_res egal_res altru_res self_res {
		bysort slno: gen v1=`V' if elder==1
		egen `V'_sibling=max(v1), by(slno)
		drop v1
	}

sort slno mid

bysort slno: gen n=_n
gen v1 =0 if elder==0 & n==1
drop if v1==0

save "$constructdata\table12.dta", replace

********************************************************************************************************************************
*** Table 13: Construct variables used in Table 13 ***
********************************************************************************************************************************
use "$constructdata\children.dta", clear 

//average village preference calculation

foreach V in patient_choices binswanger spiteful egalitarian altruistic selfish {
	bysort village_code: gen v1=_n 	     /*to get the respondent number in the next step*/
	egen v2=max(v1), by(village_code)    /*the total number of respondent per village*/
	egen v3=total(`V'), by(village_code) /*the total preference at the village level */
	gen v4=v3-`V' if `V'~=.				/*the total preference - minus the respondent's preference*/
	gen `V'_avg=v4/(v2-1)				/*average village preference */
	drop v1 v2 v3 v4
	}
		
	
save "$constructdata\table13.dta", replace


*************************************************************************
**** Tables A.1. & A.2  Construct Data used in Tables A.1 and A2 *********
*************************************************************************
	//cognitive ability survey sample of 1000 hosueholds
	use "$rawdata\cognitive_data_adult_2015.dta", clear
	keep if mid<=2
	gen 	missing1=.
	replace missing1=1 if VCI~=.
	replace missing1=1 if PRI~=.
	replace missing1=1 if WMI~=.
	replace missing1=1 if PSI~=.
	replace missing1=0 if missing1==.
	bysort slno: egen missing2=sum(missing1)
	bysort slno: gen n=_n
	keep if n==1
	rename missing2 congitive_sample_types
	la var congitive_sample_types "0 - both parents missing, 1 - parent missing, 2 - none missing"
	keep slno congitive_sample_types
	gen cognitive_sample=1 
	sort slno 
	save "$constructdata\temporary\sample_cognitive", replace 
	

	//Baseline individual charcteristics collected in 2014 
	use "$constructdata\temporary\baseline_individual_characteristics.dta", clear
	keep if mid<=2

	//Schooling 
	clonevar v1=s1_1_q9_1 
	replace v1=0 if v1==99  //replacing never attended by 0
	replace v1=0 if v1==22|v1==22|v1==23 |v1==24  //replacing pre-primary, mosque, temple, hafizia madrassa by 0
	replace v1=12 if v1==20|v1==21 //replacing vocational, and diploma by 12
	gen schooling = v1
	la var schooling "years of schooling"
	drop v1  
	//Parents education  
	bysort slno: gen v1=schooling if gender==1 & s1_1_q5<=2
	egen schooling_father = max(v1), by(slno)
	drop v1
	bysort slno: gen v1=schooling if gender==0 & s1_1_q5<=2
	egen schooling_mother = max(v1), by(slno)
	drop v1

	//Parents age  
	bysort slno: gen v1= s1_1_q6_1 if gender==1 & s1_1_q5<=2
	egen age_father = max(v1), by(slno)
	drop v1
	bysort slno: gen v1=s1_1_q6_1 if gender==0 & s1_1_q5<=2
	egen age_mother = max(v1), by(slno)
	drop v1
	
	bysort slno: gen n=_n
	keep if n==1
	
	keep slno schooling_father schooling_mother age_father age_mother
	sort slno
	merge 1:1 slno using "$constructdata\temporary\baseline_household_characteristics.dta"
	drop _m
	gen all_4500hhs=1

	//merging with identity 2014 data
	merge 1:1 slno using "$rawdata\identity_2014.dta"
	drop _m
	order slno q3_district_code q4_upazila_code q5_union_code q6_village_code
	rename q3_district_code dis_code
	rename q4_upazila_code upz_code
	rename q5_union_code uni_code
	rename q6_village_code village_code

	//village population
	sort village_code
	merge m:1 village_code using "$rawdata\village.dta"
	drop _m
	drop district_code upazila_code union_code
	sort slno 
	
	//income 2016
	merge 1:1 slno using "$rawdata\income2016.dta"
	drop if _m==2
	drop _m

	merge 1:1 slno using "$constructdata\temporary\sample_cognitive"
	drop _m
	save "$constructdata\data_with_different_samples.dta", replace


*************************************************************************
*** Table A.6: Children’s time consistency and its relation to parents’ time consistencies ***
*************************************************************************
	//time consistency - adults
	use "$rawdata\ExperimentAdult.dta", clear
	
	keep slno respondent_id sex_adult /*
*/time_q2 time_q3 time_q4 /*
*/time_set_1_q1 time_set_1_q2 time_set_1_q3 time_set_1_q4 time_set_1_q5 time_set_1_q6 /*
*/time_set_2_q1 time_set_2_q2 time_set_2_q3 time_set_2_q4 time_set_2_q5 time_set_2_q6 /*
*/time_set_3_q1 time_set_3_q2 time_set_3_q3 time_set_3_q4 time_set_3_q5 time_set_3_q6
	rename respondent_id mid
	keep if mid<=2 // parents only 
	//In order to match with children data, we use the first two choice sets for adults, and the last two choice sets for children
	//discount rate, time set 1
	gen d3_1=0 if time_set_1_q1==1
	replace d3_1=1 if time_set_1_q1==2  

	gen d3_2=0 if d3_1~=.
	replace d3_2=1 if time_set_1_q2==2  
	replace d3_2=0 if d3_1==1  

	gen d3_3=0 if d3_2~=.
	replace d3_3=1 if time_set_1_q3==2  
	replace d3_3=0 if d3_1==1|d3_2==1 

	gen d3_4=0 if d3_3~=.
	replace d3_4=1 if time_set_1_q4==2  
	replace d3_4=0 if  d3_1==1|d3_2==1 |d3_3==1 

	gen d3_5=0 if d3_4~=.
	replace d3_5=1 if time_set_1_q5==2  
	replace d3_5=0 if  d3_1==1|d3_2==1 |d3_3==1 |d3_4==1 

	gen d3_6=0 if d3_5~=.
	replace d3_6=1 if time_set_1_q6==2  
	replace d3_6=0 if  d3_1==1|d3_2==1 |d3_3==1 | d3_4==1 | d3_5==1 

	gen d3_7=0 if d3_6~=.
	replace d3_7=1 if d3_1==0 & d3_2==0 & d3_3==0 & d3_4==0 & d3_5==0 & d3_6==0  

	gen     d3=0   if d3_7~=.
	replace d3=0.1 if d3_1==1 
	replace d3=0.3 if d3_2==1
	replace d3=0.6 if d3_3==1
	replace d3=0.9 if d3_4==1
	replace d3=1.5 if d3_5==1
	replace d3=3   if d3_6==1
	replace d3=5   if d3_7==1

	//discount rate, time set 2
	gen d4_1=0 if time_set_2_q1==1
	replace d4_1=1 if time_set_2_q1==2  
	
	gen d4_2=0 if d4_1~=.
	replace d4_2=1 if time_set_2_q2==2  
	replace d4_2=0 if d4_1==1  

	gen d4_3=0 if d4_2~=.
	replace d4_3=1 if time_set_2_q3==2  
	replace d4_3=0 if d4_1==1|d4_2==1 

	gen d4_4=0 if d4_3~=.
	replace d4_4=1 if time_set_2_q4==2  
	replace d4_4=0 if  d4_1==1|d4_2==1 |d4_3==1 

	gen d4_5=0 if d4_4~=.
	replace d4_5=1 if time_set_2_q5==2  
	replace d4_5=0 if  d4_1==1|d4_2==1 |d4_3==1 |d4_4==1 

	gen d4_6=0 if d4_5~=.
	replace d4_6=1 if time_set_2_q6==2  
	replace d4_6=0 if  d4_1==1|d4_2==1 |d4_3==1 | d4_4==1 | d4_5==1 

	gen d4_7=0 if d4_6~=.
	replace d4_7=1 if d4_1==0 & d4_2==0 & d4_3==0 & d4_4==0 & d4_5==0 & d4_6==0  

	gen 	d4=0     if d4_7~=.
	replace d4=0.1 if d4_1==1 
	replace d4=0.3 if d4_2==1
	replace d4=0.6 if d4_3==1
	replace d4=0.9 if d4_4==1
	replace d4=1.5 if d4_5==1
	replace d4=3   if d4_6==1
	replace d4=5  if d4_7==1
	
	gen 	time_consistent=0 if d3~=.
	replace	time_consistent=1 if d3==d4 & d3~=.
	la var  time_consistent "Indicator for time consistency. i.e. d3=d4. So 0 value implies time inconsistent"
	keep slno mid sex_adult time_consistent
	sort slno mid 
	save "$constructdata\temporary\time_number_of_patient_choices_adult.dta", replace
	
	//time consistency - children
	use "$rawdata\ExperimentChildren.dta", clear
	keep slno respondent_id time_q6 time_q7 time_q8 time_q9 time_q10 time_q11 time_q2 time_q3 time_q4 time_q5
	rename respondent_id mid
	
	//discount rate, time set 2
	gen d3_1=0 if time_q8==1
	replace d3_1=1 if time_q8==2  

	gen d3_2=0 if d3_1~=.
	replace d3_2=1 if time_q9==2  
	replace d3_2=0 if d3_1==1  

	gen d3_7=0 if d3_2~=.
	replace d3_7=1 if d3_1==0 & d3_2==0  

	gen 	d3=0  if d3_7~=.
	replace d3=1  if d3_1==1 
	replace d3=3  if d3_2==1
	replace d3=4  if d3_7==1

	//discount rate, time set 3
	gen d4_1=0 if time_q10==1
	replace d4_1=1 if time_q10==2  

	gen d4_2=0 if d4_1~=.
	replace d4_2=1 if time_q11==2  
	replace d4_2=0 if d4_1==1  

	gen d4_7=0 if d4_2~=.
	replace d4_7=1 if d4_1==0  & d4_2==0   

	gen d4=0 if d4_7~=.
	replace d4=1 if d4_1==1 
	replace d4=3 if d4_2==1
	replace d4=4 if d4_7==1

	drop time_q6-time_q11

	gen time_consistent=0 if d3~=.
	replace time_consistent=1 if d3==d4 & d3~=.
	la var   time_consistent "Indicator for time consistency. i.e. d3=d4. So 0 value implies time inconsistent"

	keep slno mid time_consistent
	sort slno mid
	
	save 			"$constructdata\temporary\time_number_of_patient_choices_children.dta", replace
	append using  	"$constructdata\temporary\time_number_of_patient_choices_adult.dta"
	sort slno mid
	save 			"$constructdata\temporary\time_number_of_patient_choices.dta", replace 
	
	//Getting the parents time consistencies
	foreach V in time_consistent{
	bysort slno: gen v1 = `V' if sex_adult==1 & mid<=2 
	egen `V'_father=max(v1), by(slno)
	replace `V'_father=. if  mid<=2 
	drop v1
	}
	
	foreach V in time_consistent{
	bysort slno: gen v1 = `V' if sex_adult==2 & mid<=2 
	egen `V'_mother=max(v1), by(slno)
	replace `V'_mother=. if  mid<=2 
	drop v1
	}
	

	sort slno mid 
	merge m:m slno mid using "$constructdata\children.dta"
	keep if _m==3
	
	save "$constructdata\tableA6.dta", replace 


	
	
*************************************************************************
**** appendB: Construct Data used in Appendix B ****
*************************************************************************
	//generate standard deviations of children and parents preferences 
	
	use "$constructdata\parents.dta", clear
	
	foreach var in patient_choices binswanger spiteful egalitarian altruistic selfish {
	egen `var'_sd_par=sd(`var')
	}
	keep slno mid *_sd_par
	bysor slno: gen n=_n
	keep if n==1
	drop mid  n
	sort slno 
	save "$constructdata\temporary\parents_pref_sd.dta", replace 
	
	use "$constructdata\children.dta", clear
	foreach var in patient_choices binswanger spiteful egalitarian altruistic selfish {
	egen `var'_sd=sd(`var')
	}
	
	merge m:1 slno using "$constructdata\temporary\parents_pref_sd.dta"
	ta _m
	drop _m
	
	save "$constructdata\appendB.dta", replace 


************************************************************************************************
********		Creating data for the attrition analysis 		*******************
************************************************************************************************
	//Baseline (2014) individual characteristics
	use "$constructdata\temporary\baseline_individual_characteristics.dta", clear 
	merge m:m slno mid using "$constructdata\temporary\exp_individual_characteristics.dta" //merging with experiment (2016) individual characteristics
	rename _m m1
	
	//Merging with time preferences
	merge m:m slno mid using "$constructdata\temporary\time_patient_choices.dta" 
	rename _m m3
	
	
	*drop if m1==1 & m3==1
	drop m1 m3 
	//Merging with risk preferences
	merge m:m slno mid using "$constructdata\temporary\risk.dta"
	drop _m 
	
	//Merging with social preferences
	merge m:m slno mid using "$constructdata\temporary\social.dta"
	drop _m 
	//Merging with congnitive ability
	merge m:m slno mid using "$constructdata\temporary\cognitive.dta"
	*keep if _m==3 //we drop if cognitive data were not collected
	drop _m 
	
	//Merging with big-five
	merge m:m slno mid using "$constructdata\temporary\bigfive.dta"
	drop _m
	
	//Merging with locus of control
	merge m:m slno mid using "$constructdata\temporary\locus.dta"
	drop _m
	
	//Merging baseline household characteristics
	merge m:1 slno using "$constructdata\temporary\baseline_household_characteristics.dta"
	drop _m
	
	//We add income data collected in 2016
	merge m:1 slno using "$rawdata\income2016.dta"
	drop _m
	
	//Baseline village characteristics
	merge m:1 village_code using "$rawdata\village.dta"
	*drop if _m==2
	drop _m

	order slno mid village_code uni_code upz_code dis_code
	save "$constructdata\temporary\data_temporary.dta", replace 
	
	//Recoding and labelling of some variables
	
	//Schooling 
	clonevar v1=s1_1_q9_1 
	replace v1=0 if v1==99  //replacing never attended by 0
	replace v1=0 if v1==22|v1==22|v1==23 |v1==24  //replacing pre-primary, mosque, temple, hafizia madrassa by 0
	replace v1=12 if v1==20|v1==21 //replacing vocational, and diploma by 12
	gen schooling = v1
	la var schooling "years of schooling"
	drop v1  
	
	//Currently attending school
	clonevar school_attending =s1_1_q8
	replace school_attending=0 if school_attending==2
	la var school_attending "currently attending school=1, 0 otherwise"
	drop s1_1_q8
	
	//Getting the parents' age, education, & profession
	//Age
	bysort slno: gen v1=s1_1_q6_1 + 2 if gender==1 & s1_1_q5<=2 
	egen age_father = max(v1), by(slno)
	replace age_father=. if  s1_1_q5<=2
	drop v1
	
	bysort slno: gen v1=s1_1_q6_1 + 2 if gender==0 & s1_1_q5<=2 
	egen age_mother = max(v1), by(slno)
	replace age_mother=. if  s1_1_q5<=2
	drop v1
	
	//Education
	bysort slno: gen v1=s1_1_q9_1 if gender==1 & s1_1_q5<=2
	replace v1=0 if v1==99 /* replacing never attended by 0*/
	replace v1=0 if v1==22|v1==22|v1==23 | v1==24
	/* replacing pre-primary, mosque, temple, hafizia madrassa by 0*/
	replace v1=12 if v1==20|v1==21 /* replacing vocational, and diploma by 12*/
	egen schooling_father = max(v1), by(slno)
	replace schooling_father =. if  s1_1_q5<=2
	drop v1

	bysort slno: gen v1=s1_1_q9_1 if gender==0 & s1_1_q5<=2
	replace v1=0 if v1==99 /* replacing never attended by 0*/
	replace v1=0 if v1==22|v1==22|v1==23 |v1==24
	/* replacing pre-primary, mosque, temple, hafizia madrassa by 0*/
	replace v1=12 if v1==20|v1==21 /* replacing vocational, and diploma by 12*/
	egen schooling_mother = max(v1), by(slno)
	replace schooling_mother =. if  s1_1_q5<=2
	drop v1

	//Profession
	clonevar v1=s1_1_q13_1
	gen v2= 0 if v1>=1 & v1<=10 /*agriculture=1 to 10*/
	replace v2=1 if v1>=11 & v1<=32|v1>=35 & v1<=37| v1==47 |v1==52
	/*self-employed non-ag 11-32, 35-37, 47, 52*/
	replace v2=2 if v1==33 |v1>=38 & v1<=43| v1==57
	/*non-ag workers (construction) 33, 38-43, 57*/
	replace v2=3 if v1==44 |v1==50 | v1==54| v1==55 | v1==56
	/*professional 44 50, 54, 55, 56*/
	replace v2=4 if v1==64 |v1==65 /*unemployed 64, 65*/
	replace v2=5 if v1==61 |v1==67 /*housewife, household works 61, 67*/
	replace v2=6 if v1==62 |v1==63 /*students, children 63, 62*/
	replace v2=7 if v1==66 |v1==68 /*others 68, 66*/

	bysort slno: gen v3=v2 if gender==1 & s1_1_q5<=2
	egen prof_father = max(v3), by(slno)
	replace prof_father =. if  s1_1_q5<=2
	drop v3
	
	bysort slno: gen v3=v2 if gender==0 & s1_1_q5<=2
	egen prof_mother = max(v3), by(slno)
	replace prof_mother =. if  s1_1_q5<=2
	drop v1 v2 v3

	
	//Getting the parents cognitive ability and preferences
	foreach V in FSIQ_std patient_choices binswanger spiteful egalitarian altruistic selfish{
	bysort slno: gen v1 = `V' if gender==1 & s1_1_q5<=2 
	egen `V'_father=max(v1), by(slno)
	replace `V'_father=. if  s1_1_q5<=2
	drop v1
	}
	
	foreach V in FSIQ_std patient_choices binswanger spiteful egalitarian altruistic selfish{
	bysort slno: gen v1 = `V' if gender==0 & s1_1_q5<=2 
	egen `V'_mother=max(v1), by(slno)
	replace `V'_mother=. if  s1_1_q5<=2
	drop v1
	}


	sort slno mid 
	keep if s1_1_q5==3
	save "$constructdata\temporary\attrition_v1.dta", replace 


	use "$constructdata\data_with_different_samples.dta", clear
	keep if cognitive_sample==1
	keep slno cognitive_sample
	merge 1:m slno using "$constructdata\temporary\attrition_v1.dta"
	drop if _m==2
	drop _m
	save "$constructdata\temporary\attrition_v2.dta", replace 

	use "$constructdata\children.dta", clear
	keep slno sample_full_info mid	
	merge m:m slno mid using "$constructdata\temporary\attrition_v2.dta"	
	drop _m 
	
	drop if s1_1_q6_1<7 & sample_full_info==. 
	drop if s1_1_q6_1>15 & sample_full_info==.	
	gen 	completed_interview=. 
	replace completed_interview=0 if sample_full_info==.	
	replace completed_interview=1 if sample_full_info==1	

	save "$constructdata\data_for_attrition_analysis.dta", replace 

********************************************************************************************************************



******************************************************************************************************
**********	Creating data file for Table A8: tableA8.dta	********
******************************************************************************************************
	//Big Five - Adults	********

	use "$rawdata\ExperimentAdult.dta", clear
	rename respondent_id mid
	
	foreach var of varlist big_five_17_q1-big_five_17_q15 {
    replace `var' =. if `var' ==0
} 
		

foreach x of varlist big_five_17_q3 big_five_17_q7 big_five_17_q12 big_five_17_q15 {
  replace `x' = (8 - `x')
}


****** GENERATE BIG FIVE MEASURES BY ADDING UP SCORES ******

*conscientiousness 
gen count=.
replace count = 3

foreach var of varlist big_five_17_q1 big_five_17_q7 big_five_17_q11 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q1 big_five_17_q7 big_five_17_q11), missing
gen conscientiousness = v1/count
drop v1

*extraversion 
replace count = 3
foreach var of varlist big_five_17_q2 big_five_17_q8 big_five_17_q12 {
    replace count = count - 1 if `var' == .
} 
egen v1 = rowtotal(big_five_17_q2 big_five_17_q8 big_five_17_q12), missing
gen extraversion = v1/count
drop v1

*agreeableness 
replace count = 3
foreach var of varlist big_five_17_q3 big_five_17_q6 big_five_17_q13 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q3 big_five_17_q6 big_five_17_q13), missing
gen agreeableness = v1/count
drop v1

*openness 
replace count = 3
foreach var of varlist big_five_17_q4 big_five_17_q9 big_five_17_q14 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q4 big_five_17_q9 big_five_17_q14), missing
gen openness = v1/count
drop v1

*neuroticism 
replace count = 3
foreach var of varlist big_five_17_q5 big_five_17_q10 big_five_17_q15 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_17_q5 big_five_17_q10 big_five_17_q15), missing
gen neuroticism = v1/count
drop v1
drop count

keep slno mid conscientiousness extraversion agreeableness openness neuroticism 

sort slno mid
save "$constructdata\temporary\big_five_adult_v2.dta", replace

	//Locus of control - Adults	********
	use "$rawdata\ExperimentAdult.dta", clear

	rename respondent_id mid
	
	foreach var of varlist locus_q1-locus_q28 { //this is to rescale it
    replace `var' =`var' + 1 if `var' ~=.
} 
	

	gen loc_external = locus_q1 +locus_q2 + locus_q4 + locus_q6 + locus_q8 + /*
	*/ locus_q11 + locus_q14 + locus_q17 + locus_q19 + locus_q22 + locus_q23 + /*
	*/ locus_q24 + locus_q26 + locus_q27 if locus_q1-locus_q28~=.

	gen loc_internal = locus_q3 +locus_q5 + locus_q7 + locus_q9 + locus_q10 + /*
	*/ locus_q12 + locus_q13 + locus_q15 + locus_q16 + locus_q18 + locus_q20 + /*
	*/ locus_q21 + locus_q25 + locus_q28 if locus_q1-locus_q28~=.

	gen loc_index = loc_external - loc_internal if loc_internal ~=. & 	loc_external ~=.
	la var loc_index "Locus of Control Index"
	
/*Locus of Control*/
	keep slno mid loc_index 
	sort slno mid
	save "$constructdata\temporary\locus_of_control_adult_v2.dta", replace


	// Big Five - Children	********
	use "$rawdata\ExperimentChildren.dta", clear

	//Big five modules are slightly different for children below 12 and 12 and above
	//10 items asked to mothers for younger kids. For the rest, items are similar to adults
	
	foreach var of varlist big_five_6_11_q1-big_five_6_11_q10 {
    replace `var' =. if `var' ==0
	}
	
	rename respondent_id mid
	
/* GENERATE BIG FIVE MEASURES BY ADDING UP SCORES FROM SINGLE ITEMS */

*GENERATE BIG FIVE MEASURES 

g extraversion_1 = big_five_6_11_q1  
g extraversion_2 = big_five_6_11_q6 
g extraversion_ma = .

g conscientiousness_1 = big_five_6_11_q2
g conscientiousness_2 = big_five_6_11_q7
g conscientiousness_ma = .

g openness_1 = big_five_6_11_q4
g openness_2 = big_five_6_11_q9
g openness_ma = .

g agreeableness_1 = big_five_6_11_q3
g agreeableness_2 = big_five_6_11_q8
g agreeableness_ma = .

g neuroticism_1 = big_five_6_11_q5
g neuroticism_2 = big_five_6_11_q10
g neuroticism_ma = .

*Reverse scale for items which are considered negative
foreach var of varlist extraversion_1 conscientiousness_2 openness_2 agreeableness_1 ///
neuroticism_2 {
	replace `var' =  (12 - `var') 
}


*ADDING UP MEASURES

*** Extraversion ***
replace extraversion_ma = (extraversion_1 + extraversion_2)/2 if extraversion_1 != . & extraversion_2 != .
replace extraversion_ma = extraversion_1 if extraversion_1 != . & extraversion_2 == .
replace extraversion_ma = extraversion_2 if extraversion_1 == . & extraversion_2 != .
egen 	extra_ma_std = std(extraversion_ma)
replace extra_ma_std =. if extraversion_ma==.


*** conscientiousness ***
replace conscientiousness_ma = (conscientiousness_1 + conscientiousness_2)/2 if conscientiousness_1 != . & conscientiousness_2 != .
replace conscientiousness_ma = conscientiousness_1 if conscientiousness_1 != . & conscientiousness_2 == .
replace conscientiousness_ma = conscientiousness_2 if conscientiousness_1 == . & conscientiousness_2 != .
egen 	con_ma_std = std(conscientiousness_ma)
replace con_ma_std=. if conscientiousness_ma==. 

*** Openness ***
replace openness_ma = (openness_1 + openness_2)/2 if openness_1 != . & openness_2 != .
replace openness_ma = openness_1 if openness_1 != . & openness_2 == .
replace openness_ma = openness_2 if openness_1 == . & openness_2 != .
egen 	open_ma_std = std(openness_ma)
replace open_ma_std =.  if openness_ma==.

***agreeableness***
replace agreeableness_ma = (agreeableness_1 + agreeableness_2)/2 if agreeableness_1 != . & agreeableness_2 != .
replace agreeableness_ma = agreeableness_1 if agreeableness_1 != . & agreeableness_2 == .
replace agreeableness_ma = agreeableness_2 if agreeableness_1 == . & agreeableness_2 != .
egen 	agree_ma_std = std(agreeableness_ma)
replace agree_ma_std =. if agreeableness_ma==.

***neuroticism***
replace neuroticism_ma = (neuroticism_1 + neuroticism_2)/2 if neuroticism_1 != . & neuroticism_2 != .
replace neuroticism_ma = neuroticism_1 if neuroticism_1 != . & neuroticism_2 == .
replace neuroticism_ma = neuroticism_2 if neuroticism_1 == . & neuroticism_2 != .
egen 	neuro_ma_std = std(neuroticism_ma)
replace neuro_ma_std =. if neuroticism_ma==.


*LABELING
la var extraversion_ma "extraversion of child evaluated by mother"
la var conscientiousness_ma "conscientiousness of child evaluated by mother"
la var openness_ma "openness of the child evaluated by mother"
la var agreeableness_ma "agreeableness of the child evaluated by mother"
la var neuroticism_ma "neuroticism of the child evaluated by mother"


/* Reverse scale for items, which are considered negative 
Example statement
q3   Sometimes I am rude to others (scale as above)
*/

foreach x of varlist big_five_12_17_q3 big_five_12_17_q7 big_five_12_17_q12 big_five_12_17_q15 {
  replace `x' = (8 - `x')
}


****** GENERATE BIG FIVE MEASURES BY ADDING UP SCORES ******

*conscientiousness 
gen count=.
replace count = 3

foreach var of varlist big_five_12_17_q1 big_five_12_17_q7 big_five_12_17_q11 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q1 big_five_12_17_q7 big_five_12_17_q11), missing
gen conscientiousness = v1/count
drop v1

*extraversion 
replace count = 3
foreach var of varlist big_five_12_17_q2 big_five_12_17_q8 big_five_12_17_q12 {
    replace count = count - 1 if `var' == .
} 
egen v1 = rowtotal(big_five_12_17_q2 big_five_12_17_q8 big_five_12_17_q12), missing
gen extraversion = v1/count
drop v1

*agreeableness 
replace count = 3
foreach var of varlist big_five_12_17_q3 big_five_12_17_q6 big_five_12_17_q13 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q3 big_five_12_17_q6 big_five_12_17_q13), missing
gen agreeableness = v1/count
drop v1

*openness 
replace count = 3
foreach var of varlist big_five_12_17_q4 big_five_12_17_q9 big_five_12_17_q14 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q4 big_five_12_17_q9 big_five_12_17_q14), missing
gen openness = v1/count
drop v1

*neuroticism 
replace count = 3
foreach var of varlist big_five_12_17_q5 big_five_12_17_q10 big_five_12_17_q15 {
    replace count = count - 1 if `var' ==.
} 
egen v1 = rowtotal(big_five_12_17_q5 big_five_12_17_q10 big_five_12_17_q15), missing
gen neuroticism = v1/count
drop v1
drop count

keep slno mid conscientiousness extraversion agreeableness openness neuroticism

	
	sort slno mid
	save "$constructdata\temporary\big_five_children_v2.dta", replace

	//Locus of control - Children
	use "$rawdata\ExperimentChildren.dta", clear
	rename respondent_id mid
	sum locus_q1-locus_q5/* 1812 observations*/

	gen loc_external = locus_q2 +locus_q3 + locus_q4 + locus_q5 
	gen loc_internal = locus_q1 

	gen loc_index = loc_external - loc_internal if loc_internal ~=. & 	loc_external ~=.
	la var loc_index "Locus of Control Index"
				
	//Locus of Control
	keep slno mid loc_index 
	sort slno mid
	save "$constructdata\temporary\locus_of_control_children_v2.dta", replace



	//Full Scale IQ measure - Adults	********

	use "$rawdata\cognitive_data_adult_2015.dta", clear
	gen FSIQ=VCI + PRI + WMI + PSI
	replace FSIQ=236 if FSIQ>236 & FSIQ~=.
	label var FSIQ "Full Scale IQ" 
	keep slno mid FSIQ 
	sort slno mid 
	save "$constructdata\temporary\cognitive_adult_v2.dta", replace


	//Full Scale IQ measure - Children
	use "$rawdata\cognitive_data_children_2015.dta", clear
	gen FSIQ=VCI + PRI + WMI + PSI
	replace FSIQ=226 if FSIQ>226 & FSIQ~=.
	label var FSIQ "Full Scale IQ" 
	keep slno mid FSIQ 
	sort slno mid 
	save "$constructdata\temporary\cognitive_children_v2.dta", replace
	
	

	// Combining all the variables	********
	//big-five 
	use 			"$constructdata\temporary\big_five_children_v2.dta", clear
	append using  	"$constructdata\temporary\big_five_adult_v2.dta"
	sort slno mid
	save 			"$constructdata\temporary\bigfive_v2.dta", replace 
	
	//locus of control
	use 			"$constructdata\temporary\locus_of_control_children_v2.dta", clear
	append using  	"$constructdata\temporary\locus_of_control_adult_v2.dta"
	sort slno mid
	save 			"$constructdata\temporary\locus_v2.dta", replace 

	//cognitive ability
	use 			"$constructdata\temporary\cognitive_children_v2.dta", clear
	append using  	"$constructdata\temporary\cognitive_adult_v2.dta"
	sort slno mid
	save 			"$constructdata\temporary\cognitive_v2.dta", replace 
	
	
	use "$rawdata\Section_1_1.dta", clear
	rename s1_1_q1 mid
	gen gender=s1_1_q3==1
	keep slno mid gender s1_1_q5
	sort slno mid 
	merge m:m slno mid using "$constructdata\temporary\cognitive_v2.dta"
	drop _m
	
	//Merging with big-five
	merge m:m slno mid using "$constructdata\temporary\bigfive_v2.dta"
	drop _m
	
	//Merging with locus of control
	merge m:m slno mid using "$constructdata\temporary\locus_v2.dta"
	drop _m
	
	
//Getting the parents cognitive ability and preferences
	foreach V in FSIQ conscientiousness extraversion agreeableness openness neuroticism loc_index{
	bysort slno: gen v1 = `V' if gender==1 & s1_1_q5<=2 
	egen `V'_father=max(v1), by(slno)
	replace `V'_father=. if  s1_1_q5<=2
	drop v1
	}
	
	foreach V in FSIQ conscientiousness extraversion agreeableness openness neuroticism loc_index{
	bysort slno: gen v1 = `V' if gender==0 & s1_1_q5<=2 
	egen `V'_mother=max(v1), by(slno)
	replace `V'_mother=. if  s1_1_q5<=2
	drop v1
	}
	sort slno mid 
	save "$constructdata\temporary\tableA8.dta", replace 

	use "$constructdata\children.dta", replace 
	
	keep slno mid sample_full_info
	sort slno mid 
	merge m:m slno mid using "$constructdata\temporary\tableA8.dta"
	keep if _m==3
	drop sample_full_info s1_1_q5 gender 
	save "$constructdata\tableA8.dta", replace 	

	
******************************************************************************************************
******************************************************************************************************
	
	capture erase "$constructdata\temporary\attrition_v1.dta"
	capture erase "$constructdata\temporary\attrition_v2.dta"
		
	capture erase "$constructdata\temporary\baseline_household_characteristics.dta"
	capture erase "$constructdata\temporary\baseline_individual_characteristics.dta"
	
	capture erase "$constructdata\temporary\bigfive.dta"
	capture erase "$constructdata\temporary\bigfive_v2.dta"
	capture erase "$constructdata\temporary\big_five_adult.dta"
	capture erase "$constructdata\temporary\big_five_children.dta"
	capture erase "$constructdata\temporary\big_five_adult_v2.dta"
	capture erase "$constructdata\temporary\big_five_children_v2.dta"
	
	capture erase "$constructdata\temporary\children_temporary.dta"
	
	capture erase "$constructdata\temporary\cognitive.dta"
	capture erase "$constructdata\temporary\cognitive_adult.dta"
	capture erase "$constructdata\temporary\cognitive_children.dta"
	capture erase "$constructdata\temporary\cognitive_v2.dta"
	capture erase "$constructdata\temporary\cognitive_adult_v2.dta"
	capture erase "$constructdata\temporary\cognitive_children_v2.dta"
	
	capture erase "$constructdata\temporary\data_temporary.dta"
	
	capture erase "$constructdata\temporary\exp_individual_characteristics.dta"
	capture erase "$constructdata\temporary\exp_ind_charc_adult.dta"
	capture erase "$constructdata\temporary\exp_ind_charc_child.dta"
	
	capture erase "$constructdata\temporary\grandparents.dta"
	capture erase "$constructdata\temporary\hhsize.dta"
	capture erase "$constructdata\temporary\land.dta"

	capture erase "$constructdata\temporary\locus.dta"
	capture erase "$constructdata\temporary\locus_of_control_children.dta"
	capture erase "$constructdata\temporary\locus_of_control_adult.dta"
	capture erase "$constructdata\temporary\locus_v2.dta"
	capture erase "$constructdata\temporary\locus_of_control_children_v2.dta"
	capture erase "$constructdata\temporary\locus_of_control_adult_v2.dta"
	
	capture erase "$constructdata\temporary\parents_pref_sd.dta"
	capture erase "$constructdata\temporary\risk.dta"
	capture erase "$constructdata\temporary\risk_preference_children.dta"
	capture erase "$constructdata\temporary\risk_preference_adult.dta"

	capture erase "$constructdata\temporary\sample_cognitive.dta"

	capture erase "$constructdata\temporary\social.dta"
	capture erase "$constructdata\temporary\social_preference_children.dta"
	capture erase "$constructdata\temporary\social_preference_adult.dta"
	
	capture erase "$constructdata\temporary\tableA8.dta"
	capture erase "$constructdata\temporary\time_patient_choices.dta"
	capture erase "$constructdata\temporary\time_number_of_patient_choices.dta"
	capture erase "$constructdata\temporary\time_number_of_patient_choices_adult.dta"
	capture erase "$constructdata\temporary\time_number_of_patient_choices_children.dta"
	
