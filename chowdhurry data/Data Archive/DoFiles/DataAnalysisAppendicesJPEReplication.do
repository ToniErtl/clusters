***********************************************************************************************************************
*** Replication Files for Chowdhury, Sutter, and Zimmermann ***
*** "Economic preferences across generations and family clusters: A large-scale experiment in a developing country" ***
***********************************************************************************************************************
***********************************************************************************************************************

***********************************************************************************************************************
*** This file takes the constructed data and creates appendix tables and figures ***
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

*******************************************************************************************************************
*** Table A.1 - A comparison of 3,467 households (who only took part in wave one or have no children of age 6-16)**
*** to 1,001 households who have children and were invited to the second wave of data collection ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************
	
	use "$constructdata/data_with_different_samples.dta", clear 
	
	gen 	groups = 0 // all households surveyed in 2014
	replace groups = 1 if cognitive_sample==1 // cognitive survey sample 
	label define groups 0 "3,500 houseolds" 1 "1,000 households"
	label values groups groups
	
	la var age_father "Father's age (in years)"
	la var age_mother "Mother's age (in years)"
	la var schooling_father "Father's schooling (in years)"
	la var schooling_mother "Mother's schooling (in years)"
	la var hhsize "Household size"
	la var grand_parents "Grand parents in household"
	la var inc_per_cap_per_month_2016 "Per capita income per month in Taka in 2016"	
	loc outcome_variables "age_father age_mother schooling_father schooling_mother hhsize grand_parents income_per_cap"
		
	local y = 3
	
	foreach var in age_father age_mother schooling_father schooling_mother hhsize grand_parents inc_per_cap_per_month_2016 {
		
		ttest `var', by(groups)
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A1") modify
		
		local mu_1 = string(`r(mu_1)',"%9.3fc")
		local mu_2 = string(`r(mu_2)',"%9.3fc")
		
		local se_1 = `r(sd_1)'/sqrt(`r(N_1)')
		local se_1 = "(" + string(`se_1',"%9.3fc") + ")" 
		
		local se_2 = `r(sd_2)'/sqrt(`r(N_2)')
		local se_2 = "(" + string(`se_2',"%9.3fc") + ")" 
		
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.3fc")
		local sig = `r(p)'
		
		if `sig' < 0.01 {
			local diff = "`diff'***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local diff = "`diff'**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local diff = "`diff'*"
			}
		
		local y = `y' + 1
		
		putexcel B`y' = ("`mu_1'")
		putexcel C`y' = ("`mu_2'")
		putexcel D`y' = ("`diff'")
		
		local y = `y' + 1
		
		putexcel B`y' = ("`se_1'")
		putexcel C`y' = ("`se_2'")
		
		}

*******************************************************************************************************************
*** Table A.2: Difference in observable characteristics between the 542 households for which we have all data, ***
*** including experimental data, and the 458 households for which we lack experimental data but who were invited ***
*** in wave two in 2016 to collect data on cognitive skills (separated by those 268 households who then did not ***
*** participate in wave two and those 190 households who were invited and participated in the collection of cognitive skills) ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************

	use "$constructdata/children.dta", clear
	bysort slno: gen n=_n
	keep if n==1
	keep slno sample_full_info
	merge 1:1 slno using "$constructdata/data_with_different_samples.dta"
	drop _m
	keep if cognitive_sample==1
	
	gen 	groups=1 if cognitive_sample==1 //Intended to conduct cognitive ability survey
	replace groups=2 if congitive_sample_types==2 //Cognitive ability survey conducted on both parents and children
	replace groups=4 if sample_full_info==1 //Experiments conducted 
	
	label define groups 1 "Intended to conduct cognitive ability survey" 2 "Only cognitive ability survey conducted" 4 "Experiments conducted"
	label values groups groups
	
	la var age_father "Father's age (in years)"
	la var age_mother "Mother's age (in years)"
	la var schooling_father "Father's schooling (in years)"
	la var schooling_mother "Mother's schooling (in years)"
	la var hhsize "Household size"
	la var grand_parents "Grand parents in household"
	la var inc_per_cap_per_month_2016 "Per capita income per month in Taka in 2016"	
	la var population "Village population"
	
	/// (1)-(2)
	
	gen 	groups_1=1 if cognitive_sample==1 //Intended to conduct cognitive ability survey
	replace groups_1=2 if congitive_sample_types==2 //Cognitive ability survey conducted on both parents and children
	
	local y = 4
	
	foreach var in age_father age_mother schooling_father schooling_mother hhsize grand_parents inc_per_cap_per_month_2016 population {
	
		ttest `var', by(groups_1)
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A2") modify
		
		local mu_1 = string(`r(mu_1)',"%9.3fc")
		local mu_2 = string(`r(mu_2)',"%9.3fc")
		
		local se_1 = `r(sd_1)'/sqrt(`r(N_1)')
		local se_1 = "(" + string(`se_1',"%9.3fc") + ")" 
		
		local se_2 = `r(sd_2)'/sqrt(`r(N_2)')
		local se_2 = "(" + string(`se_2',"%9.3fc") + ")" 
		
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.3fc")
		local sig = `r(p)'
		
		if `sig' < 0.01 {
			local diff = "`diff'***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local diff = "`diff'**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local diff = "`diff'*"
			}
		
		local y = `y' + 1
		
		putexcel B`y' = ("`mu_1'")
		putexcel C`y' = ("`mu_2'")
		putexcel F`y' = ("`diff'")
		
		local y = `y' + 1
		
		putexcel B`y' = ("`se_1'")
		putexcel C`y' = ("`se_2'")
		
		}
		
	///(1)-(4)
	
	preserve
	
	gen groups_2 = groups_1
	replace groups_2 = 4 if sample_full_info==1 //Experiments conducted 
	recode groups_2 (2=.)
	
	local y = 4
	
	foreach var in age_father age_mother schooling_father schooling_mother hhsize grand_parents inc_per_cap_per_month_2016 population {
	
		ttest `var', by(groups_2)
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A2") modify
		
		local mu_1 = string(`r(mu_1)',"%9.3fc")
		local mu_2 = string(`r(mu_2)',"%9.3fc")
		
		local se_1 = `r(sd_1)'/sqrt(`r(N_1)')
		local se_1 = "(" + string(`se_1',"%9.3fc") + ")" 
		
		local se_2 = `r(sd_2)'/sqrt(`r(N_2)')
		local se_2 = "(" + string(`se_2',"%9.3fc") + ")" 
		
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.3fc")
		local sig = `r(p)'
		
		if `sig' < 0.01 {
			local diff = "`diff'***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local diff = "`diff'**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local diff = "`diff'*"
			}
		
		local y = `y' + 1
		
		putexcel E`y' = ("`mu_2'")
		putexcel G`y' = ("`diff'")
		
		local y = `y' + 1
		
		putexcel E`y' = ("`se_2'")
		
		}
	
	restore
	
	///(2)-(4)
	
	preserve
	
	gen groups_3 = groups_1
	replace groups_3 = 4 if sample_full_info==1 //Experiments conducted 
	recode groups_3 (1=.)
	
	local y = 4
	
	foreach var in age_father age_mother schooling_father schooling_mother hhsize grand_parents inc_per_cap_per_month_2016 population {
	
		ttest `var', by(groups_3)
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A2") modify
		
		local mu_1 = string(`r(mu_1)',"%9.3fc")
		local mu_2 = string(`r(mu_2)',"%9.3fc")
		
		local se_1 = `r(sd_1)'/sqrt(`r(N_1)')
		local se_1 = "(" + string(`se_1',"%9.3fc") + ")" 
		
		local se_2 = `r(sd_2)'/sqrt(`r(N_2)')
		local se_2 = "(" + string(`se_2',"%9.3fc") + ")" 
		
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.3fc")
		local sig = `r(p)'
		
		if `sig' < 0.01 {
			local diff = "`diff'***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local diff = "`diff'**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local diff = "`diff'*"
			}
		
		local y = `y' + 1
		
		putexcel H`y' = ("`diff'")
		
		local y = `y' + 1
		
		}
		
	restore
	
	///(3)-(4)
	
	replace groups_1 = 4 if sample_full_info==1
	gen group2 = 3 if groups_1 == 1 | groups_1 == 2 // Cognitive ability survey sample 
	replace group2 = 4 if groups_1 == 4 // Experiments conducted 
	
	local y = 4
	
	foreach var in age_father age_mother schooling_father schooling_mother hhsize grand_parents inc_per_cap_per_month_2016 population {
	
		ttest `var', by(group2)
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A2") modify
		
		local mu_1 = string(`r(mu_1)',"%9.3fc")
		local mu_2 = string(`r(mu_2)',"%9.3fc")
		
		local se_1 = `r(sd_1)'/sqrt(`r(N_1)')
		local se_1 = "(" + string(`se_1',"%9.3fc") + ")" 
		
		local se_2 = `r(sd_2)'/sqrt(`r(N_2)')
		local se_2 = "(" + string(`se_2',"%9.3fc") + ")" 
		
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.3fc")
		local sig = `r(p)'
		
		if `sig' < 0.01 {
			local diff = "`diff'***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local diff = "`diff'**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local diff = "`diff'*"
			}
		
		local y = `y' + 1
		
		putexcel D`y' = ("`mu_1'")
		putexcel I`y' = ("`diff'")
		
		local y = `y' + 1
		
		putexcel D`y' = ("`se_1'")
		
		}
		
	count if groups == 1
	putexcel B21 = ("`r(N)'")
	
	count if groups == 2
	putexcel C21 = ("`r(N)'")
	
	count if groups == 1 | groups == 2
	putexcel D21 = ("`r(N)'")
	
	count if groups == 4
	putexcel E21 = ("`r(N)'")

	
****************************************************************************************************************
***	Table A.3: Children’s preferences and their relation to parental preferences, ***
*** using inverse probability weighting to account for possible attrition ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
****************************************************************************************************************
	
	use "$constructdata/data_for_attrition_analysis.dta", clear
		
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "i.district_code"
	
	***************************************************************************************
	* Childrens preferences and their relation to parental preferences attrition adjusted *
	***************************************************************************************
	
    global Z age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	extraversion_std agreeableness_std openness_std neuroticism_std loc_std ///
	gender school_attending
	
	global Y age_at_exp schooling school_attending elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 grand_parents FSIQ_std conscientiousness_std ///
	extraversion_std agreeableness_std openness_std neuroticism_std loc_std
	
	global B gender school_attending grand_parents
	
	// Time preference - number of patient_choices
	
	teffects ipwra (patient_choices patient_choices_father patient_choices_mother $Z i.district_code) (completed_interview  `parents' `household', probit) if ustood_time==1, aequations
	
	local n = `e(N)'
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[OME1:`var'],"%9.3fc")
		local sig = (2 * (1-normal(abs(_b[OME1:`var']/_se[OME1:`var']))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[OME1:`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
		
	putexcel B36 = ("`n'")
	putexcel B37 = ("Yes")
		
	test ([OME1]patient_choices_father=[OME1]patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B38 = ("`p1'")
	
	// Risk preference 
	
	teffects ipwra (binswanger binswanger_father binswanger_mother $Z i.district_code) (completed_interview  `parents' `household', probit) if ustood_risk==1, aequations
	
	local n = `e(N)'
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[OME1:`var'],"%9.3fc")
		local sig = (2 * (1-normal(abs(_b[OME1:`var']/_se[OME1:`var']))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[OME1:`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
		
	putexcel C36 = ("`n'")
	putexcel C37 = ("Yes")
		
	test ([OME1]binswanger_father=[OME1]binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C38 = ("`p1'")
	
	eststo clear
	
	* Marginal effects
		
	 // Spiteful
	
    foreach Y in spiteful {
	gmm (cond(`Y',normalden({`Y': i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
         normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
         instruments(i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
	
	local n = `e(N)'
	
	local x = 0
	local y = 1
	
	foreach var in `Y'_father `Y'_mother gender {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		}
		
	local y = 7
	
	foreach var of varlist age_at_exp schooling {
	
		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		}
		
	local y = 11
	
	foreach var in school_attending {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		}
		
	local y = 13
	
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {

		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
		
		putexcel D36 = ("`n'")
		putexcel D37 = ("Yes")
		
		// Test father = mother
	
		gen `Y'_parents= `Y'_father -`Y'_mother
		tabulate `Y'_parents
		gen `Y'_two=2*`Y'_mother
		
		eststo: gmm (cond(`Y',normalden({`Y': `Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
			normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
			instruments(`Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
		
		foreach var in `Y'_parents {
		
			margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
			local x = `x' + 1
			
			matrix temp_`x' = r(table)
			local b_`x' = temp_`x'[1,1]
			local se_`x' = temp_`x'[2,1]
			
			local b_`x' = string(`b_`x'',"%9.3fc")
			local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
			local sig = string(`sig',"%9.3fc")
			
			putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
			putexcel D38 = ("`sig'")
			
			}
			
		}
		
	// Egalitarian
	
    foreach Y in egalitarian {
	gmm (cond(`Y',normalden({`Y': i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
         normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
         instruments(i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
	
	local n = `e(N)'
	
	local x = 0
	local y = 1
	
	foreach var in `Y'_father `Y'_mother gender {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		}
		
	local y = 7
	
	foreach var of varlist age_at_exp schooling {
	
		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		}
		
	local y = 11
	
	foreach var in school_attending {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		}
		
	local y = 13
	
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {

		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
		
		putexcel E36 = ("`n'")
		putexcel E37 = ("Yes")
		
		// Test father = mother
	
		gen `Y'_parents= `Y'_father -`Y'_mother
		tabulate `Y'_parents
		gen `Y'_two=2*`Y'_mother
		
		eststo: gmm (cond(`Y',normalden({`Y': `Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
			normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
			instruments(`Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
		
		foreach var in `Y'_parents {
		
			margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
			local x = `x' + 1
			
			matrix temp_`x' = r(table)
			local b_`x' = temp_`x'[1,1]
			local se_`x' = temp_`x'[2,1]
			
			local b_`x' = string(`b_`x'',"%9.3fc")
			local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
			local sig = string(`sig',"%9.3fc")
			
			putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
			putexcel E38 = ("`sig'")
			
			}
			
		}
		
	// Selfish
	
    foreach Y in selfish {
	gmm (cond(`Y',normalden({`Y': i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
         normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
         instruments(i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
	
	local n = `e(N)'
	
	local x = 0
	local y = 1
	
	foreach var in `Y'_father `Y'_mother gender {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		}
		
	local y = 7
	
	foreach var of varlist age_at_exp schooling {
	
		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		}
		
	local y = 11
	
	foreach var in school_attending {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		}
		
	local y = 13
	
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {

		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
		putexcel F36 = ("`n'")
		putexcel F37 = ("Yes")
		
		// Test father = mother
	
		gen `Y'_parents= `Y'_father -`Y'_mother
		tabulate `Y'_parents
		gen `Y'_two=2*`Y'_mother
		
		eststo: gmm (cond(`Y',normalden({`Y': `Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
			normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
			instruments(`Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
		
		foreach var in `Y'_parents {
		
			margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
			local x = `x' + 1
			
			matrix temp_`x' = r(table)
			local b_`x' = temp_`x'[1,1]
			local se_`x' = temp_`x'[2,1]
			
			local b_`x' = string(`b_`x'',"%9.3fc")
			local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
			local sig = string(`sig',"%9.3fc")
			
			putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
			putexcel F38 = ("`sig'")
			
			}
			
		}
		
	// Altruistic
	
    foreach Y in altruistic {
	gmm (cond(`Y',normalden({`Y': i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
         normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
         instruments(i.`Y'_father i.`Y'_mother i.gender i.school_attending i.grand_parents /// 
	                              age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
	                              schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
	                              extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
	
	local n = `e(N)'
	
	local x = 0
	local y = 1
	
	foreach var in `Y'_father `Y'_mother gender {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		}
		
	local y = 7
	
	foreach var of varlist age_at_exp schooling {
	
		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		}
		
	local y = 11
	
	foreach var in school_attending {
	
		margins `var', expression(normal(xb())) vce(unconditional) contrast(nowald)
		
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		}
		
	local y = 13
	
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {

		margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
		local x = `x' + 1
		
		matrix temp_`x' = r(table)
		local b_`x' = temp_`x'[1,1]
		local se_`x' = temp_`x'[2,1]
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
		putexcel G36 = ("`n'")
		putexcel G37 = ("Yes")
		
		// Test father = mother
	
		gen `Y'_parents= `Y'_father -`Y'_mother
		tabulate `Y'_parents
		gen `Y'_two=2*`Y'_mother
		
		eststo: gmm (cond(`Y',normalden({`Y': `Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code _cons})/ ///  
			normal({`Y':}),-normalden({`Y':})/normal(-{`Y':}))),  ///
			instruments(`Y'_two `Y'_parents i.gender i.school_attending i.grand_parents /// 
									  age_at_exp schooling elder_bro elder_sis younger_bro younger_sis age_father age_mother /// 
									  schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std ///
									  extraversion_std agreeableness_std openness_std neuroticism_std loc_std i.district_code) onestep
		
		foreach var in `Y'_parents {
		
			margins, at(`var'=generate(`var')) at(`var'=generate(`var'+1)) expression(normal(xb())) vce(unconditional) contrast(atcontrast(r) nowald)
	
			local x = `x' + 1
			
			matrix temp_`x' = r(table)
			local b_`x' = temp_`x'[1,1]
			local se_`x' = temp_`x'[2,1]
			
			local b_`x' = string(`b_`x'',"%9.3fc")
			local sig = (2 * (1-normal(abs(`b_`x''/`se_`x''))))
			local sig = string(`sig',"%9.3fc")
			
			putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A3") modify
			putexcel G38 = ("`sig'")
			
			}
			
		}


****************************************************************************************************************
*** Table A.4: Schooling of parents (distribution of years of schooling of mothers and fathers) ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
****************************************************************************************************************
	use "$constructdata/parents.dta", clear 
	
	local y = 2
	
	forvalues x = 0/17 {
	
		count if schooling_wife == `x'
		local num_w_`x' = `r(N)'
		
		count if schooling_wife != .
		local pct_w_`x' = `num_w_`x''/`r(N)'*100
		local pct_w_`x' = string(`pct_w_`x'',"%9.2fc")
		
		count if schooling_husband == `x'
		local num_h_`x' = `r(N)'
		
		count if schooling_husband != .
		local pct_h_`x' = `num_h_`x''/`r(N)'*100
		local pct_h_`x' = string(`pct_h_`x'',"%9.2fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A4") modify
		
		local y = `y' + 1
		putexcel B`y' = ("`num_w_`x''")
		putexcel C`y' = ("`pct_w_`x''")
		putexcel D`y' = ("`num_h_`x''")
		putexcel E`y' = ("`pct_h_`x''")
		
		}
	

*******************************************************************************************************************
*** Table A.5: Exchange rate between tokens and Taka, conditional on age ****
*******************************************************************************************************************

	* These are exchanged rates used in the experiments ***
	* See the detailed experimental procedures described in Appendix C

*******************************************************************************************************************
***	Table A.6: Children’s time consistency and its relation to parents’ time consistencies ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/tableA6.dta", clear
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno 
	//clustering unit, we are using households
	loc f_effect "i.district_code"
	
	*********************************************************
	* Children time consistency on parents time consistency *
	*********************************************************
	
	xi: dprobit time_consistent time_consistent_father time_consistent_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in time_consistent_father time_consistent_mother gender age_at_exp schooling school_attending age_father age_mother schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A6") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B36 = ("`n'")
	putexcel B37 = ("`r2'")
	putexcel B38 = ("Yes")
	
	test (time_consistent_father=time_consistent_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B39 = ("`p1'")
	
	test time_consistent_father time_consistent_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B40 = ("`p2'")
	
	
*******************************************************************************************************************
***	Table A.7: Differences in observable characteristics of the samples in which risk preferences were collected
*** and in which this was not the case ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************
	use "$constructdata/children.dta", clear
 
	//risk missing
	gen risk_missing=1 if binswanger==.
	replace risk_missing=0 if binswanger~=.
	
	label define risk_missing 0 "Risk preference is collected" 1 "Risk preference is missing" 
	label values risk_missing risk_missing
	
	la var gender "Gender (boys= 0, girls= 1)"
	la var age_at_exp  "Age of respondent (in years)"
	la var schooling  "Years of schooling"
	la var school_attending "Currently attending school (yes=1, no=0)"
	la var elder_bro "How many elder brothers?"
	la var elder_sis "How many elder sisters?"
	la var younger_bro  "How many younger brothers?"
	la var younger_sis  "How many younger sisters?"
	la var age_father "Age father (in years)"
	la var age_mother "Age mother (in years)"
	la var schooling_father "Schooling father (in years)"
	la var schooling_mother "Schooling mother (in years)"
	la var hhsize "Household size (# of persons)"
	la var grand_parents  "Grandparents living in household (yes=1)"
	la var inc_per_cap_per_month_2016 "Income per capita per month in 2016 (in Taka)"
	la var population "Total village population in 2015"

	local vars gender age_at_exp schooling school_attending elder_bro elder_sis younger_bro younger_sis age_father age_mother schooling_father schooling_mother hhsize grand_parents inc_per_cap_per_month_2016 population
	
	local x = 3
	
	foreach var of local vars {
		
		local x = `x' + 1
		
		ttest `var', by(risk_missing)
		local mean_1 = string(`r(mu_1)',"%9.2fc")
		local se_1 = string(`r(sd_1)',"%9.2fc")
		local mean_2 = string(`r(mu_2)',"%9.2fc")
		local se_2 = string(`r(sd_2)',"%9.2fc")
		
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.2fc")
		local se = string(`r(se)',"%9.2fc")
		local pval = string(`r(p)',"%9.2fc")
		
	
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A7") modify
		putexcel B`x' = ("`mean_1'")
		putexcel C`x' = ("`se_1'")
		putexcel D`x' = ("`mean_2'")
		putexcel E`x' = ("`se_2'")
		putexcel F`x' = ("`diff'")
		putexcel H`x' = ("`se'")
		putexcel I`x' = ("`pval'")
		
		}
		
	local n1 = string(`r(N_1)',"%9.0fc")
	putexcel B20 = ("`n1'")
	
	local n2 = string(`r(N_2)',"%9.0fc")
	putexcel D20 = ("`n2'")
	
	local n3 = `n2'+`n1'
	putexcel F20 = ("`n3'")

*******************************************************************************************************************
*** Table A.8: Descriptive statistics: Cognitive and non-cognitive skills ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************

	//parents
	use "$constructdata/tableA8.dta", clear 
	
	//wives and husbands
	
	local x = 3
	
	foreach var in FSIQ loc_index extraversion conscientiousness openness agreeableness neuroticism  {
	
		local x = `x' + 1
		
		sum `var'_father 
		
		local mean_hus = string(`r(mean)',"%9.3fc")
		local sd_hus = string(`r(sd)',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A8") modify
		putexcel B`x' = ("`mean_hus'")
		putexcel C`x' = ("`sd_hus'")
		
		sum `var'_mother
		
		local mean_wfe = string(`r(mean)',"%9.3fc")
		local sd_wfe = string(`r(sd)',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A8") modify
		putexcel D`x' = ("`mean_wfe'")
		putexcel E`x' = ("`sd_wfe'")
		
		}
		
	//children
	
	local x = 3
	
	foreach var in FSIQ loc_index extraversion conscientiousness openness agreeableness neuroticism  {
	
		local x = `x' + 1
		
		sum `var'
		
		local mean_chil = string(`r(mean)',"%9.3fc")
		local sd_chil = string(`r(sd)',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A8") modify
		putexcel F`x' = ("`mean_chil'")
		putexcel G`x' = ("`sd_chil'")
		
		}
		
*******************************************************************************************************************
*** Table A.9: Children’s preferences and their relation to parental preferences ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************
	use "$constructdata/children.dta", clear

	//Controls
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "i.district_code"

	*********************************************************************
	* Children’s preferences and their relation to parental preferences *
	*********************************************************************

	//Time preference 
	reg patient_choices patient_choices_father patient_choices_mother `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A9") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B6 = ("`n'")
	putexcel B7 = ("`r2'")
	putexcel B8 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B9 = ("`p1'")
	
	//Risk preferences 
	reg binswanger binswanger_father binswanger_mother `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A9") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C6 = ("`n'")
	putexcel C7 = ("`r2'")
	putexcel C8 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C9 = ("`p1'")
	

	//Social preferences - spiteful
	xi: dprobit spiteful spiteful_father spiteful_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A9") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D6 = ("`n'")
	putexcel D7 = ("`r2'")
	putexcel D8 = ("Yes")
	
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D9 = ("`p1'")
	
	//Social preferences - egalitarian
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A9") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E6 = ("`n'")
	putexcel E7 = ("`r2'")
	putexcel E8 = ("Yes")
	
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E9 = ("`p1'")
	
	//Social preferences - altruistic
	xi: dprobit altruistic altruistic_father altruistic_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A9") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F6 = ("`n'")
	putexcel F7 = ("`r2'")
	putexcel F8 = ("Yes")
	
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F9 = ("`p1'")

	//Social preferences - selfish
	xi: dprobit selfish selfish_father selfish_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A9") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G6 = ("`n'")
	putexcel G7 = ("`r2'")
	putexcel G8 = ("Yes")
	
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G9 = ("`p1'")
	
****************************************************************************************************************
*** Table A.10: Interacting parent’s gender and child’s gender ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
****************************************************************************************************************
	use "$constructdata/children.dta", clear

	//Controls
	loc background 			"age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "i.district_code"
	
	***********************************************
	* Interacting Parents gender and child gender *
	***********************************************
	
	//Time preference
	reg patient_choices c.patient_choices_father##gender c.patient_choices_mother##gender `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother 1.gender 1.gender#c.patient_choices_father 1.gender#c.patient_choices_mother{
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A10") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B12 = ("`n'")
	putexcel B13 = ("`r2'")
	putexcel B14 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B15 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B16 = ("`p2'")

	//Risk preferences /*non_risk_averse binswanger*/
	reg binswanger c.binswanger_father##gender c.binswanger_mother##gender `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother 1.gender 1.gender#c.binswanger_father 1.gender#c.binswanger_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A10") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C12 = ("`n'")
	putexcel C13 = ("`r2'")
	putexcel C14 = ("Yes")
		
	test (binswanger_mother=binswanger_father) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C15 = ("`p1'")
	
	test binswanger_mother binswanger_father
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C16 = ("`p2'")

	//Social preferences Variables
	
	reg spiteful spiteful_father##gender spiteful_mother##gender `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.spiteful_father 1.spiteful_mother 1.gender 1.gender#1.spiteful_father 1.gender#1.spiteful_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A10") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D12 = ("`n'")
	putexcel D13 = ("`r2'")
	putexcel D14 = ("Yes")
		
	test (1.spiteful_father=1.spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D15 = ("`p1'")
	
	test 1.spiteful_father 1.spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D16 = ("`p2'")

	//Egalitarian
	
	reg egalitarian egalitarian_father##gender egalitarian_mother##gender `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.egalitarian_father 1.egalitarian_mother 1.gender 1.gender#1.egalitarian_father 1.gender#1.egalitarian_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A10") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E12 = ("`n'")
	putexcel E13 = ("`r2'")
	putexcel E14 = ("Yes")
		
	test (1.egalitarian_father=1.egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E15 = ("`p1'")
	
	test 1.egalitarian_father 1.egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel E16 = ("`p2'")
	
	//Altruistic
	
	reg altruistic altruistic_father##gender altruistic_mother##gender `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.altruistic_father 1.altruistic_mother 1.gender 1.gender#1.altruistic_father 1.gender#1.altruistic_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A10") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F12 = ("`n'")
	putexcel F13 = ("`r2'")
	putexcel F14 = ("Yes")
		
	test (1.altruistic_father=1.altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F15 = ("`p1'")
	
	test 1.altruistic_father 1.altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F16 = ("`p2'")
	
	//Selfish
	
	reg selfish selfish_father##gender selfish_mother##gender `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.selfish_father 1.selfish_mother 1.gender 1.gender#1.selfish_father 1.gender#1.selfish_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A10") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G12 = ("`n'")
	putexcel G13 = ("`r2'")
	putexcel G14 = ("Yes")
		
	test (1.selfish_father=1.selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G15 = ("`p1'")
	
	test 1.selfish_father 1.selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G16 = ("`p2'")
	
****************************************************************************************************************
*** Table A.11: Horse-race regressions – Number of patient choices as dependent variable ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
****************************************************************************************************************
	
	use "$constructdata/children.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	loc keepvar2			"`background' FSIQ_std `big_5' loc_std"
	loc keepvar3			"schooling_father schooling_mother hhsize inc_per_cap_per_month_2016"
		
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"
	
	********************************************************************************
	* A11 Horse-race regressions – Number of patient choices as dependent variable *
	********************************************************************************
	
	// Patient choices
	xi: reg patient_choices patient_choices_father patient_choices_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' i.district_code if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B42 = ("`p2'")

	//Drop district fixed effects
	reg patient_choices patient_choices_father patient_choices_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("No")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C42 = ("`p2'")

	//drop everything else
	reg patient_choices patient_choices_father patient_choices_mother `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("No")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D42 = ("`p2'")
	
	// drop parents preferences 
	reg patient_choices `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 7
	
	foreach var in gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("No")
			
	//drop SES and parents background
	reg patient_choices patient_choices_father patient_choices_mother `background'  `siblings'  `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother gender age_at_exp schooling school_attending {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	local y = 23
	
	foreach var in FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("No")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F42 = ("`p2'")

	//drop child's background
	reg patient_choices patient_choices_father patient_choices_mother  `parents'  `household' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother { 
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	local y = 15
		
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A11") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("No")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G42 = ("`p2'")


*******************************************************************************************************************
*** Table A.12: Horse-race regressions – Gamble number picked (risk preferences) as dependent variable ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/children.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	loc keepvar2			"`background' FSIQ_std `big_5' loc_std"
	loc keepvar3			"schooling_father schooling_mother hhsize inc_per_cap_per_month_2016"
		
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"

	//Binswanger	
	xi: reg binswanger binswanger_father binswanger_mother  `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' i.district_code  if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in binswanger_father binswanger_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B42 = ("`p2'")

	//Drop district fixed effects
	reg binswanger binswanger_father binswanger_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in binswanger_father binswanger_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("No")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C42 = ("`p2'")

	//drop everything else
	reg binswanger binswanger_father binswanger_mother `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in binswanger_father binswanger_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("No")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D42 = ("`p2'")
	
	// drop parents preferences 
	reg binswanger `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 7
	
	foreach var in gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("No")
			
	//drop SES and parents background
	reg binswanger binswanger_father binswanger_mother `background'  `siblings'  `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in binswanger_father binswanger_mother gender age_at_exp schooling school_attending {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	local y = 23
	
	foreach var in FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("No")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F42 = ("`p2'")

	//drop child's background
	reg binswanger binswanger_father binswanger_mother  `parents'  `household' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in binswanger_father binswanger_mother { 
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	local y = 15
		
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A12") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("No")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G42 = ("`p2'")

*******************************************************************************************************************
*** Table Table A.13: Horse-race regressions – Spitefulness as dependent variable ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/children.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	loc keepvar2			"`background' FSIQ_std `big_5' loc_std"
	loc keepvar3			"schooling_father schooling_mother hhsize inc_per_cap_per_month_2016"
		
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"
	
	//Social preferences Variables - spiteful

	xi: dprobit spiteful spiteful_father spiteful_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' i.district_code if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in spiteful_father spiteful_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B42 = ("`p2'")

	//Drop district fixed effects
	
	xi: dprobit spiteful spiteful_father spiteful_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in spiteful_father spiteful_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("No")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C42 = ("`p2'")

	//drop everything else
	xi: dprobit spiteful spiteful_father spiteful_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in spiteful_father spiteful_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("No")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D42 = ("`p2'")
	
	// drop parents preferences 
	xi: dprobit spiteful `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 7
	
	foreach var in gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("No")
			
	//drop SES and parents background
	xi: dprobit spiteful spiteful_father spiteful_mother `background' `siblings' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in spiteful_father spiteful_mother gender age_at_exp schooling school_attending {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	local y = 23
	
	foreach var in FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("No")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F42 = ("`p2'")

	//drop child's background
	xi: dprobit spiteful spiteful_father spiteful_mother `parents' `household' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in spiteful_father spiteful_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		
		}
		
	local y = 15
		
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A13") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("No")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G42 = ("`p2'")

****************************************************************************************************************
*** Table A.14: Horse-race regressions – Egalitarian social preference as dependent variable ***
****************************************************************************************************************	
	
	use "$constructdata/children.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	loc keepvar2			"`background' FSIQ_std `big_5' loc_std"
	loc keepvar3			"schooling_father schooling_mother hhsize inc_per_cap_per_month_2016"
		
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"

	////Social preferences Variables - egalitarian
	
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' i.district_code if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in egalitarian_father egalitarian_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B42 = ("`p2'")

	//Drop district fixed effects
	
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in egalitarian_father egalitarian_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("No")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C42 = ("`p2'")

	//drop everything else
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in egalitarian_father egalitarian_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("No")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D42 = ("`p2'")
	
	// drop parents preferences 
	xi: dprobit egalitarian `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 7
	
	foreach var in gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("No")
			
	//drop SES and parents background
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `background' `siblings' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in egalitarian_father egalitarian_mother gender age_at_exp schooling school_attending {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	local y = 23
	
	foreach var in FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("No")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F42 = ("`p2'")

	//drop child's background
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `parents' `household' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in egalitarian_father egalitarian_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		
		}
		
	local y = 15
		
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A14") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("No")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G42 = ("`p2'")
	
*******************************************************************************************************************
*** Table Table A.15: Horse-race regressions – Altruistic social preferences as dependent variable ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	use "$constructdata/children.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	loc keepvar2			"`background' FSIQ_std `big_5' loc_std"
	loc keepvar3			"schooling_father schooling_mother hhsize inc_per_cap_per_month_2016"
		
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"

	//Altruistic
	
	xi: dprobit altruistic altruistic_father altruistic_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' i.district_code if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in altruistic_father altruistic_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B42 = ("`p2'")

	//Drop district fixed effects
	
	xi: dprobit altruistic altruistic_father altruistic_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in altruistic_father altruistic_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("No")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C42 = ("`p2'")

	//drop everything else
	xi: dprobit altruistic altruistic_father altruistic_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in altruistic_father altruistic_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("No")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D42 = ("`p2'")
	
	// drop parents preferences 
	xi: dprobit altruistic `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 7
	
	foreach var in gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("No")
			
	//drop SES and parents background
	xi: dprobit altruistic altruistic_father altruistic_mother `background' `siblings' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in altruistic_father altruistic_mother gender age_at_exp schooling school_attending {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	local y = 23
	
	foreach var in FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("No")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F42 = ("`p2'")

	//drop child's background
	xi: dprobit altruistic altruistic_father altruistic_mother `parents' `household' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in altruistic_father altruistic_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		
		}
		
	local y = 15
		
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("No")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G42 = ("`p2'")
	

*******************************************************************************************************************
*** Table A.16: Horse-race regressions – Selfishness as dependent variable ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/children.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	loc keepvar2			"`background' FSIQ_std `big_5' loc_std"
	loc keepvar3			"schooling_father schooling_mother hhsize inc_per_cap_per_month_2016"
		
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"

	//Selfishness
	
	xi: dprobit selfish selfish_father selfish_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' i.district_code if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in selfish_father selfish_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A16") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B42 = ("`p2'")

	//Drop district fixed effects
	
	xi: dprobit selfish selfish_father selfish_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in selfish_father selfish_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A16") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("No")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C42 = ("`p2'")

	//drop everything else
	xi: dprobit selfish selfish_father selfish_mother `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in selfish_father selfish_mother {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A16") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("No")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D42 = ("`p2'")
	
	// drop parents preferences 
	xi: dprobit selfish `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 8
	
	foreach var in gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A16") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("No")
			
	//drop SES and parents background
	xi: dprobit selfish selfish_father selfish_mother `background' `siblings' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in selfish_father selfish_mother gender age_at_exp schooling school_attending {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A15") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	local y = 23
	
	foreach var in FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A16") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
		
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("No")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F42 = ("`p2'")

	//drop child's background
	xi: dprobit selfish selfish_father selfish_mother `parents' `household' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 3
	
	foreach var in selfish_father selfish_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A16") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		
		}
		
	local y = 15
		
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 {
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A16") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
		
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("No")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G42 = ("`p2'")
	
	
*******************************************************************************************************************
*** Table A.17: Multiple hypothesis testing (Romano-Wolf) – Using the specification of Table 9 ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/children.dta", clear

	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno 
	//clustering unit, we are using households
	loc f_effect "i.district_code"
	
	********************************************************************************
	* Multiple hypothesis testing (Romano-Wolf) using the specification of Table 9 *
	********************************************************************************
	
	// Patient choices
	reg patient_choices patient_choices_father patient_choices_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A17") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel B`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
	
	putexcel B10 = ("`n'")
	putexcel B11 = ("`r2'")
	putexcel B12 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B13 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B14 = ("`p2'")
	
	
	/*Romano*/
	
	preserve
	rename patient_choices_father pfather
	rename patient_choices_mother pmother
	rwolf patient_choices if ustood_time==1, indepvar(pfather pmother) method(regress) reps(5000) nodots ///
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_patient_choices_pfather)',"%9.3fc")
	local rw_mother = string(`e(rw_patient_choices_pmother)',"%9.3fc")
	
	putexcel B5 = ("`rw_father'")
	putexcel B9 = ("`rw_mother'")
	restore
		
	// Risk - Lottery number picked
	reg binswanger binswanger_father binswanger_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')

	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother{
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A17") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel C`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
	
	putexcel C10 = ("`n'")
	putexcel C11 = ("`r2'")
	putexcel C12 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C13 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C14 = ("`p2'")
	
	
	/*Romano*/
	
	preserve
	rename binswanger_father rfather
	rename binswanger_mother rmother
	rwolf binswanger if ustood_risk==1, indepvar(rfather rmother) method(regress) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_binswanger_rfather)',"%9.3fc")
	local rw_mother = string(`e(rw_binswanger_rmother)',"%9.3fc")
	
	putexcel C5 = ("`rw_father'")
	putexcel C9 = ("`rw_mother'")
	
	restore
	
	// Spiteful
	xi: dprobit spiteful spiteful_father spiteful_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A17") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel D`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel D10 = ("`n'")
	putexcel D11 = ("`r2'")
	putexcel D12 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D13 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D14 = ("`p2'")
	
	
	preserve
	rename spiteful_father spfather
	rename spiteful_mother spmother
	rwolf spiteful if ustood_social==1, indepvar(spfather spmother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_spiteful_spfather)',"%9.3fc")
	local rw_mother = string(`e(rw_spiteful_spmother)',"%9.3fc")
	
	putexcel D5 = ("`rw_father'")
	putexcel D9 = ("`rw_mother'")
	
	restore
		
	// Egalitarian
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A17") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel E`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel E10 = ("`n'")
	putexcel E11 = ("`r2'")
	putexcel E12 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E13 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel E14 = ("`p2'")
		
	preserve
	rename egalitarian_father efather
	rename egalitarian_mother emother
	rwolf egalitarian if ustood_social==1, indepvar(efather emother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_egalitarian_efather)',"%9.3fc")
	local rw_mother = string(`e(rw_egalitarian_emother)',"%9.3fc")
	
	putexcel E5 = ("`rw_father'")
	putexcel E9 = ("`rw_mother'")
	
	restore
	
	// Altruistic
	xi: dprobit altruistic altruistic_father altruistic_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A17") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel F`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel F10 = ("`n'")
	putexcel F11 = ("`r2'")
	putexcel F12 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F13 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F14 = ("`p2'")
	
	preserve
	rename altruistic_father afather
	rename altruistic_mother amother
	rwolf altruistic if ustood_social==1, indepvar(afather amother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_altruistic_afather)',"%9.3fc")
	local rw_mother = string(`e(rw_altruistic_amother)',"%9.3fc")
	
	putexcel F5 = ("`rw_father'")
	putexcel F9 = ("`rw_mother'")
	
	restore
	
	// Selfish
	xi: dprobit selfish selfish_father selfish_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A17") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel G`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel G10 = ("`n'")
	putexcel G11 = ("`r2'")
	putexcel G12 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G13 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G14 = ("`p2'")
	
    preserve
	rename selfish_father sefather
	rename selfish_mother semother
	rwolf selfish if ustood_social==1, indepvar(sefather semother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_selfish_sefather)',"%9.3fc")
	local rw_mother = string(`e(rw_selfish_semother)',"%9.3fc")
	
	putexcel G5 = ("`rw_father'")
	putexcel G9 = ("`rw_mother'")
	
	restore

*******************************************************************************************************************
*** Table A.18: Multiple hypothesis testing (Romano-Wolf) – Using the specification of Table 10 ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/table10.dta", clear 
	
	//Controls
	loc parenting 			"negative_parenting positive_parenting"
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`parenting' `background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
		
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"
	
	// Patient choices
	reg patient_choices patient_choices_father patient_choices_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A18") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel B`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
	
	putexcel B10 = ("`n'")
	putexcel B11 = ("`r2'")
	putexcel B12 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B13 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B14 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel B15 = ("`p3'")
	
	/*Romano*/
	
	preserve
	rename patient_choices_father pfather
	rename patient_choices_mother pmother
	rwolf patient_choices if ustood_time==1, indepvar(pfather pmother) method(regress) reps(5000) nodots /// 
	controls(`parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_patient_choices_pfather)',"%9.3fc")
	local rw_mother = string(`e(rw_patient_choices_pmother)',"%9.3fc")
	
	putexcel B5 = ("`rw_father'")
	putexcel B9 = ("`rw_mother'")
	restore
		
	//Risk preferences
	reg binswanger binswanger_father binswanger_mother `parenting'  `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A18") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel C`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
	
	putexcel C10 = ("`n'")
	putexcel C11 = ("`r2'")
	putexcel C12 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C13 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C14 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel C15 = ("`p3'")
	
	//Romano-Wolf  
	
	preserve
	rename binswanger_father rfather
	rename binswanger_mother rmother
	rwolf binswanger if ustood_risk==1, indepvar(rfather rmother) method(regress) reps(5000) nodots /// 
	controls(`parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_binswanger_rfather)',"%9.3fc")
	local rw_mother = string(`e(rw_binswanger_rmother)',"%9.3fc")
	
	putexcel C5 = ("`rw_father'")
	putexcel C9 = ("`rw_mother'")
	restore
	
	//Spiteful
	
	xi: dprobit spiteful spiteful_father spiteful_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A18") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel D`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel D10 = ("`n'")
	putexcel D11 = ("`r2'")
	putexcel D12 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D13 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D14 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel D15 = ("`p3'")
	
	//Romano-Wolf  
	preserve
	rename spiteful_father spfather
	rename spiteful_mother spmother
	rwolf spiteful if ustood_social==1, indepvar(spfather spmother) method(probit) reps(5000) nodots /// 
	controls(`parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_spiteful_spfather)',"%9.3fc")
	local rw_mother = string(`e(rw_spiteful_spmother)',"%9.3fc")
	
	putexcel D5 = ("`rw_father'")
	putexcel D9 = ("`rw_mother'")
	restore
	
	
	//Egalitarian		
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A18") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel E`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel E10 = ("`n'")
	putexcel E11 = ("`r2'")
	putexcel E12 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E13 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel E14 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel E15 = ("`p3'")
	
	//Romano-Wolf  
	preserve
	rename egalitarian_father efather
	rename egalitarian_mother emother
	rwolf egalitarian if ustood_social==1, indepvar(efather emother) method(probit) reps(5000) nodots /// 
	controls(`parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_egalitarian_efather)',"%9.3fc")
	local rw_mother = string(`e(rw_egalitarian_emother)',"%9.3fc")
	
	putexcel E5 = ("`rw_father'")
	putexcel E9 = ("`rw_mother'")
	restore
	
	//Altruistic	
	
	xi: dprobit altruistic altruistic_father altruistic_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A18") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel F`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel F10 = ("`n'")
	putexcel F11 = ("`r2'")
	putexcel F12 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F13 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F14 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel F15 = ("`p3'")
	
	//Romano-Wolf  
	preserve
	rename altruistic_father afather
	rename altruistic_mother amother
	rwolf altruistic if ustood_social==1, indepvar(afather amother) method(probit) reps(5000) nodots /// 
	controls(`parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 

	local rw_father = string(`e(rw_altruistic_afather)',"%9.3fc")
	local rw_mother = string(`e(rw_altruistic_amother)',"%9.3fc")
	
	putexcel F5 = ("`rw_father'")
	putexcel F9 = ("`rw_mother'")
	restore
	
	
	//Selfish	
	
	xi: dprobit selfish selfish_father selfish_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A18") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel G`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	putexcel G10 = ("`n'")
	putexcel G11 = ("`r2'")
	putexcel G12 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G13 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G14 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel G15 = ("`p3'")
	
	//Romano-Wolf  
	preserve
	rename selfish_father sefather
	rename selfish_mother semother
	rwolf selfish if ustood_social==1, indepvar(sefather semother) method(probit) reps(5000) nodots /// 
	controls(`parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_selfish_sefather)',"%9.3fc")
	local rw_mother = string(`e(rw_selfish_semother)',"%9.3fc")
	
	putexcel G5 = ("`rw_father'")
	putexcel G9 = ("`rw_mother'")
	restore

	
*******************************************************************************************************************
*** Table A.19: Multiple hypothesis testing (Romano-Wolf) – Using the specification of Table 11 ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/table11.dta", clear

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
	
	//Patient choices
	qui reg patient_choices patient_choices_father patient_choices_mother pc_homo_f pc_homo_m pc_homo  `childs_background' `siblings' `parents' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel B`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in pc_homo_f pc_homo_m pc_homo {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B16 = ("`n'")
	putexcel B17 = ("`r2'")
	putexcel B18 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B19 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B20 = ("`p2'")
	
	test (pc_homo_f + patient_choices_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel B21 = ("`p3'")
	
	test (patient_choices_mother + pc_homo_m =0)
	local p4 = string(r(p),"%9.3fc")
	putexcel B22 = ("`p4'")
	
	//Romano-Wolf  
	preserve
	rename patient_choices_father pfather
	rename patient_choices_mother pmother
	rwolf patient_choices if ustood_time==1, indepvar(pfather pmother) method(regress) reps(5000) nodots /// 
	controls(`childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_patient_choices_pfather)',"%9.3fc")
	local rw_mother = string(`e(rw_patient_choices_pmother)',"%9.3fc")
	
	putexcel B5 = ("`rw_father'")
	putexcel B9 = ("`rw_mother'")
	restore
	
	//Risk - Lottery number picked
	reg binswanger binswanger_father binswanger_mother   binswanger_mother bins_homo_f bins_homo_m bins_homo  `childs_background' `siblings' `parents' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel C`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in bins_homo_f bins_homo_m bins_homo {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C16 = ("`n'")
	putexcel C17 = ("`r2'")
	putexcel C18 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C19 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C20 = ("`p2'")
	
	test (bins_homo_f + binswanger_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel C21 = ("`p3'")
	
	test (binswanger_mother + bins_homo_m =0)
	local p4 = string(r(p),"%9.3fc")
	putexcel C22 = ("`p4'")
	
	//Romano-Wolf  
	preserve
	rename binswanger_father rfather
	rename binswanger_mother rmother
	rwolf binswanger if ustood_risk==1, indepvar(rfather rmother) method(regress) reps(5000) nodots /// 
	controls(`childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_binswanger_rfather)',"%9.3fc")
	local rw_mother = string(`e(rw_binswanger_rmother)',"%9.3fc")
	
	putexcel C5 = ("`rw_father'")
	putexcel C9 = ("`rw_mother'")
	restore
	
	//Spiteful
	
	xi: dprobit spiteful  spiteful_father spiteful_mother  spit_homo_f spit_homo_m spit_homo `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if  ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel D`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in spit_homo_f spit_homo_m spit_homo { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		
		}
	
	putexcel D16 = ("`n'")
	putexcel D17 = ("`r2'")
	putexcel D18 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D19 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D20 = ("`p2'")
	
	test (spit_homo_f + spiteful_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel D21 = ("`p3'")
	
	test (spiteful_mother + spit_homo_m =0)
	local p4 = string(r(p),"%9.3fc")
	putexcel D22 = ("`p4'")
	
	//Romano-Wolf  
	preserve
	rename spiteful_father spfather
	rename spiteful_mother spmother
	rwolf spiteful if ustood_social==1, indepvar(spfather spmother) method(probit) reps(5000) nodots /// 
	controls(`childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_spiteful_spfather)',"%9.3fc")
	local rw_mother = string(`e(rw_spiteful_spmother)',"%9.3fc")
	
	putexcel D5 = ("`rw_father'")
	putexcel D9 = ("`rw_mother'")
	restore

	//Egalitarian
	xi: dprobit egalitarian  egalitarian_father egalitarian_mother egal_homo_f egal_homo_m  egal_homo `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel E`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in egal_homo_f egal_homo_m  egal_homo { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		
		}
	
	putexcel E16 = ("`n'")
	putexcel E17 = ("`r2'")
	putexcel E18 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E19 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel E20 = ("`p2'")
	
	test (egal_homo_f + egalitarian_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel E21 = ("`p3'")
	
	test (egalitarian_mother + egal_homo_m =0)
	local p4 = string(r(p),"%9.3fc")
	putexcel E22 = ("`p4'")
	
	//Romano-Wolf  
	preserve
	rename egalitarian_father efather
	rename egalitarian_mother emother
	rwolf egalitarian if ustood_social==1, indepvar(efather emother) method(probit) reps(5000) nodots /// 
	controls(`childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_egalitarian_efather)',"%9.3fc")
	local rw_mother = string(`e(rw_egalitarian_emother)',"%9.3fc")
	
	putexcel E5 = ("`rw_father'")
	putexcel E9 = ("`rw_mother'")

	restore
	
	//Altruistic
	xi: dprobit altruistic  altruistic_father altruistic_mother altru_homo_f altru_homo_m altru_homo  `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel F`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in altru_homo_f altru_homo_m altru_homo { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		
		}
	
	putexcel F16 = ("`n'")
	putexcel F17 = ("`r2'")
	putexcel F18 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F19 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F20 = ("`p2'")
	
	test (altru_homo_f + altruistic_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel F21 = ("`p3'")
	
	test (altruistic_mother + altru_homo_m =0)
	local p4 = string(r(p),"%9.3fc")
	putexcel F22 = ("`p4'")
	
	//Romano-Wolf  
	preserve
	rename altruistic_father afather
	rename altruistic_mother amother
	rwolf altruistic if ustood_social==1, indepvar(afather amother) method(probit) reps(5000) nodots /// 
	controls(`childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_altruistic_afather)',"%9.3fc")
	local rw_mother = string(`e(rw_altruistic_amother)',"%9.3fc")
	
	putexcel F5 = ("`rw_father'")
	putexcel F9 = ("`rw_mother'")

	restore
	
	//Selfish
	
	xi: dprobit selfish  selfish_father selfish_mother slfsh_homo_f slfsh_homo_m slfsh_homo `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel G`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in slfsh_homo_f slfsh_homo_m slfsh_homo { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A19") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		
		}
	
	putexcel G16 = ("`n'")
	putexcel G17 = ("`r2'")
	putexcel G18 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G19 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G20 = ("`p2'")
	
	test (slfsh_homo_f + selfish_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel G21 = ("`p3'")
	
	test (selfish_mother + slfsh_homo_m =0)
	local p4 = string(r(p),"%9.3fc")
	putexcel G22 = ("`p4'")
	
	//Romano-Wolf  
    preserve
	rename selfish_father sefather
	rename selfish_mother semother
	rwolf selfish if ustood_social==1, indepvar(sefather semother) method(probit) reps(5000) nodots /// 
	controls(`childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_selfish_sefather)',"%9.3fc")
	local rw_mother = string(`e(rw_selfish_semother)',"%9.3fc")
	
	putexcel G5 = ("`rw_father'")
	putexcel G9 = ("`rw_mother'")

	restore

********************************************************************************************************************
*** Table A.20: Multiple hypothesis testing (Romano-Wolf) – Using the specification of Table 12 ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	

	use "$constructdata/table12.dta", clear

	//Controls
	loc background 			"gender age_at_exp schooling school_attending"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "i.district_code"
	gl nummodels 6
	
	//Patient choices
	reg patient_choices patient_choices_father patient_choices_mother pat_cho_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel B`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in pat_cho_res_sibling {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B12 = ("`n'")
	putexcel B13 = ("`r2'")
	putexcel B14 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B15 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B16 = ("`p2'")
	

	//Romano-Wolf  
	preserve
	rename patient_choices_father pfather
	rename patient_choices_mother pmother
	rwolf patient_choices if ustood_time==1, indepvar(pfather pmother) method(regress) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_patient_choices_pfather)',"%9.3fc")
	local rw_mother = string(`e(rw_patient_choices_pmother)',"%9.3fc")
	
	putexcel B5 = ("`rw_father'")
	putexcel B9 = ("`rw_mother'")
	restore
	
	//Risk - Lottery number picked
	reg binswanger binswanger_father binswanger_mother binsw_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel C`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in binsw_res_sibling {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C12 = ("`n'")
	putexcel C13 = ("`r2'")
	putexcel C14 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C15 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C16 = ("`p2'")
	
	//Romano-Wolf  
	preserve
	rename binswanger_father rfather
	rename binswanger_mother rmother
	rwolf binswanger if ustood_risk==1, indepvar(rfather rmother) method(regress) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_binswanger_rfather)',"%9.3fc")
	local rw_mother = string(`e(rw_binswanger_rmother)',"%9.3fc")
	
	putexcel C5 = ("`rw_father'")
	putexcel C9 = ("`rw_mother'")
	restore
	
	//Spiteful
	
	xi: dprobit spiteful spiteful_father spiteful_mother spit_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel D`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in spit_res_sibling { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		
		}
	
	putexcel D12 = ("`n'")
	putexcel D13 = ("`r2'")
	putexcel D14 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D15 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D16 = ("`p2'")
	
	//Romano-Wolf  
	preserve
	rename spiteful_father spfather
	rename spiteful_mother spmother
	rwolf spiteful if ustood_social==1, indepvar(spfather spmother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_spiteful_spfather)',"%9.3fc")
	local rw_mother = string(`e(rw_spiteful_spmother)',"%9.3fc")
	
	putexcel D5 = ("`rw_father'")
	putexcel D9 = ("`rw_mother'")
	restore

	//Egalitarian
	xi: dprobit egalitarian egalitarian_father egalitarian_mother egal_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel E`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in egal_res_sibling { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		
		}
	
	putexcel E12 = ("`n'")
	putexcel E13 = ("`r2'")
	putexcel E14 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E15 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel E16 = ("`p2'")
	
	//Romano-Wolf  
	preserve
	rename egalitarian_father efather
	rename egalitarian_mother emother
	rwolf egalitarian if ustood_social==1, indepvar(efather emother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_egalitarian_efather)',"%9.3fc")
	local rw_mother = string(`e(rw_egalitarian_emother)',"%9.3fc")
	
	putexcel E5 = ("`rw_father'")
	putexcel E9 = ("`rw_mother'")

	restore
	
	//Altruistic
	xi: dprobit altruistic altruistic_father altruistic_mother altru_res_sibling  `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel F`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in altru_res_sibling { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		
		}
	
	putexcel F12 = ("`n'")
	putexcel F13 = ("`r2'")
	putexcel F14 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F15 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F16 = ("`p2'")

	//Romano-Wolf  
	preserve
	rename altruistic_father afather
	rename altruistic_mother amother
	rwolf altruistic if ustood_social==1, indepvar(afather amother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_altruistic_afather)',"%9.3fc")
	local rw_mother = string(`e(rw_altruistic_amother)',"%9.3fc")
	
	putexcel F5 = ("`rw_father'")
	putexcel F9 = ("`rw_mother'")

	restore
	
	//Selfish
	
	xi: dprobit selfish selfish_father selfish_mother self_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		local y = `y' + 1
		putexcel G`y' = ("`sig_`x''")
		
		local y = `y' + 1
		
		}
		
	local y = 9
	
	foreach var in self_res_sibling { 
		local x = `x' + 1
		
		matrix temp_b_`x' = temp_b["r1","`var'"]
		matrix temp_se_`x' = temp_se["r1","`var'"]
		
		local b_`x' = temp_b_`x'[1,1]
		local se_`x' = temp_se_`x'[1,1]
		local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		local sig_`x' = string(`sig',"%9.3fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A20") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		
		}
	
	putexcel G12 = ("`n'")
	putexcel G13 = ("`r2'")
	putexcel G14 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G15 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G16 = ("`p2'")
	
	//Romano-Wolf  
    preserve
	rename selfish_father sefather
	rename selfish_mother semother
	rwolf selfish if ustood_social==1, indepvar(sefather semother) method(probit) reps(5000) nodots /// 
	controls(`background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect') ///
	cluster (`unit') 
	
	local rw_father = string(`e(rw_selfish_sefather)',"%9.3fc")
	local rw_mother = string(`e(rw_selfish_semother)',"%9.3fc")
	
	putexcel G5 = ("`rw_father'")
	putexcel G9 = ("`rw_mother'")

	restore

*******************************************************************************************************************
*** Table A.21: Relation of parenting styles to parents’ preferences ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************
	
	use "$constructdata/table10.dta", clear
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc par_pref 			"patient_choices_father patient_choices_mother binswanger_father binswanger_mother spiteful_father spiteful_mother egalitarian_father egalitarian_mother altruistic_father altruistic_mother selfish_father selfish_mother" 
	
	loc parents 			"age_father age_mother schooling_father schooling_mother"
	loc household 		  	"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc village_fe 			"i.village_code"
	loc upazila_fe			"i.upazila_code"
	loc dist_fe				"i.district_code"
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno 
	//clustering unit, we are using households
	loc f_effect "slno"
	
	*******************************************************
	* Relation of parenting styles to parents preferences *
	*******************************************************
	
	//Patient choices
	
	reg negative_parenting `par_pref' , cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother binswanger_father binswanger_mother spiteful_father spiteful_mother egalitarian_father egalitarian_mother altruistic_father altruistic_mother selfish_father selfish_mother {
	
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A21") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
		
	putexcel B42 = ("`n'")
	putexcel B43 = ("`r2'")
	putexcel B44 = ("Yes")
		
	test `par_pref' 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B45 = ("`p1'")
	
	reg positive_parenting `par_pref' , cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother binswanger_father binswanger_mother spiteful_father spiteful_mother egalitarian_father egalitarian_mother altruistic_father altruistic_mother selfish_father selfish_mother {
	
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A21") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
		
	putexcel C42 = ("`n'")
	putexcel C43 = ("`r2'")
	putexcel C44 = ("Yes")
		
	test `par_pref' 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C45 = ("`p1'")
	
	reg negative_parenting `par_pref' `parents' `household', cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother binswanger_father binswanger_mother spiteful_father spiteful_mother egalitarian_father egalitarian_mother altruistic_father altruistic_mother selfish_father selfish_mother age_father age_mother schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 grand_parents {
	
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A21") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
		
	putexcel D42 = ("`n'")
	putexcel D43 = ("`r2'")
	putexcel D44 = ("Yes")
		
	test `par_pref' 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D45 = ("`p1'")
	
	reg positive_parenting `par_pref' `parents' `household', cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 3
	
	foreach var in patient_choices_father patient_choices_mother binswanger_father binswanger_mother spiteful_father spiteful_mother egalitarian_father egalitarian_mother altruistic_father altruistic_mother selfish_father selfish_mother age_father age_mother schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 grand_parents {
	
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A21") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
		
	putexcel E42 = ("`n'")
	putexcel E43 = ("`r2'")
	putexcel E44 = ("Yes")
		
	test `par_pref' 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E45 = ("`p1'")

*/
*******************************************************************************************************************
*** Table A.22: Summary of characteristics represented in two clusters resulting from partitioning around medoids***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	

	use "$constructdata/children_familyAggregate_stat12.dta", clear
	
	merge 1:1 slno using "$constructdata/cluster_pam_narm.dta"
	drop _merge
	
	tempfile familyAggregate_stat_clustered
	sa "`familyAggregate_stat_clustered'"

	//Table 22

	local clusterVariables "pam_narm"
	 
	local patience "patient_choicesOffspringMean patient_choices_father patient_choices_mother"
	local binswanger "binswangerOffspringMean binswanger_father binswanger_mother"
	local spitefulness "spitefulOffspringMean spiteful_father spiteful_mother"
	local egalitarian "egalitarianOffspringMean egalitarian_father egalitarian_mother"
	local altruism "altruisticOffspringMean altruistic_father altruistic_mother"
	local selfishness "selfishOffspringMean selfish_father selfish_mother"
	local variables "`patience' `binswanger' `spitefulness' `egalitarian' `altruism' `selfishness'"
	
	local x = 1
	
	foreach var of local variables {
		
		local x = `x' + 1
		
		ttest `var', by(pam_narm)
		
		local mu1 = string(`r(mu_1)',"%9.2fc")
		local mu2 = string(`r(mu_2)',"%9.2fc")
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.2fc")
		local pval = string(`r(p)',"%9.2fc")
		
		local n1 = string(`r(N_1)',"%9.0fc")
		local n2 = string(`r(N_2)',"%9.0fc")
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table A22") modify
		putexcel B`x' = (`mu1')
		putexcel C`x' = (`mu2')
		putexcel D`x' = (`diff')
		putexcel E`x' = (`pval')
		
		}
		
	putexcel B20 = (`n1')
	putexcel C20 = (`n2')

		
	
***********************************************************************************************************************
*** This file takes the constructed data and creates Appendix B tables and figures ***
***********************************************************************************************************************

//Install module rego for regressions in table B1

net from http://www.marco-sunder.de/stata/
net install rego

	
*******************************************************************************************************************
*** Table B.1: Child-parent preference relationships: Robustness checks for Table 7 in the main paper and 
*** explorations of a genetic prior for transmission ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	use "$constructdata/appendB.dta", clear  
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	loc background 		"gender age_at_exp schooling school_attending"
	loc siblings 		"elder_bro elder_sis younger_bro younger_sis"
	loc parents 		"age_father age_mother schooling_father schooling_mother"
	loc household 		"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std			"FSIQ_std"
	loc big_5			"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std			"loc_std"
	
	loc vill			"population"
	
	loc keepvar			"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions 	"excel lab dec(3) nocons nonotes"
	loc notes 			"Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text 			"District Fixed Effects are included?"
	loc ap 				"replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect 		"i.district_code"

	
	sum *_*_sd *_sd *_sd_par // we use these numbers below. 

	*****************************
	* Testing genetic transfers *
	*****************************
	
	//Time preference - patient_choices
	reg patient_choices patient_choices_father patient_choices_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2_raw = `e(r2)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x'_raw = _b[`var']
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B1") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B36 = ("`n'")
	putexcel B37 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B42 = ("`p1'")
	
	sum patient_choices_sd
	local s1 = `r(mean)'
	
	sum patient_choices_sd_par
	local s2 = `r(mean)'
	
	local genetic = 0.25*(`s1'/`s2')
	local genetic = string(`genetic',"%9.3fc")
	putexcel B41 = ("`genetic'")

	test (patient_choices_father=0.25*(`s1'/`s2')) 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B43 = ("`p2'")
	
	test (patient_choices_mother=0.25*(`s1'/`s2')) 
	local p3 = string(`r(p)',"%9.3fc")
	putexcel B44 = ("`p3'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p4 = string(`r(p)',"%9.3fc")
	putexcel B45 = ("`p4'")
	
	xi: rego patient_choices patient_choices_father patient_choices_mother \ `background' \ `parents' \ `siblings' \ `household' \ `cog_std' `big_5' \ `loc_std' \ i.district_code if ustood_time==1
	
	matrix temp = e(shapley_perc)
	matrix temp_b = temp["r1","SV1"]
	local owen_raw = temp_b[1,1]
	local owen = string(`owen_raw',"%9.2fc")
	putexcel B38 = ("`owen'")
	
	local hered = `owen_raw' * `r2_raw' 
	local hered = string(`hered',"%9.3fc")
	putexcel B39 = ("`hered'")
	
	local rf = 2*`b_1_raw'*(`s2'/`s1')
	local rf = string(`rf',"%9.3fc")
	putexcel B46 = ("`rf'")
	
	local rm = 2*`b_2_raw'*(`s2'/`s1')
	local rm = string(`rm',"%9.3fc")
	putexcel B47 = ("`rm'")

	//Risk preferences 
	reg binswanger binswanger_father binswanger_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2_raw = `e(r2)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x'_raw = _b[`var']
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B1") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C36 = ("`n'")
	putexcel C37 = ("`r2'")
	putexcel C40 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C42 = ("`p1'")
	
	sum binswanger_sd
	local s1 = `r(mean)'
	
	sum binswanger_sd_par
	local s2 = `r(mean)'
	
	local genetic = 0.25*(`s1'/`s2')
	local genetic = string(`genetic',"%9.3fc")
	putexcel C41 = ("`genetic'")

	test (binswanger_father=0.25*(`s1'/`s2')) 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C43 = ("`p2'")
	
	test (binswanger_mother=0.25*(`s1'/`s2')) 
	local p3 = string(`r(p)',"%9.3fc")
	putexcel C44 = ("`p3'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p4 = string(`r(p)',"%9.3fc")
	putexcel C45 = ("`p4'")
	
	xi: rego binswanger binswanger_father binswanger_mother \ `background' \ `parents' \ `siblings' \ `household' \ `cog_std' `big_5' \ `loc_std' \ i.district_code if ustood_risk==1
	
	matrix temp = e(shapley_perc)
	matrix temp_b = temp["r1","SV1"]
	local owen_raw = temp_b[1,1]
	local owen = string(`owen_raw',"%9.2fc")
	putexcel C38 = ("`owen'")
	
	local hered = `owen_raw' * `r2_raw' 
	local hered = string(`hered',"%9.3fc")
	putexcel C39 = ("`hered'")
	
	local rf = 2*`b_1_raw'*(`s2'/`s1')
	local rf = string(`rf',"%9.3fc")
	putexcel C46 = ("`rf'")
	
	local rm = 2*`b_2_raw'*(`s2'/`s1')
	local rm = string(`rm',"%9.3fc")
	putexcel C47 = ("`rm'")
	
	//Social preferences
	reg spiteful spiteful_father spiteful_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2_raw = `e(r2)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x'_raw = _b[`var']
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B1") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D36 = ("`n'")
	putexcel D37 = ("`r2'")
	putexcel D40 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D42 = ("`p1'")
	
	sum spiteful_sd
	local s1 = `r(mean)'
	
	sum spiteful_sd_par
	local s2 = `r(mean)'
	
	local genetic = 0.25*(`s1'/`s2')
	local genetic = string(`genetic',"%9.3fc")
	putexcel D41 = ("`genetic'")

	test (spiteful_father=0.25*(`s1'/`s2')) 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D43 = ("`p2'")
	
	test (spiteful_mother=0.25*(`s1'/`s2')) 
	local p3 = string(`r(p)',"%9.3fc")
	putexcel D44 = ("`p3'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p4 = string(`r(p)',"%9.3fc")
	putexcel D45 = ("`p4'")
	
	xi: rego spiteful spiteful_father spiteful_mother \ `background' \ `parents' \ `siblings' \ `household' \ `cog_std' `big_5' \ `loc_std' \ i.district_code if ustood_social==1
	
	matrix temp = e(shapley_perc)
	matrix temp_b = temp["r1","SV1"]
	local owen_raw = temp_b[1,1]
	local owen = string(`owen_raw',"%9.2fc")
	putexcel D38 = ("`owen'")
	
	local hered = `owen_raw' * `r2_raw' 
	local hered = string(`hered',"%9.3fc")
	putexcel D39 = ("`hered'")
	
	local rf = 2*`b_1_raw'*(`s2'/`s1')
	local rf = string(`rf',"%9.3fc")
	putexcel D46 = ("`rf'")
	
	local rm = 2*`b_2_raw'*(`s2'/`s1')
	local rm = string(`rm',"%9.3fc")
	putexcel D47 = ("`rm'")
	
	//Egalitarian
	
	reg egalitarian egalitarian_father egalitarian_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2_raw = `e(r2)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x'_raw = _b[`var']
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B1") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E36 = ("`n'")
	putexcel E37 = ("`r2'")
	putexcel E40 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E42 = ("`p1'")
	
	sum egalitarian_sd
	local s1 = `r(mean)'
	
	sum egalitarian_sd_par
	local s2 = `r(mean)'
	
	local genetic = 0.25*(`s1'/`s2')
	local genetic = string(`genetic',"%9.3fc")
	putexcel E41 = ("`genetic'")

	test (egalitarian_father=0.25*(`s1'/`s2')) 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel E43 = ("`p2'")
	
	test (egalitarian_mother=0.25*(`s1'/`s2')) 
	local p3 = string(`r(p)',"%9.3fc")
	putexcel E44 = ("`p3'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p4 = string(`r(p)',"%9.3fc")
	putexcel E45 = ("`p4'")
	
	xi: rego egalitarian egalitarian_father egalitarian_mother \ `background' \ `parents' \ `siblings' \ `household' \ `cog_std' `big_5' \ `loc_std' \ i.district_code if ustood_social==1
	
	matrix temp = e(shapley_perc)
	matrix temp_b = temp["r1","SV1"]
	local owen_raw = temp_b[1,1]
	local owen = string(`owen_raw',"%9.2fc")
	putexcel E38 = ("`owen'")
	
	local hered = `owen_raw' * `r2_raw' 
	local hered = string(`hered',"%9.3fc")
	putexcel E39 = ("`hered'")
	
	local rf = 2*`b_1_raw'*(`s2'/`s1')
	local rf = string(`rf',"%9.3fc")
	putexcel E46 = ("`rf'")
	
	local rm = 2*`b_2_raw'*(`s2'/`s1')
	local rm = string(`rm',"%9.3fc")
	putexcel E47 = ("`rm'")
	
	//Altruistic
	
	reg altruistic altruistic_father altruistic_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2_raw = `e(r2)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x'_raw = _b[`var']
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B1") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F36 = ("`n'")
	putexcel F37 = ("`r2'")
	putexcel F40 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F42 = ("`p1'")
	
	sum altruistic_sd
	local s1 = `r(mean)'
	
	sum altruistic_sd_par
	local s2 = `r(mean)'
	
	local genetic = 0.25*(`s1'/`s2')
	local genetic = string(`genetic',"%9.3fc")
	putexcel F41 = ("`genetic'")

	test (altruistic_father=0.25*(`s1'/`s2')) 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F43 = ("`p2'")
	
	test (altruistic_mother=0.25*(`s1'/`s2')) 
	local p3 = string(`r(p)',"%9.3fc")
	putexcel F44 = ("`p3'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p4 = string(`r(p)',"%9.3fc")
	putexcel F45 = ("`p4'")
	
	xi: rego altruistic altruistic_father altruistic_mother \ `background' \ `parents' \ `siblings' \ `household' \ `cog_std' `big_5' \ `loc_std' \ i.district_code if ustood_social==1
	
	matrix temp = e(shapley_perc)
	matrix temp_b = temp["r1","SV1"]
	local owen_raw = temp_b[1,1]
	local owen = string(`owen_raw',"%9.2fc")
	putexcel F38 = ("`owen'")
	
	local hered = `owen_raw' * `r2_raw' 
	local hered = string(`hered',"%9.3fc")
	putexcel F39 = ("`hered'")
	
	local rf = 2*`b_1_raw'*(`s2'/`s1')
	local rf = string(`rf',"%9.3fc")
	putexcel F46 = ("`rf'")
	
	local rm = 2*`b_2_raw'*(`s2'/`s1')
	local rm = string(`rm',"%9.3fc")
	putexcel F47 = ("`rm'")
	
	//Selfish
	
	reg selfish selfish_father selfish_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2_raw = `e(r2)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x'_raw = _b[`var']
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B1") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G36 = ("`n'")
	putexcel G37 = ("`r2'")
	putexcel G40 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G42 = ("`p1'")
	
	sum selfish_sd
	local s1 = `r(mean)'
	
	sum selfish_sd_par
	local s2 = `r(mean)'
	
	local genetic = 0.25*(`s2'/`s1')
	local genetic = string(`genetic',"%9.3fc")
	putexcel G41 = ("`genetic'")

	test (selfish_father=0.25*(`s2'/`s1')) 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G43 = ("`p2'")
	
	test (selfish_mother=0.25*(`s1'/`s2')) 
	local p3 = string(`r(p)',"%9.3fc")
	putexcel G44 = ("`p3'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p4 = string(`r(p)',"%9.3fc")
	putexcel G45 = ("`p4'")
	
	xi: rego selfish selfish_father selfish_mother \ `background' \ `parents' \ `siblings' \ `household' \ `cog_std' `big_5' \ `loc_std' \ i.district_code if ustood_social==1
	
	matrix temp = e(shapley_perc)
	matrix temp_b = temp["r1","SV1"]
	local owen_raw = temp_b[1,1]
	local owen = string(`owen_raw',"%9.2fc")
	putexcel G38 = ("`owen'")
	
	local hered = `owen_raw' * `r2_raw' 
	local hered = string(`hered',"%9.3fc")
	putexcel G39 = ("`hered'")
	
	local rf = 2*`b_1_raw'*(`s2'/`s1')
	local rf = string(`rf',"%9.3fc")
	putexcel G46 = ("`rf'")
	
	local rm = 2*`b_2_raw'*(`s2'/`s1')
	local rm = string(`rm',"%9.3fc")
	putexcel G47 = ("`rm'")
	
****************************************************************************************************************
*** Table B.2: Child preferences corrected for heritability priors *** 
****************************************************************************************************************	
	use "$constructdata/appendB.dta", clear 

	//We first generate preferences net of genetics: K -0.25*Sk/Smf(M+F)
	gen pc_minus_par	= patient_choices-0.25*(patient_choices_sd/patient_choices_sd_par)*(patient_choices_father+patient_choices_mother)
	gen bins_minus_par	= binswanger-0.25*(binswanger_sd/patient_choices_sd_par/binswanger_sd_par)*(binswanger_father+binswanger_mother)
	gen spite_minus_par	= spiteful-0.25*(spiteful_sd/spiteful_sd_par)*(spiteful_father+spiteful_mother)
	gen egal_minus_par	= egalitarian-0.25*(egalitarian_sd/egalitarian_sd_par)*(egalitarian_father+egalitarian_mother)
	gen altru_minus_par	= altruistic-0.25*(altruistic_sd/altruistic_sd_par)*(altruistic_father+altruistic_mother)
	gen self_minus_par	= selfish-0.25*(selfish_sd/selfish_sd_par)*(selfish_father+selfish_mother)

	//Controls
	loc background 		"gender age_at_exp schooling school_attending"
	loc siblings 		"elder_bro elder_sis younger_bro younger_sis"
	loc parents 		"age_father age_mother schooling_father schooling_mother"
	loc household 		"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std			"FSIQ_std"
	loc big_5			"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std			"loc_std"
	
	loc vill			"population"
	
	loc keepvar			"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions 	"excel lab dec(3) nocons nonotes"
	loc notes 			"Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text 			"District Fixed Effects are included?"
	loc ap 				"replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect 		"i.district_code"
	
	*******************************************************
	* Child preferences corrected for heritability priors *
	*******************************************************

	//Time preference 
	reg pc_minus_par patient_choices_father patient_choices_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B2") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B6 = ("`n'")
	putexcel B7 = ("`r2'")
	putexcel B8 = ("Yes")
		
	test (patient_choices_father patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B9 = ("`p1'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel B10 = ("`p2'")
	
	//Risk preferences
	reg bins_minus_par binswanger_father binswanger_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B2") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C6 = ("`n'")
	putexcel C7 = ("`r2'")
	putexcel C8 = ("Yes")
		
	test (binswanger_father binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C9 = ("`p1'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel C10 = ("`p2'")
	
	// Spiteful
	reg spite_minus_par spiteful_father spiteful_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B2") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D6 = ("`n'")
	putexcel D7 = ("`r2'")
	putexcel D8 = ("Yes")
		
	test (spiteful_father spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D9 = ("`p1'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel D10 = ("`p2'")
	
	// Egalitarian	
	reg egal_minus_par egalitarian_father egalitarian_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B2") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E6 = ("`n'")
	putexcel E7 = ("`r2'")
	putexcel E8 = ("Yes")
		
	test (egalitarian_father egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E9 = ("`p1'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel E10 = ("`p2'")
	
	//Altruistic
	reg altru_minus_par altruistic_father altruistic_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B2") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F6 = ("`n'")
	putexcel F7 = ("`r2'")
	putexcel F8 = ("Yes")
		
	test (altruistic_father altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F9 = ("`p1'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel F10 = ("`p2'")
	
	//Selfish	
	reg self_minus_par selfish_father selfish_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B2") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G6 = ("`n'")
	putexcel G7 = ("`r2'")
	putexcel G8 = ("Yes")
		
	test (selfish_father selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G9 = ("`p1'")
	
	test `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' 
	local p2 = string(`r(p)',"%9.3fc")
	putexcel G10 = ("`p2'")
	
****************************************************************************************************************
*** Table B.3. Interacting mother’s and father’s preferences while accounting for genetic transmission *** 
****************************************************************************************************************	
	
	use "$constructdata/appendB.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//We first generate preferences net of genetics: K -0.25*Sk/Smf(M+F)
	gen pc_minus_par	= patient_choices-0.25*(patient_choices_sd/patient_choices_sd_par)*(patient_choices_father+patient_choices_mother)
	gen bins_minus_par	= binswanger-0.25*(binswanger_sd/patient_choices_sd_par/binswanger_sd_par)*(binswanger_father+binswanger_mother)
	gen spite_minus_par	= spiteful-0.25*(spiteful_sd/spiteful_sd_par)*(spiteful_father+spiteful_mother)
	gen egal_minus_par	= egalitarian-0.25*(egalitarian_sd/egalitarian_sd_par)*(egalitarian_father+egalitarian_mother)
	gen altru_minus_par	= altruistic-0.25*(altruistic_sd/altruistic_sd_par)*(altruistic_father+altruistic_mother)
	gen self_minus_par	= selfish-0.25*(selfish_sd/selfish_sd_par)*(selfish_father+selfish_mother)

	//Controls
	loc background 		"gender age_at_exp schooling school_attending"
	loc siblings 		"elder_bro elder_sis younger_bro younger_sis"
	loc parents 		"age_father age_mother schooling_father schooling_mother"
	loc household 		"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std			"FSIQ_std"
	loc big_5			"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std			"loc_std"
	
	loc vill			"population"
	
	loc keepvar			"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions 	"excel lab dec(3) nocons nonotes"
	loc notes 			"Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text 			"District Fixed Effects are included?"
	loc ap 				"replace"
	
	*******************************************************************************************
	* Interacting mother’s and father’s preferences while accounting for genetic transmission *
	*******************************************************************************************
	
	//Time preference 
	reg pc_minus_par c.patient_choices_father##c.patient_choices_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother c.patient_choices_father#c.patient_choices_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B3") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (c.patient_choices_father#c.patient_choices_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B41 = ("`p1'")
	
	//Risk preferences
	reg bins_minus_par c.binswanger_father##c.binswanger_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother c.binswanger_father#c.binswanger_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B3") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("Yes")
		
	test (c.binswanger_father#c.binswanger_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C41 = ("`p1'")
	
	
	//Spiteful
	reg spite_minus_par spiteful_father##spiteful_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.spiteful_father 1.spiteful_mother 1.spiteful_father#1.spiteful_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B3") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("Yes")
		
	test (1.spiteful_father#1.spiteful_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D41 = ("`p1'")
	
	//Egalitarian	
	reg egal_minus_par egalitarian_father##egalitarian_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.egalitarian_father 1.egalitarian_mother 1.egalitarian_father#1.egalitarian_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B3") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("Yes")
		
	test (1.egalitarian_father#1.egalitarian_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E41 = ("`p1'")

	//Altruistic
	reg altru_minus_par altruistic_father##altruistic_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.altruistic_father 1.altruistic_mother 1.altruistic_father#1.altruistic_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B3") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("Yes")
		
	test (1.altruistic_father#1.altruistic_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F41 = ("`p1'")
	
	//Selfish	
	reg self_minus_par selfish_father##selfish_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.selfish_father 1.selfish_mother 1.selfish_father#1.selfish_mother gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B3") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("Yes")
		
	test (1.selfish_father#1.selfish_mother) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G41 = ("`p1'")
	
****************************************************************************************************************
*** Table B.4. Interacting parental preferences and children’s age while accounting for genetic transmission *** 
****************************************************************************************************************	
	
	use "$constructdata/appendB.dta", clear 
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//We first generate preferences net of genetics: K -0.25*Sk/Smf(M+F)
	gen pc_minus_par	= patient_choices-0.25*(patient_choices_sd/patient_choices_sd_par)*(patient_choices_father+patient_choices_mother)
	gen bins_minus_par	= binswanger-0.25*(binswanger_sd/patient_choices_sd_par/binswanger_sd_par)*(binswanger_father+binswanger_mother)
	gen spite_minus_par	= spiteful-0.25*(spiteful_sd/spiteful_sd_par)*(spiteful_father+spiteful_mother)
	gen egal_minus_par	= egalitarian-0.25*(egalitarian_sd/egalitarian_sd_par)*(egalitarian_father+egalitarian_mother)
	gen altru_minus_par	= altruistic-0.25*(altruistic_sd/altruistic_sd_par)*(altruistic_father+altruistic_mother)
	gen self_minus_par	= selfish-0.25*(selfish_sd/selfish_sd_par)*(selfish_father+selfish_mother)

	//Controls
	loc background 		"gender age_at_exp schooling school_attending"
	loc siblings 		"elder_bro elder_sis younger_bro younger_sis"
	loc parents 		"age_father age_mother schooling_father schooling_mother"
	loc household 		"hhsize inc_per_cap_per_month_2016 grand_parents"
	
	loc cog_std			"FSIQ_std"
	loc big_5			"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std			"loc_std"
	
	loc vill			"population"
	
	loc keepvar			"`background' schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions 	"excel lab dec(3) nocons nonotes"
	loc notes 			"Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text 			"District Fixed Effects are included?"
	loc ap 				"replace"

	*************************************************************************************************
	* Interacting parental preferences and children’s age while accounting for genetic transmission *
	*************************************************************************************************
		
	//Time preference 
	reg pc_minus_par c.patient_choices_father##c.age_at_exp c.patient_choices_mother##c.age_at_exp `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother age_at_exp c.patient_choices_father#c.age_at_exp c.patient_choices_mother#c.age_at_exp gender schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B4") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B40 = ("`n'")
	putexcel B41 = ("`r2'")
	putexcel B42 = ("Yes")
		
	test (c.patient_choices_father#c.age_at_exp c.patient_choices_mother#c.age_at_exp ) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel B43 = ("`p1'")


	//Risk preferences
	reg bins_minus_par c.binswanger_father##c.age_at_exp c.binswanger_mother##c.age_at_exp `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother age_at_exp c.binswanger_father#c.age_at_exp c.binswanger_mother#c.age_at_exp gender schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B4") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C40 = ("`n'")
	putexcel C41 = ("`r2'")
	putexcel C42 = ("Yes")
		
	test (c.binswanger_father#c.age_at_exp c.binswanger_mother#c.age_at_exp ) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel C43 = ("`p1'")

	//Spiteful
	reg spite_minus_par spiteful_father##c.age_at_exp spiteful_mother##c.age_at_exp `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.spiteful_father 1.spiteful_mother age_at_exp 1.spiteful_father#c.age_at_exp 1.spiteful_mother#c.age_at_exp gender schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B4") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D40 = ("`n'")
	putexcel D41 = ("`r2'")
	putexcel D42 = ("Yes")
		
	test (1.spiteful_father#c.age_at_exp 1.spiteful_mother#c.age_at_exp ) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel D43 = ("`p1'")

	//Egalitarian	
	reg egal_minus_par egalitarian_father##c.age_at_exp egalitarian_mother##c.age_at_exp `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.egalitarian_father 1.egalitarian_mother age_at_exp 1.egalitarian_father#c.age_at_exp 1.egalitarian_mother#c.age_at_exp gender schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B4") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E40 = ("`n'")
	putexcel E41 = ("`r2'")
	putexcel E42 = ("Yes")
		
	test (1.egalitarian_father#c.age_at_exp 1.egalitarian_mother#c.age_at_exp ) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel E43 = ("`p1'")

	//Altruistic
	reg altru_minus_par altruistic_father##c.age_at_exp altruistic_mother##c.age_at_exp `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.altruistic_father 1.altruistic_mother age_at_exp 1.altruistic_father#c.age_at_exp 1.altruistic_mother#c.age_at_exp gender schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B4") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F40 = ("`n'")
	putexcel F41 = ("`r2'")
	putexcel F42 = ("Yes")
		
	test (1.altruistic_father#c.age_at_exp 1.altruistic_mother#c.age_at_exp ) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel F43 = ("`p1'")

	//Selfish	
	reg self_minus_par selfish_father##c.age_at_exp selfish_mother##c.age_at_exp `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in 1.selfish_father 1.selfish_mother age_at_exp 1.selfish_father#c.age_at_exp 1.selfish_mother#c.age_at_exp gender schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		local x = `x' + 1
		local b_`x' = string(_b[`var'],"%9.3fc")
		local sig = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))
		
		if `sig' < 0.01 {
			local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			local b_`x' = "`b_`x''*"
			}
		
		local se_`x' = "(" + string(_se[`var'],"%9.3fc") + ")" 
		
		putexcel set "$tablesapp/Tables Append Analysis.xlsx", sheet("Table B4") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G40 = ("`n'")
	putexcel G41 = ("`r2'")
	putexcel G42 = ("Yes")
		
	test (1.selfish_father#c.age_at_exp 1.selfish_mother#c.age_at_exp ) 
	local p1 = string(`r(p)',"%9.3fc")
	putexcel G43 = ("`p1'")

