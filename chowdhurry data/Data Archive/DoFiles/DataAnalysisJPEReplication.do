***********************************************************************************************************************
*** Replication Files for Chowdhury, Sutter, and Zimmermann ***
*** "Economic preferences across generations and family clusters: A large-scale experiment in a developing country" ***
***********************************************************************************************************************
***********************************************************************************************************************

***********************************************************************************************************************
*** DATA ANALYSIS: This file takes the constructed data and creates the tables in the paper ***
***********************************************************************************************************************

*** Set directory 

capture cd "~/Data Archive"


************************************************
*   Set Globals for Directories                 *
************************************************
	global constructdata 	"ConstructedData"
	global rawdata 			"RawData"
	global figures 			"FiguresMain"
	global tables	  		"TablesMain"

	*** Set Stata version
	capture version 16.0
	capture log close
	set more off
	
*************************************************************************************************************
*** Figure 1: The two family clusters dependent on two factors from a principal component analysis of 
*** economic preferences *******
*************************************************************************************************************
	
	**This is produced in R. See the FiguresJPEReplication.R in the replication folder. 

*************************************************************************************************************
*** Figure 2: Figure 2: Relationship between IQ and patience, conditional on income level of country ****
*************************************************************************************************************

	**This is produced in R. See the FiguresJPEReplication.R in the replication folder. 

**************************************************************************************************************
*** Figure 3: Relationship between years of schooling and patience, conditional on income level of country ***
**************************************************************************************************************

	**This is produced in R. See the FiguresJPEReplication.R in the replication folder. 

**************************************************************************************************************
*** Table 1: Summary statistics of participants ***
*** The results estimated below requires formatting, i.e., putting in Excel manually ***
**************************************************************************************************************

//parents
	use "$constructdata\children.dta", clear 
	
	gen 	husband_farmer=1 if prof_father==0 & gender==1
	replace husband_farmer=0 if prof_father~=0 & gender==1
	
	gen 	wife_housewife=1 if prof_mother==5 & gender==0
	replace wife_housewife=0 if prof_mother~=5 & gender==0
	
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 * 1000
	
	local x = 2
	
	foreach var in age_father age_mother schooling_father schooling_mother husband_farmer wife_housewife {
		
		local x = `x' + 1
		
		sum `var'
		local mean_`var' = string(`r(mean)',"%9.2fc")
		local sd_`var' = string(`r(sd)',"%9.2fc")
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 1") modify
		putexcel B`x'=("`mean_`var''")
		putexcel C`x'=("`sd_`var''")
		
		}
	
	//children
	
	local x = `x' + 1 
	
	foreach var in gender age_at_exp schooling school_attending elder_bro elder_sis younger_bro younger_sis {
		
		local x = `x' + 1
		
		sum `var'
		local mean_`var' = string(`r(mean)',"%9.2fc")
		local sd_`var' = string(`r(sd)',"%9.2fc")
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 1") modify
		putexcel B`x'=("`mean_`var''")
		putexcel C`x'=("`sd_`var''")
		
		}

	//household 
	
	local x = `x' + 1 
	
	foreach var in hhsize grand_parents inc_per_cap_per_month_2016 population {
		
		local x = `x' + 1
		
		sum `var'
		local mean_`var' = string(`r(mean)',"%9.2fc")
		local sd_`var' = string(`r(sd)',"%9.2fc")
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 1") modify
		putexcel B`x'=("`mean_`var''")
		putexcel C`x'=("`sd_`var''")
		
		}




******************************************************************************************************************
*** Table 2: Time Prferences ****
******************************************************************************************************************
	
	*These are numbers shown to subjects who took part in the experiments 
	*See the detailed experimental procedures described in Appendix C


	******************************************************************************************************************
*** Table 3: Risk Prferences: Payoffs Offered Different Age Groups (taka) ****
******************************************************************************************************************
	
	*These are numbers shown to subjects who took part in the experiments 
	*See the detailed experimental procedures described in Appendix C
	

	******************************************************************************************************************
*** Table 4: Social Prferences ****
******************************************************************************************************************
	
	*These are numbers shown to subjects who took part in the experiments 
	*See the detailed experimental procedures described in Appendix C
	
	

******************************************************************************************************************
*** Table 5: Classification of subjects into four social preference types based ***
*** on the games presented in Table 4 ****
******************************************************************************************************************

	*This comes from the experiments, the details of whihch is described in the paper.

*******************************************************************************************************************
*** Table 6: Economic preferences of parents and children – Descriptive overview ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************

//parents
	use "$constructdata/parents.dta", clear 
	//Total
	//unclassified
	gen 	unclassified=.
	replace unclassified=0 if altruistic==1
	replace unclassified=0 if egalitarian==1 
	replace unclassified=0 if spiteful==1  
	replace unclassified=0 if selfish==1
	replace unclassified=1 if unclassified==. & altruistic~=. & egalitarian~=. & spiteful~=. &  selfish~=.
	
	* Parents - all
	
	tabstat patient_choices binswanger altruistic egalitarian spiteful selfish unclassified, statistics(mean sd) save
	tabstatmat parents_all
	mat parents_all = parents_all'
	matlist parents_all
	
	sum patient_choices
	local num_all = `r(N)'
	
	* Parents - wives
	
	tabstat patient_choices binswanger altruistic egalitarian spiteful selfish unclassified if gender == 0, statistics(mean sd) save
	tabstatmat parents_w
	mat parents_w = parents_w'
	matlist parents_w
	
	sum patient_choices if gender == 0
	local num_w = `r(N)'
	
	* Parents - husbands
	
	tabstat patient_choices binswanger altruistic egalitarian spiteful selfish unclassified if gender == 1, statistics(mean sd) save
	tabstatmat parents_h
	mat parents_h = parents_h'
	matlist parents_h
	
	sum patient_choices if gender == 1
	local num_h = `r(N)'
	

	* T-tests 
	
	loc prefs patient_choices binswanger altruistic egalitarian spiteful  selfish unclassified

	loc x = 0
	
	foreach var in `prefs' {
	
		loc x = `x' + 1
	
		ttest `var', by (gender)
		
		loc rp`x' = `r(p)'
		
		}
		
	mat ttest = `rp1' \ `rp2' \ `rp3' \ `rp4' \ `rp5' \ `rp6' \ `rp7'
	mat list ttest
	
	* Outsheet tables
	
	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 6") modify
	putexcel B5=mat(parents_all), nfor(number_d2)
	putexcel D5=mat(parents_w), nfor(number_d2)
	putexcel F5=mat(parents_h), nfor(number_d2)
	putexcel H5=mat(ttest), nfor(number_d2)
	
	putexcel B12=(`num_all')
	putexcel D12=(`num_w')
	putexcel F12=(`num_h')

//children

	use "$constructdata/children.dta", clear
	
	//unclassified
	gen 	unclassified=.
	replace unclassified=0 if altruistic==1
	replace unclassified=0 if egalitarian==1 
	replace unclassified=0 if spiteful==1  
	replace unclassified=0 if selfish==1
	replace unclassified=1 if unclassified==. & altruistic~=. & egalitarian~=. & spiteful~=. &  selfish~=.
	
	* Children - all
	
	tabstat patient_choices binswanger altruistic egalitarian spiteful selfish unclassified, statistics(mean sd) save
	tabstatmat parents_all
	mat parents_all = parents_all'
	matlist parents_all
	
	sum patient_choices
	local num_all = `r(N)'
	
	* Children - girls
	
	tabstat patient_choices binswanger altruistic egalitarian spiteful selfish unclassified if gender == 0, statistics(mean sd) save
	tabstatmat parents_g
	mat parents_g = parents_g'
	matlist parents_g
	
	sum patient_choices if gender == 0
	local num_g = `r(N)'
	
	* Children - boys
	
	tabstat patient_choices binswanger altruistic egalitarian spiteful selfish unclassified if gender == 1, statistics(mean sd) save
	tabstatmat parents_b
	mat parents_b = parents_b'
	matlist parents_b
	
	sum patient_choices if gender == 1
	local num_b = `r(N)'
	

	* T-tests 
	
	loc prefs patient_choices binswanger altruistic egalitarian spiteful  selfish unclassified

	loc x = 0
	
	foreach var in `prefs' {
	
		loc x = `x' + 1
	
		ttest `var', by (gender)
		
		loc rp`x' = `r(p)'
		
		}
		
	mat ttest = `rp1' \ `rp2' \ `rp3' \ `rp4' \ `rp5' \ `rp6' \ `rp7'
	mat list ttest
	
	* Outsheet tables
	
	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 6") modify
	putexcel B18=mat(parents_all), nfor(number_d2)
	putexcel D18=mat(parents_g), nfor(number_d2)
	putexcel F18=mat(parents_b), nfor(number_d2)
	putexcel H18=mat(ttest), nfor(number_d2)
	
	putexcel B25=(`num_all')
	putexcel D25=(`num_g')
	putexcel F25=(`num_b')

*******************************************************************************************************************
*** Table 7: Correlations of economic preferences ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************
	
	//husband and wives
	
	use "$constructdata/parents.dta", clear 

	keep if gender==1
	rename patient_choices patient_choices_h
	rename binswanger binswanger_h
	rename spiteful spiteful_h
	rename egalitarian egalitarian_h
	rename altruistic altruistic_h
	rename selfish selfish_h
	sort slno
	save "$constructdata/temporary/father.dta", replace 

	use "$constructdata/parents.dta", clear 
	keep if gender==0
	sort slno
	drop mid gender 
	rename patient_choices patient_choices_w
	rename binswanger binswanger_w
	rename spiteful spiteful_w
	rename egalitarian egalitarian_w
	rename altruistic altruistic_w
	rename selfish selfish_w
	sort slno
	save "$constructdata/temporary/mother.dta", replace 

	merge 1:1 slno using "$constructdata/temporary/father.dta"

	//correlations
	
	* We will take the output and add a star according to the sigma

	pwcorr patient_choices_h patient_choices_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel B3 = ("`rho'")

	pwcorr binswanger_h binswanger_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel B4 = ("`rho'")

	pwcorr spiteful_h spiteful_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel B5 = ("`rho'")
	
	pwcorr egalitarian_h egalitarian_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel B6 = ("`rho'")
	
	pwcorr altruistic_h altruistic_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel B7 = ("`rho'")
	
	pwcorr selfish_h selfish_w, sig 
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel B8 = ("`rho'")

	//siblings
	use "$constructdata/children.dta", clear 
	keep slno mid age_at_exp gender patient_choices binswanger spiteful egalitarian altruistic selfish 

	egen v1=max(age_at_exp), by(slno) 	
	egen v2=count(mid), by (slno)
	gen elder=1 if age_at_exp>=v1 & age_at_exp~=. & v2>1
	replace elder=0 if elder==. & age_at_exp~=. & v2>1
	la var elder "elder sibling =1, younger sibling=0" /*we have 129 children who do not have elder siblings in our data*/
	drop v1 v2
		
	foreach V in patient_choices binswanger spiteful egalitarian altruistic selfish  {
		bysort slno: gen v1=`V' if elder==1
		egen `V'_sib=max(v1), by(slno)
		drop v1
	}
	sort slno mid
	
	save "$constructdata/temporary/siblings1.dta", replace

	//correlations - siblings

	bysort slno: gen n=_n
	order slno mid n patient_choices patient_choices_sib binswanger binswanger_sib spiteful spiteful_sib egalitarian egalitarian_sib egalitarian_sib altruistic altruistic_sib altruistic_sib selfish selfish_sib
	
	keep if n==2
	
	* We will take the output and add a star according to the sigma

	pwcorr patient_choices patient_choices_sib, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel C3 = ("`rho'")

	pwcorr binswanger binswanger_sib, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel C4 = ("`rho'")

	pwcorr spiteful spiteful_sib, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel C5 = ("`rho'")
	
	pwcorr egalitarian egalitarian_sib, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel C6 = ("`rho'")
	
	pwcorr altruistic altruistic_sib, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel C7 = ("`rho'")
	
	pwcorr selfish selfish_sib, sig 
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel C8 = ("`rho'")

	//correlations mother and  children
	
	use "$constructdata/temporary/siblings1.dta", clear
	keep slno mid age_at_exp patient_choices binswanger spiteful egalitarian altruistic selfish
	sort slno mid
	save "$constructdata/temporary/siblings2.dta", replace
	
	merge m:1 slno using "$constructdata/temporary/mother.dta"
	ta _m
	drop _m
	
	* We will take the output and add a star according to the sigma

	pwcorr patient_choices patient_choices_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel D3 = ("`rho'")

	pwcorr binswanger binswanger_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel D4 = ("`rho'")

	pwcorr spiteful spiteful_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel D5 = ("`rho'")
	
	pwcorr egalitarian egalitarian_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel D6 = ("`rho'")
	
	pwcorr altruistic altruistic_w, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel D7 = ("`rho'")
	
	pwcorr selfish selfish_w, sig 
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel D8 = ("`rho'")
		
	//correlations father and  children
	use "$constructdata/temporary/siblings2.dta", clear
	merge m:1 slno using "$constructdata/temporary/father.dta"
	ta _m
	drop _m
	
	* We will take the output and add a star according to the sigma

	pwcorr patient_choices patient_choices_h, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel E3 = ("`rho'")

	pwcorr binswanger binswanger_h, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel E4 = ("`rho'")

	pwcorr spiteful spiteful_h, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel E5 = ("`rho'")
	
	pwcorr egalitarian egalitarian_h, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel E6 = ("`rho'")
	
	pwcorr altruistic altruistic_h, sig
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel E7 = ("`rho'")
	
	pwcorr selfish selfish_h, sig 
	local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
	local sig = 2*ttail(`r(N)'-2, abs(`first'))
	local rho = string(`r(rho)',"%9.3fc")
	
	if `sig' < 0.01 {
		local rho = "`rho'***"
		}
		
	if `sig' < 0.05 & `sig' >= 0.01 {
		local rho = "`rho'**"
		}
		
	if `sig' < 0.1 & `sig' >= 0.05 {
		local rho = "`rho'*"
		}

	putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 7") modify
	putexcel E8 = ("`rho'")
	

*******************************************************************************************************************
*** Table 8: Assortativity of parental preferences ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************

	use "$constructdata/parents.dta", clear
	
	//Adapting Per capita income per month in 2016 to thousands
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
	//Controls
	
	loc background 			"age_at_exp age_diff schooling schol_diff i.profession children"
	loc siblings 		 	"elder_bro elder_sis younger_bro younger_sis"
	loc household 		  	"hhsize inc_per_cap_per_month_2016"
	
	loc cog_std				"FSIQ_std"
	loc big_5				"conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std"
	loc loc_std				"loc_std"
	
	loc vill				"population"
	
	loc keepvar				"age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std `big_5' loc_std"
	
	loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "."
	
	loc text "District Fixed Effects are included?"
	loc ap "replace"

	//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "i.district_code"
	
	**********************************
	* Panel A: Husbands‘ preferences *
	**********************************

	//Time preference - number of patient_choices
	reg patient_choices patient_choices_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1 & gender==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 2
	
	foreach var in patient_choices_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B33 = ("`n'")
	putexcel B34 = ("`r2'")
	putexcel B35 = ("Yes")
		
	
	//Risk preference - lottery number picked
	reg binswanger binswanger_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1 & gender==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 2
	
	foreach var in binswanger_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C33 = ("`n'")
	putexcel C34 = ("`r2'")
	putexcel C35 = ("Yes")
	
	//Social preferences - spiteful 
	xi: dprobit spiteful spiteful_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in spiteful_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D33 = ("`n'")
	putexcel D34 = ("`r2'")
	putexcel D35 = ("Yes")
	
	//Social preferences - egalitarian
	xi: dprobit egalitarian egalitarian_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in egalitarian_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E33 = ("`n'")
	putexcel E34 = ("`r2'")
	putexcel E35 = ("Yes")
	
	//Social preferences - altruistic
	xi: dprobit altruistic altruistic_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in altruistic_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F33 = ("`n'")
	putexcel F34 = ("`r2'")
	putexcel F35 = ("Yes")
	
	//Social preferences - selfish
	xi: dprobit selfish selfish_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in selfish_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G33 = ("`n'")
	putexcel G34 = ("`r2'")
	putexcel G35 = ("Yes")
	
	*******************************
	* Panel B Wife's preferences" *
	*******************************
	
	//Time preference - number of patient_choices
	reg patient_choices patient_choices_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1 & gender==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 2
	
	foreach var in patient_choices_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8 - continued") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B33 = ("`n'")
	putexcel B34 = ("`r2'")
	putexcel B35 = ("Yes")
		
	
	//Risk preference - lottery number picked
	reg binswanger binswanger_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1 & gender==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 2
	
	foreach var in binswanger_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8 - continued") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C33 = ("`n'")
	putexcel C34 = ("`r2'")
	putexcel C35 = ("Yes")
	
	//Social preferences - spiteful 
	xi: dprobit spiteful spiteful_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in spiteful_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8 - continued") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D33 = ("`n'")
	putexcel D34 = ("`r2'")
	putexcel D35 = ("Yes")
	
	//Social preferences - egalitarian
	xi: dprobit egalitarian egalitarian_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in egalitarian_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8 - continued") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E33 = ("`n'")
	putexcel E34 = ("`r2'")
	putexcel E35 = ("Yes")
	
	//Social preferences - altruistic
	xi: dprobit altruistic altruistic_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in altruistic_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8 - continued") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F33 = ("`n'")
	putexcel F34 = ("`r2'")
	putexcel F35 = ("Yes")
	
	//Social preferences - selfish
	xi: dprobit selfish selfish_spouse `background' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & gender==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 2
	
	foreach var in selfish_spouse age_at_exp age_diff schooling schol_diff children hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 8 - continued") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G33 = ("`n'")
	putexcel G34 = ("`r2'")
	putexcel G35 = ("Yes")
*/	
*******************************************************************************************************************
*** Table 9: Children’s preferences and their relation to parental preferences ***
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

	//Time preference - number of patient_choices
	reg patient_choices patient_choices_father patient_choices_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 9") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B36 = ("`n'")
	putexcel B37 = ("`r2'")
	putexcel B38 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel B39 = ("`p1'")
	
	//Risk preferences - lottery number picked 
	reg binswanger binswanger_father binswanger_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 9") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C36 = ("`n'")
	putexcel C37 = ("`r2'")
	putexcel C38 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel C39 = ("`p1'")
	
	
	//Social preferences - spiteful
	xi: dprobit spiteful spiteful_father spiteful_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 9") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D36 = ("`n'")
	putexcel D37 = ("`r2'")
	putexcel D38 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel D39 = ("`p1'")

	//Social preferences - egalitarian
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 9") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E36 = ("`n'")
	putexcel E37 = ("`r2'")
	putexcel E38 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel E39 = ("`p1'")

	//Social preferences - altruistic
	xi: dprobit altruistic altruistic_father altruistic_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 9") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F36 = ("`n'")
	putexcel F37 = ("`r2'")
	putexcel F38 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel F39 = ("`p1'")
	
	//Social preferences - selfish
	xi: dprobit selfish selfish_father selfish_mother `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 9") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G36 = ("`n'")
	putexcel G37 = ("`r2'")
	putexcel G38 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel G39 = ("`p1'")
	
*/	
*******************************************************************************************************************
*** Table 10: Children’s preferences and their relation to parental preferences – ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************
	use "$constructdata/table10.dta", clear 
	replace inc_per_cap_per_month_2016 = inc_per_cap_per_month_2016 / 1000
	
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
	
	*********************************************************************************************************
	* Children’s preferences and their relation to parental preferences taking parenting style into account *
	*********************************************************************************************************
	
	//Time preference - number of patient_choices
	reg patient_choices patient_choices_father patient_choices_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother negative_parenting positive_parenting gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 10") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B40 = ("`n'")
	putexcel B41 = ("`r2'")
	putexcel B42 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel B43 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel B44 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel B45 = ("`p3'")
	
	
	//Risk preferences - lottery number picked
	reg binswanger binswanger_father binswanger_mother `parenting'  `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother negative_parenting positive_parenting gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 10") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C40 = ("`n'")
	putexcel C41 = ("`r2'")
	putexcel C42 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel C43 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel C44 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel C45 = ("`p3'")
	
	
	//Social preferences Variables - spiteful
	
	xi: dprobit spiteful spiteful_father spiteful_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother negative_parenting positive_parenting gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 10") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D40 = ("`n'")
	putexcel D41 = ("`r2'")
	putexcel D42 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel D43 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel D44 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel D45 = ("`p3'")
	
	//Social preferences Variables - egalitarian		
	
	xi: dprobit egalitarian egalitarian_father egalitarian_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother negative_parenting positive_parenting gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 10") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E40 = ("`n'")
	putexcel E41 = ("`r2'")
	putexcel E42 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel E43 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel E44 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel E45 = ("`p3'")
	
	//Social preferences Variables - altruistic	
	
	xi: dprobit altruistic altruistic_father altruistic_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother negative_parenting positive_parenting gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 10") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F40 = ("`n'")
	putexcel F41 = ("`r2'")
	putexcel F42 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel F43 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel F44 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel F45 = ("`p3'")
	
	//Social preferences Variables - selfish	
	xi: dprobit selfish selfish_father selfish_mother `parenting' `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother negative_parenting positive_parenting gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 10") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G40 = ("`n'")
	putexcel G41 = ("`r2'")
	putexcel G42 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel G43 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel G44 = ("`p2'")
	
	test negative_parenting positive_parenting
	local p3 = string(r(p),"%9.3fc")
	putexcel G45 = ("`p3'")
	
*******************************************************************************************************************
*** Table 11: Children’s preferences and their relation to parental preferences – ***
*** Adding homogeneity/heterogeneity of parents ***
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
	
	*********************************************************************************
	* Children preferences and their relation to parental preferences heterogeneity *
	*********************************************************************************
	
	//Patient choices
	reg patient_choices patient_choices_father patient_choices_mother pc_homo_f pc_homo_m pc_homo  `childs_background' `siblings' `parents' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother pc_homo_f pc_homo_m pc_homo {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 11") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B12 = ("`n'")
	putexcel B13 = ("`r2'")
	putexcel B14 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel B15 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel B16 = ("`p2'")
	
	test (pc_homo_f + patient_choices_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel B17 = ("`p3'")
	
	test (patient_choices_mother + pc_homo_m =0)
	local p4 = string(r(p),"%9.3fc")
	putexcel B18 = ("`p4'")
	
		
	//Risk - Lottery number picked
	
	reg binswanger binswanger_father binswanger_mother bins_homo_f bins_homo_m bins_homo  `childs_background' `siblings' `parents' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother bins_homo_f bins_homo_m bins_homo {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 11") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C12 = ("`n'")
	putexcel C13 = ("`r2'")
	putexcel C14 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel C15 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel C16 = ("`p2'")
	
	test (bins_homo_f + binswanger_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel C17 = ("`p3'")
	
	test (bins_homo_m + binswanger_mother =0) 
	local p4 = string(r(p),"%9.3fc")
	putexcel C18 = ("`p4'")
	

	//Spiteful
	
	xi: dprobit spiteful spiteful_father spiteful_mother spit_homo_f spit_homo_m spit_homo `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if  ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother spit_homo_f spit_homo_m spit_homo {	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 11") modify
		local y = `y' + 1
		putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D12 = ("`n'")
	putexcel D13 = ("`r2'")
	putexcel D14 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel D15 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel D16 = ("`p2'")
	
	test (spit_homo_f + spiteful_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel D17 = ("`p3'")
	
	test (spit_homo_m + spiteful_mother =0) 
	local p4 = string(r(p),"%9.3fc")
	putexcel D18 = ("`p4'")
			

	//Egalitarian
	
	xi: dprobit egalitarian egalitarian_father egalitarian_mother egal_homo_f egal_homo_m  egal_homo `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother egal_homo_f egal_homo_m  egal_homo {	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 11") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E12 = ("`n'")
	putexcel E13 = ("`r2'")
	putexcel E14 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel E15 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel E16 = ("`p2'")
	
	test (egal_homo_f + egalitarian_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel E17 = ("`p3'")
	
	test (egal_homo_m + egalitarian_mother =0) 
	local p4 = string(r(p),"%9.3fc")
	putexcel E18 = ("`p4'")
	
	//Altruistic
	xi: dprobit altruistic altruistic_father altruistic_mother altru_homo_f altru_homo_m altru_homo  `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother altru_homo_f altru_homo_m altru_homo {	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 11") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F12 = ("`n'")
	putexcel F13 = ("`r2'")
	putexcel F14 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel F15 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel F16 = ("`p2'")
	
	test (altru_homo_f + altruistic_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel F17 = ("`p3'")
	
	test (altru_homo_m + altruistic_mother =0) 
	local p4 = string(r(p),"%9.3fc")
	putexcel F18 = ("`p4'")
	
	//Selfish
	xi: dprobit selfish  selfish_father selfish_mother slfsh_homo_f slfsh_homo_m slfsh_homo `childs_background' `siblings'  `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother slfsh_homo_f slfsh_homo_m slfsh_homo {	
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 11") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G12 = ("`n'")
	putexcel G13 = ("`r2'")
	putexcel G14 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel G15 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel G16 = ("`p2'")
	
	test (slfsh_homo_f + selfish_father =0) 
	local p3 = string(r(p),"%9.3fc")
	putexcel G17 = ("`p3'")
	
	test (slfsh_homo_m + selfish_mother =0) 
	local p4 = string(r(p),"%9.3fc")
	putexcel G18 = ("`p4'")

*******************************************************************************************************************
*** Table 12: Children’s preferences and their relation to parental preferences – ***
*** Estimating the older sibling’s influence ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	use "$constructdata/table12.dta", clear 
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

	*******************************************
	* Estimating the older siblings influence *
	*******************************************
	
	//Patient choices
	
	reg patient_choices patient_choices_father patient_choices_mother pat_cho_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother pat_cho_res_sibling gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 12") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel B42 = ("`p2'")
	
	//Risk - lottery number picked
	
	reg binswanger binswanger_father binswanger_mother binsw_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother binsw_res_sibling gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 12") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel C42 = ("`p2'")
	
	//Social preferences - spiteful
	
	*loc background "gender age_at_exp schooling"
	xi: dprobit spiteful spiteful_father spiteful_mother spit_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother spit_res_sibling gender age_at_exp schooling { 
	
		local x = `x' + 1
		
		capture matrix temp_b_`x' = temp_b["r1","`var'"]
		capture matrix temp_se_`x' = temp_se["r1","`var'"]
		
		capture local b_`x' = temp_b_`x'[1,1]
		capture local se_`x' = temp_se_`x'[1,1]
		capture local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		capture local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			capture local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			capture local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			capture local b_`x' = "`b_`x''*"
			}
		
		capture local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 12") modify
		local y = `y' + 1
		capture putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		capture putexcel D`y' = ("`se_`x''")
		
		}
		
	local y = `y' + 2
	
	foreach var in schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
		
		local x = `x' + 1
		
		capture matrix temp_b_`x' = temp_b["r1","`var'"]
		capture matrix temp_se_`x' = temp_se["r1","`var'"]
		
		capture local b_`x' = temp_b_`x'[1,1]
		capture local se_`x' = temp_se_`x'[1,1]
		capture local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		capture local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			capture local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			capture local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			capture local b_`x' = "`b_`x''*"
			}
		
		capture local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 12") modify
		local y = `y' + 1
		capture putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		capture putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel D42 = ("`p2'")
	
	//Social preferences - egalitarian
	xi: dprobit egalitarian egalitarian_father egalitarian_mother egal_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother egal_res_sibling gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 12") modify
		local y = `y' + 1
		putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel E41 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel E42 = ("`p2'")
	
	//Social preferences - altruistic
	xi: dprobit altruistic altruistic_father altruistic_mother altru_res_sibling  `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother altru_res_sibling gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 12") modify
		local y = `y' + 1
		putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel F42 = ("`p2'")
	
	//Social preferences - selfish 
	xi: dprobit selfish selfish_father selfish_mother self_res_sibling `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1 & elder==0, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother self_res_sibling gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 12") modify
		local y = `y' + 1
		putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("Yes")
		
	test (selfish_father=selfish_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel G42 = ("`p2'")
	

*******************************************************************************************************************
*** Table 13: Children’s preferences and their relation to parental preferences – ***
*** Taking into account peers in one’s village ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
use "$constructdata/table13.dta", clear 
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

	*******************************************************************
	* Childrens preferences taking into account peers in ones village *
	*******************************************************************
	
	//Patient choices
	reg patient_choices patient_choices_father patient_choices_mother patient_choices_avg `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_time==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in patient_choices_father patient_choices_mother patient_choices_avg gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 13") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel B`y' = ("`se_`x''")
		
		}
	
	putexcel B38 = ("`n'")
	putexcel B39 = ("`r2'")
	putexcel B40 = ("Yes")
		
	test (patient_choices_father=patient_choices_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel B41 = ("`p1'")
	
	test patient_choices_father patient_choices_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel B42 = ("`p2'")
	
	//Risk preferences 
	
	reg binswanger binswanger_father binswanger_mother binswanger_avg `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_risk==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2)',"%9.3fc")
	
	local x = 0
	local y = 1
	
	foreach var in binswanger_father binswanger_mother binswanger_avg gender age_at_exp schooling school_attending schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std {
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
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 13") modify
		local y = `y' + 1
		putexcel C`y' = ("`b_`x''")
	
		local y = `y' + 1
		putexcel C`y' = ("`se_`x''")
		
		}
	
	putexcel C38 = ("`n'")
	putexcel C39 = ("`r2'")
	putexcel C40 = ("Yes")
		
	test (binswanger_father=binswanger_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel C41 = ("`p1'")
	
	test binswanger_father binswanger_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel C42 = ("`p2'")

	//Social preference - spiteful
	xi: dprobit spiteful spiteful_father spiteful_mother spiteful_avg `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in spiteful_father spiteful_mother spiteful_avg gender age_at_exp schooling school_attendance schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std { 
	
		local x = `x' + 1
		
		capture matrix temp_b_`x' = temp_b["r1","`var'"]
		capture matrix temp_se_`x' = temp_se["r1","`var'"]
		
		capture local b_`x' = temp_b_`x'[1,1]
		capture local se_`x' = temp_se_`x'[1,1]
		capture local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		capture local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			capture local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			capture local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			capture local b_`x' = "`b_`x''*"
			}
		
		capture local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 13") modify
		local y = `y' + 1
		capture putexcel D`y' = ("`b_`x''")
	
		local y = `y' + 1
		capture putexcel D`y' = ("`se_`x''")
		
		}
	
	putexcel D38 = ("`n'")
	putexcel D39 = ("`r2'")
	putexcel D40 = ("Yes")
		
	test (spiteful_father=spiteful_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel D41 = ("`p1'")
	
	test spiteful_father spiteful_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel D42 = ("`p2'")

	//Social preference - egalitarian
	xi: dprobit egalitarian egalitarian_father egalitarian_mother egalitarian_avg `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in egalitarian_father egalitarian_mother egalitarian_avg gender age_at_exp schooling school_attendance schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std { 
	
		local x = `x' + 1
		
		capture matrix temp_b_`x' = temp_b["r1","`var'"]
		capture matrix temp_se_`x' = temp_se["r1","`var'"]
		
		capture local b_`x' = temp_b_`x'[1,1]
		capture local se_`x' = temp_se_`x'[1,1]
		capture local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		capture local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			capture local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			capture local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			capture local b_`x' = "`b_`x''*"
			}
		
		capture local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 13") modify
		local y = `y' + 1
		capture putexcel E`y' = ("`b_`x''")
	
		local y = `y' + 1
		capture putexcel E`y' = ("`se_`x''")
		
		}
	
	putexcel E38 = ("`n'")
	putexcel E39 = ("`r2'")
	putexcel E40 = ("Yes")
		
	test (egalitarian_father=egalitarian_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel E41 = ("`p1'")
	
	test egalitarian_father egalitarian_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel E42 = ("`p2'")

	//Social preference - altruistic 
	xi: dprobit altruistic altruistic_father altruistic_mother  altruistic_avg `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in altruistic_father altruistic_mother  altruistic_avg gender age_at_exp schooling school_attendance schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std { 
	
		local x = `x' + 1
		
		capture matrix temp_b_`x' = temp_b["r1","`var'"]
		capture matrix temp_se_`x' = temp_se["r1","`var'"]
		
		capture local b_`x' = temp_b_`x'[1,1]
		capture local se_`x' = temp_se_`x'[1,1]
		capture local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		capture local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			capture local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			capture local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			capture local b_`x' = "`b_`x''*"
			}
		
		capture local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 13") modify
		local y = `y' + 1
		capture putexcel F`y' = ("`b_`x''")
	
		local y = `y' + 1
		capture putexcel F`y' = ("`se_`x''")
		
		}
	
	putexcel F38 = ("`n'")
	putexcel F39 = ("`r2'")
	putexcel F40 = ("Yes")
		
	test (altruistic_father=altruistic_mother) 
	local p1 = string(r(p),"%9.3fc")
	putexcel F41 = ("`p1'")
	
	test altruistic_father altruistic_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel F42 = ("`p2'")

	//Social preference - selfish 
	xi: dprobit selfish selfish_father selfish_mother selfish_avg `background' `parents' `siblings' `household' `cog_std' `big_5' `loc_std' `f_effect' if ustood_social==1, cl(`unit')
	
	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in selfish_father selfish_mother selfish_avg gender age_at_exp schooling school_attendance schooling_father schooling_mother hhsize inc_per_cap_per_month_2016 FSIQ_std conscientiousness_std extraversion_std agreeableness_std openness_std neuroticism_std loc_std { 
	
		local x = `x' + 1
		
		capture matrix temp_b_`x' = temp_b["r1","`var'"]
		capture matrix temp_se_`x' = temp_se["r1","`var'"]
		
		capture local b_`x' = temp_b_`x'[1,1]
		capture local se_`x' = temp_se_`x'[1,1]
		capture local sig = (2 * (1-normal(abs(_b[`var']/_se[`var']))))
		
		capture local b_`x' = string(`b_`x'',"%9.3fc")
		
		if `sig' < 0.01 {
			capture local b_`x' = "`b_`x''***"
			}
		
		if `sig' < 0.05 & `sig' >= 0.01 {
			capture local b_`x' = "`b_`x''**"
			}
		
		if `sig' < 0.1 & `sig' >= 0.05 {
			capture local b_`x' = "`b_`x''*"
			}
		
		capture local se_`x' = "(" + string(`se_`x'',"%9.3fc") + ")" 
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 13") modify
		local y = `y' + 1
		capture putexcel G`y' = ("`b_`x''")
	
		local y = `y' + 1
		capture putexcel G`y' = ("`se_`x''")
		
		}
	
	putexcel G38 = ("`n'")
	putexcel G39 = ("`r2'")
	putexcel G40 = ("Yes")
		
	test (selfish_father=selfish_mother)  
	local p1 = string(r(p),"%9.3fc")
	putexcel G41 = ("`p1'")
	
	test selfish_father selfish_mother
	local p2 = string(r(p),"%9.3fc")
	putexcel G42 = ("`p2'")


*******************************************************************************************************************
*** Table 14: Correlations across preferences (within individuals)***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	
	
	//Parents
	
	use "$constructdata/parents.dta", clear 
	keep slno mid gender patient_choices binswanger spiteful egalitarian altruistic selfish 
	gen 	unclassified=.
	replace unclassified=0 if altruistic==1
	replace unclassified=0 if egalitarian==1
	replace unclassified=0 if selfish==1
	replace unclassified=0 if spiteful==1
	replace unclassified=1 if unclassified==.

	loc husband patient_choices binswanger spiteful egalitarian altruistic selfish unclassified
	
	//Patient choices
	
	local x = 1
	
	foreach var in binswanger spiteful egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr patient_choices `var' if gender==1, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel B`x' = ("`rho'")
		
		}
		
	local x = 9
		
	foreach var in binswanger spiteful egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr patient_choices `var' if gender==0, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel B`x' = ("`rho'")
		
		}
	
	local x = 2
	
	// binswanger
	
	foreach var in spiteful egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr binswanger `var' if gender==1, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel C`x' = ("`rho'")
		
		}
		
	local x = 10
		
	foreach var in spiteful egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr binswanger `var' if gender==0, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel C`x' = ("`rho'")
		
		}
		
	local x = 3
	
	// spiteful
	
	foreach var in egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr spiteful `var' if gender==1, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel D`x' = ("`rho'")
		
		}
		
	local x = 11
		
	foreach var in egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr spiteful `var' if gender==0, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel D`x' = ("`rho'")
		
		}
		
	local x = 4
	
	// egalitarian
	
	foreach var in altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr egalitarian `var' if gender==1, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel E`x' = ("`rho'")
		
		}
		
	local x = 12
		
	foreach var in altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr egalitarian `var' if gender==0, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel E`x' = ("`rho'")
		
		}
		
	local x = 5
	
	// altruistic
	
	foreach var in selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr altruistic `var' if gender==1, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel F`x' = ("`rho'")
		
		}
		
	local x = 13
		
	foreach var in selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr altruistic `var' if gender==0, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel F`x' = ("`rho'")
		
		}
		
	local x = 6
	
	// selfish
	
	foreach var in unclassified {
		
		local x = `x' + 1
		
		pwcorr selfish `var' if gender==1, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel G`x' = ("`rho'")
		
		}
		
	local x = 14
		
	foreach var in unclassified {
		
		local x = `x' + 1
		
		pwcorr selfish `var' if gender==0, sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel G`x' = ("`rho'")
		
		}
	
	//Children
	
	use "$constructdata/children.dta", clear 
	
	keep slno mid age_at_exp gender patient_choices binswanger spiteful egalitarian altruistic selfish 
	gen 	unclassified=.
	replace unclassified=0 if altruistic==1
	replace unclassified=0 if egalitarian==1
	replace unclassified=0 if selfish==1
	replace unclassified=0 if spiteful==1
	replace unclassified=1 if unclassified==.

	loc children patient_choices binswanger spiteful egalitarian altruistic selfish unclassified
	
	//Patient choices
	
	local x = 17
	
	foreach var in binswanger spiteful egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr patient_choices `var', sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel B`x' = ("`rho'")
		
		}
	
	local x = 18
	
	// binswanger
	
	foreach var in spiteful egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr binswanger `var', sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel C`x' = ("`rho'")
		
		}
		
	local x = 19
	
	// spiteful
	
	foreach var in egalitarian altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr spiteful `var', sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel D`x' = ("`rho'")
		
		}
		
	local x = 20
	
	// egalitarian
	
	foreach var in altruistic selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr egalitarian `var', sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel E`x' = ("`rho'")
		
		}
		
	local x = 21
	
	// altruistic
	
	foreach var in selfish unclassified {
		
		local x = `x' + 1
		
		pwcorr altruistic `var', sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel F`x' = ("`rho'")
		
		}
		
	local x = 22
	
	// selfish
	
	foreach var in unclassified {
		
		local x = `x' + 1
		
		pwcorr selfish `var', sig 
	
		local first = `r(rho)'*(sqrt(`r(N)'-2))/(sqrt(1-(`r(rho)'^2)))
		local sig = 2*ttail(`r(N)'-2, abs(`first'))
		local rho = string(`r(rho)',"%9.3fc")
		
		if `sig' < 0.01 {
			local rho = "`rho'***"
			}
			
		if `sig' < 0.05 & `sig' >= 0.01 {
			local rho = "`rho'**"
			}
			
		if `sig' < 0.1 & `sig' >= 0.05 {
			local rho = "`rho'*"
			}

		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 14") modify
		putexcel G`x' = ("`rho'")
		
		}

	capture erase "$constructdata\temporary\siblings1.dta"
	capture erase "$constructdata\temporary\siblings2.dta"
	capture erase "$constructdata\temporary\father.dta"
	capture erase "$constructdata\temporary\mother.dta"
	
*/
*******************************************************************************************************************
*** Table 15: Summary of characteristics represented in two clusters resulting from partitioning around medoids ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	

		use "$constructdata/children.dta", clear 	
	local patience "patient_choices"
	local binswanger "binswanger"
	local spitefulness "spiteful"
	local egalitarian "egalitarian"
	local altruism "altruistic"
	local selfishness "selfish"
	local variables "`patience' `binswanger' `spitefulness' `egalitarian' `altruism' `selfishness'"
	di "`variables'"

	foreach variable of local variables {
		by slno, sort: egen `variable'OffspringPoints = count(`variable')
		by slno, sort: egen `variable'OffspringMean = total(`variable')
		replace `variable'OffspringMean = (`variable'OffspringMean)/(`variable'OffspringPoints)
		drop `variable'OffspringPoints
	}

	by slno: keep if _n == 1
	
	saveold "$constructdata/children_familyAggregate_stat12.dta", v(12) replace
	
	// Merge files
	use "$constructdata/children_familyAggregate_stat12.dta", clear
	
	merge 1:1 slno using "$constructdata/cluster_pam_full.dta"
	drop _merge
	
	tempfile familyAggregate_stat_clustered
	sa "`familyAggregate_stat_clustered'"

	//Table 13
	
	* Generating new variables
	
	gen unclassified_child = 1
	gen unclassified_father = 1
	gen unclassified_mother = 1
	
	foreach var of local variables {
	
		replace unclassified_child = 0 if `var' == 1
		replace unclassified_father = 0 if `var'_father == 1
		replace unclassified_mother = 0 if `var'_mother == 1
		
		}
	
	* Setting local

	local clusterVariables "pam_full"
	 
	local patience "patient_choicesOffspringMean patient_choices_father patient_choices_mother"
	local binswanger "binswangerOffspringMean binswanger_father binswanger_mother"
	local spitefulness "spitefulOffspringMean spiteful_father spiteful_mother"
	local egalitarian "egalitarianOffspringMean egalitarian_father egalitarian_mother"
	local altruism "altruisticOffspringMean altruistic_father altruistic_mother"
	local selfishness "selfishOffspringMean selfish_father selfish_mother"
	local unclassified "unclassified_child unclassified_father unclassified_mother"
	local variables "`patience' `binswanger' `spitefulness' `egalitarian' `altruism' `selfishness' `unclassified'"
	
	local x = 1
	
	foreach var of local variables {
		
		local x = `x' + 1
		
		ttest `var', by(pam_full)
		
		local mu1 = string(`r(mu_1)',"%9.2fc")
		local mu2 = string(`r(mu_2)',"%9.2fc")
		local diff = `r(mu_1)' - `r(mu_2)'
		local diff = string(`diff',"%9.2fc")
		local pval = string(`r(p)',"%9.2fc")
		
		local n1 = string(`r(N_1)',"%9.0fc")
		local n2 = string(`r(N_2)',"%9.0fc")
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 15") modify
		putexcel B`x' = (`mu1')
		putexcel C`x' = (`mu2')
		putexcel D`x' = (`diff')
		putexcel E`x' = (`pval')
		
		}
		
	putexcel B23 = (`n1')
	putexcel C23 = (`n2')
	
	
	
*******************************************************************************************************************
*** Table 16: Determinants of families belonging to Cluster 2  ***
*** The results estimated below requires putting the column and row headings manually in a pre-filled Excel file***
*******************************************************************************************************************	

use "`familyAggregate_stat_clustered'", clear

* Preparing the data

gen pam_full_reg = pam_full - 1
gen inc_per_cap = inc_per_cap_per_month_2016 / 1000

//regression analysis
loc outregoptions "excel lab dec(3) nocons nonotes"
	loc notes "Standard errors in parentheses are clustered at household level. *** p<0.01, ** p<0.05, * p<0.1."
	loc text "District Fixed Effects are included?"
	loc ap "replace"
//cluster
	loc unit slno //clustering unit, we are using households
	loc f_effect "slno"
	
***************************************************
* Determinants of families belonging to Cluster 2 *	
***************************************************
	
xi: dprobit pam_full_reg inc_per_cap hhsize age_father age_mother schooling_father schooling_mother FSIQ_std_father FSIQ_std_mother, robust cl(`unit')  

	local n = `e(N)'
	local r2 = string(`e(r2_p)',"%9.3fc")
	matrix temp_b = e(dfdx)
	matrix temp_se = e(se_dfdx)
	
	local x = 0
	local y = 1
	
	foreach var in inc_per_cap hhsize age_father age_mother schooling_father schooling_mother FSIQ_std_father FSIQ_std_mother { 
	
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
		
		local se_`x' = string(`se_`x'',"%9.3fc")
		
		putexcel set "$tables/Tables Main Analysis.xlsx", sheet("Table 16") modify
		local y = `y' + 1
		putexcel B`y' = ("`b_`x''")
	
		putexcel C`y' = ("`se_`x''")
		
		}
		
		putexcel B10 = ("`n'")
		
		local ll = string(`e(ll)',"%9.2fc")
		local ll = "Log likelihood = " + "`ll'"
		
		putexcel A12 = ("`ll'")
		
		local r2p = string(`e(r2_p)',"%9.3fc")
		local r2p = "Pseudo R² = " + "`r2p'"
		
		putexcel A11 = ("`r2p'")
	