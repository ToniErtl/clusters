		*The association of cognitive skills and economic preferences
		
		*2022. 02. 19
		************************************************************************************************************

	use "d:\Nem-kognitiv Dropbox\Projects\AGREEE\Kísérletek\összesített_eredmények\Horn-Kiss-Lenard2021_mindenvaltozo.dta" , clear

	*global JEBO "d:\Nem-kognitiv Dropbox\Daniel Horn\Apps\Overleaf\Gender Differences in Economic Preferences of Adolescents (prof. account)\"
	global OUT "d:\Nem-kognitiv Dropbox\Projects\AGREEE\Kísérletek\összesített_eredmények\Correlations\"
	global TEX "d:\Nem-kognitiv Dropbox\Daniel Horn\Apps\Overleaf\Correlations of economic preferences\"
	******************************************
	*Variable transformation
	******************************************

	*Time preferences
	*Delta-Beta
	gen delta=1000/timelater_ft
	gen beta=1000/(delta*timenow_ft)
	gen delta_now=1000/timenow_ft

	label var delta "Patience"
	label var delta_now "Patience, now"
	label var beta "Discount factor"

	gen fb=1 if beta<1
		replace fb=0 if beta>=1
		replace fb=. if beta==.
	label var fb "future-biased"	
	gen pb=1 if beta>=1
		replace pb=0 if beta<1
		replace pb=. if beta==.
	label var pb "present-biased"

		label var delta "Delta"
		label var delta_now "Patience 'now'"
		label var beta "Beta"
		cap drop pb
		gen pb=1 if beta<1
			replace pb=0 if beta>=1 & beta~=.
		label var pb "Present biased"
		gen beta_0=abs(beta-1)
			label var beta_0 "time inconsistency"


	*Time spent on tasks
	egen time_delta_now=rsum(TimeOK11TimeStage0OK_0 TimeOK12TimeFirstSta_0 TimeOK13TimeSecondSt_0 TimeOK14TimeThirdSta_0 TimeOK15TimeFourthSt_0 TimeOK16TimeFifthSta_0 TimeOK17TimeSixthSta_0)
	egen time_delta=rsum(TimeOK113Time1monthS_0 TimeOK114Time1monthF_0 TimeOK115Time1monthS_0 TimeOK116Time1monthT_0 TimeOK117Time1monthF_0 TimeOK118Time1monthF_0 TimeOK119Time1monthS_0)
	egen time_beta=rsum(TimeOK11TimeStage0OK_0 TimeOK12TimeFirstSta_0 TimeOK13TimeSecondSt_0 TimeOK14TimeThirdSta_0 TimeOK15TimeFourthSt_0 TimeOK16TimeFifthSta_0 TimeOK17TimeSixthSta_0 TimeOK113Time1monthS_0 TimeOK114Time1monthF_0 TimeOK115Time1monthS_0 TimeOK116Time1monthT_0 TimeOK117Time1monthF_0 TimeOK118Time1monthF_0 TimeOK119Time1monthS_0)
	egen time_risk=rsum(TimeOK110RiskStage0O_0 TimeOK111RiskFirstSt_0)
	egen time_dictator=rsum(TimeOK18DictatorGame_0)
	egen time_dictator_schoolmate=rsum(TimeOK19DictatorGame_0)
	egen time_trust=rsum(TimeOK120aTrustGameS_0 TimeOK120bTrustGameS_0)
	egen time_trust_return=rsum(TimeOK120aTrustGameS_0 TimeOK121TrustGameSt_0)
	egen time_publicgood=rsum(TimeOK112aPublicGood_0 TimeOK112bPublicGood_0)
	egen time_comp=rsum(TimeTOVBB122Competit_0 TimeTovbb123Competit_0 TimeTovbb126Competit_0 TimeSChoose129Compet_0)

		label var time_delta_now "Time spent on Task 1 (time now vs 2 weeks)"
		label var time_delta "Time spent on Task 6 (time 4 vs 6 weeks)"
		label var time_beta "Time spent on Task 1 and 6 (both time tasks)"
		label var time_risk "Time spent on 'risk''"
		label var time_dictator "Time spent on 'altruism (classmate)'"
		label var time_dictator_schoolmate "Time spent on 'altruism (schoolmate)'"
		label var time_trust "Time spent on 'trust''"
		label var time_trust_return "Time spent on 'trust-return''"
		label var time_publicgood "Time spent on 'cooperation''"
		label var time_comp "Time spent on 'competition''"
		
		
		gen time_beta_0=time_beta
			label var time_beta_0 "Time spent on 'beta''"
		gen time_pb=time_beta
			label var time_pb "Time spent on 'beta''"
		gen time_dictator_difference=time_dictator+time_dictator_schoolmate
			label var time_dictator_difference " Time spent on 'altruism (c.m-s.m.)'"
		
		

	*standardized preference measures	
	foreach var in delta beta comp dictator dictator_schoolmate trust publicgood risk trust_return {
			
		egen `var'_s=std(`var')

		}

		
	******************************************
	*Raw correlations
	*****************************************
	*lásd még a d:\Nem-kognitiv Dropbox\Projects\AGREEE\Kísérletek\összesített_eredmények\JEBO\2_regression.do filet. Ott van egy jó kis szekció a leírókról.

pwcorr delta beta risk dictator trust trust_return publicgood comp, sig
 
	eststo clear

estpost corr delta risk dictator trust trust_return publicgood comp, matrix
est store corr
esttab corr using "${TEX}\corstat.tex", replace label nonumber f noobs nogaps compress wide unstack


 **********************************************
*conditional correlations
*********************************************

global FAMILY chsup_d0 chsup_d2 fjob2-fjob9 pared_d0 pared_d2 pared_d3 books2-books8
global COGN math read
global GPA gpa_i gpa_m grade_math grade_hun grade_lit grade_math_m grade_hun_m grade_lit_m
global AGE age	


foreach dvar of varlist delta beta risk dictator trust trust_return publicgood comp {
	areg `dvar'  female  $AGE $FAMILY $COGN, a(classid) cluster(classid)
		predict `dvar'_res, res
	}
	
label var delta_res "Delta"
label var beta_res "Beta"
label var risk_res "Risk"
label var dictator_res "Altruism"
label var trust_res "Trust"
label var trust_return_res "Trust-returrn"
label var publicgood_res "Cooperation"
label var comp_res "Competition"
pwcorr delta_res risk_res dictator_res trust_res trust_return_res publicgood_res comp_res, sig
 
	eststo clear

estpost corr delta_res risk_res dictator_res trust_res trust_return_res publicgood_res comp_res, matrix
est store corr_res
esttab corr_res using "${TEX}\corstat_res.tex", replace label nonumber f noobs nogaps compress wide unstack
