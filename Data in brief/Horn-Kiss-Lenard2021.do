	*The association of gender and economic preferences
	
	*2022.02.08.
	************************************************************************************************************

*use "...\Horn-Kiss-Lenard2021.dta" , clear

*global JEBO "...directory..."

	
******************************************
*Descriptives	
*****************************************

*asdoc sum female math  read gpa SES grade FinalProfitRounded delta risk comp dictator dictator_schoolmate trust publicgood, detail label save(sumstat.doc) replace
*sutex2 female math  read gpa SES grade FinalProfitRounded delta risk comp dictator dictator_schoolmate trust publicgood, minmax digits(2) varlab saving(descstat.tex) replace
*asdoc cor female math  read gpa SES grade FinalProfitRounded delta risk comp dictator dictator_schoolmate trust publicgood, label save(corstat.doc) 
*corrtex delta beta risk dictator trust trust_return publicgood comp, file(corstat.tex) replace sig dig(2)

eststo clear

estpost corr delta beta risk dictator trust trust_return publicgood comp, matrix
est store corr
esttab corr using "${JEBO}\corstat.tex", replace label nonumber f noobs nogaps compress wide long unstack


 *Descriptive tables
	*NABC
estpost sum age ///
	pared_d1 pared_d2 pared_d3 pared_d0 fjob1-fjob9 chsup_d1 chsup_d2 chsup_d0 books1-books8 ///
	math read  ///
	gpa_i gpa_m grade_math grade_hun grade_lit grade_math_m grade_hun_m grade_lit_m ///
	if female==0
est store male

estpost sum age ///
	pared_d1 pared_d2 pared_d3 pared_d0 fjob1-fjob9 chsup_d1 chsup_d2 chsup_d0 books1-books8 ///
	math  read  ///
	gpa_i gpa_m grade_math grade_hun grade_lit grade_math_m grade_hun_m grade_lit_m ///
	if female==1
est store female


estpost ttest age ///
	pared_d1 pared_d2 pared_d3 pared_d0 fjob1-fjob9 chsup_d1 chsup_d2 chsup_d0 books1-books8 ///
	math  read  ///
	gpa_i gpa_m grade_math grade_hun grade_lit grade_math_m grade_hun_m grade_lit_m, ///
	by(male)
est store ttest

 
esttab female male using "${JEBO}\descstat_nabc.tex", replace unstack ///
mtitles("\textbf{\emph{Female}}" "\textbf{\emph{Male}}") ///
refcat(age "\textbf{\emph{NABC data}}" pared_d1 "\textit{Family}" math  "\textit{Cognitive skills}"  gpa_i "\textit{Grades}", nolabel) ///
collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Std.Dev.}} \multicolumn{1}{l}{{Obs}} \multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Std.Dev.}} \multicolumn{1}{l}{{Obs}}  \multicolumn{1}{c}{{Diff.}} \multicolumn{1}{c}{{t-stat}}) /// 
cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") label nonumber f noobs alignment(S) booktabs	 compress wide long


esttab ttest using "${JEBO}\descstat_t_nabc.tex", replace unstack  ///
mtitles("\textbf{\emph{T-test}}") ///
refcat(age "\textbf{\emph{NABC data}}" pared_d1 "\textit{Family}" math  "\textit{Cognitive skills}"  gpa_i "\textit{Grades}" , nolabel) ///
collabels(\multicolumn{1}{c}{{Diff.}} \multicolumn{1}{c}{{t-stat}}) ///
 label nonumber f noobs alignment(S) booktabs compress wide long	


	*Experimenmtal tasks
estpost sum ///
	FinalProfitRounded delta beta risk dictator dictator_schoolmate trust trust_return publicgood comp ///
	time_total time_delta_now time_delta time_beta time_risk time_dictator time_dictator_schoolmate time_trust time_trust_return time_publicgood time_comp ///
	if female==0
est store male

estpost sum ///
	FinalProfitRounded delta beta risk dictator dictator_schoolmate trust trust_return publicgood comp ///
	time_total time_delta_now time_delta time_beta time_risk time_dictator time_dictator_schoolmate time_trust time_trust_return time_publicgood time_comp ///
	if female==1
est store female


estpost ttest ///
	FinalProfitRounded delta beta risk dictator dictator_schoolmate trust trust_return publicgood comp ///
	time_total time_delta_now time_delta time_beta time_risk time_dictator time_dictator_schoolmate time_trust time_trust_return time_publicgood time_comp, ///
	by(male)
est store ttest

esttab female male using "${JEBO}\descstat_exp.tex", replace unstack ///
mtitles("\textbf{\emph{Female}}" "\textbf{\emph{Male}}") ///
refcat(FinalProfitRounded "\textbf{\emph{Experiments}}" time_total "\textbf{\emph{Time}}", nolabel) ///
collabels(\multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Std.Dev.}} \multicolumn{1}{l}{{Obs}} \multicolumn{1}{c}{{Mean}} \multicolumn{1}{c}{{Std.Dev.}} \multicolumn{1}{l}{{Obs}}  \multicolumn{1}{c}{{Diff.}} \multicolumn{1}{c}{{t-stat}}) /// 
cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") label nonumber f noobs alignment(S) booktabs	 compress wide long


esttab ttest using "${JEBO}\descstat_t_exp.tex", replace unstack  ///
mtitles("\textbf{\emph{T-test}}") ///
refcat(FinalProfitRounded "\textbf{\emph{Experiments}}" time_total "\textbf{\emph{Time}}", nolabel) ///
collabels(\multicolumn{1}{c}{{Diff.}} \multicolumn{1}{c}{{t-stat}}) ///
 label nonumber f noobs alignment(S) booktabs compress wide long	

 
**********************************************
*All controls with class FE
*********************************************
foreach dvar of varlist pared_d1 pared_d2 pared_d3 pared_d0 fjob1-fjob9 chsup_d1 chsup_d2 chsup_d0 books1-books8 ///	
						math  read gpa_i gpa_m grade_math grade_hun grade_lit grade_math_m grade_hun_m grade_lit_m  FinalProfitRounded  ///
						time_total time_delta time_delta_now time_risk time_dictator time_trust time_trust_return time_publicgood time_comp {
	areg `dvar'  female, a(classid) cluster(classid)
		estimates store `dvar'
		outreg2 using "${JEBO}\decstat_reg.doc", append tex(frag pr) label 
	}
 

coefplot (pared_d1, label(pared: low)) (pared_d2, label(pared: med)) (pared_d3, label(pared: high)) (pared_d0, label(pared: missing)) , drop(_cons) yline(0) vert scheme(s1mono) levels(95) ytitle("within class gender difference") 
		graph export "${JEBO}\pared_female.eps",  as(eps) replace 

coefplot (fjob1, label(f_job: empl.)) (fjob2, label(f_job: self-empl.)) (fjob3, label(f_job: reg. work)) (fjob4, label(f_job: occ. work)) (fjob5, label(f_job: chilc.)) (fjob6, label(f_job: ret.)) (fjob7, label(f_job: unempl.)) (fjob8, label(f_job: disabl.)) (fjob9, label(f_job: miss.))  , drop(_cons) yline(0) vert scheme(s1mono) levels(95) ytitle("within class gender difference") 
		graph export "${JEBO}\fjob_female.eps",  as(eps) replace 

coefplot (books1, label(books: 0-50)) (books2, label(books: cca. 50)) (books3, label(books: max. 150)) (books4, label(books: max 300)) (books5, label(books: 300-600)) (books6, label(books: 600-1000)) (books7, label(books: over 1000)) (books8, label(books: missing)) , drop(_cons) yline(0) vert scheme(s1mono) levels(95) ytitle("within class gender difference") 
		graph export "${JEBO}\books_female.eps",  as(eps) replace 

		
coefplot (math, label(Math z-score)) (read, label(Read z-score)) (gpa_i, label(GPA)) (grade_math, label(Math grade)) (grade_hun, label(Hun grade)) (grade_lit, label(Lit. grade))  , drop(_cons) yline(0) vert scheme(s1mono) levels(95) ytitle("within class gender difference") 
		graph export "${JEBO}\gpa_female.eps",  as(eps) replace 

		
coefplot (time_total, label(Total time)) (time_delta, label("Time task1 (now vs. 2 weeks)")) (time_delta_now, label("Time task6 (4 vs. 6 weeks)")) (time_risk, label(Time 'risk')) (time_dictator, label(Time 'altruism')) (time_trust, label(Time 'trust'')) (time_trust_return, label(Time 'trust-return')) (time_publicgood, label(Time 'cooperation')) (time_comp, label(Time 'competition')) , drop(_cons) yline(0) vert scheme(s1mono) levels(95) ytitle("within class gender difference") 
		graph export "${JEBO}\time_female.eps",  as(eps) replace 
		
		

**********************************************
*regressions
*********************************************

global FAMILY chsup_d0 chsup_d2 fjob2-fjob9 pared_d0 pared_d2 pared_d3 books2-books8
global COGN math read
global GPA gpa_i gpa_m grade_math grade_hun grade_lit grade_math_m grade_hun_m grade_lit_m
global AGE age	


**********************************************
*All preferences - no controls
*********************************************
foreach var in delta beta comp dictator dictator_schoolmate trust publicgood risk trust_return {
		
	egen `var'_s=std(`var')

	}


foreach dvar of varlist delta beta comp dictator trust publicgood risk trust_return {
	reg `dvar'_s  female , 
	estimates store `dvar'
		*outreg2 using "${JEBO}\`dvar'_none.doc", replace  tex(frag pr land) label keep(female)
}
coefplot (delta, label("Delta")) (beta, label(Beta)) (risk, label(Risk)) (dictator, label(Altruism)) (trust, label(Trust)) (trust_return, label("Trust-return")) 	(publicgood, label(Cooperation))  (comp, label(Competition)), keep(female) yline(0) vert scheme(s1mono) levels(95) ytitle("standardized beta coefficients")
	graph save "${JEBO}\prefs_none.gph", replace
	graph export "${JEBO}\prefs_none.eps", as(eps) replace 

									 

**********************************************
*DELTA -beta
*********************************************
*Graphs on raw values
twoway (hist delta if female==0,frac lcolor(gs12) fcolor(gs12) ) (hist delta if female==1, frac fcolor(none) lcolor(gs3) ) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Delta)*/ xtitle(" ")
 	graph export "${JEBO}\delta_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\delta_distribution.emf", as(emf) replace 
	
twoway (hist beta if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist beta if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Beta)*/ xtitle(" ")
 	graph export "${JEBO}\beta_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\beta_distribution.emf", as(emf) replace 

		

global PREF comp dictator trust publicgood risk 
foreach dvar of varlist delta   beta beta_0 pb {
	reg `dvar'  female, cluster(classid)
		estimates store `dvar'_0
		outreg2 using "${JEBO}\`dvar'_all.doc", replace tex(frag pr) label 
	areg `dvar'  female, a(classid) cluster(classid)
		estimates store `dvar'_1
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr) label 
	areg `dvar'  female $AGE, a(classid) cluster(classid)
		estimates store `dvar'_2
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr) label 
	areg `dvar'  female $AGE $FAMILY, a(classid) cluster(classid)
		estimates store `dvar'_3
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr) label 
	areg `dvar'  female $AGE $FAMILY $COGN, a(classid) cluster(classid)
		estimates store `dvar'_4
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr) label 
	areg `dvar'  female $AGE $FAMILY $COGN $GPA, a(classid) cluster(classid)
		estimates store `dvar'_5
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr) label 
	areg `dvar'  female  $AGE $FAMILY $COGN $GPA time_`dvar' time_total, a(classid) cluster(classid)
		estimates store `dvar'_6
	areg `dvar'  female  $AGE $FAMILY $COGN $GPA $PREF time_`dvar'  time_total, a(classid) cluster(classid)
		estimates store `dvar'_7
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr) label 
	}

	
foreach dvar of varlist delta beta beta_0 pb {
esttab `dvar'_0 `dvar'_1 `dvar'_2 `dvar'_3 `dvar'_4 `dvar'_5 `dvar'_6 `dvar'_7 using "${JEBO}\`dvar'_all.tex", replace label nonumber f alignment(S) booktabs width(\hline) nogaps compress ///
mtitles("None" "Class FE" "Age" "Family" "Cogn. skills" "Grades" "Time" "Preferences" ) ///
b(3) not ar2 star(* 0.10 ** 0.05 *** 0.01) 

}

	
	foreach dvar of varlist delta   beta beta_0 pb{
coefplot 	(`dvar'_0, label(none)) (`dvar'_1, label(+class FE)) (`dvar'_2, label(+age)) (`dvar'_3, label(+family)) (`dvar'_4, label(+cogn. skills)) (`dvar'_5, label(+grades)) (`dvar'_6, label(+time)) (`dvar'_7, label(+preferences)), keep(female) yline(0) vert scheme(s1mono) levels(95) 
	graph save "${JEBO}\`dvar'_female.gph", replace
	graph export "${JEBO}\`dvar'_female.eps", as(eps) replace 
	}

	
*sureg
mat SUREG = J(10,4,.)
	mat rownames SUREG = "Patience" "Time_inconsistency" "Present_bias" "Risk-tolerance" "Altruism_(classmate)" "Altruism_(schoolmate)" "Trust" "Trustworthiness" "Cooperation" "Competition"
	mat colnames SUREG = "Unadjusted" "All_Exogenous" "All_and_time" "All_and_time_and_preferences"


	local i=1
foreach dvar of varlist delta beta_0 pb {
	local TIME time_`dvar' time_total

	reg `dvar'  female, 
		estimates store `dvar'_0
	reg `dvar'  female i.classid, 
		estimates store `dvar'_1
	reg `dvar'  female $AGE $FAMILY $COGN $GPA i.classid, 
		estimates store `dvar'_5
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' i.classid, 
		estimates store `dvar'_6
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' $PREF i.classid, 
		estimates store `dvar'_7
suest `dvar'_0 `dvar'_1 `dvar'_5 `dvar'_6 `dvar'_7, cluster(classid)
	test [`dvar'_1_mean]female = [`dvar'_0_mean]female
		mat SUREG[`i',1]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_5_mean]female
		mat SUREG[`i',2]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_6_mean]female
		mat SUREG[`i',3]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_7_mean]female
		mat SUREG[`i',4]=int(r(p)*1000)/1000
	local i=`i'+1
	}



**********************************************
*RISK
*********************************************
*Graphs on raw values
twoway (hist risk if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist risk if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Risk)*/ xtitle(" ")
 	graph export "${JEBO}\risk_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\risk_distribution.emf", as(emf) replace 


global PREF comp dictator trust publicgood delta
foreach dvar of varlist risk {
	reg `dvar'  female , cluster(classid)
		estimates store `dvar'_0
		outreg2 using "${JEBO}\`dvar'_all.doc", replace tex(frag pr land) label keep(female) addtext(additional controls:, none)
	areg `dvar'  female, a(classid) cluster(classid)
		estimates store `dvar'_1
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +class FE)
	areg `dvar'  female $AGE, a(classid) cluster(classid)
		estimates store `dvar'_2
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +age)
	areg `dvar'  female $AGE $FAMILY, a(classid) cluster(classid)
		estimates store `dvar'_3
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +family)
	areg `dvar'  female $AGE $FAMILY $COGN, a(classid) cluster(classid)
		estimates store `dvar'_4
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +cogn. skills)
	areg `dvar'  female $AGE $FAMILY $COGN $GPA, a(classid) cluster(classid)
		estimates store `dvar'_5
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +grades)
	areg `dvar'  female  $AGE $FAMILY $COGN $GPA time_`dvar'  time_total, a(classid) cluster(classid)
		estimates store `dvar'_6
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +preferences)
	areg `dvar'  female  $AGE $FAMILY $COGN $GPA $PREF time_`dvar'  time_total, a(classid) cluster(classid)
		estimates store `dvar'_7
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +time)
	}

	
	foreach dvar of varlist risk {
esttab `dvar'_0 `dvar'_1 `dvar'_2 `dvar'_3 `dvar'_4 `dvar'_5 `dvar'_6 `dvar'_7 using "${JEBO}\`dvar'_all.tex", replace label nonumber f alignment(S) booktabs width(\hline) nogaps compress ///
mtitles("None" "Class FE" "Age" "Family" "Cogn. skills" "Grades" "Time" "Preferences" ) ///
b(3) not ar2 star(* 0.10 ** 0.05 *** 0.01) 
	}
	
	
	*Female figure
	foreach dvar of varlist risk{
coefplot 	(`dvar'_0, label(none)) (`dvar'_1, label(+class FE)) (`dvar'_2, label(+age)) (`dvar'_3, label(+family)) (`dvar'_4, label(+cogn. skills)) (`dvar'_5, label(+grades)) (`dvar'_6, label(+time)) (`dvar'_7, label(+preferences)), keep(female) yline(0) vert scheme(s1mono) levels(95) 
	graph save "${JEBO}\`dvar'_female.gph", replace
	graph export "${JEBO}\`dvar'_female.eps", as(eps) replace 
	}
	
		local i=4
foreach dvar of varlist risk {

	local TIME time_`dvar' time_total

	reg `dvar'  female, 
		estimates store `dvar'_0
	reg `dvar'  female i.classid, 
		estimates store `dvar'_1
	reg `dvar'  female $AGE $FAMILY $COGN $GPA i.classid, 
		estimates store `dvar'_5
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' i.classid, 
		estimates store `dvar'_6
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' $PREF i.classid, 
		estimates store `dvar'_7
suest `dvar'_0 `dvar'_1 `dvar'_5 `dvar'_6 `dvar'_7, cluster(classid)
	test [`dvar'_1_mean]female = [`dvar'_0_mean]female
		mat SUREG[`i',1]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_5_mean]female
		mat SUREG[`i',2]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_6_mean]female
		mat SUREG[`i',3]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_7_mean]female
		mat SUREG[`i',4]=int(r(p)*1000)/1000
	local i=`i'+1
	}

	/*
	
	*Preferences figure
	foreach dvar of varlist risk{
		foreach pref of varlist $PREF {
coefplot  (`dvar'_6, label(+`pref')), keep(`pref') vert yline(0) scheme(s1mono) levels(95) 
	graph save "${JEBO}\`dvar'_`pref'.gph", replace
		}
	}
		local dvar risk
graph combine 	"${JEBO}\`dvar'_comp.gph" "${JEBO}\`dvar'_dictator.gph"  "${JEBO}\`dvar'_trust.gph"  "${JEBO}\`dvar'_publicgood.gph"  "${JEBO}\`dvar'_delta.gph" , scheme(s1mono)
	graph export "${JEBO}\`dvar'_pref.eps", as(eps) replace 
*/
	
	
**********************************************
*Competition
*********************************************


global PREF dictator trust publicgood delta risk
foreach dvar of varlist comp {
	reg `dvar'  female SPiecerate ST_SP, cluster(classid)
		estimates store `dvar'_0
		outreg2 using "${JEBO}\`dvar'_all.doc", replace tex(frag pr land) label keep(female) addtext(additional controls:, none)
	areg `dvar'  female SPiecerate ST_SP, a(classid) cluster(classid)
		estimates store `dvar'_1
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +class FE)
	areg `dvar'  female SPiecerate ST_SP $AGE, a(classid) cluster(classid)
		estimates store `dvar'_2
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +age)
	areg `dvar'  female SPiecerate ST_SP $AGE $FAMILY, a(classid) cluster(classid)
		estimates store `dvar'_3
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +family)
	areg `dvar'  female SPiecerate ST_SP $AGE $FAMILY $COGN, a(classid) cluster(classid)
		estimates store `dvar'_4
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +cogn. skills)
	areg `dvar'  female SPiecerate ST_SP $AGE $FAMILY $COGN $GPA, a(classid) cluster(classid)
		estimates store `dvar'_5
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +grades)
	areg `dvar'  female SPiecerate ST_SP $AGE $FAMILY $COGN $GPA time_`dvar' time_total, a(classid) cluster(classid)
		estimates store `dvar'_6
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +grades)
	areg `dvar'  female  SPiecerate ST_SP $AGE $FAMILY $COGN $GPA time_`dvar' time_total $PREF , a(classid) cluster(classid)
		estimates store `dvar'_7
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +time)
	}

	foreach dvar of varlist comp {
esttab `dvar'_0 `dvar'_1 `dvar'_2 `dvar'_3 `dvar'_4 `dvar'_5 `dvar'_6 `dvar'_7 using "${JEBO}\`dvar'_all.tex", replace label nonumber f alignment(S) booktabs width(\hline) nogaps compress ///
mtitles("None" "Class FE" "Age" "Family" "Cogn. skills" "Grades"  "Time" "Preferences") ///
b(3) not ar2 star(* 0.10 ** 0.05 *** 0.01) 


}
	
	*Female figure
	foreach dvar of varlist comp {
coefplot 	(`dvar'_0, label(none)) (`dvar'_1, label(+class FE)) (`dvar'_2, label(+age)) (`dvar'_3, label(+family)) (`dvar'_4, label(+cogn. skills)) (`dvar'_5, label(+grades)) (`dvar'_6, label(+time)) (`dvar'_7, label(+preferences)), keep(female) yline(0) vert scheme(s1mono) levels(95) 
	graph save "${JEBO}\`dvar'_female.gph", replace
	graph export "${JEBO}\`dvar'_female.eps", as(eps) replace 
	}
	
	
		local i=10
foreach dvar of varlist comp {

	local TIME time_`dvar' time_total

	reg `dvar'  female, 
		estimates store `dvar'_0
	reg `dvar'  female i.classid, 
		estimates store `dvar'_1
	reg `dvar'  female $AGE $FAMILY $COGN $GPA i.classid, 
		estimates store `dvar'_5
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' i.classid, 
		estimates store `dvar'_6
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' $PREF i.classid, 
		estimates store `dvar'_7
suest `dvar'_0 `dvar'_1 `dvar'_5 `dvar'_6 `dvar'_7, cluster(classid)
	test [`dvar'_1_mean]female = [`dvar'_0_mean]female
		mat SUREG[`i',1]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_5_mean]female
		mat SUREG[`i',2]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_6_mean]female
		mat SUREG[`i',3]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_7_mean]female
		mat SUREG[`i',4]=int(r(p)*1000)/1000
	local i=`i'+1
	}

	

**********************************************
*Social preferences 
*********************************************

twoway (hist dictator if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist dictator if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Altruism)*/ xtitle(" ")
 	graph export "${JEBO}\Dictator_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\Dictator_distribution.emf", as(emf) replace 

twoway (hist dictator_schoolmate if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist dictator_schoolmate if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Altruism schoolmate)*/ xtitle(" ")
 	graph export "${JEBO}\Dictator_schoolmate_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\Dictator_schoolmate_distribution.emf", as(emf) replace 

twoway (hist dictator_difference if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist dictator_difference if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Altruism difference)*/ xtitle(" ")
 	graph export "${JEBO}\Dictator_difference_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\Dictator_difference_distribution.emf", as(emf) replace 

twoway (hist trust_return if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist trust_return if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Trust return)*/ xtitle(" ")
 	graph export "${JEBO}\Trust_return_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\Trust_return_distribution.emf", as(emf) replace 
	
twoway (hist trust if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist trust if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Trust)*/ xtitle(" ")
 	graph export "${JEBO}\Trust_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\Trust_distribution.emf", as(emf) replace 
	
twoway (hist publicgood if female==0,frac lcolor(gs12) fcolor(gs12) bin(25)) (hist publicgood if female==1, frac fcolor(none) lcolor(gs3) bin(25)) , scheme(s1mono) legend(lab (1 Male) lab (2 Female)) /*subtitle(Cooperation)*/ xtitle(" ")
 	graph export "${JEBO}\Publicgood_distribution.eps", as(eps) replace 
 	graph export "${JEBO}\Publicgood_distribution.emf", as(emf) replace 

	
global PREF comp risk delta
foreach dvar of varlist dictator dictator_schoolmate dictator_difference trust_return trust publicgood {
	reg `dvar'  female , cluster(classid)
		estimates store `dvar'_0
		outreg2 using "${JEBO}\`dvar'_all.doc", replace tex(frag pr land) label keep(female) addtext(additional controls:, none)
	areg `dvar'  female, a(classid) cluster(classid)
		estimates store `dvar'_1
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +class FE)
	areg `dvar'  female $AGE, a(classid) cluster(classid)
		estimates store `dvar'_2
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +age)
	areg `dvar'  female $AGE $FAMILY, a(classid) cluster(classid)
		estimates store `dvar'_3
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +family)
	areg `dvar'  female $AGE $FAMILY $COGN, a(classid) cluster(classid)
		estimates store `dvar'_4
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +cogn. skills)
	areg `dvar'  female $AGE $FAMILY $COGN $GPA, a(classid) cluster(classid)
		estimates store `dvar'_5
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +grades)
	areg `dvar'  female  $AGE $FAMILY $COGN $GPA time_`dvar'  time_total, a(classid) cluster(classid)
		estimates store `dvar'_6
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +preferences)
	areg `dvar'  female  $AGE $FAMILY $COGN $GPA $PREF time_`dvar'  time_total, a(classid) cluster(classid)
		estimates store `dvar'_7
		outreg2 using "${JEBO}\`dvar'_all.doc", append  tex(frag pr land) label keep(female) addtext(additional controls:, +time)
	}

	foreach dvar of varlist dictator dictator_schoolmate dictator_difference trust_return trust publicgood {
esttab `dvar'_0 `dvar'_1 `dvar'_2 `dvar'_3 `dvar'_4 `dvar'_5 `dvar'_6 `dvar'_7 using "${JEBO}\`dvar'_all.tex", replace label nonumber f alignment(S) booktabs width(\hline) nogaps compress ///
mtitles("None" "Class FE" "Age" "Family" "Cogn. skills" "Grades" "Time" "Preferences" ) ///
b(3) not ar2 star(* 0.10 ** 0.05 *** 0.01) 


}
	
	*Female figure
	foreach dvar of varlist dictator dictator_schoolmate dictator_difference trust_return trust publicgood {
coefplot 	(`dvar'_0, label(none)) (`dvar'_1, label(+class FE)) (`dvar'_2, label(+age)) (`dvar'_3, label(+family)) (`dvar'_4, label(+cogn. skills)) (`dvar'_5, label(+grades)) (`dvar'_6, label(+time)) (`dvar'_7, label(+preferences)), keep(female) yline(0) vert scheme(s1mono) levels(95) 
	graph save "${JEBO}\`dvar'_female.gph", replace
	graph export "${JEBO}\`dvar'_female.eps", as(eps) replace 
	}
	


	
		local i=5
foreach dvar of varlist dictator dictator_schoolmate  trust trust_return publicgood  {

	local TIME time_`dvar' time_total

	reg `dvar'  female, 
		estimates store `dvar'_0
	reg `dvar'  female i.classid, 
		estimates store `dvar'_1
	reg `dvar'  female $AGE $FAMILY $COGN $GPA i.classid, 
		estimates store `dvar'_5
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' i.classid, 
		estimates store `dvar'_6
	reg `dvar'  female  $AGE $FAMILY $COGN $GPA `TIME' $PREF i.classid, 
		estimates store `dvar'_7
suest `dvar'_0 `dvar'_1 `dvar'_5 `dvar'_6 `dvar'_7, cluster(classid)
	test [`dvar'_1_mean]female = [`dvar'_0_mean]female
		mat SUREG[`i',1]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_5_mean]female
		mat SUREG[`i',2]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_6_mean]female
		mat SUREG[`i',3]=int(r(p)*1000)/1000
	test [`dvar'_1_mean]female = [`dvar'_7_mean]female
		mat SUREG[`i',4]=int(r(p)*1000)/1000
	local i=`i'+1
	}

	/*
	*Preferences figure
	foreach dvar of varlist dictator dictator_schoolmate trust_return trust publicgood  {
		foreach pref of varlist $PREF {
coefplot  (`dvar'_6, label(+`pref')), keep(`pref') vert yline(0) scheme(s1mono) levels(95) 
	graph save "${JEBO}\`dvar'_`pref'.gph", replace
		}
	}
		local dvar dictator 
graph combine 	"${JEBO}\`dvar'_comp.gph" "${JEBO}\`dvar'_risk.gph" "${JEBO}\`dvar'_delta.gph" , scheme(s1mono)
	graph export "${JEBO}\`dvar'_pref.eps", as(eps) replace 

		local dvar dictator_schoolmate 
graph combine 	"${JEBO}\`dvar'_comp.gph" "${JEBO}\`dvar'_risk.gph" "${JEBO}\`dvar'_delta.gph" , scheme(s1mono)
	graph export "${JEBO}\`dvar'_pref.eps", as(eps) replace 

		local dvar trust_return 
graph combine 	"${JEBO}\`dvar'_comp.gph" "${JEBO}\`dvar'_risk.gph" "${JEBO}\`dvar'_delta.gph" , scheme(s1mono)
	graph export "${JEBO}\`dvar'_pref.eps", as(eps) replace 

		local dvar trust 
graph combine 	"${JEBO}\`dvar'_comp.gph" "${JEBO}\`dvar'_risk.gph" "${JEBO}\`dvar'_delta.gph" , scheme(s1mono)
	graph export "${JEBO}\`dvar'_pref.eps", as(eps) replace 
	
		local dvar publicgood 
graph combine 	"${JEBO}\`dvar'_comp.gph" "${JEBO}\`dvar'_risk.gph" "${JEBO}\`dvar'_delta.gph" , scheme(s1mono)
	graph export "${JEBO}\`dvar'_pref.eps", as(eps) replace 

	*/
	
outtable using "${JEBO}\sureg", mat(SUREG) replace center caption("Between model differences in female coefficients compared to the 'Class FE' model") 


****************************************************************
******Distribution of dictator vs trust_return vs trust amounts by gender
****************************************************************

********Pooled regressions
global PREF comp risk delta
	areg dictator   $AGE $FAMILY $COGN $GPA $PREF time_dictator time_total, a(classid) cluster(classid)
		predict dictator_xb, xb
		predict dictator_e, res
	areg trust_return $AGE $FAMILY $COGN $GPA $PREF time_trust_return time_total, a(classid) cluster(classid)
		predict trust_return_xb, xb
		predict trust_return_e, res
	areg trust $AGE $FAMILY $COGN $GPA $PREF time_trust time_total, a(classid) cluster(classid)
		predict trust_xb, xb
		predict trust_e, res

*Graphs on residuals
twoway (kdensity dictator_e if female==0) (kdensity trust_e if female==0, lp(dot) lc(black)) (kdensity trust_return_e if female==0, lp(dash)) , scheme(s1mono) legend(lab(1 Altruism) lab(2 Trust) lab(3 Trustworthiness)) subtitle(Male) xtitle(" ")
 	graph save 	"dictator_trust_e_male.gph", replace 
twoway (kdensity dictator_e if female==1) (kdensity trust_e if female==1, lp(dot) lc(black)) (kdensity trust_return_e if female==1, lp(dash)) , scheme(s1mono) legend(lab(1 Altruism) lab(2 Trust) lab(3 Trustworthiness)) subtitle(Female) xtitle(" ")
	graph save 	"dictator_trust_e_female.gph", replace 
	grc1leg "dictator_trust_e_female.gph" "dictator_trust_e_male.gph" , scheme(s1mono) /*subtitle(Residuals)*/
		*graph save 	"${JEBO}\dictator_trust_e.gph", replace 		
		graph export 	"${JEBO}dictator_trust_e.eps", as(eps) replace 		
		
		
*gen dictator_trustr_e=dictator_e-trust_return_e
*gen dictator_trust_e=dictator_e-trust_e

*ksmirnov 	dictator_e=trust_return_e	if male==1, group(classid)

	
	
***************************************************************
*Multiple hypothesis testing
*********************************************************************************

*Time preferences

wyoung, cmd( ///
		"areg delta female $AGE $FAMILY $COGN $GPA comp dictator trust publicgood risk time_delta time_total, a(classid) cluster(classid)" ///
		"areg beta female $AGE $FAMILY $COGN $GPA  comp dictator trust publicgood risk time_beta  time_total, a(classid) cluster(classid)" ///
		"areg pb female $AGE $FAMILY $COGN $GPA comp dictator trust publicgood risk time_pb  time_total, a(classid) cluster(classid)") ///
	familyp(female) bootstraps(1000) seed(12345) cluster(classid)
mat wyoung1a=r(table)
matselrc wyoung1a wyoung1, col(1,3,4,5)

matrix rownames wyoung1 = "Patience (Delta)" "Time inconsistency (Beta)" "Present bias (Beta$<$1)" 
matrix colnames wyoung1 = "full model coeff." "p-value (orig.)" "Westfall-Young" "Bonferroni" 
 
*Risk preferences
 
wyoung, cmd( ///
		"areg risk female $AGE $FAMILY $COGN $GPA comp dictator trust publicgood delta time_risk time_total, a(classid) cluster(classid)") ///
	familyp(female) bootstraps(1000) seed(12345) cluster(classid)
mat wyoung2a=r(table)
matselrc wyoung2a wyoung2, col(1,3,4,5)
		mat wyoung2[1,3]=.
		mat wyoung2[1,4]=.
		
matrix rownames wyoung2 = "Risk tolerance" 
matrix colnames wyoung2 = "full model coeff." "p-value (orig.)" "Westfall-Young" "Bonferroni" 

*Social preferences
 
wyoung, cmd( ///
		"areg dictator female $AGE $FAMILY $COGN $GPA comp risk delta time_dictator  time_total, a(classid) cluster(classid)" ///
		"areg dictator_schoolmate female $AGE $FAMILY $COGN $GPA comp risk delta time_dictator_schoolmate  time_total, a(classid) cluster(classid)" ///
		"areg trust female $AGE $FAMILY $COGN $GPA comp risk delta time_trust  time_total, a(classid) cluster(classid)" ///
		"areg trust_return female $AGE $FAMILY $COGN $GPA comp risk delta time_trust_return  time_total, a(classid) cluster(classid)" ///
		"areg publicgood female $AGE $FAMILY $COGN $GPA comp risk delta time_publicgood  time_total, a(classid) cluster(classid)") ///
	familyp(female) bootstraps(1000) seed(12345) cluster(classid)
mat wyoung3a=r(table)
matselrc wyoung3a wyoung3, col(1,3,4,5)


matrix rownames wyoung3 = "Altruism (classmate)" "Altruism (schoolmate)" "Trust" "Trustworthiness" "Cooperation" 
matrix colnames wyoung3 = "full model coeff." "p-value (orig.)" "Westfall-Young" "Bonferroni" 

*Competitive  preferences
 
wyoung, cmd( ///
		"areg comp female SPiecerate ST_SP $AGE $FAMILY $COGN $GPA dictator trust publicgood delta risk time_comp time_total, a(classid) cluster(classid)") ///		
	familyp(female) bootstraps(1000) seed(12345) cluster(classid)
mat wyoung4a=r(table)
matselrc wyoung4a wyoung4, col(1,3,4,5)
			mat wyoung4[1,3]=.
			mat wyoung4[1,4]=.
			
matrix rownames wyoung4 = "Competition"
matrix colnames wyoung4 = "full model coeff." "p-value (orig.)" "Westfall-Young" "Bonferroni" 
 
mat wyoung = wyoung1\wyoung2\wyoung3\wyoung4
estout matrix(wyoung, fmt(%9.3f)) using "${JEBO}wyoung.tex", style(tex) replace
