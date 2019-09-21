
*libname statement;
libname log "C:\Users\bjsul\Documents\GitHub\MSA\Fall_1_Team_Work\LogisticPhase2";

*create new factor levels for missing values;
data work.insurance_t;
	set log.insurance_t_bin;
	if cc = . then cc = 2;
	if ccpurc = . then ccpurc = 5;
	if inv = . then inv = 2;
	if hmown = . then hmown = 2;
run;

data work.insurance_v;
	set log.insurance_v_bin;
	if cc = . then cc = 2;
	if ccpurc = . then ccpurc = 5;
	if inv = . then inv = 2;
	if hmown = . then hmown = 2;
run;

*cross-freq tables for all variables with INS;
proc freq data = work.insurance_t;
	tables ins*(_all_);
run;

*collapse bins on variables with seperation concerns;
data work.insurance_t;
	set work.insurance_t;
	if cashbk = 2 then cashbk = 1;
	if mmcred = 5 then mmcred = 3;
run;

*cross freq table for INS, cashbk and mmcred;
proc freq data = work.insurance_t;
	tables ins*(cashbk mmcred);
run;

*check correleation of mmbal_bin and mm;
proc corr data = work.insurance_t;
	var mmbal_bin mm;
run;

*get all var names;
ods trace on;
proc contents data = work.insurance_t short;
	ods output variablesshort = varnames;
run;
ods trace off;

/*
data work.test;
	set work.varnames;
	variablesmin = substr(variables, find(variables, "INS"), 3);
	call symputx("varns", variables);
run;
*/

*logistic regression on all vars.;
%let allvars = ACCTAGE_Bin AGE_Bin ATM ATMAMT_Bin BRANCH CASHBK CC CCBAL_Bin CCPURC CD CDBAL_Bin CHECKS_Bin CRSCORE_Bin DDA DDABAL_Bin DEPAMT_Bin DIRDEP HMOWN HMVAL_Bin ILS ILSBAL_Bin INAREA INCOME_Bin INV INVBAL_Bin IRA IRABAL_Bin LOC LOCBAL_Bin LORES_Bin MM MMBAL_Bin MMCRED MOVED MTG MTGBAL_Bin NSF NSFAMT_Bin PHONE_Bin POSAMT_Bin POS_Bin RES SAV SAVBAL_Bin SDB TELLER_Bin;
proc logistic data = work.insurance_t;
	class _all_;
	model ins = &allvars
		/ selection = backward slstay = 0.002 clodds = pl clparm=pl;
	ods output clparmpl = work.oddsratios;
	ods output modelanova = work.type3main;
run;

%let maineffects = ATMAMT_Bin BRANCH CC CDBal_Bin Checks_bin DDA DDABAL_BIN ILS INV IRA MM NSF SAVBAL_BIN TELLER_BIN;

*export p-values for main effects only model;
proc sort data = work.type3main out=work.type3mainsort;
	by probchisq;
run;

proc export data=work.type3mainsort
	outfile="C:\Users\bjsul\Documents\GitHub\MSA\Fall_1_Team_Work\LogisticPhase2\p-vaules_maineffects.csv" replace;
run;

*interactions effects model;
%let interactions = ATMAMT_Bin|BRANCH|CC|CDBal_Bin|Checks_bin|DDA|DDABAL_BIN|ILS|INV|IRA|MM|NSF|SAVBAL_BIN|TELLER_BIN;
ods trace on;
proc logistic data = work.insurance_t plots = ROC outmodel = log.intmodel;
	class _all_;
	model ins (event ='1')= &maineffects &interactions @2
		/ selection = forward slentry = 0.002 clodds=pl clparm=pl
			ctable pprob = 0  to 0.98 by 0.02;
	score data = work.insurance_v out=work.scores fitstat outroc=roc;
	ods output classification = classtable;
	ods output parameterestimates = work.oddsratiosint;
	ods output modelANOVA = work.Type3;
	output out = predprobs p = phat;
run;

proc sort data = predprobs;
	by descending INS;
run;

*coefficient of discrimination;
proc ttest data = predprobs order=data;
	ods select statistics summarypanel;
	class INS;
	var phat;
	title 'Coefficient of Discrimination and plots';
	ods output summarypanel = Summarypanel;
run;

ods trace off;

*ks statistic;
proc npar1way data=predprobs d plot=edfplot;
	class INS;
	var phat; 
run;

*scoring on validation data / creating scored;
data work.ctable;
	set work.scores;
	do i = 0 to 1 by .001;
		if P_1 > i then predevent = 1;
			else predevent = 0;
		if predevent = 1 and INS = 1 then TP = 1;
			else TP = 0;
		if predevent = 1 and INS = 0 then FP = 1;
			else FP = 0;
		if predevent = 0 and INS = 0 then TN = 1;
			else TN = 0;
		if predevent = 0 and INS = 1 then FN = 1;
			else FN = 0;
		output;
	end;
run;

*create confusion dataset;
proc means data=ctable mean;
	class i;
	vars predevent TP FP TN FN;
	output out=work.confusion;
run;

proc sort data= ctable;
	by i;
run;

/*
proc freq data=ctable;
	table tp fp tn fn;
	by i;
run;
*/

proc means data=log.insurance_t_bin;
	var ins;
run;

*calculate goodness of fit statistics;
data accuracies;
	set work.confusion;
		where _stat_ = 'MEAN';
	accuracy = ((TP + TN)/(TP + TN + FP + FN));
	sensitivity=((TP)/(TP + FN));
	specificity=((TN)/(TN + FP));
	J = sensitivity + specificity -1;
	Lift = sensitivity / 0.3434962 ;
	depth = TP + FP;
run;	

proc sgplot data=accuracies;
	series x=depth y=Lift;
run;

*calculate goodness of fit stats for each threshold level: i;
proc means data = accuracies mean;
	class i;
	vars accuracy predevent TP TN FP FN sensitivity specificity J;
run;

*Lift Chart;
data work.roc;
	set work.roc;
	cutoff = _PROB_;
	specif = 1 - _1MSPEC_;
	depth = (_POS_+_FALPOS_)/8495*100;
	precision=_POS_/(_POS_+_FALPOS_);
	acc=_POS_+_NEG_;
	lift=precision/0.3434962;
run; 

proc sgplot data=roc;
	series x=depth y=lift;
run;

*sort by p-vaule and export odds ratios for main effects + interaction effects;
proc sort data=oddsratiosint out=oddsratiosintsort;
	by probchisq;
run;

proc export data=oddsratiosintsort
	outfile="C:\Users\bjsul\Documents\GitHub\MSA\Fall_1_Team_Work\LogisticPhase2\oddsratiosint.csv" replace;
run;

*sort type3 by p-value then export for main effects + interaction effects;
proc sort data = type3 out=type3sort;
	by probchisq;
run;

proc export data=type3sort
	outfile="C:\Users\bjsul\Documents\GitHub\MSA\Fall_1_Team_Work\LogisticPhase2\type3effects-interactions.csv" replace;
run;	

*hard coded interaction variables;
data work.insurance_t;
	set work.insurance_t;
	ddairaint = dda*ira;
	mmddabalbinint = mm*ddabal_bin;
	ddabalbinsavbalbinint = DDABAL_Bin*SAVBAL_Bin;
run;

*two way frequency table for interaction effects;
proc freq data = work.insurance_t;
	tables ins*(ddairaint mmddabalbinint ddabalbinsavbalbinint);
run;


quit;
