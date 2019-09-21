proc logistic inmodel=log.intmodel;
	score data=work.insurance_v fitstat outroc=roc out=scores;
run;

proc sort data = predprobs;
	by descending INS;
run;

*scoring on validation data / creating scored;
data work.ctable;
	set work.scores;
	do i = 0 to 1 by .01;
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

proc means data=ctable mean;
	class i;
	vars predevent TP FP TN FN;
	output out=work.confusion;
run;

proc means data=log.insurance_t_bin;
	var ins;
run;

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
