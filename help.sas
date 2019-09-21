/*proc logistic data = work.insurance_t plots = ROC outmodel = log.intmodel;
	class _all_;
	model ins = &maineffects &interactions @2
		/ selection = forward slentry = 0.002 clodds=pl clparm=pl;
	score data = log.insurance_v_bin fitstat outroc=roc;
run;
*/

proc logistic inmodel=log.intmodel;
	score data=work.insurance_v fitstat outroc=roc;
run;

data work.roc;
	set work.roc;
	cutoff = _PROB_;
	specif = 1 - _1MSPEC_;
	depth = (_POS_+_FALPOS_)/2124*100;
	precision=_POS_/(_POS_+_FALPOS_);
	acc=_POS_+_NEG_;
	lift=precision/(0.3434962);
run; 

proc sgplot data=work.roc; 
	*where 0.005 <= depth <= 0.50; 
	series y=lift x=depth; 
	refline 1.0 / axis=y; 
	title1 "Lift Chart for Validation Data"; 
	xaxis label="Depth (%)";
	yaxis label="Lift";
run; 
