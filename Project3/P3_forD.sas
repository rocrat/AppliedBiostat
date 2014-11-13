libname p "C:\Classes\AppliedBiostat\Project3";

data lung;
set p.lung_sas;
run;

proc sort data=lung;
by patid;
run;
/*count participants*/
proc freq data=lung nlevels;
table patid/ noprint;
run;
/*525 patients*/

/*create dataset for each timepoint*/
data t1;
set lung;
if funo eq 1;
run;

data t2;
set lung;
if funo eq 2;
run;
data t3;
set lung;
if funo eq 3;
run;
data t4;
set lung;
if funo eq 4;
run;

proc means data=t1 mean std max min n;
var fact_t2 age_tx cycles ecogps ctc_hem ctc_max ctc_neu;
run;

proc means data=t4 mean std max min n;
var fact_t2 age_tx cycles ecogps ctc_hem ctc_max ctc_neu ctc_oth;
run;

proc freq data =lung;
table sex trtment exptx death crpr qol_comp sx_met sx_cd sx_pri;
run;


proc transpose data=lung out=lung_w prefix=fact;
by patid;
id funo;
var fact_t2;
run;

proc transpose data=lung out=lung_wc prefix=qol_co;
by patid;
id funo;
var qol_comp;
run;
proc transpose data=lung out=lung_wf prefix=func_wb;
by patid;
id funo;
var func_wb2;
run;
proc transpose data=lung out=lung_wp prefix=phys_wb;
by patid;
id funo;
var phys_wb2;
run;
proc transpose data=lung out=lung_ws prefix=socl_wb;
by patid;
id funo;
var socl_wb2;
run;
proc transpose data=lung out=lung_wa prefix=add_crn;
by patid;
id funo;
var add_crn2;
run;
proc transpose data=lung out=lung_we prefix=emot_wb;
by patid;
id funo;
var emot_wb2;
run;

proc sort data=lung_w;
by patid;
run;
proc sort data=lung_wc;
by patid;
run;
proc sort data=lung_wf;
by patid;
run;
proc sort data=lung_wp;
by patid;
run;
proc sort data=lung_ws;
by patid;
run;
proc sort data=lung_wa;
by patid;
run;
proc sort data=lung_we;
by patid;
run;
proc sort data=t1;
by patid;
run;

/*merge wide FACT_T2 et al data with baseline data*/
data p.lung_wide;
merge lung_w (drop= _name_ _label_ ) 
	t1 (drop= funo qol_comp fact_t2) 
	lung_wc (drop = _name_ _label_)
	lung_wf (drop = _name_ _label_)
	lung_wp (drop = _name_ _label_)
	lung_ws (drop = _name_ _label_)
	lung_wa (drop = _name_ _label_)
	lung_we (drop = _name_ _label_);
by patid;
run;
/*save long dataset to permanent directory*/
data p.lung;
set lung;
run;

/****Analysis ****/


/*means and SD at each time point (Question 1)*/
proc means data=p.lung_wide mean std min max n nmiss maxdec=2;
class exptx;
var fact:;
run;
/*create output format for FUNO*/
proc format;
value funo_fmt
	1= "Baseline"
	2= "6 Weeks"
	3= "12 Weeks"
	4= "26 Weeks";
run;
proc tabulate data=p.lung;
label funo = "Time Point";
format funo funo_fmt.;
class funo exptx;
var fact_t2;
table  exptx*funo, fact_t2*(N mean std nmiss);
run; 

/*unadjusted mixed effects, random intercept, means model
contrast of treatment effect at each timepoint (Question 2)*/
proc sort data= p.lung;
by descending exptx patid descending funo;
run;
/*I changed the coding on this so that we are subtracting the control from the treatment,
this makes sense if you are expecting the new treatment to be better than the control*/
proc mixed data=p.lung order=data;
class funo exptx/ ;
model fact_t2 = funo exptx funo*exptx/s;
random int/ sub= patid type=un; 
estimate 'Difference at baseline' exptx 1 -1 funo*exptx 0 0 0 0 0 0 1 -1/ e cl;
estimate '6 weeks' exptx 1 -1 funo*exptx 0 0 0 0 1 -1 0 0/e cl;
estimate '12 weeks' exptx 1 -1 funo*exptx 0 0 1 -1 0 0 0 0/ e cl;
estimate '26 weeks' exptx 1 -1 funo*exptx 1 -1 0 0 0 0 0 0/e cl;
estimate 'treatment effect, post-baseline avg' exptx 3 -3 funo*exptx 1 -1 1 -1 1 -1 0 0/e cl divisor = 3;
run;





*Changes from baseline by arm;
proc mixed data=p.lung order=data ;
class funo exptx/ ;
model fact_t2 = funo exptx funo*exptx/s;
random int/ sub= patid type=un; 
estimate '6 weeks, control'  funo 0 0 1 -1 exptx 0 0 funo*exptx 0 0 0 0 0 1 0 -1/ e cl;
estimate '12 weeks, control'  funo 0 1 0 -1 exptx 0 0 funo*exptx 0 0 0 1 0 0 0 -1/ e cl;
estimate '26 weeks, control'  funo 1 0 0 -1 exptx 0 0 funo*exptx 0 1 0 0 0 0 0 -1/ e cl;
estimate '6 weeks, treatment'  funo 0 0 1 -1 exptx 0 0 funo*exptx  0 0 0 0 1 0 -1 0/ e cl;
estimate '12 weeks, treatment'  funo 0 1 0 -1 exptx 0 0 funo*exptx 0 0 1 0 0 0 -1 0/ e cl;
estimate '26 weeks, treatment'  funo 1 0 0 -1 exptx 0 0 funo*exptx 1 0 0 0 0 0 -1 0/ e cl;
run;

/*Find variables for use in MI by correlating with missingness or outcome*/
data lw;
set p.lung_wide;
miss1 = 0;
miss2 = 0;
miss3 = 0;
miss4 = 0;
if fact1 = . then miss1 = 1;
if fact2 = . then miss2 = 1;
if fact3 = . then miss3 = 1;
if fact4 = . then miss4 = 1;
miss = sum(of miss:);
run;

proc corr data = lw;
var fact1 sex ecogps sx: p_rt ctc: cycles crpr pd_lt6 surv_dur age_tx trtment exptx weeks months t_death func: phys: add: emot: socl:;
run;




*proc MI data = p.lung_wide out = lung_mi1 
nimpute = 2000 seed= 37;
	*MCMC nbiter=200000 initial=em (bootstrap= 100) plots = trace(mean);
	*var fact: exptx sx: p_rt ctc: cycles crpr pd_lt6 surv_dur qol: func: phys: socl: add: emot: ;
*run;

/*Chains are not well mixed so try to reduce the number of variables*/

*proc MI data = p.lung_wide out = lung_mi 
nimpute = 2000 seed= 37;
*	MCMC nbiter=200000 initial=em (bootstrap= 400) plots = trace(mean);
*	var fact: exptx sx_pri sx_sys ctc_hem cycles crpr pd_lt6 surv_dur qol: func: phys: ;
*run;

/*Still doesn't converge*/
*proc MI data = p.lung_wide out = lung_mi 
nimpute = 2000 seed= 37;
*	MCMC nbiter=200000 initial=em (bootstrap= 400) plots = trace(mean);
*	var fact: exptx sx_pri sx_sys ctc_hem cycles crpr pd_lt6 surv_dur qol: func: ;
*run;

/*Still no convergence for FACT: */
*proc MI data = p.lung_wide out = lung_mi 
nimpute = 2000 seed= 37;
*	MCMC nbiter=200000 initial=em (bootstrap= 400) plots = trace(mean);
*	var fact: exptx sx_sys cycles pd_lt6 surv_dur qol: func: ;
*run;

/*Still no convergence for FACT: */
*proc MI data = p.lung_wide out = lung_mi 
nimpute = 2000 seed= 37;
*	MCMC nbiter=200000 initial=em (bootstrap= 400) plots = trace(mean);
*	var fact: exptx sx_sys cycles pd_lt6 surv_dur func: ;
*run;
/*worked! Lets try adding in some of the vars we removed*/
*proc MI data = p.lung_wide out = lung_mi 
nimpute = 2000 seed= 37;
*	MCMC nbiter=200000 initial=em (bootstrap= 400) plots = trace(mean);
*	var fact: ecogps exptx sx_sys cycles pd_lt6 surv_dur func: ;
*run;
/*worked! Lets go with the MI below*/
proc MI data = p.lung_wide out = lung_mi 
nimpute = 2000 seed= 37;
	MCMC nbiter=200000 initial=em (bootstrap= 400) plots = trace(mean);
	var fact: ecogps exptx sx_sys cycles pd_lt6 surv_dur func: phys: add: emot: socl:;
run;
data p.lung_mi;
set lung_mi;
run;

proc MI data = p.lung_wide out = mini_mi 
nimpute = 20 seed= 37;
	MCMC nbiter=20 initial=em (bootstrap= 400) plots = trace(mean);
	var fact: ecogps exptx sx_sys cycles pd_lt6 surv_dur func: phys: add: emot: socl:;
run;
* perform analysis by imputation for each time period;
proc glm data = p.lung_mi;
	class exptx;
	model fact1 = Exptx;
	estimate "Control" intercept 1 exptx 1 0/e ;
	estimate "Treatment" intercept 1 exptx 0 1/e ;
	estimate "Difference" exptx -1 1/e ;
	by _imputation_;
 ods output  Estimates=ests;
run;
proc glm data = p.lung_mi;
	class exptx;
	model fact2 = Exptx;
	estimate "Control" intercept 1 exptx 1 0/e ;
	estimate "Treatment" intercept 1 exptx 0 1/e ;
	estimate "Difference" exptx -1 1/e ;
	by _imputation_;
 ods output Estimates=ests2;
run;
proc glm data = p.lung_mi;
	class exptx;
	model fact3 = Exptx;
	estimate "Control" intercept 1 exptx 1 0/e ;
	estimate "Treatment" intercept 1 exptx 0 1/e ;
	estimate "Difference" exptx -1 1/e ;
	by _imputation_;
 ods output Estimates=ests3;
run;
proc glm data = p.lung_mi;
	class exptx;
	model fact4 = Exptx;
	estimate "Control" intercept 1 exptx 1 0/e ;
	estimate "Treatment" intercept 1 exptx 0 1/e ;
	estimate "Difference" exptx -1 1/e ;
	by _imputation_;
 ods output Estimates=ests4;
run;
*diff at time 1;
proc mianalyze parms = ests;
	modeleffects control treatment difference;
run;

*diff at time 2;
proc mianalyze parms = ests2;
	modeleffects control treatment difference;
run;
*diff at time 3;
proc mianalyze parms = ests3;
	modeleffects control treatment difference;
run;
*diff at time 4;
proc mianalyze parms = ests4;
	modeleffects control treatment difference;
run;

data lg;
set p.lung;
if qol_comp = 9 then qol_simp = "Missing from Death";
if qol_comp = 0 then qol_simp= "Not Missing";
if qol_comp = 1 or qol_comp = 2 or qol_comp = 4 then qol_simp = "Missing from Other";
if qol_comp = 7 then qol_simp = "Missing from Illness";
if qol_comp = 3 or qol_comp = 5 or qol_comp = 8 then qol_simp="Missing from Staff";
if qol_comp = . then qol_simp= "Unknown";
run;



proc tabulate data=lg;
label funo = "Time Point";
format funo funo_fmt.;
class funo qol_simp exptx;
table  exptx*funo, qol_simp*(N);
run;

*Means at each time by arm;
proc mixed data=p.lung order=data ;
class funo exptx ;
model fact_t2 = funo exptx funo*exptx/s;
random int/ sub= patid type=un; 
lsmeans exptx*funo ;
run; 
