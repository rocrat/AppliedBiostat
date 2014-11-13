proc import
datafile = "C:\Classes\AppliedBiostat\project1\support_data.csv" DBMS = CSV out = sup;
run;

data class.sup;
set sup;
if totcst <= 0 then delete;
run;

proc univariate data=class.Sup;
var totcst;
histogram;
run;

proc means data = class.Sup mean;
var age scoma meanbp hrt temp pafi alb;
output out=center mean=age_m scoma_m meanbp_m hrt_m temp_m pafi_m alb_m;
run;

data class.sup1;
merge class.sup center;
retain age_mn scoma_mn meanbp_mn hrt_mn temp_mn pafi_mn alb_mn;
if _n_ = 1 then do;
age_mn = age_m; 
scoma_mn = scoma_m;
meanbp_mn = meanbp_m;
hrt_mn = hrt_m;
temp_mn = temp_m;
pafi_mn = pafi_m;
alb_mn = alb_m; 
end;
drop _freq_ _type_ age_m scoma_m meanbp_m hrt_m temp_m pafi_m alb_m;
age_c = age - age_mn;
scoma_c = scoma - scoma_mn;
meanbp_c = meanbp - meanbp_mn;
hrt_c = hrt - hrt_mn;
temp_c = temp - temp_mn;
pafi_c = pafi - pafi_mn;
alb_c = alb - alb_mn;
ltotcst = log (totcst);
run;



proc univariate data=class.Sup1;
var ltotcst;
histogram;
run;

proc sgscatter data=class.sup1;
matrix age scoma meanbp hrt temp pafi alb ltotcst;
run;


proc glm data=class.sup1 plots = (diagnostics);
class sex dzgroup num_co race;
model ltotcst = age_c age_c*age_c scoma_c scoma_c*scoma_c meanbp_c meanbp_c*meanbp_c hrt_c hrt_c*hrt_c temp_c temp_c*temp_c pafi_c pafi_c*pafi_c alb_c alb_c*alb_c sex race dzgroup num_co;
run;

proc glm data=class.sup1 plots = (diagnostics);
class sex dzgroup num_co race;
model ltotcst = age_c scoma_c scoma_c*scoma_c meanbp_c meanbp_c*meanbp_c hrt_c hrt_c*hrt_c temp_c pafi_c pafi_c*pafi_c alb_c sex race dzgroup num_co;
run;

%include "C:/Program Files/SASHome/SASFoundation/9.3/macros/RCS_Reg.sas" /nosource;

%RCS_Reg( infile= sup1, 
dir_data = C:\Classes\SASandDataMgmt,
Main_spline_var= scoma_c, knots_msv = 5 75 95,
Oth_spline_var1= meanbp_c,
Oth_spline_var2= hrt_c,
Oth_spline_var3= pafi_c,
typ_reg= lin, dep_var= ltotcst,
adjust_var= age_c temp_c alb_c );

proc univariate data=class.sup1;
var scoma_c;
histogram;
run;
 
proc iml;                   /* begin IML session */

start MySqrt(x);            /* begin module */
   y = 1;                   /* initialize y */
   do until(w<1e-3);        /* begin DO loop */
      z = y;                /* set z=y */
      y = 0.5#(z+x/z);      /* estimate square root */
      w = abs(y-z);         /* compute change in estimate */
   end;                     /* end DO loop */
   return(y);               /* return approximation */
finish;                     /* end module */
