data farm;
set "C:\Classes\AppliedBiostat\Project2\frmgham.sas7bdat";
run;

proc export data=farm
outfile = "C:\Classes\AppliedBiostat\Project2\frmgham.csv"
DBMS=csv;
run;
