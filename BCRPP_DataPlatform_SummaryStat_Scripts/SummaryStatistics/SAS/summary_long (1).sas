/*========================================================================================================
Program: summary_long.sas
Updated: 01/28/2022
Purpose: Create SAS macro to generate 
   	 (1) Summary Statistics Word Document
   	 (2) tab-delimited plain txt file for Summary Statistics in LONG format (one row per variable)
Notes:
(1) The following macros will be included in this macro, so please save them in the same directory:
	- Read in macros: readin_CORE.sas, readin_BRCA.sas, readin_MMD.sas
	- List of macro variables: varlist_CORE.sas, varlist_BRCA.sas, varlist_MMD.sas
(2) Use the raw standardized data file directly. This differs from the macro "summary_wide.sas", 
	which uses the harmonized SAS dataset.
(3) Macro parameters:
    dir   = working directory where the raw data files, read in macros, and variable lists reside
    study = cohort name, such as NHS, NHS2
    dd    = data section labeled by each data dictionary tab, such as CORE, BRCA, MMD
    date  = date in the name of the cohort's tab-delimited plain txt file (YYYYMMDD)

========================================================================================================*/


%macro numargs(arg);
   %let n=1;
   %do %until (%qscan(&arg,%eval(&n),%str( ))=%str());
      %let n=%eval(&n+1);
   %end;
   %eval(&n-1)
%mend numargs;



%macro summary_long (dir=, study=, dd=, date=, );

options center nodate nolabel orientation=portrait;

* Turn off proc titles;
ods noproctitle;
* Suppress the output of "The SAS System" header;
title ' ';

* Date stamp when running the program;
data _null_;
  call symput('stamp', put(date(), yymmddn8.));
run;

%include "&dir.readin_&dd..sas";
%readin_&dd;

%include "&dir.varlist_&dd..sas";


* (1) Generate Summary Statistics Word Document;

ods listing close;
ods rtf file="&dir.BCRP_&study._Summary_Stats_for_&dd._variables_&stamp..rtf" startpage=no;
title1 font='Times New Roman' bold "Summary Statistics for BCRP &study &dd Covariate Data";

proc freq data=&study._&dd;
  tables &&&dd.cate / missing nopercent;
run;

%do v=1 %to %numargs(&&&dd.cont);
        %let var = %scan(&&&dd.cont, &v);
        proc freq data=&study._&dd;
           where &var in (666, 777, 888);
           tables &var / missing nopercent;
        run;
        proc means data=&study._&dd n nmiss min q1 median mean q3 max maxdec=1;
           where &var not in (666, 777, 888);
           var &var;
        run;
%end;

title1;
ods rtf close;
ods listing;


* (2) Generate tab-delimited plain txt file for Summary Statistics;

%do u=1 %to %numargs(&&&dd.cate);
        %let x = %scan(&&&dd.cate, &u);
	proc freq data=&study._&dd noprint;
	   tables &x / out=cateout&u missing nopercent;
	run;
	data cateout&u;
	   set cateout&u;
  	   length VARIABLE $30;
	   VARIABLE = "&x";
	   rename &x = VALUE;
	run;
%end;

data cateout;
  set cateout1 - cateout%numargs(&&&dd.cate);
run;

data _null_;
  file "&dir.BCRP_&study._&dd._summary_stats_cate_&stamp..txt" delimiter='09'x dsd dropover;
  if _n_=1 then
  do;
     put 'VARIABLE' '09'x 'VALUE' '09'x 'COUNT' '09'x 'PERCENT';
  end;
  set cateout;
  format PERCENT 4.2;
  put VARIABLE VALUE COUNT PERCENT;
run;

%do v=1 %to %numargs(&&&dd.cont);
        %let y = %scan(&&&dd.cont, &v);
        proc freq data=&study._&dd noprint;
           where &y in (666, 777, 888);
           tables &y / out=contmiss&v missing nopercent;
        run;	
	proc transpose data=contmiss&v (drop=percent) out=conttr&v prefix=N; 
	   id &y;
	   var count;	
	run;
	data conttr&v;
	   set conttr&v (drop=_name_);
	   length VARIABLE $30;
	   VARIABLE = "&y";
 	run;
        proc means data=&study._&dd noprint n nmiss min q1 median mean q3 max maxdec=1;
           where &y not in (666, 777, 888);
           var &y;
	   output out=contout&v n=N nmiss=NMISS min=MIN q1=Q1 median=MEDIAN mean=MEAN q3=Q3 max=MAX;
        run;
  	data contout&v;
	   set contout&v (drop=_TYPE_ _FREQ_);
	   length VARIABLE $30;
	   VARIABLE = "&y";
 	run;
	data contout2_&v;
	   merge contout&v conttr&v;
	   by VARIABLE;
	run;
%end;

data contout2;
  set contout2_1 - contout2_%numargs(&&&dd.cont);
  if N666=. then N666 = 0;
  if N777=. then N777 = 0;
  if N888=. then N888 = 0;
  drop NMISS;
run;

data _null_;
  file "&dir.BCRP_&study._&dd._summary_stats_cont_&stamp..txt" delimiter='09'x dsd dropover;
  if _n_=1 then 
  do;
     put 
	'VARIABLE' '09'x 
	'N666' '09'x 
	'N777' '09'x 
	'N888' '09'x 
	'N' '09'x 
	'MIN' '09'x 
	'Q1' '09'x 
	'MEDIAN' '09'x 
	'MEAN' '09'x 
	'Q3' '09'x 
	'MAX';
  end;
  set contout2;
  format MIN Q1 MEDIAN MEAN Q3 MAX best12.;
  put VARIABLE N666 N777 N888 N MIN Q1 MEDIAN MEAN Q3 MAX;
run;

%mend summary_long;



/*
* Example to call the macro;

%let here = /udd/sthoh/request/Pete/BCRP_U01/;
%summary_long (dir=&here, study=NHS, dd=CORE, date=20210624);
*/
