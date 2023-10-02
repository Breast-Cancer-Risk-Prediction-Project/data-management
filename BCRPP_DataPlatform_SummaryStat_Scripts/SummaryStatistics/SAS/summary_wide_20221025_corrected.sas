/*========================================================================================================
Program: summary_wide.sas
Updated: 10/25/2022
Purpose: Create SAS macro to generate comma-delimited plain txt file for Summary Statistics
	 broken down by study/race/ethnicity and in WIDE format (one row per stratum combination).
Notes:
(1) The following macros will be included in this macro, so please save them in the same directory:
        - List of macro variables: varlist_CORE.sas, varlist_BRCA.sas, varlist_MMD.sas
(2) Use the SAS dataset directly. This differs from the macro "summary_long",
        which uses the standardized plain text data file instead.
(3) Macro parameters:
    dir   = working directory where the SAS datasets and variable lists reside
            the SAS dataset name: nhs_core.sas7bdat, cps3_brca.sas7bdat, etc.
    study = cohort name, such as NHS, NHS2
    dd    = data section labeled by each data dictionary tab, such as CORE, BRCA, MMD

========================================================================================================*/


%macro numargs(arg);
   %let n=1;
   %do %until (%qscan(&arg,%eval(&n),%str( ))=%str());
      %let n=%eval(&n+1);
   %end;
   %eval(&n-1)
%mend numargs;



%macro summary_wide (dir=, study=, dd=);

options center nodate nolabel orientation=portrait;

* Turn off proc titles;
ods noproctitle;
* Suppress the output of "The SAS System" header;
title ' ';

* Date/year stamp when running the program;
data _null_;
  call symput('stamp', put(date(), yymmddn8.));
  call symput('crrtyr', put(year(date()), 8.));
run;

* Use harmonized SAS dataset directly;
* This differs from the macro "summary_long", which starts from the raw data files;
libname outlib "&dir";
%let lowstudy = %lowcase(&study);
%let lowdd = %lowcase(&dd);

proc format;
  value racef 
	1 = 'White' 
	2 = 'Black/African American'
	3 = 'Asian'
	4 = 'Native Hawaiian/ Pacific Islander'
	5 = 'American Indian/Alaska Native'
	6 = 'Other, including multiracial'
	888 = 'Missing';
  value ethnf 
	0 = 'Non-Hispanic/Non-Latino'
	1 = 'Hispanic/Latino'
	888 = 'Missing';
run;

* Derive categorized variables for CORE and BRCA datasets, respectively;
%if &dd=CORE %then %do;
	%include "&dir.varlist_&dd..sas";
	* Extended list of CORE categorical variables;
	%let CORElst = c_birth_year c_age c_agemenarche c_bmi &COREcate_minus;

	* Derive categorized birth_year, age, agemenarche and bmi;
	data &lowstudy._&lowdd;
	  set outlib.&lowstudy._&lowdd;
	  format race racef. ethnicity ethnf.;
	  length study $6 c_birth_year $9 c_age $5 c_agemenarche $4 c_bmi $9;
	  study = "&study";

	  if birth_year=8888 then c_birth_year = 'DK';
	  else if . < birth_year < 1900 then c_birth_year = 'LT1900';
	  else if 1900 le birth_year < 1910 then c_birth_year = '1900_1909';
	  else if 1910 le birth_year < 1920 then c_birth_year = '1910_1919';
	  else if 1920 le birth_year < 1930 then c_birth_year = '1920_1929';
	  else if 1930 le birth_year < 1940 then c_birth_year = '1930_1939';
	  else if 1940 le birth_year < 1950 then c_birth_year = '1940_1949';
	  else if 1950 le birth_year < 1960 then c_birth_year = '1950_1959';
	  else if 1960 le birth_year < 1970 then c_birth_year = '1960_1969';
	  else if 1970 le birth_year < 1980 then c_birth_year = '1970_1979';
	  else if 1980 le birth_year < 1990 then c_birth_year = '1980_1989';
	  else if 1990 le birth_year < 2000 then c_birth_year = '1990_1999';
	  else if 2000 le birth_year ge &crrtyr then c_birth_year = 'GE2000';

	  if age=888 then c_age = 'DK';
	  else if . < age < 20 then c_age = 'LT20';
	  else if 20 le age < 30 then c_age = '20_29';
	  else if 30 le age < 40 then c_age = '30_39';
	  else if 40 le age < 50 then c_age = '40_49';
	  else if 50 le age < 60 then c_age = '50_59';
	  else if 60 le age < 70 then c_age = '60_69';
	  else if 70 le age < 80 then c_age = '70_79';
	  else if 80 le age < 90 then c_age = '80_89';
	  else if 90 le age < 100 then c_age = '90_99';
	  else if 100 le age < 120 then c_age = 'GE100';

	  if agemenarche=777 then c_agemenarche = 'NA';
	  else if agemenarche=888 then c_agemenarche = 'DK';
	  else if . < agemenarche le 12 then c_agemenarche = 'LE12';
	  else if 12 < agemenarche le 13 then c_agemenarche = '13';
	  else if 13 < agemenarche le 14 then c_agemenarche = '14';
	  else if 14 < agemenarche le 15 then c_agemenarche = '15';
	  else if 15 < agemenarche < 20 then c_agemenarche = 'GT15';

	  if bmi=888 then c_bmi = 'DK';
	  else if . < bmi < 18.5 then c_bmi = 'LT18.5';
	  else if 18.5 le bmi < 25 then c_bmi = '18.5_24.9';
	  else if 25.0 le bmi < 30 then c_bmi = '25.0_29.9';
	  else if 30.0 le bmi < 35 then c_bmi = '30.0_34.9';
	  else if 35.0 le bmi < 40 then c_bmi = '35.0_39.9';
	  else if 40.0 le bmi < 100 then c_bmi = 'GE40.0';
	run;
%end;
%else %if &dd=BRCA %then %do;
	* Short list of BRCA categorical variables; 
	%let BRCAlst = c_dxdate_primary1 c_invasive_primary1 c_detection_primary1 c_er_primary1 
		 c_famHist grade_primary1 sizecat_primary1;

	* Derive categorized dxdate_primary1;
	data &lowstudy._&lowdd;
	  set outlib.&lowstudy._&lowdd;
	  length c_dxdate_primary1 $9 c_invasive_primary1 $6 c_detection_primary1 $9 c_er_primary1 $3;
	  if dxdate_primary1 = 8888 then c_dxdate_primary1 = 'DK';
	  else if dxdate_primary1 = 7777 then c_dxdate_primary1 = 'NA';
	  else if dxdate_primary1 < 1930 then c_dxdate_primary1 = 'LT1930';
	  else if 1930 le dxdate_primary1 < 1940 then c_dxdate_primary1 = '1930_1939';
	  else if 1940 le dxdate_primary1 < 1950 then c_dxdate_primary1 = '1940_1949';
	  else if 1950 le dxdate_primary1 < 1960 then c_dxdate_primary1 = '1950_1959';
	  else if 1960 le dxdate_primary1 < 1970 then c_dxdate_primary1 = '1960_1969';
	  else if 1970 le dxdate_primary1 < 1980 then c_dxdate_primary1 = '1970_1979';
	  else if 1980 le dxdate_primary1 < 1990 then c_dxdate_primary1 = '1980_1989';
	  else if 1990 le dxdate_primary1 < 2000 then c_dxdate_primary1 = '1990_1999';
	  else if 2000 le dxdate_primary1 < 2010 then c_dxdate_primary1 = '2000_2009';
	  else if 2010 le dxdate_primary1 < 2020 then c_dxdate_primary1 = '2010_2019';
	  else if 2020 le dxdate_primary1 ge &crrtyr then c_dxdate_primary1 = 'GE2020';

	  if invasive_primary1=1 then c_invasive_primary1 = 'inv';
	  else if invasive_primary1=2 then c_invasive_primary1 = 'insitu';
	  else if invasive_primary1=777 then c_invasive_primary1 = 'NA';
	  else if invasive_primary1=888 then c_invasive_primary1 = 'DK';

          if detection_primary1=1 then c_detection_primary1 = 'screen';
          else if detection_primary1=2 then c_detection_primary1 = 'nonscreen';
          else if detection_primary1=777 then c_detection_primary1 = 'NA';
          else if detection_primary1=888 then c_detection_primary1 = 'DK';

          if er_primary1=0 then c_er_primary1 = 'neg';
          else if er_primary1=1 then c_er_primary1 = 'pos';
          else if er_primary1=777 then c_er_primary1 = 'NA';
          else if er_primary1=888 then c_er_primary1 = 'DK';
	run;

	* Merge in the race/ethnicity variables from CORE dataset;
	proc sort data=outlib.&lowstudy._core (keep=id race ethnicity fhx_fdr_brca) out=&lowstudy._core; 
	  by id;
	run; 
	proc sort data=&lowstudy._&lowdd; by id; run;

	* Keep the cases only: (inm = in BRCA dataset) & (dxdate_primary1 ne 7777);
	data &lowstudy._&lowdd;
	  merge &lowstudy._&lowdd (in=inm) &lowstudy._core;
	  by id;
	  if inm & (dxdate_primary1 ne 7777);
	  format race racef. ethnicity ethnf.;
          length study $6 c_famHist $3;
	  study = "&study";

	  if fhx_fdr_brca=1 then c_famHist = 'yes';
	  else if fhx_fdr_brca=1 then c_famHist = 'no';
	  else if fhx_fdr_brca=888 then c_famHist = 'DK';

	  * Recode the missing values for the observations in BRCA but not in CORE;
  	  if race=. then race = 888; 
  	  if ethnicity=. then ethnicity = 888; 
	  if fhx_fdr_brca=. then fhx_fdr_brca = 888;

	  keep id study race ethnicity &&&dd.lst;
	run;
%end;

* CORE: Output total number of subjects (TotalSubjects) in each race/ethnicity stratum;
* BRCA: Output total number of cases (TotalCases) in each race/ethnicity stratum;
proc freq data=&lowstudy._&lowdd noprint;
  tables study*race*ethnicity / out=strout missing list nopercent;
run;
proc sort data=strout (drop=percent); by study race ethnicity; run;

%do i=1 %to %numargs(&&&dd.lst);
	%let var = %scan(&&&dd.lst, &i);

	* Derive variable name without the prefix 'c_' for the categorized variables; 
	%if (&dd=CORE & &i <= 4) | (&dd=BRCA & &i <= 5) %then %do;
	  %let noc_var = %substr(&var, 3);
	%end;
	%else %do;
	  %let noc_var = &var;
	%end;

	proc freq data=&lowstudy._&lowdd noprint;
	  tables study*race*ethnicity*&var / out=frqout&i missing list nopercent;
	run;

	* Recode 777/7777 (if any) to NA for the original categorical variables;
	* Recode 888/8888 to DK for the original categorical variables;
	* Convert them from numeric variables to character variables;
	%if (&dd=CORE & &i > 4) | (&dd=BRCA & &i > 5) %then %do;
	    data frqout&i; 
		set frqout&i (drop=percent rename=(&var=num_&var));
		length &var $4;
                if num_&var in (777, 7777) then &var = 'NA';
		else if num_&var in (888, 8888) then &var = 'DK';  
		else &var = put(num_&var, 4.);
	    run;
	%end;

	* Transpose frqout from the LONG format to the WIDE format;
	proc transpose data=frqout&i out=trout&i prefix=&noc_var._;
	  by study race ethnicity;
	  id &var;
	  var count;
	run;

	* Recode the missing cell to zero;
	data trout&i; 
	  set trout&i (drop=_name_); 
	  array adjvar &noc_var.:;
	  do over adjvar;
	     if adjvar = . then adjvar = 0;
	  end;
	run;
	proc sort data=trout&i; by study race ethnicity; run;
%end;

data wide;
  merge strout trout1 - trout%numargs(&&&dd.lst);
  by study race ethnicity;
  %if &dd=CORE %then %do; rename count = TotalSubjects; %end;
  %else %if &dd=BRCA %then %do; rename count = TotalCases; %end;
run;

proc export data = wide 
            outfile = "&dir.BCRP_&study._&dd._summaries_stratified_by_race_ethnicity_&stamp..csv"
            dbms=csv replace;
  putnames = yes;
run;

proc datasets lib=work mt=data nolist kill;
quit;

%mend summary_wide;



/*
* Example to call the macro;

%let here = /udd/sthoh/request/Pete/BCRP_U01/;
%summary_wide (dir=&here, study=NHS, dd=CORE);
*/
