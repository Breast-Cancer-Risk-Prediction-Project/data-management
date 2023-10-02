/*=================================================================================================
Program: readin_CORE.sas
Updated: 01/28/2022
Purpose: Create SAS macro to read in the CORE covariate data file in standardized format.
Notes:
(1) This macro will be included in summary_long.sas, where the macro parameters are specified:
    dir   = directory where the CORE data file, read in macro, and variable list reside
    study = cohort name, such as NHS, NHS2
    date  = date in the name of the cohort's tab-delimited plain txt file (YYYYMMDD)

(2) The CORE covariate data file in standardized format:
    File name: ACRONYM_SECTION_yyyymmdd.txt 
	- SECTION refers to the data dictionary tab, labeled as such: CORE, BRCA, and MMD 
	- ACRONYM refers to the cohort acronym for your study 
	- Example: NHS_CORE_20210610.txt
    File format: tab-delimited text format (.txt)
	- The column names should be the names of the variables specified in the data dictionary
	- The column order should be the same as the variables appear in the data dictionary
	- Each row should represent a subject 

==================================================================================================*/
 

%macro readin_CORE;
    
data &study._CORE;
	%let _EFIERR_ = 0;	/* set the ERROR detection macro variable */
	%let _EFIREC_ = 0;	/* clear export record count macro variable */
	infile "&dir.&study._CORE_&date..txt" firstobs=2 delimiter='09'x dsd pad missover lrecl=32767;
          format subject_id $20. ;
          format id $15. ;
          informat record_date ddmmyy10. ;
          format record_date ddmmyy10. ;
          format age 5. ;
          format qcycle best12. ;
          format baseline best12. ;
          format lastfollowup best12. ;
          format sex best12. ;
          format race best12. ;
          format ethnicity best12. ;
          format education best12. ;
          format AJAncestry best12. ;
          format height best12. ;
          format weight best12. ;
          format bmi best12. ;
          format bmi_earlyadult best12. ;
          format waist best12. ;
          format hip best12. ;
          format whr best12. ;
          format alcohol_status best12. ;
          format alcohol_init best12. ;
          format alcohol_amt best12. ;
          format alcohol_stop best12. ;
          format alcohol_dur best12. ;
          format smoking_init best12. ;
          format smoking_amt best12. ;
          format smoking_dur best12. ;
          format smoking_status best12. ;
          format smoking_stop best12. ;
          format fhx_fdr_brca best12. ;
          format Biopsies_yesno best12. ;
          format Biopsies_number best12. ;
          format BBD_history best12. ;
          format BBD_number best12. ;
          format BBD_type1 best12. ;
          format BBD_year1 best12. ;
          format BBD_type2 best12. ;
          format BBD_year2 best12. ;
          format BBD_type3 best12. ;
          format BBD_year3 best12. ;
          format BBD_type4 best12. ;
          format BBD_year4 best12. ;
          format agemenarche best12. ;
          format parous best12. ;
          format parity best12. ;
          format age_preg1 best12. ;
          format age_preg2 best12. ;
          format age_preg3 best12. ;
          format age_preg4 best12. ;
          format age_preg5 best12. ;
          format age_preg6 best12. ;
          format age_preg7 best12. ;
          format age_preg8 best12. ;
          format age_preg9 best12. ;
          format age_preg10 best12. ;
          format breastfeed best12. ;
          format breastfeed_dur best12. ;
          format ocuse_ever best12. ;
          format ocuse_current best12. ;
          format ocuse_dur best12. ;
          format ocuse_start best12. ;
          format ocuse_stop best12. ;
          format othcontracep_ever best12. ;
          format othcontracep_current best12. ;
          format meno_reason best12. ;
          format meno_age best12. ;
          format meno_status best12. ;
          format hrtuse best12. ;
          format hrt_dur best12. ;
          format hrtuse_ep best12. ;
          format hrtep_dur best12. ;
          format hrtuse_eonly best12. ;
          format hrteonly_dur best12. ;
          format pa_mets best12. ;
          format pa_pct best12. ;
          format screen_ever best12. ;
          format screen_start best12. ;
	do;
          EFIOUT + 1;
          input subject_id $ @;
          input id $ @;
          input record_date @;
          input qcycle @;
          input baseline @;
          input lastfollowup @;
          input birth_year @;
          input sex @;
          input age @;
          input race @;
          input ethnicity @;
          input education @;
          input AJAncestry @;
          input height @;
          input weight @;
          input bmi @;
          input waist @;
          input hip @;
          input whr @;
          input bmi_earlyadult @;
          input alcohol_status @;
          input alcohol_init @;
          input alcohol_amt @;
          input alcohol_stop @;
          input alcohol_dur @;
          input smoking_init @;
          input smoking_amt @;
          input smoking_dur @;
          input smoking_status @;
          input smoking_stop @;
          input fhx_fdr_brca @;
          input Biopsies_yesno @;
          input Biopsies_number @;
          input BBD_history @;
          input BBD_number @;
          input BBD_type1 @;
          input BBD_year1 @;
          input BBD_type2 @;
          input BBD_year2 @;
          input BBD_type3 @;
          input BBD_year3 @;
          input BBD_type4 @;
          input BBD_year4 @;
          input agemenarche @;
          input parous @;
          input age_preg1 @;
          input parity @;
          input age_preg2 @;
          input age_preg3 @;
          input age_preg4 @;
          input age_preg5 @;
          input age_preg6 @;
          input age_preg7 @;
          input age_preg8 @;
          input age_preg9 @;
          input age_preg10 @;
          input breastfeed @;
          input breastfeed_dur @;
          input breastfeed_dur_b1 @;
          input breastfeed_dur_b2 @;
          input breastfeed_dur_b3 @;
          input breastfeed_dur_b4 @;
          input breastfeed_dur_b5 @;
          input breastfeed_dur_b6 @;
          input breastfeed_dur_b7 @;
          input breastfeed_dur_b8 @;
          input breastfeed_dur_b9 @;
          input breastfeed_dur_b10 @;
          input ocuse_ever @;
          input ocuse_current @;
          input ocuse_dur @;
          input ocuse_start @;
          input ocuse_stop @;
          input othcontracep_ever @;
          input othcontracep_current @;
          input meno_status @;
          input meno_age @;
          input meno_reason @;
          input hrtuse @;
          input hrt_dur @;
          input hrtuse_ep @;
          input hrtep_dur @;
          input hrtuse_eonly @;
          input hrteonly_dur @;
          input pa_mets @;
          input pa_pct @;
          input screen_ever @;
          input screen_start @;
          input lastscreen_year @;
          ;
        end;
	if _ERROR_ then call symputx('_EFIERR_', 1);	/* set ERROR detection macro variable */
run;

%mend readin_CORE;

