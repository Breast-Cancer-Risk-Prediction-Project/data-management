/*=================================================================================================
Program: readin_BRCA.sas
Updated: 01/31/2022
Purpose: Create SAS macro to read in the BRCA covariate data file in standardized format.
Notes:
(1) This macro will be included in summary_long.sas, where the macro parameters are specified:
    dir   = directory where the BRCA data file, read in macro, and variable list reside
    study = cohort name, such as NHS, NHS2
    date  = date in the name of the cohort's tab-delimited plain txt file (YYYYMMDD)

(2) The BRCA covariate data file in standardized format:
    File name: ACRONYM_SECTION_yyyymmdd.txt
        - SECTION refers to the data dictionary tab, labeled as such: CORE, BRCA, and MMD
        - ACRONYM refers to the cohort acronym for your study
        - Example: NHS_BRCA_20210610.txt
    File format: tab-delimited text format (.txt)
        - The column names should be the names of the variables specified in the data dictionary
        - The column order should be the same as the variables appear in the data dictionary
        - Each row should represent a subject

==================================================================================================*/


%macro readin_BRCA;

data &study._BRCA;
	%let _EFIERR_ = 0; 	/* set the ERROR detection macro variable */
       	%let _EFIREC_ = 0;     	/* clear export record count macro variable */
       	infile "&dir.&study._BRCA_&date..txt" firstobs=2 delimiter='09'x dsd pad missover lrecl=32767;
          format id $15. ;
          format lastfup best12. ;
          format dxdate_primary1 best12. ;
          format detection_primary1 best12. ;
          format detection_detail_primary1 best12. ;
          format invasive_primary1 best12. ;
          format dxdate_primary2 best12. ;
          format detection_primary2 best12. ;
          format detection_detail_primary2 best12. ;
          format invasive_primary2 best12. ;
          format stage_primary1 best12. ;
          format grade_primary1 best12. ;
          format size_primary1 best12. ;
          format sizecat_primary1 best12. ;
          format er_primary1 best12. ;
          format pr_primary1 best12. ;
          format her2_primary1 best12. ;
          format ki67_primary1 best12. ;
          format ki67cat_primary1 best12. ;
          format stage_primary2 best12. ;
          format grade_primary2 best12. ;
          format size_primary2 best12. ;
          format sizecat_primary2 best12. ;
          format er_primary2 best12. ;
          format pr_primary2 best12. ;
          format her2_primary2 best12. ;
          format ki67_primary2 best12. ;
          format ki67cat_primary2 best12. ;
        do;
          EFIOUT + 1;
          input id $ @;
          input lastfup @;
          input dxdate_primary1 @;
          input detection_primary1 @;
          input detection_detail_primary1 @;
          input invasive_primary1 @;
          input dxdate_primary2 @;
          input detection_primary2 @;
          input detection_detail_primary2 @;
          input invasive_primary2 @;
          input stage_primary1 @;
          input grade_primary1 @;
          input size_primary1 @;
          input sizecat_primary1 @;
          input er_primary1 @;
          input pr_primary1 @;
          input her2_primary1 @;
          input ki67_primary1 @;
          input ki67cat_primary1 @;
          input stage_primary2 @;
          input grade_primary2 @;
          input size_primary2 @;
          input sizecat_primary2 @;
          input er_primary2 @;
          input pr_primary2 @;
          input her2_primary2 @;
          input ki67_primary2 @;
          input ki67cat_primary2 @;
          ;
        end;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
   run;

%mend readin_BRCA;

