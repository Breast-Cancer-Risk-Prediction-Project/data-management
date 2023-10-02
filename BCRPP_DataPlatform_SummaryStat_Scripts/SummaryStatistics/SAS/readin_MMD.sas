/*=================================================================================================
Program: readin_MMD.sas
Updated: 01/28/2022
Purpose: Create SAS macro to read in the MMD covariate data file in standardized format.
Notes:
(1) This macro will be included in summary_long.sas, where the macro parameters are specified:
    dir   = directory where the MMD data file, read in macro, and variable list reside
    study = cohort name, such as NHS, NHS2
    date  = date in the name of the cohort's tab-delimited plain txt file (YYYYMMDD)

(2) The MMD covariate data file in standardized format:
    File name: ACRONYM_SECTION_yyyymmdd.txt
        - SECTION refers to the data dictionary tab, labeled as such: CORE, BRCA, and MMD
        - ACRONYM refers to the cohort acronym for your study
        - Example: NHS_MMD_20210610.txt
    File format: tab-delimited text format (.txt)
        - The column names should be the names of the variables specified in the data dictionary
        - The column order should be the same as the variables appear in the data dictionary
        - Each row should represent a subject

==================================================================================================*/


%macro readin_MMD;

data &study._MMD;
	%let _EFIERR_ = 0;	/* set the ERROR detection macro variable */
	%let _EFIREC_ = 0;	/* clear export record count macro variable */
        infile "&dir.&study._MMD_&date..txt" firstobs=2 delimiter='09'x dsd pad missover lrecl=32767;
          format subject_id $20. ;
          informat Mam_Qdate ddmmyy10. ;
          format Mam_Qdate ddmmyy10. ;
          format MamYear best12. ;
          format MamAge 5.1 ;
          format MamBMI best12. ;
          format MamMenoStat best12. ;
          format MamHRTCurrent best12. ;
          format MamType best12. ;
          format MamProcess best12. ;
          format ThreshPkg best12. ;
          format ThreshPkg_detail best12. ;
          format VolDenSoftware best12. ;
          format VolDenSoftware_detail best12. ;
          format MamDensity best12. ;
          format PDArea_A best12. ;
          format DenArea_A best12. ;
          format NonDArea_A best12. ;
        do;
          EFIOUT + 1;
          input subject_id @;
          input Mam_Qdate @;
          input MamYear @;
          input MamAge @;
          input MamBMI @;
          input MamMenoStat @;
          input MamHRTCurrent @;
          input MamType @;
          input MamProcess @;
          input ThreshPkg @;
          input ThreshPkg_detail $ @;
          input VolDenSoftware @;
          input VolDenSoftware_detail $ @;
          input MamDensity @;
          input PDArea_A @;
          input DenArea_A @;
          input NonDArea_A;
          ;
        end;
       if _ERROR_ then call symputx('_EFIERR_',1);	/* set ERROR detection macro variable */
   run;

%mend readin_MMD;

