* varlist_CORE.sas;
* Include all the variables listed in the CORE data dictionary except subject_id and id;

%let COREcont = 
birth_year
age
height
weight
bmi
waist
hip
whr
bmi_earlyadult
alcohol_init
alcohol_amt
alcohol_stop
alcohol_dur
smoking_init
smoking_amt
smoking_dur
smoking_stop
agemenarche
age_preg1
age_preg2
age_preg3
age_preg4
age_preg5
age_preg6
age_preg7
age_preg8
age_preg9
age_preg10
breastfeed_dur
breastfeed_dur_b1
breastfeed_dur_b2
breastfeed_dur_b3
breastfeed_dur_b4
breastfeed_dur_b5
breastfeed_dur_b6
breastfeed_dur_b7
breastfeed_dur_b8
breastfeed_dur_b9
breastfeed_dur_b10
ocuse_dur 
ocuse_start
ocuse_stop
meno_age 
hrt_dur
hrtep_dur
hrteonly_dur
pa_mets
pa_pct
screen_start
lastscreen_year
;


%let COREcate_minus = 
qcycle
baseline
lastfollowup
sex
education
AJAncestry
alcohol_status
smoking_status
fhx_fdr_brca
Biopsies_yesno
Biopsies_number
BBD_history
BBD_number
BBD_type1
BBD_year1
BBD_type2
BBD_year2
BBD_type3
BBD_year3
BBD_type4
BBD_year4
parous
parity
breastfeed
ocuse_ever
ocuse_current
othcontracep_ever
othcontracep_current
meno_status
meno_reason
hrtuse
hrtuse_ep
hrtuse_eonly
screen_ever
;

%let COREcate = race ethnicity record_date &COREcate_minus; 
