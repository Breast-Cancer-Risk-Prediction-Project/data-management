library(boxr)
library(expss) #count_if function
library(plyr)
library(dplyr)
library(devtools)
library(tibble)
library(tidyverse)
library(purrr)
library(stringr)

# initializing Box authentication process
box_auth(client_id = "627lww8un9twnoa8f9rjvldf7kb56q1m", client_secret = "gSKdYKLd65aQpZGrq9x4QVUNnn5C8qqm")




change_case_match <- function(data_dict ,df){
  var_names <- data_dict %>% filter(!is.na(`Variable Name`) ) %>% select(`Variable Name`) %>% dplyr::rename(`var_name` = `Variable Name`)
  
  non_matching_names <- var_names %>% filter(!var_name %in%  base::intersect(var_names$var_name, names(df ) )  )
  non_matching_names <- non_matching_names %>% filter(tolower(var_name) %in% tolower(names(df)) )
  if(length(non_matching_names$var_name) == 0){
    print(" No changes needed, all variable cases match!")
    return(df)
  } else{
    print("Cases for some variables in data will be changed to: ")
    print(non_matching_names$var_name)
    for (i in 1:length(non_matching_names[,1])){
      match_position <- match( tolower(non_matching_names$var_name[i] ),  tolower(names(df)  ) ) 
      if (!is.na(match_position )){
        names(df)[match_position] <- non_matching_names$var_name[i]
        
      }
      
    }
    return(df )
  }
  
}



add_missing_columns = function(df, data_dict) {
  list_columns_in_dictionary <- data_dict$`Variable Name`
  if(all(list_columns_in_dictionary %in% names(df) ) ){
    print("all columns from data dictionary present in data")
    return(df)
  }
  else{
    for(x in list_columns_in_dictionary){
      if (!x %in% names(df)) df <- df %>% tibble::add_column(!!x := NA)
      else df
    }
    return(df)
    
  }
  
}



#[3a] Function to read box IDs
READ.DATA= function(data, data_dict){
  
  # making sure all variable cases match data dictionary
  data <- change_case_match(data_dict = data_dict,
                            df = data)
  
  # creating columns with columns that are present in data dictionary
  data <- add_missing_columns(df = data, data_dict = data_dict)
  
  # Get study names into a list
  study_name<- factor(data$study)
  study_name= levels(study_name)
  
  transform_values <- function(variable,data){
    if (!variable %in% colnames(data) ){
      variable <- sym(variable)
      data  <- data %>% dplyr::mutate({{variable}} := NA)
    }
    
    variable <- sym(variable)
    data <- data %>% dplyr::mutate({{variable}} := ifelse(is.na({{variable}})| {{variable}} == 888, "DK", {{variable}} ))
    data
  }
  
  if (all( c("birth_year", "agemenarche", "bmi") %in% names(data) )   ) { 
    data <- data %>% 
      mutate(birth_year = case_when( birth_year >= 1900 & birth_year <= 1909 ~ "1900_1909",
                                     birth_year >= 1910 & birth_year <= 1919 ~ "1910_1919",
                                     birth_year >= 1930 & birth_year <= 1939 ~"1930_1939",
                                     birth_year >= 1940 & birth_year <= 1949 ~"1940_1949",
                                     birth_year >= 1950 & birth_year <= 1959 ~"1950_1959",
                                     birth_year >= 1960 & birth_year <= 1969 ~"1960_1969",
                                     birth_year >= 1970 & birth_year <= 1979 ~"1970_1979",
                                     birth_year >= 1980 & birth_year <= 1989 ~"1980_1989",
                                     birth_year >= 1990 & birth_year <= 1999 ~"1990_1999",
                                     birth_year >= 2000 & birth_year <= 2010 ~"2000_2009",
                                     .default = as.character(birth_year)) ) %>%
      
      mutate(agemenarche = case_when( agemenarche <=12 ~"LE_12",
                                      agemenarche == 13 ~ "13",
                                      agemenarche == 14 ~"14",
                                      agemenarche == 15 ~"15",
                                      agemenarche == 777 ~ "777",
                                      agemenarche == 888 ~"DK",
                                      !agemenarche %in% c("LE_12", "13", "14", "15","777" ,"DK",13,14,15,777) ~ "GT_15",
                                      .default = as.character(agemenarche)) ) %>%

      
      mutate(bmi= case_when( bmi < 18.5 ~"<18.5",
                             bmi>= 18.5 & bmi < 24.9 ~ "18.5_<24.9",
                             bmi>= 24.9 & bmi < 29.9 ~"24.9_<29.9",
                             bmi>= 29.9 & bmi < 34.9 ~ "29.9_<34.9",
                             bmi>= 34.9 & bmi < 39.9 ~ "34.9_<39.9",
                             bmi>= 39.9 & bmi != 888 ~ ">=39.9",
                             bmi == 888 ~ "DK",
                             .default = as.character(bmi) ) )
  }
  

  if (c("dxdate_primary1") %in% names(data) ){
    data <- data %>% 
      mutate(dxdate_primary1 = case_when(dxdate_primary1 >= 1900 &dxdate_primary1 <= 1909 ~ "1900_1909",
                                         dxdate_primary1 >= 1910 & dxdate_primary1 <= 1919 ~"1910_1919",
                                         dxdate_primary1 >= 1920 & dxdate_primary1 <= 1929 ~"1920_1929",
                                         dxdate_primary1 >= 1930 & dxdate_primary1 <= 1939 ~ "1930_1939",
                                         dxdate_primary1 >= 1940 & dxdate_primary1 <= 1949 ~ "1940_1949",
                                         dxdate_primary1 >= 1950 & dxdate_primary1 <= 1959 ~"1950_1959",
                                         dxdate_primary1 >= 1960 & dxdate_primary1 <= 1969 ~ "1960_1969",
                                         dxdate_primary1 >= 1970 & dxdate_primary1 <= 1979 ~ "1970_1979",
                                         dxdate_primary1 >= 1980 & dxdate_primary1 <= 1989 ~ "1980_1989",
                                         dxdate_primary1 >= 1990 & dxdate_primary1 <= 1999 ~ "1990_1999",
                                         dxdate_primary1 >= 2000 & dxdate_primary1 <= 2009 ~ "2000_2009",
                                         dxdate_primary1 >= 2010 & dxdate_primary1 <= 2019 ~ "2010_2019",
                                         .default = as.character(dxdate_primary1) )) }
  
  
  
  data <- data %>% reduce(.x = data %>% select(-race,-ethnicity, -age) %>% colnames(),
                          ~transform_values(variable = .y, .),
                          .init = .)
  data
}


READ.AGE = function(data, study2,race2, ethnicity2, ageRange ){
  row = c()
  for (age2 in ageRange){
    findages <- nrow(filter(data, study == study2, ethnicity == ethnicity2, race == race2, age==age2 ))
    row <- append(row, findages)
    row <- sum(row)
  }
  row
}
  

MAKE.DF = function(data){

  
  create_variables_from_levels= function(data, variable_name){
    variable <- sym(variable_name)
    if(variable_name %in% names(data)){
    levels_list <- data %>% count({{variable}}) %>% select({{variable}}) %>% 
      unlist() %>% as.character()
    return(paste0(variable_name, "_", levels_list) )
    } else{
      message(variable_name, " not in data")
    }
    
  }
  
  race_name = c("White", "Black/African American", "Asian", "Native Hawaiin/ Pacific Islander", "American Indian/Alaska Native", "Other, including multiracial", "Missing")
  eth_name = c("Non-Hispanic/Non-Latino", "Hispanic/Latino", "Missing")
  study_name = factor(data$study)
  study_name = levels(study_name)
  race <- character()
  ethnicity <- character()
  study <- character()

  for (a in race_name){
    for (b in eth_name){
      for ( c in study_name){

        race <- c(race, a)
        ethnicity <- c(ethnicity, b)
        study <- c(study,c)

        df1 <- data.frame(study,race, ethnicity)
        df1$TotalCases <- ""
        colnames_df2 <- c("age<20", "age20.29","age30.39", "age40.49", "age50.59", "age60.69", "age70.79", 
                          "age80.89", "age90.99","age100.109", "ageDK",
                          create_variables_from_levels(data,"birth_year"),
                          create_variables_from_levels(data, "agemenarche"),
                          create_variables_from_levels(data, "meno_status"),
                          create_variables_from_levels(data, "meno_reason"),
                          create_variables_from_levels(data,"bmi"),
                          create_variables_from_levels(data,"qcycle"),
                          create_variables_from_levels(data,"baseline"),
                          create_variables_from_levels(data,"lastfollowup"),
                          create_variables_from_levels(data,"sex"),
                          create_variables_from_levels(data,"education"),
                          create_variables_from_levels(data,"AJAncestry"),
                          create_variables_from_levels(data,"alcohol_status"),
                          create_variables_from_levels(data,"smoking_status"),
                          create_variables_from_levels(data,"Biopsies_yesno"),
                          create_variables_from_levels(data,"Biopsies_number"),
                          create_variables_from_levels(data,"BBD_history"),
                          create_variables_from_levels(data,"BBD_number"),
                          create_variables_from_levels(data,"BBD_type1"),
                          create_variables_from_levels(data,"BBD_type2"),
                          create_variables_from_levels(data,"BBD_type3"),
                          create_variables_from_levels(data,"BBD_type4"),
                          create_variables_from_levels(data,"parous"),
                          create_variables_from_levels(data,"parity"),
                          create_variables_from_levels(data,"fhx_fdr_brca"),
                          create_variables_from_levels(data,"ocuse_ever"),
                          create_variables_from_levels(data,"ocuse_current"),
                          create_variables_from_levels(data,"othcontracep_ever"),
                          create_variables_from_levels(data,"othcontracep_current"),
                          create_variables_from_levels(data, "hrtuse"),
                          create_variables_from_levels(data, "hrtuse_ep"),
                          create_variables_from_levels(data, "hrtuse_eonly"),
                          #create_variables_from_levels(data, "pa_mets"),
                          #create_variables_from_levels(data, "pa_pct"),
                          create_variables_from_levels(data, "screen_ever"),
                          #create_variables_from_levels(data, "screen_start"),
                          #create_variables_from_levels(data, "lastscreen_year"),
                          create_variables_from_levels(data, "prev_brca"),
                          create_variables_from_levels(data, "prev_ca"),
                          create_variables_from_levels(data,"dxdate_primary1"),
                          create_variables_from_levels(data,"invasive_primary1"),
                          create_variables_from_levels(data,"detection_primary1"),
                          create_variables_from_levels(data,"er_primary1"),
                          create_variables_from_levels(data,"famHist"),
                          create_variables_from_levels(data,"grade_primary1"),
                          create_variables_from_levels(data,"sizecat_primary1"))
        
        df2 <- data.frame(matrix(nrow = nrow(df1), ncol = length(colnames_df2)))
        colnames(df2) <- colnames_df2
        df <- cbind(df1, df2)
      }
    }
  }
  df 
}



ADD.DATA = function(data, df){

  
  creating_variable_summary = function(variable_name_string,df,data, study1, ethnicity1, race1, row){
    column_list <- df %>% select(starts_with(variable_name_string)) %>% colnames()  
    variable_value <- str_remove(string = column_list,pattern = paste0(variable_name_string, "_") )
    var_sym <- sym(variable_name_string)
    df <- df %>% mutate(across(.cols = column_list, 
                                 ~ ifelse(row_number() %in% c(row),
                                  nrow(dplyr::filter(data, study == study1,ethnicity == ethnicity1, race == race1, {{var_sym}} == str_remove(string = cur_column(), pattern =paste0( variable_name_string,"_"))) ),
                                  .)))

    df
  }
  
  row = 1
  study_name = factor(data$study)
  study_name = levels(study_name)
  
  race_name = c("White", "Black/African American", "Asian", "Native Hawaiin/ Pacific Islander", "American Indian/Alaska Native", "Other, including multiracial", "Missing")
  race_num = c(1,2,3,4,5,6,888)
  eth_name = c("Non-Hispanic/Non-Latino", "Hispanic/Latino", "Missing")
  eth_num = c(0, 1, 888)
  
  race1 <- setNames(as.list(race_num), race_name)
  eth1 <- setNames(as.list(eth_num), eth_name )
  
  race <- character()
  ethnicity <- character()
  study <- character()
  
  for (a in race_name){
    for (b in eth_name){
      for ( c in study_name){

        df[row, 4] <- nrow( filter( data, study ==c, ethnicity == eth1[[b]], race == race1[[a]] )  )

        df[row,5] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 10:19 )

        df[row,6] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 20:29 )
        
        df[row,7] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 30:39 )
        
        df[row,8] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 40:49 )
        
        df[row,9] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 50:59 )
        
        df[row,10] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 60:69 )
        
        df[row,11] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 70:79 )
        
        df[row,12] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 80:89 )
        
        df[row,13] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]],ageRange = 90:99 )
        
        df[row,14] <- READ.AGE(data,study2 =  c, ethnicity2 = eth1[[b]],  race2 = race1[[a]], ageRange = 100:109 )

        df[row,15] <- nrow( filter( data,study == c,  ethnicity == eth1[[b]], race == race1[[a]], age == 888  ) )
        

        df <- creating_variable_summary(variable_name_string = "birth_year",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "AJAncestry",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "agemenarche",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "bmi",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        
        df <- creating_variable_summary(variable_name_string = "qcycle",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "baseline",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "lastfollowup",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "sex",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "education",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "alcohol_status",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "smoking_status",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "Biopsies_yesno",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "Biopsies_number",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "BBD_history",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "BBD_number",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "BBD_type1",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "BBD_type2",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "BBD_type3",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "BBD_type4",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "parous",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "parity",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "fhx_fdr_brca",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "ocuse_ever",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "ocuse_current",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "othcontracep_ever",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "othcontracep_current",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "meno_status",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "meno_reason",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "hrtuse",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "hrtuse_ep",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "hrtep_dur",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "hrtuse_eonly",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        # df <- creating_variable_summary(variable_name_string = "pa_pct",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        #df <- creating_variable_summary(variable_name_string = "pa_mets",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "screen_ever",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        #df <- creating_variable_summary(variable_name_string = "screen_start",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        #df <- creating_variable_summary(variable_name_string = "lastscreenyear_year",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "prev_brca",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        df <- creating_variable_summary(variable_name_string = "prev_ca",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        #INCIDENT VARIABLES
        
        df <- creating_variable_summary(variable_name_string = "dxdate_primary1",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        df <- creating_variable_summary(variable_name_string = "invasive_primary1",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        df <- creating_variable_summary(variable_name_string = "detection_primary1",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        df <- creating_variable_summary(variable_name_string = "er_primary1",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)

        df <- creating_variable_summary(variable_name_string = "famHist",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        df <- creating_variable_summary(variable_name_string = "grade_primary1",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        df <- creating_variable_summary(variable_name_string = "sizecat_primary1",df = df,data =data, study1 = c,ethnicity1 =eth1[[b]], race1 = race1[[a]], row = row)
        
        row = row + 1
        
        }
      
    }
  }
  df
}



  
MAKE.SUMMSTAT = function(data, data_dict){
  data = READ.DATA(data, data_dict)
  empty_df = MAKE.DF(data)
  df = ADD.DATA(data, empty_df)
  df
}

######## FOR NCI studies that use BOX ############

# Reading core and incident data dictionaries
core.data.dict <- box_read(1031372696258) %>%
  filter(Category == "Core")

incident.data.dict <- box_read(1031372696258) %>%
  filter(Category == "Incident Breast Cancer")


#READING CORE FILE IDS FOR DATA FILES  
cps2_core_file_id <- "961944833707" # CPS2
cps3_core_file_id <- "961945416499"
nhs_core_file_id <- "1245072725343" #NHS
nhs2_core_file_id <- "1234617009726" #NHS2
plco_core_file_id <- "1288862420965" # PLCO


# READING INCIDENT FILE IDS
cps2_incident_file_id <- "961945474656"
plco_incident_file_id <- "1288860429050"
nhs_incident_file_id <- "1245060603521"
nhs2_incident_file_id <- "1234617300601"


# MAKING CORE SUMMSTAT FILES

#CPS2 CORE
cps2_core_df <- box_read(cps2_core_file_id)
cps2_core_df$study <- "CPS2"
names(cps2_core_df)
cps2_summ <- MAKE.SUMMSTAT(data = cps2_core_df, core.data.dict)

# NHS CORE
nhs_core_df <- box_read(nhs_core_file_id)
nhs_core_df$study <- "NHS"
nhs_core_summ <- MAKE.SUMMSTAT(data = nhs_core_df, core.data.dict)


#PLCO CORE
plco_bcrpp_core_df <- box_read(plco_core_file_id)
plco_bcrpp_core_df$study <- "PLCO"
plco_core_summ <- MAKE.SUMMSTAT(data = plco_bcrpp_core_df, core.data.dict)

#MAKING INCIDENT SUMMSTAT FILES

# CPS2 INCIDENT 
cps2_incident_df <- box_read(cps2_incident_file_id)
cps2_incident_df$study <- "CPS2"

cps2_incident_df<- merge(x = cps2_core_df  %>%
                           select(id,race, ethnicity, age),
                         y = cps2_incident_df, by = "id")


cps2_incident_summ <- MAKE.SUMMSTAT(data = cps2_incident_df,incident.data.dict)


#NHS INCIDENT 

nhs_bcrpp_incident_df <- box_read(nhs_incident_file_id)
nhs_bcrpp_incident_df$study <- "NHS"
nhs_core_df$id
nhs_bcrpp_incident_df<- merge(x = nhs_core_df  %>%
                           select(id,race, ethnicity, age),
                         y = nhs_bcrpp_incident_df %>% mutate(id = str_remove(string = subject_id, "NHS_")), by = "id")


nhs_bcrpp_incident_summ <- MAKE.SUMMSTAT(data = nhs_bcrpp_incident_df, incident.data.dict)


# NHS2 INCIDENT
nhs2_bcrpp_incident_df <- box_read(nhs2_incident_file_id)
nhs2_bcrpp_incident_df$study <- "NHS2"

nhs2_bcrpp_incident_df<- merge(x = nhs2_core_df  %>%
                                 select(id,race, ethnicity, age),
                               y = nhs2_bcrpp_incident_df %>% mutate(id = str_remove(string = subject_id, "NHS2_")), by = "id")


nhs2_bcrpp_incident_summ <- MAKE.SUMMSTAT(data = nhs2_bcrpp_incident_df, incident.data.dict)

#PLCO INCIDENT
plco_bcrpp_incident_df <- box_read(plco_incident_file_id)

plco_bcrpp_incident_df$study <- "PLCO"

plco_bcrpp_incident_df <- merge(x = plco_bcrpp_core_df %>% select(id,race, ethnicity, age),
                                y = plco_bcrpp_incident_df, by = "id")


plco_bcrpp_incident_summ <- MAKE.SUMMSTAT(data = plco_bcrpp_incident_df, incident.data.dict)


#######################
# FOR STUDIES THAT DO NOT USE BOX
#######################
#set working directory
# enter the entire path name for the folder that contains the data here 
#within the quotation marks
setwd("")

# Reading core data dictionary
data.dict <- read_csv("./Data Dictionary/BCRP_DataDictionary.csv")

core.data.dict <- data.dict %>% 
  filter(Category == "Core")  

# Reading incident data dictionary
incident.data.dict <- data.dict %>% 
  filter(Category == "Incident Breast Cancer")


# GENERATIONS SUMMARY STATISTICS
# GS CORE SUMMSTAT
# enter name of file in quotes
generations_bcrpp_core_df <- read.csv("")

generations_bcrpp_core_df$study <- "GS"
generations_core_summ <- MAKE.SUMMSTAT(data = generations_bcrpp_core_df, core.data.dict)

# GENERATIONS INCIDENT SUMMSTAT
# enter name of file within the quotes
generations_bcrpp_incident_df <- read.csv("")

# adding the study name
generations_bcrpp_incident_df$study <- "GS"

#adding id, race, ethnicity and age variables to the incidence df
generations_bcrpp_incident_df <- merge(x= generations_bcrpp_core_df %>% select(id, race, ethnicity, age), y= generations_bcrpp_incident_df, by = "id")

generations_bcrpp_incident_summ <- MAKE.SUMMSTAT(data = generations_bcrpp_incident_df, incident.data.dict)



############
# WRITING FILES BACK TO BOX
## FOR STUDIES THAT USE BOX
##################

#GENERATIONS STUDY
# adding CORE Summary Statistics

box_write(object = generations_bcrpp_core_summ, dir_id = "251162485433", file_name = "BCRPP_GS_CORE_summaries.csv")

box_write(object = generations_bcrpp_incident_summ, dir_id = "251162485433",file_name = "BCRPP_GS_BRCA_summaries.csv")




