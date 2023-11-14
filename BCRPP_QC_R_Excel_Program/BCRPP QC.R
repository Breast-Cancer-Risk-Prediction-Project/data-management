#install.packages('tidyverse')
library(tidyverse)
# install.packages("rlang")
library(rlang)
# install.packages('boxr')
library(boxr)
# install.packages('glue') 
library(glue)
# install.packages('stringr')
library(stringr)
#install.packages('lubridate')
library(lubridate)

#install.packages("arsenal")
library(arsenal)

#install.packages("gt")
library(gt)
#install.packages("ICD10gm")
library(ICD10gm)
#install.packages("readxl")
library(readxl)
#install.packages("openxlsx")
library(openxlsx)

# initializing Box authentication process
box_auth(client_id = "627lww8un9twnoa8f9rjvldf7kb56q1m", client_secret = "gSKdYKLd65aQpZGrq9x4QVUNnn5C8qqm")

# reading in an excel sheet containing file IDs
# for risk factor, core and survival data
#file_ids_data <- box_read(996100787501)



##################### MISC FUNCTIONS###################
# These functions are tools used in the program to arrange data in appropriate formats/ arrange the output.
######################################################

# This function unlists values from strings
# for example, if a variable has valid values of 0 and 1 and this is written in the rules excel as 0, 1,
# R will read the entire cell as a string, i.e, "0,1"
# this function will separate 0 and 1 from the string and add them to a list
# helps in preliminary QC checks where more than one value may be valid for a variable


get_range_values<- function( values_vector) {
  # values_vector  The vector containing the multiple values that are considered valid (accepts either numeric or text formats) for the variable
  
  # Returns result: a list of all the separated valid values
  
  if( any(grepl("[A-Za-z]" , values_vector) == FALSE ) ){
    if ( class ( values_vector) == 'character'){
      return (as.numeric(unlist(strsplit(values_vector, "," ) ) ) )
    } else{
      return ( c(values_vector))
    }
  }
  else {
    return ( unlist(strsplit(values_vector, "," )))
  }
}


# Function to arrange the comment columns post QC filtering
arrange.change.comments <- function(data){
  # data The data where comment columns are to be arranged
  # Returns data: data with the comment columns re-arranged after its corresponding variable
  
  comment_columns <- data %>% select(ends_with('.data.change')) %>% colnames() #extract comment columns
  data_columns <- data %>% select(!ends_with('.data.change')) %>% colnames() #extract other columns
  column_pairs <- list() # initializing empty list
  for (i in 1:length(data_columns)){  
    #this loop takes each data column and creates a vector of the variable and its comment column
    # These vectors are then added to the empty list column_pairs that was initialized
    
    column_pairs[[(length(column_pairs) + 1)]] <- c(comment_columns[i],data_columns[i] )
  }
  
  data <- reduce(   # Reduce function to use the vector pairs to re-arrange the columns in the data
    .x = column_pairs, 
    .f = ~ relocate(.x, .y[1], .after = .y[2]),
    .init = data)
  return(data)
}

# Function to arrange warning comment columns 
arrange.warning.comments <- function(data){
  # data The data where comment columns are to be arranged
  # Returns data: data with the warning comment columns re-arranged after its corresponding variable
  
  if (!any(grepl('warning', names(data)))){
    return (data)
  }
  else {
    flag_columns <- data %>% select(ends_with('.data.warning')) %>% colnames() #extract comment columns
    data_columns <- data %>% select(!ends_with('.data.change') & !ends_with('.data.warning')) %>% colnames() #extract other columns
    column_pairs <- list() # initializing empty list
    for (i in 1:length(data_columns)){  
      #this loop takes each data column and creates a vector of the variable and its comment column
      # These vectors are then added to the empty list column_pairs that was initialized
      
      column_pairs[[(length(column_pairs) + 1)]] <- c(flag_columns[i],data_columns[i] )
    }
    
    data <- reduce(   # Reduce function to use the vector pairs to re-arrange the columns in the data
      .x = column_pairs, 
      .f = ~ relocate(.x, .y[1], .after = .y[2]),
      .init = data)
    
    
    return(data)
  }
}

arrange.all.comments<- function(data){
  # data The data where comment columns are to be arranged
  # Returns data: data with the data change comment columns re-arranged after its corresponding variable
  
  if (!any(grepl('data.change', names(data)))){
    return (data)
  }
  else{
    
    
    new_column_pairs <- c()
    data_columns <- c()
    comment_columns <- data %>% select(ends_with('.data.change')) %>% colnames()
    data_columns <- comment_columns %>% str_remove('.data.change')
    
    for (i in 1:length(data_columns)){  
      #this loop takes each data column and creates a vector of the variable and its comment column
      # These vectors are then added to the empty list column_pairs that was initialized
      
      new_column_pairs[[(length(new_column_pairs) + 1)]] <- c(comment_columns[i],data_columns[i] )
    }
    
    data <- reduce(   # Reduce function to use the vector pairs to re-arrange the columns in the data
      .x = new_column_pairs, 
      .f = ~ relocate(.x, .y[1], .after = .y[2]),
      .init = data)
    return(data)
    
  }
}

change_case_match <- function(data_dict ,df){
  var_names <- data_dict %>% filter(!is.na(`Variable Name`) ) %>% select(`Variable Name`) %>% rename(`var_name` = `Variable Name`)
  
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

is.Unique <- function(vector){
  return (vector[!(duplicated(vector)| duplicated(vector, fromLast=TRUE))])
  
}


list_missing_columns <- function(data_dict, df){
  #names(data_dict) <- as.character(data_dict)
  #data_dict_vars <- data_dict %>% filter(!is.na(Variable)) %>% select(Variable) %>% unlist() %>% as.character()
  missing_vars <- data_dict %>% filter(!is.na(`Variable Name`)) %>% 
    filter(!tolower(`Variable Name`) %in% tolower(names(df))) %>%
    select(`Variable Name`) %>% unlist() %>% as.character()
  if(length(missing_vars) == 0){
    return("No variables missing!")
  }
  else{
    print("These variables are not present")
    return(missing_vars)
  }
  
}



changes.summary <- function(df){
  only.changes <- df  %>% select(ends_with('data.change'),-starts_with("Comments"))
  if (ncol(only.changes) != 0 ){
    only.changes[is.na(only.changes)] <- "No changes made"
    formula.changes <- as.formula(paste("~", paste( names(only.changes), collapse = " + " ) ) )
    changes.summary <- summary(tableby(formula.changes, data = only.changes) ,text = NULL)
    changes.summary <- as.data.frame(changes.summary)
    names(changes.summary)[1] <- "Changes"
    return(changes.summary %>% filter(Changes != "No changes made"))
  } else {
    print("There were no updates made to rows!")
    message_summ <- "There were no updates made to rows!"
    return(data.frame("Changes" = message_summ))
    
  }
}

warnings.summary<- function(df){
  only.warnings <- df %>% select(ends_with('data.warning'), -starts_with("Comments"))
  if (ncol(only.warnings) != 0 ){
    only.warnings[is.na(only.warnings)] <- "No warnings detected"
    formula.warnings<-as.formula( paste("~", paste(names(only.warnings), collapse = " + ") ) )
    warnings.summary <-summary( tableby(formula.warnings, data = only.warnings)  ,text = NULL)
    warnings.summary <- as.data.frame(warnings.summary)
    names(warnings.summary)[1] <- "Warnings"
    return(warnings.summary %>% filter(Warnings != "No warnings detected"))
  } else{
    message_summ <- "There were no warnings added!"
    return(data.frame("Warnings" = message_summ))
    
  }
}


core_summary_report <- function(core.dict, qc_df, study_name ){
  full_summ <- function(qc_df){
    changes_summary <-changes.summary(qc_df)
    print(changes_summary)
    flags_summary <- warnings.summary(qc_df)
    print(flags_summary)
    #Function to add vector as column
    summary_df <- bind_rows(changes_summary, flags_summary)
    return(summary_df )
  }
  #names(core.dict) <- as.character(core.dict[3,] )
  
  
  sheets_xl <- list("QC Data" = qc_df,
                    "Missing Variables" = qc_df %>% list_missing_columns(data_dict = core.dict) %>% as.data.frame(),
                    "ID" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Identification/Dates") %>% select(`Variable Name`) %>% unlist() %>% as.character() ), -starts_with("Comments")) %>% full_summ(),
                    "Demographics" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Demographics") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ(),
                    "Anthropometry" =qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Anthropometry") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ(),
                    "Alcohol and Tobacco" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Alcohol and Tobacco") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ(),
                    "Personal and Family History" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Personal and family Health History") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ(),
                    "Reproductive History" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Reproductive History") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ(),
                    "Hormone Replacement Therapy" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Hormone Replacement Therapy") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ(),
                  "Physical Activity" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Physical Activity") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ(),
                  "Screening History" = qc_df %>% select(contains(core.dict %>% filter(`Sub-Category` == "Screening History") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ())  
  
  
  writexl::write_xlsx(sheets_xl, path = paste0("./", study_name, " BCRPP Core QC Report.xlsx") )
  
}


incident_summary_report <- function(incident.dict, qc_df, study_name ){
  full_summ <- function(qc_df){
    changes_summary <-changes.summary(qc_df)
    print(changes_summary)
    flags_summary <- warnings.summary(qc_df)
    print(flags_summary)
    #Function to add vector as column
    summary_df <- bind_rows(changes_summary, flags_summary)
    return(summary_df )
  }
  
  
  
  sheets_xl <- list("QC Data" = qc_df,
                    "Missing Variables" = qc_df %>% list_missing_columns(data_dict = incident.dict) %>% as.data.frame(),
                    "Diagnostic" = qc_df %>% select(contains(incident.dict %>% filter(`Sub-Category` == "Diagnostic") %>% select(`Variable Name`) %>% unlist() %>% as.character() ), -starts_with("Comments")) %>% full_summ(),
                    "Pathology" = qc_df %>% select(contains(incident.dict %>% filter(`Sub-Category` == "Pathology") %>% select(`Variable Name`) %>% unlist() %>% as.character()), -starts_with("Comments")) %>% full_summ() )
                    
                    
  writexl::write_xlsx(sheets_xl, path = paste0("./", study_name, " BCRPP Incident Cases QC Report.xlsx") )
                    
}


#################################################### PART 1 #############################################################
#################################################### MODIFYING DATA TO PREVENT 777/888 inconsistency errors #############
#########################################################################################################################


########################### QC Change Functions ###################
# The next functions change the values according to the excel rules
# Each function filters rows that need to be changed differently
# the functions make columns ending in suffix 'data.change' 
# for rows where data is changed
###################################################################

# QC function to change data according to the variable's value 
# without considering any other variable values

# For example, MenoStat can only have values of 1, 2 and 888 regardless of what 
# values the other variables may have

valid.changes <- function(data,params){
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  
  # Returns data: The data frame with variable values changed according to rules with corresponding comment columns mentioning the change
  print('====> in valid.changes')
  if (!(params$variable)  %in% names(data)) {
    print(glue(' {params$Variable} not present in data set, skipping this QC step'))
    return (data)
  }else{
    #converting variable name to symbol
    variable_symbol <- sym(params$variable)
    # creating a comment name 
    comment1 <- paste0(variable_symbol, '.data.change')
    # using the variable symbol and comment1 to change the data and leave a comment
    data <- data %>% mutate(!!comment1 := ifelse(!!variable_symbol %in% get_range_values(params$value_changed),
                                                 # ifelse(nchar(comment)==0,
                                                 #          params$comment,
                                                 #         paste0(comment, " | ", params$comment)), 
                                                 params$comment,
                                                 data[[comment1]]),
                            !!variable_symbol := ifelse( !!variable_symbol %in% get_range_values(params$value_changed), as.numeric(as.numeric(params$new_value)), !!variable_symbol)
    )
    return(data)
  }
}



crossvalid1.changes <- function(data,params){
  # Similar function to valid.
  # Corrects 777 and 888 values according to cross variables.
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  
  
  print('====> in crossvalid1.changes')
  if (!(params$variable)  %in% names(data) | !(params$cross_variable_1 %in% names(data))) {
    print(glue(' {params$Variable} not present in data set, skipping this QC step'))
    return (data)
  } else {
    # creating symbol for variable
    variable_symbol = sym(params$variable)
    # creating symbol for first cross variable
    crossvar1_symbol = sym(params$cross_variable_1)
    #creating symbol for comment corresponding to variable
    comment1 <- paste0(variable_symbol, '.data.change')
    # mutate is used to add the change 777 and 888 values associated with the variable and leave a comment 
    data <- data %>% mutate( !!comment1 := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value),
                                                   params$comment,
                                                   data[[comment1]]),
                             !!variable_symbol := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value),
                                                          as.numeric(params$new_value),
                                                          !!variable_symbol) )
    return (data)
  }
}



crossvalid2.changes <- function(data,params){
  # Corrects 777 and 888 values using three cross variables.
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  
  # Returns data frame with the 777 and 888 values  changed and corresponding comments added
  print('=====> in crossvalid2.changes')
  #Corrects 777 and 888 values for three cross variables.
  if (!(params$variable)  %in% names(data) | !(params$cross_variable_1) %in% names(data) | !(params$cross_variable_2) %in% names(data)) {
    print(glue(' {params$Variable} not present in data set, skipping this QC step'))
    return (data)
  } else{
    #creates smbol for variable  
    variable_symbol = sym(params$variable)
    # symbol for first cross variable
    crossvar1_symbol = sym(params$cross_variable_1)
    # symbol for second cross variable
    crossvar2_symbol = sym(params$cross_variable_2)
    
    
    # symbol created for the comment
    comment1 <- paste0(variable_symbol, '.data.change')
    # mutate adds comment about change to be made based on 
    #conditions listed by the rules
    data <- data %>% mutate( !!comment1 := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value)
                                                   & !!crossvar2_symbol %in% get_range_values(params$cross_variable_2_value),
                                                   params$comment,
                                                   data[[comment1]]),
                             # mutate changes 777 and 888 values for the variable based on
                             #conditions listed by the rules
                             !!variable_symbol := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value)
                                                          & !!crossvar2_symbol %in% get_range_values(params$cross_variable_2_value),
                                                          as.numeric(params$new_value),
                                                          !!variable_symbol)
    )
    return(data)
  }
}




crossvalid3.changes <- function(data,params){
  # Corrects 777 and 888 values using three cross variables.
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  
  # Returns data frame with the 777 and 888 values  changed and corresponding comments added
  print('====> in crossvalid3.changes')
  #Corrects 777 and 888 values for three cross variables.
  if (!(params$variable)  %in% names(data) | !(params$cross_variable_1) %in% names(data) | !(params$cross_variable_2) %in% names(data)
      | !(params$cross_variable_3) %in% names(data)) {
    print(glue(' {params$Variable} not present in data set, skipping this QC step'))
    return (data)
  } else {
    #creates smbol for variable  
    variable_symbol = sym(params$variable)
    # symbol for first cross variable
    crossvar1_symbol = sym(params$cross_variable_1)
    # symbol for second cross variable
    crossvar2_symbol = sym(params$cross_variable_2)
    
    # symbol for third cross variable
    crossvar3_symbol = sym(params$cross_variable_3)
    # symbol created for the comment
    comment1 <- paste0(variable_symbol, '.data.change')
    # mutate adds comment about change to be made based on 
    #conditions listed by the rules
    data <- data %>% mutate( !!comment1 := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) 
                                                   & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value)
                                                   & !!crossvar2_symbol %in% get_range_values(params$cross_variable_2_value)
                                                   & !!crossvar3_symbol %in% get_range_values(params$cross_variable_3_value),
                                                   params$comment,
                                                   data[[comment1]]),
                             # mutate changes 777 and 888 values for the variable based on
                             #conditions listed by the rules
                             !!variable_symbol := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) 
                                                          & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value)
                                                          & !!crossvar2_symbol %in% get_range_values(params$cross_variable_2_value)
                                                          & !!crossvar3_symbol %in% get_range_values(params$cross_variable_3_value),
                                                          as.numeric(params$new_value),
                                                          !!variable_symbol)
    )
    return(data)
  }
}


crossvalid4.changes <- function(data,params){
  # Correction for 777 and 888 values for four cross variables.
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  # Returns data frame with the 777 and 888 values  changed and corresponding comments added
  
  print('======> in crossvalid4.changes')
  if (!(params$variable)  %in% names(data) | !(params$cross_variable_1) %in% names(data) | !(params$cross_variable_2) %in% names(data)
      | !(params$cross_variable_3) %in% names(data) | !(params$cross_variable_4) %in% names(data) ) {
    print(glue(' {params$Variable} not present in data set, skipping this QC step'))
    return (data)
  } else {
    # create symbol for the variable
    variable_symbol = sym(params$variable)
    # create symbol for first cross variable
    crossvar1_symbol = sym(params$cross_variable_1)
    # create sumbol for second cross variable
    crossvar2_symbol = sym(params$cross_variable_2)
    # create symbol for third cross variable
    crossvar3_symbol = sym(params$cross_variable_3)
    # create symbol for fourth cross variable
    crossvar4_symbol = sym(params$cross_variable_4)
    # create symbol for the comment column
    comment1 <- paste0(variable_symbol, '.data.change')
    # mutate adds comment about change to be made based on 
    # conditions listed by the rules
    data <- data %>% mutate( !!comment1 := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value)
                                                   & !!crossvar2_symbol %in% get_range_values(params$cross_variable_2_value)
                                                   & !!crossvar3_symbol %in% get_range_values(params$cross_variable_3_value)
                                                   & !!crossvar4_symbol %in% get_range_values(params$cross_variable_4_value),
                                                   params$comment,
                                                   data[[comment1]]),
                             # mutate changes 777 and 888 values for the variable based on
                             #conditions listed by the rules
                             !!variable_symbol := ifelse( !!variable_symbol %in% get_range_values(params$value_changed) & !!crossvar1_symbol %in% get_range_values(params$cross_variable_1_value)
                                                          & !!crossvar2_symbol %in% get_range_values(params$cross_variable_2_value)
                                                          & !!crossvar3_symbol %in% get_range_values(params$cross_variable_3_value)
                                                          & !!crossvar4_symbol %in% get_range_values(params$cross_variable_4_value),
                                                          as.numeric(params$new_value),
                                                          !!variable_symbol)
    )
    return(data)
  }
}


range.changes <- function(data, params){
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  
  # Returns data: The data frame with variable values changed according to rules with corresponding comment columns mentioning the change
  print(' ====> in range.changes')
  
  if (!params$variable %in% names(data)){
    print(glue(' {params$variable} not present in data set, skipping this QC step'))
    return (data)
  } else {
    #converting variable name to symbol
    variable_symbol <- sym(params$variable)
    # creating a comment name 
    comment1 <- paste0(variable_symbol, '.data.change')
    # using the variable symbol and comment to change the data and leave a comment
    data <- data %>% mutate(!!comment1 := ifelse(!!parse_expr(params$value_changed),
                                                 # ifelse(nchar(comment)==0,
                                                 #          params$comment,
                                                 #         paste0(comment, " | ", params$comment)), 
                                                 params$comment,
                                                 data[[comment1]]),
                            !!variable_symbol := ifelse( !!parse_expr(params$value_changed),
                                                         eval(rlang::parse_expr(params$new_value)),
                                                         !!variable_symbol)
    )
    return(data)
  }
}

cross_range1.changes <- function(data, params){
  print('========> in cross_range1.changes')
  if (!(params$variable)  %in% names(data) | !(params$cross_variable_1) %in% names(data) ) {
    print(glue(' {params$variable} or {params$cross_variable_1} not present in data set, skipping this QC step'))
    return (data)
  } else{
    message("Variable: ", params$variable, " Cross Variable: ", params$cross_variable_1)
    #converting variable name to symbol
    variable_symbol <- sym(params$variable)
    # create symbol for first cross variable
    crossvar1_symbol = sym(params$cross_variable_1)
    # creating a comment name 
    comment1 <- paste0(variable_symbol, '.data.change')
    data <- data %>% mutate(!!comment1 := ifelse( !!parse_expr(params$value_changed)
                                                  & !!parse_expr(params$cross_variable_1_value),
                                                  params$comment,
                                                  data[[comment1]]),
                            !!variable_symbol := ifelse(!!parse_expr(params$value_changed)
                                                        & !!parse_expr(params$cross_variable_1_value),
                                                        eval(rlang::parse_expr(as.character(params$new_value)) ),
                                                        !!variable_symbol))
    return(data)   
  }
  
}




cross_range2.changes <- function(data, params){
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  
  # Returns data: The data frame with variable values changed according to rules with corresponding comment columns mentioning the change
  print('====> in cross_range2.changes')
  if (!(params$variable)  %in% names(data) | !(params$cross_variable_1) %in% names(data) | !(params$cross_variable_2) %in% names(data)) {
    print(glue(' {params$variable} not present in data set, skipping this QC step'))
    return (data)
  } else{
    
    #converting variable name to symbol
    variable_symbol <- sym(params$variable)
    # create symbol for first cross variable
    crossvar1_symbol = sym(params$cross_variable_1)
    # create symbol for second cross variable
    crossvar2_symbol = sym(params$cross_variable_2)
    message("variable: ", params$variable, " cross variable 1: ", params$cross_variable_1, " cross variable 2: ", params$cross_variable_2 )
    # creating a comment name
    comment1 <- paste0(variable_symbol, '.data.change')
    # using the variable symbol and comment to change the data and leave a comment
    data <- data %>% mutate(!!comment1 := ifelse(!!parse_expr(params$value_changed)
                                                 & !!parse_expr(params$cross_variable_1_value)
                                                 & !! parse_expr(params$cross_variable_2_value),
                                                 # ifelse(nchar(comment)==0,
                                                 #          params$comment,
                                                 #         paste0(comment, " | ", params$comment)),
                                                 params$comment,
                                                 data[[comment1]]),
                            !!variable_symbol := ifelse(!!parse_expr(params$value_changed)
                                                        & !!parse_expr(params$cross_variable_1_value)
                                                        & !! parse_expr(params$cross_variable_2_value),
                                                        eval(rlang::parse_expr(as.character(params$new_value))),
                                                        !!variable_symbol)
    )
    return(data)
  }
}


cross_range3.changes <- function(data, params){
  # data: The data frame where the data is to be changed
  # params: The data frame of the rules which are being used to change the data
  
  # Returns data: The data frame with variable values changed according to rules with corresponding comment columns mentioning the change
  print('====> in cross_range3.changes')
  if (!(params$variable)  %in% names(data) | !(params$cross_variable_1) %in% names(data) | !(params$cross_variable_2) %in% names(data)|
      !(params$cross_variable_3) %in% names(data)) {
    print(glue(' {params$variable} or {params$cross_variable_1} or {params$cross_variable_2} or {params$cross_variable_3} not present in data set, skipping this QC step'))
    return (data)
  } else{
    
    #converting variable name to symbol
    variable_symbol <- sym(params$variable)
    # create symbol for first cross variable
    crossvar1_symbol = sym(params$cross_variable_1)
    # create symbol for second cross variable
    crossvar2_symbol = sym(params$cross_variable_2)
    # create symbol for third cross variable
    crossvar3_symbol = sym(params$cross_variable_3)
    # creating a comment name
    comment1 <- paste0(variable_symbol, '.data.change')
    # using the variable symbol and comment to change the data and leave a comment
    data <- data %>% mutate(!!comment1 := ifelse(!!parse_expr(params$value_changed)
                                                 & !!parse_expr(params$cross_variable_1_value)
                                                 & !! parse_expr(params$cross_variable_2_value)
                                                 & !! parse_expr(params$cross_variable_3_value),
                                                 # ifelse(nchar(comment)==0,
                                                 #          params$comment,
                                                 #         paste0(comment, " | ", params$comment)),
                                                 params$comment,
                                                 data[[comment1]]),
                            !!variable_symbol := ifelse(!!parse_expr(params$value_changed)
                                                        & !!parse_expr(params$cross_variable_1_value)
                                                        & !! parse_expr(params$cross_variable_2_value)
                                                        & !! parse_expr(params$cross_variable_3_value),
                                                        eval(rlang::parse_expr(as.character(params$new_value))),
                                                        !!variable_symbol)
    )
    return(data)
  }
}





################# Main Functions##########
# this contains the main body that calls all the functions above.
# the main objective is to take the rules and run apply to the data set.
# the output data set after one line of the rules is applied
# will be the inout for the next line of rules
# and so on untill all the rules are run on the data set.
#######################################################

# run one type of correction
# the rules are all the rules FOR THIS type
# the data is all the data.
# returns the data that is corrected according to the 
# qa rules that is called.

# using the reduce function, this will be mapped with the rules to the data set 
# so that all the functions are executed on the data set

run_qa_type.changes<-function(data,params){
  data <- rlang::exec(params$type,data=data,params=params)
  data
}

#' changes_qc
#'
#' @param rules A tibble defining the how to correct variables
#' @param data  The data being corrected
#'
#' @return 
#'


changes_qc <- function(rules,data){
  # the main function that applies the rules to the data
  
  ## don't overwrite a users preference.
  ## save the old value, which may be false.
  orig<-options('dplyr.summarise.inform'=FALSE) 
  
  # in case this program is run multiple times on the same data
  # existing change columns will be removed first
  data <- data %>% select(-ends_with("data.change") )
  # create empty comment columns for every variable
  # this column will have comments specifying where changes were made 
  data[paste0(names(data),'.data.change')] <- NA
  
  
  # mapping the rules through every data frame
  data <-  rules  %>% pmap(~list(...)) %>% 
    reduce(run_qa_type.changes,.init=data) %>%
    arrange.change.comments() #%>% # arranging comment columns next to the corresponding variable
  #creating a list of comment columns that are empty because no changes were made for that value
  empty_change_columns<- data %>% select_if(function(x){all(is.na(x))}) %>% select(ends_with("data.change")) %>% colnames()
  #removing the change columns that are empty
  data <- data %>% select(-all_of(empty_change_columns))
  options(orig)
  data
}




# writing the resultant data frame to an excel sheet in Box
# this file is now the data for the second part of the QC program 
# the QC filtering program will filter rows from this data frame
# that flags rows that require further verification

#box_write(df.changes,dir_id = 150319090948, file_name =  'Data with QC.csv', description = "This version of the data file has changes made to the data based on the QC program and comment next to rows where changes have been made.")

#################################################### PART 2 ##############################################################
#################################################### ADDING FLAGS FOR ROWS THAT REQUIRE MORE VERIFICATION ###################
#############################################################################################################################


# function for QC related to ranges
# for example, the valid accepted values of BMI (excluding 777 and 888) are between 16 and 50

range.warnings <- function(data,params){
  print('========> in range.warnings')
  if (!(params$Variable) %in% names(data) ){
    print(glue(' {params$Variable} not present in data set, skipping this QC step'))
    return (data)
  }
  else{
    message("variable: ",params$`Variable`," min: ",params$`Valid Value Lower`,' max: ',params$`Valid Value Higher`)
    # creating symbol of variable
    variable_symbol = sym(params$`Variable`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = paste0(comment_symbol, '.data.warning')
    
    data <- data %>% mutate(!!comment2 := ifelse( !!variable_symbol < eval(rlang::parse_expr(as.character( params$`Valid Value Lower` ) )) 
                                                  & !(!!variable_symbol) %in% c(777,888)|
                                                    !!variable_symbol > eval( rlang::parse_expr ( as.character (params$`Valid Value Higher`)))
                                                  & !(!!variable_symbol) %in% c(777,888) ,
                                                  ifelse(is.na(data[[comment2]]), params$Comments, paste(data[[comment2]], ' | ', params$Comments)), 
                                                  data[[comment2]]) )
    return(data)
  }
}


# QC function to catch inconsistencies according to the variable's value 
# without considering any other variable values

# For example, MenoStat can only have values of 1, 2 and 888 regardless of what 
# values the other variables may have
valid.warnings <- function(data,params){
  # data: The data frame where the data is to be checked
  # params: The data frame of the rules which are being used to change the data
  
  # Returns data: The data frame with warning comments next to rows where further verification is needed
  print('=========> in valid.warnings')
  if (!(params$Variable)  %in% names(data)) {
    print(glue(" {params$Variable} not present - skipping this QC step") )
    return (data)
  }else{
    message("Variable: ", params$Variable)
    #converting variable name to symbol
    variable_symbol <- sym(params$Variable)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = paste0(comment_symbol, '.data.warning')
    
    
    
    # using the variable symbol and comment2 to change the data and leave a comment
    data <- data %>% mutate(!!comment2 := ifelse( !(!!variable_symbol) %in% get_range_values(params$`Valid Values`),
                                                  ifelse(is.na(data[[comment2]]),
                                                         params$Comments,
                                                         paste(data[[comment2]], ' | ', params$Comments) ), 
                                                  data[[comment2]]))
    return(data)
  }
}

# QC function to catch inconsistencies related to another variable's value
# for example, Parity has to be 0 if parous is 0

crossvalid1.warnings <- function(data, params){
  print('==========> in crossvalid1.warnings')
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data)){
    print(glue('{params$Variable} or {params$`Cross Variable 1`} not present in data set, skipping this QC step') )
    return (data)
  }
  else {
    
    message("variable: ", params$`Variable`, " Cross Variable 1: ", params$`Cross Variable 1`) 
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = paste0(comment_symbol, '.data.warning')
    
    data <- data %>% 
      mutate(!!comment2 := ifelse( !!variable_symbol  %in% get_range_values(params$`Valid Values`) 
                                   & !(!!crossvar1_symbol) %in% get_range_values(params$`Cross Variable 1 Value`),
                                   ifelse( is.na(data[[comment2]]),
                                           params$Comments,
                                           paste(data[[comment2]], ' | ', params$Comments) ), 
                                   data[[comment2]]) )
    return(data)
  }
}


# QC function to catch inconsistencies related to two other values of variables
crossvalid2.warnings <- function(data, params){
  print('=========> in crossvalid2.warnings')
  
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data) | !params$`Cross Variable 2` %in% names(data)){
    print(glue('{params$Variable} or cross variables not present in data set, skipping this QC step') )
    return (data)
  }
  else{
    message("variable: ", params$`Variable`, " Cross Variable 1: ", params$`Cross Variable 1`, " Cross Variable 2: ", params$`Cross Variable 2`) 
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    crossvar2_symbol = sym(params$`Cross Variable 2`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = paste0(comment_symbol, '.data.warning')
    
    data <- data %>%
      mutate(!!comment2 := ifelse(!!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar1_symbol) %in% get_range_values(params$`Cross Variable 1 Value`)
                                  |!!variable_symbol %in% get_range_values(params$`Valid Values`) &!(!!crossvar2_symbol) %in% get_range_values(params$`Cross Variable 2 Value`),
                                  
                                  ifelse(is.na(data[[comment2]]),
                                         params$Comments,
                                         paste(data[[comment2]], ' | ', params$Comments)), 
                                  data[[comment2]]) )
    return(data)
  }
}


# QC function to catch inconsistencies related to three other values of variables
crossvalid3.warnings <- function(data, params){
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data) | !params$`Cross Variable 2` %in% names(data)
      | !params$`Cross Variable 3` %in% names(data) ){
    print(glue('{params$Variable} or cross variables not present in data set, skipping this QC step') )
    return(data)
  }
  else{
    print("===========> in crossvalid3.warnings")
    message("variable: ", params$`Variable`, "Cross Variable 1: ", params$`Cross Variable 1`, "Cross Variable 2: ", params$`Cross Variable 2`, "Cross Variable 3: ", params$`Cross Variable 3`) 
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    crossvar2_symbol = sym(params$`Cross Variable 2`)
    crossvar3_symbol = sym(params$`Cross Variable 3`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = paste0(comment_symbol, '.data.warning')
    
    data <- data  %>% 
      mutate(!!comment2 := ifelse(  !!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar1_symbol) %in% get_range_values(params$`Cross Variable 1 Value`)
                                    |!!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar2_symbol) %in% get_range_values(params$`Cross Variable 2 Value`)
                                    |!!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar3_symbol) %in% get_range_values(params$`Cross Variable 3 Value`),
                                    
                                    ifelse(is.na(data[[comment2]]),
                                           params$Comments,
                                           paste(data[[comment2]], ' | ', params$Comments) ),
                                    data[[comment2]] )  )
    return(data)
  }
}



# QC function to catch inconsistencies related to four other values of variables
crossvalid4.warnings <- function(data, params){
  print("===========> in crossvalid4.warnings")
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data) | !params$`Cross Variable 2` %in% names(data)
      | !params$`Cross Variable 3` %in% names(data) | !params$`Cross Variable 4` %in% names(data)){
    print(glue('{params$Variable} or cross variables not present in data set, skipping this QC step') )
    return(data)
  }
  else{
    message("variable: ", params$`Variable`, "Cross Variable 1: ", params$`Cross Variable 1`, "Cross Variable 2: ", params$`Cross Variable 2`) 
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    crossvar2_symbol = sym(params$`Cross Variable 2`)
    crossvar3_symbol = sym(params$`Cross Variable 3`)
    crossvar4_symbol = sym(params$`Cross Variable 4`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = paste0(comment_symbol, '.data.warning')
    
    data <- data  %>% 
      mutate(!!comment2 := ifelse(  !!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar1_symbol) %in% get_range_values(params$`Cross Variable 1 Value`)
                                    |!!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar2_symbol) %in% get_range_values(params$`Cross Variable 2 Value`)
                                    |!!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar3_symbol) %in% get_range_values(params$`Cross Variable 3 Value`)
                                    |!!variable_symbol %in% get_range_values(params$`Valid Values`) & !(!!crossvar4_symbol) %in% get_range_values(params$`Cross Variable 4 Value`),
                                    
                                    ifelse(is.na(data[[comment2]]),
                                           params$Comments,
                                           paste(data[[comment2]], ' | ', params$Comments) ),
                                    data[[comment2]] )  )
    return(data)
  }
}

# QC function to catch inconsistencies related to ranges which also depend on other cross variables
# for example, Parity can only be more than 0 if parous is 1
# in the case that parous is 1, 
# a warning comment will be left if Parity is less than 1 or greater than 15

#crossrange.warnings<- function(data, params){

#print('===========> in crossrange.warnings')
#if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data) | !params$Formula_Variable %in% names(data)){
#print(glue('Either {params$Variable}, cross variables or {params$Formula_Variable} not present in data set, skipping this QC step') )
# return (data)
#} else{
#message("variable: ", params$`Variable`, "Cross Variable: ", params$`Cross Variable 1`) 
#variable_symbol = sym(params$`Variable`)
#crossvar1_symbol = sym(params$`Cross Variable 1`)
# creating symbol for the comment variable
# comment_symbol = sym(params$Comment_Variable)
# creating comment column
#  comment2 = paste0(comment_symbol, '.data.warning')
#   if (any(grepl("[A-Za-z]", params$`Cross Variable 1 Value` ) == FALSE  ) ){

#      data <- data %>%
#        mutate( !!comment2 := ifelse( !!crossvar1_symbol %in% get_range_values(params$`Cross Variable 1 Value`)
#                                     & !(!!variable_symbol) %in% c(777,888)
#                                      & !(!!parse_expr(params$Formula_Condition)),
#                                      ifelse( is.na(data[[comment2]]), params$Comments, paste(data[[comment2]], ' | ', params$Comments) ),
#                                      data[[comment2]] ) )
#      return(data)
#    } else if (any(grepl('[^[:alnum:]]', params$`Cross Variable 1 Value`) == TRUE) ){
#      data <- data %>%
#        mutate( !!comment2 := ifelse( #!!crossvar1_symbol %in% get_range_values(params$`Cross Variable 1 Value`)
#          !!parse_expr(params$`Cross Variable 1 Value`)
#          & !(!!variable_symbol) %in% c(777,888)
#          & !(!!parse_expr(params$Formula_Condition)),
#          ifelse( is.na(data[[comment2]]), params$Comments, paste(data[[comment2]], ' | ', params$Comments) ),
#          data[[comment2]] ) )
#      return(data)
#    }
#  }
#}

crossrange.warnings <- function(data, params){
  print('=====> in crossrange.warnings')
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data)
      | any(!get_range_values(params$`Formula_Variable`) %in% names(data))  ){
    print(glue('Either {params$Variable} or cross variables not present in data set, skipping this QC step') )
    return(data)
    # first if bracket closing
  } else{
    
    message("variable: ", params$`Variable`, " Cross Variable: ", params$`Cross Variable 1`)
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = sym(paste0(comment_symbol, '.data.warning'))
    if (any(grepl("[A-Za-z]", params$`Cross Variable 1 Value` ) == FALSE  )  ){
      
      data <- data %>%
        mutate( formula_condition_fail = !(!!parse_expr(params$Formula_Condition)),
                # NA values resulting from formula condition fail need to be set to FALSE
                formula_condition_fail = if_else(is.na(formula_condition_fail), FALSE, formula_condition_fail),
                
                !!comment2 := ifelse( !!crossvar1_symbol %in% get_range_values(params$`Cross Variable 1 Value`)
                                      & formula_condition_fail,
                                      ifelse( is.na({{comment2}}), params$Comments, paste({{comment2}}, ' | ', params$Comments) ),
                                      {{comment2}}) )
      
      return(data %>% select(-formula_condition_fail))
      
    } # if bracket for no formula crossrange for numbers
    else if (any(grepl('[^[:alnum:]]' , params$`Cross Variable 1 Value`) == TRUE ) ) {
      # print(params$Comments)
      data <- data %>%
        mutate( formula_condition_fail = !(!!parse_expr(params$Formula_Condition)),
                # NA values resulting from formula condition fail need to be set to FALSE
                formula_condition_fail = if_else(is.na(formula_condition_fail), FALSE, formula_condition_fail),
                cross_var_val_match = !!parse_expr(params$`Cross Variable 1 Value`),
                cross_var_val_match = if_else(is.na(cross_var_val_match), FALSE, cross_var_val_match),
                !!comment2 := ifelse( cross_var_val_match & formula_condition_fail,
                                      ifelse( is.na({{comment2}}),
                                              params$Comments,
                                              paste({{comment2}}, ' | ', params$Comments) ),
                                      {{comment2}}) )
      
      #print(data %>% select(formula_condition_fail, cross_var_val_match, {{comment2}}) %>% slice(1) )
      
      return(data %>% select(-c(formula_condition_fail, cross_var_val_match)) )
    }# else bracket for formula
    else {
      
      
      data <- data %>%
        mutate( formula_condition_fail = !(!!parse_expr(params$Formula_Condition)),
                # NA values resulting from formula condition fail need to be set to FALSE
                formula_condition_fail = if_else(is.na(formula_condition_fail), FALSE, formula_condition_fail),
                !!comment2 := ifelse( !!crossvar1_symbol %in% get_range_values(params$`Cross Variable 1 Value`)
                                      & formula_condition_fail,
                                      ifelse( is.na({{comment2}}), params$Comments, paste({{comment2}}, ' | ', params$Comments) ),
                                      {{comment2}} ) )
      return(data %>% select(-formula_condition_fail))
      
    }# else function for letters and no formula
    
  } # second main else function, everything is under here
  
} #function close bracket



crossrange2.warnings<- function(data, params){
  
  print('===========> in crossrange2.warnings')
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data) | !params$`Cross Variable 2` %in% names(data)
      | any(!get_range_values(params$`Formula_Variable`) %in% names(data)) ){
    print(glue('Either {params$Variable}, cross variables or formula variables not present in data set, skipping this QC step') )
    return (data)
  } else{
    message("variable: ", params$`Variable`, "Cross Variable: ", params$`Cross Variable 1`, "Cross Variable 2: ", params$`Cross Variable 2`) 
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    crossvar2_symbol = sym(params$`Cross Variable 2`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = sym(paste0(comment_symbol, '.data.warning') )
    data <- data %>%
      mutate(  formula_condition_fail = !(!!parse_expr(params$Formula_Condition)),
               # NA values resulting from formula condition fail need to be set to FALSE
               formula_condition_fail = if_else(is.na(formula_condition_fail), FALSE, formula_condition_fail),
               cross_var_1_val_match = !!parse_expr(params$`Cross Variable 1 Value` ),
               cross_var_1_val_match = ifelse(is.na(cross_var_1_val_match ), FALSE, cross_var_1_val_match ),
               cross_var_2_val_match = !!parse_expr(params$`Cross Variable 2 Value` ),
               cross_var_2_val_match = ifelse(is.na(cross_var_2_val_match), FALSE, cross_var_2_val_match),
               !!comment2 := ifelse(cross_var_1_val_match & cross_var_2_val_match
                                    & formula_condition_fail,
                                    ifelse( is.na({{comment2}}), params$Comments, paste({{comment2}}, ' | ', params$Comments) ),
                                    {{comment2}} ) )
    
    return(data %>% select(-c(formula_condition_fail, cross_var_1_val_match, cross_var_2_val_match )) )
  }
}


value_check.warnings <- function(data, params){
  
  print('=======> in value_check.warnings')
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data)){
    print( glue('{params$Variable} or cross variables not present in data set, skipping this QC step') )
    return (data)
  } else {
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = paste0(comment_symbol, '.data.warning')
    
    data <- data %>% 
      mutate( !!comment2 := ifelse( !(!!variable_symbol) %in% c(777,888)
                                    & !!variable_symbol > crossvar1_symbol,
                                    ifelse(is.na(data[[comment2]]),params$Comments, paste(data[[comment2]], ' | ', params$Comments)),
                                    data[[comment2]] ) )
    return(data)
  }
}


crossrange_not.warnings <- function(data, params){
  print('=========> in crossrange_not.warnings')
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data)
      | any(!get_range_values(params$`Formula_Variable`) %in% names(data) )) {
    print(glue(' {params$Variable} or cross variables not present in data set, skipping this QC step') )
    return (data)
  } else {
    message(" variable: " , params$Variable, " Cross Variable: " , params$`Cross Variable 1`)
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = sym(paste0(comment_symbol, '.data.warning') )
    data <- data %>% mutate(  formula_condition_fail = !(!!parse_expr(params$Formula_Condition)),
                              # NA values resulting from formula condition fail need to be set to FALSE
                              formula_condition_fail = if_else(is.na(formula_condition_fail), FALSE, formula_condition_fail),
                              !!comment2 := ifelse(!(!!variable_symbol) %in% get_range_values(params$`Valid Values (Not)`)
                                                   & !(!!crossvar1_symbol %in% get_range_values(params$`Cross Variable 1 Value (Not)`))
                                                   & formula_condition_fail,
                                                   ifelse(is.na({{comment2}}),params$Comments, paste({{comment2}}, ' | ', params$Comments)),
                                                   {{comment2}}) )
    
    
    return(data %>% select(-formula_condition_fail))
  }
}


crossrange_not2.warnings <- function(data, params){
  print('=============> in cross_range_not2.warnings')
  if (!params$Variable %in% names(data) | !params$`Cross Variable 1` %in% names(data) | !params$`Cross Variable 2` %in% names(data)
      | any(!get_range_values(params$`Formula_Variable`) %in% names(data))){
    print(glue(' {params$Variable} or cross variables not present in data set, skipping this QC step') )
    return (data)
    
  } else {
    message("variable: ", params$`Variable`, " Cross Variable: ", params$`Cross Variable 1`, " Cross Variable 2: ", params$`Cross Variable 2`) 
    variable_symbol = sym(params$`Variable`)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    crossvar2_symbol = sym(params$`Cross Variable 2`)
    # creating symbol for the comment variable
    comment_symbol = sym(params$Comment_Variable)
    # creating comment column
    comment2 = sym(paste0(comment_symbol, '.data.warning') )
    
    data <- data %>% mutate(formula_condition_fail = !(!!parse_expr(params$Formula_Condition)),
                            # NA values resulting from formula condition fail need to be set to FALSE
                            formula_condition_fail = if_else(is.na(formula_condition_fail), FALSE, formula_condition_fail),
                            !!comment2 := ifelse(!(!!variable_symbol) %in% get_range_values(params$`Valid Values (Not)`)
                                                 & formula_condition_fail
                                                 & !(!!crossvar1_symbol) %in% get_range_values(params$`Cross Variable 1 Value (Not)`)
                                                 & !(!!crossvar2_symbol) %in% get_range_values(params$`Cross Variable 2 Value (Not)`),
                                                 ifelse(is.na({{comment2}}), params$Comments, paste({{comment2}}, ' | ', params$Comments)),
                                                 {{comment2}}) )
    
    
    return(data %>% select(-formula_condition_fail) )
  }
}



intdate.warnings <- function(data, params){
  print('=============> in intdate.warnings')
  
  
  if (!params$Variable %in% names(data) ){
    print(glue('{params$Variable} not in data sets, skipping this QC step '))
    return(data)
  } 
  
  if(any(is.na(data$intDate_known))){
    print(glue("{params$Variable} has at least one NA value, skipping this QC step"))
    return(data)
  }
  
  if(any(is.na(data$intYear)) | any(is.na(data$intMonth)) | any(is.na(data$intDay)) |  !params$`Cross Variable 1` %in% names(data) |
     !params$`Cross Variable 2` %in% names(data) | !params$`Cross Variable 3` %in% names(data) ){
    print(glue("intDay, intMonth and intYear either have at least one NA value or are not in data, skipping this QC step"))
    return(data)
  }
  
  
  else {
    variable_symbol = sym(params$Variable)
    crossvar1_symbol = sym(params$`Cross Variable 1`)
    crossvar2_symbol = sym(params$`Cross Variable 2`)
    crossvar3_symbol = sym(params$`Cross Variable 3`)
    #creating comment column
    comment2 =  paste0(sym(params$Comment_Variable), '.data.warning')
    
    if (any(data$intDate_known == "Y") == TRUE ){
      
      if(any(is.na(data$intYear) )| !params$`Cross Variable 1` %in% names(data)){
        
        print(glue("intYear either has at least one NA value or is not in data, skipping this QC step"))
        return(data)
      }
      else{
        data <- data %>% mutate(!!comment2 := ifelse( !!variable_symbol != !!crossvar1_symbol 
                                                      & !(!!variable_symbol) %in% c(777,888, 8000)
                                                      & !!crossvar1_symbol != 888,
                                                      ifelse(is.na(data[[comment2]]), params$Comments, paste(data[[comment2]], ' | ', params$Comments)),
                                                      data[[comment2]] ))
        return(data)
        
      }
    }
    if (any(data$intDate_known == "MY" ) == TRUE)  {
      if(any(is.na(data$intYear)) | any(is.na(data$intMonth)) | !params$`Cross Variable 1` %in% names(data) |
         !params$`Cross Variable 2` %in% names(data) ) {
        print(glue("intYear or intMonth either have at least one NA value or are not in data, skipping this QC step"))
        return(data)
        
      }
      else{
        
        data <- data %>% mutate(!!comment2 := ifelse( year(lubridate::my(!!variable_symbol) ) != !!crossvar1_symbol
                                                      &!(year(lubridate::my(!!variable_symbol))) %in% c(777,888, 8000)
                                                      & !(!!crossvar1_symbol) %in% c(888, 8000)
                                                      | month(lubridate::my(!!variable_symbol) ) != !!crossvar2_symbol
                                                      &!(month(lubridate::my(!!variable_symbol))) %in% c(888)
                                                      & !!crossvar2_symbol != 888,
                                                      ifelse(is.na(data[[comment2]]), params$Comments, paste(data[[comment2]], ' | ', params$Comments)),
                                                      data[[comment2]] ))
        return(data)
        
        
      }
    }
    
    if (any(data$intDate_known == "DMY" ) == TRUE){
      
      if(any(is.na(data$intYear)) | any(is.na(data$intMonth)) | any(is.na(data$intDay)) |  !params$`Cross Variable 1` %in% names(data) |
         !params$`Cross Variable 2` %in% names(data) | !params$`Cross Variable 3` %in% names(data) ){
        print(glue("intDay, intMonth and intYear either have at least one NA value or are not in data, skipping this QC step"))
        return(data)
      } 
      else{
        
        
        data <- data %>% mutate(!!comment2 := ifelse( year(lubridate::dmy(!!variable_symbol) ) != !!crossvar1_symbol
                                                      &!(year(lubridate::dmy(!!variable_symbol))) %in% c(888, 8000)
                                                      & !(!!crossvar1_symbol) %in% c(888, 8000)
                                                      | month(lubridate::dmy(!!variable_symbol) ) != !!crossvar2_symbol
                                                      &!(month(lubridate::dmy(!!variable_symbol))) %in% c(888)
                                                      & !!crossvar2_symbol != 888
                                                      | day(lubridate::dmy(!!variable_symbol) ) != !!crossvar3_symbol
                                                      &!(day(lubridate::dmy(!!variable_symbol))) %in% c(888)
                                                      & !!crossvar3_symbol != 888,
                                                      ifelse(is.na(data[[comment2]]), params$Comments, paste(data[[comment2]], ' | ', params$Comments)),
                                                      data[[comment2]] ))
        return(data)
        
      }
    }
    
    
  }
  
}



# run one type of QC Type (Range, crossvalid,...)
# the rules are all the rules FOR THIS QC_TYPE
# the data is all the data.
# returns a subset of the data that is NOT valid according to the 
# qa rules.
run_qa_type.warnings<-function(data,params){
  rlang::exec(params$`QC Type`,data=data,params=params)
}

#' warnings_qc
#'
#' @param rules A tibble defining the how to validiate variables 
#' @param data  The data being validated
#'
#' @return 
#'

warnings_qc <- function(params, data){
  
  orig<-options('dplyr.summarise.inform'=FALSE)
  
  # in case this program is run multiple times on the same data
  # existing flag columns will be removed first
  data <- data %>% select(-ends_with("data.warning"))
  data <- data %>% select(-starts_with("Comment"))
  # create empty comment columns for every variable
  data[paste0(names(data),'.data.warning')] <- NA
  data <- data %>% select(-ends_with('.data.change.data.warning'))
  #data <- data %>% mutate_if(is.logical, as.character)
  
  res <- params %>% pmap(~list(...)) %>% 
    reduce(run_qa_type.warnings,.init=data) %>%
    arrange.warning.comments() %>% # arranging comment columns next to the corresponding variable
    arrange.all.comments() 
  
  #creating a list of warning column names that are empty because that value did not have any flags
  empty_warning_columns <- res %>% select_if(function(x){all(is.na(x))}) %>% select(ends_with("data.warning")) %>% colnames()
  # removing the list of column names from the data
  res <- res %>% select(-all_of(empty_warning_columns))
  options(orig)
  #filtering rows where at least one comment has been added either for a change or flag
  res <- res  %>% filter(!if_all(names(res %>% select(ends_with("data.change"),ends_with("data.warning"))), is.na) ) 
  
  # adding extra columns for studies to make comments on
  res <-res %>% reduce(.x = res %>% select(ends_with("data.change"), ends_with("data.warning"))  %>% colnames(),
                       .f = ~add_column(.x, "Comments from Study for {.y[1]}" := NA, .after = .y[1], .name_repair = "minimal"),
                       .init = res)
  res
  
}


#################################################################################################################
################################################################################################
########## PLEASE RUN THE CODE BELOW IF YOU ARE NOT USING BOX TO STORE DATA #################################

##############################################################
#set working directory
# enter the entire path name for the folder that contains the data here 
#within the quotation marks
setwd("")

# enter the path where the QC report should be outputted
# within the quotation marks
path_to_output <- ""

# Reading core data dictionary
data.dict <- read_csv("./Data Dictionary/BCRP_DataDictionary.csv")

core.data.dict <- data.dict %>% 
  filter(Category == "Core")  

# Reading incident data dictionary
incident.data.dict <- data.dict %>% 
  filter(Category == "Incident Breast Cancer")

  
## CORE DATA QC
# Reading core data
# put the name of the data file inside the quotation marks
#include the file extension as well
core.data <- read.csv("")
#core.data <- read_excel("")


# making sure core data variable name cases match cases in BCRPP data dictionary
core.data <- change_case_match(data_dict = core.data.dict,
                               df = core.data)

# reading Core correction rules 
bcrpp.core.correction.rules <- read_excel("./Core QC Rules/BCRPP Correction Rules.xlsx")

# apply bcrpp correction rules to data 
core.data.changes <- changes_qc(rules = bcrpp.core.correction.rules,
                                data = core.data)

# reading Core warning rules
bcrpp.core.warning.rules <- read_excel("./Core QC Rules/BCRPP Warning Rules.xlsx")



# applying warning rules to data that already has changes made to core data
core.data.qc <- warnings_qc(params =bcrpp.core.warning.rules,
                            data = core.data)


# MAKING REPORT 

# Populate "study_name" with name of Study
core_summary_report(core.dict = core.data.dict, qc_df = core.data.qc, study_name = "")

### INCIDENT CASES QC 
# BEING DEVELOPED

#incident.data <- read.csv()

#bcrpp.incident.correction.rules <- read_excel("./Incident QC Rules/BCRPP Incident Core Changes.xlsx")

#bcrpp.incident.warning.rules <- read_excel("./Incident QC Rules/BCRPP Incident Core Warnings.xlsx")



#################################################################################################################
################################################################################################
########## PLEASE RUN THE CODE BELOW IF YOU ARE USING BOX TO STORE DATA #################################

##############################################################

core.data.dict <- box_read(869084480019) %>% filter(Category == "Core")

# reading correction rules from Box
bcrpp.core.correction.rules <- box_read(996117218706)

# reading warning rules from Box
bcrpp.core.warning.rules <- box_read(996103073482)


# apply bcrpp correction rules to data 
core.data.changes <- changes_qc(rules = bcrpp.core.correction.rules,
                                data = core.data)


# applying warning rules to data that already has changes made to core data
core.data.qc <- warnings_qc(params =bcrpp.core.warning.rules,
                            data = core.data)

# MAKING REPORT 

# Populate "study_name" with name of Study
core_summary_report(core.dict = core.data.dict, qc_df = core.data.qc, study_name = "")

