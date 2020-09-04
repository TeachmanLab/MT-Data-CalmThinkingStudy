library(reshape2)
library(plyr)
library(dplyr) # Needs to be loaded after dplyr
library(data.table)
require(lubridate)
library(anytime)

#----------------------------------------------------------------------
raw_data_dir = "C:/Users/anbag/code/MT-Data-CalmThinkingStudy/data"
setwd(raw_data_dir)
filenames = list.files(raw_data_dir, pattern="*.csv", full.names=FALSE)

data = lapply(filenames, read.csv) # If you downloaded the data from the server run this
# data = lapply(filenames, function(i){read.csv(i,  sep="\t", header=TRUE)}) # If you downloaded the data from the MindTrails run this

names(data) = unlist(lapply(filenames, function(f){unlist(strsplit(f,split='_', fixed=FALSE))[1]})) # Assign file names to their respective data frames in the list `data`

#---------------------------

#---------------------------
# function `standardize_colnames` enforces a standardized column naming scheme across data tables
#  For instance, `participandID` replaces the columns `participant_id` and `id`

standardize_colnames = function(df){
  df_colnames = colnames(df)
  
  if("participant_id" %in% df_colnames){
    df = df %>% select(participantID = participant_id, everything()) 
  }
  if(("study" %in% df_colnames)&("id" %in% df_colnames)){
    df = df %>% select(systemID = study, participantID = id, everything()) 
  }
  if(("study_id" %in% df_colnames)){
    df = df %>% select(systemID = study_id, everything()) 
  }
  if(("study_extension" %in% df_colnames)){
    df = df %>% select(systemID = id, everything()) 
  }
  
  if(("session_name" %in% df_colnames)){
    df = df %>% select(session = session_name, everything()) 
  }
  if(("current_session" %in% df_colnames)){
    df = df %>% select(session = current_session, everything()) 
  }

  if(("date" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$date))) 
  }
  if(("dateSent" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$dateSent))) 
  }
  if(("last_login_date" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$last_login_date))) 
  }
  if(("dateTime" %in% df_colnames)){
    df = mutate(df, dateTime = anytime(as.factor(df$last_login_date))) 
  }
  
  # ----- Leave commented out for now. Sonia uses tag col later to check for duplicates
  
  
  # Remove empty `tag` and `coronavirus` columns, if they exist
  
   
#  for (col in list("tag", "coronavirus")) {
#    if (col %in% colnames(df)) {
#      df = df[,-c(grep(col, colnames(df)))]
#    }
#  }

  return(df)
}
#---------------------------

# Standardize column names across dataset
data = lapply(data, standardize_colnames)


# -------------- Handle special case of Participant table -------------------

data$participant = data$participant %>% select(participantID = id, everything()) # Standardize participantID field
data$participant = filter(data$participant, test_account == 0 & admin == 0) # Remove admin and test accounts


# -------------- Handle special case of DASS --------------------------------
eligible <- filter(data$dass21AS, session == "ELIGIBLE")
eligible <- eligible[!rev(duplicated(rev(eligible[, c("participantID")]))),]
other <- filter(data$dass21AS, session != "ELIGIBLE")
data$dass21AS <- rbind(eligible, other)

#----------------------------------------------------------------------------
# function `add_participant_info` creates helper columns that explain
#  launch membership and condition assignment
add_participant_info = function(data, study_name){
  if(study_name == "R01"){
    
    # Create new variable to differentiate soft and true launch Ps
    data$participant$launch_type = NA
    data$participant = mutate(data$participant, launch_type = ifelse(participantID >= 159,"TRUE","SOFT"))
    
    # Create new variable describing how Ps were assigned to a condition
    data$participant$condition_assignment_method = NA
    manual = c(43,45,57,63,67,71,82,90,94,96,97,104,108,120,130,131,132,140)
    data$participant = mutate(data$participant, condition_assignment_method = ifelse(participantID %in% manual,"manual","algorithm")) 
  
  }
  return(data)
}
#---------------------------
# Add helper columns
data = add_participant_info(data, "R01")
#---------------------------


#---------------------------
# Extract the participants for the given study
get_study_participants = function(data, study_name){
  study = data$study
  participant = data$participant
  systemID_match = select(participant, participantID, systemID)
  merge_study_participant_tables = left_join(study, systemID_match, by="systemID") 
  participant_study_tables = inner_join(participant, merge_study_participant_tables, by=c("participantID","systemID")) 
  tmp = data.frame()
  if(study_name == "TET"){
    tmp = filter(participant_study_tables, study_extension == "TET")
  }
  else if(study_name == "GIDI"){
    tmp = filter(participant_study_tables, study_extension == "GIDI")
  }
  else if(study_name == "R01"){
    tmp = filter(participant_study_tables, (study_extension != "TET")&(study_extension != "GIDI"))
  }
  return(tmp)
}
#---------------------------
# The second argument of `get_study_participants` is the study name, so it can be `R01`, `TET`, or `GIDI`
# TODO: Move above comment to the function description when describing params

study_name = "R01"
study_participants = get_study_participants(data, study_name)

# Extract the participant ids of the selected study (here is `R01`)
participantIDs = study_participants$participantID

# extract the system ids of the selected study (here is `R01`)
systemIDs = study_participants$systemID
cat("number of participant in study", study_name, "is: ", length(participantIDs))
#---------------------------

#---------------------------
#`select_study_participants_data` function extracts the data of each tables for the users of the selected study
select_study_participants_data = function(data, participant_ids, system_ids, study_name = "R01"){
  tmp = list()
  
  cnt = 1
  for(df in data){
    if("systemID" %in% colnames(df)){
      df = subset(df, systemID %in% systemIDs)
    }
    else if("participantID" %in% colnames(df)){
      df = subset(df, participantID %in% participantIDs)
    } 
    tmp[[cnt]] = df
    cnt = cnt + 1
  }
  names(tmp) = names(data)
  return(tmp)
}
#---------------------------
#restore the data of each tables for the users of the selected study in the participant_data 
participant_data = select_study_participants_data(data, participantIDs, systemIDs, "R01")
#---------------------------

#---------------------------
# function `remove_duplicates` shows which ids (systemID or participantID, depending on the table) have duplicated values (across all data tables) and returns the dataset without duplication 
remove_duplicates = function(data){
  tmp = list()
  cnt = 1
  cols = list()
  for(name in names(data)){
    if(name == 'taskLog'){
      cols = list("systemID","session","task_name","tag")
    }
    else if((name == 'participant') | (name=="attritionPrediction")) {
      cols = list("participantID")
    }
    else if(name == 'study'){
      cols = list("systemID", "session")
    }
    else if(name == 'affect'){
      cols = list("participantID", "session", "tag")
    }
    else{
      cols = list("participantID", "session")
    }
    
    duplicated_rows = data[[name]][(duplicated(data[[name]][, c(unlist(cols))])),]
    if(dim(duplicated_rows)[1] > 0){
      cat("There are duplicated values in the table:", name)
      tmp[[cnt]] = data[[name]][!duplicated(data[[name]][, c(unlist(cols))]), ] 
      rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
      cnt = cnt + 1
    }else{
      cat("No duplicates in the table:", name)
      cat("\n-------------------------\n")
      tmp[[cnt]] = data[[name]]
      cnt = cnt + 1
    }
  }
  
  names(tmp) = names(data)
  return(tmp)
}
#---------------------------
# `participant_data` is a collection of tables without any duplication
participant_data = remove_duplicates(participant_data)
#---------------------------


#---------------------------
#the range of each item in the table is stored in the data_summary
data_summary = lapply(participant_data, summary)
for (i in 1:length(participant_data)){
  assign(paste(paste("df", i, sep=""), "summary", sep="."), data_summary[[i]])
}
data_summary
#---------------------------


#---------------------------
# prefer not to answer coding for each table
# pna =  -1 or 555 (default)
# this function return the participant/system Ids of the row with prefer not to answer value for each table
get_ids_with_pna = function(df, pna = 555){
  tmp_df = df[ , -which(names(df) %in% c("participantID", "systemID", "session"))]
  tmp_cols = apply(tmp_df, 2, function(col) names(which(col == pna)))
  
  # TODO: Explain these vars please
  idx_list = list()
  cnt_idx = 1
  par_id_list = list()
  cnt1 = 1
  sys_id_list = list()
  cnt2 = 1
  
  for(col in tmp_cols){
    for(idx in col){
      idx_list[[cnt_idx]] = idx
      cnt_idx = cnt_idx + 1
    }
  }
  idx_list = unlist(idx_list, recursive=FALSE)
  idx_list = idx_list[!duplicated(idx_list)]
  if(length(idx_list) != 0){
    if("participantID" %in% colnames(df)){
      for(idx in idx_list){
        par_id_list[[cnt1]] = df[idx,]$participantID
        cnt1 = cnt1 + 1
      }
      par_id_list = unlist(par_id_list, recursive=FALSE)
      par_id_list = par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if("systemID" %in% colnames(df)){
      for(idx in idx_list){
        sys_id_list[[cnt1]] = df[idx,]$systemID
        cnt1 = cnt1 + 1
      }
      sys_id_list = unlist(sys_id_list, recursive=FALSE)
      sys_id_list = sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
    
  }
  # else{
  #   return(cat("\nNo entries with prefer not to answer = ", pna, " found!\n"))
  # }
} 
#---------------------------
ids_with_pna = lapply(participant_data, get_ids_with_pna)
ids_with_pna
#---------------------------


#---------------------------
# this function return the participant/system Ids with null values in each table
get_ids_with_missing = function(df){
  tmp_df = df[ , -which(names(df) %in% c("participantID", "systemID", "session"))]
  tmp_cols = apply(tmp_df, 2, function(col) names(which(is.na(col))))
  # is.na(x)
  
  # TODO: Describe these vars
  idx_list = list()
  cnt_idx = 1
  par_id_list = list()
  cnt1 = 1
  sys_id_list = list()
  cnt2 = 1
  
  for(col in tmp_cols){
    for(idx in col){
      idx_list[[cnt_idx]] = idx
      cnt_idx = cnt_idx + 1
    }
  }
  idx_list = unlist(idx_list, recursive=FALSE)
  idx_list = idx_list[!duplicated(idx_list)]
  if(length(idx_list) != 0){
    if("participantID" %in% colnames(df)){
      for(idx in idx_list){
        par_id_list[[cnt1]] = df[idx,]$participantID
        cnt1 = cnt1 + 1
      }
      par_id_list = unlist(par_id_list, recursive=FALSE)
      par_id_list = par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if("systemID" %in% colnames(df)){
      for(idx in idx_list){
        sys_id_list[[cnt1]] = df[idx,]$systemID
        cnt1 = cnt1 + 1
      }
      sys_id_list = unlist(sys_id_list, recursive=FALSE)
      sys_id_list = sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
    
  }
  # else{
  #   return(cat("\nNo entries with missing values found!\n"))
  # }
} 
#---------------------------
ids_with_missing = lapply(participant_data, get_ids_with_missing)
ids_with_missing
#---------------------------


#---------------------------
# Create an object with the number of taks that should be done per session
number_of_tasks = c(2, 14, 8, 5) 
names(number_of_tasks) = c("eligibility", "preTest", "firstSession", "secondSession")
number_of_tasks #e.g., session eligibility should have 2 different task 
#---------------------------
# function`session_task_check` returns the participant's session status - whether they've completed a session or are in the middle of the session
session_task_check = function(df, session_name){
  tmp = ddply(df,~systemID+session,summarise,number_of_distinct_tasks=length(unique(task_name)))
  tmp2 = filter(tmp, session == session_name) 
  tmp2$stage = NULL
  tmp2 = transform(tmp2, stage = ifelse(number_of_distinct_tasks == number_of_tasks[[session_name]], "completed", "in_progress"))
  return(tmp2)
}
#---------------------------
# the second argument can be any session name of the study
# we can use this `number_of_distinct_task_for_session` variable to make sure, participant didn't skip any tasks 
number_of_distinct_task_for_session = session_task_check(participant_data$taskLog, "preTest")
number_of_distinct_task_for_session
#---------------------------


# TODO: check for the following:
# 1. Repeated sessions: Look for rows in `taskLog` df with dups of [session, systemid, taskname]
# Should we retain the most recent? -Anna
repeated_tasks = data$taskLog[duplicated(data$taskLog[,c('session','systemID', 'task_name')]),]
repeated_tasks

# 2. Repeated initial screener with diff values. If ineligible -> eligible, drop all. Else, keep most recent
# Update - no way to tell if they went from ineligible -> eligible, as I think the DASS table only retains the most recent completion record
# per session, including in eligibility. So, should just drop all p's who repeated the eligibility. - Anna
repeated_eligible = data$dass21AS[duplicated(data$dass21AS[,c('participantID', 'session')]),]

# 3. Unreasonable reported age ( where current year - data$demographics$birth_year > 105 or current year - data$demographics$birth_year < 18)
current_year = as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))
unreasonable_age <- ifelse(
  (
    data$demographics$birth_year != 555) & 
    (current_year - data$demographics$birth_year > 105 | current_year - data$demographics$birth_year < 18
  ), 
  TRUE, 
  FALSE)
unreasonable_age <- data$demographics[unreasonable_age,]


# 4. People who started in control, then did active sessions later on. Not sure how to check for this :/ 

