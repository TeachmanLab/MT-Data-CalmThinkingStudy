# ---------------------------------------------------------------------------- #
# INSERT TITLE
# Authors: Sonia Baee and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

setwd("/Users/soniabaee/Documents/Projects/MindTrails/R01/")

library(dplyr)
library(reshape2)
library(plyr)
library(data.table)
require(lubridate)
library(anytime)

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#----------------------------------------------------------------------
raw_data_dir <- "/Users/soniabaee/Documents/Projects/MindTrails/R01/Raw-Data-analysis"
setwd(raw_data_dir)
filenames <- list.files(raw_data_dir, pattern = "*.csv", full.names = FALSE)

# If you downloaded the data from the server run this

data <- lapply(filenames, read.csv)

# If you downloaded the data from the MindTrails run this

# data <- lapply(filenames, function(i){read.csv(i, sep = "\t", header = TRUE)})

split_char <- '-'
names(data) <- unlist(lapply(filenames, function(f){unlist(strsplit(f, split = split_char, fixed = FALSE))[1]}))
#---------------------------
cat("the list of tables: ")
cat("---------------------------\n")
names(data)
cat("---------------------------\n")
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# function `standardize_colnames` enforces a standardized column naming scheme across data tables
# For instance, `participandID` replaces the columns `participant_id` and `id`
standardize_columns_special_tables <- function(data){
  
  participant_table <- grep("^participant", names(data))
  data[[names(data)[participant_table][1]]] <-
    data[[names(data)[participant_table][1]]] %>% select(systemID = study_id, participantID = id, everything())
  
  study_table <- grep("^study", names(data))
  data[[names(data)[study_table][1]]] <-
    data[[names(data)[study_table][1]]] %>% select(systemID = id, everything())
  
  taskLog_table <- grep("^task_log", names(data))
  data[[names(data)[taskLog_table][1]]] <-
    data[[names(data)[taskLog_table][1]]] %>% select(systemID = study_id, everything())
  
  return(data)
}
#---------------------------
#---------------------------
data <- standardize_columns_special_tables(data)
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
standardize_columns <- function(df){
  df_colnames <- colnames(df)
  
  if("participant_id" %in% df_colnames){
    df <- df %>% select(participantID = participant_id, everything())
  }
  
  date_columns <- grep("^date", df_colnames)
  if(length(date_columns) != 0){
    for(i in date_columns){
      df <- mutate(df, dateTime = anytime(as.factor(df[[df_colnames[i]]])))
    }
  }
  
  if(("session_name" %in% df_colnames)){
    df <- df %>% select(session = session_name, everything())
  }
  if(("current_session" %in% df_colnames)){
    df <- df %>% select(session = current_session, everything())
  }
  
  return(df)
}
#---------------------------
#---------------------------
# Standardize column names across dataset
data <- lapply(data, standardize_columns)
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# function `add_participant_info` creates helper columns that explain
#  launch membership and condition assignment
add_participant_info <- function(data, study_name){
  if(study_name == "R01"){
    
    #remove admin and test_account
    data$participant <- filter(data$participant, test_account == 0 & admin == 0)
    
    # Create new variable to differentiate soft and true launch Ps
    data$participant$launch_type <- NA
    data$participant <- mutate(data$participant, launch_type = ifelse(participantID >= 159, "TRUE", "SOFT"))
    
    # Create new variable describing how Ps were assigned to a condition
    data$participant$condition_assignment_method <- NA
    manual <- c(43, 45, 57, 63, 67, 71, 82, 90, 94, 96, 97, 104, 108, 120, 130, 131, 132, 140)
    data$participant <- mutate(data$participant, condition_assignment_method = ifelse(participantID %in% manual, "manual", "algorithm"))
    
    # # we don't need this part because we have all coaches as a test_account and we already took care of them
    # # Create a label for coaches :
    # data$participant$coach_id <- NA
    # coaches <- c(8, 10, 41, 42, 49, 50, 54, 55, 56, 68, 74, 400, 906, 1103, 1107, 1111, 1112, 1772)
    # data$participant <- mutate(data$participant, coach_id = ifelse(participantID %in% coaches, "coach", "normal"))
    
    #special IDs:
    #2004: Assigned to R01 condition but has a TET label. This was due to a bug at launch.
    #According to a message by Dan, the studyExtension field was not properly being passed through to the data server,
    #and this was fixed on 4/7/2020.2004
    
    #2005: Assigned to R01 condition but has a TET label. This was due to a bug at launch. According to Dan,
    #the studyExtension field was not properly being passed through to the data server, and this was fixed on 4/7/2020.
    
    specialIDs <- c(2004, 2005)
    tmp <- filter(data$participant, participantID %in% specialIDs)
    specialIDs_systemIDs <- tmp$systemID
    if(all(is.na(data$study[which(data$study$systemID %in% specialIDs_systemIDs), ]$study_extension))){
      print("No manual changes needed for the special IDs! We alread took care of it in the server.")
    }else{
      data$study[which(data$study$systemID %in% specialIDs_systemIDs), ]$study_extension <- ""
    }
    
  }
  return(data)
}
#---------------------------
#---------------------------
# Add helper columns
data <- add_participant_info(data, "R01")
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# Extract the participants for the given study
# the second argument of `study_ID_function` is the study name, so it can be `R01`, `TET`, or `GIDI`
get_study_participants <- function(data, study_name){
  study <- data$study
  participant <- data$participant
  systemID_match <- select(participant, participantID, systemID)
  merge_study_participant_tables <- left_join(study, systemID_match, by = "systemID")
  participant_study_tables <- inner_join(participant, merge_study_participant_tables, by = c("participantID", "systemID"))
  tmp <- data.frame()
  if(study_name == "TET"){
    tmp <- filter(participant_study_tables, study_extension == "TET")
  }
  else if(study_name == "GIDI"){
    tmp <- filter(participant_study_tables, study_extension == "GIDI")
  }
  else if(study_name == "R01"){
    tmp <- filter(participant_study_tables, (study_extension != "TET")&(study_extension != "GIDI"))
  }
  return(tmp)
}
#---------------------------
#---------------------------
study_name <- "R01"
study_participants <- get_study_participants(data, "R01")

# extract the participant ids of the selected study (here is `R01`)
participantIDs <- study_participants$participantID
# extract the system ids of the selected study (here is `R01`)
systemIDs <- study_participants$systemID

cat("number of participant in study", study_name, "is: ", length(participantIDs))
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
#`select_study_participants_data` function extracts the data of each tables for the users of the selected study
select_study_participants_data <- function(data, participant_ids, system_ids, study_name = "R01"){
  tmp <- list()
  
  cnt <- 1
  for(df in data){
    if("systemID" %in% colnames(df)){
      df <- subset(df, systemID %in% systemIDs)
    }
    else if("participantID" %in% colnames(df)){
      df <- subset(df, participantID %in% participantIDs)
    }
    tmp[[cnt]] <- df
    cnt <- cnt + 1
  }
  names(tmp) <- names(data)
  return(tmp)
}
#---------------------------
#---------------------------
#restore the data of each tables for the users of the selected study in the participant_data
participant_data <- select_study_participants_data(data, participantIDs, systemIDs, "R01")
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# Update active column of some specific participants
#---------------------------
update_active_column <- function(data){
  #Participant 891, 1627, 1663, and 1852 were supposed to be labelled
  #'active = false' after a period of inactivity, but this switch did not occur in the system.
  selected_users <- c(891, 1627, 1663, 1852)
  data$participant[which(data$participant$participantID %in% selected_users), ]$active <- 0
  
  return(data)
}
#---------------------------
#---------------------------
updated_participant_data <- update_active_column(participant_data)
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# special participants
#---------------------------
update_specific_participants <- function(data){
  
  #Participant 1992 only progressed to the early Pre-test phase before the R01 study closed,
  #but re-engaged with the program at a later point and got assigned to a TET study
  #condition, so we change their progress to what it was in R01 before the switch happened.
  #Participants 1992, 2004, and 2005 received 'studyExtension = TET' labels incorrectly as we
  #were switching over to the TET study.
  tmp_systemID <- data$participant[which(data$participant$participantID == 1992), ]$systemID
  data$study[which(data$study$systemID == tmp_systemID), ]$conditioning <- "NONE"
  data$study[which(data$study$systemID == tmp_systemID), ]$session <- "preTest"
  data$study[which(data$study$systemID == tmp_systemID), ]$study_extension <- ""
  
  # special_participant_IDs <- c(2004, 2005)
  # tmp_systemID <- data$participant[which(data$participant$participantID %in% special_participant_IDs), ]$systemID
  # data$study[which(data$study$systemID %in% tmp_systemID), ]$study_extension <- ""
  
  return(data)
  
}
#---------------------------
#---------------------------
updated_participant_data <- update_specific_participants(updated_participant_data)
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# function `remove_duplicates` shows which ids (systemID or participantID, depending on the table) have
# duplicated values (across all data tables) and returns the dataset without duplication
# return the data with the any duplication
remove_duplicates <- function(data){
  tmp <- list()
  cnt <- 1
  data <- participant_data
  for(name in names(data)){
    if(name == 'task_log'){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("systemID", "session", "task_name", "tag")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values in the table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("systemID", "session", "task_name", "tag")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication in the table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if(name == 'participant'){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if(name == 'js_psych_trial'){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", "button_pressed", "internal_node_id", "stimulus")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][,  c("participantID", "button_pressed", "internal_node_id", "stimulus")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if(name == 'study'){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("systemID", "session")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("systemID", "session")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if(name == 'affect'){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", "session", "tag")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID", "session", "tag")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if((name == 'attrition_prediction') || (name == 'error_log') || (name == 'sms_log')){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if(name == 'angular_training'){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", "session_counter", "session", "button_pressed", "step_title", "stimulus_name")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!(duplicated(data[[name]][, c("participantID", "session_counter", "session", "button_pressed", "step_title", "stimulus_name")])), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if(name == 'email_log'){
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", "session", "email_type", "date_sent")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID", "session", "email_type", "date_sent")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else{
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", "session")])), ]
      if(dim(duplicated_rows)[1] > 0){
        cat("there is duplicated values for table:", name)
        cat("\n")
        cat("for the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID", "session")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      }else{
        cat("no duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
  }
  
  names(tmp) <- names(data)
  return(tmp)
}
#---------------------------
#---------------------------
# `participant_data_no_duplication` is a collection of tables without any duplication
# based on the id show the duplication in tables
participant_data_no_duplication <- remove_duplicates(participant_data)
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# -------------- Handle special case of DASS --------------------------------
# for participant who have duplication in dass21As we keep the last entry
# eligible <- filter(participant_data$dass21AS, session == "ELIGIBLE")
# eligible <- eligible[!rev(duplicated(rev(eligible[, c("participantID")]))), ]
# other <- filter(participant_data$dass21AS, session != "ELIGIBLE")
# participant_data$dass21AS <- rbind(eligible, other)
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
#the range of each item in the table is stored in the data_summary
data_summary <- lapply(participant_data, summary)
for (i in 1:length(no_duplicated_data)){
  assign(paste(paste("df", i, sep = ""), "summary", sep = "."), data_summary[[i]])
}
data_summary
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# prefer not to answer coding for each table
# pna =  -1 or 555
# this function return the participant/system Ids of the row with prefer not to answer value for each table
get_ids_with_pna <- function(df, pna = 555){
  tmp_df <- df[, -which(names(df) %in% c("participantID", "systemID", "session"))]
  tmp_cols <- apply(tmp_df, 2, function(col) names(which(col == pna)))
  idx_list <- list()
  cnt_idx <- 1
  par_id_list <- list()
  cnt1 <- 1
  sys_id_list <- list()
  cnt2 <- 1
  for(col in tmp_cols){
    for(idx in col){
      idx_list[[cnt_idx]] <- idx
      cnt_idx <- cnt_idx + 1
    }
  }
  idx_list <- unlist(idx_list, recursive = FALSE)
  idx_list <- idx_list[!duplicated(idx_list)]
  if(length(idx_list) != 0){
    if("participantID" %in% colnames(df)){
      for(idx in idx_list){
        par_id_list[[cnt1]] <- df[idx, ]$participantID
        cnt1 <- cnt1 + 1
      }
      par_id_list <- unlist(par_id_list, recursive = FALSE)
      par_id_list <- par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if("systemID" %in% colnames(df)){
      for(idx in idx_list){
        sys_id_list[[cnt1]] <- df[idx, ]$systemID
        cnt1 <- cnt1 + 1
      }
      sys_id_list <- unlist(sys_id_list, recursive = FALSE)
      sys_id_list <- sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
    
  }
  # else{
  #   return(cat("\nNo entries with prefer not to answer = ", pna, " found!\n"))
  # }
}
#---------------------------
ids_with_pna <- lapply(participant_data, get_ids_with_pna)
ids_with_pna
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# this function return the participant/system Ids with null values in each table
get_ids_with_missing <- function(df){
  tmp_df <- df[, -which(names(df) %in% c("participantID", "systemID", "session"))]
  tmp_cols <- apply(tmp_df, 2, function(col) names(which(is.na(col))))
  # `idx_list` is a list of row' index that has null value
  idx_list <- list()
  cnt_idx <- 1
  # `par_id_list` is a list of participant ids with the null value
  par_id_list <- list()
  cnt1 <- 1
  # `sys_id_list` is a list of system ids with the null value
  sys_id_list <- list()
  cnt2 <- 1
  for(col in tmp_cols){
    for(idx in col){
      idx_list[[cnt_idx]] <- idx
      cnt_idx <- cnt_idx + 1
    }
  }
  idx_list <- unlist(idx_list, recursive = FALSE)
  idx_list <- idx_list[!duplicated(idx_list)]
  if(length(idx_list) != 0){
    if("participantID" %in% colnames(df)){
      for(idx in idx_list){
        par_id_list[[cnt1]] <- df[idx, ]$participantID
        cnt1 <- cnt1 + 1
      }
      par_id_list <- unlist(par_id_list, recursive = FALSE)
      par_id_list <- par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if("systemID" %in% colnames(df)){
      for(idx in idx_list){
        sys_id_list[[cnt1]] <- df[idx, ]$systemID
        cnt1 <- cnt1 + 1
      }
      sys_id_list <- unlist(sys_id_list, recursive = FALSE)
      sys_id_list <- sys_id_list[!duplicated(sys_id_list)]
      return(sys_id_list)
    }
    
  }
  # else{
  #   return(cat("\nNo entries with missing values found!\n"))
  # }
}
#---------------------------
ids_with_missing <- lapply(participant_data, get_ids_with_missing )
ids_with_missing
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
#create an object with the number of taks that should be done per session
number_of_tasks <- c(2, 14, 8, 5)
names(number_of_tasks) <- c("Eligibility", "preTest", "firstSession", "secondSession")
number_of_tasks #e.g., session eligibility should have 2 different task
#---------------------------
#`session_task_check` function, return if the participant complete a session or it is in the middle of the session
session_task_check <- function(df, session_name){
  tmp <- ddply(df, ~systemID = session, summarise, number_of_distinct_tasks = length(unique(task_name)))
  tmp2 <- filter(tmp, session == session_name)
  tmp2$stage <- NULL
  tmp2 <- transform(tmp2, stage = ifelse(number_of_distinct_tasks == number_of_tasks[[session_name]], "completed", "middle"))
  return(tmp2)
}
#---------------------------
# the second argument can be any session name of the study
# we can use this `number_of_distinct_task_for_session` variable to make sure, participant didn't skip any tasks
number_of_distinct_task_for_session <- session_task_check(participant_data$taskLog, "preTest")
number_of_distinct_task_for_session
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# dropout
#claudia was using 'current_task_index' which I didn't find any documentation for that!
tmp <- filter(data$taskLog, task_name == "SESSION_COMPLETE" & systemID %in% participantIDs)
View(tmp)
#---------------------------
lastSessionComp <- aggregate(tmp[, c('session', 'task_name')], list(tmp$systemID), tail, 1)
names(lastSessionComp) <- c('systemID', 'session', 'task_name')
#---------------------------
participant_lastSession <- left_join(data$participant, lastSessionComp, by = "systemID")
participant_lastSession <- participant_lastSession[, c('participantID', 'systemID', 'active', 'session', 'last_login_date', 'email_reminders', 'phone_reminders')]
View(participant_lastSession)
#---------------------------
participant_lastSession$date <- as.Date(participant_lastSession$last_login_date, format = "%Y-%m-%d %H:%M:%S")
participant_lastSession$dayDiff <- difftime(now(), participant_lastSession$date, units = c("days"))
#---------------------------
problematicUsers <- filter(participant_lastSession, (active == 1) & (session != 'PostFollowUp') & (dayDiff > 21))
View(problematicUsers)

#---------------------------
# in the middle of fifth session
View(filter(data$participant, participantID == 412))
View(filter(data$taskLog, systemID == 412))

#completed the fifth session but not followup
View(filter(data$participant, participantID == 577))
View(filter(data$taskLog, systemID == 577)) #evaluation and assessing program are not done