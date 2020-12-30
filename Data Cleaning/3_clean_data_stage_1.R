# ---------------------------------------------------------------------------- #
# Clean Data (Stage 1)
# Authors: Sonia Baee and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running this script, set your working directory and put the raw data
# obtained from 1_get_raw_data.ipynb in a folder called "data/raw". The present 
# script will import the raw data from that folder and output intermediate data
# in a new folder called "data/intermediate/stage_1_cleaning".

# ---------------------------------------------------------------------------- #
# Store working directory and load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load packages

library(dplyr)
library(reshape2)
library(plyr)
library(data.table)
library(lubridate)
library(anytime)

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to identify columns matching a grep pattern in a data frame.
# When used with lapply, function can be applied to all data frames in a list.

identify_columns <- function(df, grep_pattern) {
  df_colnames <- colnames(df)
  
  selected_columns <- grep("id", df_colnames)
  if (length(selected_columns) != 0) {
    df_colnames[selected_columns]
  }
}

# ---------------------------------------------------------------------------- #
# Import raw data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of raw CSV data files

raw_data_dir <- paste0(wd_dir, "/data/raw")
filenames <- list.files(raw_data_dir, pattern = "*.csv", full.names = FALSE)

# Import data files and store them in a list

data <- lapply(paste0(raw_data_dir, "/", filenames), read.csv)

# Name each data file in the list

split_char <- "-"
names(data) <- unlist(lapply(filenames, 
                             function(f) {
                               unlist(strsplit(f, 
                                               split = split_char, 
                                               fixed = FALSE))[1]
                             }
                             ))

# Report the names of the imported tables

cat("The following tables were imported: ")
names(data)

# ---------------------------------------------------------------------------- #
# Part I. Database-Wide Data Cleaning ----
# ---------------------------------------------------------------------------- #

# The following code sections apply to data from every study in the "calm" SQL 
# database (i.e., R01, TET, GIDI).

# ---------------------------------------------------------------------------- #
# Remove irrelevant tables ----
# ---------------------------------------------------------------------------- #

# TODO: CONSIDER REMOVING TABLES THAT ARE (A) BLANK ("coach_log", "data", 
# "evaluation_how_learn", "media", "missing_data_log", "stimuli", "trial", 
# and "verification_code") OR (B) IRRELEVANT TO R01, TET, OR GIDI (MAYBE
# "visit". "random_condition" SEEMS TO APPLY ONLY TO TET, IN WHICH CASE IT
# CAN BE REMOVED DURING R01-SPECIFIC DATA CLEANING BELOW. ASKED DAN ABOUT
# BLANK AND IRRELEVANT TABLES ON 12/30/20.










# ---------------------------------------------------------------------------- #
# Rename "id" columns in "participant" and "study" tables ----
# ---------------------------------------------------------------------------- #

# Except where noted below, in the "calm" database each table has an "id" 
# column that identifies the rows in that table. By convention, when a table 
# contains a column that corresponds to the "id" column of another table, the 
# derived column's name starts with the name of the table whose "id" column it 
# refers to and ends with "id". For example, "participant_id" refers to "id" in 
# the "participant" table, and "study_id" refers to "id" in the "study" table.

# Each participant has only one "id" in the "participant" table and only one 
# "id" in the "study" table, but these ids are not always the same. To make 
# indexing tables by participant simpler, we rename "id" in the "participant" 
# table to "participant_id" and rename "id" in the "study" table to "study_id". 
# We treat "participant_id" as the primary identifier for each participant;
# once a table is indexed by "participant_id", "study_id" is superfluous.

# The exception to the naming convention above is that for measures that have
# multiple tables (i.e., one main table and one or more companion tables that
# contain responses to items in which multiple response options were possible),
# the "id" variable in the companion table corresponds to the "id" variable in
# the main table (but is not named "main_table_id" as would be expected by the
# convention). For example, the "id" column in the "demographics_race" table
# corresponds to the "id" column in the "demographics" table.

# Define function to rename "id" in "participant" table to "participant_id"
# and to rename "id" in "study" table to "study_id". The function is named
# standardize_columns_special_tables because its scope is limited to only
# the specific tables that are specified in the function.

# TODO: CONSIDER RENAMING THIS FUNCTION RENAME_ID_COLUMNS

standardize_columns_special_tables <- function(data) {
  participant_table <- grep("^participant", names(data))
  data[[names(data)[participant_table][1]]] <-
    data[[names(data)[participant_table][1]]] %>% select(participant_id = id,
                                                         everything())
  
  study_table <- grep("^study", names(data))
  data[[names(data)[study_table][1]]] <-
    data[[names(data)[study_table][1]]] %>% select(study_id = id,
                                                   everything())
  
  return(data)
}

# Run function

data <- standardize_columns_special_tables(data)

# ---------------------------------------------------------------------------- #
# Add participant_id to all participant-specific tables ----
# ---------------------------------------------------------------------------- #

# Use function "identify_columns" (defined above) to identify columns containing 
# "id" in each table

lapply(data, identify_columns, grep_pattern = "id")

# Add participant_id to "study" and "task_log" tables. These are the only tables
# that are participant specific but currently indexed by study_id instead of by
# participant_id.

participant_id_study_id_match <- 
  select(data$participant, participant_id, study_id)

data$study <- merge(data$study,
                    participant_id_study_id_match,
                    by = "study_id", 
                    all.x = TRUE)

data$task_log <- merge(data$task_log,
                       participant_id_study_id_match,
                       by = "study_id", 
                       all.x = TRUE)

# TODO: NEED TO CHECK WHAT IMPORT_LOG, RANDOM_CONDITION, AND VISIT TABLES ARE
# AND WHETHER THEY ARE ALSO PARTICIPANT SPECIFIC ASKED DAN ABOUT THESE TABLES
# ON 12/30/20.










# ---------------------------------------------------------------------------- #
# Remove admin and test accounts ----
# ---------------------------------------------------------------------------- #

# TODO: CHECK BELOW










data$participant <- filter(data$participant, test_account == 0 & admin == 0)

# ---------------------------------------------------------------------------- #
# Identify and recode date columns ----
# ---------------------------------------------------------------------------- #

# TODO: ADD EXPLANATION ONCE APPROACH IS FINALIZED











# Use function "identify_columns" (defined above) to identify columns containing 
# "date" in each table

lapply(data, identify_columns, grep_pattern = "date")

# View structure of columns containing "date" in each table

view_date_str <- function(df, df_name) {
  print(paste0("Table: ", df_name))
  cat("\n")
  
  df_colnames <- colnames(df)
  date_columns <- grep("date", df_colnames)
  
  if (length(date_columns) != 0) {
    for (i in date_columns) {
      print(paste0(df_colnames[i]))
      str(df[, i])
      print(paste0("Number NA: ", sum(is.na(df[, i]))))
      print(paste0("Number blank: ", sum(df[, i] == "")))
      print(paste0("Number 555: ", sum(df[, i] == 555, na.rm = TRUE)))
      print("Number of characters: ")
      print(table(nchar(df[, i])))
    }
  } else {
    print('No columns containing "date" found.')
  }
  
  cat("----------")
  cat("\n")
}

invisible(mapply(view_date_str, df = data, df_name = names(data)))

# Consider recoding columns containing "date"

# TODO: THERE ARE SEVERAL ISSUES WITH THE FUNCTION recode_date BELOW. JEREMY
# TO RETURN TO THESE ISSUES AFTER REVIEWING THE REST OF THE SCRIPT (SEE FILE
# "2020.12.28 Checking Date Variables.txt" FOR MORE DETAILS ON THESE ISSUES).

# 1. THE FUNCTION recode_date BELOW ONLY APPLIES TO VARIABLES THAT START WITH
# "date". THERE ARE OTHER DATE VARIABLES THAT DO NOT START WITH "date" BUT DO
# CONTAIN THE WORD "date" (SEE FUNCTION identify_date_columns ABOVE). HOWEVER,
# NOTE THAT SOME VARIABLES (I.E., THOSE IN THE COVID19 TABLE DO NOT APPEAR TO
# BE TIMESTAMPS; RATHER, THEY APPEAR TO BE USER-SPECIFIED DATES); TAKE CARE IN
# DECIDING WHETTHER AND HOW THESE VARIABLES ARE RECODED (IT MAY BE
# APPROPRIATE TO RECODE THEM AS THOUGH THEY ARE TIMESTAMPS).
# 2. IN TABLES THAT HAVE MULTPLE COLUMNS CONTAINING "DATE" (SEE FUNCTION
# identify_date_columns ABOVE), THE FUNCTION recode_date BELOW CREATES ONLY
# ONE VARIABLE CALLED dateTime. THUS, NOT ALL DATE VARIABLES ARE RECODED.
# INSTEAD, THE FUNCTION recode_date BELOW SHOULD RECODE EACH DATE VARIABLE IN 
# A WAY THAT RETAINS THE ORIGINAL VARIABLE NAMES.
# 3. IN VARIABLES THAT HAVE SOME BLANKS (I.E., VALUES OF ""; SEE FUNCTION
# view_date_str ABOVE), THE ANYTIME FUNCTION IN THE FUNCTION recode_date BELOW 
# SOMEHOW GENERATES DATES/TIMES FOR THE BLANKS, RATHER THAN LEAVING THEM BLANK.
# IT ALSO CHANGES THE VALUES OF THE DATES/TIMES IN THE ORIGINAL VARIABLE. THUS,
# THE DATA ARE CHANGED IN A WAY THAT MAY BE INACCURATE.
# 4. IN VARIABLES THAT HAVE SOME VALUES OF 555 (SEE FUNCTION view_date_str
# ABOVE), THE ANYTIME FUNCTION IN THE FUNCTION recode_date BELOW CHANGES THE
# VALUES TO NA. THIS IS NOT A BIG DEAL, BUT IT WOULD BE BETTER TO LEAVE THE
# VALUES AS 555 SO THAT THEY CAN BE EXPLICITLY RECODED TO NA ELSEWHERE. THIS
# WAY VALUES ARE NOT RECODED WITHOUT US BEING EXPLICITLY AWARE OF IT.
# 5. THE TIMEZONE USED BY THE ANYTIME FUNCTION NEEDS TO BE CHECKED. DAN TOLD
# JEREMY ON 12/28/2020 THAT ALL DATES IN THE DATABASE ARE IN EST.
# 6. IT'S UNCLEAR WHY WE NEED TO USE THE ANYTIME FUNCTION IN THE FIRST PLACE.

recode_date <- function(df) {
  df_colnames <- colnames(df)
  
  date_columns <- grep("^date", df_colnames)
  if (length(date_columns) != 0) {
    for (i in date_columns) {
      df <- mutate(df, dateTime = anytime(as.factor(df[[df_colnames[i]]])))
    }
  }
  
  return(df)
}

data <- lapply(data, recode_date)











# ---------------------------------------------------------------------------- #
# Identify and rename session columns ----
# ---------------------------------------------------------------------------- #

# TODO: ADD EXPLANATION ONCE APPROACH IS FINALIZED











# Use function "identify_columns" (defined above) to identify columns containing 
# "session" in each table

lapply(data, identify_columns, grep_pattern = "session")

# View structure of columns containing "session" in each table

view_session_str <- function(df, df_name) {
  print(paste0("Table: ", df_name))
  cat("\n")
  
  df_colnames <- colnames(df)
  session_columns <- grep("session", df_colnames)
  
  if (length(session_columns) != 0) {
    for (i in session_columns) {
      print(paste0(df_colnames[i]))
      str(df[, i])
      print(paste0("Number NA: ", sum(is.na(df[, i]))))
      print(paste0("Number blank: ", sum(df[, i] == "")))
      print(paste0("Number 555: ", sum(df[, i] == 555, na.rm = TRUE)))
      print("Number of characters: ")
      print(table(nchar(df[, i])))
    }
  } else {
    print('No columns containing "session" found.')
  }
  
  cat("----------")
  cat("\n")
}

invisible(mapply(view_session_str, df = data, df_name = names(data)))

# Consider renaming "session", "current_session", and "session_name" columns

# TODO: JEREMY TO CHECK THIS AFTER REVIEWING THE REST OF THE SCRIPT. UNCLEAR 
# THAT SESSION, CURRENT_SESSION, AND SESSION_NAME SHOULD BE RENAMED TO THE 
# SAME THING AS THEY SEEM TO REFER TO DIFFERENT THINGS AND THEY CAN TAKE ON
# DIFFERENT VALUES. SESSION_NAME ALSO SEEMS TO CONTAIN TWO KINDS OF INFO IN
# SOME TABLES (I.E., BOTH TIME POINT AND COMPLETION STATUS IN ACTION_LOG AND
# TASK_LOG; BOTH TIME POINT AND GIFT CARD AWARD STATUS IN GIFT_LOG). IT MAY
# BE BETTER TO STORE EACH PIECE OF INFO IN A SEPARATE VARIABLE. SEE FILE
# "2020.12.29 Checking Session Variables.txt" FOR MORE DETAILS.

rename_session <- function(df) {
  df_colnames <- colnames(df)
  
  if (("session_name" %in% df_colnames)) {
    df <- df %>% select(session = session_name, everything())
  }
  if (("current_session" %in% df_colnames)) {
    df <- df %>% select(session = current_session, everything())
  }
  
  return(df)
}

data <- lapply(data, rename_session)











# ---------------------------------------------------------------------------- #
# Correct study extensions ----
# ---------------------------------------------------------------------------- #

# TODO: CHECK BELOW










# 2004 and 2005: Enrolled in R01 study and assigned to R01 condition but 
#                given a TET study extension. This was due to a bug at 
#                launch of the TET study. According to a message by Dan 
#                Funk, the study_extension field was not properly being 
#                passed through to the Data Server. This was fixed on 
#                4/7/2020, but the study_extension for these participants
#                needs to be changed back to "".

specialIDs <- c(2004, 2005)
tmp <- filter(data$participant, participant_id %in% specialIDs)
specialIDs_study_ids <- tmp$study
if (all(data$study[data$study$study_id %in% 
                   specialIDs_study_ids, ]$study_extension == "")) {
  print("Study extension for special IDs already corrected in server.")
} else {
  data$study[data$study$study_id %in% 
               specialIDs_study_ids, ]$study_extension <- ""
}

# ---------------------------------------------------------------------------- #
# Part II. Extract Data for Desired Study ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Extract participant data ----
# ---------------------------------------------------------------------------- #

# TODO: CHECK BELOW





# Extract the participants for the given study. The second argument of the
# function is study_name, which can be "R01", "TET", or "GIDI"

get_study_participants <- function(data, study_name) {
  study <- data$study
  participant <- data$participant
  systemID_match <- select(participant, participant_id, study_id)
  merge_study_participant_tables <- left_join(study, 
                                              systemID_match, 
                                              by = "study_id")
  participant_study_tables <- inner_join(participant, 
                                         merge_study_participant_tables, 
                                         by = c("participant_id", "study_id"))
  tmp <- data.frame()
  if (study_name == "TET") {
    tmp <- filter(participant_study_tables, 
                  study_extension == "TET")
  }
  else if (study_name == "GIDI") {
    tmp <- filter(participant_study_tables, 
                  study_extension == "GIDI")
  }
  else if (study_name == "R01") {
    tmp <- filter(participant_study_tables, 
                  (study_extension != "TET") & (study_extension != "GIDI"))
  }
  return(tmp)
}

study_name <- "R01"
study_participants <- get_study_participants(data, study_name)

# Extract the participant ids of the selected study (here is "R01")

participantIDs <- study_participants$participantID

# Extract the system ids of the selected study (here is "R01")

systemIDs <- study_participants$systemID

cat("Number of participants in study", study_name, ": ", length(participantIDs))

# ---------------------------------------------------------------------------- #
# Extract all data ----
# ---------------------------------------------------------------------------- #

# TODO: CHECK BELOW










# "select_study_participants_data" function extracts the data of each table for 
# the users of the selected study

select_study_participants_data <- function(data, 
                                           participant_ids, 
                                           system_ids, 
                                           study_name = "R01") {
  tmp <- list()
  
  cnt <- 1
  for (df in data) {
    if ("systemID" %in% colnames(df)) {
      df <- subset(df, systemID %in% systemIDs)
    }
    else if ("participantID" %in% colnames(df)) {
      df <- subset(df, participantID %in% participantIDs)
    }
    tmp[[cnt]] <- df
    cnt <- cnt + 1
  }
  names(tmp) <- names(data)
  return(tmp)
}

# Restore data of each table for users of selected study in participant_data

participant_data <- 
  select_study_participants_data(data, participantIDs, systemIDs, "R01")

# ---------------------------------------------------------------------------- #
# Part III. R01-Specific Data Cleaning ----
# ---------------------------------------------------------------------------- #

# The following code sections are specific to data for the R01 study. The code
# may not be relevant to the TET and GIDI studies and will need to be revised
# for those studies as needed.

# ---------------------------------------------------------------------------- #
# Add participant information ----
# ---------------------------------------------------------------------------- #

# TODO: ADD EXPLANATION ONCE APPROACH IS FINALIZED










# Function "add_participant_info" creates helper columns that explain
# launch membership and condition assignment

add_participant_info <- function(data, study_name) {
  if (study_name == "R01") {
    
    # Create new variable describing how participants were assigned to condition
    
    data$participant$condition_assignment_method <- NA
    manual <- c(43, 45, 57, 63, 67, 71, 82, 90, 94, 96, 97, 104, 108, 120, 130, 
                131, 132, 140)
    data$participant <- 
      mutate(data$participant, 
             condition_assignment_method = ifelse(participant_id %in% manual, 
                                                  "manual", "algorithm"))
    
    # Create new variable to differentiate soft and true launch participants
    
    data$participant$launch_type <- NA
    data$participant <- 
      mutate(data$participant,
             launch_type = ifelse(participant_id >= 159, "TRUE", "SOFT"))
    
    # TODO: Add indicator coaching_account (see previous version of code)
    
  }
  return(data)
}

data <- add_participant_info(data, "R01")

# ---------------------------------------------------------------------------- #
# Exclude participants ----
# ---------------------------------------------------------------------------- #

# TODO: Exclude soft-launch participants










# Confirm that accounts for coaches have already been removed (should all be
# test accounts, which were removed above)

coaches <- c(8, 10, 41, 42, 49, 50, 54, 55, 56, 68, 74, 400, 906, 1103, 
             1107, 1111, 1112, 1772)

if (sum(data$participant$participant_id %in% coaches) != 0) {
  data$participant <-
    data$participant[!(data$participant$participant_id %in% coaches), ]
} else {
  print("Coaching accounts already removed.")
}

# ---------------------------------------------------------------------------- #
# Edit participant information: Participant spanning two studies ----
# ---------------------------------------------------------------------------- #

# TODO: CHECK BELOW










# Participant 1992 only progressed to the early preTest phase before the R01 
# study closed, but re-engaged with the program at a later point and got 
# assigned to a TET study condition, so we change their progress to what it 
# was in R01 before the switch happened.

# TODO: task_log shows that participant 1992 completed their first preTest 
# task (Credibility) on 4/15/2020, the day after TET launched on 4/14/2020.
# This means that they did not even progress to preTest before TET launched.
# Jeremy to ensure all their data after Eligibility is removed for R01 data
# cleaning. The code below does not appear to actually remove any data.

# TODO: Add an IF statement so the code below only applies to R01 data. For
# TET data the participant's information should not be changed.

update_specific_participants <- function(data) {
  tmp_study_id <- 
    data$participant[which(data$participant$participant_id == 1992), ]$study_id
  data$study[which(data$study$study_id == tmp_study_id), ]$conditioning <- "NONE"
  data$study[which(data$study$study_id == tmp_study_id), ]$session <- "preTest"
  
  return(data)
}

updated_participant_data <- update_specific_participants(updated_participant_data)

# ---------------------------------------------------------------------------- #
# Edit participant information: Inaccurate active column ----
# ---------------------------------------------------------------------------- #

# TODO: CHECK BELOW










# Update active column of some specific participants

update_active_column <- function(data) {
  
  # Participant 891, 1627, 1663, and 1852 were supposed to be labeled
  # "active = false" after a period of inactivity, but this switch did not occur 
  # in the system.
  
  selected_users <- c(891, 1627, 1663, 1852)
  data$participant[which(data$participant$participantID %in% 
                           selected_users), ]$active <- 0
  
  return(data)
}

updated_participant_data <- update_active_column(participant_data)










# TODO: JEREMY TO CHECK EVERYTHING BELOW THIS










# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# Function "remove_duplicates" shows which ids (systemID or participantID, 
# depending on the table) have duplicated values (across all data tables) and 
# returns the dataset without duplication

remove_duplicates <- function(data) {
  tmp <- list()
  cnt <- 1
  data <- participant_data
  for (name in names(data)) {
    if (name == "task_log") {
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("systemID", 
                                                                   "session", 
                                                                   "task_name", 
                                                                   "tag")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values in the table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("systemID", 
                                                                "session", 
                                                                "task_name", 
                                                                "tag")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication in the table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if (name == "participant") {
      duplicated_rows <- 
        data[[name]][(duplicated(data[[name]][, c("participantID")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- 
          data[[name]][!duplicated(data[[name]][, c("participantID")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if (name == "js_psych_trial") {
      duplicated_rows <- 
        data[[name]][(duplicated(data[[name]][, c("participantID", 
                                                  "button_pressed", 
                                                  "internal_node_id", 
                                                  "stimulus")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- 
          data[[name]][!duplicated(data[[name]][, c("participantID", 
                                                    "button_pressed", 
                                                    "internal_node_id", 
                                                    "stimulus")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if (name == "study") {
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("systemID", 
                                                                   "session")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("systemID", 
                                                                "session")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if (name == "affect") {
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", 
                                                                   "session", 
                                                                   "tag")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID", 
                                                                "session", 
                                                                "tag")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if ((name == "attrition_prediction") || (name == "error_log") || (name == "sms_log")) {
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$systemID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if (name == "angular_training") {
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", 
                                                                   "session_counter", 
                                                                   "session", 
                                                                   "button_pressed", 
                                                                   "step_title", 
                                                                   "stimulus_name")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!(duplicated(data[[name]][, c("participantID", 
                                                                 "session_counter", 
                                                                 "session", 
                                                                 "button_pressed", 
                                                                 "step_title", 
                                                                 "stimulus_name")])), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else if (name == "email_log") {
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", 
                                                                   "session", 
                                                                   "email_type", 
                                                                   "date_sent")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID", 
                                                                "session", 
                                                                "email_type", 
                                                                "date_sent")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
    else {
      duplicated_rows <- data[[name]][(duplicated(data[[name]][, c("participantID", 
                                                                   "session")])), ]
      if (dim(duplicated_rows)[1] > 0) {
        cat("There are duplicated values for table:", name)
        cat("\n")
        cat("For the following ids: ", duplicated_rows$participantID)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]][!duplicated(data[[name]][, c("participantID", 
                                                                "session")]), ]
        rownames(tmp[[cnt]]) <- 1:nrow(tmp[[cnt]])
        cnt <- cnt + 1
      } else {
        cat("No duplication for table:", name)
        cat("\n-------------------------\n")
        tmp[[cnt]] <- data[[name]]
        cnt <- cnt + 1
      }
    }
  }
  
  names(tmp) <- names(data)
  return(tmp)
}

# "participant_data_no_duplication" is a collection of tables without any duplication
# based on the id show the duplication in tables

participant_data_no_duplication <- remove_duplicates(participant_data)

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# Handle special case of DASS

# For participant who have duplication in dass21As we keep the last entry

# eligible <- filter(participant_data$dass21AS, session == "ELIGIBLE")
# eligible <- eligible[!rev(duplicated(rev(eligible[, c("participantID")]))), ]
# other <- filter(participant_data$dass21AS, session != "ELIGIBLE")
# participant_data$dass21AS <- rbind(eligible, other)

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# The range of each item in the table is stored in the data_summary

data_summary <- lapply(participant_data, summary)
for (i in 1:length(no_duplicated_data)) {
  assign(paste(paste("df", i, sep = ""), "summary", sep = "."), data_summary[[i]])
}

data_summary

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# Prefer not to answer coding for each table
# pna = -1 or 555
# This function return the participant/system Ids of the row with prefer not to
# answer value for each table

get_ids_with_pna <- function(df, pna = 555) {
  tmp_df <- df[, -which(names(df) %in% c("participantID", 
                                         "systemID", 
                                         "session"))]
  tmp_cols <- apply(tmp_df, 2, function(col) names(which(col == pna)))
  idx_list <- list()
  cnt_idx <- 1
  par_id_list <- list()
  cnt1 <- 1
  sys_id_list <- list()
  cnt2 <- 1
  for (col in tmp_cols) {
    for (idx in col) {
      idx_list[[cnt_idx]] <- idx
      cnt_idx <- cnt_idx + 1
    }
  }
  idx_list <- unlist(idx_list, recursive = FALSE)
  idx_list <- idx_list[!duplicated(idx_list)]
  if (length(idx_list) != 0) {
    if ("participantID" %in% colnames(df)) {
      for (idx in idx_list) {
        par_id_list[[cnt1]] <- df[idx, ]$participantID
        cnt1 <- cnt1 + 1
      }
      par_id_list <- unlist(par_id_list, recursive = FALSE)
      par_id_list <- par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if ("systemID" %in% colnames(df)) {
      for (idx in idx_list) {
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

ids_with_pna <- lapply(participant_data, get_ids_with_pna)
ids_with_pna

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

# This function return the participant/system Ids with null values in each table

get_ids_with_missing <- function(df) {
  tmp_df <- df[, -which(names(df) %in% c("participantID", 
                                         "systemID", 
                                         "session"))]
  tmp_cols <- apply(tmp_df, 2, function(col) names(which(is.na(col))))
  
  # "idx_list" is a list of row index that has null value
  
  idx_list <- list()
  cnt_idx <- 1
  
  # "par_id_list" is a list of participant ids with the null value
  
  par_id_list <- list()
  cnt1 <- 1
  
  # "sys_id_list" is a list of system ids with the null value
  
  sys_id_list <- list()
  cnt2 <- 1
  for (col in tmp_cols) {
    for (idx in col) {
      idx_list[[cnt_idx]] <- idx
      cnt_idx <- cnt_idx + 1
    }
  }
  idx_list <- unlist(idx_list, recursive = FALSE)
  idx_list <- idx_list[!duplicated(idx_list)]
  if (length(idx_list) != 0) {
    if ("participantID" %in% colnames(df)) {
      for (idx in idx_list) {
        par_id_list[[cnt1]] <- df[idx, ]$participantID
        cnt1 <- cnt1 + 1
      }
      par_id_list <- unlist(par_id_list, recursive = FALSE)
      par_id_list <- par_id_list[!duplicated(par_id_list)]
      return(par_id_list)
    }
    else if ("systemID" %in% colnames(df)) {
      for (idx in idx_list) {
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

ids_with_missing <- lapply(participant_data, get_ids_with_missing )
ids_with_missing

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# Create an object with the number of tasks that should be done per session

number_of_tasks <- c(2, 14, 8, 5)
names(number_of_tasks) <- c("Eligibility", "preTest", "firstSession", "secondSession")
number_of_tasks # e.g., session eligibility should have 2 different tasks
#---------------------------
# "session_task_check" function, return if the participant complete a session or 
# it is in the middle of the session

session_task_check <- function(df, session_name) {
  tmp <- ddply(df, 
               ~systemID = session, 
               summarise, 
               number_of_distinct_tasks = length(unique(task_name)))
  tmp2 <- filter(tmp, session == session_name)
  tmp2$stage <- NULL
  tmp2 <- transform(tmp2, 
                    stage = ifelse(number_of_distinct_tasks == number_of_tasks[[session_name]], 
                                   "completed", 
                                   "middle"))
  return(tmp2)
}
#---------------------------
# The second argument can be any session name of the study
# We can use this "number_of_distinct_task_for_session" variable to make sure 
# participant didn't skip any tasks

number_of_distinct_task_for_session <- 
  session_task_check(participant_data$taskLog, "preTest")
number_of_distinct_task_for_session
#---------------------------

# ---------------------------------------------------------------------------- #
# INSERT HEADING ----
# ---------------------------------------------------------------------------- #

#---------------------------
# Dropout
# Claudia was using "current_task_index". I didn't find any documentation for that!

tmp <- filter(data$taskLog, 
              task_name == "SESSION_COMPLETE" & systemID %in% participantIDs)
View(tmp)
#---------------------------
lastSessionComp <- aggregate(tmp[, c("session", "task_name")], 
                             list(tmp$systemID), 
                             tail, 
                             1)
names(lastSessionComp) <- c("systemID", "session", "task_name")
#---------------------------
participant_lastSession <- left_join(data$participant, 
                                     lastSessionComp, 
                                     by = "systemID")
participant_lastSession <- participant_lastSession[, c("participantID", 
                                                       "systemID", 
                                                       "active", 
                                                       "session", 
                                                       "last_login_date", 
                                                       "email_reminders", 
                                                       "phone_reminders")]
View(participant_lastSession)
#---------------------------
participant_lastSession$date <- as.Date(participant_lastSession$last_login_date, 
                                        format = "%Y-%m-%d %H:%M:%S")
participant_lastSession$dayDiff <- difftime(now(), 
                                            participant_lastSession$date, 
                                            units = c("days"))
#---------------------------
problematicUsers <- filter(participant_lastSession, 
                           (active == 1) & (session != "PostFollowUp") & (dayDiff > 21))
View(problematicUsers)

#---------------------------
# In the middle of fifth session

View(filter(data$participant, participantID == 412))
View(filter(data$taskLog, systemID == 412))

# Completed the fifth session but not follow-up

View(filter(data$participant, participantID == 577))
View(filter(data$taskLog, systemID == 577)) # Evaluation and assessing program are not done