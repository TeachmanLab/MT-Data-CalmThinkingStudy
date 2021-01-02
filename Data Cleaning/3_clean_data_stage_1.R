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
  
  selected_columns <- grep(grep_pattern, df_colnames)
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

cat("Imported tables: ")
names(data)

# ---------------------------------------------------------------------------- #
# Part I. Database-Wide Data Cleaning ----
# ---------------------------------------------------------------------------- #

# The following code sections apply to data from every study in the "calm" SQL 
# database (i.e., R01, TET, GIDI).

# ---------------------------------------------------------------------------- #
# Remove irrelevant tables ----
# ---------------------------------------------------------------------------- #

# The following tables are vestiges of earlier studies and not used in the R01,
# TET, or GIDI studies and contain no data. They can be removed.

unused_tables <- c("coach_log", "data", "media", "missing_data_log", "stimuli", 
                  "trial", "verification_code")

# The "evaluation_how_learn" table was not used in the R01, TET, or GIDI studies
# because its "how_learn" item was moved to the demographics measure before the
# R01 study launch. The item is called "ptp_reason" in the "demographics" table.
# The "evaluation_how_learn" table contains no data and can be removed.

unused_tables <- c(unused_tables, "evaluation_how_learn")

# The following tables are used internally by the MindTrails system and contain
# no information relevant to individuals' participation in the R01, TET, or GIDI
# studies. Although they have data, they can be removed.

system_tables <- c("export_log", "id_gen", "import_log", "password_token",
                   "random_condition", "visit")

# TODO: DECIDE WHETHER TO RETAIN "js_psych_trial", WHICH CONTAINs INFORMATION 
# ABOUT USER ACTIVITY DURING RR MEASURE. DAN SAID RETAINING "js_psych_trial" MAY
# CAUSE CONFUSION BECAUSE WE SWITCHED TO "angular_training" FOR RR USER ACTIVITY
# DATA IN 7/2020 (6 R01 PARTIICPNATS HAVE RR DATA THERE). WILL ASK REST OF TEAM
# FOR INPUT ABOUT WHETHER TO RETAIN THIS TABLE WITH CLEAR DOCUMENTATION IN CODE
# BOOK THAT ITS DATA IS NOT COMPARABLE TO RR USER DATA IN "angular_training" OR
# TO REMOVE THIS TABLE (AND RR USER DATA IN "angular_training" FROM PIPELINE).










# Remove tables

data <- data[!(names(data) %in% c(unused_tables, system_tables))]

# TODO: ONCE THIS SECTION IS COMPLETE, CONSIDER EXPORTING AN R OBJECT WITH THE
# RESULTING TABLES AND LOADING IT BACK AGAIN TO SAVE TIME









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
# and to rename "id" in "study" table to "study_id".

rename_id_columns <- function(data) {
  data$participant <- data$participant %>% select(participant_id = id,
                                                  everything())
  data$study <- data$study %>% select(study_id = id, everything())
  return(data)
}

# Run function

data <- rename_id_columns(data)

# ---------------------------------------------------------------------------- #
# Add participant_id to all participant-specific tables ----
# ---------------------------------------------------------------------------- #

# Use function "identify_columns" (defined above) to identify columns containing 
# "id" in each table

lapply(data, identify_columns, grep_pattern = "id")

# Add participant_id to "study" and "task_log" tables. These are participant-
# specific tables but are currently indexed by study_id, not participant_id.

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

# Add "participant_id" to support tables, which are currently indexed by the 
# "id" column of the main table they support. First, for each main table,
# select its "participant_id" and "id" columns and list its support tables.

participant_id_demographics_id_match <- 
  select(data$demographics, participant_id, id)

demographics_support_table <- "demographics_race"

participant_id_evaluation_id_match <- 
  select(data$evaluation, participant_id, id)

evaluation_support_tables <- c("evaluation_coach_help_topics",
                               "evaluation_devices",
                               "evaluation_how_learn",
                               "evaluation_places",
                               "evaluation_preferred_platform",
                               "evaluation_reasons_control")

participant_id_mental_health_history_id_match <- 
  select(data$mental_health_history, participant_id, id)

mental_health_history_support_tables <- c("mental_health_change_help",
                                          "mental_health_disorders",
                                          "mental_health_help",
                                          "mental_health_why_no_help")

participant_id_reasons_for_ending_id_match <- 
  select(data$reasons_for_ending, participant_id, id)

reasons_for_ending_support_tables <- c("reasons_for_ending_change_med",
                                       "reasons_for_ending_device_use",
                                       "reasons_for_ending_location",
                                       "reasons_for_ending_reasons")

participant_id_session_review_id_match <- 
  select(data$session_review, participant_id, id)

session_review_support_table <- "session_review_distractions"

# Now define a function that uses the selected "participant_id" and "id" 
# columns from each main table and the list of the main table's support 
# tables to add "participant_id" to each support table based on the "id"

add_participant_id <- function(data, id_match, support_tables) {
  output <- vector("list", length(data))
  
  for (i in 1:length(data)) {
    if (names(data)[[i]] %in% support_tables) {
      output[[i]] <- merge(data[[i]], id_match, by = "id", all.x = TRUE)
    } else {
      output[[i]] <- data[[i]]
    }
  }
  
  names(output) <- names(data)
  return(output)
}

# Run the function for each set of support tables

data <- add_participant_id(data = data,
                           id_match = participant_id_demographics_id_match,
                           support_tables = demographics_support_table)

data <- add_participant_id(data = data,
                           id_match = participant_id_evaluation_id_match,
                           support_tables = evaluation_support_tables)

data <- add_participant_id(data = data,
                           id_match = participant_id_mental_health_history_id_match,
                           support_tables = mental_health_history_support_tables)

data <- add_participant_id(data = data,
                           id_match = participant_id_reasons_for_ending_id_match,
                           support_tables = reasons_for_ending_support_tables)

data <- add_participant_id(data = data,
                           id_match = participant_id_session_review_id_match,
                           support_tables = session_review_support_table)

# ---------------------------------------------------------------------------- #
# Remove admin and test accounts ----
# ---------------------------------------------------------------------------- #

# Identify participant_ids that are not admin or test accounts

admin_test_account_ids <- 
  data$participant[data$participant$admin == 1 |
                     data$participant$test_account == 1, ]$participant_id

# Define function that removes in each table rows indexed by participant_ids of 
# admin and test accounts

remove_admin_test_accounts <- function(data, admin_test_account_ids) {
  output <- vector("list", length(data))
  
  for (i in 1:length(data)) {
    if ("participant_id" %in% colnames(data[[i]])) {
      output[[i]] <- subset(data[[i]], 
                          !(participant_id %in% admin_test_account_ids))
    } else {
      output[[i]] <- data[[i]]
    }
  }
  
  names(output) <- names(data)
  return(output)
}

# Run function

data <- remove_admin_test_accounts(data, admin_test_account_ids)

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
# Check for repeated columns across tables ----
# ---------------------------------------------------------------------------- #

# Define function that identifies column names that are repeated across tables

find_repeated_column_names <- function(data, ignored_columns) {

  for (i in 1:length(data)) {
    for (j in 1:length(data[[i]])) {
      if (!(names(data[[i]][j]) %in% ignored_columns)) {
        for (k in 1:length(data)) {
          if ((i != k) &
              names(data[[i]][j]) %in% names(data[[k]])) {
            print(paste0(names(data[i]), ": ", names(data[[i]][j]),
                         "     is also in     ", names(data[k])))
          }
        }
      }
    }
  }
}

# Run function

ignored_columns <- c("participant_id", "study_id", "X", "id", "date", "session", 
                     "tag", "time_on_page", "date_sent")

find_repeated_column_names(data, ignored_columns)

# TODO: REVIEW THE OUTPUT ABOVE AND ENSURE ANY SHARED COLUMN NAMES THAT ARE
# EXPECTED TO HAVE THE SAME VALUES DO INDEED HAVE THE SAME VALUES FOR A GIVEN
# PARTICIPANT_ID (e.g., "receive_gift_cards" appears in "participant"
# and "study" tables but has different values).










# ---------------------------------------------------------------------------- #
# Correct study extensions ----
# ---------------------------------------------------------------------------- #

# Participants 2004 and 2005 enrolled in R01 study and were assigned to R01 
# condition but were given a TET study extension. This was due to a bug at 
# launch of the TET study. According to Dan Funk, the "study_extension" field
# was not properly being passed through to the Data Server. This was fixed on 
# 4/7/2020, but the "study_extension" for these participants needs to be 
# changed back to "".

specialIDs <- c(2004, 2005)

if (all(data$study[data$study$participant_id %in% 
                   specialIDs, ]$study_extension == "")) {
  print("Study extension for special IDs already corrected in server.")
} else {
  data$study[data$study$participant_id %in%
             specialIDs, ]$study_extension <- ""
}

# ---------------------------------------------------------------------------- #
# Arrange columns and sort tables ----
# ---------------------------------------------------------------------------- #

# TODO: MAKE BASIC COLUMNS SPECIFIC AND SORT TABLES IN CONSISTENT WAY. CONSIDER
# STARTING WITH PARTICIPANT_ID, STUDY_ID (IF PRESENT), AND THEN ID (IF PRESENT).
# ALSO CONSIDER REMOVING X. SKIP THIS TASK FOR NOW AS COLUMNS MAY CHANGE BELOW.










# ---------------------------------------------------------------------------- #
# Part II. Filter Data for Desired Study ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Filter data from "participant" and "study" tables ----
# ---------------------------------------------------------------------------- #

# TODO: SEE IF SONIA ACTUALLY WANTS THE TABLES MERGED FOR REFERENCE. IF NOT,
# THEN JUST EXTRACT THE PARTICIPANT_IDS AND GET RID OF THE MERGED TABLE.










# Define function to merge "participant" and "study" tables for easy reference 
# and filter data for participants in the desired study. The second argument 
# of the function ("study_name") can be "R01", "TET", or "GIDI".

filter_participant_study_data <- function(data, study_name) {
  participant_table <- data$participant
  study_table <- data$study
  participant_study_tables <- inner_join(participant_table, 
                                         study_table, 
                                         by = c("participant_id", "study_id"))
  if (study_name == "R01") {
    tmp <- filter(participant_study_tables, study_extension == "")
  } else if (study_name == "TET") {
    tmp <- filter(participant_study_tables, study_extension == "TET")
  } else if (study_name == "GIDI") {
    tmp <- filter(participant_study_tables, study_extension == "GIDI")
  }
}

# Run function for desired study

study_name <- "R01"
participant_study_data <- filter_participant_study_data(data, study_name)

# Identify participant_ids in study

participant_ids <- participant_study_data$participant_id

# Report number of participants enrolled in study

cat("Number of participants enrolled in", study_name, "study:", 
    length(participant_ids))

# ---------------------------------------------------------------------------- #
# Filter all data ----
# ---------------------------------------------------------------------------- #

# TODO: IF SONIA DOES NOT ACTUALLY CARE ABOUT MERGED TABLE ABOVE, THEN REMOVE
# THE SECTION ABOVE AND GET RELEVANT PARTICIPANT_IDS IN THIS FUNCTION DIRECTLY










# Define function to filter all data for participants in the desired study
# based on participant_ids obtained above

filter_all_data <- function(data, participant_ids) {
  output <- vector("list", length(data))
  
  for (i in 1:length(data)) {
    if ("participant_id" %in% colnames(data[[i]])) {
      output[[i]] <- subset(data[[i]], participant_id %in% participant_ids)
    } else {
      output[[i]] <- data[[i]]
    }
  }
  
  names(output) <- names(data)
  return(output)
}

# Run function

data <- filter_all_data(data, participant_ids)

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
