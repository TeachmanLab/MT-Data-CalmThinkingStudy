# ---------------------------------------------------------------------------- #
# Clean Data
# Authors: Sonia Baee and Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# TODO: Review Changes and Issues Log and consider whether other changes are
# needed





# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running this script, set your working directory. The present script 
# will import deidentified raw data obtained from "2_deidentify_data.R" (which 
# outputted redacted raw data files in the "data/raw" folder with "-redacted" 
# appended to the original file name for raw data files that needed redaction) 
# and output intermediate data in a new folder called 
# "data/intermediate/stage_1_cleaning".

# ---------------------------------------------------------------------------- #
# Store working directory, install correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Run the code below to ensure you are running the same version of R used at the
# time the script was written. If you need to install a previous version, go to
# https://cran.r-project.org/bin/windows/base/old/

script_R_version <- "R version 4.0.3 (2020-10-10)"
current_R_version <- R.Version()$version.string

if(current_R_version != script_R_version) {
  stop(paste0("This script is based on ", script_R_version,
              ". You are running ", current_R_version, "."))
}

# Load packages using "groundhog", which installs and loads the most recent
# versions of packages available on the specified date ("groundhog_day"). This 
# is important for reproducibility so that everyone running the script is using
# the same versions of packages used at the time the script was written.

# Note that packages may take longer to load the first time you load them with
# "groundhog.library". This is because you may not have the correct versions of 
# the packages installed based on the "groundhog_day". After "groundhog.library"
# automatically installs the correct versions alongside other versions you may 
# have installed, it will load the packages more quickly.

# If in the process of loading packages with "groundhog.library" for the first 
# time the console states that you first need to install "Rtools", follow steps 
# here (https://cran.r-project.org/bin/windows/Rtools/) for installing "Rtools" 
# and putting "Rtools" on the PATH. Then try loading the packages again.

library(groundhog)
meta.groundhog("2021-07-01")
groundhog_day <- "2021-01-01"

groundhog.library(data.table, groundhog_day)
groundhog.library(plyr, groundhog_day)
groundhog.library(dplyr, groundhog_day)
groundhog.library(reshape2, groundhog_day)
groundhog.library(lubridate, groundhog_day)
groundhog.library(anytime, groundhog_day)

# TODO: At the end of data cleaning figure out whether all of these packages 
# are actually used; we may be able to remove some of them.





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
# Import deidentified raw data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of CSV data files

raw_data_dir <- paste0(wd_dir, "/data/raw")
filenames <- list.files(raw_data_dir, pattern = "*.csv", full.names = FALSE)

# Import data files and store them in a list

dat <- lapply(paste0(raw_data_dir, "/", filenames), read.csv)

# Name each data file in the list

split_char <- "-"
names(dat) <- unlist(lapply(filenames, 
                            function(f) {
                              unlist(strsplit(f, 
                                              split = split_char, 
                                              fixed = FALSE))[1]
                            }
                            ))

# Report the names of the imported tables

cat("Imported tables: ")
names(dat)

# ---------------------------------------------------------------------------- #
# Part I. Database-Wide Data Cleaning ----
# ---------------------------------------------------------------------------- #

# The following code sections apply to data from every study in the "calm" SQL 
# database (i.e., Calm Thinking, TET, GIDI).

# ---------------------------------------------------------------------------- #
# Remove irrelevant tables ----
# ---------------------------------------------------------------------------- #

# The following tables are vestiges of earlier studies and not used in the Calm
# Thinking, TET, or GIDI studies and contain no data. They can be removed.

unused_tables <- c("coach_log", "data", "media", "missing_data_log", "stimuli", 
                  "trial", "verification_code")

# The "evaluation_how_learn" table was not used in the Calm Thinking, TET, or GIDI 
# studies because its "how_learn" item was moved to the demographics measure before 
# the Calm Thinking study launch. The item is called "ptp_reason" in the "demographics" 
# table. The "evaluation_how_learn" table contains no data and can be removed.

unused_tables <- c(unused_tables, "evaluation_how_learn")

# The following tables are vestiges of earlier studies and not used in the Calm
# Thinking, TET, or GIDI studies. Although they contain data, after removing admin 
# and test accounts they contain no data corresponding to a "participant_id" (the
# rows that have data have a blank "participant_id"). They can be removed.

unused_tables <- c(unused_tables, "imagery_prime", "impact_anxious_imagery")

# The following tables are used internally by the MindTrails system and contain
# no information relevant to individuals' participation in the Calm Thinking, TET, 
# or GIDI studies. Although they have data, they can be removed.

system_tables <- c("export_log", "id_gen", "import_log", "password_token",
                   "random_condition", "visit")

# Remove tables

dat <- dat[!(names(dat) %in% c(unused_tables, system_tables))]

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
  dat$participant <- dat$participant %>% select(participant_id = id,
                                                everything())
  dat$study <- dat$study %>% select(study_id = id, everything())
  return(dat)
}

# Run function

dat <- rename_id_columns(dat)

# ---------------------------------------------------------------------------- #
# Add participant_id to all participant-specific tables ----
# ---------------------------------------------------------------------------- #

# Use function "identify_columns" (defined above) to identify columns containing 
# "id" in each table

lapply(dat, identify_columns, grep_pattern = "id")

# Add participant_id to "study" and "task_log" tables. These are participant-
# specific tables but are currently indexed by study_id, not participant_id.

participant_id_study_id_match <- 
  select(dat$participant, participant_id, study_id)

dat$study <- merge(dat$study,
                   participant_id_study_id_match,
                   by = "study_id", 
                   all.x = TRUE)

dat$task_log <- merge(dat$task_log,
                      participant_id_study_id_match,
                      by = "study_id", 
                      all.x = TRUE)

# Add "participant_id" to support tables, which are currently indexed by the 
# "id" column of the main table they support. First, for each main table,
# select its "participant_id" and "id" columns and list its support tables.

participant_id_demographics_id_match <- 
  select(dat$demographics, participant_id, id)

demographics_support_table <- "demographics_race"

participant_id_evaluation_id_match <- 
  select(dat$evaluation, participant_id, id)

evaluation_support_tables <- c("evaluation_coach_help_topics",
                               "evaluation_devices",
                               "evaluation_how_learn",
                               "evaluation_places",
                               "evaluation_preferred_platform",
                               "evaluation_reasons_control")

participant_id_mental_health_history_id_match <- 
  select(dat$mental_health_history, participant_id, id)

mental_health_history_support_tables <- c("mental_health_change_help",
                                          "mental_health_disorders",
                                          "mental_health_help",
                                          "mental_health_why_no_help")

participant_id_reasons_for_ending_id_match <- 
  select(dat$reasons_for_ending, participant_id, id)

reasons_for_ending_support_tables <- c("reasons_for_ending_change_med",
                                       "reasons_for_ending_device_use",
                                       "reasons_for_ending_location",
                                       "reasons_for_ending_reasons")

participant_id_session_review_id_match <- 
  select(dat$session_review, participant_id, id)

session_review_support_table <- "session_review_distractions"

# Now define a function that uses the selected "participant_id" and "id" 
# columns from each main table and the list of the main table's support 
# tables to add "participant_id" to each support table based on the "id"

add_participant_id <- function(data, id_match, support_tables) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if (names(dat)[[i]] %in% support_tables) {
      output[[i]] <- merge(dat[[i]], id_match, by = "id", all.x = TRUE)
    } else {
      output[[i]] <- dat[[i]]
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# Run the function for each set of support tables

dat <- add_participant_id(data = dat,
                          id_match = participant_id_demographics_id_match,
                          support_tables = demographics_support_table)

dat <- add_participant_id(data = dat,
                          id_match = participant_id_evaluation_id_match,
                          support_tables = evaluation_support_tables)

dat <- add_participant_id(data = dat,
                          id_match = participant_id_mental_health_history_id_match,
                          support_tables = mental_health_history_support_tables)

dat <- add_participant_id(data = dat,
                          id_match = participant_id_reasons_for_ending_id_match,
                          support_tables = reasons_for_ending_support_tables)

dat <- add_participant_id(data = dat,
                          id_match = participant_id_session_review_id_match,
                          support_tables = session_review_support_table)

# ---------------------------------------------------------------------------- #
# Correct test accounts ----
# ---------------------------------------------------------------------------- #

# Changes/Issues log on 1/28/21 indicates that participant 1097 should not be a
# test account. Recode "test_account" accordingly.

dat$participant[dat$participant$participant_id == 1097, ]$test_account <- 0

# Changes/Issues log on 4/16/21 indicates that participant 1663 should be a test 
# account. The account was created for participant 1537 because they were having
# technical issues, but the account was never used.

dat$participant[dat$participant$participant_id == 1663, ]$test_account <- 1

# ---------------------------------------------------------------------------- #
# Remove admin and test accounts ----
# ---------------------------------------------------------------------------- #

# Identify participant_ids that are admin or test accounts

admin_test_account_ids <- 
  dat$participant[dat$participant$admin == 1 |
                    dat$participant$test_account == 1, ]$participant_id

# Define function that removes in each table rows indexed by participant_ids of 
# admin and test accounts

remove_admin_test_accounts <- function(data, admin_test_account_ids) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if ("participant_id" %in% colnames(dat[[i]])) {
      output[[i]] <- subset(dat[[i]], 
                            !(participant_id %in% admin_test_account_ids))
    } else {
      output[[i]] <- dat[[i]]
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# Run function

dat <- remove_admin_test_accounts(dat, admin_test_account_ids)

# ---------------------------------------------------------------------------- #
# Label redacted columns ----
# ---------------------------------------------------------------------------- #

# Specify a character vector of columns whose values should be labeled as "REDACTED", 
# with each column listed as "<table_name>$<column_name>" (e.g., "participant$email"). 
# If no column is to be labeled as "REDACTED", specify NULL without quotes (i.e., 
# "redacted_columns <- NULL").

# On 1/11/2021, Dan Funk said that the following columns are redacted but should
# not be given that they could be useful for analysis. These logical columns
# have all rows == NA.

unnecessarily_redacted_columns <- c("participant$coached_by_id",
                                    "participant$first_coaching_format")

# On 1/11/2021, Dan Funk said that the following columns are redacted and should 
# be. These character columns have all rows == "".

necessarily_redacted_columns <- c("participant$email", "participant$full_name",
                                  "participant$password")

# On 1/11/2021, Dan Funk said that the following columns are redacted and should 
# be. These numeric columns have all rows == NA.

necessarily_redacted_columns <- c(necessarily_redacted_columns, 
                                  "participant$phone", 
                                  "participant$password_token_id")

# On 1/11/2021, Dan Funk said that the following column is redacted and should 
# be. This logical column has all rows == NA.

necessarily_redacted_columns <- c(necessarily_redacted_columns,
                                  "participant$verification_code_id")

# On 1/13/2021, Dan Funk said that the following column is redacted and should 
# be. This character column has all rows == "US", which is its default value in 
# the Data Server.

necessarily_redacted_columns <- c(necessarily_redacted_columns, 
                                  "participant$award_country_code")

# On 1/13/2021, Dan Funk said that the following column is redacted and should 
# be. This numeric column has all rows == 0, which is its default value in the 
# Data Server.

necessarily_redacted_columns <- c(necessarily_redacted_columns, 
                                  "participant$attrition_risk")

# On 1/13/2021, Dan Funk said that the following columns are redacted and should 
# be. These integer columns have all rows == 0, which is their default value in 
# the Data Server.

necessarily_redacted_columns <- c(necessarily_redacted_columns, 
                                  "participant$blacklist",
                                  "participant$can_text_message", 
                                  "participant$coaching",
                                  "participant$verified", 
                                  "participant$wants_coaching")

# Collect all redacted columns

redacted_columns <- c(unnecessarily_redacted_columns, necessarily_redacted_columns)

# Define function to convert redacted columns to characters and label as "REDACTED"

label_redacted_columns <- function(data, redacted_columns) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    output[[i]] <- dat[[i]]
    
    for (j in 1:length(dat[[i]])) {
      table_i_name <- names(dat[i])
      column_j_name <- names(dat[[i]][j])
      table_i_column_j_name <- paste0(table_i_name, "$", column_j_name)
      
      if (table_i_column_j_name %in% redacted_columns) {
        output[[i]][, column_j_name] <- as.character(output[[i]][, column_j_name])
        output[[i]][, column_j_name] <- "REDACTED"
      }
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# Run function

dat <- label_redacted_columns(dat, redacted_columns)

# ---------------------------------------------------------------------------- #
# Remove irrelevant columns ----
# ---------------------------------------------------------------------------- #

# The "tag" columns in the following tables are not used in the Calm Thinking, TET, 
# or GIDI studies and contain no data. They can be removed.

unused_columns <- paste0(c("angular_training", "anxiety_identity", 
                           "anxiety_triggers", "assessing_program", 
                           "bbsiq", "cc", "coach_prompt", "comorbid", 
                           "covid19", "credibility", "dass21_as", 
                           "demographics", "evaluation", "gidi", "help_seeking",
                           "js_psych_trial", "mechanisms",
                           "mental_health_history", "oa", 
                           "return_intention", "rr", "session_review", 
                           "technology_use", "wellness"),
                         "$tag")

# The following "how_learn_other" columns in "evaluation" are not used in the 
# Calm Thinking, TET, or GIDI studies because the "how_learn_other" item was 
# moved to the demographics measure before Calm Thinking study launch. The item 
# is called "ptp_reason_other" in the "demographics" table. The two columns 
# below contain no data and can be removed.

unused_columns <- c(unused_columns, "evaluation$how_learn_other",
                    "evaluation$how_learn_other_link")

# The following columns are also not used in the Calm Thinking, TET, or GIDI 
# studies and contain no data. They can be removed.

unused_columns <- c(unused_columns, "action_log$action_value",
                    "angular_training$study",
                    "mental_health_history$other_help_text",
                    "participant$random_token",
                    "participant$return_date",
                    "reasons_for_ending$other_why_in_control",
                    "sms_log$type")

# Define function to remove irrelevant columns

remove_columns <- function(data, columns_to_remove) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    output[[i]] <- dat[[i]]
    
    for (j in 1:length(dat[[i]])) {
      table_i_name <- names(dat[i])
      column_j_name <- names(dat[[i]][j])
      table_i_column_j_name <- paste0(table_i_name, "$", column_j_name)
      
      if (table_i_column_j_name %in% columns_to_remove) {
        output[[i]] <- output[[i]][, !(names(output[[i]]) %in% column_j_name)]
      }
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# Specify a character vector of columns to be removed, with each column listed
# as "<table_name>$<column_name>" (e.g., "js_psych_trial$tag"). If no column is 
# to be removed, specify NULL without quotes (i.e., "columns_to_remove <- NULL").

# Unused columns defined above can be removed

columns_to_remove <- unused_columns

# Remove "over18" from "participant" table. Dan Funk said that for the Calm
# Thinking study we moved this item to the DASS-21 page (and thus to "dass21_as") 
# and that the "over18" column in the "participant" table should be disregarded.

columns_to_remove <- c(columns_to_remove, "participant$over18")

# Run function

dat <- remove_columns(dat, columns_to_remove)

# ---------------------------------------------------------------------------- #
# Identify any remaining blank columns ----
# ---------------------------------------------------------------------------- #

# Define function to identify columns whose rows are all blank (interpreted by 
# R as NA) or, if column is of class type "character", whose rows are all "". 
# Do this after removing admin and test accounts because some columns may have 
# been used during testing but not during the study itself. If no columns are 
# blank besides those that are ignored in the search, nothing will be outputted.

find_blank_columns <- function(data, ignored_columns) {
  for (i in 1:length(dat)) {
    for (j in 1:length(dat[[i]])) {
      table_i_name <- names(dat[i])
      column_j_name <- names(dat[[i]][j])
      table_i_column_j_name <- paste0(table_i_name, "$", column_j_name)
      
      if (!(table_i_column_j_name %in% ignored_columns)) {
        if (all(is.na(dat[[i]][[j]]))) {
          cat(paste0(table_i_column_j_name,
                     "     , class ", class(dat[[i]][[j]]), ",",
                     "     has all rows == NA", "\n"))
        } else if (all(dat[[i]][[j]] == "")) {
          cat(paste0(table_i_column_j_name,
                     "     , class ", class(dat[[i]][[j]]), ",",
                     '     has all rows == ""', "\n"))
        }
      }
    }
  }
}

# Specify a character vector of columns to be ignored, with each column listed
# as "<table_name>$<column_name>" (e.g., "js_psych_trial$tag"). If no column is 
# to be ignored, specify NULL without quotes (i.e., "ignored_columns <- NULL").

ignored_columns <- NULL

# Run function. If blank columns are identified, consider whether they need to
# be added (a) to the set of columns to be indicated as "REDACTED" (see above)
# or (b) to the set of irrelevant columns to be removed (see above).

find_blank_columns(dat, ignored_columns)

# ---------------------------------------------------------------------------- #
# Identify and recode time stamp and date columns ----
# ---------------------------------------------------------------------------- #

# Use function "identify_columns" (defined above) to identify columns containing 
# "date" in each table

lapply(dat, identify_columns, grep_pattern = "date")

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

invisible(mapply(view_date_str, df = dat, df_name = names(dat)))

# Some "date" and "date_submitted" fields are blank in "js_psych_trial" table. 
# Changes/Issues log states on 10/7/2019 that a timeout on Recognition Ratings 
# led to some of these data not being recorded for four participants. Based on
# "task_log", each participant completed "RR" at "preTest", but the data are
# not recorded in "js_psych_trial". Mark the blank "session" fields for these
# entries as "preTest" and replace their "date" and "date_submitted" with the
# corresponding "date_completed" from "task_log".

blank_date_ids <- unique(dat$js_psych_trial[dat$js_psych_trial$date == "" |
                           dat$js_psych_trial$date_submitted == "", 
                         "participant_id"])

for (i in 1:length(blank_date_ids)) {
  dat$js_psych_trial[dat$js_psych_trial$participant_id == blank_date_ids[i] &
                       dat$js_psych_trial$session == "", 
                     "session"] <- "preTest"
  
  dat$js_psych_trial[dat$js_psych_trial$participant_id == blank_date_ids[i] &
                       dat$js_psych_trial$session == "preTest", 
                     c("date", "date_submitted")] <- 
    dat$task_log[dat$task_log$participant_id == blank_date_ids[i] &
                   dat$task_log$session_name == "preTest" &
                   dat$task_log$task_name == "RR",
                 "date_completed"]
}

# Note: "last_session_date" in "study" table is blank where "current_session" is 
# "preTest". Henry Behan said on 9/17/21 that this is expected.

table(dat$study[dat$study$last_session_date == "", "current_session"], 
      useNA = "always")

# Note: "last_login_date" in "participant" table is blank for participant 3659.
# This participant has no data in any tables besides "participant" and "study".
# Henry Behan said on 9/22/21 that this participant emailed the study team on 
# "2020-11-12 02:25:00 EST" saying they were eligible but had an issue creating 
# an account. An account was made manually by an admin; thus, we presumably have 
# screening data for them (indexed by "session_id") but cannot connect it to their
# "participant_id". The participant is considered officially enrolled in the TET 
# study; thus, this needs to be accounted for in the TET participant flow diagram.

dat$participant[dat$participant$last_login_date == "", "participant_id"]

# The following columns across tables are system-generated date and time stamps.
# Dan Funk said on 10/1/21 that all of these are in EST time zone (note: EST, or
# UTC - 5, all year, not "America/New York", which switches between EST and EDT).

system_date_time_cols <- c("date", "date_created", "date_sent", "date_submitted",
                           "last_login_date", "last_session_date",
                           "date_completed")

# The following column in "return_intention" table is user-provided dates and 
# times. Dan Funk said on 9/24/21 that this data is collected in the user's 
# local time but converted to UTC when stored in the database.

user_date_time_cols <- "return_date"

# Define function to reformat system-generated time stamps and user-provided dates 
# and times and add time zone

recode_date_time_timezone <- function(data) {
  for (i in 1:length(dat)) {
    table_name <- names(dat[i])
    colnames <- names(dat[[i]])
    target_colnames <- colnames[colnames %in% c(system_date_time_cols,
                                                user_date_time_cols)]
    
    if (length(target_colnames) != 0) {
      for (j in 1:length(target_colnames)) {
        # Create new variable for POSIXct values. Recode blanks as NA.
        
        POSIXct_colname <- paste0(target_colnames[j], "_as_POSIXct")
        
        dat[[i]][, POSIXct_colname] <- dat[[i]][, target_colnames[j]]
        dat[[i]][dat[[i]][, POSIXct_colname] == "", POSIXct_colname] <- NA
        
        # Specify time zone as "UTC" for user-provided "return_date" in 
        # "return_intention" and as "EST" for all system-generated 
        # time stamps. Specify nonstandard format to parse "date_sent" in 
        # "sms_log". Other columns are in standard format.
        
        if (table_name == "return_intention" & target_colnames[j] == "return_date") {
          dat[[i]][, POSIXct_colname] <-
            as.POSIXct(dat[[i]][, POSIXct_colname], 
                       tz = "UTC")
        } else if (table_name == "sms_log" & target_colnames[j] == "date_sent") {
          dat[[i]][, POSIXct_colname] <- 
            as.POSIXct(dat[[i]][, POSIXct_colname],
                       tz = "EST", 
                       format = "%m/%d/%Y %H:%M")
        } else {
          dat[[i]][, POSIXct_colname] <-
            as.POSIXct(dat[[i]][, POSIXct_colname], 
                       tz = "EST")
        }
      }
    }
  }
  
  return(dat)
}

# Run function

dat <- recode_date_time_timezone(dat)

# Create new variables for filtering data based on system-generated time stamps. In 
# most tables, the only system-generated time stamp is "date", but "js_psych_trial" 
# table also has "date_submitted". Other tables do not have "date" but have other 
# system-generated time stamps (i.e., "attrition_prediction" table has "date_created"; 
# "email_log", "error_log", and "sms_log" tables have "date_sent"; "gift_log" table
# has "date_created" and "date_sent"; "participant" table has "last_login_date";
# "study" table has "last_session_date"; "task_log" table has "date_completed"). 
# Given that some tables that have multiple system-generated time stamps, let 
# "system_date_time_earliest" and "system_date_time_latest" represent the earliest
# and latest time stamps, respectively, for each row in the table.

for (i in 1:length(dat)) {
  table_name <- names(dat[i])
  colnames <- names(dat[[i]])
  
  dat[[i]][, "system_date_time_earliest"] <- NA
  dat[[i]][, "system_date_time_latest"] <- NA
  
  if (table_name == "js_psych_trial") {
    dat[[i]][, "system_date_time_earliest"] <- pmin(dat[[i]][, "date_as_POSIXct"], 
                                                    dat[[i]][, "date_submitted_as_POSIXct"],
                                                    na.rm = TRUE)
    dat[[i]][, "system_date_time_latest"] <-   pmax(dat[[i]][, "date_as_POSIXct"], 
                                                    dat[[i]][, "date_submitted_as_POSIXct"],
                                                    na.rm = TRUE)
  } else if (table_name == "attrition_prediction") {
    dat[[i]][, "system_date_time_earliest"] <- dat[[i]][, "date_created_as_POSIXct"]
    dat[[i]][, "system_date_time_latest"] <-   dat[[i]][, "date_created_as_POSIXct"]
  } else if (table_name %in% c("email_log", "error_log", "sms_log")) {
    dat[[i]][, "system_date_time_earliest"] <- dat[[i]][, "date_sent_as_POSIXct"]
    dat[[i]][, "system_date_time_latest"] <-   dat[[i]][, "date_sent_as_POSIXct"]
  } else if (table_name == "gift_log") {
    dat[[i]][, "system_date_time_earliest"] <- pmin(dat[[i]][, "date_created_as_POSIXct"], 
                                                    dat[[i]][, "date_sent_as_POSIXct"],
                                                    na.rm = TRUE)
    dat[[i]][, "system_date_time_latest"] <-   pmax(dat[[i]][, "date_created_as_POSIXct"], 
                                                    dat[[i]][, "date_sent_as_POSIXct"],
                                                    na.rm = TRUE)
  } else if (table_name == "participant") {
    dat[[i]][, "system_date_time_earliest"] <- dat[[i]][, "last_login_date_as_POSIXct"]
    dat[[i]][, "system_date_time_latest"] <-   dat[[i]][, "last_login_date_as_POSIXct"]
  } else if (table_name == "study") {
    dat[[i]][, "system_date_time_earliest"] <- dat[[i]][, "last_session_date_as_POSIXct"]
    dat[[i]][, "system_date_time_latest"] <-   dat[[i]][, "last_session_date_as_POSIXct"]
  } else if (table_name == "task_log") {
    dat[[i]][, "system_date_time_earliest"] <- dat[[i]][, "date_completed_as_POSIXct"]
    dat[[i]][, "system_date_time_latest"] <-   dat[[i]][, "date_completed_as_POSIXct"]
  } else if ("date" %in% colnames) {
    dat[[i]][, "system_date_time_earliest"] <- dat[[i]][, "date_as_POSIXct"]
    dat[[i]][, "system_date_time_latest"] <-   dat[[i]][, "date_as_POSIXct"]
  }
}

# The following columns in the "covid9" table are participant-provided dates

user_date_cols <- c("symptoms_date", "test_antibody_date", "test_covid_date")

# Define function to reformat participant-provided dates so that they do not
# contain empty times, which were not assessed

recode_date <- function(data) {
  for (i in 1:length(dat)) {
    colnames <- names(dat[[i]])
    target_colnames <- colnames[colnames %in% user_date_cols]
    
    if (length(target_colnames) != 0) {
      for (j in 1:length(target_colnames)) {
        # Create new variable for Date values. Recode blanks as NA.
        
        Date_colname <- paste0(target_colnames[j], "_as_Date")
        
        dat[[i]][, Date_colname] <- dat[[i]][, target_colnames[j]]
        dat[[i]][dat[[i]][, Date_colname] == "", Date_colname] <- NA
        
        # Columns are in a standard format
        
        dat[[i]][, Date_colname] <- as.Date(dat[[i]][, Date_colname])
      }
    }
  }
  
  return(dat)
}

# Run function. Given that these columns will be read back into R as characters, 
# they will need to be reconverted back to Date using the "as.Date" function.

dat <- recode_date(dat)

# The following "covid19" columns indicate whether the participant preferred not 
# to provide a date. Do not reformat these as dates.

covid19_user_date_pna_cols <- c("symptoms_date_no_answer", 
                                "test_antibody_date_no_answer",
                                "test_covid_date_no_answer")

# ---------------------------------------------------------------------------- #
# Identify and rename session-related columns ----
# ---------------------------------------------------------------------------- #

# Use function "identify_columns" (defined above) to identify columns containing 
# "session" in each table

lapply(dat, identify_columns, grep_pattern = "session")

# View structure of columns containing "session" in each table

view_session_str <- function(data) {
  for (i in 1:length(dat)) {
    print(paste0("Table: ", names(dat[i])))
    cat("\n")
    
    colnames <- names(dat[[i]])
    session_colnames <- colnames[grep("session", colnames)]
    
    if (length(session_colnames) != 0) {
      for (j in 1:length(session_colnames)) {
        session_colname <- session_colnames[j]
        session_colname_class <- class(dat[[i]][, session_colname])
        
        print(paste0(session_colname))
        print(paste0("Class: ", session_colname_class))
        
        if (length(unique(dat[[i]][, session_colname])) > 20) {
          print("First 20 unique levels: ")
          print(unique(dat[[i]][, session_colname])[1:20])
        } else {
          print("All unique levels: ")
          print(unique(dat[[i]][, session_colname]))
        }
        
        print(paste0("Number NA: ", sum(is.na(dat[[i]][, session_colname]))))
        
        if (!("POSIXct" %in% session_colname_class)) {
          print(paste0("Number blank: ", sum(dat[[i]][, session_colname] == "")))
          print(paste0("Number 555: ", sum(dat[[i]][, session_colname] == 555,
                                           na.rm = TRUE)))
        }
        
        cat("\n")
      }
    } else {
      print('No columns containing "session" found.')
      cat("\n")
    }
    
    cat("----------")
    cat("\n", "\n")
  }
}

view_session_str(dat)

# Rename selected session-related columns to clarify conflated content of some
# columns and to enable consistent naming (i.e., "session_only") across tables
# for columns that contain only session information

  # Given that "session" column in "dass21_as" and "oa" tables contains both
  # session information and eligibility status, rename column to reflect this.
  # Also create new column "session_only" with "ELIGIBLE" and "" entries of
  # original "session" column recoded as "Eligibility" (to reflect that these
  # entries were collected at the eligibility screener time point.

  table(dat$dass21_as$session)
  table(dat$oa$session)

  # Given that "session" column in "angular_training" table contains both
  # session information and task-related information (i.e., "flexible_thinking",
  # "Recognition Ratings"), rename column to reflect this.

  table(dat$angular_training$session)

  # Given that "session_name" column in "gift_log" table contains both session
  # information and an indicator of whether an admin awarded the gift card (i.e.,
  # "AdminAwarded"), rename column to reflect this.

  table(dat$gift_log$session_name)
  
  # Rename remaining "session_name" columns (in "action_log" and "task_log"
  # tables) and remaining "session" columns to "session_only" to reflect that
  # they contain only session information. Do not rename "current_session"
  # column of "study" table because "current_session" does not index entries
  # within participants; rather, it reflects participants' current sessions.
  
  # Note: The resulting "session_only" column contains values of "COMPLETE" in
  # some tables (i.e., "action_log", "email_log") but not others (Henry Behan 
  # said on 9/14/21 that the "task_log" table was not designed to record values 
  # of "COMPLETE" in the original "session" column). Also, although "task_log"
  # table contains entries at Eligibility, "action_log" table does not; Henry 
  # Behan said on 9/13/21 said that "action_log" table does not record data 
  # until the participant has created an account.
  
for (i in 1:length(dat)) {
  if (names(dat[i]) %in% c("dass21_as", "oa")) {
    names(dat[[i]])[names(dat[[i]]) == "session"] <- "session_and_eligibility_status"
    
    dat[[i]][, "session_only"] <- dat[[i]][, "session_and_eligibility_status"]
    dat[[i]][dat[[i]][, "session_only"] %in% c("ELIGIBLE", ""), 
                "session_only"] <- "Eligibility"
  } else if (names(dat[i]) == "angular_training") {
    names(dat[[i]])[names(dat[[i]]) == "session"] <- "session_and_task_info"
  } else if (names(dat[i]) == "gift_log") {
    names(dat[[i]])[names(dat[[i]]) == "session_name"] <- "session_and_admin_awarded_info"
  } else if (names(dat[i]) %in% c("action_log", "task_log")) {
    names(dat[[i]])[names(dat[[i]]) == "session_name"] <- "session_only"
  } else if ("session" %in% names(dat[[i]])) {
    names(dat[[i]])[names(dat[[i]]) == "session"] <- "session_only"
  }
}

# ---------------------------------------------------------------------------- #
# Check for repeated columns across tables ----
# ---------------------------------------------------------------------------- #

# Define function that identifies column names that are repeated across tables.
# This is used to identify potential columns to check as to whether their values
# are the same for a given "participant_id" across tables.

find_repeated_column_names <- function(data, ignored_columns) {
  for (i in 1:length(dat)) {
    for (j in 1:length(dat[[i]])) {
      if (!(names(dat[[i]][j]) %in% ignored_columns)) {
        for (k in 1:length(dat)) {
          if ((i != k) &
              names(dat[[i]][j]) %in% names(dat[[k]])) {
            print(paste0(names(dat[i]), "$", names(dat[[i]][j]),
                         "     is also in     ", names(dat[k])))
          }
        }
      }
    }
  }
}

# Define system-related columns to be ignored. Note: The meanings and possible 
# values of some of these columns differ across tables.

key_columns <- c("participant_id", "study_id", "session_id", "id", "X")
raw_timepoint_columns <- c("session", "session_name", "tag")
computed_timepoint_columns <- c("session_and_eligibility_status", "session_only")
raw_date_columns <- c("date", "date_created", "date_sent")
computed_date_columns <- c("date_as_POSIXct", "date_created_as_POSIXct",
                           "date_sent_as_POSIXct",
                           "system_date_time_earliest", "system_date_time_latest")
duration_columns <- c("time_on_page")
log_columns <- c("device", "exception", "successful", "task_name")

# Define other columns that have the same names across the indicated tables but 
# that have different meanings or possible values

# "receive_gift_cards" in "participant" means that the participant is eligible
# to receive gift cards (i.e., has supplied and verified their phone number),
# whereas the same column in "study" means that the participant is assigned to
# a study condition that will be awarded gift cards.

participant_study_columns <- "receive_gift_cards"

# "conditioning" in "angular_training" table may not always correspond with 
# "conditioning" in "study" table where "session" in "angular_training" table
# matches "current_session" in "study" table due to how "current_session" is
# defined in "study" table. See the study-specific data cleaning section "Check 
# 'conditioning' values in 'angular_training' and 'study' tables" below for
# more information about this and related issues.

angular_training_study_columns <- "conditioning"

# "js_psych_trial" contains user activity for the Recognition Ratings measure
# until early July 2020, whereas "angular_training" contains user activity for 
# the Recognition Ratings measure after that. In addition, "angular_training" 
# contains all user activity for training. Because the "js_psych_trial" and
# "angular_training" tables represent different ways of tracking user activity, 
# their shared column names are not necessarily comparable.

js_psych_trial_angular_training_columns <- c("button_pressed", "correct", "rt", 
                                             "rt_first_react", "stimulus", 
                                             "time_elapsed", "trial_type")

# The following shared column names represent different items across tables.

session_review_evaluation_columns <- "distracted"

session_review_reasons_for_ending_columns <- "location"

evaluation_reasons_for_ending_columns <- c("easy", "focused", "helpful", 
                                           "interest", "privacy",
                                           "understand_training")

reasons_for_ending_covid19_columns <- "work"

anxiety_triggers_covid19_columns <- "thoughts"

# "timezone" in "participant" is the timezone gleaned from the participant's web
# browser and serves as the default timezone presented in the "return_intention"
# measure. However, in the "return_intention" measure participants can change the
# default timezone, giving "timezone" in "return_intention" a different meaning.

participant_return_intention_columns <- "timezone"

# Collect all columns to be ignored

ignored_columns <- c(key_columns, 
                     raw_timepoint_columns, computed_timepoint_columns,
                     raw_date_columns, computed_date_columns,
                     duration_columns, log_columns,
                     participant_study_columns,
                     angular_training_study_columns,
                     js_psych_trial_angular_training_columns,
                     session_review_evaluation_columns,
                     session_review_reasons_for_ending_columns,
                     evaluation_reasons_for_ending_columns,
                     reasons_for_ending_covid19_columns,
                     anxiety_triggers_covid19_columns,
                     participant_return_intention_columns)

# Run function

find_repeated_column_names(dat, ignored_columns)

# ---------------------------------------------------------------------------- #
# Correct study extensions ----
# ---------------------------------------------------------------------------- #

# Participants 2004 and 2005 enrolled in the Calm Thinking study and were assigned 
# to a Calm Thinking condition but were given a TET study extension due to a bug at 
# launch of the TET study. According to Dan Funk, the "study_extension" field was 
# not properly being passed through to the Data Server. This was fixed on 4/7/2020, 
# but the "study_extension" for these participants needs to be changed back to "".

specialIDs <- c(2004, 2005)

if (all(dat$study[dat$study$participant_id %in% 
                    specialIDs, ]$study_extension == "")) {
  print("Study extension for special IDs already corrected in server.")
} else {
  dat$study[dat$study$participant_id %in%
              specialIDs, ]$study_extension <- ""
}

# ---------------------------------------------------------------------------- #
# Part II. Filter Data for Desired Study ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Specify desired study ----
# ---------------------------------------------------------------------------- #

# Specify desired study ("Calm" for Calm Thinking study, "TET" for TET study,
# "GIDI" for GIDI study)

study_name <- "Calm"

# ---------------------------------------------------------------------------- #
# Define enrollment period and participant_ids ----
# ---------------------------------------------------------------------------- #

# TODO: Further test best data type for filtering (e.g., POSIXct)





# Define function that gets open/close dates for official enrollment period for 
# desired study. Once TET enrollment closes, replace the NA with the close date.
# The enrollment period is needed to filter eligibility screenings, which are
# not indexed by "participant_id" (participant_ids are created only for eligible
# participants who create an account).

get_enroll_dates <- function(study_name) {
  if (study_name == "Calm") {
    official_enroll_open_date <- "2019-03-18 17:00:00 America/New York"
    official_enroll_close_date <- "2020-04-06 23:59:00 America/New York"
  } else if (study_name == "TET") {
    official_enroll_open_date <- "2020-04-07 00:00:00 America/New York"
    official_enroll_close_date <- NA
  } else if (study_name == "GIDI") {
    official_enroll_open_date <- "2020-07-10 13:00:00 America/New York"
    official_enroll_close_date <- "2020-12-12 23:59:00 America/New York"
  }
  official_enroll_dates <- list(open = official_enroll_open_date,
                                close = official_enroll_close_date)
  return(official_enroll_dates)
}

# Define function that gets participant_ids for desired study

get_participant_ids <- function(data, study_name) {
  if (study_name == "Calm") {
    participant_ids <- dat$study[dat$study$study_extension == "", "participant_id"]
  } else if (study_name == "TET") {
    participant_ids <- dat$study[dat$study$study_extension == "TET", "participant_id"]
  } else if (study_name == "GIDI") {
    participant_ids <- dat$study[dat$study$study_extension == "GIDI", "participant_id"]
  }
  return(participant_ids)
}

# ---------------------------------------------------------------------------- #
# Filter all data ----
# ---------------------------------------------------------------------------- #

# Define function that filters all data for desired study. Use official enrollment
# period to filter eligibility screenings (which excludes screenings during any soft
# launch period). Use participant_ids to filter other tables. Note that "gidi" table
# was not used in Calm Thinking study and that "condition_assignment_settings" table, 
# for which "participant_id" is irrelevant, is retained only for Calm Thinking study; 
# it is not used in TET or GIDI studies.

filter_all_data <- function(data, study_name) {
  official_enroll_dates <- get_enroll_dates(study_name)
  participant_ids <- get_participant_ids(dat, study_name)
  
  if (study_name == "Calm") {
    screening_tbls <- "dass21_as"
    irrelevant_tbls <- "gidi"
  } else if (study_name %in% c("TET", "GIDI")) {
    screening_tbls <- c("dass21_as", "oa")
    irrelevant_tbls <- "condition_assignment_settings"
  }
  
  dat <- dat[!(names(dat) %in% irrelevant_tbls)]
  
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if (names(dat[i]) %in% screening_tbls) {
      if (!is.na(official_enroll_dates$close)) {
        output[[i]] <- dat[[i]][(dat[[i]][, "session_only"] == "Eligibility" &
                                   dat[[i]][, "date"] >= official_enroll_dates$open &
                                   dat[[i]][, "date"] <= official_enroll_dates$close) |
                                  dat[[i]][, "participant_id"] %in% participant_ids, ]
      } else if (is.na(official_enroll_dates$close)) {
        output[[i]] <- dat[[i]][(dat[[i]][, "session_only"] == "Eligibility" &
                                   dat[[i]][, "date"] >= official_enroll_dates$open) |
                                  dat[[i]][, "participant_id"] %in% participant_ids, ]
      }
    } else if ("participant_id" %in% names(dat[[i]])) {
      output[[i]] <- dat[[i]][dat[[i]][, "participant_id"] %in% participant_ids, ]
    } else {
      output[[i]] <- dat[[i]]
    }
  }

  names(output) <- names(dat)
  return(output)
}

# Run function

dat <- filter_all_data(dat, study_name)

# ---------------------------------------------------------------------------- #
# Part III. Calm Thinking Study-Specific Data Cleaning ----
# ---------------------------------------------------------------------------- #

# The following code sections are specific to data for the Calm Thinking study. 
# The code may not be relevant to the TET and GIDI studies and will need to be 
# revised for those studies as needed.

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# "action_log" table has no data prior to 9/10/2020 because during the period it
# was implemented (8/28/2019 to 10/18/2019) it had been collecting more data than 
# intended; therefore, the data were deleted (see entries in Changes/Issues log on 
# 8/28/2019, 10/18/2019). Data collection seems to have resumed on 9/8/2020 (see 
# entry in Changes/Issues log on 9/8/2020).

# "covid19" table has no data prior to 4/23/2020 because this table was intended
# for TET participants. Calm Thinking participants who accessed the site after
# TET launched on 4/7/2020 completed this questionnaire even though it was not
# considered part of the Calm Thinking study.

# ---------------------------------------------------------------------------- #
# Recode "coronavirus" column of "anxiety_triggers" table ----
# ---------------------------------------------------------------------------- #

# "coronavirus" column of "anxiety_triggers" table has no data other than 0 prior 
# to 4/4/2020 because this column was intended for TET participants. Calm Thinking
# participants who accessed the site after TET launched on 4/7/2020 completed this
# item even though it was not considered part of the Calm Thinking study. Recode
# the 0 and 999 values (which indicate the item was not assessed) as NA.

dat$anxiety_triggers[dat$anxiety_triggers$coronavirus %in% c(0, 999), 
                     "coronavirus"] <- NA

# ---------------------------------------------------------------------------- #
# Add participant information ----
# ---------------------------------------------------------------------------- #

# Define function to add helper columns to "participant" table regarding how
# condition was assigned, whether participants enrolled during the soft launch 
# versus the true launch of the study, how high/low risk was classified, and 
# whether accounts are for coaches

add_participant_info <- function(data, study_name) {
  if (study_name == "Calm") {
    
    # Create new variable describing how participants were assigned to condition.
    # See Changes/Issues log entries on 3/10/2019 and 4/9/2019, which state that
    # the soft launch participants 69 to 154 and 43 to 140 below were manually
    # assigned to condition for testing and training purposes.
    
    dat$participant$condition_assignment_method <- NA
    manual_cond <- c(69, 73, 76, 77, 91, 103, 105, 112, 136, 148, 154)
    manual_cond <- c(manual_cond, 43, 45, 57, 63, 67, 71, 82, 90, 94, 96, 97, 104, 
                     108, 120, 130, 131, 132, 140)
    dat$participant <- 
      mutate(dat$participant,
             condition_assignment_method = ifelse(participant_id %in% manual_cond, 
                                                  "manual", "algorithm"))
    
    # Create new variable to differentiate soft and official launch participants
    
    dat$participant$launch_type <- NA
    dat$participant <- 
      mutate(dat$participant,
             launch_type = ifelse(participant_id >= 157, "OFFICIAL", "SOFT"))
    
    # Create new variable describing how participants were classified as high vs.
    # low risk of dropout. Soft launch participants were manually classified. Also,
    # see Changes/Issues log entry on 4/16/2019, which states that the official 
    # participants below were manually classified as high risk "to account for the 
    # drop in attrition scores the past week"; the participants were then randomly 
    # assigned to "HR_COACH" or "HR_NO_COACH".
    
    dat$participant$risk_classification_method <- NA
    manual_risk <- c(258, 259, 255, 250, 251, 246, 247, 242, 240, 235)
    dat$participant <- 
      mutate(dat$participant, 
             risk_classification_method = ifelse(launch_type == "SOFT" |
                                                   participant_id %in% manual_risk, 
                                                 "manual", "algorithm"))
    
    # Create new indicator variable for coaching accounts
    
    coaching_participant_ids <- c(8, 10, 41, 42, 49, 50, 54, 55, 56, 68, 74, 400, 
                                  906, 1103, 1107, 1111, 1112, 1772)
    
    dat$participant$coaching_account <- NA
    dat$participant <- 
      mutate(dat$participant,
             coaching_account = ifelse(participant_id %in% coaching_participant_ids, 
                                       TRUE, FALSE))
  }
  return(dat)
}

# Run function

dat <- add_participant_info(dat, study_name)

# ---------------------------------------------------------------------------- #
# Exclude participants ----
# ---------------------------------------------------------------------------- #

# Confirm that accounts for coaches have already been removed (should all be test 
# accounts, which were removed above)

if (sum(dat$participant$coaching_account) != 0) {
  dat$participant <-
    dat$participant[dat$participant$coaching_account == FALSE, ]
} else {
  print("Coaching accounts already removed.")
}

# Identify official-launch participant_ids and exclude soft-launch participants 
# from all tables. Note that "dass21_as" needs to retain screenings for ineligible
# or uninterested participants (i.e., those without "participant_id") during the
# official enrollment period (already restricted above) because number ineligible
# and number uninterested need to be reported in the participant flow diagram.

official_participant_ids <- 
  dat$participant[dat$participant$launch_type == "OFFICIAL", ]$participant_id

for (i in 1:length(dat)) {
  if (names(dat[i]) == "dass21_as") {
    dat[[i]] <- dat[[i]][dat[[i]][, "participant_id"] %in% official_participant_ids |
                           (dat[[i]][, "session_only"] == "Eligibility" &
                              (is.na(dat[[i]][, "participant_id"]))), ]
  } else if ("participant_id" %in% names(dat[[i]])) {
    dat[[i]] <- dat[[i]][dat[[i]][, "participant_id"] %in% official_participant_ids, ]
  } else {
    dat[[i]] <- dat[[i]]
  }
}

# ---------------------------------------------------------------------------- #
# Edit participant information: Participant spanning two studies ----
# ---------------------------------------------------------------------------- #

# Participant 1992 completed only "dass21_as" before Calm Thinking enrollment 
# closed on 2020-04-06 23:59 EDT but re-engaged with the program after the TET 
# study launched on 2020-04-07 00:00 EDT and got assigned to a TET condition.
# Thus, we change their condition, current session, and last session date to
# what they were in Calm Thinking and remove all their data after the launch of 
# TET. Make "last_session_date" in "study" table the same as "date_completed"
# for "dass21_as" task in "task_log" table, but make "last_login_date" in
# "participant" table NA (as the exact value is unknown). (Note: In TET data 
# cleaning, the participant's data should not be changed or removed.)

dat$study[dat$study$participant_id == 1992, ]$conditioning <- "NONE"
dat$study[dat$study$participant_id == 1992, ]$current_session <- "preTest"

task_log_1992_elig_dass_row <- dat$task_log[dat$task_log$participant_id == 1992 & 
                                              dat$task_log$session_only == "Eligibility" &
                                              dat$task_log$task_name == "DASS21_AS", ]

dat$study[dat$study$participant_id == 1992, "last_session_date"] <- 
  task_log_1992_elig_dass_row[, "date_completed"]
dat$study[dat$study$participant_id == 1992, "last_session_date_as_POSIXct"] <- 
  task_log_1992_elig_dass_row[, "date_completed_as_POSIXct"]
dat$study[dat$study$participant_id == 1992, "system_date_time_earliest"] <- 
  task_log_1992_elig_dass_row[, "system_date_time_earliest"]
dat$study[dat$study$participant_id == 1992, "system_date_time_latest"] <- 
  task_log_1992_elig_dass_row[, "system_date_time_latest"]

dat$participant[dat$participant$participant_id == 1992, 
                c("last_login_date", "last_login_date_as_POSIXct",
                  "system_date_time_earliest", "system_date_time_latest")] <- NA

for (i in 1:length(dat)) {
  if ("system_date_time_earliest" %in% names(dat[[i]]) &
      "participant_id" %in% names(dat[[i]])) {
    dat[[i]] <- dat[[i]][!(dat[[i]][, "participant_id"] == 1992 &
                             (!is.na(dat[[i]][, "system_date_time_earliest"]) &
                             dat[[i]][, "system_date_time_earliest"] >= 
                               "2020-04-07 00:00 EDT")), ]
  } else {
    dat[[i]] <- dat[[i]]
  }
}

# ---------------------------------------------------------------------------- #
# Obtain time of last collected data ----
# ---------------------------------------------------------------------------- #

# Identify latest value for system-generated time stamps across all tables

output <- data.frame(table = rep(NA, length(dat)),
                     max_system_date_time_latest = rep(NA, length(dat)))
output$max_system_date_time_latest <- as.POSIXct(output$max_system_date_time_latest,
                                                 tz = "EST")

for (i in 1:length(dat)) {
  output$table[i] <- names(dat[i])
  
  if ("system_date_time_latest" %in% names(dat[[i]]) &
      !(all(is.na(dat[[i]][, "system_date_time_latest"])))) {
    output$max_system_date_time_latest[i] <- 
      max(dat[[i]][, "system_date_time_latest"], na.rm = TRUE)
  } else {
    output$max_system_date_time_latest[i] <- NA
  }
}

max(output$max_system_date_time_latest, na.rm = TRUE) # "2020-11-13 22:13:27 EST"

# ---------------------------------------------------------------------------- #
# Identify participants with inaccurate "active" column ----
# ---------------------------------------------------------------------------- #

# Participants were supposed to be labeled as inactive at "preTest" or after 21 
# days of inactivity before "PostFollowUp" or "COMPLETE", but a few were not 
# labeled as such (unclear why). If not labeled as inactive after inactivity,
# the participant might not be sent a final reminder email and might not be told 
# their account is closed when they return to the site. Such participants are
# listed below, but their values of "active" are not changed given that whether
# and how this unexpected behavior matters will depend on the specific analysis.

inactive_participant_ids <- 
  dat$study[dat$study$current_session == "preTest" |
               ((Sys.time() - dat$study$last_session_date_as_POSIXct > 21) &
                  !(dat$study$current_session %in% c("PostFollowUp", "COMPLETE"))), 
             "participant_id"]
mislabeled_inactive_participant_ids <- 
  dat$participant[dat$participant$participant_id %in% inactive_participant_ids &
                    dat$participant$active == 1, 
                   "participant_id"]
mislabeled_inactive_participant_ids # 891, 1627, 1852

# Participants were otherwise supposed to be labeled as active (default value),
# but a few were labeled as inactive (unclear why). In these cases, participants
# may have incorrectly been sent a final reminder email or told that their account 
# was closed when they returned to the site. Again, such participants are listed
# below, but their values of "active" are not changed.

active_participant_ids <- 
  dat$participant[!(dat$participant$participant_id %in% inactive_participant_ids),
                  "participant_id"]
mislabeled_active_participant_ids <-
  dat$participant[dat$participant$participant_id %in% active_participant_ids &
                    dat$participant$active == 0,
                  "participant_id"]
mislabeled_active_participant_ids # 191, 329, 723

# ---------------------------------------------------------------------------- #
# Check "conditioning" values in "angular_training" and "study" tables ----
# ---------------------------------------------------------------------------- #

# Note: "conditioning" is blank for some rows of "angular_training". Dan Funk 
# said on 1/4/2021 that pressing the "Continue" button (i.e., "button_pressed" == 
# "continue", which has a high prevalence in these cases) does not always contain 
# a condition. He said that he believes these participants had a session timeout 
# of some kind and likely received a red-error bar saying "you are not logged in" 
# and prompting them to go back to the main site; however, they can ignore the 
# prompt and continue anyway.

nrow(dat$angular_training[dat$angular_training$conditioning == "", ])

# Create aggregated "angular_training" dataset to check "conditioning" column
# and to compare this column's values with "conditioning" in "study" table

ang_train_ag <- dat$angular_training[dat$angular_training$conditioning != "", 
                                     c("participant_id", 
                                       "conditioning", 
                                       "session_and_task_info")]
ang_train_ag <- unique(ang_train_ag)
ang_train_ag <- ang_train_ag[order(ang_train_ag$participant_id), ]

# Check for participants in "TRAINING" after "firstSession", which should not 
# occur because "TRAINING" participants are classified as high/low risk and 
# assigned to "LR_TRAINING", "HR_COACH", or "HR_NO_COACH" before Session 2.

training_ids_past_s1 <- 
  unique(ang_train_ag[ang_train_ag$conditioning == "TRAINING" & 
                        ang_train_ag$session_and_task_info != "firstSession", 
                      "participant_id"])

#   For participant 285, Changes/Issues Log on 5/2/2019 says that their participant
#   ID was missing from "credibility" table and thus was entered manually, at which 
#   point they were assigned to "HR_COACH" (by that point, they had completed the
#   second training session, and they were contacted by a coach before starting
#   the third training session).

training_ids_past_s1_ignore <- 285

#   For participants 249, 639, 645, 984, and 1049, Henry Behan said on 9/7/2021 that 
#   there seems to have been an issue with "angular_training" table picking up these 
#   participants' assigned condition after the first training session. They do have 
#   participant IDs in "credibility" table. They were assigned to "LR_TRAINING" or 
#   "HR_NO_COACH"; "angular_training" shows that they did receive CBM training.

training_ids_past_s1_ignore <- c(training_ids_past_s1_ignore, 
                                 249, 639, 645, 984, 1049)

setdiff(training_ids_past_s1, training_ids_past_s1_ignore)

# Check for participants in "LR_TRAINING", "HR_COACH", or "HR_NO_COACH" at 
# "firstSession", which should not occur because "TRAINING" participants are not
# classified as high/low risk and assigned to one of these conditions until they 
# complete the "affect" questions following the first training session.

risk_ids_at_s1 <- 
  ang_train_ag[ang_train_ag$session_and_task_info == "firstSession" &
                 ang_train_ag$conditioning %in% 
                   c("HR_COACH", "HR_NO_COACH", "LR_TRAINING"), 
               "participant_id"]

#   Participant 161's condition changes from "TRAINING" to "HR_NO_COACH" during 
#   Session 1; nevertheless, it seems that they continued receiving CBM training.

risk_ids_at_s1_ignore <- 161

#   Participant 545 has one row for "LR_TRAINING" at Session 1 two months after 
#   completing the fifth training session; nevertheless, no additional scenarios 
#   seem to have been completed.

risk_ids_at_s1_ignore <- c(risk_ids_at_s1_ignore, 545)

setdiff(risk_ids_at_s1, risk_ids_at_s1_ignore)

# Check that condition stays the same from "secondSession" through "fifthSession"

ang_train_ag_s2_to_s5 <- ang_train_ag[ang_train_ag$session_and_task_info %in% 
                                        c("secondSession", "thirdSession", 
                                          "fourthSession", "fifthSession"), ]
ang_train_ag_s2_to_s5_less <- 
  unique(ang_train_ag_s2_to_s5[, c("participant_id", "conditioning")])

summary <- ang_train_ag_s2_to_s5_less %>% 
  group_by(participant_id) %>% summarise(count = n())
summarySubset <- subset(summary, summary$count > 1)

cond_change_ids_past_s1 <- summarySubset$participant_id

#   Participants 285, 249, 639, 645, 984, 1049 switch from "TRAINING" to "LR_TRAINING", 
#   "HR_COACH", or "HR_NO_COACH" after Session 2. See above for explanation.

cond_change_ids_past_s1_ignore <- c(285, 249, 639, 645, 984, 1049)

setdiff(cond_change_ids_past_s1, cond_change_ids_past_s1_ignore)

# Check for switching between "CONTROL" and another condition, which should not
# occur given that "CONTROL" participants are not rerandomized at Stage 2

control_ids <- unique(ang_train_ag[ang_train_ag$conditioning == "CONTROL", 
                                   "participant_id"])

ang_train_ag_s1_to_s5 <- ang_train_ag[ang_train_ag$session_and_task_info %in% 
                                        c("firstSession", "secondSession", 
                                          "thirdSession", "fourthSession", 
                                          "fifthSession"), ]
ang_train_ag_s1_to_s5_control_less <- 
  unique(ang_train_ag_s1_to_s5[ang_train_ag_s1_to_s5$participant_id %in% control_ids, 
                               c("participant_id", "conditioning")])

summary <- ang_train_ag_s1_to_s5_control_less %>% 
  group_by(participant_id) %>% summarise(count = n())
summarySubset <- subset(summary, summary$count > 1)

control_change_ids <- summarySubset$participant_id

#   Participant 382 is in "NONE" condition and did CBM-I training at "firstSession"
#   and then switched to "CONTROL" and did psychoeducation from "secondSession"
#   through "fifthSession". Henry Behan said on 9/9/2021 that CBM-I training is the
#   default for the "NONE" condition and that he is unsure why the switch occurred.
#   How this participant is handled will depend on the specific analysis.

# TODO: Flag this participant





# Check for "conditioning" in "angular_training" table not matching "conditioning"
# in "study" table at the same session

study_less <- dat$study[, c("participant_id", "conditioning", "current_session")]
names(study_less)[names(study_less) == "conditioning"] <- "current_conditioning"

ang_study_less_merge <- merge(ang_train_ag, study_less, by = "participant_id", all.x = TRUE)
ang_study_less_merge_same_session <- ang_study_less_merge[ang_study_less_merge$session_and_task_info ==
                                                            ang_study_less_merge$current_session, ]

ang_study_cond_mismatch_same_session_ids <- 
  ang_study_less_merge_same_session[(ang_study_less_merge_same_session$conditioning !=
                                       ang_study_less_merge_same_session$current_conditioning), 
                                    "participant_id"]

#   For participant 176, whose condition is "NONE" in "angular_training" table but 
#   "TRAINING" in "study" table at the first training session, Henry Behan said on 
#   9/7/2021 that he thinks the "angular_training" table did not pick up their 
#   assigned condition for some reason. They did receive CBM-I training during the 
#   first session. However, they did not complete the training session, which Henry 
#   Behan said on 9/8/2021 is likely due to attrition (vs. a programming error).

ang_study_cond_mismatch_same_session_ids_ignore <- 176

#   For participant 510, whose condition is "TRAINING" in "angular_training" table
#   but "LR_TRAINING" in "study" table at the first training session, it seems they 
#   completed session 1 training scenarios. Based on "task_log" table, they completed 
#   "affect_post" at Session 1, so the attrition algorithm classified them and updated 
#   their "study" table condition to "LR_TRAINING". However, because they didn't do
#   "ReturnIntention" at Session 1, they did not get "SESSION_COMPLETE" in "task_log" 
#   table and "current_session" in "study" table did not advance to "secondSession".
#   Thus, "current_session" remains "firstSession", leading to the discrepancy.

ang_study_cond_mismatch_same_session_ids_ignore <- 
  c(ang_study_cond_mismatch_same_session_ids_ignore, 510)

#   For participant 645, whose condition is "TRAINING" in "angular_training" table
#   through part of the second training session but "HR_NO_COACH" in "study" table
#   at Session 2, it seems "angular_training" table did not pick up their assigned 
#   condition until partway into Session 2. Also see this participant above.

ang_study_cond_mismatch_same_session_ids_ignore <- 
  c(ang_study_cond_mismatch_same_session_ids_ignore, 645)

setdiff(ang_study_cond_mismatch_same_session_ids, 
        ang_study_cond_mismatch_same_session_ids_ignore)

# Check for "conditioning" at Session 5 in "angular_training" table not matching 
# "conditioning" at "COMPLETE" in "study" table

ang_study_less_merge_s5_complete <- 
  ang_study_less_merge[ang_study_less_merge$session_and_task_info == "fifthSession" &
                         ang_study_less_merge$current_session == "COMPLETE", ]

ang_study_cond_mismatch_s5_complete_ids <- 
  ang_study_less_merge_s5_complete[(ang_study_less_merge_s5_complete$conditioning !=
                                      ang_study_less_merge_s5_complete$current_conditioning), 
                                   "participant_id"]

#   For participant 1458, whose condition is "CONTROL" in "angular_training" table
#   through Session 5 but "HR_COACH" in "study" table at "COMPLETE", Changes/Issues
#   log on 1/28/21 states that participant's condition was changed to "HR_COACH" in 
#   error after participant completed the study in "CONTROL". Thus, change condition 
#   in "study" table back to "CONTROL".

dat$study$conditioning[dat$study$participant_id == 1458] <- "CONTROL"

ang_study_cond_mismatch_s5_complete_ids_ignore <- 1458

setdiff(ang_study_cond_mismatch_s5_complete_ids, 
        ang_study_cond_mismatch_s5_complete_ids_ignore)

# Check for participants in "NONE", which should not occur because participants
# are assigned to "TRAINING" or "CONTROL" prior to starting first training session.

none_ids <- ang_train_ag[ang_train_ag$session_and_task_info %in% 
                           c("firstSession", "secondSession", "thirdSession",
                             "fourthSession", "fifthSession") &
                           ang_train_ag$conditioning == "NONE",
                         "participant_id"]

#   For participants 176 and 382, see above for explanation

none_ids_ignore <- c(176, 382)

#   For participants 390 and 396, Changes/Issues log on 5/21/2019 says that these
#   participant IDs were missing from the "dass" table and thus manually inputted
#   there. These participants were in "NONE" at "firstSession" (where they received 
#   CBM-I training) and "LR_TRAINING" starting at "secondSession".

none_ids_ignore <- c(none_ids_ignore, 390, 396)

#   For participant 403, condition is also "NONE" at "firstSession" (where they 
#   received CBM-I training) and "LR_TRAINING" starting at "secondSession".

none_ids_ignore <- c(none_ids_ignore, 403)

setdiff(none_ids, none_ids_ignore)

# ---------------------------------------------------------------------------- #
# Clean "angular_training" table ----
# ---------------------------------------------------------------------------- #

# TODO: Check on the following potential issues identified in the process of
# checking that data are deidentified. Also consider adding an indicator for
# each kind of row below and clarifying what session the Recognition Ratings
# and Quick Thinking (also called Flexible Thinking) Exercise were done. Note
# that some of the rows below may not arise in the Calm Thinking Study, but
# still check for whether they are present.

# Create new columns for sorting rows based on the tasks they correspond to

dat$angular_training$task <- NA
dat$angular_training$subtask <- NA
dat$angular_training$subtask_detail <- NA

# TODO: Label rows for Anxious Imagery Prime task

dat$angular_training$task[dat$angular_training$trial_type == "FillInBlank" &
                            (dat$angular_training$step_title %in%
                               c("Use Your Imagination", "Use your Imagination"))] <- 
  "anx_imag_prime"





# 1. Some rows are participant descriptions of an anxious situation for the Use 
# Your Imagination task at "firstSession" before starting training

rows1 <- dat$angular_training[dat$angular_training$trial_type == "FillInBlank" &
                                (dat$angular_training$step_title %in%
                                   c("Use Your Imagination", "Use your Imagination")), ]

table(rows1$conditioning) # TODO: Why are there rows for conditions other than 
# those below? Asked Henry 1/14/2021. He said it looks
# like the table may have been pulling the incorrect
# condition. See if this is still an issue after you
# check the conditioning variable above.

expected_conditions <- c("TRAINING",
                         "TRAINING_ORIG", "TRAINING_30", "TRAINING_CREATE", 
                         "TRAINING_ED")

question1 <- rows1[!(rows1$conditioning %in% expected_conditions), ]
View(question1)
write.csv(question1, "./temp_cleaning/angular_training_question1.csv", 
          row.names = FALSE)





# 2. Some rows are participant responses for training scenarios at "fifthSession"
# that required filling in a blank (vs. completing a word fragment). Prior to
# 2/15/2019, these responses were indexed not with a "step_title" value of 
# "scenario", but with a "step_title" value of the scenario's title, which 
# subsequently was stored in "stimulus_name". Among the scenario titles prior 
# to this change was "pub"; after this change, "pub" was renamed to "bar".

scenario_titles <- 
  c(unique(dat$angular_training[dat$angular_training$step_title == 
                                  "scenario", ]$stimulus_name), "pub")

rows2 <- dat$angular_training[dat$angular_training$trial_type == "FillInBlank" &
                                (dat$angular_training$step_title == "scenario" |
                                   dat$angular_training$step_title %in% 
                                   scenario_titles), ]

table(rows2$conditioning)
table(rows2$session_and_task_info)

# TODO: Label rows for Quick Thinking Exercise

dat$angular_training$task[dat$angular_training$stimulus_name == 
                            "flex_thinking_explanations"] <- "quick_thinking"





# 3. Henry says that this criterion reflects participants' responses to the Quick
# Thinking Exercise (also called Flexible Thinking Exercise). Also see his email
# "MT Flex Thinking data for control pps" on 9/27/21 for a draft cleaning script.

rows3 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "flex_thinking_explanations", ]

# TODO: Not all rows have "step_title" of "Exercise: Quick Thinking" due to a
# programming error.

table(rows3$step_title)





table(rows3$conditioning) # TODO: Some Calm Thinking Participants seem to have 
# gotten this. Check this.

View(rows3[rows3$participant_id %in% dat$study[dat$study$study_extension == 
                                                 "", ]$participant_id, ])





table(rows3$session_and_task_info) # TODO: For "CONTROL" participants, the "session" 
# column is populated by "flexible_thinking", so the session is
# unclear. Consider clarifying the session.

table(rows3[rows3$session_and_task_info == "flexible_thinking", ]$conditioning)
table(rows3[rows3$conditioning != "CONTROL", ]$session_and_task_info)

table(rows3$step_title)
table(rows3$trial_type)

View(dat$angular_training[dat$angular_training$conditioning == "CONTROL", ])
View(dat$angular_training[dat$angular_training$conditioning != "CONTROL", ])






# # 4. Henry Behan said these criteria reflect scenarios created by participants 
# in the Write Your Own Scenario exercise in the "TRAINING_CREATE" condition of 
# the TET study. No participants completed this in Calm Thinking.

rows4_all_conditions <- dat$angular_training[dat$angular_training$trial_type == 
                                               "FillInBlank" &
                                               dat$angular_training$stimulus_name == 
                                               "" &
                                               dat$angular_training$step_title == 
                                               "", ]
nrow(rows4_all_conditions)




# 5. Henry Behan said this criterion reflects participants' explanations as to 
# why the they created occurred in the Write Your Own Scenario exercise in the
# "TRAINING_CREATE" condition of the TET study. No participants completed this
# in Calm Thinking.

rows5 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "training_create_explanations", ]
nrow(rows5)

# Confirm no rows remain unaccounted for

ignored_ids <- c(rows1$id, rows2$id, rows3$id, rows4_all_conditions$id, rows5$id)

remaining <- dat$angular_training[!(dat$angular_training$id %in% ignored_ids) &
                                    dat$angular_training$trial_type == 
                                    "FillInBlank", ]

nrow(remaining) == 0

# 6. For indicating the session at which Recognition Ratings were completed, 
# see Henry's email "MT Flex Thinking data for control pps" on 9/30/21 for a 
# draft cleaning script.

# Create new column for session-only information

dat$angular_training$session_only <- 
  ifelse(dat$angular_training$session_and_task_info %in%
           c("firstSession", "secondSession", "thirdSession", "fourthSession", 
             "fifthSession"),
         dat$angular_training$session_and_task_info,
         NA)

# TODO: Label rows for Recognition Ratings

dat$angular_training$task[dat$angular_training$session_and_task_info == 
                            "Recognition Ratings"] <- "recognition_ratings"

# TODO: Determine session. Revise so that "readinessHeader" isn't just NA

for (i in 1:nrow(dat$angular_training)) {
  if (dat$angular_training$task[i] %in% "recognition_ratings") {
    if (dat$angular_training$stimulus_name[i] == "readinessHeader") {
      dat$angular_training$session_only[i] <- NA
    } else {
      if (dat$angular_training$session_index[i] == 0) {
        dat$angular_training$session_only[i] <- "preTest"
      } else if (dat$angular_training$session_index[i] == 3) {
        dat$angular_training$session_only[i] <- "thirdSession"
      } else if (dat$angular_training$session_index[i] == 5) {
        dat$angular_training$session_only[i] <- "fifthSession"
      } else if (dat$angular_training$session_index[i] == 6) {
        dat$angular_training$session_only[i] <- "PostFollowUp"
      } else if (dat$angular_training$session_index[i] == 7) {
        dat$angular_training$session_only[i] <- "PostFollowUp2"
      } 
    }
  }
}

# TODO: Continue reviewing Henry's code with Line 55


View(dat$angular_training[dat$angular_training$task %in% "recognition_ratings", ])



angular3$date <- format(as.Date(strptime(angular3$date, '%Y-%m-%d %H:%M')), "%x")

ratings2 <- filter(ratings, participant_id > 2272)

ratings2 <- ratings2 %>% 
  group_by(participant_id) %>%
  mutate(time_no = as.numeric(as.factor(date)))

if(any(ratings2$time_no > 4)) {
  "TRUE"
} else {
  "FALSE"
}





# ---------------------------------------------------------------------------- #
# Clean "reasons_for_ending" table ----
# ---------------------------------------------------------------------------- #

# Changes/Issues Log on 10/7/2019 says that some completers of the 2-month follow-
# up assessment were incorrectly administered "reasons_for_ending". No data were
# collected after this measure. Thus, these entries can be deleted. Note that the
# "reasons_for_ending" task is not recorded in the "task_log" table.

reasons_for_ending_complete_ids <- 
  dat$reasons_for_ending[dat$reasons_for_ending$session_only == "COMPLETE", "id"]

reasons_for_ending_tbls <- c("reasons_for_ending",
                             "reasons_for_ending_change_med",
                             "reasons_for_ending_device_use",
                             "reasons_for_ending_location",
                             "reasons_for_ending_reasons")

for (i in 1:length(dat)) {
  if (names(dat)[i] %in% reasons_for_ending_tbls) {
    dat[[i]] <- dat[[i]][!(dat[[i]][, "id"] %in% reasons_for_ending_complete_ids), ]
  } else {
    dat[[i]] <- dat[[i]]
  }
}

# ---------------------------------------------------------------------------- #
# Exclude screenings resembling bots ----
# ---------------------------------------------------------------------------- #

# There are many session_ids in "dass21_as" table on 12/6/2019. Most of them have
# "time_on_page" of exactly 1 or 10 at screening, in which case none of them got
# a "participant_id"; these 162,171 unique session_ids appear to be bots.

summary <- dat$dass21_as %>%
  group_by(as.Date(date)) %>%
  summarise(count=n())
head(summary[order(summary$count, decreasing = TRUE), ])

table(dat$dass21_as[as.Date(dat$dass21_as$date) == "2019-12-06", ]$time_on_page,
      dat$dass21_as[as.Date(dat$dass21_as$date) == "2019-12-06", ]$session_only)

bot_session_ids <- dat$dass21_as[as.Date(dat$dass21_as$date) == "2019-12-06" &
                                   dat$dass21_as$time_on_page %in% c(1, 10) &
                                   dat$dass21_as$session_only == "Eligibility",
                                 "session_id"]
sum(!is.na(dat$dass21_as[dat$dass21_as$session_id %in% bot_session_ids, 
                         "participant_id"]))

length(unique(bot_session_ids))

# Remove the screenings for these bots (each had exactly one screening)

dat$dass21_as <- dat$dass21_as[!(dat$dass21_as$session_id %in% bot_session_ids), ]

# ---------------------------------------------------------------------------- #
# Identify and remove nonmeaningful duplicates ----
# ---------------------------------------------------------------------------- #

# For rows that have duplicated values on every meaningful column (i.e., every
# column except "X" and "id"), keep only the last row after sorting by "id" for
# tables that contain "id" (throw error if "attrition_prediction", "participant", 
# or "study" tables, which lack "id", contain multiple rows per "participant_id",
# in which case they will need to be sorted and have their rows consolidated).

for (i in 1:length(dat)) {
  meaningful_cols <- names(dat[[i]])[!(names(dat[[i]]) %in% c("X", "id"))]
  
  if (names(dat[i]) %in% c("attrition_prediction", "participant", "study")) {
    if (nrow(dat[[i]]) != length(unique(dat[[i]][, "participant_id"]))) {
      error(paste0("Unexpectedly, table ", names(dat[i]), 
                   "contains multiple rows for at least one participant_id"))
    }
  } else if ("id" %in% names(dat[[i]])) {
    dat[[i]] <- dat[[i]][order(dat[[i]][, "id"]), ]
    
    dat[[i]] <- dat[[i]][!duplicated(dat[[i]][, meaningful_cols],
                                     fromLast = TRUE), ]
  } else {
    stop(paste0("Table ", names(dat[i]), "needs to be checked for duplicates"))
  }
}

# ---------------------------------------------------------------------------- #
# Handle multiple screenings and report participant flow up to enrollment ----
# ---------------------------------------------------------------------------- #

# Six participants did not have their "participant_id" connected to all screening 
# attempts for their corresponding "session_id" (unclear why, as only for some, not 
# all, attempts without "participant_id" was the participant ineligible on age, and 
# all attempts were eligible on DASS). Correct this.

unique_s_p_ids <- unique(dat$dass21_as[dat$dass21_as$session_only == "Eligibility", 
                                       c("session_id", "participant_id")])

n_unq_s_p_ids <- unique_s_p_ids %>% 
  group_by(session_id) %>% 
  summarise(count=n())

s_ids_with_mlt_p_ids <- n_unq_s_p_ids$session_id[n_unq_s_p_ids$count > 1]
length(unique(s_ids_with_mlt_p_ids))

unique_s_p_ids_rest <- unique_s_p_ids[unique_s_p_ids$session_id %in% s_ids_with_mlt_p_ids, ]
unique_s_p_ids_rest <- unique_s_p_ids_rest[!is.na(unique_s_p_ids_rest$participant_id), ]

nrow(unique_s_p_ids_rest[duplicated(unique_s_p_ids_rest$session_id), ]) == 0

for (i in 1:nrow(unique_s_p_ids_rest)) {
  for (j in 1:nrow(dat$dass21_as)) {
    if (dat$dass21_as$session_id[j] == unique_s_p_ids_rest$session_id[[i]]) {
      dat$dass21_as$participant_id[j] <- unique_s_p_ids_rest$participant_id[[i]]
    }
  }
}

# Define DASS-21-AS items

dass21_as_items <- c("bre", "dry", "hea", "pan", "sca", "tre", "wor")

# After removing nonmeaningful duplicates (see above), for remaining rows that 
# have duplicated values on DASS-21-AS items, "over18", and "time_on_page" for 
# a given "session_id" and "session_only", keep only the last row after sorting
# based on "id"

response_cols <- c(dass21_as_items, "over18", "time_on_page")

dat$dass21_as <- dat$dass21_as[order(dat$dass21_as$id), ]

dat$dass21_as <- dat$dass21_as[!duplicated(dat$dass21_as[, c(response_cols, 
                                                             "session_id", 
                                                             "session_only")],
                                           fromLast = TRUE), ]

# Compute number of multiple rows per "session_id" at screening

dass21_as_eligibility <- dat$dass21_as[dat$dass21_as$session_only == "Eligibility", ]

n_eligibility_rows <- dass21_as_eligibility %>% 
                      group_by(session_id, session_only) %>% 
                      summarise(count=n()) %>%
                      as.data.frame()

names(n_eligibility_rows)[names(n_eligibility_rows) == "count"] <- "n_eligibility_rows"

dat$dass21_as <- merge(dat$dass21_as, 
                       n_eligibility_rows, 
                       c("session_id", "session_only"), 
                       all.x = TRUE,
                       sort = FALSE)

# Compute mean "time_on_page" across multiple rows per "session_id". Note that 
# this currently only applies to rows at screening.

time_on_page_mean <- aggregate(dass21_as_eligibility$time_on_page, 
                               list(dass21_as_eligibility$session_id,
                                    dass21_as_eligibility$session_only), 
                               mean)
names(time_on_page_mean) <- c("session_id", "session_only", "time_on_page_mean")

time_on_page_mean[is.nan(time_on_page_mean[, "time_on_page_mean"]), 
                  "time_on_page_mean"] <- NA

dat$dass21_as <- merge(dat$dass21_as, 
                       time_on_page_mean, 
                       c("session_id", "session_only"), 
                       all.x = TRUE,
                       sort = FALSE)

# Compute number of unique rows on DASS-21-AS items per "session_id" at screening. 
# If a participant has more than two sets of unique values on DASS-21-AS items, 
# we will exclude them from analysis given concerns about their data integrity. 
# Otherwise, we will include them in analysis, even if they have two or more entries 
# for "over18" (their final "over18" entry had to be TRUE for them to enroll in the 
# program). However, we will compute the column mean across their unique DASS-21-AS 
# item entries and then use these column means to compute their average item score
# (taking the mean of available column means). In this way, we will have one set 
# of items and one average item score for analysis that take into account the 
# participant's multiple unique item entries.

unique_dass21_as_eligibility_items <- 
  unique(dass21_as_eligibility[, c("participant_id", "session_id",
                                   "session_only",
                                   dass21_as_items)])

n_eligibility_unq_item_rows <- unique_dass21_as_eligibility_items %>% 
                               group_by(session_id, session_only) %>% 
                               summarise(count=n())

n_eligibility_unq_item_rows <- as.data.frame(n_eligibility_unq_item_rows)
names(n_eligibility_unq_item_rows)[names(n_eligibility_unq_item_rows) == "count"] <-
  "n_eligibility_unq_item_rows"

dat$dass21_as <- merge(dat$dass21_as, 
                       n_eligibility_unq_item_rows, 
                       c("session_id", "session_only"), 
                       all.x = TRUE,
                       sort = FALSE)

# Compute column mean of unique values on DASS-21-AS items per "session_id",
# treating values of "Prefer Not to Answer" as NA without recoding them as NA in 
# the actual dataset. Note that this currently only applies to rows at screening.

unique_items <- unique_dass21_as_eligibility_items

unique_items[, dass21_as_items][unique_items[, dass21_as_items] == 555] <- NA

for (i in 1:length(dass21_as_items)) {
  col_name <- dass21_as_items[i]
  col_mean_name <- paste0(dass21_as_items[i], "_mean")
  
  dass21_as_item_mean <- aggregate(unique_items[, col_name], 
                                   list(unique_items$session_id,
                                        unique_items$session_only), 
                                   mean, na.rm = TRUE)
  names(dass21_as_item_mean) <- c("session_id", "session_only", col_mean_name)
  
  dass21_as_item_mean[is.nan(dass21_as_item_mean[, col_mean_name]), 
                      col_mean_name] <- NA
  
  dat$dass21_as <- merge(dat$dass21_as, 
                         dass21_as_item_mean, 
                         c("session_id", "session_only"), 
                         all.x = TRUE,
                         sort = FALSE)
}

# Compute DASS-21-AS total score per row (as computed by system, not accounting
# for multiple entries) by taking mean of available raw items and multiplying 
# by 7 (to create "dass21_as_total"). Treat "Prefer Not to Answer" as NA without 
# recoding it as NA in the actual dataset. At screening, multiply by 2 to interpret 
# against eligibility criterion ("dass21_as_total_interp"; >= 10 is eligible) and 
# create indicator ("dass21_as_eligible") to reflect eligibility on DASS-21-AS.

dat$dass21_as$dass21_as_total <- NA
dat$dass21_as$dass21_as_total_interp <- NA
dat$dass21_as$dass21_as_eligible <- NA

temp_dass21_as <- dat$dass21_as
temp_dass21_as[, dass21_as_items][temp_dass21_as[, dass21_as_items] == 555] <- NA

for (i in 1:nrow(temp_dass21_as)) {
  if (all(is.na(temp_dass21_as[i, dass21_as_items]))) {
    dat$dass21_as$dass21_as_total[i] <- NA
  } else {
    dat$dass21_as$dass21_as_total[i] <- 
      rowMeans(temp_dass21_as[i, dass21_as_items], na.rm = TRUE)*7
  }
}

for (i in 1:nrow(dat$dass21_as)) {
  if (dat$dass21_as$session_only[i] == "Eligibility") {
    dat$dass21_as$dass21_as_total_interp[i] <- dat$dass21_as$dass21_as_total[i]*2
    
    if (is.na(dat$dass21_as$dass21_as_total_interp[i])) {
      dat$dass21_as$dass21_as_eligible[i] <- 0
    } else if (dat$dass21_as$dass21_as_total_interp[i] < 10) {
      dat$dass21_as$dass21_as_eligible[i] <- 0
    } else if(dat$dass21_as$dass21_as_total_interp[i] >= 10) {
      dat$dass21_as$dass21_as_eligible[i] <- 1
    }
  }
}

# Compute DASS-21-AS total score for analysis (accounting for multiple entries at 
# screening). At screening, take mean of available item column means and multiply 
# by 7 (to create "dass21_as_total_anal"); at other time points, copy values from
# "dass21_as_total" into "dass21_as_total_anal".

dass21_as_item_means <- paste0(dass21_as_items, "_mean")

dat$dass21_as$dass21_as_total_anal <- NA

for (i in 1:nrow(dat$dass21_as)) {
  if (dat$dass21_as$session_only[i] == "Eligibility") {
    if (all(is.na(dat$dass21_as[i, dass21_as_item_means]))) {
      dat$dass21_as$dass21_as_total_anal[i] <- NA
    } else {
      dat$dass21_as$dass21_as_total_anal[i] <- 
        rowMeans(dat$dass21_as[i, dass21_as_item_means], na.rm = TRUE)*7
    }
  } else {
    dat$dass21_as$dass21_as_total_anal[i] <- dat$dass21_as$dass21_as_total[i]
  }
}

# Report number of participants screened, enrolled, and not enrolled. For not 
# enrolled, report the reason; for people with multiple entries, base the
# reason on the most recent entry (but note that nonenrollment following each
# attempt could have occurred for a different reason--e.g., not eligible on age,
# not eligible on DASS, eligible but not interested). 

dass21_as_eligibility_last <- 
  dat$dass21_as[dat$dass21_as$session_only == "Eligibility", ]
dass21_as_eligibility_last <- 
  dass21_as_eligibility_last[order(dass21_as_eligibility_last$session_id, 
                                   dass21_as_eligibility_last$id), ]
dass21_as_eligibility_last <- 
  dass21_as_eligibility_last[!duplicated(dass21_as_eligibility_last$session_id, 
                                         fromLast = TRUE), ]

# 5267 were screened for eligibility

nrow(dass21_as_eligibility_last) 

# Of the 5267 screened, 3519 did not enroll and 1748 enrolled

nrow(dass21_as_eligibility_last[is.na(dass21_as_eligibility_last$participant_id), ])
nrow(dass21_as_eligibility_last[!is.na(dass21_as_eligibility_last$participant_id), ])

# Of the 3519 who did not enroll, 774 were ineligible on DASS but eligible on age, 
# 23 were ineligible on both DASS and age, 111 were eligible on DASS but ineligible
# on age, and 2611 were eligible on both DASS and age

nrow(dass21_as_eligibility_last[is.na(dass21_as_eligibility_last$participant_id) &
                                  dass21_as_eligibility_last$dass21_as_eligible == 0 &
                                  dass21_as_eligibility_last$over18 == "true", ])
nrow(dass21_as_eligibility_last[is.na(dass21_as_eligibility_last$participant_id) &
                                  dass21_as_eligibility_last$dass21_as_eligible == 0 &
                                  dass21_as_eligibility_last$over18 == "false", ])
nrow(dass21_as_eligibility_last[is.na(dass21_as_eligibility_last$participant_id) &
                                  dass21_as_eligibility_last$dass21_as_eligible == 1 &
                                  dass21_as_eligibility_last$over18 == "false", ])
nrow(dass21_as_eligibility_last[is.na(dass21_as_eligibility_last$participant_id) &
                                  dass21_as_eligibility_last$dass21_as_eligible == 1 &
                                  dass21_as_eligibility_last$over18 == "true", ])

# Note that if screening data from participants who did not enroll is going to be
# analyzed, 17 participants should be excluded from analysis because they have more 
# than two sets of unique values on DASS-21-AS items

exclude_nonenrolled_session_ids <- 
  dass21_as_eligibility_last[is.na(dass21_as_eligibility_last$participant_id) &
                                    dass21_as_eligibility_last$n_eligibility_unq_item_rows > 2, 
                             "session_id"]
length(exclude_nonenrolled_session_ids)

table(dass21_as_eligibility_last[is.na(dass21_as_eligibility_last$participant_id), 
                                 "n_eligibility_unq_item_rows"])

# Of the 1748 who did enroll, 6 participants should be excluded from analysis 
# because they have more than two sets of unique values on DASS-21-AS items

exclude_enrolled_participant_ids <- 
  dass21_as_eligibility_last[!is.na(dass21_as_eligibility_last$participant_id) &
                               dass21_as_eligibility_last$n_eligibility_unq_item_rows > 2, 
                             "participant_id"]
length(exclude_enrolled_participant_ids)

table(dass21_as_eligibility_last[!is.na(dass21_as_eligibility_last$participant_id), 
                                 "n_eligibility_unq_item_rows"])

# Create indicator "exclude_analysis" to reflect nonenrolled/enrolled participants
# who should be excluded from analysis

dat$dass21_as$exclude_analysis <- 0

dat$dass21_as$exclude_analysis[dat$dass21_as$session_id %in% 
                                 exclude_nonenrolled_session_ids |
                               dat$dass21_as$participant_id %in% 
                                 exclude_enrolled_participant_ids] <- 1

# Add "exclude_analysis" to "participant" table

dat$participant$exclude_analysis <- 0
dat$participant$exclude_analysis[dat$participant$participant_id %in%
                                   exclude_enrolled_participant_ids] <- 1

# ---------------------------------------------------------------------------- #
# Identify and remove INSERT duplicates ----
# ---------------------------------------------------------------------------- #

# TODO: 4 rows of "js_psych_trial" have "internal_node_id" == "", where "trial
# _index" is 0. Codebook says these columns should correspond to each other.

table(dat$js_psych_trial$internal_node_id, useNA = "always")
table(dat$js_psych_trial$trial_index, useNA = "always")

View(dat$js_psych_trial[dat$js_psych_trial$internal_node_id == "", ])





# TODO: Note that repeated "dass21_as" screening attempts are reflected in
# "task_log" only for some participants. Thus, "task_log" should not be used
# to identify repeated screening attempters.

View(dat$task_log[dat$task_log$participant_id == 177, ])
View(dat$dass21_as[dat$dass21_as$participant_id %in% 177, ])

View(dat$task_log[dat$task_log$participant_id == 1529, ])
View(dat$dass21_as[dat$dass21_as$participant_id %in% 1529, ])




# TODO: Check Changes/Issues log about "angular_training" repeated tasks. Didn't 
# see anything. Asked Henry to confirm the columns and give potential reasons for
# multiple entries on 11/4/21.

test <- dat$angular_training[dat$angular_training$participant_id == 164, ]
View(test)
View(test[duplicated(test[, c("participant_id", 
                              "session_and_task_info",
                              "session_counter",
                              "step_title",
                              "stimulus",
                              "stimulus_name")]), ])

target_cols <- c("participant_id", 
                 "session_and_task_info",
                 "session_counter",
                 "step_title",
                 "stimulus",
                 "stimulus_name")
angular_training_dup <- 
  dat$angular_training[duplicated(dat$angular_training[, target_cols]), ]
write.csv(angular_training_dup, file = "./temp_cleaning/angular_training_dup.csv",
          row.names = FALSE)







# TODO: Asked Henry to confirm these columns and give potential reasons for multiple 
# entries on 11/4/21.

test2 <- dat$js_psych_trial[dat$js_psych_trial$participant_id == 247, ]
View(test2)
View(test2[duplicated(test2[, c("participant_id",
                                "session_only",
                                "internal_node_id", 
                                "stimulus"), ]), ])

target_cols <- c("participant_id",
                 "session_only",
                 "internal_node_id", 
                 "stimulus")
js_psych_trial_dup <- 
  dat$js_psych_trial[duplicated(dat$js_psych_trial[, target_cols]), ]
write.csv(js_psych_trial_dup, file = "./temp_cleaning/js_psych_trial_dup.csv",
          row.names = FALSE)





# TODO: Investigate other tables yielding duplicates





#   2 duplicated rows for table: credibility
#   With these ' participant_id ':  174 174

View(dat$credibility[dat$credibility$participant_id == 174, ]) # SAME DATA

#   1 duplicated rows for table: return_intention
#   With these ' participant_id ':  1762

View(dat$return_intention[dat$return_intention$participant_id == 1762, ]) # SAME DATA AND DATE

#   1 duplicated rows for table: bbsiq
#   With these ' participant_id ':  1529

View(dat$bbsiq[dat$bbsiq$participant_id == 1529, ]) # SAME DATA AND DATE

#   1 duplicated rows for table: oa
#   With these ' participant_id ':  1860

View(dat$oa[dat$oa$participant_id == 1860, ]) # SAME DATA AND DATE

#   3 duplicated rows for other tasks in table: task_log
#   With these 'participant_id':  1762 1529 1860

View(dat$task_log[dat$task_log$participant_id == 1762, ]) # CONSISTENT WITH ABOVE
View(dat$task_log[dat$task_log$participant_id == 1529, ]) # CONSISTENT WITH ABOVE
View(dat$task_log[dat$task_log$participant_id == 1860, ]) # CONSISTENT WITH ABOVE





# TODO: Define function

compute_n_rows_col_means <- function(df, index_cols, time_on_col, item_cols) {
  # Compute number of rows per index columns
  
  n_rows <- df %>%
    group_by(across(all_of(index_cols))) %>%
    summarise(count=n()) %>%
    as.data.frame()
  
  names(n_rows)[names(n_rows) == "count"] <- "n_rows"
  
  df2 <- merge(df, n_rows, index_cols, all.x = TRUE, sort = FALSE)
  
  # Compute mean "time_on_" column across multiple rows per index columns
  
  time_on_col_mean_name <- paste0(time_on_col, "_mean")
  
  time_on_col_mean <- aggregate(df2[, time_on_col], 
                                 as.list(df2[, index_cols]), 
                                 mean)
  names(time_on_col_mean) <- c(index_cols, time_on_col_mean_name)
  
  time_on_col_mean[is.nan(time_on_col_mean[, time_on_col_mean_name]), 
                   time_on_col_mean_name] <- NA
  
  df3 <- merge(df2, time_on_col_mean, index_cols, all.x = TRUE, sort = FALSE)
  
  # Compute number of unique rows on item columns per index columns. We will 
  # compute the column mean across the unique item entries.
  
  unique_items <- unique(df[, c(index_cols, item_cols)])
  
  n_unq_item_rows <- unique_items %>% 
    group_by(across(all_of(index_cols))) %>% 
    summarise(count=n()) %>%
    as.data.frame()
  
  names(n_unq_item_rows)[names(n_unq_item_rows) == "count"] <- "n_unq_item_rows"
  
  df4 <- merge(df3, n_unq_item_rows, index_cols, all.x = TRUE, sort = FALSE)
  
  # If multiple unique values on any item per index columns are present, compute 
  # column means across unique values for all items, treating values of "Prefer
  # Not to Answer" as NA without recoding them as NA in the actual dataset
  
  df5 <- df4
  
  unique_items[, item_cols][unique_items[, item_cols] == 555] <- NA
  
  if (any(df5$n_unq_item_rows > 1)) {
    for (i in 1:length(item_cols)) {
      col_name <- item_cols[i]
      col_mean_name <- paste0(item_cols[i], "_mean")
      
      item_mean <- aggregate(unique_items[, col_name], 
                             as.list(unique_items[, index_cols]),
                             mean, 
                             na.rm = TRUE)
      names(item_mean) <- c(index_cols, col_mean_name)
      
      item_mean[is.nan(item_mean[, col_mean_name]), col_mean_name] <- NA
      
      df5 <- merge(df5, item_mean, index_cols, all.x = TRUE, sort = FALSE)
    }
  }
  
  return(df5)
}

# TODO: Run function

test <- compute_n_rows_col_means(dat$credibility, 
                                 c("participant_id", "session_only"),
                                 "time_on_page",
                                 c("confident_online", "important"))

test <- compute_n_rows_col_means(dat$return_intention, 
                                 c("participant_id", "session_only"),
                                 "time_on_page",
                                 "days_till_returning")

bbsiq_items <- 
  c("breath_flu", "breath_physically", "breath_suffocate", "chest_heart", 
    "chest_indigestion", "chest_sore", "confused_cold", "confused_outofmind", 
    "confused_work", "dizzy_ate", "dizzy_ill", "dizzy_overtired", "friend_helpful", 
    "friend_incompetent", "friend_moreoften", "heart_active", "heart_excited", 
    "heart_wrong", "jolt_burglar", "jolt_dream", "jolt_wind", "lightheaded_eat", 
    "lightheaded_faint", "lightheaded_sleep", "party_boring", "party_hear", 
    "party_preoccupied", "shop_bored", "shop_concentrating", "shop_irritating", 
    "smoke_cig", "smoke_food", "smoke_house", "urgent_bill", "urgent_died", 
    "urgent_junk", "vision_glasses", "vision_illness", "vision_strained", 
    "visitors_bored", "visitors_engagement", "visitors_outstay")

test <- compute_n_rows_col_means(dat$bbsiq, 
                                 c("participant_id", "session_only"),
                                 "time_on_page",
                                 bbsiq_items)

test <- compute_n_rows_col_means(dat$oa, 
                                 c("participant_id", "session_only"),
                                 "time_on_page",
                                 c("axf", "axs", "avo", "wrk", "soc"))

test <- compute_n_rows_col_means(dat$task_log, 
                                 c("participant_id", "session_only", "task_name", "tag"),
                                 "time_on_task",
                                 "task_name")






# TODO: Check "dat_no_dup" to ensure it's what expected







# Define functions to report and remove duplicated rows. "report_remove_dups_df"
# is used within "report_remove_dups_list".

report_remove_dups_df <- function(df, df_name, target_cols, index_col) {
  duplicated_rows <- df[duplicated(df[, target_cols]), ]
  if (nrow(duplicated_rows) > 0) {
    cat(nrow(duplicated_rows), "duplicated rows for table:", df_name)
    cat("\n")
    cat("With these '", index_col, "': ", duplicated_rows[, index_col])
    cat("\n-------------------------\n")
    output <- df[!duplicated(df[, target_cols]), ]
    rownames(output) <- 1:nrow(output)
  } else {
    cat("No duplicated rows for table:", df_name)
    cat("\n-------------------------\n")
    output <- df
  }
  return(output)
}

report_remove_dups_list <- function(data) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    if (names(dat[i]) %in% c("condition_assignment_settings", 
                             "demographics_race",
                             "error_log",
                             "evaluation_coach_help_topics", "evaluation_devices",
                             "evaluation_places", "evaluation_preferred_platform",
                             "evaluation_reasons_control", 
                             "mental_health_change_help", "mental_health_disorders", 
                             "mental_health_help", "mental_health_why_no_help", 
                             "reasons_for_ending_change_med", "reasons_for_ending_device_use",
                             "reasons_for_ending_location", "reasons_for_ending_reasons",
                             "session_review_distractions",
                             "sms_log")) {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("X", "id"), 
                                           "id")
    } else if (names(dat[i]) == "affect") {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id", 
                                             "session_only", 
                                             "tag"), 
                                           "participant_id")
    } else if (names(dat[i]) == "angular_training") {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id", 
                                             "session_and_task_info",
                                             "session_counter",
                                             "step_title",
                                             "stimulus",
                                             "stimulus_name"), 
                                           "participant_id")
    } else if (names(dat[i]) %in% c("attrition_prediction", "participant")) {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           "participant_id", 
                                           "participant_id")
    } else if (names(dat[i]) == "dass21_as") {
      duplicated_rows_eligibility <- 
        dat[[i]][dat[[i]][, "session_only"] == "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id",
                                            "session_only",
                                            "session_id")])), ]
      duplicated_rows_other <-
        dat[[i]][dat[[i]][, "session_only"] != "Eligibility" &
                   (duplicated(dat[[i]][, c("participant_id",
                                            "session_only")])), ]
      duplicated_rows <- rbind(duplicated_rows_eligibility, duplicated_rows_other)
      
      if (nrow(duplicated_rows) > 0) {
        p_ids <- duplicated_rows_eligibility[!is.na(duplicated_rows_eligibility$participant_id),
                                             "participant_id"]
        s_ids <- duplicated_rows_eligibility[is.na(duplicated_rows_eligibility$participant_id),
                                             "session_id"]
        
        cat(nrow(duplicated_rows_eligibility), 
            "duplicated rows at Eligibility for table:", names(dat[i]))
        cat("\n")
        cat("With these ", length(p_ids), "'participant_id' (where available): ", p_ids)
        cat("\n")
        cat("And with ", length(s_ids), "'session_id' (where 'participant_id' unavailable)")
        cat("\n")
        cat(nrow(duplicated_rows_other), 
            "duplicated rows at other time points for table:", names(dat[i]))
        if (nrow(duplicated_rows_other) > 0) {
          cat("\n")
          cat("With these 'participant_id': ", duplicated_rows_other$participant_id)
        }
        cat("\n-------------------------\n")
        
        output[[i]] <- setdiff(dat[[i]], duplicated_rows)
        rownames(output[[i]]) <- 1:nrow(output[[i]])
      } else {
          cat("No duplicated rows for table:", names(dat[i]))
          cat("\n-------------------------\n")
          output[[i]] <- dat[[i]]
        }
    } else if (names(dat[i]) == "email_log") {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id", 
                                             "session_only", 
                                             "email_type", 
                                             "date_sent"), 
                                           "participant_id")
    } else if (names(dat[i]) == "gift_log") {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id", 
                                             "session_and_admin_awarded_info",
                                             "order_id"), 
                                           "participant_id")
    } else if (names(dat[i]) == "js_psych_trial") {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id",
                                             "session_only",
                                             "internal_node_id", 
                                             "stimulus"), 
                                           "participant_id")
    } else if (names(dat[i]) == "task_log") {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id", 
                                             "session_only", 
                                             "task_name", 
                                             "tag"), 
                                           "participant_id")

      duplicated_rows_dass21_as_eligibility <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name", 
                                         "tag")]) &
                   dat[[i]][, "session_only"] == "Eligibility" &
                   dat[[i]][, "task_name"] == "DASS21_AS", ]
      duplicated_rows_other <- 
        dat[[i]][duplicated(dat[[i]][, c("participant_id", 
                                         "session_only", 
                                         "task_name", 
                                         "tag")]) &
                   !(dat[[i]][, "session_only"] == "Eligibility" &
                   dat[[i]][, "task_name"] == "DASS21_AS"), ]
      if (nrow(duplicated_rows_dass21_as_eligibility) > 0 |
          nrow(duplicated_rows_other) > 0) {
        cat(nrow(duplicated_rows_dass21_as_eligibility),
            "duplicated rows for DASS21_AS at Eligibility in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_dass21_as_eligibility$participant_id)
        cat("\n")
        cat(nrow(duplicated_rows_other),
            "duplicated rows for other tasks in table:", names(dat[i]))
        cat("\n")
        cat("With these 'participant_id': ", duplicated_rows_other$participant_id)
        cat("\n-------------------------\n")
      }
    } else if (names(dat[i]) == "study") {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id", 
                                             "current_session"), 
                                           "participant_id")
    } else {
      output[[i]] <- report_remove_dups_df(dat[[i]], 
                                           names(dat[i]), 
                                           c("participant_id", 
                                             "session_only"), 
                                           "participant_id")
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# "dat_no_dup" is list of tables without any duplication based on "target_cols"

dat_no_dup <- report_remove_dups_list(dat)





# ---------------------------------------------------------------------------- #
# Arrange columns and sort tables ----
# ---------------------------------------------------------------------------- #

# TODO: Make basic columns specific and sort tables in consistent way. Consider
# starting with "participant_id", "study_id" (if present), and then "id" (if 
# present). Consider ordering by "id". Also consider removing "X".

# Draft of code from Taylor
# 
# nameList = list('id', "session_name")
# 
# for (i in 1:length(data)){
#   for(colName in nameList){
#     if(colName %in% colnames(data[[i]])){
#       colnames(data[[i]]) <- 
#         c(colName, colnames(data[[i]])[(colnames(data[[i]]) != colName) == TRUE])
#     }
#   }
# }





# ---------------------------------------------------------------------------- #
# Write clean data files ----
# ---------------------------------------------------------------------------- #

# Ensure that consistent format with timezone will output when writing to CSV. 
# Given that these columns will be read back into R as characters, they will need 
# to be reconverted back to POSIXct using the "as.POSIXct" function.

for (i in 1:length(dat)) {
  POSIXct_colnames <- c(names(dat[[i]])[grep("as_POSIXct", names(dat[[i]]))],
                        "system_date_time_earliest",
                        "system_date_time_latest")
  dat[[i]][, POSIXct_colnames] <- format(dat[[i]][, POSIXct_colnames],
                                         usetz = TRUE)
}

# Create folder for clean data

dir.create("./data/clean")

# Write CSV files to clean data folder

for (i in 1:length(dat)) {
  write.csv(dat[[i]], 
            paste0("./data/clean/", names(dat[i]), ".csv"),
            row.names = FALSE)
}