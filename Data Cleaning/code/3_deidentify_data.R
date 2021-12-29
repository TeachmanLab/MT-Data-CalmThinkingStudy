# ---------------------------------------------------------------------------- #
# Redact Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running this script, restart R (CTRL+SHIFT+F10 on Windows), set your
# working directory to the parent folder, and ensure that the raw data obtained
# from "1_get_raw_data.ipynb" are in "./data/raw". This script will import raw 
# data from "./data/raw" and output redacted data into "./data/redacted".

# For raw data files that contain potential identifiers, this script redacts the
# relevant data so that subsequent cleaning can be run on a dataset that can be 
# shared and made public. This way all data cleaning steps are transparent and 
# reproducible without compromising potentially identifiable information.

# Rather than changing the structure of the raw data files, this script replaces
# potentially identifiable values with the value "REDACTED_BY_CLEANING_SCRIPT".
# Because it does not change the raw data structure, the script should run on 
# already redacted data files in addition to raw data files without error.

# Scope: This script is based on data dumped from "calm" database on the Data
# Server ("teachmanlab") on 12/3/2020. The script may need to be updated when
# applied to data downloaded after this date, as there may have been changes
# to the database or newly collected data not accounted for by this script.

# Note: Some data were already redacted upon export from the "ws-02" Web Server
# to the "teachmanlab" Data Server. The following script "4_clean_data.R" labels
# the values of the affected columns as "REDACTED_ON_DATA_SERVER". The present 
# script did not perform that redaction.

# ---------------------------------------------------------------------------- #
# Store working directory, install correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./MT-Data-CalmThinkingStudy/Data Cleaning/code/2_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import raw data ----
# ---------------------------------------------------------------------------- #

# Obtain file names of raw CSV data files

raw_data_dir <- paste0(wd_dir, "/data/raw")
filenames <- list.files(raw_data_dir, pattern = "*.csv", full.names = FALSE)

# Import data files into list

dat <- lapply(paste0(raw_data_dir, "/", filenames), read.csv)

# Name data tables in list

split_char <- "-"
names(dat) <- unlist(lapply(filenames,
                            function(x) {
                              unlist(strsplit(x, 
                                              split = split_char, 
                                              fixed = FALSE))[1]
                            }))

# Report names of imported tables

cat("Imported tables: ")
names(dat)

# ---------------------------------------------------------------------------- #
# Specify columns to retain ----
# ---------------------------------------------------------------------------- #

# Regarding date-related columns across tables, Bethany Teachman indicated on 
# 1/13/2021 that MindTrails is not subject to HIPAA regulations, so restrictions 
# on disclosing dates of training, dates of test measures, and birth years for 
# any participants over age 89 do not apply

# Regarding "user_agent" in "task_log" table, Dan Funk said on 1/13/2021 that it 
# is not identifiable. Retain it.

# Regarding "session_id" in "dass21_as" and "oa" tables, Dan Funk said on 1/13/2021
# that it is transient, encrypted by HTTPS, and used to index participants before 
# they create an account and get assigned a "participant_id". Retain it.

# "worth_per_week" of "assessing_program" table has what appear to be some unclear
# responses that will need to addressed in analysis-specific data cleaning, but
# otherwise the responses are roughly numeric. Retain them.

table(dat$assessing_program$worth_per_week)

# "news" and "social_media" of "covid19" table have what appear to be some
# unclear responses that will need to be addressed in analysis-specific data
# cleaning, but otherwise the responses are roughly numeric. Retain them.

table(dat$covid19$news)
table(dat$covid19$social_media)

# ---------------------------------------------------------------------------- #
# Determine "button_pressed" data in "angular_training" table to redact ----
# ---------------------------------------------------------------------------- #

# "button_pressed" column of "angular_training" table has some free-text responses. 
# It has no identifiers for rows with "trial_type" values listed below. However, we 
# will redact "button_pressed" for rows in which "trial_type" is "FillInBlank", 
# which contain various kinds of free-text responses (described below).

checked_trial_type_rows <- c("Countdown", "Intro", "MissingLetter",
                             "page", "Question", "RandomNonPreviousStatement",
                             "Slider", "ThoughtBubble")

table(dat$angular_training$trial_type[!(dat$angular_training$trial_type %in% 
                                          checked_trial_type_rows)],
      useNA = "always")

#   1. Some rows are participant descriptions of an anxious situation for the Use 
#   Your Imagination task at "firstSession" before starting training

rows1 <- dat$angular_training[dat$angular_training$trial_type == 
                                "FillInBlank" &
                                (dat$angular_training$step_title == 
                                   "Use Your Imagination" |
                                   dat$angular_training$step_title == 
                                   "Use your Imagination"), ]

#   Note: Analysis-specific data cleaning will need to address issues with entries
#   (a) for unexpected values of "conditioning" and (b) at sessions other than 
#   "firstSession". For now, we focus on deidentification.

table(rows1$conditioning)
table(rows1$session)

#   2. Some rows are participant responses for training scenarios at "fifthSession"
#   that required filling in a blank (vs. completing a word fragment). Prior to
#   2/15/2019, these responses were indexed not with a "step_title" value of 
#   "scenario", but with a "step_title" value of the scenario's title, which 
#   subsequently was stored in "stimulus_name". Among the scenario titles prior 
#   to this change was "pub"; after this change, "pub" was renamed to "bar".

scenario_titles <- 
  c(unique(dat$angular_training[dat$angular_training$step_title == 
                                  "scenario", ]$stimulus_name), "pub")

rows2 <- dat$angular_training[dat$angular_training$trial_type == "FillInBlank" &
                                (dat$angular_training$step_title == "scenario" |
                                   dat$angular_training$step_title %in% 
                                   scenario_titles), ]

#   3. Henry Behan said that this criterion reflects participants' responses to 
#   the Quick Thinking Exercise (also called Flexible Thinking Exercise)

rows3 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "flex_thinking_explanations", ]

table(rows3$trial_type)

#   Note: Analysis-specific data cleaning will need to address these issues: (a) 
#   not all rows have "step_title" of "Exercise: Quick Thinking" due to programming 
#   error; (b) some Calm Thinking participants appear to have completed this task; 
#   and (c) for "CONTROL" participants, "session" is populated by "flexible_thinking", 
#   so the session number is unclear.

table(rows3$step_title)

table(rows3$conditioning)

table(rows3$session)
table(rows3[rows3$session == "flexible_thinking", ]$conditioning)

#   4. Henry Behan said these criteria reflect scenarios created by participants 
#   in the Write Your Own Scenario exercise in the "TRAINING_CREATE" condition of 
#   the TET study.

rows4_training_create <- dat$angular_training[dat$angular_training$trial_type == 
                                                "FillInBlank" &
                                                dat$angular_training$conditioning == 
                                                  "TRAINING_CREATE" &
                                                dat$angular_training$stimulus_name == 
                                                  "" &
                                                dat$angular_training$step_title == 
                                                  "", ]

#   However, one admin/test account in "CONTROL" also appears to have completed 
#   this task, so we cannot restrict to "TRAINING_CREATE" for deidentification

rows4_all_conditions <- dat$angular_training[dat$angular_training$trial_type == 
                                               "FillInBlank" &
                                               dat$angular_training$stimulus_name == 
                                                 "" &
                                               dat$angular_training$step_title == 
                                                 "", ]

table(rows4_all_conditions$conditioning)

#   5. Henry Behan said this criterion reflects participants' explanations as to 
#   why the scenario they created occurred in the Write Your Own Scenario exercise 
#   in the "TRAINING_CREATE" condition of the TET study

rows5 <- dat$angular_training[dat$angular_training$stimulus_name == 
                                "training_create_explanations", ]

table(rows5$trial_type)

# Confirm no rows remain unaccounted for

ignored_ids <- c(rows1$id, rows2$id, rows3$id, rows4_all_conditions$id, rows5$id)

remaining <- dat$angular_training[!(dat$angular_training$id %in% ignored_ids) &
                                    dat$angular_training$trial_type == 
                                      "FillInBlank", ]

nrow(remaining) == 0

# ---------------------------------------------------------------------------- #
# Redact "button_pressed" data for "FillInBlank" rows in "angular_training" ----
# ---------------------------------------------------------------------------- #

dat$angular_training$button_pressed[dat$angular_training$trial_type == "FillInBlank"] <- 
  "REDACTED_BY_CLEANING_SCRIPT"

# ---------------------------------------------------------------------------- #
# Redact columns in other tables ----
# ---------------------------------------------------------------------------- #

# Specify a character vector of columns ("<table_name>$<column_name>") whose 
# values should be replaced with "REDACTED_BY_CLEANING_SCRIPT". If no column is 
# to be redacted, specify NULL without quotes (i.e., "redact_columns <- NULL").

# The following columns should be redacted because they have free-text responses
# that are currently too time consuming to manually check for deidentification

redact_free_text_cols <- c("assessing_program$compare_to_others",
                           "coach_prompt$difficult_to_understand",
                           "coach_prompt$other_feedback",
                           "coach_prompt$technical_difficulties",
                           "demographics$ptp_reason_other",
                           "evaluation$other_place",
                           "evaluation$other_reason_control",
                           "evaluation$problems_desc",
                           "evaluation$other_coaching",
                           "evaluation$other_help_topic",
                           "help_seeking$other",
                           "mental_health_history$change_help_text",
                           "mental_health_history$help_other_text",
                           "mental_health_history$other_disorder",
                           "mental_health_history$other_why_no_help",
                           "reasons_for_ending$end_other_desc",
                           "reasons_for_ending$change_med_desc",
                           "reasons_for_ending$control_desc",
                           "reasons_for_ending$location_desc",
                           "return_intention$not_return_reasons",
                           "session_review$other_distraction",
                           "session_review$other_location_desc")

# Redact "email", "full_name", "password", and "phone" in "participant" table
# for admin and test accounts that have this data

redact_participant_cols <- c("participant$email", "participant$full_name",
                             "participant$password", "participant$phone")

# Collect all columns to redact

redact_cols <- c(redact_free_text_cols, redact_participant_cols)

# Define function to recode any meaningful values in given columns to be redacted
# as "REDACTED_BY_CLEANING_SCRIPT". Regarding meaningful values, note that in some
# columns "N/A" appears to be a programmed response option, whereas in others it
# does not (also, users could still manually type some variation of "N/A"); thus, 
# cases of "N/A" should not be interpreted as exhaustive of the category.

redact_columns <- function(dat, redact_cols) {
  output <- vector("list", length(dat))
  
  for (i in 1:length(dat)) {
    output[[i]] <- dat[[i]]
    
    for (j in 1:length(dat[[i]])) {
      table_i_name <- names(dat[i])
      column_j_name <- names(dat[[i]][j])
      table_i_column_j_name <- paste0(table_i_name, "$", column_j_name)
      
      if (table_i_column_j_name %in% redact_cols) {
        output[[i]][!(output[[i]][, column_j_name] %in% c("", "N/A", "555")) &
                      !(is.na(output[[i]][, column_j_name])), 
                    column_j_name] <- "REDACTED_BY_CLEANING_SCRIPT"
      }
    }
  }
  
  names(output) <- names(dat)
  return(output)
}

# Run function

dat <- redact_columns(dat, redact_cols)

# ---------------------------------------------------------------------------- #
# Redact "order_id" data from "gift_log" and "import_log" ----
# ---------------------------------------------------------------------------- #

# All "order_id" values start with "RA"

length(dat$gift_log$order_id[dat$gift_log$order_id != ""]) ==
  length(grep("RA", dat$gift_log$order_id))

# Redact "order_id" in "gift_log" table for security reasons

dat$gift_log$order_id[!(dat$gift_log$order_id == "")] <- "REDACTED_BY_CLEANING_SCRIPT"

# Redact "order_id" values from "error" column of "import_log" table

nrow(dat$import_log[grep("RA", dat$import_log$error), ]) ==
  nrow(dat$import_log[grep("orderId\":\"RA.*\",\"sessionName", dat$import_log$error), ])

dat$import_log$error <- sub("orderId\":\"RA.*\",\"sessionName", 
                            "orderId\":\"REDACTED_BY_CLEANING_SCRIPT\",\"sessionName",
                            dat$import_log$error)

nrow(dat$import_log[grep("RA", dat$import_log$error), ]) == 0

# ---------------------------------------------------------------------------- #
# Redact phone numbers from "sms_log" ----
# ---------------------------------------------------------------------------- #

# Redact phone numbers from "exception" of "sms_log" table

ignored_values <- c("A 'To' phone number is required.",
                    "Authenticate",
                    "The message From/To pair violates a blacklist rule.")

temp <- dat$sms_log[dat$sms_log$exception != "" &
                      !(dat$sms_log$exception %in% ignored_values), ]
temp <- temp[order(temp$exception), ]
nrow(temp)

dat$sms_log[grepl(paste0("Permission to send an SMS has not been enabled for the ",
                         "region indicated by the 'To' number:"), 
                  dat$sms_log$exception), ]$exception <-
  paste0("Permission to send an SMS has not been enabled for the ",
         "region indicated by the 'To' number: [REDACTED_BY_CLEANING_SCRIPT]")

dat$sms_log[grepl("The 'To' number", 
                  dat$sms_log$exception) &
               grepl("is not a valid phone number.", 
                     dat$sms_log$exception), ]$exception <-
  "The 'To' number [REDACTED_BY_CLEANING_SCRIPT] is not a valid phone number."

dat$sms_log[grepl("To number", dat$sms_log$exception) &
               grepl("is not a mobile number", 
                     dat$sms_log$exception), ]$exception <- 
  "To number: [REDACTED_BY_CLEANING_SCRIPT], is not a mobile number"

deidentified_values <- 
  c(paste0("Permission to send an SMS has not been enabled for the region ",
           "indicated by the 'To' number: [REDACTED_BY_CLEANING_SCRIPT]"),
    "The 'To' number [REDACTED_BY_CLEANING_SCRIPT] is not a valid phone number.",
    "To number: [REDACTED_BY_CLEANING_SCRIPT], is not a mobile number")

temp2 <- dat$sms_log[dat$sms_log$exception != "" &
                       !(dat$sms_log$exception %in% c(ignored_values, 
                                                      deidentified_values)), ]
temp2 <- temp2[order(temp2$exception), ]
nrow(temp2) == 0

# ---------------------------------------------------------------------------- #
# List tables that have been redacted ----
# ---------------------------------------------------------------------------- #

# List all tables that have been redacted by this script so that the redacted
# files can be named appropriately when outputted

redacted_tbls <- c("angular_training",
                   unique(gsub("\\$.*", "", redact_cols)),
                   "gift_log",
                   "import_log",
                   "sms_log")

# ---------------------------------------------------------------------------- #
# Export redacted data ----
# ---------------------------------------------------------------------------- #

# Prepare filenames, preventing "-redacted" from being appended multiple times
# if the script is run on any files that had already been redacted

redacted_filename_stems <- sub("*.csv", "", 
                               filenames[grep(paste0(redacted_tbls, collapse = "-|"), 
                                              filenames)])

redacted_filename_stems <- gsub("-redacted", "", redacted_filename_stems)

redacted_filenames <- paste0(redacted_filename_stems, "-redacted.csv")

# Write redacted CSV files. Remember not to share corresponding raw data files.

dir.create("./data/redacted")

dat_red <- dat[names(dat) %in% redacted_tbls]

for (i in 1:length(dat_red)) {
  write.csv(dat_red[[i]], 
            paste0("./data/redacted/", redacted_filenames[i]),
            row.names = FALSE)
}