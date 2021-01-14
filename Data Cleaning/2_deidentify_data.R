# ---------------------------------------------------------------------------- #
# Deidentify Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running this script, set your working directory and put the raw data
# obtained from "1_get_raw_data.ipynb" in a folder called "data/raw". The 
# script will import the raw data from that folder and, for files that require
# redaction, output the redacted files into the same folder with "-redacted"
# appended to the file name. The unredacted file can then be manually deleted.

# Rather than changing the structure of the raw data files, the present script 
# is designed to label instances of redaction with "REDACTED". Because it does
# not change the structure of the raw data files, the present script should run
# on redacted raw data files in addition to unredacted raw data files. In this
# way, the script reflects exactly what was redacted.

# Scope: This script is based on data dumped from "calm" database on the Data
# Server ("teachmanlab") on 12/3/2020. The script may need to be updated when
# applied to data downloaded after this date, as there may have been changes
# to the database or newly collected data not accounted for by this script.

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
  warning(paste0("This script is based on ", script_R_version,
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
# Deidentify Data ----
# ---------------------------------------------------------------------------- #

# Before doing any data cleaning, confirm that the raw data are deidentified so
# that subsequent cleaning scripts can be run on a deidentified dataset that can
# be shared and made public. This way all data cleaning steps can be transparent
# and reproducible without compromising identifiable information.

# ---------------------------------------------------------------------------- #
# Determine which columns contain identifiers ----
# ---------------------------------------------------------------------------- #

# Regarding various dates, Bethany indicated on 1/13/2021 that MindTrails is not
# subject to HIPAA regulations, so restrictions on dislcosing dates of service, 
# dates of test measures, and birth years for people over age 89 do not apply.

# TODO: Manually inspect columns and determine which ones may have identifiers

# Consider "button_pressed" column of "angular_training" for rows in which
# "trial_type" is "FillInBlank". No identifiers are present for rows in which
# "trial_type" is "Countdown", "Intro", "MissingLetter", "page", "Question",
# "RandomNonPreviousStatement", "Slider", or "ThoughtBubble".

# Some rows are participant descriptions of an anxious situation for the Use 
# Your Imagination task at "firstSession" before starting training

rows1 <- data$angular_training[data$angular_training$trial_type == "FillInBlank" &
                                 (data$angular_training$step_title == "Use Your Imagination" |
                                    data$angular_training$step_title == "Use your Imagination"), ]

View(rows1)
nrow(rows1)
table(rows1$session)

# Some rows are participant responses for training scenarios at "fifthSession"
# that required filling in a blank (vs. completing a word fragment). Prior to
# 2/15/2019, these responses were indexed not with "step_title" of "scenario"
# but with "step_title" of the scenario's title, which subsequently was stored
# in "stimulus_name".

scenario_titles <- 
  unique(data$angular_training[data$angular_training$step_title == 
                                 "scenario", ]$stimulus_name)

rows2 <- data$angular_training[data$angular_training$trial_type == "FillInBlank" &
                                 (data$angular_training$step_title == "scenario" |
                                    data$angular_training$step_title %in% scenario_titles), ]

View(rows2)
nrow(rows2)
table(rows2$conditioning)

# Henry says that this criterion reflects participants' responses to the Quick
# Thinking Exercise (also called Flexible Thinking Exercise). Not all rows have 
# "step_title" of "Exercise: Quick Thinking" due to a programming error.

rows3 <- data$angular_training[data$angular_training$stimulus_name == 
                                 "flex_thinking_explanations", ]

View(rows3)
nrow(rows3)
table(rows3$conditioning)
table(rows3$step_title)

# Henry says these criteria reflect scenarios created by participants in the
# Write Your Own Scenario exercise in the "TRAINING_CREATE" condition of the 
# TET study. However, one admin/test account ("participant_id" == 8) in the 
# "CONTROL" condition also appears to have completed this task.

rows4_training_create <- data$angular_training[data$angular_training$trial_type == "FillInBlank" &
                                 data$angular_training$conditioning == "TRAINING_CREATE" &
                                 data$angular_training$stimulus_name == "" &
                                 data$angular_training$step_title == "", ]

rows4_all_conditions <- data$angular_training[data$angular_training$trial_type == "FillInBlank" &
                                           data$angular_training$stimulus_name == "" &
                                           data$angular_training$step_title == "", ]

nrow(rows4_all_conditions)
table(rows4_all_conditions$conditioning)

# Henry says this criterion reflects participants' explanations as to why the
# they created occurred in the Write Your Own Scenario exercise in the
# "TRAINING_CREATE" condition of the TET study

rows5 <- data$angular_training[data$angular_training$stimulus_name == 
                                 "training_create_explanations", ]

View(rows5)
nrow(rows5)
table(rows5$conditioning)
table(rows5$trial_type)

# TODO: Seem to be some admin and test accounts that have "FillInBlank" rows in 
# other cases. These rows do not appear after removing these accounts. Three of
# the rows seem to involve the scenario "pub", which may have been removed;
# asked Henry on 1/14/2021.

ignored_ids <- c(rows1$id, rows2$id, rows3$id, rows4_all_conditions$id, rows5$id)

remaining <- data$angular_training[!(data$angular_training$id %in% ignored_ids) &
                             data$angular_training$trial_type == "FillInBlank", ]

View(remaining)
nrow(remaining)










# Consider "compare_to_others" and "worth_per_week" of "assessing_program"

# Consider "difficult_to_understand", "other_feedback", and 
# "technical_difficulties" of "coach_prompt"

# Consider "news" and "social_media" of "covid19"

# Consider "ptp_reason_other" in "demographics"

# Consider "other_place", "other_reason_control", "problems_desc",
# "other_coaching", and "other_help_topic" in "evaluation"

# Consider "other" in "help_seeking"

# Consider "change_help_text", "help_other_text", "other_disorder", and
# "other_why_no_help" in "mental_health_history"

# Consider "end_other_desc", "change_med_desc", "control_desc", and 
# "location_desc" in "reasons_for_ending"

# Consider "not_return_reasons" in "return_intention"

# Consider "other_distraction" and "other_location_desc" in "session_review"











# Regarding "user_agent" in "task_log", Dan Funk said on 1/13/2021 that it is 
# not identifiable. Retain it.

# Regarding "session_id" in "dass21_as" and "oa", Dan Funk said on 1/13/2021
# that it is transient, encrypted by HTTPS, and used to index participants
# before they create an account and get assigned a "participant_id". Retain it.








# TODO: Asked Dan for report of free-response columns on 1/11/20.










# TODO: Redact "order_id" in "gift_log" (and in "error" column of "import_log")
# for security reasons










# TODO: Redact "email", "full_name", "password", and "phone" in "participant" 
# for admin and test accounts that have this data.

View(data$participant[data$participant$email != "" |
                             data$participant$full_name != "" |
                             data$participant$password != "" |
                             !is.na(data$participant$phone), ])










# ---------------------------------------------------------------------------- #
# Remove phone numbers from "sms_log" ----
# ---------------------------------------------------------------------------- #

# Remove phone numbers from "exception" column of "sms_log"

ignored_values <- c("A 'To' phone number is required.",
                    "Authenticate",
                    "The message From/To pair violates a blacklist rule.")

temp <- data$sms_log[data$sms_log$exception != "" &
                       !(data$sms_log$exception %in% ignored_values), ]
temp <- temp[order(temp$exception), ]
View(temp)

data$sms_log[grepl("Permission to send an SMS has not been enabled for the region indicated by the 'To' number:", 
                   data$sms_log$exception), ]$exception <-
  "Permission to send an SMS has not been enabled for the region indicated by the 'To' number: [REDACTED]"

data$sms_log[grepl("The 'To' number", 
                   data$sms_log$exception) &
               grepl("is not a valid phone number.", 
                     data$sms_log$exception), ]$exception <-
  "The 'To' number [REDACTED] is not a valid phone number."

data$sms_log[grepl("To number", data$sms_log$exception) &
               grepl("is not a mobile number", 
                     data$sms_log$exception), ]$exception <- 
  "To number: [REDACTED], is not a mobile number"

deidentified_values <- 
  c("Permission to send an SMS has not been enabled for the region indicated by the 'To' number: [REDACTED]",
    "The 'To' number [REDACTED] is not a valid phone number.",
    "To number: [REDACTED], is not a mobile number")

temp2 <- data$sms_log[data$sms_log$exception != "" &
                        !(data$sms_log$exception %in% ignored_values) &
                        !(data$sms_log$exception %in% deidentified_values), ]
temp2 <- temp2[order(temp2$exception), ]
View(temp2)

# ---------------------------------------------------------------------------- #
# Save deidentified data ----
# ---------------------------------------------------------------------------- #

# TODO: Handle this better for the case that "-redacted.csv" is already in the
# file name. Currently it would save the file as "-redacted-redacted.csv".
# Also, note that it currently also saves the column X named by R.










# Save redacted "sms_log". Remember to manually delete the original file.

sms_log_redacted_filename <- paste0(gsub("*.csv", "",
                                     filenames[grep("sms_log", filenames)]),
                                     "-redacted.csv")

write.csv(data$sms_log, paste0("./data/raw/", sms_log_redacted_filename),
          row.names = FALSE)
          