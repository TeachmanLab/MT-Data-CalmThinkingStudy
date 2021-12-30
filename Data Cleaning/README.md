# README

Author: [Jeremy W. Eberle](https://github.com/jwe4ec)

This README describes centralized data cleaning for the [MindTrails Project](https://mindtrails.virginia.edu/) Calm Thinking Study, an NIMH-funded ([R01MH113752](https://reporter.nih.gov/project-details/9513058)) randomized controlled trial of interpretation bias training for anxious adults (ClinicalTrials.gov [NCT03498651](https://clinicaltrials.gov/ct2/show/NCT03498651?term=NCT03498651&draw=2&rank=1)).

## Table of Contents

1. [Data on Open Science Framework](#data-on-open-science-framework)
2. [Coaching-Related Data on UVA Box](#coaching-related-data-on-uva-box)
3. [Cleaning Scripts: Setup](#cleaning-scripts-setup-and-file-relations)
4. [Cleaning Scripts: Functionality](#cleaning-scripts-functionality)
5. [Next Steps](#next-steps)

## Data on Open Science Framework

Centrally cleaned data from the "teachmanlab" Data Server are stored in the [MindTrails Calm Thinking Study](https://osf.io/zbd52/) project on the Open Science Framework (OSF). The project has two components, with different permissions: a [Private Component](https://osf.io/jwvnb/) and a [Public Component](https://osf.io/s8v3h/).

### Private Component

The [Private Component](https://osf.io/jwvnb/) contains the full set of raw data tables (with some exceptions) dumped from the "calm" SQL database on the "teachmanlab" Data Server on December 3, 2020 (using [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb)). The folder structure is below.

The exceptions are that only redacted versions of "gift_log", "import_log", and "sms_log" tables are included (redacted using [3_redact_data.R](code/3_redact_data.R)). 

```
.
├── data
└── └── 1_raw_full               # 67 CSV files (e.g., "dass21_as-03_12_2020.csv", "angular_training-03_12_2020.csv", 
                                 #   "gift_log-03_12_2020-redacted.csv")
```

Researchers can request access to files on this component by contacting the study team ([studyteam@mindtrails.org](mailto:studyteam@mindtrails.org)).

### Public Component

The [Public Component](https://osf.io/s8v3h/) contains a partial set of raw data tables (i.e., those obtained using [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb) that did not need redaction), redacted data tables (from [3_redact_data.R](code/3_redact_data.R)), and intermediately clean data tables (from [4_clean_data.R](code/4_clean_data.R)). The folder structure is below.

Note: Tables in the `1_raw_full` folder of the [Private Component](#private-component) that are not in the `1_raw_partial` folder of this [Public Component](https://osf.io/s8v3h/) contain free-text responses that may or may not have identifiers. In the [Public Component](https://osf.io/s8v3h/), redacted versions of such tables are in `2_redacted`.

```
.
├── data                    
├── ├── 1_raw_partial            # 53 CSV files (did not need redaction; e.g., "dass21_as-03_12_2020.csv")
├── ├── 2_redacted               # 14 CSV files (needed redaction; e.g., "angular_training-03_12_2020-redacted.csv", 
│   │                            #   "gift_log-03_12_2020-redacted.csv")
├── ├── 3_intermediate_clean     # 50 CSV files (note: 17 files were deemed irrelevant and removed during cleaning)
├── materials
├── ├── appendices               # Appendices
└── └── codebooks                # Codebooks
```

## Coaching-Related Data on UVA Box

Additional coaching-related data were not centrally cleaned. These measures (Coach Session Review, Coach Session Tracking, Nonresponder Coaching Survey) were collected via Qualtrics or Google Sheets and are stored privately in a [MindTrails UVA Box folder](https://virginia.app.box.com/folder/118765162561).

For more details about these data, cleaned by Alex Werntz and Allie Silverman, see the Measures Appendix and Measures Codebook on the [Public Component](#public-component). Researchers can request access to the data by contacting the study team ([studyteam@mindtrails.org](mailto:studyteam@mindtrails.org)).

## Cleaning Scripts: Setup and File Relations

The scripts in the [`code`](code) subfolder of this [Data Cleaning](https://github.com/jwe4ec/MT-Data-CalmThinkingStudy/tree/jeremy/Data%20Cleaning) folder import the full raw data files, redact certain files, and clean the redacted and remaining raw files to yield intermediately clean files. The resulting files are considered only intermediately cleaned because further analysis-specific cleaning will be required for any given analysis.

To run the cleaning scripts, create a parent folder (with any desired name, indicated by `.` below) with two subfolders: `data` and `code`. The working directory must be set to the parent folder in order for the scripts to import and export data correctly using relative file paths.

```
.                                # Parent folder (i.e., working directory)
├── data                         # Data subfolder
└── code                         # Code subfolder
```

If you have access to the full raw data (from the [Private Component](#private-component)), you can reproduce the redaction. Put all the raw data files in a subfolder of `data` called `1_raw_full`. When you run the scripts, [3_redact_data.R](code/3_redact_data.R) will create `2_redacted` and files therein, and [4_clean_data.R](code/4_clean_data.R) will create `3_intermediate_clean` and files therein.

```
.
├── data                    
├── ├── 1_raw_full               # 67 CSV files from Private Component
├── ├── (2_redacted)             # Folder with 14 CSV files will be created by "3_redact_data.R"
├── └── (3_intermediate_clean)   # Folder with 50 CSV files will be created by "4_clean_data.R"
└── ...
```

If you have access to the partial raw data and the redacted data (from the [Public Component](#public-component)), put the partial raw data files in a subfolder of `data` called `1_raw_partial` and the redacted data files in a subfolder called `2_redacted`. When you run the scripts, [4_clean_data.R](code/4_clean_data.R) will create `3_intermediate_clean` and files therein.

```
.
├── data                    
├── ├── 1_raw_partial            # 53 CSV files from Public Component
├── ├── 2_redacted               # 14 CSV files from Public Component
├── └── (3_intermediate_clean)   # Folder with 50 CSV files will be created by "4_clean_data.R"
└── ...
```

Put the cleaning scripts in the `code` subfolder of the parent folder. The scripts are to be run in the order listed. Assuming you already have full or partial raw data, start with [2_define_functions.R](code/2_define_functions.R). If you have full raw data, run [3_redact_data.R](3_redact_data.R) next; otherwise, skip it. Run the remaining scripts.

At the top of each R script, restart R (CTRL+SHIFT+F10 on Windows) and set your working directory to the parent folder (CTRL+SHIFT+H on Windows).

```
.
├── ...
├── code
├── ├── 1_get_raw_data.ipynb     # Dump 67 CSV files from "calm" SQL database on Data Server (for "1_raw_full")
├── ├── 2_define_functions.R     # Define functions for use by subsequent R scripts
├── ├── 3_redact_data.R          # Redact 14 CSV files from "1_raw_full" and output them to "2_redacted"
├── ├── 4_clean_data.R           # Clean 14 CSV files from "2_redacted" and 53 CSV files from "1_raw_full"
│   │                            #   or "1_raw_partial" and output 50 CSV files to "3_intermediate_clean"
└── └── 5_import_clean_data.R    # Import 50 CSV files from "3_intermediate_clean"
```

## Cleaning Scripts: Functionality

### 1_get_raw_data.ipynb

This Jupyter Notebook script (author: [Sonia Baee](https://github.com/soniabaee)) dumps the full set of 67 raw CSV data files from the "calm" SQL database on the "teachmanlab" Data Server as of December 3, 2020. This dump defines the end of data collection for the study. [4_clean_data.R](code/4_clean_data.R) below identifies that the last system-generated timestamp for a Calm Thinking participant in this dataset is "2020-11-13 22:13:27 EST".

### 2_define_functions.R

This R script defines functions for use by subsequent R scripts, which source this file at the top of each script.

### 3_redact_data.R

This R script performs the following functions. Here, *redact* means to replace relevant values with "REDACTED_BY_CLEANING_SCRIPT", retaining the structure of the raw data files.

- Specify columns to retain that were considered for redaction
- Determine which "button_pressed" data in "angular_training" table to redact
- Redact "button_pressed" data for "FillInBlank" rows in "angular_training" table
- Redact free-text responses for certain other columns that may contain identifiers
- Redact "order_id" data from "gift_log" and "import_log" tables
- Redact phone numbers from "sms_log" table

Note: So that "order_id" and phone number data are not retained in the full raw data, only redacted versions of the "gift_log", "import_log", and "sms_log" tables are stored in `1_raw_full`, and the unredacted versions dumped by [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb) were deleted.

By contrast, unredacted versions of other redacted tables are retained in `1_raw_full` on the [Private Component](#private-component) because these tables contain free-text responses that may or may not contain identifiers. Notably, participants were not asked to provide identifiers in their responses.

### 4_clean_data.R

This R script performs the following functions.

#### Part I. Database-Wide Data Cleaning

Part I applies to data for all three studies (Calm Thinking, TET, GIDI) in the "calm" SQL database.

- Remove irrelevant tables
- Rename "id" columns in "participant" and "study" tables
- Add "participant_id" to all participant-specific tables
- Correct test accounts
- Remove admin and test accounts
- Label columns redacted by server with "REDACTED_ON_DATA_SERVER"
- Remove irrelevant columns
- Identify any remaining blank columns
- Identify and recode time stamp and date columns
  - Including create new variables for filtering data based on system-generated time stamps
- Identify and rename session-related columns
- Check for repeated columns across tables
- Correct study extensions

#### Part II. Filter Data for Desired Study

In this case, data are filtered for the Calm Thinking Study.

- Define enrollment period and participant_ids
- Filter all data

#### Part III: Calm Thinking Study-Specific Data Cleaning

- Note lack of data for some tables
- Recode "coronavirus" column of "anxiety_triggers" table
  - Note: Column was not intended for Calm Thinking participants
- Add participant information
  - Create variable describing how participants were assigned to condition
  - Create variable to differentiate soft and official launch participants
  - Create variable describing how participants were classified as high/low dropout risk
  - Create indicator variable for coaching accounts
- Exclude participants
  - Confirm that accounts for coaches have already been removed
  - Identify official-launch participant_ids and exclude soft-launch participants
- Edit participant information: Participant spanning two studies
- Obtain time of last collected data
- Identify participants with inaccurate "active" column
- Check "conditioning" values in "angular_training" and "study" tables
- Clean "reasons_for_ending" table
- Exclude screenings resembling bots
- Identify and remove nonmeaningful duplicates
- Handle multiple screenings and report participant flow up to enrollment
- Identify unexpected multiple entries
- Investigate unexpected multiple entries
- Handle unexpected multiple entries
- Arrange columns and sort tables

### 5_import_clean_data.R

TODO

## Next Steps

TODO
