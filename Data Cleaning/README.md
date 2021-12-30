# README

Author: [Jeremy W. Eberle](https://github.com/jwe4ec)

This README describes centralized data cleaning for the [MindTrails Project](https://mindtrails.virginia.edu/) Calm Thinking Study, an NIMH-funded ([R01MH113752](https://reporter.nih.gov/project-details/9513058)) randomized controlled trial of interpretation bias training for anxious adults (ClinicalTrials.gov [NCT03498651](https://clinicaltrials.gov/ct2/show/NCT03498651?term=NCT03498651&draw=2&rank=1)).

## Table of Contents

- [Data on Open Science Framework](#data-on-open-science-framework)
  - [Private Component](#private-component)
  - [Public Component](#public-component)
- [Coaching-Related Data on UVA Box](#coaching-related-data-on-uva-box)
- [Cleaning Scripts: Setup](#cleaning-scripts-setup-and-file-relations)
- [Cleaning Scripts: Functionality](#cleaning-scripts-functionality)
  - [1_get_raw_data.ipynb](#1_get_raw_data.ipynb)
  - [2_define_functions.R](#2_define_functions.R)
  - [3_redact_data.R](#3_redact_data.R)
  - [4_clean_data.R](#4_clean_data.R)
  - [5_import_clean_data.R](#5_import_clean_data.R)
- [Cleaning and Analysis Considerations](#cleaning-and-analysis-considerations)
  - [For Calm Thinking, TET, and GIDI Studies](#for-calm-thinking-tet-and-gidi-studies)
  - [For Calm Thinking Study](#for-calm-thinking-study)
  - [For TET Study](#for-tet-study)
- [Next Steps](#next-steps)

## Data on Open Science Framework

Raw and centrally cleaned data from the "calm" SQL database are stored in the [MindTrails Calm Thinking Study](https://osf.io/zbd52/) project on the Open Science Framework (OSF). The project has two components, with different permissions: a [Private Component](https://osf.io/jwvnb/) and a [Public Component](https://osf.io/s8v3h/).

### Private Component

The [Private Component](https://osf.io/jwvnb/) contains the full set of 67 raw data tables (with some exceptions) dumped from the "calm" SQL database on the "teachmanlab" Data Server on December 3, 2020 (using [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb)). The folder structure is below.

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
- Add "participant_id" to all participant-specific tables (see [Participant Indexing](#participant-indexing) for more details)
- Correct test accounts (see [Test Accounts](#test-accounts) for more details)
- Remove admin and test accounts
- Label columns redacted by server with "REDACTED_ON_DATA_SERVER"
- Remove irrelevant columns
- Identify any remaining blank columns
- Identify and recode time stamp and date columns
  - Correct blank "session", "date", and "date_submitted" in "js_psych_trial" table for some participants
  - Note that participant 3659 is considered enrolled in TET but lacks screening data (see [TET Participant Flow](#tet_participant_flow) for more details)
  - Recode system-generated timestamps as POSIXct data types in "EST" and user-provided timestamps as POSIXct data types in "UTC"
  - Create variables for filtering on system-generated time stamps (see [Filtering on System-Generated Timestamps](#filtering-on-system-generated-timestamps) for more details)
  - Reformat user-provided dates so that they do not contain empty times, which were not assessed
- Identify and rename session-related columns (see [Session-Related Columns](#session-related-columns) for more details)
- Check for repeated columns across tables (see [Repeated Column Names](#repeated-column-names) for more details)
- Correct study extensions (see [Study Extensions](#study-extensions) for more details)

#### Part II. Filter Data for Desired Study

Part II filters data for the Calm Thinking Study, but the "study_name" can be changed to filter data for the TET or GIDI studies if desired.

- Define enrollment period and participant_ids (see [Enrollment Period](#enrollment-period) for more details)
- Filter all data

#### Part III: Calm Thinking Study-Specific Data Cleaning

Part III cleans the Calm Thinking Study data. Most of the tasks will also be needed for TET and GIDI, but the code will need to be revised.

- Note lack of data for some tables (see [Launch of TET Study](#launch-of-tet-study) for more details)
- Recode "coronavirus" column of "anxiety_triggers" table (see [Launch of TET Study](#launch-of-tet-study) for more details)
- Add participant information
  - Create variable describing how participants were assigned to condition
  - Create variable to differentiate soft and official launch participants
  - Create variable describing how participants were classified as high/low dropout risk (see [Dropout Risk](#dropout-risk) for more details)
  - Create indicator variable for coaching accounts
- Exclude participants
  - Confirm that accounts for coaches have already been removed
  - Identify official-launch participant_ids and exclude soft-launch participants
- Edit participant information: Participant spanning two studies
- Obtain time of last collected data
- Identify participants with inaccurate "active" column (see ["active" Column](#active-column) for more details)
- Check "conditioning" values in "angular_training" and "study" tables
  - Note that "conditioning" is blank for some rows of "angular_training"
  - Check for participants in "TRAINING" after "firstSession", which should not occur
  - Check that condition stays the same from "secondSession" through "fifthSession"
  - Check for switching between "CONTROL" and another condition, which should not occur (see [Condition Switching](#condition-switching) for more details)
  - Check for "conditioning" at Session 5 in "angular_training" table not matching "conditioning" at "COMPLETE" in "study" table
  - Check for participants in "NONE", which should not occur
- Clean "reasons_for_ending" table
- Exclude screenings resembling bots
- Identify and remove nonmeaningful duplicates
- Handle multiple screenings (see [Multiple Screening Attempts](#multiple-screening-attempts) for details)
  - Correct "participant_id" not linking to all screening attempts for corresponding "session_id"
  - For duplicated values on DASS-21-AS items, "over18", and "time_on_page" for a given "session_id" and "session_only", keep last row
  - Compute number of multiple rows per "session_id" at screening, mean "time_on_page" across these rows, and number of unique rows
  - Compute column mean of unique values on DASS-21-AS items per "session_id"
  - Compute DASS-21-AS total score "dass21_as_total" (as computed by system, not accounting for multiple entries)
  - Multiply "dass21_as_total" score by 2 to compute "dass21_as_total_interp" for interpretation against eligibility criterion
  - Create indicator "dass21_as_eligible" to reflect eligibility on DASS-21-AS
  - Compute DASS-21-AS total score "dass21_as_total_anal" for analysis (accounting for multiple entries at screening)
- Report participant flow up to enrollment and identify analysis exclusions (see [Participant Flow and Analysis Exclusions](#participant-flow-and-analysis-exclusions) for details)
  - Report number of participants screened, enrolled, and not enrolled (for not enrolled, report reason based on most recent entry)
  - Identify session_ids of participants who did not enroll to be excluded from any analysis of screening data
  - Identify participant_ids of participants who did enroll to be excluded from any analysis
  - Create indicator "exclude_analysis" to reflect participants who should be excluded from analysis and add it to "participant" table

TODO: Jeremy to continue here

- Identify unexpected multiple entries
- Investigate unexpected multiple entries
- Handle unexpected multiple entries
- Arrange columns and sort tables

### 5_import_clean_data.R

This R script imports the intermediately cleaned Calm Thinking Study data and converts system-generated timestamps back to POSIXct data types given that [4_clean_data.R](code/4_clean_data.R) outputted them as characters. As such, this script serves as a starting point for further cleaning and analysis.

## Cleaning and Analysis Considerations

This section highlights some considerations prompted by data cleaning that may be relevant to further cleaning or to analysis. Refer to the actual script for more details.

### For Calm Thinking, TET, and GIDI Studies

#### Indexing Participants

Part I of [4_clean_data.R](code/4_clean_data.R) ensured all participant-specific data is indexed by "participant_id". Use "participant_id" (not "study_id") to refer to participants.

#### Filtering on System-Generated Timestamps

Part I of [4_clean_data.R](code/4_clean_data.R) creates variables "system_date_time_earliest" and "system_date_time_latest" in each table given that some tables have multiple system-generated timestamps. "system_date_time_earliest" and "system_date_time_latest" represent the earliest and latest time stamps, respectively, for each row in the table. These can be useful for filtering the entire dataset on certain timestamps.

#### Session-Related Columns

Part I of [4_clean_data.R](code/4_clean_data.R) revealed that in some tables (e.g., "dass21_as") "session" conflates time point with other information (e.g., eligibility status). In these tables, "session" was renamed to reflect the information it contains (e.g., "session_and_eligibility_status"), and "session_only" was created to reflect only the time point. In some tables (i.e., "angular_training", "gift_log") it was unclear how to extract the time point, so these tables lack "session_only". In tables where "session" did not conflate time point with other information, "session" was renamed "session_only".

Thus, "session_only" is the preferred column for filtering by time point, but not all tables have it. Moreover, "session_only" includes values of "COMPLETE" in some tables (i.e., "action_log", "email_log") but not others (i.e., "task_log"). As a result, care must be taken when filtering data by time point.

#### Repeated Column Names

Part I of [4_clean_data.R](code/4_clean_data.R) revealed that although some tables contain the same column name, the meanings of the columns differ. As a result, care must be taken when comparing columns between tables. See the cleaning script for explanations of repeated column names.

#### Study Extensions

Part I of [4_clean_data.R](code/4_clean_data.R) corrects the "study_extension" for participants 2004 and 2005, who are enrolled in Calm Thinking.

#### Enrollment Period

Part II of [4_clean_data.R](code/4_clean_data.R) defines the enrollment periods for Calm Thinking, TET, and GIDI in the "America/New_York" timezone, as this is timezone where the study team is based. "America/New_York" is preferred to "EST" because "America/New_York" accounts for switches between "EST" and "EDT". By contrast, system-generated timestamps are stored only in "EST" because this is how they are stored in the "calm" SQL database on the "teachmanlab" Data Server.

The enrollment period is needed to filter screening data, most of which is not indexed by "participant_id" but is required for the participant flow diagram.

### For Calm Thinking Study

#### Test Accounts

Part I of [4_clean_data.R](code/4_clean_data.R) corrected test accounts: Participant 1097 should not be a test account and participant 1663 should.

#### Launch of TET Study

Part III of [4_clean_data.R](code/4_clean_data.R) revealed that Calm Thinking participants who accessed the site after TET launched on 4/7/2020 completed some tasks (e.g., "covid19" table, "coronavirus" in "anxiety_triggers" table) designed for TET participants. The data are retained to reflect the tasks participants completed.

#### Dropout Risk

Part III of [4_clean_data.R](code/4_clean_data.R) indicates that some official-launch participants were manually classified as high risk for dropout (vs. classified by the attrition algorithm) and then Stage 2 randomized to condition. See "risk_classification_method" in "participant" table.

#### "active" Column

Part III of [4_clean_data.R](code/4_clean_data.R) indicates that for "active" in "participant" table, participants 891, 1627, 1852 are mislabeled as active and that participants 191, 329, 723 are mislabeled as inactive. However, because the "active" column may have affected final reminder emails or notices of account closure, the mislabeled data are retained to reflect potential unexpected behavior of the site for these participants.

#### Condition Switching

Part III of [4_clean_data.R](code/4_clean_data.R) revealed various cases of unexpected values for "conditioning" in "angular_training". See cleaning script for details.

Importantly, participant 382 received CBM-I training at Session 1 and then psychoeducation at Sessions 2-5. How this participant is handled will depend on the specific analysis.

#### Multiple Screening Attempts

TODO

#### Participant Flow and Analysis Exclusions

TODO

### For TET Study

#### Participant Flow

Part I of [4_clean_data.R](code/4_clean_data.R) revealed that participant 3659 lacks screening data but is considered officially enrolled in TET. Thus, care should be taken to ensure that this participant is reflected appropriately in the TET flow diagram.

## Next Steps

TODO
