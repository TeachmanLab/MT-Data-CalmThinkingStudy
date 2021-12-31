# README

Author: [Jeremy W. Eberle](https://github.com/jwe4ec)

This README describes centralized data cleaning for the [MindTrails Project](https://mindtrails.virginia.edu/) Calm Thinking Study, an NIMH-funded ([R01MH113752](https://reporter.nih.gov/project-details/9513058)) sequential multiple assignment randomized controlled trial of web-based interpretation bias training for anxious adults (ClinicalTrials.gov [NCT03498651](https://clinicaltrials.gov/ct2/show/NCT03498651?term=NCT03498651&draw=2&rank=1)).

## Table of Contents

- [Data on Open Science Framework](#data-on-open-science-framework)
  - [Private Component](#private-component)
  - [Public Component](#public-component)
- [Coaching-Related Data on UVA Box](#coaching-related-data-on-uva-box)
- [Cleaning Scripts: Setup and File Relations](#cleaning-scripts-setup-and-file-relations)
- [Cleaning Scripts: Functionality](#cleaning-scripts-functionality)
  - [1_get_raw_data.ipynb](#1_get_raw_dataipynb)
  - [2_define_functions.R](#2_define_functionsR)
  - [3_redact_data.R](#3_redact_dataR)
  - [4_clean_data.R](#4_clean_dataR)
  - [5_import_clean_data.R](#5_import_clean_dataR)
- [Further Cleaning and Analysis Considerations](#further-cleaning-and-analysis-considerations)
  - [For Calm Thinking, TET, and GIDI Studies](#for-calm-thinking-tet-and-gidi-studies)
  - [For Calm Thinking Study](#for-calm-thinking-study)
  - [For TET and GIDI Studies](#for-tet-and-gidi-studies)
- [Next Steps](#next-steps)
- [Resources](#resources)
  - [Appendices and Codebooks](#appendices-and-codebooks)
  - [MindTrails Changes and Issues Log](#mindtrails-changes-and-issues-log)
  - [MindTrails Wiki](#mindtrails-wiki)
  - [Outtakes: Clean "angular_training"](#outtakes-clean-angular-training)
  - [Outtakes: Create Reports](#outtakes-create-reports)

## Data on Open Science Framework

Raw and centrally cleaned data from the "calm" SQL database are stored in the [MindTrails Calm Thinking Study](https://osf.io/zbd52/) project on the Open Science Framework (OSF). The project has two components, with different permissions: a [Private Component](https://osf.io/jwvnb/) and a [Public Component](https://osf.io/s8v3h/).

### Private Component

The [Private Component](https://osf.io/jwvnb/) contains the full set of 67 raw data tables (with some exceptions) dumped from the "calm" SQL database on the "teachmanlab" Data Server on December 3, 2020 (using [1_get_raw_data.ipynb](#1_get_raw_dataipynb)). The folder structure is below.

The exceptions are that only redacted versions of "gift_log", "import_log", and "sms_log" tables are included (redacted using [3_redact_data.R](#3_redact_dataR)). 

```
.
├── data
└── └── 1_raw_full               # 67 CSV files (e.g., "dass21_as-03_12_2020.csv", "angular_training-03_12_2020.csv", 
                                 #   "gift_log-03_12_2020-redacted.csv")
```

Researchers can request access to files on this component by contacting the study team ([studyteam@mindtrails.org](mailto:studyteam@mindtrails.org)).

### Public Component

The [Public Component](https://osf.io/s8v3h/) contains a partial set of raw data tables (i.e., those obtained using [1_get_raw_data.ipynb](#1_get_raw_dataipynb) that did not need redaction), redacted data tables (from [3_redact_data.R](#3_redact_dataR)), and intermediately clean data tables (from [4_clean_data.R](#4_clean_dataR)). The folder structure is below.

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

For more details about these data, cleaned by Alex Werntz and Allie Silverman, see the measures-related [appendix and codebook](#appendices-and-codebooks) on the [Public Component](#public-component). Researchers can request access to the data by contacting the study team ([studyteam@mindtrails.org](mailto:studyteam@mindtrails.org)).

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

Put the cleaning scripts in the `code` subfolder. The scripts are to be run in the order listed. Assuming you already have full or partial raw data, start with [2_define_functions.R](code/2_define_functions.R). If you have full raw data, run [3_redact_data.R](code/3_redact_data.R) next; otherwise, skip it. Run the remaining scripts.

At the top of each R script, restart R (CTRL+SHIFT+F10 on Windows) and set your working directory to the parent folder (CTRL+SHIFT+H).

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

### [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb)

This Jupyter Notebook script (author: [Sonia Baee](https://github.com/soniabaee)) dumps the full set of 67 raw CSV files from the "calm" SQL database on the "teachmanlab" Data Server as of December 3, 2020. This dump defines the end of data collection for the study. [4_clean_data.R](#4_clean_dataR) below identifies that the last system-generated timestamp for a Calm Thinking participant in this dataset is "2020-11-13 22:13:27 EST".

### [2_define_functions.R](code/2_define_functions.R)

This R script defines functions for use by subsequent R scripts, which source this file at the top of each script.

### [3_redact_data.R](code/3_redact_data.R)

This R script performs the following functions. Here, *redact* means to replace relevant values with "REDACTED_BY_CLEANING_SCRIPT", retaining the structure of the raw data files.

- Specify columns to retain that were considered for redaction
- Determine which "button_pressed" data in "angular_training" table to redact
- Redact "button_pressed" data for "FillInBlank" rows in "angular_training" table
- Redact free-text responses for certain other columns that may contain identifiers
- Redact "order_id" data from "gift_log" and "import_log" tables
- Redact phone numbers from "sms_log" table

Note: So that "order_id" and phone number data are not retained in the full raw data, only redacted versions of the "gift_log", "import_log", and "sms_log" tables are stored in `1_raw_full`, and the unredacted versions dumped by [1_get_raw_data.ipynb](#1_get_raw_dataipynb) were deleted.

By contrast, unredacted versions of other redacted tables are retained in `1_raw_full` on the [Private Component](#private-component) because these tables contain free-text responses that may or may not contain identifiers. Notably, participants were not asked to provide identifiers in their responses.

### [4_clean_data.R](code/4_clean_data.R)

This R script performs the following functions.

#### Part I. Database-Wide Data Cleaning

Part I applies to data for all three studies (Calm Thinking, TET, GIDI) in the "calm" SQL database.

- Remove irrelevant tables
- Rename "id" columns in "participant" and "study" tables
- Add "participant_id" to all participant-specific tables (see [Participant Indexing](#participant-indexing) for details)
- Correct test accounts (see [Test Accounts](#test-accounts) for details)
- Remove admin and test accounts
- Label columns redacted by server with "REDACTED_ON_DATA_SERVER"
- Remove irrelevant columns
- Identify any remaining blank columns
- Identify and recode time stamp and date columns
  - Correct blank "session", "date", and "date_submitted" in "js_psych_trial" table for some participants
  - Note that participant 3659 is considered enrolled in TET but lacks screening data (see [TET Participant Flow](#tet_participant_flow) for details)
  - Recode system-generated timestamps as POSIXct data types in "EST" and user-provided timestamps as POSIXct data types in "UTC"
  - Create variables for filtering on system-generated time stamps (see [Filtering on System-Generated Timestamps](#filtering-on-system-generated-timestamps) for details)
  - Reformat user-provided dates so that they do not contain empty times, which were not assessed
- Identify and rename session-related columns (see [Session-Related Columns](#session-related-columns) for details)
- Check for repeated columns across tables (see [Repeated Column Names](#repeated-column-names) for details)
- Correct study extensions (see [Study Extensions](#study-extensions) for details)

#### Part II. Filter Data for Desired Study

Part II filters data for the Calm Thinking Study, but the "study_name" can be changed to filter data for the TET or GIDI studies if desired.

- Define enrollment period and participant_ids (see [Enrollment Period](#enrollment-period) for details)
- Filter all data

#### Part III: Calm Thinking Study-Specific Data Cleaning

Part III cleans the Calm Thinking Study data. Most of the tasks will also be needed for TET and GIDI, but the code will need to be revised.

- Note lack of data for some tables (see [Launch of TET Study](#launch-of-tet-study) for details)
- Recode "coronavirus" column of "anxiety_triggers" table (see [Launch of TET Study](#launch-of-tet-study) for details)
- Add participant information
  - Create variable describing how participants were assigned to condition
  - Create variable to differentiate soft and official launch participants
  - Create variable describing how participants were classified as high/low dropout risk (see [Dropout Risk](#dropout-risk) for details)
  - Create indicator variable for coaching accounts
- Exclude participants
  - Confirm that accounts for coaches have already been removed
  - Identify official-launch participant_ids and exclude soft-launch participants
- Edit participant information: Participant spanning two studies
- Obtain time of last collected data
- Identify participants with inaccurate "active" column (see ["active" Column](#active-column) for details)
- Check "conditioning" values in "angular_training" and "study" tables
  - Note that "conditioning" is blank for some rows of "angular_training"
  - Check for participants in "TRAINING" after "firstSession", which should not occur
  - Check that condition stays the same from "secondSession" through "fifthSession"
  - Check for switching between "CONTROL" and another condition, which should not occur (see [Condition Switching](#condition-switching) for details)
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
- Identify unexpected multiple entries (see [Unexpected Multiple Entries](#unexpected-multiple-entries) for details)
- Investigate unexpected multiple entries (see [Unexpected Multiple Entries](#unexpected-multiple-entries) for details)
- Handle unexpected multiple entries (see [Unexpected Multiple Entries](#unexpected-multiple-entries) for details)
- Arrange columns and sort tables (see [Table Sorting](#table-sorting) for details)

### [5_import_clean_data.R](code/5_import_clean_data.R)

This R script imports the intermediately cleaned Calm Thinking Study data and converts system-generated timestamps back to POSIXct data types given that [4_clean_data.R](#4_clean_dataR) outputs them as characters. As such, this script serves as a starting point for further cleaning and analysis.

## Cleaning and Analysis Considerations

This section highlights some considerations prompted by data cleaning that may be relevant to further cleaning or to analysis. Refer to the actual script for more details.

### For Calm Thinking, TET, and GIDI Studies

#### Participant Indexing

Part I of [4_clean_data.R](#4_clean_dataR) indexes all participant-specific data by "participant_id". Refer to participants by "participant_id" (not "study_id").

#### Filtering on System-Generated Timestamps

Part I of [4_clean_data.R](#4_clean_dataR) creates "system_date_time_earliest" and "system_date_time_latest" in each table given that some tables have multiple system-generated timestamps. They are the earliest and latest timestamps for each row in the table--useful for filtering the entire dataset.

#### Session-Related Columns

Part I of [4_clean_data.R](#4_clean_dataR) reveals that in some tables (e.g., "dass21_as") "session" conflates time point with other information (e.g., eligibility status). Here, "session" is renamed to reflect the information it contains (e.g., "session_and_eligibility_status"), and "session_only" is created to reflect only the time point. In some tables (i.e., "angular_training", "gift_log") it is unclear how to extract the time point, so these tables lack "session_only". In tables where "session" does not conflate time point with other information, "session" is renamed "session_only".

Thus, "session_only" is the preferred column for filtering by time point, but not all tables have it. Moreover, "session_only" includes values of "COMPLETE" in some tables (i.e., "action_log", "email_log") but not others (i.e., "task_log"). Thus, filter by time point with care.

#### Repeated Column Names

Part I of [4_clean_data.R](#4_clean_dataR) reveals that although some tables contain the same column name, the meanings of the columns differ. As a result, care must be taken when comparing columns between tables. See the cleaning script for explanations of repeated column names.

#### Study Extensions

Part I of [4_clean_data.R](#4_clean_dataR) corrects the "study_extension" for participants 2004 and 2005, who are enrolled in Calm Thinking.

#### Enrollment Period

Part II of [4_clean_data.R](#4_clean_dataR) defines the enrollment periods for Calm Thinking, TET, and GIDI in the "America/New_York" timezone, as this is study team's timezone. "America/New_York" is preferred to "EST" because "America/New_York" accounts for switches between "EST" and "EDT". By contrast, system-generated timestamps are stored only in "EST" as this is how they are stored in the "calm" SQL database.

Enrollment periods are used to filter screening data, most of which is not indexed by "participant_id" but required for participant flow diagrams.

### For Calm Thinking Study

#### Test Accounts

Part I of [4_clean_data.R](#4_clean_dataR) corrects test accounts: Participant 1097 should not be a test account and participant 1663 should.

#### Launch of TET Study

Part III of [4_clean_data.R](#4_clean_dataR) reveals that Calm Thinking participants who accessed the site after TET launched on 4/7/2020 completed some tasks (e.g., "covid19" table, "coronavirus" in "anxiety_triggers" table) designed for TET. The data are retained to reflect the tasks completed.

#### Dropout Risk

Part III of [4_clean_data.R](#4_clean_dataR) indicates that some official-launch participants were manually classified as high risk for dropout (vs. classified by attrition algorithm) and then randomized to Stage 2 condition. See "risk_classification_method" in "participant" table.

#### "active" Column

Part III of [4_clean_data.R](#4_clean_dataR) indicates that for "active" in "participant" table, participants 891, 1627, 1852 are mislabeled as active and participants 191, 329, 723 are mislabeled as inactive. The "active" column may have affected final reminder emails or notices of account closure. Thus, the mislabeled data are retained to reflect potential unexpected behavior of the site for these participants.

#### Condition Switching

Part III of [4_clean_data.R](#4_clean_dataR) reveals various cases of unexpected values for "conditioning" in "angular_training". See cleaning script for details.

Notably, participant 382 received CBM-I training at Session 1 and psychoeducation at Sessions 2-5. Their handling will depend on the analysis.

#### Multiple Screening Attempts

After removing nonmeaningful duplicates (i.e., for duplicated values on every column in table except "X" and "id", keep last row after sorting by "id") for all tables, Part III of [4_clean_data.R](#4_clean_dataR) first corrects cases where "participant_id" is not linked to all screening attempts by its corresponding "session_id" in "dass21_as" table.

Second, the script removes duplicates on DASS-21-AS items, "over18", and "time_on_page" columns in "dass21_as" table for a given "session_id" and "session_only" time point by keeping the last row after sorting by "session_id", "session_only", and "id". The idea is that duplicates on these columns do not reflect unique screening attempts.

Third, the script counts the number of multiple screening attempts remaining for each "session_id" at screening ("n_eligibility_rows") and computes the mean "time_on_page" across those rows for each "session_id". This "time_on_page_mean" is used for analysis. It represents the mean time a given "session_id" spent on the page across their screening attempts, which could reflect different responses on DASS-21-AS items, different responses on "over18", or both.

To isolate unique responses on DASS-21-AS items, the script counts the number of unique rows on DASS-21-AS items for each "session_id" at screening ("n_eligibility_unq_item_rows"). The study team decided that participants with more than two sets of unique rows on DASS-21-AS items will be excluded from analysis due to concerns about data integrity, whereas those with two sets of unique rows on DASS-21-AS items will be included, even if they have two or more entries for "over18". The script does not exclude the former participants, but rather marks them for exclusion (see [Participant Flow and Analysis Exclusions](#participant-flow-and-analysis-exclusions)).

Fourth, the script computes column means for DASS-21-AS items across these unique DASS-21-AS item rows for each "session_id", treating values of "prefer not to answer" as NA without recoding them as NA in the actual table. These column means are used to compute a total score for analysis ("dass21_as_total_anal") below.

Fifth, the script seeks to distinguish whether a given ineligible screening attempt is ineligible due to the DASS-21-AS responses or due to age. Given that the site allows multiple screening attempts and scores each in isolation from the others, the script computes a total score for each attempt ("dass21_as_total") by taking the mean of available DASS-21-AS items (again treating values of "prefer not to answer" as NA without actually recoding them) and multiplying by 7.

Sixth, this per-attempt "dass21_as_total" score is multiplied by 2 to get "dass21_as_total_interp", which is compared with the criterion (>= 10 is eligible). Seventh, the script creates "dass21_as_eligible" to indicate eligibility status on the DASS-21-AS--used to report [participant flow](#participant-flow-and-analysis-exclusions).

Finally, the script computes a per-"session_id" total DASS-21-AS score for analysis ("dass21_as_total_anal") by taking the mean of available DASS-21-AS column means (from above; again treating values of "prefer not to answer" as NA without actually recoding them) and multiplying by 7. Given that this score accounts for multiple unique rows on DASS-21-AS items, use this as the baseline score in analysis.

#### Participant Flow and Analysis Exclusions

Part III of [4_clean_data.R](#4_clean_dataR) reports number of participants screened (*n* = 5267), enrolled (*n* = 1748), and not enrolled (*n* = 3519). 

For participants with multiple entries who did not enroll, the script bases the reason they did not enroll on their last entry, though recognizing that non-enrollment following each attempt could have occurred for a different reason. Of the 3519 who did not enroll, 774 were ineligible on DASS but eligible on age, 23 were ineligible on both DASS and age, 111 were eligible on DASS but ineligible on age, and 2611 were eligible on both DASS and age (but did not create an account).

Participants with more than two unique rows on DASS-21-AS items ("n_eligibility_unq_item_rows" > 2) are marked for exclusion from analysis using "exclude_analysis" in "dass21_as" and "participant" tables. 17 non-enrolled participants should be excluded from any analysis of screening data, and 6 enrolled participants should be excluded from any analysis.

#### Unexpected Multiple Entries

After removing nonmeaningful duplicates (i.e., for duplicated values on every column in table except "X" and "id", keep last row after sorting by "id") for all tables, Part III of [4_clean_data.R](#4_clean_dataR) checks for unexpected multiple entries for all tables (e.g., multiple rows for a given "participant_id" and "session_only" time point where only one row is expected). However, it is unclear how to check for multiple entries in "angular_training" and "js_psych_trial" tables, so they are not precisely checked.

Note: "task_log" does not reflect some entries in other tables ("dass21_as", "credibility"). Thus, do not rely on "task_log" to find multiple entries or reflect task completion.

Besides [multiple screening attempts](#multiple-screening-attempts), multiple entries (found in "credibility", "return_intention", "bbsiq", "oa", and "task_log" tables) are handled by computing (a) number of rows ("n_rows") for a given set of index columns (e.g., "participant_id", "session_only"); (b) mean "time_on_" values (e.g., "time_on_page") across those rows, for use in analysis (e.g., "time_on_page_mean"); and (c) number of rows with unique values for a given set of items  ("n_unq_item_rows").

If multiple unique rows are present ("n_unq_item_rows" > 1), we compute column means for all items for analysis, treating values of "prefer not to answer" as NA without actually recoding them. However, in this dataset the multiple entries have identical item responses (just different "time_on_page" or "time_on_task" values), so column means are unneeded.

#### Table Sorting

Given that "X" (row name in "calm" SQL database on "teachmanlab" Data Server) is in every table and uniquely identifies every row, whereas "id", though in every table, does not distinguish all rows, all tables are sorted on "X" before export.

### For TET and GIDI Studies

Note: In addition to considering the issue below, consider how the Calm Thinking issues may similarly apply to TET and GIDI. They likely do.

#### Participant Flow

Part I of [4_clean_data.R](#4_clean_dataR) reveals that participant 3659 lacks screening data but is considered officially enrolled in TET. Thus, care should be taken to ensure that this participant is reflected appropriately in the TET participant flow diagram.

## Next Steps

As noted above, this centralized cleaning of Calm Thinking data yields data deemed intermediately cleaned because further cleaning will be needed for any given analysis. We focused on issues that cut across multiple tables or that will affect almost any analysis. And in many cases, we opted to flag issues for further cleaning and analysis rather than implement decisions suitable for only a narrow application.

Here are some known next steps for further cleaning and analysis:

- Use [5_import_clean_data.R](#5_import_clean_dataR) as a starting point for further cleaning and analysis
- Review the following items and conduct further cleaning as needed for your analysis
  - [Cleaning and Analysis Considerations](#cleaning-and-analysis-considerations) above
  - [MindTrails Changes and Issues log](#mindtrails-changes-and-issues-log) entries
- Further consider the following issues not addressed by centralized cleaning
  - Exclude participants indicated by "exclude_analysis" in "dass21_as" and "participant" tables
  - Clean "angular_training" and "js_psych_trial" tables (see [Outtakes: Clean "angular_training"](#outtakes-clean-angular-training) for details)
  - Handle values of "prefer not to answer" (coded as 555; see [Outtakes: Create Reports](#outtakes-create-reports) for details)
  - Check the response ranges of each item (see [Outtakes: Create Reports](#outtakes-create-reports) for details)
  - Appropriately handle missing data (see [Outtakes: Create Reports](#outtakes-create-reports) for details)
  - Reconcile [coaching-related data](#coaching-related-data-on-uva-box) with `3_intermediate_clean` data

## Resources

### Appendices and Codebooks

Several appendices and codebooks for the Calm Thinking study are on the [Public Component](#public-component).

### MindTrails Changes and Issues Log

This is a log of site changes, data issues, etc., tagged by study that is privately stored by the study team. For log entries tagged for Calm Thinking through 12/30/2021, a note has been added indicating whether the entry (a) does not apply to data cleaning; (b) was addressed by centralized cleaning (and if so, how); or (c) needs to be considered for analysis-specific data cleaning. If you address an issue for a specific analysis, please note in the log how you addressed it and provide a link to your code.

Researchers can request access to relevant information from the log by contacting the study team ([studyteam@mindtrails.org](mailto:studyteam@mindtrails.org)).

### MindTrails Wiki

This is a wiki with MindTrails Project-wide and study-specific information that is privately stored by the study team.

Researchers can request access to relevant information from the wiki by contacting the study team ([studyteam@mindtrails.org](mailto:studyteam@mindtrails.org)).

### Outtakes: Clean "angular_training"

[3_redact_data.R](#3_redact_dataR) mentions various issues encountered with "angular_training" when determining which "button_pressed" data to redact. Although it was decided not to clean "angular_training" centrally, referring to these issues may be a starting point.

In addition, the script [old/outtakes/outtakes_clean_angular_training.R](old/outtakes/outtakes_clean_angular_training.R) started to address these and other issues but was abandoned. Refer to this script with caution; it is incomplete and may be inaccurate.

### Outtakes: Create Reports

The script [old/outtakes/outtakes_create_reports.R](old/outtakes/outtakes_create_reports.R) was an initial attempt to create reports related to (a) "prefer not to answer" values, (b) response ranges of items, and (c) instances of missing data. However, it was decided not to include such reports in centralized cleaning.

This script may be a starting point for such reports, but refer to it with caution, as it was abandoned, is incompatible with the current cleaning scripts, and may be incomplete or inaccurate.
