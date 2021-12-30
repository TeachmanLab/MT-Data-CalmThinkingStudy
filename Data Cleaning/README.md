# README

## Overview

Author: Jeremy W. Eberle

This README describes centralized data cleaning for the Calm Thinking Study, an NIMH-funded ([R01MH113752](https://reporter.nih.gov/project-details/9513058)) study of the [MindTrails Project](https://mindtrails.virginia.edu/) targeting interpretation bias in anxious adults (ClinicalTrials.gov [NCT03498651](https://clinicaltrials.gov/ct2/show/NCT03498651?term=NCT03498651&draw=2&rank=1)).

### Table of Contents

1. [Data](#data)
2. [Cleaning Scripts](#cleaning-scripts)

## Data

The data are stored in the [MindTrails Calm Thinking Study](https://osf.io/zbd52/) project on the Open Science Framework (OSF). The project has two components, with different permissions: a [Private Component](https://osf.io/jwvnb/) and a [Public Component](https://osf.io/s8v3h/).

### Private Component

The [Private Component](https://osf.io/jwvnb/) contains the full set of raw data tables (with some exceptions) dumped from the SQL database on the "teachmanlab" Data Server on December 3, 2020 (using [1_get_raw_data.ipynb](code/1_get_raw_data.ipynb)). The folder structure is below.

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

## Cleaning Scripts

The scripts in the [`code`](code) subfolder of this [Data Cleaning](https://github.com/jwe4ec/MT-Data-CalmThinkingStudy/tree/jeremy/Data%20Cleaning) folder import the full raw data files, redact certain files, and clean the redacted and remaining raw files to yield intermediately clean files. The resulting files are considered only intermediately cleaned because further analysis-specific cleaning will be required for any given analysis.

### Setup

To run the cleaning scripts, create a parent folder (with any desired name, indicated by `.` below) with two subfolders: `data` and `code`. The working directory must be set to the parent folder in order for the scripts to import and export data correctly using relative file paths.

```
.                                # Parent folder (i.e., working directory)
├── data                         # Data subfolder
└── code                         # Code subfolder
```

If you have access to the full raw data (from the [Private Component](#private-component)), you can reproduce the redaction. Put all the raw data files in a subfolder of `data` called `1_raw_full`. When you run the scripts, "3_redact_data.R" will create `2_redacted` and files therein, and `4_clean_data.R` will create `3_intermediate_clean` and files therein.

```
.
├── data                    
├── ├── 1_raw_full               # 67 CSV files from [Private Component](#private-component)
├── ├── (2_redacted)             # Folder with 14 CSV files will be created by "3_redact_data.R"
├── └── (3_intermediate_clean)   # Folder with 50 CSV files will be created by "4_clean_data.R"
└── ...
```

If you have access to the partial raw data and the redacted data (from the [Public Component](#public-component)), put the partial raw data files in a subfolder of `data` called `1_raw_partial` and the redacted data files in a subfolder called `2_redacted`. When you run the scripts, `4_clean_data.R` will create `3_intermediate_clean` and files therein.

```
.
├── data                    
├── ├── 1_raw_partial            # 53 CSV files from [Public Component](#public-component)
├── ├── 2_redacted               # 14 CSV files from [Public Component](#public-component)
├── └── (3_intermediate_clean)   # Folder with 50 CSV files will be created by "4_clean_data.R"
└── ...
```

Put the cleaning scripts in the `code` subfolder of the parent folder.

```
.
├── ...
├── code
├── ├── 1_get_raw_data.ipynb     # Dump 67 CSV files from "teachmanlab" Data Server (for "1_raw_full")
├── ├── 2_define_functions.R     # Define functions for use by subsequent R scripts
├── ├── 3_redact_data.R          # Redact 14 CSV files from "1_raw_full" and output them to "2_redacted"
├── ├── 4_clean_data.R           # Clean 14 CSV files from "2_redacted" and 53 CSV files from "1_raw_full"
│   │                            #   and output 50 CSV files to "3_intermediate_clean"
└── └── 5_import_clean_data.R    # Import 50 CSV files from "3_intermediate_clean"
```
