# orgdata (development version)

Things in `dev` branch

- Implicit zero


# orgdata 0.0.5 - alpha

- Recode variables from specification in `tbl_Kode` uses:
  1. FILGRUPPE as `ALLE` for common recode for all groups
  2. Specified FILGRUPPE with empty LESID for common recode within group 
  3. Specified LESID to recode specific LESID
  

- Specified LESID will overrule empty LESID with specified FILGRUPPE for similar columnames
- Specified FILGRUPPE will overrule FILGRUPPE of type `ALLE`
  
- Error message if LESID without specifying FILGRUPPE

Changes is in PR #4

# orgdata 0.0.4 - alpha

- MANHEADER and ADDKOL uses common helper function `is_col_separate()`
- MANHEADER `old` convert to integer and use and index for columns

Changes is in PR #2

# orgdata 0.0.3 - alpha

- LESID is not unique id but a combination of LESID and FILGRUPPE
- Standard columns include 3 TABS and 3 VALS
- ADDKOL input uses comma as separate and `VAL1=TOTAL, TAB1=ICD`is valid input

Changes is in [PR #1](https://github.com/helseprofil/orgdata/pull/1)


# orgdata 0.0.2 - alpha

Things that are implemented

* Bla.. bla.. bla..

