# orgdata (development version)

Things in `dev` branch

- Implicit zero


# orgdata 0.0.5 - alpha

- Recode variables from specification in `tbl_Kode` uses:
  1. **GENERAL** variables are defined in FILGRUPPE as `ALLE` and are used to
     recode variables in all groups.
  2. **COMMON** variables are when FILGRUPPE is specified but have empty LESID.
     This will recode variables within selected group.
  3. **SPECIFIC** variables are when FILGRUPPE and LESID are specified. This
     will recode variables in that specified FILGRUPPE of the specified FILID.

- When all these three specification exist in `tbl_Kode`:
   - **SPECIFIC** variables will overrule **COMMON** variables
   - **COMMON** variables will overrule **GENERAL** variables

- Write as `<NA>` in codebook under column `FRA` when specifying missing
  variables indicating that a missing column to be recoded to value in column `TIL`. This
  will differentiate between real missing and a real column value of `NA`. (#5)
  
- Error message will be given if LESID is specified without FILGRUPPE since
  LESID is not unique ID.

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

