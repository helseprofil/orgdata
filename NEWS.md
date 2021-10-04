# orgdata (development version)

Things in `dev` branch

- Standardize some most used arguments to `read_file()` such as `nrows`,
  `header`, `skip`, `trimws` and `na`. Read details in `read_file()` function
  description.
- Output to `read_file()` as data.table class.
- Use standard columnames with `V1`, `V2` etc when argment `header = FALSE` is specified.
- Error message with list of unmatch columns in `do_column_standard()`
- Give clear message and debug message.
- Fix (#28) GEO derived from two columns with empty INNLESARG.


# orgdata 0.0.9 - alpha
- When MANHEADER is used then the new columname must be specified in the respective standard column (#21) 
- Deprecated arguments `geo` and `val` in `make_file`. Output data must use standard
  columnames instead of keeping the columnames from original dataset.
- Rename functions `read_raw` or `lesraw` to `make_file` or `lag_fil` (#27)
- Alle functions uses underscore "_" for both english and norwegian.

# orgdata 0.0.8 - alpha
- Add new columns if one of the standard columns is missing in the original
  data. The value to be inserted to the new column must use symbol less than `<`
  and more than `>`. For instance when column `KJONN` doesn't exist in the
  original data, we can specify with `<2>` in under column `KJONN` in the Access
  registration database. The output will add a new column `KJONN` with value `2`. (#15)
- Fix #13 and #18
- Default `orgdata.verbose` is `TRUE`.
- Options for `orgdata.implicit.null` with default as `TRUE`. Use
  `options(orgdata.implicit.null = FALSE)` to deactivate (#19)

# orgdata 0.0.7 - alpha

- Implicit zero (#11). Discussion is in [Gist](https://gist.github.com/ybkamaleri/cd789560d595d7a0d6eb46a23395fc51 "implicit-null")
- Use version specific for imported packages.
- Rename standard column `LANDBAK` to `LANDSSB` for column in original data
  received from SSB containing information about country of origin.
- Save file as specified in column `MAPPE` in Access registration database or
  specify in `path` argument for function `save_file`. (#12)

Changes is in PR #11 and #12


# orgdata 0.0.6 - alpha

- Create GEO code from two separate columns. This has to be defined in Access
  registration under `GEO` with comma separated eg. `nameGeoCol1, nameGeoCol2`.
- Order standard columns in the output dataset with this order for the first four columns:
    - `GEO`, `AAR`, `ALDER`, `KJONN`
- Change norwegian name for `save_file` from `lagfil` to `lagrefil`.
- Use column name `KOLNAVN` instead of `ADDKOL`.
- Rename function `do_addcols` and `get_addcols` to `do_colname` and
  `get_colname` to be consistent with the changes in Access registration database.

Changes is in PR #8

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

- MANHEADER and KOLNAVN uses common helper function `is_col_separate()`
- MANHEADER `old` convert to integer and use and index for columns

Changes is in PR #2

# orgdata 0.0.3 - alpha

- LESID is not unique id but a combination of LESID and FILGRUPPE
- Standard columns include 3 TABS and 3 VALS
- KOLNAVN input uses comma as separate and `VAL1=TOTAL, TAB1=ICD`is valid input

Changes is in [PR #1](https://github.com/helseprofil/orgdata/pull/1)


# orgdata 0.0.2 - alpha

Things that are implemented

* Bla.. bla.. bla..

