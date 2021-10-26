# orgdata (development version)

Things in `dev` branch

- Compute function to create a new category based on the existing categories eg.
  UTDANN has categories 0 to 4. Compute can regroup category 1 to 3 into 1 group
  ie. group 5.
  
- Fix #85 `see_file()` list all the columns when not specified. The variables
  are sorted whenever possible.

# orgdata 0.3.5
- Aggregate now give total to all dimensions including those specified in `AGGKOL` (#82)
- Function `see_file()` accept column index as well (#83)

# orgdata 0.3.4

- Recode variables using regular expression when defined in codebook with type
  `RE`. Finding pattern can either be written in ordinary regular expression ie.
  `\\d{4}.*` or with `rex()` package. (#78)
- New feature for checking categories for variables with `see_file()` (#75)

# orgdata 0.3.3

- Fix #65 make TABS and VALS dynamic for easy extention for these columns (#66)
- Fix #64 recode of variable that has different class (#68)
- Fix #63 implicit null includes all possible VAL columns when exist (#69)
- Fix #70 recode GEO of different object class (#71)
- Fix #67 aggregate with total values for standard variables ie. `UTDANN`,
  `LANDSSB`, `LANDBAK` and `INNVKAT` (#72)
- Fix #61 use AGGKOL in Access registration database to specify other columns to
  aggregate other than the standard eg. `KJONN`, `TAB1`, `TAB2` etc. (#73)

# orgdata 0.3.2
- Fix #55 to recode standard variables via codebook instead of hardcoded (#58)
- Fix #52 skip split if not specified (#59)
- Fix #57 split column with duplicated values will keep the original column (#60)
- Fix #56 aggregate all VAL columns whenever specified and not only specific to
  `VAL1` (#62)

# orgdata 0.3.1
- Edit verbose messages

# orgdata 0.3.0
- Reshape dataset from wide to long. Reshape can have more than one `measure
  variables`. Please read how this is specified in Access registration database.
- Split columns must have equal number of values to the defined `SPLITTIL`.
  Duplicate the value if it is less than the maximum `SPLITTIL`. For example for
  value `0` in column `LANDSSB` which will be split into `LANDBAK` and
  `INNVKAT`, the value will be duplicated into `00` to avoid split with value `NA`.
- Recode for `LANDBAK` and `INNVKAT` after aggregating are done internally ie.
  hard coded, in `do_aggregate_recode_standard()`. Total is coded with `20`. Any
  eventuality for future change should also look other related functions such as
  `is_aggregate_standard_cols()` and `is_col_int()`.

# orgdata 0.2

## orgdata 0.2.4
- Change argument parameter for `find_spec()` function.
- Update text document in several places.
- Add colour type *warn2* for warning message without `Warning:` prefix.

## orgdata 0.2.3
- Request (#43) messages with specific colour
- Fix (#46) recode to string even though columns is type integer or numeric.

## orgdata 0.2.2

- Unknown bydel ie. *(uoppgitt)* is added when enumeration areas codes ie.
  *(grunnkrets)* for bydel is `XXXX9999` in function `geo_level()`.
- Add unknown grunnkrets for kommune when not available since some of the
  datasets have unknown grunnkrets that aren't listed in API downloaded data (#39).
- Exclude `TAB1`, `TAB2` and `TAB3` from being aggregated. (#44) 

## orgdata 0.2.1

- Recode for aggregated variables uses `AG` in TYPE column in the codebook
  instead of FILGRUPPE with `AGGREGATE` as it was implemented in ver 0.2.0. This
  will make it possible so specify FILGRUPPE and LESID to implement the
  principle for **GENERAL**, **COMMON** and **SPECIFIC** variables.
- Change function name `do_aggregate_recode` to `do_aggregate_recode_standard`
  for standard variables.


## orgdata 0.2.0

- Recode for aggregated categories can be defined in *Recode* form ie. codebook,
  and use `AGGREGATE` in the specification under FILGRUPPE
- Delete rows when defined in codebook using minus symbol under TIL column.
  Similar principles is implemented for **GENERAL**, **COMMON** and **SPECIFIC**
  feature as in recode. Read detail in [ver 0.0.5 - alpha](https://helseprofil.github.io/orgdata/news/index.html#orgdata-0-0-5-alpha).
- Display both columnames to be recoded that are found in the dataset or those
  that aren't found when defined as `ALLE` in the codebook so user will be aware
  of its existence.

# orgdata 0.1

- Standardize some most used arguments to `read_file()` such as `nrows`,
  `header`, `skip`, `trimws` and `na`. Read details in `read_file()` function
  description.
- Output to `read_file()` as data.table class.
- Use standard columnames with `V1`, `V2` etc when argment `header = FALSE` is specified.
- Error message with list of unmatch columns in `do_column_standard()`.
- Give clearer message and debug message eg. `Execute: read_file()`.
- Change `MAPPE` to `UTMAPPE` to make it more explicit for path specification to save file.  
- Defun `orgdata.active` global options to use columnames from original dataset.
- Use global options `options(orgdata.debug.nrow = TRUE)` to read only first 20 rows. Suitable for debug purposes.  
- Fix (#28) GEO derived from two columns with empty INNLESARG. 
- Add column `LEVEL` for granularity level ie. grunnkrets, fylke, kommune, bydel etc

# orgdata 0.0.1 - alpha version

## orgdata 0.0.9 - alpha
- When MANHEADER is used then the new columname must be specified in the respective standard column (#21) 
- Deprecated arguments `geo` and `val` in `make_file`. Output data must use standard
  columnames instead of keeping the columnames from original dataset.
- Rename functions `read_raw` or `lesraw` to `make_file` or `lag_fil` (#27)
- Alle functions uses underscore "_" for both english and norwegian.

## orgdata 0.0.8 - alpha
- Add new columns if one of the standard columns is missing in the original
  data. The value to be inserted to the new column must use symbol less than `<`
  and more than `>`. For instance when column `KJONN` doesn't exist in the
  original data, we can specify with `<2>` in under column `KJONN` in the Access
  registration database. The output will add a new column `KJONN` with value `2`. (#15)
- Fix #13 and #18
- Default `orgdata.verbose` is `TRUE`.
- Options for `orgdata.implicit.null` with default as `TRUE`. Use
  `options(orgdata.implicit.null = FALSE)` to deactivate (#19)

## orgdata 0.0.7 - alpha

- Implicit zero (#11). Discussion is in [Gist](https://gist.github.com/ybkamaleri/cd789560d595d7a0d6eb46a23395fc51 "implicit-null")
- Use version specific for imported packages.
- Rename standard column `LANDBAK` to `LANDSSB` for column in original data
  received from SSB containing information about country of origin.
- Save file as specified in column `MAPPE` in Access registration database or
  specify in `path` argument for function `save_file`. (#12)

Changes is in PR #11 and #12


## orgdata 0.0.6 - alpha

- Create GEO code from two separate columns. This has to be defined in Access
  registration under `GEO` with comma separated eg. `nameGeoCol1, nameGeoCol2`.
- Order standard columns in the output dataset with this order for the first four columns:
    - `GEO`, `AAR`, `ALDER`, `KJONN`
- Change norwegian name for `save_file` from `lagfil` to `lagrefil`.
- Use column name `KOLNAVN` instead of `ADDKOL`.
- Rename function `do_addcols` and `get_addcols` to `do_colname` and
  `get_colname` to be consistent with the changes in Access registration database.

Changes is in PR #8

## orgdata 0.0.5 - alpha

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

## orgdata 0.0.4 - alpha

- MANHEADER and KOLNAVN uses common helper function `is_col_separate()`
- MANHEADER `old` convert to integer and use and index for columns

Changes is in PR #2

## orgdata 0.0.3 - alpha

- LESID is not unique id but a combination of LESID and FILGRUPPE
- Standard columns include 3 TABS and 3 VALS
- KOLNAVN input uses comma as separate and `VAL1=TOTAL, TAB1=ICD`is valid input

Changes is in [PR #1](https://github.com/helseprofil/orgdata/pull/1)


## orgdata 0.0.2 - alpha

Things that are implemented

* Bla.. bla.. bla..

