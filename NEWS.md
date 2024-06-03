# orgdata 1.5.0 (dev)
- Some internal functions made more general to be used in other packages

# orgdata 1.4.9
- `geo_merge()` gains a `localtable` argument. It can now be used on mapping tables generated with `geo_map_multi`. 
- Add `geo_map_multi()`, to generate a multiyear mapping table. [9b3e1a1](https://github.com/helseprofil/orgdata/commit/9b3e1a1071af6839bdcc1eb1716da33b62867144)
- Fix `geo_merge()` [d163c9a](https://github.com/helseprofil/orgdata/commit/d163c9ac08996b0a6ab0be5d6faee2bdbd5b2c1c)
- Temporarily made it possible to run on both FHI and HDIR systems. 
- Allow specification of encoding in `read_file` [ba980a7](https://github.com/helseprofil/orgdata/commit/ba980a791f96a18a4e2f1782f48ca0f48cf36bd2)

# orgdata 1.4.8
- Check filegroup input is correct and give suggestion if not found [db19c0d](https://github.com/helseprofil/orgdata/commit/db19c0d364b2cc1d1dbac1ec590c50920f0c1a0c).
- Fix `debug.rows` options.
- Check global options are updated [a10ced4](https://github.com/helseprofil/orgdata/commit/a10ced412f590fb1a407bc33755fc032b7cf4a8a)


# orgdata 1.4.7
- Can use different encodings for reading files and Access database. Encoding
  can be specified in `options` with either `orgdata.encoding.access` or
  `orgdata.encoding.csv`. Check
  [config](https://github.com/helseprofil/config/blob/main/config-orgdata.yml)
  file.
- Fix geo splitting in `geo_recode()` using `fix=TRUE` as default argument.
- Change options name from `debug.row` to `debug.rows`.
- Fix problem updating global options when config file has changed (#320)

# orgdata 1.4.6
- Should not give unnecessary warning with `dummy_grk` (#318)
- Rename argument `year` to `year.geo` in `make_file()` to be more explicit (#317)

# orgdata 1.4.5
- Have access to future data from API (#316).
- Use `pak` package for upgrade.

# orgdata 1.4.2
- Display output when called.
- Require `norgeo` package version 2.3.1

# orgdata 1.4.1
- Refactor some codes.
- Specify encoding via config.yml file.
- Use `norgeo` package version 2.3.0 from Github ie. dev, instead of CRAN.

# orgdata 1.3.0
- Fix selecting files without `KOBLID` (#315)
- Add more testing.
- Avoid warning when connection to the database is already closed.
- Create environment for status flow ie. `orgEnv`.
- Can use `select` arg to ease choosing file(s) in `make_file()` instead of `KOBLID`.
  Read the document on how to use the argument.
- Refactor some codes.
- Edit some documents.
- Fix `dbDisconnect` error (#312)
- Edit message (#313)
- Edit few other messages as well.
- Manage encoding warning for R 4.1 and below.
- Filtering with `IBRUKTIL` and `IBRUKFRA` for selecting original files.
  `IBRUKTIL` uses `>=` and `IBRUKFRA` uses `<` of the specified date. (#309)
- Update vignette on debugging.
- Utilise input from config when mutating columns [#ref](https://github.com/helseprofil/orgdata/commit/38ff241e37959bb426e833bf1597a5755c317184)
- Add welcome logo.
- Use integer (#310)
- Proper use of boolean (#311)

# orgdata 1.2.0
- Update vignette.
- Use keyword `delete` or `slett` when recoding with RE options to `""`. This
  replaces `empty` and `tom` as in #285.
- Selecting geo levels when geo codes derived from two columns will use the
  maximum number of digits to find geo level (#307)
- Rewrite some codes for speed. (#231)
- Add unidentified municipalities as `xx99` to `geo_map()`. Unidentified municipalities do not exist from API. (#304)
- Age group with `AgeCat` can use mix categories easily with `[x]`. See example in `find_age_category()`. (#305) 
- Fix bugs when writing and reading data from warehouse ie. marked the
  `KONTROLLERT` column in Access (#298)
- Fix bugs when accessing or deleting data with `see_data()` from data warehouse
  (#300)
- Upgrade package version dependency.
- Create unknown municipality codes with `xx99` in `geo_recode()` function ie.
  geo record table in Access. (#302)
- Recode unknown municipality codes with `xx99` for known county. (#303)
- MANHEADER accepts regular expression to select columname in addition to column
  index (#301)
- Function `read_file()` accept SPSS file too (#297)
- Function `read_file()` now accept txt file extension.

# orgdata 1.1.0
- Make geo abbreviation clearer in the global options and no more depending on
  the order of geo names (#286)

# orgdata 1.0
- Order does matter when using functions in EXTRA column (#294)
- Fix S3 age category functions `find_age_category()` (#294) 
- Delete unwanted column in the output when using `AgeCat` function in EXTRA (#295)
- Use global options in input argument when relevant instead of using `match.arg()` (#263)
- Edit document here and there.
- Use codebook to recode age category for better speed (#292)
- Read or delete data in the data warehouse can use multiple koblid in `see_data()`. Use `"all"`
  in `koblid` argument to select all data on the chosen filegroup (#291)
- Fixed duplicated age groups (#290).
- Use either `empty` or `tom` to represent regular expression to replace to since Access makes symbol `""` to be invisible (#285)
- Change function name from `see_org()` to `see_data()` for viewing data in the data warehouse.
- Use symbol `|` to separate multiple arguments in column `EXTRA` (#288)
- Group age to specific or specified interval with `AgeCat()`. This function can be use in table for filegroup under `EXTRA` column (#287 #289)
- Delete raw of similar columns with multiple specifications (#282)
- Different ways to recode of similar column ie. duplicated, with defined lesid will give error.
- Show current installed version against the new release version.
- Some minor text editing.
- Delete older file of DuckDB automatically since new version can't read the older files (#280)
- Actively ask users to update with given options when new release version is available.
- Control all columns that should be numeric don't contain any string. The
  columns to be controls for is now dynamic (#281)
- Rename function alias from `rf()` to `rdf()` since `rf()` is already in use in *stats* package.
- Deleting row with specification of lesid and without lesid ie. common filegroup, create errors. This is not fixed (#279)
- Need to update version whenever a new release is available to ensure everyone is using the latest release. Else users can't load the package.
- Filter active files with date was done pragmatically ie. all files with `BRUKTIL` date other than `01-01-9999` will be excluded. Now filtering with date will be compared against current date. (#272)
- Delete dataset from the database when unmark column `KONTROLLERT` instead of updating the dataset due to time consuming by updating it. The users have to mark the column to save or read the dataset in the database. (#278)
- Change default for argument `raw` in `make_file()` to `FALSE` as in [config](https://github.com/helseprofil/config/blob/main/config-orgdata.yml "config") file.
- Some text editing here and there.
- Function `is_colour_txt()` can specify symbol directly without needing to rely on the global options or to use `withr` package. Just for cosmetic purposes :smiley:
- Standard columns should be uppercase. Using lowercase creates error when GEO comes
  from two separated columns especially when handling recode from codebook (#277)
- Standardize arguments names in `see_org()` function.
- Parallel processing is deactivated since it doesn't work smoothly with
  Access and DuckDB connection ie. DBI package, due to the problem with
  not-exportable objects for
  [future](https://cran.r-project.org/web/packages/future/vignettes/future-4-non-exportable-objects.html
  "future") package.
- Replace SQLite with DuckDB again due to the speed for DuckDB. But since DuckDB
  is under active development, the new version might not work with the file
  created by older version. In such situation older DuckDB database should be
  deleted and re-run with the newer DuckDB version.
- When loading the package, users will be reminded whenever new version is available.
- Show as data frame when warning `NAs with coercion` instead of just the GEO
  number where the coercion took place (#274)
- Replace DuckDB with SQLite (#271)
- Columnames are case insensitive (#115)
- Use ellipsis for other arguments of `data.table::fwrite()` in `save_file()`
  function.
- Implicit null for number of geo digits is moved to config file. This make it
  easy to maintain and expand.
- Use `future.apply` package conditionally to reduce package vulnerability.
- Deactivate dependency packages for parallel processing. Use it conditionally
  ie. to install manually if needed.
- Reshape multiple columns containing more than one `VAL` is handled properly.
  When reshaping multiple columns to be just one `VAL` then leaving
  `RESHAPE_VAL` empty ie. use all columns not defined in `RESHAPE_ID` should
  work as before (#269)
- Give more information when error to reshape data, especially long reshape.
  Most of the time the source of error is in defining `RESHAPE_KOL` and
  `RESHAPE_VAL` (#268)
- Can reshape multiple columns when reshaping to LONG. Applicable when there are
  multiple columns represent number of cases and one column represent
  denominator for all these numerators (#266)
- Use *KOBLID* to add or delete from Duck database (#261)
- Depends for *norgeo* package refers directly to CRAN version instead of GitHub
  repo.
- Use function `geo_merge()` to add geo granularity that aren't from API. This
  can be a csv, xls or any other format that is accepted by `read_file` (#262)
- Deactive KONTROLLERT without the need to umark from Access database by using
  argument `raw = TRUE` when using function `make_file()` (#264)
- Use *KOBLID* as table name in DuckDB instead of *FILID* since *FILID* is not a
  unique number ie. can be used by multiple *KOBLID*.
- Update text and website 
- Change function name from `geo_levels()` to `geo_map()` for mapping geo codes
  granularity.
- Change helper function names for `read_file()`. (#250) 
- Fixed bugs when reading file from the web with https (#251)
- `read_file()` accept Stata file with `dta` extension (#252)
- Use yaml format for global configuration file to ease update. The file is
  located in [config repo](https://github.com/helseprofil/config/blob/main/config-orgdata.yml) (#256)
- Raw data that have been controlled for are saved in DuckDB database format.
  This increase reading speed especially for big files. Mark column
  *KONTROLLERT* in the Access registration database to activate this function.
  Unmark to read from the original raw file instead. (#257)
- Use `see_org()` to read the raw data in the database. Argument `action =
  "delete"` can be used to delete the data from the raw database.
- Function `geo_merge()` for merging geo codes that aren't available from API to
  the mapping table ie. *tblGeo*, in the geo database. The data could be in any
  file format accepted by `read_file()` function. The data to be merged must
  have column to be merged ie. `id.file`, that is equivalent to the column id in
  the database ie. `id.table`. The `id.file` must be unique.
- Handle unbalanced parentheses in post recode whenever possible when `raw` is
  used, else give error message. #246
- `read_file()` accept filegroup name as argument in `file` to read the
  completed file after running `make_file()` function. #247
- Debug functions can go deeper to show helper functions as well with
  `debug_opt("deep")` or `options(orgdata.debug = "deep")` #243
- Post recode uses type `PS` in codebook. The function is used when there is a
  need to recode the value of a column after the dataset have been clean and
  aggregated. Specification to select the row to be recoded uses either standard
  expression or R syntax of `data.table` style. When using R syntax the value
  must have `raw` prefix eg. `raw(AAR %in% c(2000, 2005))`. #244 #245
- Delete rows with `"-"` minus symbol in `TIL` column in the codebook is
  accepted for `do_recode_post()`.
- Able to aggregate to country level in addition to other geographical levels (#240)
- Download the geo code to aggregate with only once despite the different
  geographical levels the dataset to be aggregated into (#241)
- Update package via function `update_orgdata()`. Basically it's just a wrapper
  for `remotes::install_github()`.
- Can aggregate to country level as well (#240)
- Speed up aggregate process (#241) 
- Fixed the bugs with reshape wide consisting multiple dimensions ie. more than
  one *TAB* columns. (#228)
- Ensure melting columns are not converted to factor (#234)
- Some columns should be numeric type and these columns are **GEO**, **AAR**,
  **KJONN**, **ALDER** and all **VALs**. They are also specify in global options
  `orgdata.num`. Ensure thise columns are numeric and give warning as well as
  log when coercion where *NA* is introduced. (#235)
- Use log file to check what or which codes that have problem. (#237)
- Warn and give logfile if GEO can't be converted to integer before geo recode. (#233 #236)
- Able to read files that have no specific extension but they have to be coma or
  semicolon separated (#227)
- Ensure all **VAL** columns are numeric type (#229)
- Give explicit warning when **GEO** codes have character that can cause
  coercion resulting in converting GEO codes to `NA` (#229)
- Log files for `code99` also include koblid. The files will be named as
  `code99_koblidxxx` (#222)
- Running parallel processing is unstable. Not sure if the process crash due to
  MS Access that can't handle constant requests or ... Anyway, running parallel
  now will only use 50% of the available cores (#224)
- User can specify more or less cores than 50% if needed. To use 75% cores by
  specifying `parallel = 0.75` in the argment `make_file()` or in the global
  options `orgdata.parallel` (#225)
- Connect to both database with common function. (#212)
- Use global options `orgdata.year` to specify production year if not using
  current year. (#216)
- Use parallel processing in `make_file()` with argument `parallel = TRUE`.
  (#217)
- Get feedback for successful and unsuccessful filegroups when running
  `make_filegroups()`. (#199)
- Reshape wide is not limited to only 3 reshape columns as it was. (#200 #201)
- Create log files in default orgdata folder instead of occupying the REM. (#202)
- Delete old bydel codes ie. before 2003, except for Oslo. This is due to the
  unstable geographical recoding of bydel. Use `EXTRA` column on filegroup level
  with argument `DeleteOldBydel`. (#204 #206)
- Log files use `KOBLID` to be more specific. (#208)
- Reshape to wide format can now implement multiple reshape dimensions which are
  usually one or multiple TAB columns. For instance for *LESEFERD* of `tot_elev`
  is the product of both `mestringsnivå` and `klassetrinn`. (#188)
- The output will be arrange on a standard column order. (#190)
- Debugging will be easier with a wrapper function `debug_opt()`. (#196)
- Fixed #147 to implement reshape wide format to create denominator. This
  function is only applicable to certain file structure received from SSB. For
  example filegroup *LESEFERD* where column `tot_elev` represents the total
  number of student with `mestringsnivå` and not the grand total of students.
  The number of students with `mestringsnivå` is represented in column
  `ant_elev`. Therefore the long format for `mestringsnivå` needs to be
  restructured to wide with value from the `ant_elev` to ensure summing up
  `tot_elev` when creating denominator will not create a grand total of students
  instead of the total number of students with `mestringsnivå`. (#184)
- Fixed #162 when saving geographical codebook with `write = TRUE` and the table
  doesn't exist in the *geo-code* database.
- Stop looping the geographical levels when `orgdata.debug.geo` or
  `orgdata.debug.aggregate` are active and make the default to `kommune`. (#166)
- Add batch date from codebook as reference when debugging. (#168)
- Hide warning messages if file has been controlled for any possible errors. It
  means the column *KONTROLLERT* in table for original files is marked for the
  specific file. (#171)
- Refactoring function for recode of geographical codes. (#174)
- Geo codes for enumeration areas and towns that aren't able to be merged will
  be converted to `unknown` codes with either `xxxx9999` or `xxxx99`. (#177)
- Create default folder at `C:/Users/YourUserName/orgdata_logs` when `path`
  argument is not specified in `save_file()`. (#179)
- Create unknown codes for enumeration areas and town codes if not able to
  recode. Most probably is due to the enumeration codes before 2002. The Excel
  file from SSB is not clean and unstable to be used for recoding. The unknown codes
  are based on municipality codes with added `xxxx99` or `xxxx9999`. As in #177
  but recode is done on municipality codes before merging back to the original
  dataset. (#182)
- Add `codeDelete` in `log` for geographical codes that aren't able to be
  merged. The codes will be excluded in the dataset. To access all the deleted
  codes use `log$codeDelete`. (#149)
- Give explicit error message when `path` is missing in `save_file()`. (#152)
- Able to select any valid year to recode geographical codes and aggregate
  accordingly. But recoding geographical codes backward isn't possible. (#153)
- Error message will split a long vector if exist. (#153)
- Add column with batch date when the geographical codes were downloaded form
  SSB when creating a geo codebook with `geo_map()` or `geo_recode()`. (#156)
- Actively select only the first geo code when old geo code is split into
  multiple new geo codes. (#159)
- Able to select a base year for geo recode based on the year available in the
  original data. This is available in column `AAR` in the dataset. Use argument

  `base` or global option `orgdata.recode.base` with logical input. `TRUE` will
  select the base year for recoding geographical code from the year of the
  original file to the current year. Default is `FALSE` ie. include all
  available geographical codes available in the codebook. (#157)
- Fixed #139 for `orgdata.debug.geo` keep original geo codes for enumeration
  areas before adding 9999. (#140)
- Fixed #142 show codes that have problem to recode directly instead of row
  numbers (#144)
- Save all codes that have problem in `log` environment for easy access. To list
  the codes is either with `log$code00` or `log$codeShort`
- Recode geo even when argument `aggregate = FALSE` in `make_file()` function.
- Rename `make_filegroup` and `lag_filgruppe` to plural ie. `make_filegroups`
  and `lag_filgrupper`.
- Use options `orgdata.debug.rows` to select only specific row(s) for further
  processing. It can be activated via global options with
  `options(orgdata.debug.rows = 20:50)` or via argument `row = 20:50` in
  `make_file()` to select row 20 to 50.
- Fixed #135 with incorrect geo recode. (#131)
- Make multiple filegroups via `make_filegroups`. (#137)
- Fixed #132 LANDSSB must be string
- Convert whitespace to NA to be able to delete all rows with NA
- Fixed #119 able to mutate for TABS and VALS as well (#126)
- Fixed #122 delete rows with NA via EXTRA column (#127)
- Fixed #118 warning text when column(s) aren't defined in FILGRUPPE and will be
  deleted (#128)
- Edit error message for columns with existing NA value before aggregating.
  Total value will be NA and this will conflict with the allready existing NA
  category in the aggregated column(s). Therefore existing NA value in the
  selected column(s) must be recoded to a valid value.
- Use `options(orgdata.debug.geo = TRUE)` to keep old geo codes for debuging (#120)
- Use `reset_options()` to reset to default options.
- Warn when process discontinued due to debugging.
- Add vignettes for **Standardize git** and **Debugging**
- Fixed #121 recode geographical code for split codes (#120)
- Change database filename to **raw-database_BE.accdb**
- Fixed #106 split long messages (#107)
- Fixed #108 #112 grunnkrets codes that have changed before 2002 not available
  via API from SSB while code changes for municipality includes changes
  from 1977. Check from [SSB
  website](https://www.ssb.no/klass/klassifikasjoner/131/versjoner "SSB"). We
  use the municipality codes to create uspesified grunnkrets codes for data
  before 2002 (#109 #113)
- Fixed #110 updating SQL code for new table name for codebook (#111)
- Check columns to aggregate for any possible `NA` (#98). Columns that have `NA`
  should be recoded to `uoppgitt` or something equivalent since leaving the
  category to `NA` will conflict with `NA` representing total value when
  aggregating.
- Fixed #100 for grunnkrets that ends with `00` have no correspond codes from
  SSB API. Need to add it manually (#101)
- Fixed #99 when geo codes fails to be recoded then the row index will be shown (#103)
- Geo codes ends with 4 zeros `xxxx0000` neither have equivalent codes from SSB
  nor representing a correct coding structure as so called `Delområde` that ends
  with 2 zeros `xxxxxx00`. To avoid missing the information, these geo codes are
  recoded to `xxxx9999` with function `is_grunnkrets_0000()` as in PR (#103).
- `see_file()` accepts just a single numeric as well.
- Fixed #85 `see_file()` list all the columns when columnames or column indexes
  are not specified. The variables are sorted whenever possible. (#87)
- Add more function tests (#88)
- Exclude `LANDSSB` in aggregate when split to `LANDBAK` and `INNVKAT`. This is
  because code `0` will be recoded to `20` when split and causes unnecessary more
  rows (#84)
- Delete deprecated functions.
- Fixed #93 when source level can't be identified due to `NA`.
- Fixed #95 for grunnkrets codes that aren't missing but have less number of
  digits ie. less than 7 digits. Assuming these are codes for municipality then
  `9999` is added at the end of these codes (#96)
- Gives row number for GEO codes that get coerced as `NA` when converted to
  integer. This will make it easy to check in the original raw data (#96)
- Aggregate now give total to all dimensions including those specified in
  `AGGKOL` (#82)
- Function `see_file()` accept column index as well (#83)
- Recode variables using regular expression when defined in codebook with type
  `RE`. Finding pattern can either be written in ordinary regular expression ie.
  `\\d{4}.*` or with `rex()` package. (#78)
- New feature for checking categories for variables with `see_file()` (#75)
- Fixed #65 make TABS and VALS dynamic for easy extension for these columns (#66)
- Fixed #64 recode of variable that has different class (#68)
- Fixed #63 implicit null includes all possible VAL columns when exist (#69)
- Fixed #70 recode GEO of different object class (#71)
- Fixed #67 aggregate with total values for standard variables ie. `UTDANN`,
  `LANDSSB`, `LANDBAK` and `INNVKAT` (#72)
- Fixed #61 use AGGKOL in Access registration database to specify other columns to
  aggregate other than the standard eg. `KJONN`, `TAB1`, `TAB2` etc. (#73)
- Fixed #55 to recode standard variables via codebook instead of hard coded (#58)
- Fixed #52 skip split if not specified (#59)
- Fixed #57 split column with duplicated values will keep the original column (#60)
- Fixed #56 aggregate all VAL columns whenever specified and not only specific to
  `VAL1` (#62)
- Edit verbose messages
- Reshape dataset from wide to long. Reshape can have more than one `measure
  variables`. Please read how this is specified in Access registration database.
- Split columns must have equal number of values to the defined `SPLITTIL`.
  Duplicate the value if it is less than the maximum `SPLITTIL`. For example for
  value `0` in column `LANDSSB` which will be split into `LANDBAK` and
  `INNVKAT`, the value will be duplicated into `00` to avoid split with value `NA`.
- Recode for `LANDBAK` and `INNVKAT` after aggregating are done internally ie.
  hard coded, in `do_aggregate_recode_standard()`. Total is coded with `20`. Any
  eventuality for future change should also look other related functions such as
  `is_aggregate_standard_cols()` and `is_col_num()`.
- Change argument parameter for `find_spec()` function.
- Update text document in several places.
- Add colour type *warn2* for warning message without `Warning:` prefix.
- Request (#43) messages with specific colour
- Fixed (#46) recode to string even though columns is type integer or numeric.
- Unknown bydel ie. *(uoppgitt)* is added when enumeration areas codes ie.
  *(grunnkrets)* for bydel is `XXXX9999` in function `geo_level()`.
- Add unknown grunnkrets for kommune when not available since some of the
  datasets have unknown grunnkrets that aren't listed in API downloaded data (#39).
- Exclude `TAB1`, `TAB2` and `TAB3` from being aggregated. (#44) 
- Recode for aggregated variables uses `AG` in TYPE column in the codebook
  instead of FILGRUPPE with `AGGREGATE` as it was implemented in ver 0.2.0. This
  will make it possible so specify FILGRUPPE and LESID to implement the
  principle for **GENERAL**, **COMMON** and **SPECIFIC** variables.
- Change function name `do_aggregate_recode` to `do_aggregate_recode_standard`
  for standard variables.
- Recode for aggregated categories can be defined in *Recode* form ie. codebook,
  and use `AGGREGATE` in the specification under FILGRUPPE
- Delete rows when defined in codebook using minus symbol under TIL column.
  Similar principles is implemented for **GENERAL**, **COMMON** and **SPECIFIC**
  feature as in recode. Read detail in [ver 0.0.5 - alpha](https://helseprofil.github.io/orgdata/news/index.html#orgdata-0-0-5-alpha).
- Display both columnames to be recoded that are found in the dataset or those
  that aren't found when defined as `ALLE` in the codebook so user will be aware
  of its existence.
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
- When MANHEADER is used then the new columname must be specified in the respective standard column (#21) 
- Deprecated arguments `geo` and `val` in `make_file`. Output data must use standard
  columnames instead of keeping the columnames from original dataset.
- Rename functions `read_raw` or `lesraw` to `make_file` or `lag_fil` (#27)
- Alle functions uses underscore "_" for both english and norwegian.
- Add new columns if one of the standard columns is missing in the original
  data. The value to be inserted to the new column must use symbol less than `<`
  and more than `>`. For instance when column `KJONN` doesn't exist in the
  original data, we can specify with `<2>` in under column `KJONN` in the Access
  registration database. The output will add a new column `KJONN` with value `2`. (#15)
- Fix #13 and #18
- Default `orgdata.verbose` is `TRUE`.
- Options for `orgdata.implicit.null` with default as `TRUE`. Use
  `options(orgdata.implicit.null = FALSE)` to deactivate (#19)
- Changes is in PR #11 and #12 is as the following:
- Implicit zero (#11). Discussion is in [Gist](https://gist.github.com/ybkamaleri/cd789560d595d7a0d6eb46a23395fc51 "implicit-null")
- Use version specific for imported packages.
- Rename standard column `LANDBAK` to `LANDSSB` for column in original data
  received from SOB containing information about country of origin.
- Save file as specified in column `MAPPE` in Access registration database or
  specify in `path` argument for function `save_file`. (#12)
- Changes is in PR #8 is as the following:
- Create GEO code from two separate columns. This has to be defined in Access
  registration under `GEO` with comma separated eg. `nameGeoCol1, nameGeoCol2`.
- Order standard columns in the output dataset with this order for the first four columns:
    - `GEO`, `AAR`, `ALDER`, `KJONN`
- Change norwegian name for `save_file` from `lagfil` to `lagrefil`.
- Use column name `KOLNAVN` instead of `ADDKOL`.
- Rename function `do_addcols` and `get_addcols` to `do_colname` and
  `get_colname` to be consistent with the changes in Access registration database.
- Changes is in PR #4 is as the following:
- Recode variables from specification in `tbl_KodeBok` uses:
  1. **GENERAL** variables are defined in FILGRUPPE as `ALLE` and are used to
     recode variables in all groups.
  2. **COMMON** variables are when FILGRUPPE is specified but have empty LESID.
     This will recode variables within selected group.
  3. **SPECIFIC** variables are when FILGRUPPE and LESID are specified. This
     will recode variables in that specified FILGRUPPE of the specified FILID.

- When all these three specification exist in `tbl_KodeBok`:
   - **SPECIFIC** variables will overrule **COMMON** variables
   - **COMMON** variables will overrule **GENERAL** variables

- Write as `<NA>` in codebook under column `FRA` when specifying missing
  variables indicating that a missing column to be recoded to value in column `TIL`. This
  will differentiate between real missing and a real column value of `NA`. (#5)
  
- Error message will be given if LESID is specified without FILGRUPPE since
  LESID is not unique ID.


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

