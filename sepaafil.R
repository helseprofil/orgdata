## Start
library(orgdata)
op <- options()
(options(orgdata.verbose = TRUE))


## Les original fil som det er ved å velge FILID
lesfil(6)


## Aggregere utvalgte filer med KOBLID
df <- lesorg("BEFOLKNING", koblid = 13)
df

## Les filen og bruk oppsettet i Access
df <- lesraw("BEFOLKNING")

## Lag CSV fil
save_file(df, "BEFOLKNING")

## Evt. hvis alle filer har vært kontrollert
lesraw("BEFOLKNING", save = TRUE)
