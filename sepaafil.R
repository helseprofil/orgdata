## Start
library(orgdata)
op <- options()
(options(orgdata.verbose = TRUE))


## Les original fil som det er ved å velge FILID
lesfil(1)


## Aggregere utvalgte filer med KOBLID
df <- lesraw("BEFOLKNING", koblid = 13)

## Les filen og bruk oppsettet i Access
df <- lesraw("BEFOLKNING")

## Lag CSV fil
save_file(df, "BEFOLKNING")

## Evt. hvis alle filer har vært kontrollert
lesraw("BEFOLKNING", save = TRUE)


## TIPS AND TRICKS ---------------------------
## Lest bare en bestemt antall rader f.eks 5 rader
lesfil(file = 1, nrows = 5) #for csv fil
