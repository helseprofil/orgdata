## Start
library(orgdata)

## Les original fil som det er ved å velge FILID
df <- lesfil(1)

## Aggregere utvalgte filer med KOBLID
df <- lesraw("BEFOLKNING", koblid = 13)

## Les filen og bruk oppsettet i Access
df <- lesraw("BEFOLKNING")

## Lagre CSV fil
save_file(df, "BEFOLKNING")

## Evt. hvis alle filer har vært kontrollert
lesraw("BEFOLKNING", save = TRUE)


## TIPS AND TRICKS ------------------------------------
## Lest bare en bestemt antall rader f.eks 5 rader
lesfil(file = 1, nrows = 5) #for csv fil
lesfil(file = 28, n_max = 5) #for excel fil

## Sjekk kategorier evt. omkoding
dt <- lesfil(file = 1)
dt[, .N, keyby = landb] #kategori for landb i original data

df <- lesraw("TEST01", koblid = 1)
df[, .N, keyby = LANDB] #kategori i omkodet aggregerte data
df[, .N, keyby = LANDF]

## Åpne hjemmeside
website()
