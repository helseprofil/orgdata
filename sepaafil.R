## Start
library(orgdata)

## Les original fil som det er ved å velge FILID
df <- les_fil(1)

## Aggregere utvalgte filer med KOBLID
df <- lag_fil("BEFOLKNING", koblid = 13)

## Les filen og bruk oppsettet i Access
df <- lag_fil("BEFOLKNING")

## Lagre CSV fil
save_file(df, "BEFOLKNING")

## Evt. hvis alle filer har vært kontrollert
lag_fil("BEFOLKNING", save = TRUE)


## TIPS AND TRICKS ------------------------------------
## Lest bare en bestemt antall rader f.eks 5 rader
## kan også brukes andre argumenter f.eks header = FALSE, skip = 0, sep = ; osv
les_fil(file = 1, nrows = 5)

## Sjekk kategorier evt. omkoding
dt <- les_fil(file = 1)
dt[, .N, keyby = landb] #kategori for landb i original data

df <- lag_fil("TEST01", koblid = 1)
df[, .N, keyby = LANDB] #kategori i omkodet aggregerte data
df[, .N, keyby = LANDF]

## Få å finne kategorier bare for kommune eller spesifik kjonn
df[LEVEL == "kommune", .N, keyby = LANDBAK]
df[KJONN == 1, .N, keyby = LANDBAK]


## Åpne hjemmeside
website()

## Sjekk versjon
packageVersion("orgdata")

## Se alle global options
orgdata:::opt.orgdata
