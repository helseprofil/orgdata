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
les_fil(file = 1, nrows = 5) #for csv fil
les_fil(file = 28, n_max = 5) #for excel fil

## Sjekk kategorier evt. omkoding
dt <- les_fil(file = 1)
dt[, .N, keyby = landb] #kategori for landb i original data

df <- lag_fil("TEST01", koblid = 1)
df[, .N, keyby = LANDB] #kategori i omkodet aggregerte data
df[, .N, keyby = LANDF]

## Åpne hjemmeside
website()

## Sjekk versjon
packageVersion("orgdata")
