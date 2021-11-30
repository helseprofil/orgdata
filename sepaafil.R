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

## Lager flere FILGRUPPER, velg måten du liker å skrive det
## FILGRUPPE blir laget som csv direkte
lag_filgruppe("BEFOLKNING", "NEET", "DODE") #med " "
lag_filgruppe(BEFOLKNING, NEET, DODE) #uten



## TIPS AND TRICKS ------------------------------------
## Lest bare en bestemt antall rader f.eks 5 rader
## kan også brukes andre argumenter f.eks header = FALSE, skip = 0, sep = ; osv
les_fil(file = 1, nrows = 5)

## Sjekk kategorier evt. omkoding
dt <- les_fil(file = 1)
se_fil(dt) #alle kolonner
se_fil(dt, 1) #bare kolonne 1
se_fil(dt, c(1:5)) #kolonner 1 til 5
se_fil(dt, c(1,4,6))
se_fil(dt, LANDBAK, INNVKAT)

dt[, .N, keyby = landb] #kategori for landb i original data

df <- lag_fil("TEST01", koblid = 1)
df[, .N, keyby = LANDB] #kategori i omkodet aggregerte data
df[, .N, keyby = LANDF]

## Få å finne kategorier bare for kommune eller spesifik kjonn
df[LEVEL == "kommune", .N, keyby = LANDBAK]
df[KJONN == 1, .N, keyby = LANDBAK]

## Sjekk hvilke linje
df[5] #Viser linje nr. 5
df[c(1,5,6)] #Viser linjer 1,5 og 8
df[c(2, 4:8)] #Viser linjer 2 og 4 til 8

## Se alle rader i datasettet
View(df)

## Åpne hjemmeside
website()

## Sjekk versjon
packageVersion("orgdata")

## Se alle global options
orgdata:::opt.orgdata
