## Start
library(orgdata)

##   HUSK Å ALLTID TILORDNE ET OBJEKT for make_file og read_file ! ###
##   dt <- make_file()
## dvs. ^

## Les original fil som det er ved å velge FILID
df <- les_fil(1)

## Aggregere utvalgte filer med KOBLID
dt <- lag_fil("BEFOLKNING", koblid = 13)

## Les filen og bruk oppsettet i Access
dt <- lag_fil("BEFOLKNING")

## Lagre CSV fil
save_file(dt, "BEFOLKNING")

## Å lage det et annet sted
save_file(dt, name = "MinFil", path = "C:/Navn/Til/Mappen")

## Evt. hvis alle filer har vært kontrollert
lag_fil("BEFOLKNING", save = TRUE)

## Lager flere FILGRUPPER, velg måten du liker å skrive det
## FILGRUPPE blir laget som csv direkte
lag_filgrupper("BEFOLKNING", "NEET", "DODE") #med " "
lag_filgrupper(BEFOLKNING, NEET, DODE) #uten



## TIPS AND TRICKS ------------------------------------
## Lest bare en bestemt antall rader f.eks 5 rader
## kan også brukes andre argumenter f.eks header = FALSE, skip = 0, sep = ; osv
les_fil(file = 1, nrows = 5)

## Sjekk kategorier evt. omkoding
df <- les_fil(file = 1)
se_fil(df) #alle kolonner
se_fil(df, 1) #bare kolonne 1
se_fil(df, c(1:5)) #kolonner 1 til 5
se_fil(df, c(1,4,6))
se_fil(df, LANDBAK, INNVKAT)

df[, .N, keyby = landb] #kategori for landb i original data

dt <- lag_fil("TEST01", koblid = 1)
dt[, .N, keyby = LANDB] #kategori i omkodet aggregerte data
dt[, .N, keyby = LANDF]

## Få å finne kategorier bare for kommune eller spesifik kjonn
dt[LEVEL == "kommune", .N, keyby = LANDBAK]
dt[KJONN == 1, .N, keyby = LANDBAK]

## Sjekk koder f.eks GEO 1050110
dt[GEO == 1050110]

## Sjekk flere koder samtidig
dt[GEO %in% c(1050110, 1050122)]

## Sjekk GEO koder starter med 301
dt[GEO %like% "^301"]

## Sjekk GEO avsluttet med 99
dt[GEO %like% "99$"]

## Se alle rader i datasettet
View(df)

## Se alle GEO koder som har problemer med omkoding
log$code00  #for code som avsluttet med 00 eller 0000
log$codeShort #for coder som ikke er 7 eller 8 digit

## Åpne hjemmeside
website()

## Sjekk versjon
packageVersion("orgdata")

## Se alle global options
orgdata:::opt.orgdata

## DEBUGGING ----------------------------
## For å se original kode og hva det skal bli kodet til
debug_opt("geo")

## For å se hva kodene skal bli aggregert til
debug_opt("aggregate")

## HUSK å nullstille etter bruk av debug funksjon
reset_opt()
