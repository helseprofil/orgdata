## Aggregere filgruppe
library(orgdata)
op <- options()
df <- lesorg("BEFOLKNING")

## Lag CSV fil
save_file(df, "BEFOLKNING")


## Aggregere utvalgte filer med KOBLID
df <- lesorg("BEFOLKNING", koblid = 13)
df

## Se raw fil med FILID
fil <- lesfil(file = 6)
