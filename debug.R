## Hvordan gjøre man med debugging
## --------------------------------

## Se gamle GEO og nye omkodet GEO
options(orgdata.debug.geo = TRUE)
dt <- make_file("TRANGBODD", koblid = 70, aggregate = FALSE)

## Tilbakestille oppsettet
reset_options()
