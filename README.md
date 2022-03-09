# orgdata

Denne branch dvs. **user** skal brukes for å beholde alle pakker versjoner som
brukes av orgdata for å sikre at orgdata skal alltid fungere som den skal per
release versjon.

For å bruke det må **user** repo klonet i din pc.

``` sh
git clone -b user https://github.com/helseprofil/orgdata.git
```

Deretter kjøre alle linjene filen *update.R*.

```r
if(!requireNamespace("renv")) install.packages("renv")
renv::restore()
```
