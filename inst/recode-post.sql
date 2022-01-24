select FILGRUPPE, KOL, TYPE, FRA, TIL
from tbl_KodeBok
where FILGRUPPE = '%s'
  and VERSJONTIL = #9999-01-01#
  and TYPE = 'PS'
