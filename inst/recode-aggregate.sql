select FILGRUPPE, KOL, TYPE, FRA, TIL
from tbl_Kode
where FILGRUPPE = '%s' and VERSJONTIL = #9999-01-01#
