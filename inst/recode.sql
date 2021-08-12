select FILGRUPPE, LESID, KOL, TYPE, FRA, TIL
from tbl_Kode
where FILGRUPPE = '%s' and VERSJONTIL = #9999-01-01# and (LESID is NULL or LESID = %d )
