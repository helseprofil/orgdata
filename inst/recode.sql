select FILGRUPPE, LESID, KOL, TYPE, FRA, TIL
from tbl_Kode
where VERSJONTIL = #9999-01-01# and ( FILGRUPPE = 'ALLE' or LESID = %d )
