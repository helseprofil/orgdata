select FILGRUPPE, LESID, KOL, FRA, TIL
from tbl_Kode
where (FILGRUPPE = '%s' or FILGRUPPE = 'ALLE')
  and VERSJONTIL = #9999-01-01#
  and (LESID is NULL or LESID = '%s')
  and TIL <> '-'
  and TYPE <> 'AG'
  and TYPE <> 'KB'
