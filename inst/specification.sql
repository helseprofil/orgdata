SELECT KOBLID, tbl_Koble.FILID, tbl_Koble.FILGRUPPE, FILNAVN, IBRUKTIL, KONTROLLERT, tbl_Innlesing.*
FROM tbl_Innlesing
INNER JOIN (tbl_Koble
            INNER JOIN tbl_Orgfile
            ON tbl_Koble.FILID = tbl_Orgfile.FILID)
ON tbl_Koble.LESID = tbl_Innlesing.LESID
AND tbl_Koble.FILGRUPPE = tbl_Innlesing.FILGRUPPE
WHERE tbl_Koble.FILGRUPPE = '%s'
AND tbl_Orgfile.IBRUKTIL >= '%f'
