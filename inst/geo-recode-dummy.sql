SELECT oldCode, currentCode, changeOccurred
FROM %s
WHERE CInt(changeOccurred) < %d;
