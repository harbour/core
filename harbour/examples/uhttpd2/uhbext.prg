/*
 * $Id$
 */

/************************************************************
*
* Functions candidates to be a part of Harbour's core or RTL
*
*************************************************************/


FUNC split(cSeparator, cString)
LOCAL aRet := {}, nI

  DO WHILE (nI := AT(cSeparator, cString)) > 0
    AADD(aRet, LEFT(cString, nI - 1))
    cString := SUBSTR(cString, nI + LEN(cSeparator))
  ENDDO
  AADD(aRet, cString)
RETURN aRet


FUNC join(cSeparator, aData)
LOCAL cRet := "", nI

  FOR nI := 1 TO LEN(aData)
    IF nI > 1;  cRet += cSeparator
    ENDIF
    IF     VALTYPE(aData[nI]) $ "CM";  cRet += aData[nI]
    ELSEIF VALTYPE(aData[nI]) == "N";  cRet += LTRIM(STR(aData[nI]))
    ELSEIF VALTYPE(aData[nI]) == "D";  cRet += IF(!EMPTY(aData[nI]), DTOC(aData[nI]), "")
    ELSE
    ENDIF
  NEXT
RETURN cRet
