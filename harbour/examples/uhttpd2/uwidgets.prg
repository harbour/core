/*
 * $Id$
 */

#include "hbclass.ch"

#pragma -kM+

MEMVAR session, server, get, post

//============================================================
CLASS UWMain
  DATA  aChilds     INIT {}

  METHOD Add()
  METHOD Paint()
ENDCLASS


FUNC UWMainNew()
LOCAL oW := UWMain()
  session["_uthis", "main"] := oW
RETURN oW


METHOD Paint() CLASS UWMain
  UWrite('<html><link href="/files/main.css" type=text/css rel=stylesheet>')
  UWrite('<meta http-equiv="content-type" content="text/html; charset=windows-1257">')
  UWrite('<script language="javascript" src="/files/main.js"></script>')
  UWrite('<body>')
  AEVAL(Self:aChilds, {|x| X:Paint()})
  UWrite('</body></html>')
RETURN Self


METHOD Add(oWidget) CLASS UWMain
  AADD(Self:aChilds, oWidget)
RETURN Self


//============================================================
CLASS UWLayoutGrid
  DATA  aChilds     INIT {{{}}}     // {{{}}, {{}}} ;   {{{}, {}}}

  METHOD Add()
  METHOD Paint()
ENDCLASS


FUNC UWLayoutGridNew()
LOCAL oW := UWLayoutGrid()
RETURN oW


METHOD Paint() CLASS UWLayoutGrid
LOCAL aRow, aCell
  UWrite('<table>')
  FOR EACH aRow IN Self:aChilds
    UWrite('<tr>')
    FOR EACH aCell IN aRow
      UWrite('<td>')
      AEVAL(aCell, {|o| o:Paint()})
      UWrite('</td>')
    NEXT
    UWrite('</tr>')
  NEXT
  UWrite('</table>')
RETURN Self


METHOD Add(oWidget, nRow, nCol) CLASS UWLayoutGrid
LOCAL nI, nJ, aI
  IF nRow > LEN(Self:aChilds)
    FOR nI := LEN(Self:aChilds) + 1 TO nRow
      aI := ARRAY(LEN(Self:aChilds[1]))
      FOR nJ := 1 TO LEN(Self:aChilds[1])
        aI[nJ] := {}
      NEXT
      AADD(Self:aChilds, aI)
    NEXT
  ENDIF
  IF nCol > LEN(Self:aChilds[1])
    FOR nI := LEN(Self:aChilds[1]) + 1 TO nCol
      AEVAL(Self:aChilds, {|x| AADD(x, {})})
    NEXT
  ENDIF
  AADD(Self:aChilds[nRow, nCol], oWidget)
RETURN Self


//============================================================
CLASS UWHtml
  DATA  cText

  METHOD Paint()
ENDCLASS


FUNC UWHtmlNew(cText)
LOCAL oW := UWHtml()
  oW:cText := cText
RETURN oW


METHOD Paint() CLASS UWHtml
  UWrite(Self:cText)
RETURN Self


//============================================================
CLASS UWLabel
  DATA  cText
  DATA  cID
  DATA  cStyle

  METHOD Paint()
ENDCLASS


FUNC UWLabelNew(cText, cID, cStyle)
LOCAL oW := UWLabel()
  oW:cText := cText
  SetWId(oW, cID)
  oW:cStyle := cStyle
RETURN oW


METHOD Paint() CLASS UWLabel
  UWrite('<div' + IIF(Self:cID != NIL, ' id="' + Self:cID + '"', "") + ;
         IIF(Self:cStyle != NIL, ' style="' + Self:cStyle + '"', "") + '>' + ;
         UHtmlEncode(Self:cText) + '</span>')
RETURN Self


//============================================================
CLASS UWForm
  DATA  cAction
  DATA  cMethod   INIT "POST"
  DATA  aChilds   INIT {}

  METHOD Add()
  METHOD Paint()
ENDCLASS


FUNC UWFormNew(cAction)
LOCAL oW := UWForm()
  oW:cAction := cAction
RETURN oW


METHOD Add(oWidget) CLASS UWForm
  AADD(Self:aChilds, oWidget)
RETURN Self


METHOD Paint() CLASS UWForm
  UWrite('<form action="' + Self:cAction + '" method="' + Self:cMethod + '">')
  AEVAL(Self:aChilds, {|x| X:Paint()})
  UWrite('</form>')
RETURN Self


//============================================================
CLASS UWInput
  DATA  cName
  DATA  cValue
  DATA  cID
  DATA  cStyle

  METHOD Paint()
ENDCLASS


FUNC UWInputNew(cName, cValue, cID, cStyle)
LOCAL oW := UWInput()
  oW:cName := cName
  oW:cValue := cValue
  SetWId(oW, cID)
  oW:cStyle := cStyle
RETURN oW


METHOD Paint() CLASS UWInput
  UWrite('<input type="text" name="' + IIF(Self:cName != NIL, Self:cName, "") + ;
         '" value="' + IIF(Self:cValue != NIL, UHtmlEncode(Self:cValue), "") + '">')
RETURN Self


//============================================================
CLASS UWPassword
  DATA  cName
  DATA  cValue

  METHOD Paint()
ENDCLASS


FUNC UWPasswordNew(cName)
LOCAL oW := UWPassword()
  oW:cName := cName
RETURN oW


METHOD Paint() CLASS UWPassword
  UWrite('<input type="password" name="' + IIF(Self:cName != NIL, Self:cName, "") + ;
         '" value="' + IIF(Self:cValue != NIL, Self:cValue, "") + '">')
RETURN Self


//============================================================
CLASS UWSubmit
  DATA  cName
  DATA  cValue

  METHOD Paint()
ENDCLASS


FUNC UWSubmitNew(cName, cValue)
LOCAL oW := UWSubmit()
  oW:cName := cName
  oW:cValue := cValue
RETURN oW


METHOD Paint() CLASS UWSubmit
  UWrite('<input type="submit" name="' + IIF(Self:cName != NIL, Self:cName, "") + ;
         '" value="' + IIF(Self:cValue != NIL, UHtmlEncode(Self:cValue), "") + '">')
RETURN Self


//============================================================
CLASS UWSeparator
  METHOD Paint()
ENDCLASS


FUNC UWSeparatorNew()
LOCAL oW := UWSeparator()
RETURN oW


METHOD Paint() CLASS UWSeparator
  UWrite('<hr>')
RETURN Self


//============================================================
CLASS UWMenu
  DATA  aItems    INIT {}

  METHOD AddItem()
  METHOD Paint()
ENDCLASS


FUNC UWMenuNew()
LOCAL oB := UWMenu()
RETURN oB


METHOD AddItem(cTitle, cLink) CLASS UWMenu
  AADD(Self:aItems, {cTitle, cLink})
RETURN Self


METHOD Paint() CLASS UWMenu
LOCAL nI
  UWrite('<div>')
  FOR nI := 1 TO LEN(Self:aItems)
     IF nI != 1
       UWrite('&nbsp;|&nbsp;')
     ENDIF
     UWrite('<a href="' + Self:aItems[nI, 2] + '">' + UHtmlEncode(Self:aItems[nI, 1]) + '</a>')
  NEXT
  UWrite('</div>')
RETURN Self


//============================================================
CLASS UWBrowse
  DATA cID
  DATA aColumns   INIT {}
  DATA nArea

  DATA nRecno
  DATA lBof       INIT .F.
  DATA lEof       INIT .F.

  METHOD AddColumn()
  METHOD Paint()
  METHOD PaintBody()
  METHOD Ajax()
  METHOD Skipper()
ENDCLASS


FUNC UWBrowseNew(cID)
LOCAL oW := UWBrowse()
  SetWId(oW, cID)
  oW:nArea := SELECT()
RETURN oW


METHOD AddColumn(nID, cTitle, cField, lRaw) CLASS UWBrowse
  AADD(Self:aColumns, {nID, cTitle, cField, !EMPTY(lRaw)})
RETURN Self


METHOD Paint() CLASS UWBrowse
  UWrite('<div id="' + Self:cID + '">')
  Self:PaintBody()
  UWrite('</div>')
RETURN Self


METHOD PaintBody() CLASS UWBrowse
LOCAL nI, nJ, xI, xField, nArea

  nArea := SELECT()
  DBSELECTAREA(Self:nArea)
  IF Self:nRecNo == NIL
    DBGOTOP()
    Self:nRecno := RECNO()
    Self:Skipper(0)
  ELSE
    DBGOTO(Self:nRecno)
    Self:Skipper(0)
    Self:nRecno := RECNO()
  ENDIF
  IF ! Self:lBof
    UWrite('<a href="" onclick="ubrcall(' + "'" + Self:cID + "','action=prevpg');return false;" + '">&lt;</a> ')
  ELSE
    UWrite('&lt; ')
  ENDIF
  IF ! Self:lEof
    UWrite('<a href="" onclick="ubrcall(' + "'" + Self:cID + "','action=nextpg');return false;" + '">&gt;</a> ')
  ELSE
    UWrite('&gt; ')
  ENDIF
  UWrite('<table class="ubr"><tr>')

  // Header
  UWrite('<tr>')
  FOR nI := 1 TO LEN(Self:aColumns)
     UWrite('<th>' + UHtmlEncode(Self:aColumns[nI, 2]) + '</th>')
  NEXT
  UWrite('</tr>')

  // Body
  DBGOTO(Self:nRecno)
  FOR nI := 1 TO 20
    IF EOF();  EXIT
    ENDIF
    UWrite('<tr>')
    FOR nJ := 1 TO LEN(Self:aColumns)
      xField := Self:aColumns[nJ, 3]
      IF VALTYPE(xField) == "C"
        xI := FIELDGET(FIELDPOS(xField))
      ELSEIF VALTYPE(xField) == "B"
        xI := EVAL(xField)
      ENDIF
      IF     VALTYPE(xI) == "C";  xI := TRIM(xI)
      ELSEIF VALTYPE(xI) == "N";  xI := STR(xI)
      ELSEIF VALTYPE(xI) == "D";  xI := DTOC(xI)
      ELSE ;  xI := "VALTYPE()==" + VALTYPE(xI)
      ENDIF
      IF ! Self:aColumns[nJ, 4]
        xI := UHtmlEncode(xI)
      ENDIF
      UWrite('<td><nobr>' + xI + '</nobr></td>')
    NEXT
    UWrite('</tr>')
    DBSKIP()
  NEXT
  UWrite('</table>')
  DBSELECTAREA(nArea)
RETURN Self


METHOD Ajax(cAction) CLASS UWBrowse
LOCAL nI, nJ, aI, aJ, xI

  IF cAction == "nextpg"
     (Self:nArea)->(Self:Skipper(20))
  ELSEIF cAction == "prevpg"
     (Self:nArea)->(Self:Skipper(-20))
  ENDIF
  Self:PaintBody()
RETURN Self


METHOD Skipper(nSkip) CLASS UWBrowse
  DBGOTO(Self:nRecno)
  DBSKIP(nSkip)
  Self:nRecno := RECNO()
  IF EOF()
    DBSKIP(-1)
    Self:nRecno := RECNO()
    Self:lEof := EOF()
  ELSE
    DBSKIP(20)
    Self:lEof := EOF()
  ENDIF
  DBGOTO(Self:nRecno)
  IF BOF()
     Self:lBof := .T.
  ELSE
    DBSKIP(-1)
    IF BOF()
      Self:lBof := .T.
    ELSE
      DBSKIP(1)
      Self:lBof := .F.
    ENDIF
  ENDIF
  Self:nRecno := RECNO()
RETURN Self


/********************************************************************
*
*  Default procedure handlers
*
********************************************************************/

PROC UProcWidgets(cURL, aMap)
LOCAL aStack, aURL, aFrame, cI, nI, nL, lRet

  ? "cURL:", cURL
  IF HB_HHasKey(aMap, cURL)
    // aStack[i] = {url_part, function, variables}
    IF (aStack := HGetDef(session, "_ustack")) == NIL
      session["_ustack"] := aStack := {}
    ENDIF

    aURL := split("/", cURL)
    nI := 1
    nL := MIN(LEN(aURL), LEN(aStack))
    DO WHILE nI <= nL
      IF aStack[nI, 1] == aURL[nI]
        nI++
      ELSE
        EXIT
      ENDIF
    ENDDO

    // Exit procedures
    DO WHILE nI <= LEN(aStack)
      aFrame := ATAIL(aStack)
      IF aFrame[2] != NIL
         session["_uthis"] := aFrame[3]
         EVAL(aFrame[2], "EXIT")
         session["_uthis"] := NIL
      ENDIF
      ASIZE(aStack, LEN(aStack) - 1)
    ENDDO
    aFrame := NIL

    lRet := .T.
    // Enter procedures
    DO WHILE nI <= LEN(aURL)
      cI := join("/", ASIZE(ACLONE(aURL), nI))
      IF HB_HHasKey(aMap, cI)
        session["_uthis"] := {"idhash"=>{=>}}
        IF (lRet := EVAL(aMap[cI], "INIT")) == .T.
          AADD(aStack, {aURL[nI], aMap[cI], session["_uthis"]})
          session["_uthis"] := NIL
        ELSE
          session["_uthis"] := NIL
          EXIT
        ENDIF
      ELSE
        AADD(aStack, {aURL[nI], NIL, NIL})
      ENDIF
      nI++
    ENDDO

    IF lRet
      session["_uthis"] :=  ATAIL(aStack)[3]
      IF server["REQUEST_METHOD"] == "GET"
        EVAL(ATAIL(aStack)[2], "GET")
      ELSEIF server["REQUEST_METHOD"] == "POST"
        EVAL(ATAIL(aStack)[2], "POST")
      ENDIF
      ATAIL(aStack)[3] := session["_uthis"]
      session["_uthis"] := NIL
    ENDIF
  ELSE
    USetStatusCode(404)
  ENDIF
RETURN


PROC UWDefaultHandler(cMethod)
LOCAL cID, oW
  IF cMethod == "GET"
    IF (cID := HGetDef(get, "ajax")) == NIL
      session["_uthis", "main"]:Paint()
    ELSE
      IF (oW := GetWidgetById(cID)) != NIL
        UAddHeader("Content-type", "text/html; charset=windows-1257")
        oW:Ajax(HGetDef(get, "action"))
      ENDIF
    ENDIF
  ENDIF
RETURN


STATIC PROC SetWId(oW, cID)
  IF cID != NIL
    oW:cID := cID
    session["_uthis", "idhash", cID] := oW
  ENDIF
RETURN


FUNC GetWidgetById(cID)
RETURN HGetDef(session["_uthis", "idhash"], cID)
