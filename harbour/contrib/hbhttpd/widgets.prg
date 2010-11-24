/*
 * $Id$
 */

#include "hbclass.ch"

#pragma -km+

MEMVAR session, server, get, post

//============================================================

CREATE CLASS UWMain

   DATA  aChilds     INIT {}

   METHOD Add( oWidget )
   METHOD Paint()

ENDCLASS

FUNCTION UWMainNew()

   LOCAL oW := UWMain()

   session["_uthis", "main"] := oW

   RETURN oW

METHOD Paint() CLASS UWMain

   UWrite( '<html><link href="/files/main.css" type=text/css rel=stylesheet>' )
   UWrite( '<meta http-equiv="content-type" content="text/html; charset=windows-1257">' )
   UWrite( '<script language="javascript" src="/files/main.js"></script>' )
   UWrite( '<body>' )
   AEval( Self:aChilds, {|x| X:Paint() } )
   UWrite( '</body></html>' )

   RETURN Self

METHOD Add( oWidget ) CLASS UWMain

   AAdd( Self:aChilds, oWidget )

   RETURN Self

//============================================================

CREATE CLASS UWLayoutGrid

   DATA  aChilds     INIT { { {} } }     // {{{}}, {{}}} ;   {{{}, {}}}

   METHOD Add( oWidget, nRow, nCol )
   METHOD Paint()

ENDCLASS

FUNCTION UWLayoutGridNew()

   LOCAL oW := UWLayoutGrid()

   RETURN oW

METHOD Paint() CLASS UWLayoutGrid

   LOCAL aRow, aCell

   UWrite( '<table>' )
   FOR EACH aRow IN Self:aChilds
      UWrite( '<tr>' )
      FOR EACH aCell IN aRow
         UWrite( '<td>' )
         AEval( aCell, {|o| o:Paint() } )
         UWrite( '</td>' )
      NEXT
      UWrite( '</tr>' )
   NEXT
   UWrite( '</table>' )

   RETURN Self

METHOD Add( oWidget, nRow, nCol ) CLASS UWLayoutGrid

   LOCAL nI, nJ, aI

   IF nRow > Len( Self:aChilds )
      FOR nI := Len( Self:aChilds ) + 1 TO nRow
         aI := Array( Len( Self:aChilds[1] ) )
         FOR nJ := 1 TO Len( Self:aChilds[1] )
            aI[nJ] := {}
         NEXT
         AAdd( Self:aChilds, aI )
      NEXT
   ENDIF
   IF nCol > Len( Self:aChilds[1] )
      FOR nI := Len( Self:aChilds[1] ) + 1 TO nCol
         AEval( Self:aChilds, {|x| AAdd( x, {} ) } )
      NEXT
   ENDIF
   AAdd( Self:aChilds[nRow, nCol], oWidget )

   RETURN Self

//============================================================

CREATE CLASS UWHtml

   DATA  cText

   METHOD Paint()

ENDCLASS

FUNCTION UWHtmlNew( cText )

   LOCAL oW := UWHtml()

   oW:cText := cText

   RETURN oW

METHOD Paint() CLASS UWHtml

   UWrite( Self:cText )

   RETURN Self

//============================================================

CREATE CLASS UWLabel

   DATA  cText
   DATA  cID
   DATA  cStyle

   METHOD Paint()

ENDCLASS

FUNCTION UWLabelNew( cText, cID, cStyle )

   LOCAL oW := UWLabel()

   oW:cText := cText
   SetWId( oW, cID )
   oW:cStyle := cStyle

   RETURN oW

METHOD Paint() CLASS UWLabel

   UWrite( '<div' + iif( Self:cID != NIL, ' id="' + Self:cID + '"', "" ) + ;
      iif( Self:cStyle != NIL, ' style="' + Self:cStyle + '"', "" ) + '>' + ;
      UHtmlEncode( Self:cText ) + '</span>' )

   RETURN Self

//============================================================

CREATE CLASS UWForm

   DATA  cAction
   DATA  cMethod   INIT "POST"
   DATA  aChilds   INIT {}

   METHOD Add( oWidget )
   METHOD Paint()

ENDCLASS

FUNCTION UWFormNew( cAction )

   LOCAL oW := UWForm()

   oW:cAction := cAction

   RETURN oW

METHOD Add( oWidget ) CLASS UWForm

   AAdd( Self:aChilds, oWidget )

   RETURN Self

METHOD Paint() CLASS UWForm

   UWrite( '<form action="' + Self:cAction + '" method="' + Self:cMethod + '">' )
   AEval( Self:aChilds, {|x| X:Paint() } )
   UWrite( '</form>' )

   RETURN Self

//============================================================

CREATE CLASS UWInput

   DATA  cName
   DATA  cValue
   DATA  cID
   DATA  cStyle

   METHOD Paint()

ENDCLASS

FUNCTION UWInputNew( cName, cValue, cID, cStyle )

   LOCAL oW := UWInput()

   oW:cName := cName
   oW:cValue := cValue
   SetWId( oW, cID )
   oW:cStyle := cStyle

   RETURN oW

METHOD Paint() CLASS UWInput

   UWrite( '<input type="text" name="' + iif( Self:cName != NIL, Self:cName, "" ) + ;
      '" value="' + iif( Self:cValue != NIL, UHtmlEncode( Self:cValue ), "" ) + '">' )

   RETURN Self

//============================================================

CREATE CLASS UWPassword

   DATA  cName
   DATA  cValue

   METHOD Paint()

ENDCLASS

FUNCTION UWPasswordNew( cName )

   LOCAL oW := UWPassword()

   oW:cName := cName

   RETURN oW

METHOD Paint() CLASS UWPassword

   UWrite( '<input type="password" name="' + iif( Self:cName != NIL, Self:cName, "" ) + ;
      '" value="' + iif( Self:cValue != NIL, Self:cValue, "" ) + '">' )

   RETURN Self

//============================================================

CREATE CLASS UWSubmit

   DATA  cName
   DATA  cValue

   METHOD Paint()

ENDCLASS

FUNCTION UWSubmitNew( cName, cValue )

   LOCAL oW := UWSubmit()

   oW:cName := cName
   oW:cValue := cValue

   RETURN oW

METHOD Paint() CLASS UWSubmit

   UWrite( '<input type="submit" name="' + iif( Self:cName != NIL, Self:cName, "" ) + ;
      '" value="' + iif( Self:cValue != NIL, UHtmlEncode( Self:cValue ), "" ) + '">' )

   RETURN Self

//============================================================

CREATE CLASS UWSeparator

   METHOD Paint()

ENDCLASS

FUNCTION UWSeparatorNew()

   LOCAL oW := UWSeparator()

   RETURN oW

METHOD Paint() CLASS UWSeparator

   UWrite( '<hr>' )

   RETURN Self

//============================================================

CREATE CLASS UWMenu

   DATA  aItems    INIT {}

   METHOD AddItem( cTitle, cLink )
   METHOD Paint()

ENDCLASS

FUNCTION UWMenuNew()

   LOCAL oB := UWMenu()

   RETURN oB

METHOD AddItem( cTitle, cLink ) CLASS UWMenu

   AAdd( Self:aItems, { cTitle, cLink } )

   RETURN Self

METHOD Paint() CLASS UWMenu

   LOCAL nI

   UWrite( '<div>' )
   FOR nI := 1 TO Len( Self:aItems )
      IF nI != 1
         UWrite( '&nbsp;|&nbsp;' )
      ENDIF
      UWrite( '<a href="' + Self:aItems[nI, 2] + '">' + UHtmlEncode( Self:aItems[nI, 1] ) + '</a>' )
   NEXT
   UWrite( '</div>' )

   RETURN Self

//============================================================

CREATE CLASS UWBrowse

   DATA cID
   DATA aColumns   INIT {}
   DATA nArea

   DATA nRecno
   DATA lBof       INIT .F.
   DATA lEof       INIT .F.

   METHOD AddColumn( nID, cTitle, cField, lRaw )
   METHOD Paint()
   METHOD PaintBody()
   METHOD Ajax( cAction )
   METHOD Skipper( nSkip )

ENDCLASS

FUNCTION UWBrowseNew( cID )

   LOCAL oW := UWBrowse()

   SetWId( oW, cID )
   oW:nArea := Select()

   RETURN oW

METHOD AddColumn( nID, cTitle, cField, lRaw ) CLASS UWBrowse

   AAdd( Self:aColumns, { nID, cTitle, cField, !Empty( lRaw ) } )

   RETURN Self

METHOD Paint() CLASS UWBrowse

   UWrite( '<div id="' + Self:cID + '">' )
   Self:PaintBody()
   UWrite( '</div>' )

   RETURN Self

METHOD PaintBody() CLASS UWBrowse

   LOCAL nI, nJ, xI, xField, nArea

   nArea := Select()
   dbSelectArea( Self:nArea )
   IF Self:nRecNo == NIL
      DBGOTOP()
      Self:nRecno := RecNo()
      Self:Skipper( 0 )
   ELSE
      dbGoto( Self:nRecno )
      Self:Skipper( 0 )
      Self:nRecno := RecNo()
   ENDIF
   IF ! Self:lBof
      UWrite( '<a href="" onclick="ubrcall(' + "'" + Self:cID + "','action=prevpg');return false;" + '">&lt;</a> ' )
   ELSE
      UWrite( '&lt; ' )
   ENDIF
   IF ! Self:lEof
      UWrite( '<a href="" onclick="ubrcall(' + "'" + Self:cID + "','action=nextpg');return false;" + '">&gt;</a> ' )
   ELSE
      UWrite( '&gt; ' )
   ENDIF
   UWrite( '<table class="ubr"><tr>' )

   // Header
   UWrite( '<tr>' )
   FOR nI := 1 TO Len( Self:aColumns )
      UWrite( '<th>' + UHtmlEncode( Self:aColumns[nI, 2] ) + '</th>' )
   NEXT
   UWrite( '</tr>' )

   // Body
   dbGoto( Self:nRecno )
   FOR nI := 1 TO 20
      IF Eof();  EXIT
      ENDIF
      UWrite( '<tr>' )
      FOR nJ := 1 TO Len( Self:aColumns )
         xField := Self:aColumns[nJ, 3]
         IF ValType( xField ) == "C"
            xI := FieldGet( FieldPos( xField ) )
         ELSEIF ValType( xField ) == "B"
            xI := Eval( xField )
         ENDIF
         IF     ValType( xI ) == "C";  xI := RTrim( xI )
         ELSEIF ValType( xI ) == "N";  xI := Str( xI )
         ELSEIF ValType( xI ) == "D";  xI := DToC( xI )
         ELSE ;  xI := "VALTYPE()==" + ValType( xI )
         ENDIF
         IF ! Self:aColumns[nJ, 4]
            xI := UHtmlEncode( xI )
         ENDIF
         UWrite( '<td><nobr>' + xI + '</nobr></td>' )
      NEXT
      UWrite( '</tr>' )
      dbSkip()
   NEXT
   UWrite( '</table>' )
   dbSelectArea( nArea )

   RETURN Self

METHOD Ajax( cAction ) CLASS UWBrowse

   IF cAction == "nextpg"
      ( Self:nArea ) -> ( Self:Skipper( 20 ) )
   ELSEIF cAction == "prevpg"
      ( Self:nArea ) -> ( Self:Skipper( - 20 ) )
   ENDIF
   Self:PaintBody()

   RETURN Self

METHOD Skipper( nSkip ) CLASS UWBrowse

   dbGoto( Self:nRecno )
   dbSkip( nSkip )
   Self:nRecno := RecNo()
   IF Eof()
      dbSkip( - 1 )
      Self:nRecno := RecNo()
      Self:lEof := Eof()
   ELSE
      dbSkip( 20 )
      Self:lEof := Eof()
   ENDIF
   dbGoto( Self:nRecno )
   IF Bof()
      Self:lBof := .T.
   ELSE
      dbSkip( - 1 )
      IF Bof()
         Self:lBof := .T.
      ELSE
         dbSkip( 1 )
         Self:lBof := .F.
      ENDIF
   ENDIF
   Self:nRecno := RecNo()

   RETURN Self


/********************************************************************
*
*  Default procedure handlers
*
********************************************************************/

PROCEDURE UProcWidgets( cURL, aMap )

   LOCAL aStack, aURL, aFrame, cI, nI, nL, lRet

   ? "cURL:", cURL
   IF HB_HHasKey( aMap, cURL )
      // aStack[i] = {url_part, function, variables}
      IF ( aStack := hb_HGetDef( session, "_ustack" ) ) == NIL
         session["_ustack"] := aStack := {}
      ENDIF

      aURL := uhttpd_split( "/", cURL )
      nI := 1
      nL := Min( Len( aURL ), Len( aStack ) )
      DO WHILE nI <= nL
         IF aStack[nI, 1] == aURL[nI]
            nI ++
         ELSE
            EXIT
         ENDIF
      ENDDO

      // Exit procedures
      DO WHILE nI <= Len( aStack )
         aFrame := ATAIL( aStack )
         IF aFrame[2] != NIL
            session["_uthis"] := aFrame[3]
            Eval( aFrame[2], "EXIT" )
            session["_uthis"] := NIL
         ENDIF
         ASize( aStack, Len( aStack ) - 1 )
      ENDDO
      aFrame := NIL

      lRet := .T.
      // Enter procedures
      DO WHILE nI <= Len( aURL )
         cI := uhttpd_join( "/", ASize( AClone(aURL ), nI ) )
         IF HB_HHasKey( aMap, cI )
            session["_uthis"] := { "idhash" => { => } }
            IF ( lRet := Eval( aMap[cI], "INIT" ) ) == .T.
               AAdd( aStack, { aURL[nI], aMap[cI], session["_uthis"] } )
               session["_uthis"] := NIL
            ELSE
               session["_uthis"] := NIL
               EXIT
            ENDIF
         ELSE
            AAdd( aStack, { aURL[nI], NIL, NIL } )
         ENDIF
         nI ++
      ENDDO

      IF lRet
         session["_uthis"] :=  ATAIL( aStack )[3]
         IF server["REQUEST_METHOD"] == "GET"
            Eval( ATAIL( aStack )[2], "GET" )
         ELSEIF server["REQUEST_METHOD"] == "POST"
            Eval( ATAIL( aStack )[2], "POST" )
         ENDIF
         ATAIL( aStack )[3] := session["_uthis"]
         session["_uthis"] := NIL
      ENDIF
   ELSE
      USetStatusCode( 404 )
   ENDIF

   RETURN

PROCEDURE UWDefaultHandler( cMethod )

   LOCAL cID, oW

   IF cMethod == "GET"
      IF ( cID := hb_HGetDef( get, "ajax" ) ) == NIL
         session["_uthis", "main"]:Paint()
      ELSE
         IF ( oW := UGetWidgetById( cID ) ) != NIL
            UAddHeader( "Content-type", "text/html; charset=windows-1257" )
            oW:Ajax( hb_HGetDef( get, "action" ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE SetWId( oW, cID )

   IF cID != NIL
      oW:cID := cID
      session["_uthis", "idhash", cID] := oW
   ENDIF

   RETURN

FUNCTION UGetWidgetById( cID )
   RETURN hb_HGetDef( session["_uthis", "idhash"], cID )
