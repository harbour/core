/*
 * $Id$
 */

#include "hbclass.ch"

#pragma -km+

MEMVAR session, server, get, post

// ============================================================

CREATE CLASS UWMain

   VAR  aChilds     INIT {}

   METHOD Add( oWidget )
   METHOD Paint()

ENDCLASS

FUNCTION UWMainNew()

   LOCAL oW := UWMain()

   session[ "_uthis", "main" ] := oW

   RETURN oW

METHOD Paint() CLASS UWMain

   UWrite( '<html><link href="/files/main.css" type=text/css rel=stylesheet>' )
   UWrite( '<meta http-equiv="content-type" content="text/html; charset=UTF-8">' )
   UWrite( '<script language="javascript" src="/files/main.js"></script>' )
   UWrite( '<body>' )
   AEval( Self:aChilds, {| x | x:Paint() } )
   UWrite( '</body></html>' )

   RETURN Self

METHOD Add( oWidget ) CLASS UWMain

   AAdd( Self:aChilds, oWidget )

   RETURN Self

// ============================================================

CREATE CLASS UWLayoutGrid

   VAR  aChilds     INIT { { {} } }     // {{{}}, {{}}} ;   {{{}, {}}}

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
         AEval( aCell, {| o | o:Paint() } )
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
         aI := Array( Len( Self:aChilds[ 1 ] ) )
         FOR nJ := 1 TO Len( Self:aChilds[ 1 ] )
            aI[ nJ ] := {}
         NEXT
         AAdd( Self:aChilds, aI )
      NEXT
   ENDIF
   IF nCol > Len( Self:aChilds[ 1 ] )
      FOR nI := Len( Self:aChilds[ 1 ] ) + 1 TO nCol
         AEval( Self:aChilds, {| x | AAdd( x, {} ) } )
      NEXT
   ENDIF
   AAdd( Self:aChilds[ nRow, nCol ], oWidget )

   RETURN Self

// ============================================================

CREATE CLASS UWHtml

   VAR  cText

   METHOD Paint()

ENDCLASS

FUNCTION UWHtmlNew( cText )

   LOCAL oW := UWHtml()

   oW:cText := cText

   RETURN oW

METHOD Paint() CLASS UWHtml

   UWrite( Self:cText )

   RETURN Self

// ============================================================

CREATE CLASS UWLabel

   VAR  cText
   VAR  cID
   VAR  cStyle

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

// ============================================================

CREATE CLASS UWForm

   VAR  cAction
   VAR  cMethod   INIT "POST"
   VAR  aChilds   INIT {}

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
   AEval( Self:aChilds, {| x | x:Paint() } )
   UWrite( '</form>' )

   RETURN Self

// ============================================================

CREATE CLASS UWInput

   VAR  cName
   VAR  cValue
   VAR  cID
   VAR  cStyle

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

// ============================================================

CREATE CLASS UWPassword

   VAR  cName
   VAR  cValue

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

// ============================================================

CREATE CLASS UWSubmit

   VAR  cName
   VAR  cValue

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

// ============================================================

CREATE CLASS UWSeparator

   METHOD Paint()

ENDCLASS

FUNCTION UWSeparatorNew()

   LOCAL oW := UWSeparator()

   RETURN oW

METHOD Paint() CLASS UWSeparator

   UWrite( '<hr>' )

   RETURN Self

// ============================================================

CREATE CLASS UWMenu

   VAR  aItems    INIT {}

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
      UWrite( '<a href="' + Self:aItems[ nI, 2 ] + '">' + UHtmlEncode( Self:aItems[ nI, 1 ] ) + '</a>' )
   NEXT
   UWrite( '</div>' )

   RETURN Self

// ============================================================

CREATE CLASS UWBrowse

   VAR aColumns   INIT {}
   VAR nPageSize  INIT 0
   VAR nPos       INIT 0

   METHOD AddColumn( nID, cTitle, cField, lRaw )
   METHOD Output()

ENDCLASS

FUNC UWBrowseNew()

   LOCAL oW := UWBrowse()

   RETURN oW

METHOD AddColumn( nID, cTitle, cField, lRaw ) CLASS UWBrowse

   AAdd( Self:aColumns, { nID, cTitle, cField, ! Empty( lRaw ) } )

   RETURN Self

METHOD Output() CLASS UWBrowse

   LOCAL cRet := "", nI, xI, xField, nPos, cUrl, cI, lValidate

   cRet += '<table class="ubr"><tr>'

   // Header
   cRet += '<tr>'
   FOR nI := 1 TO Len( Self:aColumns )
      cRet += '<th>' + UHtmlEncode( Self:aColumns[ nI, 2 ] ) + '</th>'
   NEXT
   cRet += '</tr>'

   // Body
   nPos := 0
   dbGoTop()
   IF Self:nPageSize > 0 .AND. Self:nPos > 0
      dbSkip( Self:nPos )
   ENDIF
   DO WHILE ! Eof()
      cRet += '<tr>'
      FOR nI := 1 TO Len( Self:aColumns )
         xField := Self:aColumns[ nI, 3 ]
         IF HB_ISSTRING( xField )
            xI := FieldGet( FieldPos( xField ) )
         ELSEIF HB_ISBLOCK( xField )
            xI := Eval( xField )
         ENDIF
         SWITCH ValType( xI )
         CASE "C"  ; xI := RTrim( xI ); EXIT
         CASE "N"  ; xI := Str( xI ); EXIT
         CASE "D"  ; xI := DToC( xI ); EXIT
         OTHERWISE ; xI := "ValType()==" + ValType( xI )
         ENDSWITCH
         IF ! Self:aColumns[ nI, 4 ]
            xI := UHtmlEncode( xI )
         ENDIF
         cRet += '<td><nobr>' + xI + '</nobr></td>'
      NEXT
      cRet += '</tr>'
      dbSkip()
      IF ++nPos >= Self:nPageSize
         EXIT
      ENDIF
   ENDDO
   cRet += '</table>'
   IF ! Eof() .OR. Self:nPos > 0
      cUrl := server[ "REQUEST_URI" ]
      IF ( nI := At( "?_ucs=", cUrl ) ) == 0
         nI := At( "&_ucs=", cUrl )
      ENDIF
      IF ( lValidate := nI > 0 )
         cUrl := Left( cUrl, nI - 1 )
      ENDIF
      IF ( nI := At( "?_pos=", cUrl ) ) == 0
         nI := At( "&_pos=", cUrl )
      ENDIF
      IF nI > 0
         cUrl := Left( cUrl, nI - 1 )
      ENDIF
      cUrl += iif( "?" $ cUrl, "&", "?" ) + "_pos="
      cRet := '<br>' + cRet
      IF ! Eof()
         cI := cUrl + hb_ntos( Self:nPos + Self:nPageSize )
         cRet := '<a href="' + iif( lValidate, UUrlChecksum( cI ), cI ) + '">&gt;&gt;</a>' + cRet
      ENDIF
      IF Self:nPos > 0
         cI := cUrl + hb_ntos( Max( 0, Self:nPos - Self:nPageSize ) )
         cRet := '<a href="' + iif( lValidate, UUrlChecksum( cI ), cI ) + '">&lt;&lt;</a>&nbsp;&nbsp;' + cRet
      ENDIF
   ENDIF

   RETURN cRet

// ============================================================

CREATE CLASS UWOption

   VAR aOption   INIT {}
   VAR cValue

   METHOD Add( cTitle, cCode, lRaw )
   METHOD Output()

ENDCLASS

FUNC UWOptionNew()

   LOCAL oW := UWOption()

   RETURN oW

METHOD Add( cTitle, cCode, lRaw ) CLASS UWOption

   AAdd( Self:aOption, { iif( ! Empty( lRaw ), cTitle, UHtmlEncode( cTitle ) ), cCode } )

   RETURN Self

METHOD Output() CLASS UWOption

   LOCAL cRet := ""

   AEval( Self:aOption, {| X | cRet += hb_StrFormat( '<option value="%s"%s>%s</option>', UHtmlEncode( X[ 2 ] ), iif( X[ 2 ] == Self:cValue, " selected", "" ), X[ 1 ] ) } )

   RETURN cRet

/********************************************************************
*
*  Default procedure handlers
*
********************************************************************/

PROCEDURE UProcWidgets( cURL, aMap )

   LOCAL aStack, aURL, aFrame, cI, nI, nL, lRet

   IF hb_HHasKey( aMap, cURL )
      // aStack[ i ] := { url_part, function, variables }
      IF ( aStack := hb_HGetDef( session, "_ustack" ) ) == NIL
         session[ "_ustack" ] := aStack := {}
      ENDIF

      aURL := uhttpd_split( "/", cURL )
      nI := 1
      nL := Min( Len( aURL ), Len( aStack ) )
      DO WHILE nI <= nL
         IF aStack[ nI, 1 ] == aURL[ nI ]
            nI++
         ELSE
            EXIT
         ENDIF
      ENDDO

      // Exit procedures
      DO WHILE nI <= Len( aStack )
         aFrame := ATail( aStack )
         IF aFrame[ 2 ] != NIL
            session[ "_uthis" ] := aFrame[ 3 ]
            Eval( aFrame[ 2 ], "EXIT" )
            session[ "_uthis" ] := NIL
         ENDIF
         ASize( aStack, Len( aStack ) - 1 )
      ENDDO
      aFrame := NIL

      lRet := .T.
      // Enter procedures
      DO WHILE nI <= Len( aURL )
         cI := uhttpd_join( "/", ASize( AClone( aURL ), nI ) )
         IF hb_HHasKey( aMap, cI )
            session[ "_uthis" ] := { "idhash" => { => } }
            IF ( lRet := Eval( aMap[ cI ], "INIT" ) ) == .T.
               AAdd( aStack, { aURL[ nI ], aMap[ cI ], session[ "_uthis" ] } )
               session[ "_uthis" ] := NIL
            ELSE
               session[ "_uthis" ] := NIL
               EXIT
            ENDIF
         ELSE
            AAdd( aStack, { aURL[ nI ], NIL, NIL } )
         ENDIF
         nI++
      ENDDO

      IF lRet
         session[ "_uthis" ] := ATail( aStack )[ 3 ]
         IF server[ "REQUEST_METHOD" ] == "GET"
            Eval( ATail( aStack )[ 2 ], "GET" )
         ELSEIF server[ "REQUEST_METHOD" ] == "POST"
            Eval( ATail( aStack )[ 2 ], "POST" )
         ENDIF
         ATail( aStack )[ 3 ] := session[ "_uthis" ]
         session[ "_uthis" ] := NIL
      ENDIF
   ELSE
      USetStatusCode( 404 )
   ENDIF

   RETURN

PROCEDURE UWDefaultHandler( cMethod )

   LOCAL cID, oW

   IF cMethod == "GET"
      IF ( cID := hb_HGetDef( get, "ajax" ) ) == NIL
         session[ "_uthis", "main" ]:Paint()
      ELSE
         IF ( oW := UGetWidgetById( cID ) ) != NIL
            UAddHeader( "Content-type", "text/html; charset=UTF-8" )
            oW:Ajax( hb_HGetDef( get, "action" ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE SetWId( oW, cID )

   IF cID != NIL
      oW:cID := cID
      session[ "_uthis", "idhash", cID ] := oW
   ENDIF

   RETURN

FUNCTION UGetWidgetById( cID )

   RETURN hb_HGetDef( session[ "_uthis", "idhash" ], cID )

STATIC FUNCTION uhttpd_split( cSeparator, cString )

   LOCAL aRet := {}
   LOCAL nI

   DO WHILE ( nI := At( cSeparator, cString ) ) > 0
      AAdd( aRet, Left( cString, nI - 1 ) )
      cString := SubStr( cString, nI + Len( cSeparator ) )
   ENDDO
   AAdd( aRet, cString )

   RETURN aRet

STATIC FUNCTION uhttpd_join( cSeparator, aData )

   LOCAL cRet := ""
   LOCAL nI

   FOR nI := 1 TO Len( aData )

      IF nI > 1
         cRet += cSeparator
      ENDIF

      SWITCH ValType( aData[ nI ] )
      CASE "C"
      CASE "M" ; cRet += aData[ nI ]; EXIT
      CASE "N" ; cRet += hb_ntos( aData[ nI ] ); EXIT
      CASE "D" ; cRet += iif( Empty( aData[ nI ] ), "", DToC( aData[ nI ] ) ); EXIT
      ENDSWITCH
   NEXT

   RETURN cRet
