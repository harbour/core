/*
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
 * www - http://harbour-project.org
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
   AEval( ::aChilds, {| x | x:Paint() } )
   UWrite( '</body></html>' )

   RETURN Self

METHOD Add( oWidget ) CLASS UWMain

   AAdd( ::aChilds, oWidget )

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
   FOR EACH aRow IN ::aChilds
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

   IF nRow > Len( ::aChilds )
      FOR nI := Len( ::aChilds ) + 1 TO nRow
         aI := Array( Len( ::aChilds[ 1 ] ) )
         FOR nJ := 1 TO Len( ::aChilds[ 1 ] )
            aI[ nJ ] := {}
         NEXT
         AAdd( ::aChilds, aI )
      NEXT
   ENDIF
   IF nCol > Len( ::aChilds[ 1 ] )
      FOR nI := Len( ::aChilds[ 1 ] ) + 1 TO nCol
         AEval( ::aChilds, {| x | AAdd( x, {} ) } )
      NEXT
   ENDIF
   AAdd( ::aChilds[ nRow, nCol ], oWidget )

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

   UWrite( ::cText )

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

   UWrite( '<div' + iif( ::cID != NIL, ' id="' + ::cID + '"', "" ) + ;
      iif( ::cStyle != NIL, ' style="' + ::cStyle + '"', "" ) + '>' + ;
      UHtmlEncode( ::cText ) + '</span>' )

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

   AAdd( ::aChilds, oWidget )

   RETURN Self

METHOD Paint() CLASS UWForm

   UWrite( '<form action="' + ::cAction + '" method="' + ::cMethod + '">' )
   AEval( ::aChilds, {| x | x:Paint() } )
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

   UWrite( '<input type="text" name="' + iif( ::cName != NIL, ::cName, "" ) + ;
      '" value="' + iif( ::cValue != NIL, UHtmlEncode( ::cValue ), "" ) + '">' )

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

   UWrite( '<input type="password" name="' + iif( ::cName != NIL, ::cName, "" ) + ;
      '" value="' + iif( ::cValue != NIL, ::cValue, "" ) + '">' )

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

   UWrite( '<input type="submit" name="' + iif( ::cName != NIL, ::cName, "" ) + ;
      '" value="' + iif( ::cValue != NIL, UHtmlEncode( ::cValue ), "" ) + '">' )

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

   AAdd( ::aItems, { cTitle, cLink } )

   RETURN Self

METHOD Paint() CLASS UWMenu

   LOCAL item

   UWrite( '<div>' )
   FOR EACH item IN ::aItems
      IF ! item:__enumIsFirst()
         UWrite( '&nbsp;|&nbsp;' )
      ENDIF
      UWrite( '<a href="' + item[ 2 ] + '">' + UHtmlEncode( item[ 1 ] ) + '</a>' )
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

FUNCTION UWBrowseNew()
   RETURN UWBrowse()

METHOD AddColumn( nID, cTitle, cField, lRaw ) CLASS UWBrowse

   AAdd( ::aColumns, { nID, cTitle, cField, ! Empty( lRaw ) } )

   RETURN Self

METHOD Output() CLASS UWBrowse

   LOCAL cRet := "", nI, xI, xField, nPos, cUrl, cI, lValidate, col

   cRet += '<table class="ubr"><tr>'

   // Header
   cRet += '<tr>'
   FOR EACH col IN ::aColumns
      cRet += '<th>' + UHtmlEncode( col[ 2 ] ) + '</th>'
   NEXT
   cRet += '</tr>'

   // Body
   nPos := 0
   dbGoTop()
   IF ::nPageSize > 0 .AND. ::nPos > 0
      dbSkip( ::nPos )
   ENDIF
   DO WHILE ! Eof()
      cRet += '<tr>'
      FOR EACH col IN ::aColumns
         xField := col[ 3 ]
         IF HB_ISSTRING( xField )
            xI := FieldGet( FieldPos( xField ) )
         ELSEIF HB_ISEVALITEM( xField )
            xI := Eval( xField )
         ENDIF
         SWITCH ValType( xI )
         CASE "C"  ; xI := RTrim( xI ); EXIT
         CASE "N"  ; xI := hb_ntos( xI ); EXIT
         CASE "D"  ; xI := DToC( xI ); EXIT
         OTHERWISE ; xI := "ValType()==" + ValType( xI )
         ENDSWITCH
         IF ! col[ 4 ]
            xI := UHtmlEncode( xI )
         ENDIF
         cRet += '<td><nobr>' + xI + '</nobr></td>'
      NEXT
      cRet += '</tr>'
      dbSkip()
      IF ++nPos >= ::nPageSize
         EXIT
      ENDIF
   ENDDO
   cRet += '</table>'
   IF ! Eof() .OR. ::nPos > 0
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
      cRet := '<br />' + cRet
      IF ! Eof()
         cI := cUrl + hb_ntos( ::nPos + ::nPageSize )
         cRet := '<a href="' + iif( lValidate, UUrlChecksum( cI ), cI ) + '">&gt;&gt;</a>' + cRet
      ENDIF
      IF ::nPos > 0
         cI := cUrl + hb_ntos( Max( 0, ::nPos - ::nPageSize ) )
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

FUNCTION UWOptionNew()
   RETURN UWOption()

METHOD Add( cTitle, cCode, lRaw ) CLASS UWOption

   AAdd( ::aOption, { iif( ! Empty( lRaw ), cTitle, UHtmlEncode( cTitle ) ), cCode } )

   RETURN Self

METHOD Output() CLASS UWOption

   LOCAL cRet := ""

   AEval( ::aOption, {| X | cRet += hb_StrFormat( '<option value="%s"%s>%s</option>', UHtmlEncode( X[ 2 ] ), iif( X[ 2 ] == ::cValue, " selected", "" ), X[ 1 ] ) } )

   RETURN cRet

/********************************************************************
*
*  Default procedure handlers
*
********************************************************************/

PROCEDURE UProcWidgets( cURL, aMap )

   LOCAL aStack, aURL, aFrame, cI, nI, nL, lRet

   IF cURL $ aMap
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
         IF cI $ aMap
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
   LOCAL xData

   FOR EACH xData IN aData

      IF ! xData:__enumIsFirst()
         cRet += cSeparator
      ENDIF

      SWITCH ValType( xData )
      CASE "C"
      CASE "M" ; cRet += xData; EXIT
      CASE "N" ; cRet += hb_ntos( xData ); EXIT
      CASE "D" ; cRet += iif( Empty( xData ), "", DToC( xData ) ); EXIT
      ENDSWITCH
   NEXT

   RETURN cRet
