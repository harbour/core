/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* 2007-04-11, Hannes Ziegler <hz AT knowlexbase.com>
   Added method :setHeader()
   Added method :attachFile()
   Added method :detachFile()
   Added method :getFileName()
   Added method :isMultiPart()
   Added method :getMultiParts()
*/

#include "hbclass.ch"

CREATE CLASS TipMail

   VAR hHeaders
   // received fields may be more than once.
   VAR aReceived INIT {}

   METHOD New( cBody, oEncoder )    CONSTRUCTOR
   METHOD SetBody( cBody )
   METHOD GetBody()
   METHOD GetRawBody()              INLINE ::cBody
   METHOD SetEncoder( cEncoder )

   /*
   METHOD FWrite( nFile )
   METHOD FRead( nFile )
   METHOD Send( sSocket )
   METHOD Recv( sSocket )
   */
   METHOD FromString( cMail, cBoundary, nPos )
   METHOD ToString()

   METHOD GetFieldPart( cPart )
   METHOD GetFieldOption( cPart, cOption )
   METHOD SetFieldPart( cPart, cValue )
   METHOD SetFieldOption( cPart, cOption, cValue )
   METHOD SetCharset( cCharset ) INLINE ::cCharset := iif( HB_ISSTRING( cCharset ), cCharset, "ISO-8859-1" )

   METHOD GetContentType() INLINE ::GetFieldPart( "Content-Type" )
   METHOD GetCharEncoding() INLINE ::GetFieldOption( "Content-Type", "encoding" )

   METHOD Attach( oSubPart )
   METHOD NextAttachment()
   METHOD CountAttachments()  INLINE Len( ::aAttachments )
   METHOD GetAttachment()
   METHOD ResetAttachment()   INLINE ::nAttachPos := 1

   METHOD MakeBoundary()

   METHOD isMultiPart()
   METHOD getMultiParts( aParts )

   METHOD setHeader( cSubject, cFrom, xTo, xCC, xBCC )
   METHOD attachFile( cFileName )
   METHOD detachFile( cPath )
   METHOD getFileName()

   HIDDEN:

   VAR cBody
   VAR lBodyEncoded INIT .F.
   VAR oEncoder
   VAR aAttachments
   VAR nAttachPos   INIT 1
   VAR cCharset

ENDCLASS

METHOD New( cBody, oEncoder ) CLASS TipMail

   // Set header fileds to non-sensitive
   ::hHeaders := hb_HSetCaseMatch( { => }, .F. )
   ::aAttachments := {}

   IF ValType( oEncoder ) $ "CO"
      ::setEncoder( oEncoder )
   ENDIF

   IF cBody != NIL
      ::setBody( cBody )
   ENDIF

   ::SetCharset()

   RETURN Self

METHOD SetEncoder( cEncoder ) CLASS TipMail

   IF HB_ISSTRING( cEncoder )
      ::oEncoder := tip_GetEncoder( cEncoder )
   ELSE
      ::oEncoder := cEncoder
   ENDIF
   ::hHeaders[ "Content-Transfer-Encoding" ] := ::oEncoder:cName

   RETURN .T.

METHOD SetBody( cBody ) CLASS TipMail

   IF ::oEncoder != NIL
      ::cBody := ::oEncoder:Encode( cBody )
      ::hHeaders[ "Content-Transfer-Encoding" ] := ::oEncoder:cName
      ::lBodyEncoded := .T.  // GD needed to prevent an extra crlf from being appended
   ELSE
      ::cBody := cBody
   ENDIF
   // not needed
   // ::hHeaders[ "Content-Length" ] := hb_ntos( Len( cBody ) )

   RETURN .T.

METHOD GetBody() CLASS TipMail

   IF ::cBody == NIL
      RETURN NIL
   ELSEIF ::oEncoder != NIL
      RETURN ::oEncoder:Decode( ::cBody )
   ENDIF

   RETURN ::cBody

METHOD GetFieldPart( cPart ) CLASS TipMail

   LOCAL nPos, cEnc

   nPos := hb_HPos( ::hHeaders, cPart )
   IF nPos == 0
      RETURN ""
   ELSE
      cEnc := hb_HValueAt( ::hHeaders, nPos )
      nPos := At( ";", cEnc )
      IF nPos != 0
         cEnc := SubStr( cEnc, 1, nPos - 1 )
      ENDIF
   ENDIF

   RETURN cEnc

METHOD GetFieldOption( cPart, cOption ) CLASS TipMail

   LOCAL nPos, aMatch
   LOCAL cEnc

   nPos := hb_HPos( ::hHeaders, cPart )
   IF nPos == 0
      RETURN ""
   ELSE
      cEnc := hb_HValueAt( ::hHeaders, nPos )
      // Case insensitive check
      aMatch := hb_regex( ";\s*" + cOption + "\s*=\s*([^;]*)", cEnc, .F. )
      IF ! Empty( aMatch )
         cEnc := aMatch[ 2 ]
      ELSE
         RETURN ""
      ENDIF
   ENDIF

   RETURN cEnc

METHOD SetFieldPart( cPart, cValue ) CLASS TipMail

   LOCAL nPos, cEnc

   nPos := hb_HPos( ::hHeaders, cPart )
   IF nPos == 0
      ::hHeaders[ cPart ] := cValue
   ELSE
      cEnc := hb_HValueAt( ::hHeaders, nPos )
      nPos := At( ";", cEnc )
      IF nPos == 0
         ::hHeaders[ cPart ] := cValue
      ELSE
         ::hHeaders[ cPart ] := cValue + SubStr( cEnc, nPos )
      ENDIF
   ENDIF

   RETURN .T.

METHOD SetFieldOption( cPart, cOption, cValue ) CLASS TipMail

   LOCAL nPos, aMatch
   LOCAL cEnc

   nPos := hb_HPos( ::hHeaders, cPart )
   IF nPos == 0
      RETURN .F.
   ELSE
      cEnc := hb_HValueAt( ::hHeaders, nPos )
      aMatch := hb_regex( "(.*?;\s*)" + cOption + "\s*=[^;]*(.*)?", cEnc, .F. )
      IF Empty( aMatch )
         ::hHeaders[ cPart ] := cEnc += "; " + cOption + '="' + cValue + '"'
      ELSE
         ::hHeaders[ cPart ] := aMatch[ 2 ] + cOption + '="' + ;
            cValue + '"' + aMatch[ 3 ]
      ENDIF
   ENDIF

   RETURN .T.

METHOD Attach( oSubPart ) CLASS TipMail

   IF HB_ISOBJECT( oSubPart ) .AND. oSubPart:ClassName == "TIPMAIL"
      // reset wrong content-type
      IF At( "multipart/", Lower( ::GetFieldPart( "Content-Type" ) ) ) == 0
         ::hHeaders[ "Content-Type" ] := "multipart/mixed"
      ENDIF

      AAdd( ::aAttachments, oSubPart )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD NextAttachment() CLASS TipMail

   IF ::nAttachPos > Len( ::aAttachments )
      RETURN NIL
   ELSE
      ::nAttachPos++
   ENDIF

   RETURN ::aAttachments[ ::nAttachPos - 1 ]

METHOD GetAttachment() CLASS TipMail

   IF ::nAttachPos > Len( ::aAttachments )
      RETURN NIL
   ENDIF

   RETURN ::aAttachments[ ::nAttachPos ]

METHOD ToString() CLASS TipMail

   LOCAL cBoundary, cElem, i
   LOCAL cRet := ""

   // this is a multipart message; we need a boundary
   IF Len( ::aAttachments ) > 0
      ::hHeaders[ "Mime-Version" ] := "1.0"
   ENDIF

   IF Len( ::aAttachments ) > 0
      // reset failing content type
      IF At( "multipart/", Lower( ::GetFieldPart( "Content-Type" ) ) ) == 0
         ::hHeaders[ "Content-Type" ] := "multipart/mixed"
      ENDIF

      // have we got it already?
      cBoundary := ::GetFieldOption( "Content-Type", "Boundary" )
      IF Empty( cBoundary )
         cBoundary := ::MakeBoundary()
         IF ! ::SetFieldOption( "Content-Type", "Boundary", cBoundary )
            ::hHeaders[ "Content-Type" ] := ;
               'multipart/mixed; boundary="' + cBoundary + '"'
         ENDIF
      ENDIF
   ENDIF

   // Begin output the fields
   // Presenting them in a "well-known" order
   IF "Return-Path" $ ::hHeaders
      cRet += "Return-Path: " + ::hHeaders[ "Return-Path" ] + e"\r\n"
   ENDIF
   IF "Delivered-To" $ ::hHeaders
      cRet += "Delivered-To: " + ::hHeaders[ "Delivered-To" ] + e"\r\n"
   ENDIF
   FOR EACH cElem IN ::aReceived
      cRet += "Received: " + cElem + e"\r\n"
   NEXT
   IF "Date" $ ::hHeaders
      cRet += "Date: " + ::hHeaders[ "Date" ] + e"\r\n"
   ENDIF
   IF "From" $ ::hHeaders
      cRet += "From: " + ::hHeaders[ "From" ] + e"\r\n"
   ENDIF
   IF "To" $ ::hHeaders
      cRet += "To: " + ::hHeaders[ "To" ] + e"\r\n"
   ENDIF
   IF "Subject" $ ::hHeaders
      cRet += "Subject: " + ::hHeaders[ "Subject" ] + e"\r\n"
   ENDIF
   IF Len( ::aAttachments ) > 0
      cRet += "Mime-Version:" + ::hHeaders[ "Mime-Version" ] + e"\r\n"
   ENDIF

   FOR i := 1 TO Len( ::hHeaders )
      cElem := Lower( hb_HKeyAt( ::hHeaders, i ) )
      IF !( cElem == "return-path" ) .AND. ;
         !( cElem == "delivered-to" ) .AND. ;
         !( cElem == "date" ) .AND. ;
         !( cElem == "from" ) .AND. ;
         !( cElem == "to" ) .AND. ;
         !( cElem == "subject" ) .AND. ;
         !( cElem == "mime-version" )
         cRet += ;
            hb_HKeyAt( ::hHeaders, i ) + ": " + ;
            hb_HValueAt( ::hHeaders, i ) + e"\r\n"
      ENDIF
   NEXT

   // end of Header
   cRet += e"\r\n"

   // Body
   IF ! Empty( ::cBody )
      IF Empty( ::aAttachments )
         // cRet += ::cBody + iif( lAttachment, "", e"\r\n" )
         cRet += ::cBody + iif( ::lBodyEncoded, "", e"\r\n" )
      ELSE
         // GD - if there are attachements the body of the message has to be treated as an attachment.
         cRet += "--" + cBoundary + e"\r\n"
         cRet += "Content-Type: text/plain; charset=" + ::cCharset + "; format=flowed" + e"\r\n"
         cRet += "Content-Transfer-Encoding: 7bit" + e"\r\n"
         cRet += "Content-Disposition: inline" + e"\r\n" + e"\r\n"
         cRet += ::cBody + e"\r\n"
      ENDIF
   ENDIF

   IF ! Empty( ::aAttachments )
      // Eventually go with mime multipart
      FOR i := 1 TO Len( ::aAttachments )
         cRet += "--" + cBoundary + e"\r\n"
         cRet += ::aAttachments[ i ]:ToString()
      NEXT
      cRet += "--" + cBoundary + "--" + e"\r\n"
   ENDIF

   RETURN cRet

METHOD FromString( cMail, cBoundary, nPos ) CLASS TipMail

   LOCAL oSubSection, cSubBoundary
   LOCAL nLinePos, nSplitPos, nBodyPos
   LOCAL cValue, cLastField

   IF Len( ::aAttachments ) > 0
      ::aAttachments := {}
   ENDIF

   IF Len( ::hHeaders ) > 0
      ::hHeaders := hb_HSetCaseMatch( { => }, .F. )
   ENDIF

   IF Len( ::aReceived ) > 0
      ::aReceived := {}
   ENDIF

   // Part 1: parsing header
   IF ! HB_ISNUMERIC( nPos )
      nPos := 1
   ENDIF

   nLinePos := hb_At( e"\r\n", cMail, nPos )
   DO WHILE nLinePos > nPos
      // going on with last field?
      IF ( SubStr( cMail, nPos, 1 ) == " " .OR. SubStr( cMail, nPos, 1 ) == e"\t" );
               .AND. cLastField != NIL
         cValue := LTrim( SubStr( cMail, nPos, nLinePos - nPos ) )
         IF Lower( cLastField ) == "received"
            ::aReceived[ Len( ::aReceived ) ] += " " + cValue
         ELSE
            ::hHeaders[ cLastField ] += " " + cValue
         ENDIF
      ELSE
         nSplitPos := hb_At( ":", cMail, nPos )
         cLastField := SubStr( cMail, nPos, nSplitPos - nPos )
         cValue := LTrim( SubStr( cMail, nSplitPos + 1, nLinePos - nSplitPos - 1 ) )
         IF Lower( cLastField ) == "received"
            AAdd( ::aReceived, cValue )
         ELSE
            ::hHeaders[ cLastField ] := cValue
         ENDIF
      ENDIF

      nPos := nLinePos + 2
      nLinePos := hb_At( e"\r\n", cMail, nPos )
      // Prevents malformed body to affect us
      IF cBoundary != NIL .AND. hb_At( "--" + cBoundary, cMail, nPos ) == 1
         RETURN 0
      ENDIF
   ENDDO

   // now we may have a body or a multipart message; multipart
   // messages may also have a "fake" body, that is usually not
   // displayed, between their headers and the first multipart
   // boundary.

   IF "Content-Transfer-Encoding" $ ::hHeaders
      ::oEncoder := tip_GetEncoder( ::hHeaders[ "Content-Transfer-Encoding" ] )
   ENDIF

   // se if we have subparts:
   IF At( "multipart/", Lower( ::GetFieldPart( "Content-Type" ) ) ) > 0
      cSubBoundary := ::GetFieldOption( "Content-Type", "Boundary" )
      // strip " on boundary
      IF Left( cSubBoundary, 1 ) == '"'
         cSubBoundary := SubStr( cSubBoundary, 2, Len( cSubBoundary ) - 2 )
      ENDIF
   ENDIF

   nPos := nLinePos + 2
   nBodyPos := nPos
   nLinePos := hb_At( e"\r\n", cMail, nPos )

   DO WHILE nLinePos >= nPos
      // Avoid useless tests for empty lines
      IF nLinePos == nPos
         nPos += 2
         nLinePos := hb_At( e"\r\n", cMail, nPos )
         LOOP
      ENDIF

      // have we met the boundary?
      IF cBoundary != NIL .AND. hb_At( "--" + cBoundary, cMail, nPos ) == nPos
         EXIT
      ENDIF

      // Have we met a section?
      IF cSubBoundary != NIL .AND. ;
            hb_At( "--" + cSubBoundary, cMail, nPos ) == nPos

         // is it the last subsection?
         IF hb_At( "--", cMail, nPos + Len( cSubBoundary ) + 2, nLinePos ) > 0
            EXIT
         ENDIF

         // set our body
         IF nBodyPos > 0
            ::cBody := SubStr( cMail, nBodyPos, nPos - nBodyPos )
            nBodyPos := 0
         ENDIF

         // Add our subsection
         oSubSection := TIPMail():New()
         nPos := oSubSection:FromString( cMail, cSubBoundary, ;
            nLinePos + 2 )

         IF nPos > 0
            AAdd( ::aAttachments, oSubSection )
         ELSE
            RETURN 0
         ENDIF
         // I must stay on the boundary found by the subsection to
         // enter in this part of the loop again.

      ELSE
         // nPos := nLinePos + 2
         /* 04/05/2004 - <maurilio.longo@libero.it>
            Instead of testing every single line of mail until we find next boundary, if there is a boundary we
            jump to it immediatly, this saves thousands of EOL test and makes splitting of a string fast
         */
         nPos := iif( ! Empty( cSubBoundary ), hb_At( "--" + cSubBoundary, cMail, nPos ), iif( ! Empty( cBoundary ), hb_At( "--" + cBoundary, cMail, nPos ), nLinePos + 2 ) )
      ENDIF

      nLinePos := hb_At( e"\r\n", cMail, nPos )
   ENDDO

   // set our body if needed
   IF nBodyPos > 0
      ::cBody := SubStr( cMail, nBodyPos, nPos - nBodyPos )
   ENDIF

   RETURN nPos

METHOD MakeBoundary() CLASS TipMail

   LOCAL cBound := "=_0" + Space( 17 )
   LOCAL i

   FOR i := 4 TO 20
      cBound := Stuff( cBound, i, 1, Chr( hb_Random( 0, 25 ) + Asc( "A" ) ) )
   NEXT

   cBound += "_TIP_" + DToS( Date() ) + ;
      "_" + StrTran( Time(), ":" )

   RETURN cBound

METHOD setHeader( cSubject, cFrom, xTo, xCC, xBCC ) CLASS TipMail

   LOCAL aTo, aCC, aBCC, i, imax
   LOCAL cTo, cCC, cBCC

   IF ! HB_ISSTRING( cSubject )
      cSubject := ""
   ENDIF

   IF ! HB_ISSTRING( cFrom )
      RETURN .F.
   ENDIF

   IF HB_ISSTRING( xTo )
      aTo := { xTo }
   ELSEIF HB_ISARRAY( xTo )
      aTo := xTo
   ENDIF

   IF Empty( aTO )
      RETURN .F.
   ENDIF

   IF HB_ISSTRING( xCC )
      aCC := { xCC }
   ELSEIF HB_ISARRAY( xCC )
      aCC := xCC
   ENDIF

   IF HB_ISSTRING( xBCC )
      aBCC := { xBCC }
   ELSEIF HB_ISARRAY( xBCC )
      aBCC := xBCC
   ENDIF

   IF ! ::setFieldPart( "Subject", WordEncodeQ( cSubject, ::cCharset ) )
      RETURN .F.
   ENDIF

   IF ! ::setFieldPart( "From", LTrim( WordEncodeQ( tip_GetNameEmail( AllTrim( cFrom ) ), ::cCharset ) + " <" + tip_GetRawEmail( AllTrim( cFrom ) ) + ">" ) )
      RETURN .F.
   ENDIF

   cTo := ""
   imax := Len( aTO )
   FOR i := 1 TO imax
      cTo += LTrim( WordEncodeQ( tip_GetNameEmail( AllTrim( aTo[ i ] ) ), ::cCharset ) + " <" + tip_GetRawEmail( AllTrim( aTo[ i ] ) ) + ">" )
      IF i < imax
         cTo += "," + tip_CRLF() + " "
      ENDIF
   NEXT

   IF Empty( cTo )
      RETURN .F.
   ENDIF

   IF ! ::setFieldPart( "To", cTo )
      RETURN .F.
   ENDIF

   IF ! Empty( aCC )
      cCC := ""
      imax := Len( aCC )
      FOR i := 2 TO imax
         cCC += LTrim( WordEncodeQ( tip_GetNameEmail( AllTrim( aCC[ i ] ) ), ::cCharset ) + " <" + tip_GetRawEmail( AllTrim( aCC[ i ] ) ) + ">" )
         IF i < imax
            cCC += "," + tip_CRLF() + " "
         ENDIF
      NEXT

      IF ! Empty( cCC ) .AND. ! ::setFieldPart( "Cc", cCC )
         RETURN .F.
      ENDIF
   ENDIF

   IF ! Empty( aBCC )
      cBCC := ""
      imax := Len( aBCC )
      FOR i := 2 TO imax
         cBCC += LTrim( WordEncodeQ( tip_GetNameEmail( AllTrim( aBCC[ i ] ) ), ::cCharset ) + " <" + tip_GetRawEmail( AllTrim( aBCC[ i ] ) ) + ">" )
         IF i < imax
            cBCC += "," + tip_CRLF() + " "
         ENDIF
      NEXT

      IF ! Empty( cBCC ) .AND. ! ::setFieldPart( "Bcc", cBCC )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

METHOD attachFile( cFileName ) CLASS TipMail

   LOCAL cContent := hb_MemoRead( cFileName )
   LOCAL cMimeType := tip_FileMimeType( cFileName )
   LOCAL cDelim := hb_ps()

   LOCAL oAttach

   IF Empty( cContent )
      RETURN .F.
   ENDIF

   oAttach   := TIPMail():new( cContent, "base64" )
   cFileName := SubStr( cFileName, RAt( cFileName, cDelim ) + 1 )

   oAttach:setFieldPart( "Content-Type", cMimeType )
   oAttach:setFieldOption( "Content-Type", "name", cFileName )

   oAttach:setFieldPart( "Content-Disposition", "attachment" )
   oAttach:setFieldOption( "Content-Disposition", "filename", cFileName )

   RETURN ::attach( oAttach )

METHOD detachFile( cPath ) CLASS TipMail

   LOCAL cContent := ::getBody()
   LOCAL cFileName := ::getFileName()
   LOCAL cDelim := hb_ps()
   LOCAL nFileHandle

   IF Empty( cFileName )
      RETURN .F.
   ENDIF

   IF HB_ISSTRING( cPath )
      cFileName := StrTran( cPath + cDelim + cFileName, cDelim + cDelim, cDelim )
   ENDIF

   nFileHandle := FCreate( cFileName )
   IF FError() != 0
      RETURN .F.
   ENDIF

   FWrite( nFileHandle, cContent )
   FClose( nFileHandle )

   RETURN FError() == 0

METHOD getFileName() CLASS TipMail
   RETURN StrTran( ::getFieldOption( "Content-Type", "name" ), '"' )

METHOD isMultiPart() CLASS TipMail
   RETURN "multipart/" $ Lower( ::GetFieldPart( "Content-Type" ) )

METHOD getMultiParts( aParts ) CLASS TipMail

   LOCAL oSubPart, lReset := .F.

   ::resetAttachment()

   IF ! HB_ISARRAY( aParts )
      aParts := {}
   ENDIF

   DO WHILE ( oSubPart := ::nextAttachment() ) != NIL
      lReset := .T.
      AAdd( aParts, oSubPart )
      IF oSubPart:countAttachments() > 0
         oSubPart:getMultiparts( aParts )
      ENDIF
   ENDDO

   IF lReset
      ::resetAttachment()
   ENDIF

   RETURN aParts

STATIC FUNCTION WordEncodeQ( cData, cCharset )

   LOCAL nPos
   LOCAL c
   LOCAL cString
   LOCAL nLineLen := 0
   LOCAL lToEncode := .F.

   IF Empty( cCharset )
      RETURN cData
   ENDIF

   /* TOFIX: Add support to handle long string. */

   cString := "=?" + cCharset + "?" + "Q" + "?"

   FOR nPos := 1 TO hb_BLen( cData )
      c := hb_BSubStr( cData, nPos, 1 )
      IF hb_BCode( c ) > 126 .OR. ;
         c $ '=?!"#$@[\]^`{|}~_' .OR. ;
         hb_BCode( c ) <= 32
         cString += "=" + hb_NumToHex( hb_BCode( c ), 2 )
         nLineLen += 3
         lToEncode := .T.
      ELSE
         cString += c
         nLineLen += 1
      ENDIF
   NEXT

   RETURN iif( lToEncode, cString + "?=", cData )

FUNCTION tip_GetRawEmail( cAddress )

   LOCAL tmp, tmp1

   IF ( tmp := At( "<", cAddress ) ) > 0
      IF ( tmp1 := hb_At( ">", cAddress, tmp + 1 ) ) > 0
         RETURN SubStr( cAddress, tmp + 1, tmp1 - tmp - 1 )
      ENDIF
   ENDIF

   RETURN cAddress

FUNCTION tip_GetNameEmail( cAddress )

   LOCAL tmp

   IF ( tmp := At( "<", cAddress ) ) > 0
      IF hb_At( ">", cAddress, tmp + 1 ) > 0
         RETURN RTrim( Left( cAddress, tmp - 1 ) )
      ENDIF
   ENDIF

   RETURN cAddress
