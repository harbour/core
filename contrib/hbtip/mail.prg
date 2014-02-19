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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "fileio.ch"

CREATE CLASS TIPMail

   VAR hHeaders
   // received fields may be more than once.
   VAR aReceived INIT {}

   METHOD New( cBody, xEncoder )    CONSTRUCTOR
   METHOD SetBody( cBody )
   METHOD GetBody()
   METHOD GetRawBody()              INLINE ::cBody
   METHOD SetEncoder( xEncoder )

   METHOD FromString( cMail, cBoundary, nPos )
   METHOD HeadersToString()
   METHOD ToString()

   METHOD GetFieldPart( cPart )
   METHOD GetFieldOption( cPart, cOption )
   METHOD SetFieldPart( cPart, cValue )
   METHOD SetFieldOption( cPart, cOption, cValue )
   METHOD SetCharset( cCharset ) INLINE ::cCharset := hb_defaultValue( cCharset, "UTF-8" )

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

   METHOD setHeader( cSubject, cFrom, xTo, xCC )
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

METHOD New( cBody, xEncoder ) CLASS TIPMail

   ::hHeaders := { => }
   hb_HCaseMatch( ::hHeaders, .F. )
   ::aAttachments := {}

   ::setEncoder( xEncoder )
   ::setBody( cBody )
   ::SetCharset()

   RETURN Self

METHOD SetEncoder( xEncoder ) CLASS TIPMail

   IF HB_ISSTRING( xEncoder )
      ::oEncoder := tip_GetEncoder( xEncoder )
   ELSE
      ::oEncoder := xEncoder
   ENDIF

   IF HB_ISOBJECT( ::oEncoder )
      ::hHeaders[ "Content-Transfer-Encoding" ] := ::oEncoder:cName
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD SetBody( cBody ) CLASS TIPMail

   IF HB_ISOBJECT( ::oEncoder )
      ::cBody := ::oEncoder:Encode( cBody )
      ::hHeaders[ "Content-Transfer-Encoding" ] := ::oEncoder:cName
      ::lBodyEncoded := .T.  // needed to prevent an extra CRLF from being appended [GD]
   ELSEIF HB_ISSTRING( cBody ) .OR. cBody == NIL
      ::cBody := cBody
   ENDIF

   RETURN .T.

METHOD GetBody() CLASS TIPMail
   RETURN iif( HB_ISOBJECT( ::oEncoder ), ::oEncoder:Decode( ::cBody ), ::cBody )

METHOD GetFieldPart( cPart ) CLASS TIPMail

   LOCAL nPos
   LOCAL cEnc

   IF hb_HGetRef( ::hHeaders, cPart, @cEnc )
      IF ( nPos := At( ";", cEnc ) ) != 0
         cEnc := Left( cEnc, nPos - 1 )
      ENDIF
      RETURN cEnc
   ENDIF

   RETURN ""

METHOD GetFieldOption( cPart, cOption ) CLASS TIPMail

   LOCAL aMatch
   LOCAL cEnc

   IF hb_HGetRef( ::hHeaders, cPart, @cEnc ) .AND. ;
      ! Empty( aMatch := hb_regex( ";\s*" + cOption + "\s*=\s*([^;]*)", cEnc, .F. /* Case insensitive */ ) )
      RETURN aMatch[ 2 ]
   ENDIF

   RETURN ""

METHOD SetFieldPart( cPart, cValue ) CLASS TIPMail

   LOCAL nPos
   LOCAL cEnc

   IF HB_ISSTRING( cValue ) .AND. ! Empty( cValue )
      IF hb_HGetRef( ::hHeaders, cPart, @cEnc ) .AND. ;
         ( nPos := At( ";", cEnc ) ) > 0
         ::hHeaders[ cPart ] := cValue + SubStr( cEnc, nPos )
      ELSE
         ::hHeaders[ cPart ] := cValue
      ENDIF
   ENDIF

   RETURN .T.

METHOD SetFieldOption( cPart, cOption, cValue ) CLASS TIPMail

   LOCAL aMatch

   IF HB_ISSTRING( cPart ) .AND. cPart $ ::hHeaders .AND. ;
      HB_ISSTRING( cOption ) .AND. ! Empty( cOption )

      aMatch := hb_regex( "(.*?;\s*)" + cOption + "\s*=[^;]*(.*)?", ::hHeaders[ cPart ], .F. )

      IF Empty( aMatch )
         ::hHeaders[ cPart ] += "; " + cOption + "=" + '"' + cValue + '"'
      ELSE
         ::hHeaders[ cPart ] := aMatch[ 2 ] + cOption + "=" + '"' + cValue + '"' + aMatch[ 3 ]
      ENDIF

      RETURN .T.
   ENDIF

   RETURN .F.

METHOD Attach( oSubPart ) CLASS TIPMail

   IF HB_ISOBJECT( oSubPart ) .AND. oSubPart:ClassName() == "TIPMAIL"
      // reset wrong content-type
      IF ! ::isMultiPart()
         ::hHeaders[ "Content-Type" ] := "multipart/mixed"
      ENDIF

      AAdd( ::aAttachments, oSubPart )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD NextAttachment() CLASS TIPMail

   IF ::nAttachPos > Len( ::aAttachments )
      RETURN NIL
   ENDIF

   RETURN ::aAttachments[ ::nAttachPos++ ]

METHOD GetAttachment() CLASS TIPMail

   IF ::nAttachPos > Len( ::aAttachments )
      RETURN NIL
   ENDIF

   RETURN ::aAttachments[ ::nAttachPos ]

METHOD HeadersToString() CLASS TIPMail

   LOCAL cRet := ""
   LOCAL cElem, i

   // Begin output the fields, presenting them in a "well-known" order
   FOR EACH cElem IN { "Return-Path", "Delivered-To" }
      IF cElem $ ::hHeaders
         cRet += cElem + ": " + ::hHeaders[ cElem ] + e"\r\n"
      ENDIF
   NEXT

   FOR EACH cElem IN ::aReceived
      cRet += "Received: " + cElem + e"\r\n"
   NEXT

   FOR EACH cElem IN { "Date", "From", "To", "Subject" }
      IF cElem $ ::hHeaders
         cRet += cElem + ": " + ::hHeaders[ cElem ] + e"\r\n"
      ENDIF
   NEXT

   IF ! Empty( ::aAttachments )
      cRet += "Mime-Version: " + ::hHeaders[ "Mime-Version" ] + e"\r\n"
   ENDIF

   FOR EACH i IN ::hHeaders
      SWITCH Lower( cElem := i:__enumKey() )
      CASE "return-path"
      CASE "delivered-to"
      CASE "date"
      CASE "from"
      CASE "to"
      CASE "subject"
      CASE "mime-version"
         EXIT
      OTHERWISE
         cRet += cElem + ": " + i + e"\r\n"
      ENDSWITCH
   NEXT

   RETURN cRet

METHOD ToString() CLASS TIPMail

   LOCAL cBoundary, i
   LOCAL cRet

   // this is a multipart message; we need a boundary
   IF ! Empty( ::aAttachments )
      ::hHeaders[ "Mime-Version" ] := "1.0"

      // reset failing content type
      IF ! ::isMultiPart()
         ::hHeaders[ "Content-Type" ] := "multipart/mixed"
      ENDIF

      // have we got it already?
      cBoundary := ::GetFieldOption( "Content-Type", "Boundary" )
      IF Empty( cBoundary )
         cBoundary := ::MakeBoundary()
         IF ! ::SetFieldOption( "Content-Type", "Boundary", cBoundary )
            ::hHeaders[ "Content-Type" ] := "multipart/mixed; boundary=" + '"' + cBoundary + '"'
         ENDIF
      ENDIF
   ENDIF

   // Header
   cRet := ::HeadersToString() + e"\r\n"

   // Body
   IF ! Empty( ::cBody )
      IF Empty( ::aAttachments )
         cRet += ::cBody + iif( ::lBodyEncoded, "", e"\r\n" )
      ELSE
         // if there are attachements, the body of the message has to be treated as an attachment. [GD]
         cRet += ;
            "--" + cBoundary + e"\r\n" + ;
            "Content-Disposition: inline" + e"\r\n" + ;
            "Content-Type: text/plain; charset=" + ::cCharset + "; format=flowed" + e"\r\n" + ;
            "Content-Transfer-Encoding: 7bit" + e"\r\n" + ;
            e"\r\n" + ;
            ::cBody + ;
            e"\r\n"
      ENDIF
   ENDIF

   IF ! Empty( ::aAttachments )
      // Eventually go with MIME multipart
      FOR EACH i IN ::aAttachments
         cRet += "--" + cBoundary + e"\r\n" + i:ToString() + e"\r\n"
      NEXT
      cRet += "--" + cBoundary + "--" + e"\r\n"
   ENDIF

   RETURN cRet

METHOD FromString( cMail, cBoundary, nPos ) CLASS TIPMail

   LOCAL oSubSection, cSubBoundary
   LOCAL nLinePos, nSplitPos, nBodyPos
   LOCAL cValue, cLastField

   IF ! HB_ISSTRING( cMail )
      RETURN 0
   ENDIF

   IF Len( ::aAttachments ) > 0
      ::aAttachments := {}
   ENDIF

   IF Len( ::hHeaders ) > 0
      ::hHeaders := { => }
      hb_HCaseMatch( ::hHeaders, .F. )
   ENDIF

   IF Len( ::aReceived ) > 0
      ::aReceived := {}
   ENDIF

   // Part 1: parsing header
   hb_default( @nPos, 1 )

   DO WHILE ( nLinePos := hb_At( e"\r\n", cMail, nPos ) ) > nPos
      // going on with last field?
      IF ( SubStr( cMail, nPos, 1 ) == " " .OR. SubStr( cMail, nPos, 1 ) == e"\t" ) .AND. ;
         cLastField != NIL

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

      // Prevents malformed body to affect us
      IF HB_ISSTRING( cBoundary ) .AND. hb_At( "--" + cBoundary, cMail, nPos ) == 1
         RETURN 0
      ENDIF
   ENDDO

   /* Now we may have a body or a multipart message; multipart
      messages may also have a "fake" body, that is usually not
      displayed, between their headers and the first multipart
      boundary. */

   IF "Content-Transfer-Encoding" $ ::hHeaders
      ::oEncoder := tip_GetEncoder( ::hHeaders[ "Content-Transfer-Encoding" ] )
   ENDIF

   // se if we have subparts:
   IF ::isMultiPart()
      cSubBoundary := ::GetFieldOption( "Content-Type", "Boundary" )
      // strip " on boundary
      IF hb_LeftIs( cSubBoundary, '"' )
         cSubBoundary := SubStr( cSubBoundary, 2, Len( cSubBoundary ) - 2 )
      ENDIF
   ENDIF

   nPos := nLinePos + 2
   nBodyPos := nPos

   DO WHILE ( nLinePos := hb_At( e"\r\n", cMail, nPos ) ) >= nPos
      // Avoid useless tests for empty lines
      IF nLinePos == nPos
         nPos += 2
         LOOP
      ENDIF

      // have we met the boundary?
      IF HB_ISSTRING( cBoundary ) .AND. hb_At( "--" + cBoundary, cMail, nPos ) == nPos
         EXIT
      ENDIF

      // Have we met a section?
      IF HB_ISSTRING( cSubBoundary ) .AND. hb_At( "--" + cSubBoundary, cMail, nPos ) == nPos

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
         /* 2004-05-04 - <maurilio.longo@libero.it>
            Instead of testing every single line of mail until we find next boundary, if there is a boundary we
            jump to it immediatly, this saves thousands of EOL test and makes splitting of a string fast
          */
         nPos := iif( ! Empty( cSubBoundary ), hb_At( "--" + cSubBoundary, cMail, nPos ), iif( ! Empty( cBoundary ), hb_At( "--" + cBoundary, cMail, nPos ), nLinePos + 2 ) )
      ENDIF
   ENDDO

   // set our body if needed
   IF nBodyPos > 0
      ::cBody := SubStr( cMail, nBodyPos, nPos - nBodyPos )
   ENDIF

   RETURN nPos

METHOD MakeBoundary() CLASS TIPMail

   LOCAL cBound := "=_0"
   LOCAL i

   FOR i := 1 TO 17
      cBound += Chr( Asc( "A" ) + hb_Random( 0, 25 ) )
   NEXT

   RETURN cBound + "_TIP_" + DToS( Date() ) + StrTran( Time(), ":" )

METHOD setHeader( cSubject, cFrom, xTo, xCC ) CLASS TIPMail

   LOCAL aTo, aCC
   LOCAL cTo, cCC

   LOCAL cName
   LOCAL cAddr

   LOCAL i

   IF ! HB_ISSTRING( cFrom ) .OR. Empty( cFrom )
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

   ::setFieldPart( "Subject", WordEncodeQ( hb_defaultValue( cSubject, "" ), ::cCharset ) )
   ::setFieldPart( "From", LTrim( WordEncodeQ( tip_GetNameEmail( AllTrim( cFrom ) ), ::cCharset ) + " <" + tip_GetRawEmail( AllTrim( cFrom ) ) + ">" ) )

   cTo := ""
   FOR EACH i IN aTo
      IF ! Empty( i )
         IF ! Empty( cTo )
            cTo += "," + e"\r\n" + " "
         ENDIF
         i := AllTrim( i )
         cName := tip_GetNameEmail( i )
         cAddr := tip_GetRawEmail( i )
         cTo += iif( cName == cAddr, cAddr, LTrim( WordEncodeQ( cName, ::cCharset ) ) + " <" + cAddr + ">" )
      ENDIF
   NEXT

   IF Empty( cTo )
      RETURN .F.
   ENDIF

   ::setFieldPart( "To", cTo )

   IF ! Empty( aCC )
      cCC := ""
      FOR EACH i IN aCC
         IF ! Empty( i )
            IF ! Empty( cCC )
               cCC += "," + e"\r\n" + " "
            ENDIF
            i := AllTrim( i )
            cName := tip_GetNameEmail( i )
            cAddr := tip_GetRawEmail( i )
            cCC += iif( cName == cAddr, cAddr, LTrim( WordEncodeQ( cName, ::cCharset ) ) + " <" + cAddr + ">" )
         ENDIF
      NEXT

      IF ! Empty( cCC )
         ::setFieldPart( "Cc", cCC )
      ENDIF
   ENDIF

   RETURN .T.

METHOD attachFile( cFileName ) CLASS TIPMail

   LOCAL cContent := hb_MemoRead( cFileName )
   LOCAL cMimeType := tip_FileMimeType( cFileName )
   LOCAL cDelim := hb_ps()
   LOCAL oAttach
   LOCAL nAttr

   IF Empty( cContent )
      RETURN .F.
   ENDIF

   oAttach   := TIPMail():new( cContent, "base64" )
   cFileName := SubStr( cFileName, RAt( cFileName, cDelim ) + 1 )

   oAttach:setFieldPart( "Content-Disposition", "attachment" )
   oAttach:setFieldOption( "Content-Disposition", "filename", cFileName )
   oAttach:setFieldPart( "Content-Type", cMimeType )
   oAttach:setFieldOption( "Content-Type", "name", cFileName )
   IF hb_FGetAttr( cFileName, @nAttr ) .AND. nAttr != 0
      oAttach:setFieldOption( "Content-Type", "x-unix-mode", hb_NumToHex( __tip_FAttrToUmask( nAttr ), 4 ) )
   ENDIF

   RETURN ::attach( oAttach )

METHOD detachFile( cPath ) CLASS TIPMail

   LOCAL cContent := ::getBody()
   LOCAL cFileName := ::getFileName()

   IF Empty( cFileName )
      RETURN .F.
   ENDIF

   IF HB_ISSTRING( cPath )
      cFileName := hb_DirSepAdd( cPath ) + cFileName
   ENDIF

   RETURN hb_MemoWrit( cFileName, cContent )

METHOD getFileName() CLASS TIPMail
   RETURN StrTran( ::getFieldOption( "Content-Type", "name" ), '"' )

METHOD isMultiPart() CLASS TIPMail
   RETURN "multipart/" $ Lower( ::GetFieldPart( "Content-Type" ) )

METHOD getMultiParts( aParts ) CLASS TIPMail

   LOCAL oSubPart, lReset := .F.

   ::resetAttachment()

   hb_default( @aParts, {} )

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

   LOCAL c
   LOCAL cString
   LOCAL lToEncode := .F.

   IF ! Empty( cCharset )

      /* TOFIX: Add support to handle long string. */

      cString := ""
      FOR EACH c IN cData  /* FOR EACH on byte stream */
         IF hb_BCode( c ) > 126 .OR. ;
            c $ '=?!"#$@[\]^`{|}~_' .OR. ;
            hb_BCode( c ) <= 32
            cString += "=" + hb_NumToHex( hb_BCode( c ), 2 )
            lToEncode := .T.
         ELSE
            cString += c
         ENDIF
      NEXT
   ENDIF

   RETURN iif( lToEncode, "=?" + cCharset + "?Q?" + cString + "?=", cData )

FUNCTION tip_GetRawEmail( cAddress )

   LOCAL tmp, tmp1

   IF ( tmp := At( "<", cAddress ) ) > 0 .AND. ;
      ( tmp1 := hb_At( ">", cAddress, tmp + 1 ) ) > 0
      RETURN AllTrim( SubStr( cAddress, tmp + 1, tmp1 - tmp - 1 ) )
   ENDIF

   RETURN cAddress

FUNCTION tip_GetNameEmail( cAddress )

   LOCAL tmp

   IF ( tmp := At( "<", cAddress ) ) > 0 .AND. ;
      hb_At( ">", cAddress, tmp + 1 ) > 0
      RETURN RTrim( Left( cAddress, tmp - 1 ) )
   ENDIF

   RETURN cAddress

FUNCTION __tip_FAttrToUmask( nAttr )
   RETURN hb_bitOr( ;
      Min( hb_bitAnd( nAttr, HB_FA_SUID ), 1 ) * 0x4000, ;
      Min( hb_bitAnd( nAttr, HB_FA_SGID ), 1 ) * 0x2000, ;
      Min( hb_bitAnd( nAttr, HB_FA_SVTX ), 1 ) * 0x1000, ;
      Min( hb_bitAnd( nAttr, HB_FA_RUSR ), 1 ) * 0x0400, ;
      Min( hb_bitAnd( nAttr, HB_FA_WUSR ), 1 ) * 0x0200, ;
      Min( hb_bitAnd( nAttr, HB_FA_XUSR ), 1 ) * 0x0100, ;
      Min( hb_bitAnd( nAttr, HB_FA_RGRP ), 1 ) * 0x0040, ;
      Min( hb_bitAnd( nAttr, HB_FA_WGRP ), 1 ) * 0x0020, ;
      Min( hb_bitAnd( nAttr, HB_FA_XGRP ), 1 ) * 0x0010, ;
      Min( hb_bitAnd( nAttr, HB_FA_ROTH ), 1 ) * 0x0004, ;
      Min( hb_bitAnd( nAttr, HB_FA_WOTH ), 1 ) * 0x0002, ;
      Min( hb_bitAnd( nAttr, HB_FA_XOTH ), 1 ) * 0x0001 )
