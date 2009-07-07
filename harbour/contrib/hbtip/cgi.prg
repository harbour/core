/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TipCgi Class oriented cgi protocol
 *
 * Copyright 2006 Lorenzo Fiorini <lorenzo_fiorini@teamwork.it>
 *
 * code from:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * www - http://www.harbour-project.org
 *
 *    CGI Session Manager Class
 *
 * Copyright 2003-2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://www.xharbour.org
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

#include "hbclass.ch"
#include "tip.ch"
#include "common.ch"
#include "fileio.ch"

#define CGI_IN  0
#define CGI_OUT 1
#define _CRLF chr(13)+chr(10)
#define _BR "<br />"

CLASS TIpCgi

   DATA HTTP_RAW_POST_DATA

   DATA cCgiHeader
   DATA cHtmlPage
   DATA hGets        INIT {=>}
   DATA hPosts       INIT {=>}
   DATA hCookies     INIT {=>}
   DATA hSession     INIT {=>}
   DATA bSavedErrHandler
   DATA cSessionSavePath
   DATA cSID
   DATA cDumpSavePath
   DATA lDumpHtml    INIT FALSE

   METHOD New()
   METHOD Header( hOptions )
   METHOD Redirect( cUrl )
   METHOD Print( cString )
   METHOD Flush()
   METHOD ErrHandler()

   METHOD StartHtml( hOptions )
   METHOD EndHtml()
   METHOD StartFrameSet( hOptions )
   METHOD EndFrameSet()
   METHOD SaveHtmlPage( cFile )

   METHOD StartSession()
   METHOD DestroySession()
   METHOD CreateSID( cCRCKey ) INLINE ::cSID := TIP_GenerateSID( cCrcKey )
   METHOD CheckCrcSID( cSID, cCRCKey ) INLINE TIP_CheckSID( cSID, cCRCKey )
   METHOD SessionEncode()
   METHOD SessionDecode( cData )

ENDCLASS

METHOD New() CLASS TIpCgi

   LOCAL aTemp
   LOCAL aVar
   LOCAL lPost
   LOCAL nCount
   LOCAL nLen
   LOCAL nRead
   LOCAL cTemp

   ::bSavedErrHandler := ErrorBlock( { |e| ::ErrHandler( e ) } )

   ::cCgiHeader := ""
   ::cHtmlPage := ""

   lPost := ( "POST" $ Upper( getenv( "REQUEST_METHOD" ) ) )
   if lPost
      nLen := val( getenv( "CONTENT_LENGTH" ) )
      cTemp := space( nLen )
      if ( ( nRead := FRead( CGI_IN, @cTemp, nLen, 0 ) ) != nLen )
         ::ErrHandler( "post error read " + str( nRead ) + " instead of " + str( nLen ) )
      else
         ::HTTP_RAW_POST_DATA := cTemp
         aTemp := HB_ATOKENS( cTemp, "&" )
         nLen := Len( aTemp )
         if nLen > 0
            for nCount := 1 TO nLen
               aVar := HB_ATOKENS( aTemp[ nCount ], "=" )
               if Len( aVar ) == 2
                  ::hPosts[ alltrim( TipEncoderUrl_Decode( aVar[ 1 ] ) ) ] := TipEncoderUrl_Decode( aVar[ 2 ] )
               endif
            next
         endif
      endif
   else
      cTemp := getenv( "QUERY_STRING" )
      if !empty( cTemp )
         aTemp := HB_ATOKENS( cTemp, "&" )
         nLen := Len( aTemp )
         if nLen > 0
            for nCount := 1 TO nLen
               aVar := HB_ATOKENS( aTemp[ nCount ], "=" )
               if Len( aVar ) == 2
                  ::hGets[ alltrim( TipEncoderUrl_Decode( aVar[ 1 ] ) ) ] := TipEncoderUrl_Decode( aVar[ 2 ] )
               endif
            next
         endif
      endif
   endif

   cTemp := getenv( "HTTP_COOKIE" )
   if !empty( cTemp )
      aTemp := HB_ATOKENS( cTemp, ";" )
      nLen := Len( aTemp )
      if nLen > 0
         for nCount := 1 TO nLen
            aVar := HB_ATOKENS( aTemp[ nCount ], "=" )
            if Len( aVar ) == 2
               ::hCookies[ alltrim( TipEncoderUrl_Decode( aVar[ 1 ] ) ) ] := TipEncoderUrl_Decode( aVar[ 2 ] )
            endif
         next
      endif
   endif

   RETURN Self

METHOD Header( cValue ) CLASS TIpCgi

   if empty( cValue )
      ::cCgiHeader += "Content-Type: text/html" + _CRLF
   else
      ::cCgiHeader += cValue + _CRLF
   endif

   RETURN Self

METHOD Redirect( cUrl ) CLASS TIpCgi

   ::cCgiHeader += "Location: " + cUrl + _CRLF

   RETURN Self

METHOD Print( cString ) CLASS TIpCgi

   ::cHtmlPage += cString + _CRLF

   RETURN Self

METHOD Flush() CLASS TIpCgi

   LOCAL nLen
   LOCAL cStream
   LOCAL lRet

   LOCAL nH
   LOCAL cFile
   LOCAL nFileSize

   LOCAL cSID := ::cSID
   LOCAL cSession

   hb_hEval( ::hCookies, { |k,v| ::cCgiHeader += "Set-Cookie: " + k + "=" + v + ";" + _CRLF } )

   cStream := ::cCgiHeader + _CRLF + ::cHtmlPage + _CRLF

   nLen := len( cStream )

   lRet := ( Fwrite( CGI_OUT, cStream, nLen ) == nLen )

   if ::lDumpHtml
      if empty( ::cDumpSavePath )
         ::cDumpSavePath := "/tmp/"
      endif
      if ( nH := FCreate( ::cDumpSavePath + "dump.html", FC_NORMAL ) ) != -1
         Fwrite( nH, ::cHtmlPage, len( ::cHtmlPage ) )
      endif
      FClose( nH )
   endif

   ::cCgiHeader := ""
   ::cHtmlPage := ""

   if !empty( cSID )

     cFile := ::cSessionSavePath + "SESSIONID_" + cSID

     cSession := ::SessionEncode()

     nFileSize := len( cSession )

     if ( nH := FCreate( cFile, FC_NORMAL ) ) != -1
        if ( FWrite( nH, @cSession,  nFileSize ) ) != nFileSize
           ::Print( "ERROR: On writing session file : " + cFile + ", File error : " + hb_cStr( FError() ) )
        endif
        FClose( nH )
     else
        ::Print( "ERROR: On writing session file : " + cFile + ", File error : " + hb_cStr( FError() ) )
     endif

   endif

   RETURN lRet

METHOD DestroySession( cID ) CLASS TIpCgi

   LOCAL cFile
   LOCAL cSID := ::cSID
   LOCAL lRet

   if !empty( cID )
      cSID := cID
   endif

   if !empty( cSID )

      ::hSession := {=>}

      cFile := ::cSessionSavePath + "SESSIONID_" + cSID

      if !( lRet := ( FErase( cFile ) == 0 ) )
         ::Print( "ERROR: On deleting session file : " + cFile + ", File error : " + hb_cStr( FError() ) )
      else
        ::hCookies[ "SESSIONID" ] := cSID + "; expires= " + TIP_DateToGMT( DATE() - 1 )
        ::CreateSID()
        cSID := ::cSID
        ::hCookies[ "SESSIONID" ] := cSID
      endif

   endif

RETURN lRet

METHOD ErrHandler( xError ) CLASS TIpCgi

   LOCAL nCalls

   ::Print( '<table border="1">' )

   ::Print( '<tr><td>SCRIPT NAME:</td><td>' + getenv( 'SCRIPT_NAME' ) + '</td></tr>' )

   if ISOBJECT( xError )
      ::Print( '<tr><td>CRITICAL ERROR:</td><td>' + xError:Description + '</td></tr>' )
      ::Print( '<tr><td>OPERATION:</td><td>' + xError:Operation + '</td></tr>' )
      ::Print( '<tr><td>OS ERROR:</td><td>' + alltrim( str( xError:OsCode ) ) + ' IN ' + xError:SubSystem + '/' + alltrim( str( xError:SubCode ) ) + '</td></tr>' )
      ::Print( '<tr><td>FILENAME:</td><td>' + right( xError:FileName, 40 ) + '</td></tr>' )
   ELSEIF ISCHARACTER( xError )
      ::Print( '<tr><td>ERROR MESSAGE:</td><td>' + xError + '</td></tr>' )
   endif

   for nCalls := 2 to 6
      if !empty( procname( nCalls ) )
         ::Print( '<tr><td>PROC/LINE:</td><td>' + procname( nCalls ) + "/" + alltrim( str( procline( nCalls ) ) ) + '</td></tr>' )
      endif
   next

   ::Print( '</table>' )

   ::Flush()

   RETURN NIL

METHOD StartHtml( hOptions ) CLASS TIpCgi

   ::cHtmlPage += '<?xml version="1.0"' + HtmlOption( hOptions, 'encoding', ' ' ) + '?>' + _CRLF + ;
                  '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' + _CRLF + ;
                  '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' + _CRLF + ;
                  '<html xmlns="http://www.w3.org/1999/xhtml">' + ;
                  '<head>' + ;
                  HtmlTag( hOptions, 'title', 'title' ) + ;
                  HtmlScript( hOptions ) + ;
                  HtmlStyle( hOptions ) + ;
                  '</head>' + ;
                  '<body ' + ;
                     HtmlAllOption( hOptions ) + ;
                  '>'

   RETURN Self

METHOD EndHtml() CLASS TIpCgi

   ::cHtmlPage += '</body></html>'

   RETURN Self

METHOD StartFrameSet( hOptions ) CLASS TIpCgi

   ::cHtmlPage += '<?xml version="1.0"?>' + _CRLF + ;
                  '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' + _CRLF + ;
                  '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' + _CRLF + ;
                  '<html xmlns="http://www.w3.org/1999/xhtml">' + ;
                  '<head>' + ;
                  HtmlTag( hOptions, 'title', 'title' ) + ;
                  HtmlScript( hOptions ) + ;
                  HtmlStyle( hOptions ) + ;
                  '</head>' + ;
                  '<frameset ' + ;
                     HtmlValue( hOptions, 'frame' ) + ;
                  '>'

   RETURN Self

METHOD EndFrameSet( hOptions ) CLASS TIpCgi

   ::cHtmlPage += '</frameset><noframes>' + ;
                     HtmlValue( hOptions, 'frame' ) + ;
                  '</noframes></html>'

   RETURN Self

METHOD SaveHtmlPage( cFile ) CLASS TIpCgi

   LOCAL nFile
   LOCAL lSuccess
   LOCAL nLen
   LOCAL cStream

   cStream := ::cHtmlPage + _CRLF

   nLen := len( cStream )

   nFile := FCreate( cFile )

   if nFile != 0
      lSuccess := ( FWrite( nFile, cStream, nLen ) == nLen )
      FClose( nFile )
   else
      lSuccess := .F.
   endif

   RETURN lSuccess

METHOD StartSession( cSID ) CLASS TIpCgi

   LOCAL nH
   LOCAL cFile
   LOCAL nFileSize
   LOCAL cBuffer

   if empty( cSID )

      if ( nH := hb_HPos( ::hGets, "SESSIONID" ) ) != 0
         cSID := hb_HValueAt( ::hGets, nH )
      ELSEIF ( nH := hb_HPos( ::hPosts, "SESSIONID" ) ) != 0
         cSID := hb_HValueAt( ::hPosts, nH )
      ELSEIF ( nH := hb_HPos( ::hCookies, "SESSIONID" ) ) != 0
         cSID := hb_HValueAt( ::hCookies, nH )
      endif

   endif

   if empty( ::cSessionSavePath )
      ::cSessionSavePath := "/tmp/"
   endif

   if !empty( cSID )

      ::cSID := cSID

      cFile := ::cSessionSavePath + "SESSIONID_" + cSID

      if hb_FileExists( cFile )
         if ( nH := FOpen( cFile, FO_READ ) ) != -1
            nFileSize := FSeek( nH, 0, FS_END )
            FSeek( nH, 0, FS_SET )
            cBuffer := Space( nFileSize )
            if ( FRead( nH, @cBuffer,  nFileSize ) ) != nFileSize
               ::ErrHandler( "ERROR: On reading session file : " + cFile + ", File error : " + hb_cStr( FError() ) )
            else
               ::SessionDecode( cBuffer )
            endif
            FClose( nH )
         endif
      else
         ::ErrHandler( "ERROR: On opening session file : " + cFile + ", file not exist." )
      endif

   else

      ::CreateSID()
      ::hSession := {=>}

   endif

   ::hCookies[ "SESSIONID" ] := ::cSID

   RETURN Self

METHOD SessionEncode() CLASS TIpCgi

   RETURN HB_Serialize( ::hSession )

METHOD SessionDecode( cData ) CLASS TIpCgi

   ::hSession := HB_Deserialize( cData )

   RETURN Valtype( ::hSession ) == "H"

STATIC FUNCTION HtmlTag( xVal, cKey, cDefault )

   LOCAL cVal := ""

   DEFAULT cDefault TO ""

   if !empty( xVal ) .AND. !empty( cKey )
      if hb_hHasKey( xVal, cKey )
         cVal := hb_hGet( xVal, cKey )
         hb_hDel( xVal, cKey )
      endif
   endif

   if cVal == ""
      cVal := cDefault
   endif

   if !( cVal == "" )
      cVal := "<" + cKey + ">" + cVal + "</" + cKey + ">"
   endif

   RETURN cVal

STATIC FUNCTION HtmlAllTag( hTags, cSep )

   LOCAL cVal := ""

   DEFAULT cSep TO " "

   hb_hEval( hTags, { |k| cVal += HtmlTag( hTags, k ) + cSep } )

   RETURN cVal

STATIC FUNCTION HtmlOption( xVal, cKey, cPre, cPost, lScan )

   LOCAL cVal := ""

   if !empty( xVal )
      if empty( cKey )
         cVal := xVal
      ELSEIF hb_hHasKey( xVal, cKey )
         cVal := hb_hGet( xVal, cKey )
         if empty( lScan )
            hb_hDel( xVal, cKey )
         endif
         cVal := cKey + '="' + cVal + '"'
         if cPre != NIL
            cVal := cPre + cVal
         endif
         if cPost != NIL
            cVal := cVal + cPost
         endif
      endif
   endif

   RETURN cVal

STATIC FUNCTION HtmlAllOption( hOptions, cSep )

   LOCAL cVal := ""

   DEFAULT cSep TO " "

   if !empty( hOptions )
      hb_hEval( hOptions, { |k| cVal += HtmlOption( hOptions, k,,, .T. ) + cSep } )
   endif

   RETURN cVal

STATIC FUNCTION HtmlValue( xVal, cKey, cDefault )

   LOCAL cVal := ""

   DEFAULT cDefault TO ""

   if !empty( xVal ) .AND. !empty( cKey )
      if hb_hHasKey( xVal, cKey )
         cVal := hb_hGet( xVal, cKey )
         hb_hDel( xVal, cKey )
      endif
   endif

   if cVal == ""
      cVal := cDefault
   endif

   RETURN cVal

STATIC FUNCTION HtmlAllValue( hValues, cSep )

   LOCAL cVal := ""

   DEFAULT cSep TO " "

   if !empty( hValues )
      hb_hEval( hValues, { |k| cVal += HtmlValue( hValues, k ) + cSep } )
   endif

   RETURN cVal

STATIC FUNCTION HtmlScript( xVal, cKey )

   LOCAL cVal := ""
   LOCAL nPos
   LOCAL cTmp

   DEFAULT cKey TO "script"

   if !empty( xVal )
      if ( nPos := hb_HPos( xVal, cKey ) ) != 0
         cVal := hb_HValueAt( xVal, nPos )
         if valtype( cVal ) == "H"
            if ( nPos := hb_HPos( cVal, "src" ) ) != 0
               cVal := hb_HValueAt( cVal, nPos )
               if ISCHARACTER( cVal )
                  cVal := { cVal }
               endif
               if ISARRAY( cVal )
                  cTmp := ""
                  ascan( cVal, { |cFile| cTmp += '<script src="' + cFile + '" type="text/javascript">' + _CRLF } )
                  cVal := cTmp
               endif
            endif
            if ( nPos := hb_HPos( cVal, "var" ) ) != 0
               cVal := hb_HValueAt( cVal, nPos )
               if ISCHARACTER( cVal )
                  cVal := { cVal }
               endif
               if ISARRAY( cVal )
                  cTmp := ""
                  ascan( cVal, { |cVar| cTmp += cVar } )
                  cVal := '<script type="text/javascript">' + _CRLF + '<!--' + _CRLF + cTmp + _CRLF + '-->' + _CRLF + '</script>' + _CRLF
               endif
            endif
         endif
         hb_hDel( xVal, cKey )
      endif
   endif

   RETURN cVal

STATIC FUNCTION HtmlStyle( xVal, cKey )

   LOCAL cVal := ""
   LOCAL nPos
   LOCAL cTmp

   DEFAULT cKey TO "style"

   if !empty( xVal )
      if ( nPos := hb_HPos( xVal, cKey ) ) != 0
         cVal := hb_HValueAt( xVal, nPos )
         if valtype( cVal ) == "H"
            if ( nPos := hb_HPos( cVal, "src" ) ) != 0
               cVal := hb_HValueAt( cVal, nPos )
               if ISCHARACTER( cVal )
                  cVal := { cVal }
               endif
               if ISARRAY( cVal )
                  cTmp := ""
                  ascan( cVal, { |cFile| cTmp += '<link rel="StyleSheet" href="' + cFile + '" type="text/css" />' + _CRLF } )
                  cVal := cTmp
               endif
            endif
            if ( nPos := hb_HPos( cVal, "var" ) ) != 0
               cVal := hb_HValueAt( cVal, nPos )
               if ISCHARACTER( cVal )
                  cVal := { cVal }
               endif
               if ISARRAY( cVal )
                  cTmp := ""
                  ascan( cVal, { |cVar| cTmp += cVar } )
                  cVal := '<style type="text/css">' + _CRLF + '<!--' + _CRLF + cTmp + _CRLF + '-->' + _CRLF + '</style>' + _CRLF
               endif
            endif
         endif
         hb_hDel( xVal, cKey )
      endif
   endif

   RETURN cVal
