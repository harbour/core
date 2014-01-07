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

#include "hbclass.ch"

/*
* An URL:
* http://gian:passwd@www.niccolai.ws/mypages/mysite/page.html?avar=0&avar1=1
* ^--^   ^--^ ^----^ ^-------------^ ^----------------------^ ^------------^
* cProto  UID  PWD      cServer             cPath                 cQuery
*                                    ^------------^ ^-------^
*                                      cDirectory     cFile
*                                                   ^--^ ^--^
*                                                 cFname cExt
*/

CREATE CLASS TUrl

   VAR cAddress  INIT ""
   VAR cProto    INIT ""
   VAR cUserid   INIT ""
   VAR cPassword INIT ""
   VAR cServer   INIT ""
   VAR cPath     INIT ""
   VAR cQuery    INIT ""
   VAR cFile     INIT ""
   VAR nPort     INIT -1

   METHOD New( cUrl )
   METHOD SetAddress( cUrl )
   METHOD BuildAddress()
   METHOD BuildQuery()
   METHOD AddGetForm( xPostData )

   HIDDEN:

   CLASS VAR cREuri   INIT hb_regexComp( "(?:(.*)://)?([^?/]*)(/[^?]*)?\??(.*)" )
   CLASS VAR cREServ  INIT hb_regexComp( "(?:([^:@]*):?([^@:]*)@|)([^:]+):?(.*)" )
   CLASS VAR cREFile  INIT hb_regexComp( "^((?:/.*/)|/)*(.*)$" )

ENDCLASS


METHOD New( cUrl ) CLASS TUrl

   ::SetAddress( cUrl )

   RETURN Self

METHOD SetAddress( cUrl ) CLASS TUrl

   LOCAL aMatch, cServer, cPath

   IF ! HB_ISSTRING( cUrl )
      RETURN .F.
   ENDIF

   ::cAddress := cUrl
   ::cProto := ""
   ::cUserid := ""
   ::cPassword := ""
   ::cServer := ""
   ::cPath := ""
   ::cQuery := ""
   ::cFile := ""
   ::nPort := -1

   IF Empty( cUrl )
      RETURN .T.
   ENDIF

   // TOPLEVEL url parsing
   aMatch := hb_regex( ::cREuri, cUrl )

   // May fail
   IF Empty( aMatch )
      RETURN .F.
   ENDIF

   ::cProto := Lower( aMatch[ 2 ] )
   cServer := aMatch[ 3 ]
   cPath := aMatch[ 4 ]
   ::cQuery := aMatch[ 5 ]

   // server parsing (never fails)
   aMatch := hb_regex( ::cREServ, cServer )
   ::cUserId := aMatch[ 2 ]
   ::cPassword := aMatch[ 3 ]
   ::cServer := aMatch[ 4 ]
   ::nPort := Val( aMatch[ 5 ] )
   IF ::nPort < 1
      ::nPort := -1
   ENDIF

   // Parse path and file (never fails)
   aMatch := hb_regex( ::cREFile, cPath )
   ::cPath := aMatch[ 2 ]
   ::cFile := aMatch[ 3 ]

   RETURN .T.


METHOD BuildAddress() CLASS TUrl

   LOCAL cRet := ""

   IF ::cProto != NIL
      ::cProto := Lower( ::cProto )
   ENDIF

   IF ! Empty( ::cProto ) .AND. ! Empty( ::cServer )
      cRet := ::cProto + "://"
   ENDIF

   IF ! Empty( ::cUserid )
      cRet += ::cUserid
      IF ! Empty( ::cPassword )
         cRet += ":" + ::cPassword
      ENDIF
      cRet += "@"
   ENDIF

   IF ! Empty( ::cServer )
      cRet += ::cServer
      IF ::nPort > 0
         cRet += ":" + hb_ntos( ::nPort )
      ENDIF
   ENDIF

   IF Len( ::cPath ) == 0 .OR. !( Right( ::cPath, 1 ) == "/" )
      ::cPath += "/"
   ENDIF

   cRet += ::cPath + ::cFile
   IF ! Empty( ::cQuery )
      cRet += "?" + ::cQuery
   ENDIF

   IF Len( cRet ) == 0
      cRet := NIL
   ELSE
      ::cAddress := cRet
   ENDIF

   RETURN cRet

METHOD BuildQuery() CLASS TUrl

   LOCAL cLine

   IF Len( ::cPath ) == 0 .OR. !( Right( ::cPath, 1 ) == "/" )
      ::cPath += "/"
   ENDIF

   cLine := ::cPath + ::cFile
   IF ! Empty( ::cQuery )
      cLine += "?" + ::cQuery
   ENDIF

   RETURN cLine

METHOD AddGetForm( xPostData )

   LOCAL cData := ""
   LOCAL item

   DO CASE
   CASE HB_ISHASH( xPostData )
      FOR EACH item IN xPostData
         cData += ;
            tip_URLEncode( AllTrim( hb_CStr( item:__enumKey() ) ) ) + "=" + ;
            tip_URLEncode( AllTrim( hb_CStr( item ) ) )
         IF ! item:__enumIsLast()
            cData += "&"
         ENDIF
      NEXT
   CASE HB_ISARRAY( xPostData )
      FOR EACH item IN xPostData
         cData += ;
            tip_URLEncode( AllTrim( hb_CStr( item:__enumIndex() ) ) ) + "=" + ;
            tip_URLEncode( AllTrim( hb_CStr( item ) ) )
         IF ! item:__enumIsLast()
            cData += "&"
         ENDIF
      NEXT
   CASE HB_ISSTRING( xPostData )
      cData := xPostData
   ENDCASE

   IF Empty( cData )
      RETURN NIL
   ENDIF

   RETURN ::cQuery += iif( Empty( ::cQuery ), "", "&" ) + cData
