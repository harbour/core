/*
 * Harbour Project source code:
 * Cgi Class
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "cgi.ch"

CREATE CLASS TCgi FROM THtml

   VAR nH
   VAR Server_Software
   VAR Server_Name
   VAR Gateway_Interface
   VAR Server_Protocol
   VAR Server_Port
   VAR Request_Method
   VAR Http_Accept
   VAR Http_User_agent
   VAR Http_Referer
   VAR Path_Info
   VAR Path_Translated
   VAR Script_Name
   VAR Query_String
   VAR Remote_Host
   VAR Remote_Addr
   VAR ipAddress
   VAR Remote_User
   VAR Auth_Type
   VAR Auth_User
   VAR Auth_Pass
   VAR Content_Type
   VAR Content_Length
   VAR Annotation_Server

   VAR aQueryFields INIT {}

   METHOD New( cInBuffer )

   METHOD Field( cQueryName )

   METHOD ToObject()

ENDCLASS

METHOD New( cInBuffer ) CLASS TCgi

   LOCAL i
   LOCAL aTemp
   LOCAL aVar

   ::nH := HtmlPageHandle()

   ::Server_Software   := GetEnv( "SERVER_SOFTWARE" )
   ::Server_Name       := GetEnv( "SERVER_NAME" )
   ::Gateway_Interface := GetEnv( "GATEWAY_INTERFACE" )
   ::Server_Protocol   := GetEnv( "SERVER_PROTOCOL" )
   ::Server_Port       := GetEnv( "SERVER_PORT" )
   ::Request_Method    := GetEnv( "REQUEST_METHOD" )
   ::Http_Accept       := GetEnv( "HTTP_ACCEPT" )
   ::Http_User_agent   := GetEnv( "HTTP_USER_AGENT" )
   ::Http_Referer      := GetEnv( "HTTP_REFERER" )
   ::Path_Info         := GetEnv( "PATH_INFO" )
   ::Path_Translated   := GetEnv( "PATH_TRANSLATED" )
   ::Script_Name       := GetEnv( "SCRIPT_NAME" )
   ::Query_String      := GetEnv( "QUERY_STRING" )
   ::Remote_Host       := GetEnv( "REMOTE_HOST" )
   ::Remote_Addr       := GetEnv( "REMOTE_ADDR" )
   ::ipAddress         := GetEnv( "REMOTE_ADDR" )
   ::Remote_User       := GetEnv( "REMOTE_USER" )
   ::Auth_Type         := GetEnv( "AUTH_TYPE" )
   ::Auth_User         := GetEnv( "AUTH_USER" )
   ::Auth_Pass         := GetEnv( "AUTH_PASS" )
   ::Content_Type      := GetEnv( "CONTENT_TYPE" )
   ::Content_Length    := GetEnv( "CONTENT_LENGTH" )
   ::Annotation_Server := GetEnv( "ANNOTATION_SERVER" )

   IF cInBuffer != NIL
      ::Query_String := RTrim( cInBuffer )
   ELSE
      IF "POST" $ Upper( ::Request_Method )
         ::Query_String := RTrim( FReadStr( STD_IN, Val( ::CONTENT_LENGTH ) ) )
      ENDIF
   ENDIF

   IF ! Empty( ::Query_String )

      ::aQueryFields := {}

      aTemp := hb_ATokens( ::Query_String, "&" )           // separate fields

      FOR i := 1 TO Len( aTemp )
         aVar := hb_ATokens( aTemp[ i ], "=" )
         IF Len( aVar ) == 2
            AAdd( ::aQueryFields, { aVar[ 1 ], HtmlDecodeUrl( aVar[ 2 ] ) } )
         ENDIF
      NEXT

   ENDIF

   RETURN ::ToObject()

/****
*
*        TCGI():ToObject()
*
*        Creates instance variables out of CGI FORM return values
*        or URL encoded content.
*
*        It subclasses the TCgi class to a *new* class
*/

METHOD ToObject() CLASS TCgi

   LOCAL i
   LOCAL nScope := 1
   LOCAL aDb
   LOCAL oNew

   STATIC s_n := 0

   // --> create new oObject class from this one...
   aDb := HBClass():New( "NewCgi" + StrZero( ++s_n, 3 ), { "TCgi" } )

   FOR i := 1 TO Len( ::aQueryFields )

      IF ::aQueryFields[ i, 2 ] == NIL .OR. Empty( ::aQueryFields[ i, 2 ] )
         ::aQueryFields[ i, 2 ] := ""
      ENDIF

      adb:AddData( ::aQueryFields[ i, 1 ], ::aQueryFields[ i, 2 ], , nScope )
   NEXT

   adb:Create()
   oNew := adb:Instance()
   oNew:aQueryFields      := ::aQueryFields
   oNew:Server_Software   := ::Server_Software
   oNew:Server_Name       := ::Server_Name
   oNew:Gateway_Interface := ::Gateway_Interface
   oNew:Server_Protocol   := ::Server_Protocol
   oNew:Server_Port       := ::Server_Port
   oNew:Request_Method    := ::Request_Method
   oNew:Http_Accept       := ::Http_Accept
   oNew:Http_User_agent   := ::Http_User_agent
   oNew:Http_Referer      := ::Http_Referer
   oNew:Path_Info         := ::Path_Info
   oNew:Path_Translated   := ::Path_Translated
   oNew:Script_Name       := ::Script_Name
   oNew:Query_String      := ::Query_String
   oNew:Remote_Host       := ::Remote_Host
   oNew:Remote_Addr       := ::Remote_Addr
   oNew:ipAddress         := ::ipAddress
   oNew:Remote_User       := ::Remote_User
   oNew:Auth_Type         := ::Auth_Type
   oNew:Content_Type      := ::Content_Type
   oNew:Content_Length    := ::Content_Length
   oNew:Annotation_Server := ::Annotation_Server
   oNew:nH                := iif( HtmlPageHandle() == NIL, STD_OUT, HtmlPageHandle() )

   RETURN oNew

METHOD Field( cQueryName ) CLASS TCgi

   LOCAL cRet := ""
   LOCAL nRet

   __defaultNIL( @cQueryName, "" )

   nRet := AScan( ::aQueryFields, {| x | Upper( x[ 1 ] ) == Upper( cQueryName ) } )

   IF nRet > 0
      cRet := ::aQueryFields[ nRet, 2 ]
   ENDIF

   RETURN cRet

FUNCTION ParseString( cString, cDelim, nRet )

   LOCAL cBuf
   LOCAL aElem
   LOCAL nPosFim
   LOCAL nSize
   LOCAL i

   nSize := Len( cString ) - Len( StrTran( cString, cDelim ) ) + 1
   aElem := Array( nSize )

   cBuf := cString
   FOR i := 1 TO nSize
      nPosFim := At( cDelim, cBuf )

      IF nPosFim > 0
         aElem[ i ] := SubStr( cBuf, 1, nPosFim - 1 )
      ELSE
         aElem[ i ] := cBuf
      ENDIF

      cBuf := SubStr( cBuf, nPosFim + 1, Len( cBuf ) )

   NEXT

   RETURN aElem[ nRet ]

/****
*
*     CgiParseVar()
*
*     Separates elements of a CGI query environment variable
*
*/

FUNCTION CgiParseVar( cEnvVar )

   cEnvVar := HtmlDecodeUrl( cEnvVar )

   IF "=" $ cEnvVar .AND. Len( cEnvVar ) > At( "=", cEnvVar )
      cEnvVar := AllTrim( SubStr( cEnvVar, At( "=", cEnvVar ) + 1 ) )
   ELSE
      cEnvVar := ""
   ENDIF

   RETURN cEnvVar
