
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Cgi Class
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include "hbclass.ch"
#include "default.ch"
#include "html.ch"



CLASS oCgi FROM HTML

DATA nH
DATA Server_Software
DATA Server_Name
DATA Gateway_Interface
DATA Server_Protocol
DATA Server_Port
DATA Request_Method
DATA Http_Accept
DATA Http_User_agent
DATA Http_Referer
DATA Path_Info
DATA Path_Translated
DATA Script_Name
DATA Query_String
DATA Remote_Host
DATA Remote_Addr
DATA ipAddress
DATA Remote_User
DATA Auth_Type
DATA Auth_User
DATA Auth_Pass
DATA Content_Type
DATA Content_Length
DATA Annotation_Server

DATA aQueryFields      INIT {}

METHOD New( c )
METHOD CGIField( c )

METHOD toObject()


ENDCLASS



/****
*
*     oCgi():new()
*
*
*
*/

METHOD New( cInBuffer ) Class oCgi
LOCAL cBuff, i, nBuff
LOCAL aTemp := {}
LOCAL aVar  := {}

   // function in oHtm.prg
   ::nH   := PageHandle()

   ::Server_Software      :=  GetEnv( "SERVER_SOFTWARE"   )
   ::Server_Name          :=  GetEnv( "SERVER_NAME"       )
   ::Gateway_Interface    :=  GetEnv( "GATEWAY_INTERFACE" )
   ::Server_Protocol      :=  GetEnv( "SERVER_PROTOCOL"   )
   ::Server_Port          :=  GetEnv( "SERVER_PORT"       )
   ::Request_Method       :=  GetEnv( "REQUEST_METHOD"    )
   ::Http_Accept          :=  GetEnv( "HTTP_ACCEPT"       )
   ::Http_User_agent      :=  GetEnv( "HTTP_USER_AGENT"   )
   ::Http_Referer         :=  GetEnv( "HTTP_REFERER"      )
   ::Path_Info            :=  GetEnv( "PATH_INFO"         )
   ::Path_Translated      :=  GetEnv( "PATH_TRANSLATED"   )
   ::Script_Name          :=  GetEnv( "SCRIPT_NAME"       )
   ::Query_String         :=  GetEnv( "QUERY_STRING"      )
   ::Remote_Host          :=  GetEnv( "REMOTE_HOST"       )
   ::Remote_Addr          :=  GetEnv( "REMOTE_ADDR"       )
   ::ipAddress            :=  GetEnv( "REMOTE_ADDR"       )
   ::Remote_User          :=  GetEnv( "REMOTE_USER"       )
   ::Auth_Type            :=  GetEnv( "AUTH_TYPE"         )
   ::Auth_User            :=  GetEnv( "AUTH_USER"         )
   ::Auth_Pass            :=  GetEnv( "AUTH_PASS"         )
   ::Content_Type         :=  GetEnv( "CONTENT_TYPE"      )
   ::Content_Length       :=  GetEnv( "CONTENT_LENGTH"    )
   ::Annotation_Server    :=  GetEnv( "ANNOTATION_SERVER" )

   IF cInBuffer != NIL
      ::Query_String := RTRIM( cInBuffer )
   ELSE
      IF "POST" $ UPPER(::Request_Method)
         ::Query_String := RTRIM(FReadStr( STD_IN, VAL(::CONTENT_LENGTH) ))
      ENDIF
   ENDIF

   IF !empty( ::Query_String )
     
     ::aQueryFields := {}

     aTemp := listAsArray( ::Query_String, "&" )  // separate fields

     IF LEN( aTemp ) != 0
        FOR i=1 TO LEN( aTemp )
           aVar := LISTASARRAY( aTemp[i], "=" )
           IF LEN( aVar ) == 2
            AADD(::aQueryFields, {aVar[1], decodeURL(aVar[2]) } )
           ENDIF
        NEXT
     ENDIF
   ENDIF

//#ifdef _CLASS_CH
RETURN ::ToObject()  //Self
//#else
//RETURN Self
//#endif



//#ifdef _CLASS_CH


/****
*
*        oCgi():ToObject()
*
*        Creates instance variables out of CGI FORM return values
*        or URL encoded content.
*
*        It subclasses the oCGI class to a *new* class
*/

METHOD ToObject() CLAss ocgi
local i, bBlock
local cFldName
LOCAL nScope:=1
LOCAL aDb, oDb, hNewClass, oNew, n
LOCAL hNew
STATIC sn := 0

// --> create new oObject class from this one...
sn++
//aDB := ClassNew( "NewCgi"+STRZERO(sn,3), {|| oCGI() }, "cgi" )
aDb:= hbClass():New("NewCgi"+STRZERO(sn,3),__CLS_PARAM("oCgi" ) )
FOR i = 1 TO LEN( ::aQueryFields )
    IF ::aQueryFields[i,2] == NIL .OR. EMPTY(::aQueryFields[i,2])
       ::aQueryFields[i,2] := ""
    ENDIF
    //ClassData( ::aQueryFields[i,1], ::aQueryFields[i,2] )
    adb:AddData( ::aQueryFields[i,1],::aQueryFields[i,2],,nScope)
//    adb:AddData( ,,,nScope)
//  aDb:AddMultiData(,,if(.F.,1,if(.F.,2,if(.F.,4,nScope) ) ) + if(.F.,16,0 ),__MULTIPARAM("nH" ) ) 
NEXT
adb:Create()
//aDB                       := ClassMAKE()
//hNewClass                 := aDB[_CLASS_HANDLE]
oNew                      := adb:Instance()
//aDb                       := ClassEndIns( oNew, aDB )
//aDB[_CLASS_OBJECT]        := oNew
//oNew:Dict                 := aDB
oNew:aQueryFields         := ::aQueryFields
oNew:Server_Software      := ::Server_Software
oNew:Server_Name          := ::Server_Name
oNew:Gateway_Interface    := ::Gateway_Interface
oNew:Server_Protocol      := ::Server_Protocol
oNew:Server_Port          := ::Server_Port
oNew:Request_Method       := ::Request_Method
oNew:Http_Accept          := ::Http_Accept
oNew:Http_User_agent      := ::Http_User_agent
oNew:Http_Referer         := ::Http_Referer
oNew:Path_Info            := ::Path_Info
oNew:Path_Translated      := ::Path_Translated
oNew:Script_Name          := ::Script_Name
oNew:Query_String         := ::Query_String
oNew:Remote_Host          := ::Remote_Host
oNew:Remote_Addr          := ::Remote_Addr
oNew:ipAddress            := ::ipAddress
oNew:Remote_User          := ::Remote_User
oNew:Auth_Type            := ::Auth_Type
oNew:Content_Type         := ::Content_Type
oNew:Content_Length       := ::Content_Length
oNew:Annotation_Server    := ::Annotation_Server
oNew:nH                   := IF( PageHandle()==NIL, STD_OUT, PageHandle() )

RETURN oNew

//#endif

// ripped from HARBOUR

METHOD CGIField( cQueryName ) Class oCgi

   LOCAL cRet := ""
   LOCAL nRet

   DEFAULT cQueryName := ""

   nRet := aScan( ::aQueryFields, ;
      { |x| upper( x[1] ) = upper( cQueryName ) } )

   IF nRet > 0
      cRet := ::aQueryFields[nRet, 2]
   ENDIF

RETURN( cRet )


// ripped from HARBOUR

FUNCTION ParseString( cString, cDelim, nRet )

   LOCAL cBuf, aElem, nPosFim, nSize, i

   nSize := len( cString ) - len( StrTran( cString, cDelim, '' ) ) + 1
   aElem := array( nSize )

   cBuf := cString
   i := 1
   FOR i := 1 TO nSize
      nPosFim := at( cDelim, cBuf )

      IF nPosFim > 0
         aElem[i] := substr( cBuf, 1, nPosFim - 1 )
      ELSE
         aElem[i] := cBuf
      ENDIF

      cBuf := substr( cBuf, nPosFim + 1, len( cBuf ) )

   NEXT i

RETURN( aElem[ nRet ] )


// ripped from HARBOUR

FUNCTION Hex2Dec( cHex )

   LOCAL aHex := { { "0", 00 }, ;
                   { "1", 01 }, ;
                   { "2", 02 }, ;
                   { "3", 03 }, ;
                   { "4", 04 }, ;
                   { "5", 05 }, ;
                   { "6", 06 }, ;
                   { "7", 07 }, ;
                   { "8", 08 }, ;
                   { "9", 09 }, ;
                   { "A", 10 }, ;
                   { "B", 11 }, ;
                   { "C", 12 }, ;
                   { "D", 13 }, ;
                   { "E", 14 }, ;
                   { "F", 15 } }
   LOCAL nRet
   LOCAL nRes

   nRet := ascan( aHex, { |x| upper( x[1] ) = upper( left( cHex, 1 ) ) } )
   nRes := aHex[nRet, 2] * 16
   nRet := ascan( aHex, { |x| upper( x[1] ) = upper( right( cHex, 1 ) ) } )
   nRes += aHex[nRet, 2]

   RETURN( nRes )

