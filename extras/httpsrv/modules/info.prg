/*
 * uHTTPD info page
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
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

MEMVAR _SERVER, _REQUEST, _GET, _POST, _COOKIE, _SESSION, _HTTP_REQUEST, _HTTP_RESPONSE

FUNCTION HRBMAIN()

   LOCAL cHtml := ;
      "<big>Server Info</big>" + ;
/*    "<br /><br />If it is first time you see this page reload it to see cookies<br /><br />" */ + ;
      '<br /><br />Return to <a href="/">Main Page</a><br /><br />' + ;
      DisplayVars( _Server       , "SERVER Vars" ) + "<br />" + ;
      DisplayVars( _HTTP_REQUEST , "HTTP Request Headers" ) + "<br />" + ;
      DisplayVars( _HTTP_RESPONSE, "HTTP Response Headers" ) + "<br />" + ;
      DisplayVars( _Get          , "GET Vars" ) + "<br />" + ;
      DisplayVars( _Post         , "POST Vars" ) + "<br />" + ;
      DisplayVars( _Cookie       , "COOKIE Vars" ) + "<br />" + ;
/*    DisplayVars( _Files        , "FILE Vars" ) + "<br />" */ + ;
      DisplayVars( _Request      , "REQUEST Vars" ) + "<br />" + ;
      DisplayVars( _Session      , "SESSION Vars" ) + "<br />"

   // Set a simple cookie
#if 0
   LOCAL oCookie := uhttpd_CookieNew( "localhost", "/", 1, 0 )
   oCookie:SetCookie( "samplecookie", "test" )
   oCookie:SetCookie( "samplecookie2", "test2" )
#endif

   _SESSION[ "Session_Var1" ] := "Test1"
   _SESSION[ "Session_Var2" ] := "Test2"

   RETURN cHtml

STATIC FUNCTION DisplayVars( hHash, cTitle )
   RETURN ;
      "<table width='90%' align='center' border='1'>" + ;
      "<th colspan=2>" + hb_CStr( cTitle ) + "</th>" + ;
      "<tr>" + ;
      "<th width='20%'>KEY</th>" + ;
      "<th width='80%'>VALUE</th>" + ;
      "</tr>" + ;
      DisplayHash( hHash ) + ;
      "</table>"

STATIC FUNCTION DisplayHash( hHash )

   LOCAL cHtml := ""
   LOCAL cKey, cSubKey, xValue

   FOR EACH cKey IN hHash:Keys
      cHtml += "<tr>"
      IF HB_ISHASH( hHash[ cKey ] )
         cHtml += ;
            "<td>" + hb_CStr( cKey ) + "</td>" + ;
            "<td>-------</td>"
         FOR EACH cSubKey IN hHash[ cKey ]:Keys
            xValue := hHash[ cKey ][ cSubKey ]
            cHtml += ;
               "<tr>" + ;
               "<td>" + hb_CStr( cSubKey ) + "</td>" + ;
               "<td>" + iif( Empty( xValue ), "<i>no value</i>", hb_CStr( xValue ) ) + "</td>" + ;
               "</tr>"
         NEXT
      ELSE
         xValue := hHash[ cKey ]
         cHtml += ;
            "<td>" + hb_CStr( cKey ) + "</td>" + ;
            "<td>" + iif( Empty( xValue ), "<i>no value</i>", hb_CStr( xValue ) ) + "</td>"
      ENDIF
      cHtml += "</tr>"
   NEXT

   RETURN cHtml
