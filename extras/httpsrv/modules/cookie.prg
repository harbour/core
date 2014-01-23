/*
 * Harbour Project source code:
 *    uHTTPD cookie example
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
 * www - http://www.harbour-project.org
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

MEMVAR _REQUEST

FUNCTION HRBMAIN()

   LOCAL cHtml := ""
   LOCAL cCookie := uhttpd_GetField( "mycookie" )
   LOCAL cAction := uhttpd_GetField( "action" )
   LOCAL oCookie

   // hb_ToOutDebug( "cCookie: %s, cAction: %s\n\r", hb_ValToExp( cCookie ), cAction )

   hb_default( @cCookie, "" )
   hb_default( @cAction, "" )

   // Sample page embedded
#pragma __cstream | cHtml += %s
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8" />
    <title>Harbour uHTTPD Server cookie example</title>
    <link rel="shortcut icon" href="favicon.ico" type="image/x-icon" />
</head>
<body>
    <h1>Simple uHTTPD server cookie example</h1>
    <br />
    <br />
    <form name=test action="/cgi-bin/cookie.hrb" method="post">
        Type something: <input type="text" name="mycookie" value="<%COOKIE_VALUE%>">
        <input type="submit">
        <input type="hidden" name="action" value="gotoinfo">
    </form>
    Pressing button you will redirect to /info page. Look at COOKIE values.
    <br />You will see a "mycookie" variable name.
    <br />
    <br />Return to <a href="/">Main Page</a>
</body>
</html>
#pragma __endtext

   IF Empty( cAction )
      // Set a simple cookie
      oCookie := uhttpd_CookieNew( "localhost", "/", 1, 0 )
      // cleaning previous cookie
      oCookie:DeleteCookie( "mycookie" )

      cHtml := StrTran( cHtml, "<%COOKIE_VALUE%>", cCookie )
   ELSEIF cAction == "gotoinfo"
      // Set a simple cookie
      oCookie := uhttpd_CookieNew( "localhost", "/", 1, 0 )
      oCookie:SetCookie( "mycookie", cCookie )
      uhttpd_SetHeader( "Location", "/info" )
      // uhttpd_Write( "cookie set <a href='/info'>Go to info page</a>" )
      RETURN NIL
   ENDIF

   RETURN cHtml
