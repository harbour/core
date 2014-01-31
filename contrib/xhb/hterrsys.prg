/*
 * Harbour Project source code:
 *    HTML output conversion
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

#include "error.ch"
#include "cgi.ch"

#include "fileio.ch"

STATIC s_bFixCorrupt
STATIC s_cErrFooter := " "

/* DefError()
*/

#if 0

STATIC FUNCTION xhb_cgi_DefError( e )

   LOCAL i
   LOCAL cMessage   := ""
   LOCAL cErrString := ""
   LOCAL nH         := hb_defaultValue( HtmlPageHandle(), F_ERROR )

   // by default, division by zero yields zero
   IF e:genCode == EG_ZERODIV
      RETURN 0
   ENDIF

   IF e:genCode == EG_CORRUPTION
      IF HB_ISEVALITEM( s_bFixCorrupt )
         Eval( s_bFixCorrupt, e )
         RETURN .F.
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   // for network open error, set NetErr() and subsystem default
   IF e:genCode == EG_OPEN .AND. ( e:osCode == 32 .OR. e:osCode == 5 ) ;
         .AND. e:canDefault

      NetErr( .T. )
      RETURN .F.
   ENDIF

   // for lock error during dbAppend(), set NetErr() and subsystem default
   IF e:genCode == EG_APPENDLOCK .AND. e:canDefault

      NetErr( .T. )
      RETURN .F.
   ENDIF

   // build error message
   cMessage += ErrorMessage( e )

   // display message and traceback
   IF ! Empty( e:osCode )
      cMessage += " (DOS Error: " + hb_ntos( e:osCode ) + ")"
   ENDIF

   cErrString := ;
      CRLF() + ;
      "</td></tr></table>" + CRLF() + ;
      '<table bgcolor="white" border cellpadding=1 cellspacing=1 cols=2 width=80%>' + ;
      '<tr><td bgcolor="black" align="center">' + ;
      '<font face="verdana" size="5" color="white">' + CRLF() + ;
      "<b>ERROR REPORT</b>" + ;
      "</td></tr>" + ;
      '<tr><td bgcolor="blue">' + ;
      '<font face="verdana" size="2" color="white">' + CRLF() + ;
      "Date: " + hb_DToC( Date(), "yyyy-mm-dd" ) + "<br />" + "Time: " + Time() + "<br />" + ;
      "</td></tr>" + ;
      '<tr><td bgcolor="red">' + ;
      '<font face="verdana" size="2" color="white">' + CRLF() + ;
      '<em>' + cMessage + '</em>' + ;
      '</td></tr><tr><td bgcolor="cyan">' + CRLF() + ;
      '<font face="verdana" size="2" color="black">' + CRLF() + ;
      "ERRORCODE: " + hb_ntos( e:GenCode ) + "<br />" + CRLF() + ;
      "SUBSYSTEM: " + e:SubSystem + "<br />" + CRLF() + ;
      "DESCRIPTION: " + e:Description + "<br />" + CRLF() + ;
      "OPERATION: " + e:Operation + "<br />" + CRLF() + ;
      "FILENAME: " + e:FileName + "<br />" + CRLF() + ;
      "TRIES: " + hb_ntos( e:Tries ) + CRLF() + ;
      '</td></tr>' + ;
      '<tr><td bgcolor="red">' + ;
      '<font face="verdana" size="2" color="white">' + CRLF() + ;
      '<em>'

   i := 2
   DO WHILE ! Empty( ProcName( i ) )
      cErrString += "Called from " + RTrim( ProcName( i ) ) + ;
         "(" + hb_ntos( ProcLine( i ) ) + ") <br />" + CRLF()
      i++
   ENDDO

   cErrstring += ;
      '</em>' + ;
      '</td></tr>' + ;
      '<tr><td bgcolor="black">' + ;
      '<font face="verdana" size="2" color="white">' + CRLF() + ;
      "Extra notes..." + ;
      "</td>" + CRLF() + "</tr>" + CRLF() + "</table>" + CRLF()

   FWrite( nH, "<br />" + cErrString + CRLF() )
   hb_MemoWrit( "error.Log", HardCR( cErrString ) + CRLF() + ;
      HardCR( MemoRead( "error.Log" ) ) )

   FWrite( nH, "</td>" + CRLF() + "</tr>" + CRLF() + "</table>" + CRLF() )

   HtmlJSCmd( nH, 'Alert("There was an error processing your request:\n' + ;
      'Look at the bottom of this page for\n' + ;
      'error description and parameters...");' )
   FWrite( nH, "</font>" + CRLF() + "</body></html>" + CRLF() )

   CLOSE ALL

   ErrorLevel( 1 )
   QUIT

   RETURN .F.

#endif

FUNCTION SetCorruptFunc( bFunc )

   IF HB_ISEVALITEM( bFunc )
      s_bFixCorrupt := bFunc
   ENDIF

   RETURN s_bFixCorrupt

FUNCTION SetErrorFooter()

   RETURN s_cErrFooter

/***
* ErrorMessage()
*/

#if 0

STATIC FUNCTION ErrorMessage( e )

   LOCAL cMessage := ""

   // start error message
   cMessage += iif( e:severity > ES_WARNING, "Error ", "Warning " )

   // add subsystem name if available
   IF HB_ISSTRING( e:subsystem )
      cMessage += e:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF HB_ISNUMERIC( e:subCode )
      cMessage += "/" + hb_ntos( e:subCode )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF HB_ISSTRING( e:description )
      cMessage += "<br />  " + e:description
   ENDIF

   // add either filename or operation
   IF ! Empty( e:filename )
      cMessage += ": " + e:filename

   ELSEIF ! Empty( e:operation )
      cMessage += ": " + e:operation

   ENDIF
   cMessage += CRLF()

   RETURN cMessage

#endif
