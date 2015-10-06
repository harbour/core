/*
 * HTML output conversion
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (Porting this library to Harbour)
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

#include "error.ch"
#include "cgi.ch"

#include "fileio.ch"

STATIC s_bFixCorrupt
STATIC s_cErrFooter := " "

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
      hb_eol() + ;
      "</td></tr></table>" + hb_eol() + ;
      '<table bgcolor="white" border cellpadding=1 cellspacing=1 cols=2 width=80%>' + ;
      '<tr><td bgcolor="black" align="center">' + ;
      '<font face="verdana" size="5" color="white">' + hb_eol() + ;
      "<b>ERROR REPORT</b>" + ;
      "</td></tr>" + ;
      '<tr><td bgcolor="blue">' + ;
      '<font face="verdana" size="2" color="white">' + hb_eol() + ;
      "Date: " + hb_DToC( Date(), "yyyy-mm-dd" ) + "<br />" + "Time: " + Time() + "<br />" + ;
      "</td></tr>" + ;
      '<tr><td bgcolor="red">' + ;
      '<font face="verdana" size="2" color="white">' + hb_eol() + ;
      '<em>' + cMessage + '</em>' + ;
      '</td></tr><tr><td bgcolor="cyan">' + hb_eol() + ;
      '<font face="verdana" size="2" color="black">' + hb_eol() + ;
      "ERRORCODE: " + hb_ntos( e:GenCode ) + "<br />" + hb_eol() + ;
      "SUBSYSTEM: " + e:SubSystem + "<br />" + hb_eol() + ;
      "DESCRIPTION: " + e:Description + "<br />" + hb_eol() + ;
      "OPERATION: " + e:Operation + "<br />" + hb_eol() + ;
      "FILENAME: " + e:FileName + "<br />" + hb_eol() + ;
      "TRIES: " + hb_ntos( e:Tries ) + hb_eol() + ;
      '</td></tr>' + ;
      '<tr><td bgcolor="red">' + ;
      '<font face="verdana" size="2" color="white">' + hb_eol() + ;
      '<em>'

   i := 2
   DO WHILE ! Empty( ProcName( i ) )
      cErrString += "Called from " + RTrim( ProcName( i ) ) + ;
         "(" + hb_ntos( ProcLine( i ) ) + ") <br />" + hb_eol()
      i++
   ENDDO

   cErrstring += ;
      '</em>' + ;
      '</td></tr>' + ;
      '<tr><td bgcolor="black">' + ;
      '<font face="verdana" size="2" color="white">' + hb_eol() + ;
      "Extra notes..." + ;
      "</td>" + hb_eol() + "</tr>" + hb_eol() + "</table>" + hb_eol()

   FWrite( nH, "<br />" + cErrString + hb_eol() )
   hb_MemoWrit( "error.Log", HardCR( cErrString ) + hb_eol() + ;
      HardCR( MemoRead( "error.Log" ) ) )

   FWrite( nH, "</td>" + hb_eol() + "</tr>" + hb_eol() + "</table>" + hb_eol() )

   HtmlJSCmd( nH, 'Alert("There was an error processing your request:\n' + ;
      'Look at the bottom of this page for\n' + ;
      'error description and parameters...");' )
   FWrite( nH, "</font>" + hb_eol() + "</body></html>" + hb_eol() )

   dbCloseAll()

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

#if 0

STATIC FUNCTION ErrorMessage( e )

   // start error message
   LOCAL cMessage := iif( e:severity > ES_WARNING, "Error ", "Warning " )

   // add subsystem name if available
   cMessage += hb_defaultValue( e:subsystem, "???" )

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
   DO CASE
   CASE ! HB_ISNULL( e:filename )
      cMessage += ": " + e:filename
   CASE ! Empty( e:operation )
      cMessage += ": " + e:operation
   ENDCASE
   cMessage += hb_eol()

   RETURN cMessage

#endif
