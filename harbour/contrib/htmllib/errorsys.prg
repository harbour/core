/*
 * $Id$
 */


/***
*
*	Errorsys.prg
*
*  Standard Clipper error handler
*
*  Copyright (c) 1990-1993, Computer Associates International, Inc.
*  All rights reserved.
*
*  Compile:  /m /n /w
*
*/

/*
 * Harbour Project source code:
 *    HTML output conversion
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




#include "default.ch"
#include "error.ch"


#define DEF_ERR_HEADER "Date : "+DTOC(Date())+"<BR>"+;
                       "Time : "+Time()+"<BR>"


// put messages to STDERR
#command ? <list,...>   =>  ?? Chr(13) + Chr(10) ; ?? <list>
#command ?? <list,...>  =>  OutErr(<list>)


// used below
#xTranslate NTRIM(<n>) => ALLTrim( Str( <n> ) )

REQUEST HARDCR
REQUEST MEMOWRIT

STATIC sbFixCorrupt
STATIC scErrFooter := " "


/***
*	ErrorSys()
*
*	Note:  automatically executes at startup
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC ErrorSys()
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
     ErrorBlock( {|e| DefError(e)} )
return



/***
*	DefError()
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNC DefError(e)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
local i, cMessage:= ""
Local cErrString := ""
LOCAL nDispCount := DispCount()
Local aError     := {}
LOCAL nH := IF( PageHandle() == NIL, 0, PageHandle() )

     // by default, division by zero yields zero
     IF ( e:genCode == EG_ZERODIV )
          return (0)
     END

      IF ( e:genCode == EG_CORRUPTION )
              if valtype(sbFixCorrupt) == "B"
                  EVAL( sbFixCorrupt, e )
                  RETURN .F.
              ELSE
                  RETURN .F.
              ENDIF
      ENDIF

	// for network open error, set NETERR() and subsystem default
     IF ( e:genCode == EG_OPEN .and. (e:osCode == 32 .OR. e:osCode == 5);
          .and. e:canDefault )

          NETERR(.T.)
          RETURN (.F.)                                           // NOTE

     END


     // for lock error during APPEND BLANK, set NETERR() and subsystem default
     IF ( e:genCode == EG_APPENDLOCK .and. e:canDefault )

          NETERR(.T.)
          RETURN (.F.)                                           // NOTE

     END

	// build error message
     cMessage   += ErrorMessage(e)


	// display message and traceback
     IF ( !Empty(e:osCode) )
          cMessage += " (DOS Error   : " + NTRIM(e:osCode) + ")"
     END

     // RESET System //

     cErrString := CRLF()+"</TD></TR></TABLE>"+CRLF()
     cErrString += '<TABLE bgcolor="white" border CellPadding=1 CellSpacing=1 COLS=2 WIDTH=80%>'

     cErrString += '<TR><TD bgcolor="black" align="CENTER">'
     cErrstring += '<FONT face = "verdana" size ="5" color="white">'+CRLF()
     cErrString += "<B>ERROR REPORT</B>"
     cErrString += "</TD></TR>"

     cErrString += '<TR><TD bgcolor="blue">'
     cErrstring += '<FONT face = "verdana" size ="2" color="white">'+CRLF()
     cErrString += DEF_ERR_HEADER
     cErrString += "</TD></TR>"

     cErrString += '<TR><TD bgcolor="red">'
     cErrstring += '<FONT face ="verdana" size ="2" color="white">'+CRLF()
     cErrString +=  '<EM>'+cMessage+'</EM>'


     cErrString += '</TD></TR><TR><TD bgcolor="cyan">'+CRLF()
     cErrstring += '<FONT face ="verdana" size ="2" color="black">'+CRLF()
     cErrString += "ERRORCODE...... :"+ NTRIM(e:GenCode)+"<BR>"+CRLF()
     cErrString += "SUBSYSTEM..... :"+ e:SubSystem     +"<BR>"+CRLF()
     cErrString += "DESCRIPTION...:"+ e:Description   +"<BR>"+CRLF()
     cErrString += "OPERATION......:"+ e:Operation     +"<BR>"+CRLF()
     cErrString += "FILENAME........ :"+ e:FileName      +"<BR>"+CRLF()
     cErrString += "TRIES............. :"+ NTRIM(e:Tries)+CRLF()

     cErrString += '</TD></TR>'
     cErrString += '<TR><TD bgcolor="red">'
     cErrstring += '<FONT face ="verdana" size ="2" color="white">'+CRLF()
     cErrstring += '<EM>'

     i := 2

     while ( !Empty(ProcName(i)) )

          cErrString += "Called from "+ Trim(ProcName(i)) + ;
               "(" + NTRIM(ProcLine(i)) + ") <BR>" + CRLF()

		i++
     END

     cErrstring += '</EM>'
     cErrString += '</TD></TR>'
     cErrString += '<TR><TD bgcolor="black">'
     cErrstring += '<FONT face ="verdana" size ="2" color="white">'+CRLF()
     cErrstring += "Extra Notes..."

     cErrString += "</TD>"+CRLF()+"</TR>"+CRLF()+"</TABLE>"+CRLF()
     FWrite( nH,  "<BR>"+cErrString+CRLF() )
     //컴컴컴컴  Write/Append Error Log
     MemoWrit( "Error.Log", HARDCR(cErrString)+CRLF()+;
                            HARDCR( MEMOREAD("Error.Log") ) )

     FWrite( nH,  "</TD>"+CRLF()+"</TR>"+CRLF()+"</TABLE>"+CRLF() )

/*
     FWrite( nH,  "<FORM NAME='MyForm'>"+CRLF() )
     FWrite( nH,  "<INPUT TYPE=BUTTON NAME='MyButton'"+CRLF() )
     FWrite( nH,  "onFocus=")
     FWrite( nH,  ["alert('Hello')">]+CRLF() )
     FWrite( nH,  "</FORM>"+CRLF() )
     FWrite( nH,  [<A HREF="alert('ERROR!!!')>"]+CRLF() )
*/

     JavaCMD( nH, 'alert("There was an error processing your request:\n'+;
                         'Look at the bottom of this page for\n'+;
                         'error description and parameters...");' )
     FWrite( nH,  "</FONT>"+CRLF()+"</BODY></HTML>"+CRLF() )

     CLOSE ALL

     // give up
     ErrorLevel(1)
     QUIT

RETURN(.F.)



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION SetCorruptFunc( bFunc )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
if valtype( bFunc ) == "B"
   sbFixCorrupt := bFunc
ENDIF

RETURN sbFixCorrupt


FUNCTION SetErrorFooter()
RETURN( scErrFooter )


/***
*	ErrorMessage()
*/
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNC ErrorMessage(e)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
local cMessage :=""

	// start error message
     cMessage += if( e:severity > ES_WARNING, "Error ", "Warning " )


	// add subsystem name if available
     IF ( ValType(e:subsystem) == "C" )
		cMessage += e:subsystem()
     ELSE
		cMessage += "???"
     END


	// add subsystem's error code if available
	if ( ValType(e:subCode) == "N" )
		cMessage += ("/" + NTRIM(e:subCode))
     ELSE
		cMessage += "/???"
     END


	// add error description if available
	if ( ValType(e:description) == "C" )
          cMessage += ("<BR>  " + e:description)
     END


	// add either filename or operation
	if ( !Empty(e:filename) )
          cMessage += (": " + e:filename)

     ELSEIF ( !Empty(e:operation) )
		cMessage += (": " + e:operation)

     END
     cMessage += CRLF()


return (cMessage)

