/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The default error handler
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    ERRORSYS() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "error.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      ERRORSYS()
 *  $CATEGORY$
 *      Error recovery
 *  $ONELINER$
 *      Install default error handler
 *  $SYNTAX$
 *      ERRORSYS() --> NIL
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      ERRORSYS() always return NIL.
 *  $DESCRIPTION$
 *      ERRORSYS() is called upon startup by Harbour and install the default
 *      error handler. Normally you should not call this function directly,
 *      instead use ERRORBLOCK() to install your own error handler.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ERRORSYS() works exactly like CA-Clipper's ERRORSYS().
 *  $SEEALSO$
 *      ERRORBLOCK(),Error class
 *  $END$
 */

PROCEDURE ErrorSys

   ErrorBlock( { | oError | DefError( oError ) } )

   RETURN

STATIC FUNCTION DefError( oError )
   LOCAL cMessage
   LOCAL cDOSError

   LOCAL aOptions
   LOCAL nChoice

   LOCAL n

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV
      RETURN 0
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. ;
      oError:osCode == 32 .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   cMessage := ErrorMessage( oError )
   IF ! Empty( oError:osCode )
      cDOSError := "(DOS Error " + LTrim( Str( oError:osCode ) ) + ")"
   ENDIF

   // Build buttons

   aOptions := {}

// AAdd( aOptions, "Break" )
   AAdd( aOptions, "Quit" )

   IF oError:canRetry
      AAdd( aOptions, "Retry" )
   ENDIF

   IF oError:canDefault
      AAdd( aOptions, "Default" )
   ENDIF

   // Show alert box

   nChoice := 0
   WHILE nChoice == 0

      IF Empty( oError:osCode )
         nChoice := Alert( cMessage, aOptions )
      ELSE
         nChoice := Alert( cMessage + ";" + cDOSError, aOptions)
      ENDIF

   ENDDO

   IF ! Empty( nChoice )
      DO CASE
      CASE aOptions[ nChoice ] == "Break"
         Break( oError )
      CASE aOptions[ nChoice ] == "Retry"
         RETURN .T.
      CASE aOptions[ nChoice ] == "Default"
         RETURN .F.
      ENDCASE
   ENDIF

   // "Quit" selected

   IF ! Empty( oError:osCode )
      cMessage += " " + cDOSError
   ENDIF

   QOut( cMessage )

   n := 2
   WHILE ! Empty( ProcName( n ) )
      QOut("Called from " + ProcName( n ) + ;
               "(" + AllTrim( Str( ProcLine( n++ ) ) ) + ")")
   ENDDO

   QUIT

   RETURN .F.

// [vszakats]

STATIC FUNCTION ErrorMessage( oError )
   LOCAL cMessage

   // start error message
   cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   IF ISCHARACTER( oError:subsystem )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF ISNUMBER( oError:subCode )
      cMessage += "/" + LTrim( Str( oError:subCode ) )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF ISCHARACTER( oError:description )
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   DO CASE
   CASE !Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE !Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   RETURN cMessage
