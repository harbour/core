/*
 * $Id$
 */

// Standard Harbour ErrorSys system

#include "error.ch"

#define ISCHAR(var)     (ValType(var) == "C")
#define ISNUM(var)      (ValType(var) == "N")

//----------------------------------------------------------------------------//

init procedure ClipInit

   // public getlist := {}    TODO!

   ErrorSys()

return

//----------------------------------------------------------------------------//

static function DefError( oError )
   LOCAL cMessage

   local cInfo := ""
   local n := 2

   cMessage := ErrorMessage(oError)
   IF !Empty(oError:osCode)
      cMessage += " (DOS Error " + LTrim(Str(oError:osCode)) + ")"
   ENDIF

   QOut( cMessage + Chr( 13 ) + Chr( 10 ))

   do while ! Empty( ProcName( n ) )
      QOut("Called from " + ProcName( n ) + ;
               "(" + AllTrim( Str( ProcLine( n++ ) ) ) + ")")
   enddo

   // TOFIX: Removing ErrorLevel() call will cause a VM error
   //        don't know why [vszel]
   ErrorLevel(1)
   QUIT

return .F.

//----------------------------------------------------------------------------//

procedure ErrorSys

   ErrorBlock( { | oError | DefError( oError ) } )

return

// [vszel]

STATIC FUNCTION ErrorMessage(oError)
   LOCAL cMessage

   // start error message
   cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   IF ISCHAR(oError:subsystem)
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF ISNUM(oError:subCode)
      cMessage += "/" + LTrim(Str(oError:subCode))
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF ISCHAR(oError:description)
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   DO CASE
   CASE !Empty(oError:filename)
      cMessage += ": " + oError:filename
   CASE !Empty(oError:operation)
      cMessage += ": " + oError:operation
   ENDCASE

   RETURN cMessage

//----------------------------------------------------------------------------//
