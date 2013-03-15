/*
 * $Id$
 */

// Testing Harbour AND OR operators

PROCEDURE Main()

   ? "Testing logical shortcuts"

   IF .F. .AND. DispAndReturnNIL() // and it should not break!
   ENDIF

   ? "Testing .T. .T."
   AndOr( .T., .T. )

   ? "Testing .T. .F."
   AndOr( .T., .F. )

   ? "Testing .F. .F."
   AndOr( .F., .F. )

   ? "Testing errors..."
   AndOr( 1, .T. )

   RETURN

FUNCTION DispAndReturnNIL()

   ? "this should not show!"

   RETURN NIL

FUNCTION AndOr( lValue1, lValue2 )

   IF lValue1 .AND. lValue2
      ? "They are both true"
   ELSE
      ? "They are not both true"
   ENDIF

   IF lValue1 .OR. lValue2
      ? "At least one of them is true"
   ELSE
      ? "None of them are true"
   ENDIF

   RETURN NIL
