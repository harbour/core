/*
 * $Id$
 */

// Testing Harbour AND OR operators

PROCEDURE Main()

   QOut( "Testing logical shortcuts" )

   IF .F. .AND. QOut( "this should not show!" ) // and it should not break!
   ENDIF

   QOut( "Testing .t. .t." )
   AndOr( .T. , .T. )

   QOut( "Testing .t. .f." )
   AndOr( .T. , .F. )

   QOut( "Testing .f. .f." )
   AndOr( .F. , .F. )

   QOut( "Testing errors..." )
   AndOr( 1, .T. )

   RETURN

FUNCTION AndOr( lValue1, lValue2 )

   IF lValue1 .AND. lValue2
      QOut( "They are both true" )
   ELSE
      QOut( "They are not both true" )
   ENDIF

   IF lValue1 .OR. lValue2
      QOut( "At least one of them is true" )
   ELSE
      QOut( "None of them are true" )
   ENDIF

   RETURN nil
