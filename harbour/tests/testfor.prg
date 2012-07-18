/*
 * $Id$
 */

PRCOEDURE Main()

   LOCAL i

   FOR i := 1 TO 10

      QOut( i )

      IF i == 4 .AND. .T.
         __Accept( "" )
         QOut( i )
         i := 9
         QOut( i )
         __Accept( "" )
      ENDIF

   NEXT

   RETURN
