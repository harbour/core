/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL i

   FOR i := 1 TO 10

      ? i

      IF i == 4 .AND. .T.
         __Accept( "" )
         ? i
         i := 9
         ? i
         __Accept( "" )
      ENDIF

   NEXT

   RETURN
