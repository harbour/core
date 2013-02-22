/*
 * $Id$
 */

#require "hbtest"

PROCEDURE Main()

   HBTEST 2 + 2        IS 4
   HBTEST 2 + 2        IS 5
   HBTEST "a" + "b"    IS "ab"
   HBTEST 2 + ""       IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C: F:S"
   HBTEST 2 + Chr( 9 ) IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C:" + Chr( 9 ) + " F:S"
   HBTEST Chr( 0 )     IS Chr( 0 )
   HBTEST 0d20111213   IS 0d20111213
   HBTEST NIL          IS NIL
   HBTEST .T.          IS .T.
   HBTEST .T.          IS .F.
   HBTEST .T. - .F.    IS "E 1 BASE 1082 Argument error (-) OS:0 #:0 A:2:L:.T.;L:.F. F:S"
   HBTEST " " + " "    IS "  "

   RETURN
