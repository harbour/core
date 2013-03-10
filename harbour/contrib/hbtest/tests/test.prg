/*
 * $Id$
 */

#require "hbtest"

PROCEDURE Main()

   HBTEST 2 + 2           IS 4
   HBTEST "a" + "b"       IS "ab"
   HBTEST Chr( 0 )        IS Chr( 0 )
   HBTEST 0d20111213      IS 0d20111213
   HBTEST NIL             IS NIL
   HBTEST .T.             IS .T.
   HBTEST " " + " "       IS "  "

   /* RTEs */
   HBTEST 2 + ""          IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C: F:S"
   HBTEST 2 + Chr( 9 )    IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C:\011 F:S"
   HBTEST 2 + '"'         IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C:\042 F:S"
   HBTEST 2 + "'"         IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C:' F:S"
   HBTEST "" + 0d20111213 IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:C:;D:0d20111213 F:S"
   HBTEST .T. - .F.       IS "E 1 BASE 1082 Argument error (-) OS:0 #:0 A:2:L:.T.;L:.F. F:S"

   /* mismatches */
   HBTEST hb_BChar( 254 ) IS hb_BChar( 255 )
   HBTEST 0d20111213      IS 0d20111214
   HBTEST 2 + 2           IS 5
   HBTEST .T.             IS .F.

   RETURN
