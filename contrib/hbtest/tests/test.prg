
#require "hbtest"

PROCEDURE Main()

   LOCAL cTable := hb_FNameExtSet( __FILE__, ".dbf" )

   dbCreate( cTable, { { "TESTM", "M", 10, 0 } }, NIL, .T., "w_TEST" )
   dbAppend()
   w_TEST->TESTM := "hello"

   HBTEST 2 + 2             IS 4
   HBTEST "a" + "b"         IS "ab"
   HBTEST Chr( 0 )          IS Chr( 0 )
   HBTEST 0d20111213        IS 0d20111213
   HBTEST NIL               IS NIL
   HBTEST .T.               IS .T.
   HBTEST " " + " "         IS "  "
   HBTEST w_TEST->TESTM     IS "hello"
   HBTEST { "a", {} }       IS '{"a", {}}'
   HBTEST { "a" => 100 }    IS '{"a"=>100}'
   HBTEST TestObj()         IS '__itemSetObjRaw( {NIL, NIL, NIL, "Harbour", NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}, {{"ERROR",}} )'

   /* RTEs */
   HBTEST 2 + ""            IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C: F:S"
   HBTEST 2 + Chr( 9 )      IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C:\011 F:S"
   HBTEST 2 + '"'           IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C:\042 F:S"
   HBTEST 2 + "'"           IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:2;C:' F:S"
   HBTEST "" + 0d20111213   IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:C:;D:0d20111213 F:S"
   HBTEST .T. - .F.         IS "E 1 BASE 1082 Argument error (-) OS:0 #:0 A:2:L:.T.;L:.F. F:S"
   HBTEST w_TEST->TESTM + 0 IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:M:hello;N:0 F:S"
   HBTEST {} + 0            IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:A:{ Array of 0 Items };N:0 F:S"

   /* mismatches */
   HBTEST hb_BChar( 254 )   IS hb_BChar( 255 )
   HBTEST 0d20111213        IS 0d20111214
   HBTEST 2 + 2             IS 5
   HBTEST .T.               IS .F.

   dbCloseArea()
   hb_dbDrop( cTable )

   RETURN

STATIC FUNCTION TestObj()

   LOCAL o := ErrorNew()

   o:description := "Harbour"

   RETURN o
