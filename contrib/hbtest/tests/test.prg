/* Copyright 2013 Viktor Szakats (vszakats.net/harbour) */

#require "hbtest"

PROCEDURE Main()

   LOCAL tmp, hbtest_Table_OK

   hb_dbCreateTemp( "w_TEST", { { "TESTM", "M", 10, 0 } } )
   dbAppend()
   w_TEST->TESTM := "hello"

   HBTEST 2 + 2             IS 4
   HBTEST "a" + "b"         IS "ab"
   HBTEST hb_BChar( 0 )     IS hb_BChar( 0 )
   HBTEST 0d20111213        IS 0d20111213
   HBTEST NIL               IS NIL
   HBTEST .T.               IS .T.
   HBTEST " " + " "         IS "  "
   HBTEST w_TEST->TESTM     IS "hello"
   HBTEST { "a", {} }       IS '{"a", {}}'
   HBTEST { "a" => 100 }    IS '{"a"=>100}'
   HBTEST hbtest_Object()   IS '__itemSetObjRaw( {NIL, NIL, NIL, "Harbour", NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}, {{"ERROR",}} )'

   /* RTEs */
   HBTEST 2 + ""            IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 F:S"
   HBTEST 2 + hb_BChar( 9 ) IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 F:S"
   HBTEST 2 + '"'           IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 F:S"
   HBTEST 2 + "'"           IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 F:S"
   HBTEST "" + 0d20111213   IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 F:S"
   HBTEST .T. - .F.         IS "E 1 BASE 1082 Argument error (-) OS:0 #:0 F:S"
   HBTEST w_TEST->TESTM + 0 IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 F:S"
   HBTEST {} + 0            IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 F:S"

   dbCloseArea()

   /* test table */
   hbtest_Table_OK := hbtest_Table()
   HBTEST hbtest_Table_OK   IS .T.
   HBTEST LastRec()         IS 500
   dbCloseArea()

   /* mismatches */
   HBTEST hb_BChar( 254 )   IS hb_BChar( 255 )
   HBTEST 0d20111213        IS 0d20111214
   HBTEST 2 + 2             IS 5
   HBTEST .T.               IS .F.

   ? "Test types:", ""
   FOR EACH tmp IN hbtest_AllTypes()
      ?? ValType( tmp )
   NEXT
   ?

   ? "Test values:"
   FOR EACH tmp IN hbtest_AllValues()
      ? ValType( tmp ), tmp, hb_ValToExp( tmp )
   NEXT
   ?

   RETURN
