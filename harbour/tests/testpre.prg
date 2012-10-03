/*
 * $Id$
 */

REQUEST __PP_STDRULES

PROCEDURE Main()

   LOCAL cString, l_pp
   LOCAL i, j, aScript

   CLS

   l_pp := __pp_init()

   ? "Testing Harbour run-time preprocessing"
   ? "======================================"
   ?

   cString := "@ 10, 10 SAY 'Hello!'"
   ? cString
   ? __pp_process( l_pp, cString )
   ?

   cString := "? 'Hello mom'"
   ? cString
   ? __pp_process( l_pp, cString )
   ?

   cString := 'SET RELATION TO Something INTO MySelf'
   ? cString
   ? __pp_process( l_pp, cString )
   ?

   cString := 'SET RELATION ADDITIVE TO Something INTO YourSelf'
   ? cString
   ? __pp_process( l_pp, cString )
   ?

   cString := "#xcommand DEFAULT <v1> := <x1> => IF <v1> == NIL ; <v1> := <x1> ; END"
   ? cString
   IF __pp_addRule( l_pp, cString )
      ? "Rule added successfully !"
   ELSE
      ? "Rule addition failed ..."
   ENDIF

   cString := 'DEFAULT x := 100'
   ? cString
   ? __pp_process( l_pp, cString )
   ?

   ? "Press <Enter>..."
   __Accept( "" )

   CLS

   aScript := { 'cMyDatabase := "dontknow.dbf"', ;
                'USE (cMyDatabase)', ;
                'GO TOP', ;
                '', ;
                '? MYFIELD, YOURFIELD', ;
                '', ;
                'WAIT "Press <Enter> key..."', ;
                '', ;
                'CLOSE ALL' }

   FOR j := 1 TO 2
      ? iif( j == 1, "Before", "After" ) + " __pp_process()"
      ? "==================="
      ?
      FOR i := 1 TO Len( aScript )

         ? iif( j == 1, aScript[ i ], __pp_process( l_pp, aScript[ i ] ) )

      NEXT
      ?
      ? "Press <Enter> key..."
      __Accept( "" )
      CLS
   NEXT

   RETURN
