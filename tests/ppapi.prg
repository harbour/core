
REQUEST __pp_StdRules

PROCEDURE Main()

   LOCAL cString, l_pp
   LOCAL i, j, aScript

   l_pp := __pp_Init()

   CLS

   ? "Testing Harbour run-time preprocessing"
   ? "======================================"
   ?

   cString := "@ 10, 10 SAY 'Hello!'"
   ? cString
   ? __pp_Process( l_pp, cString )
   ?

   cString := "? 'Hello mom'"
   ? cString
   ? __pp_Process( l_pp, cString )
   ?

   cString := 'SET RELATION TO Something INTO MySelf'
   ? cString
   ? __pp_Process( l_pp, cString )
   ?

   cString := 'SET RELATION ADDITIVE TO Something INTO YourSelf'
   ? cString
   ? __pp_Process( l_pp, cString )
   ?

   cString := "#xcommand DEFAULT <v1> := <x1> => IF <v1> == NIL ; <v1> := <x1> ; END"
   ? cString
   IF __pp_AddRule( l_pp, cString )
      ? "Rule added successfully !"
   ELSE
      ? "Rule addition failed ..."
   ENDIF

   cString := 'DEFAULT x := 100'
   ? cString
   ? __pp_Process( l_pp, cString )
   ?

   WAIT "Press any key..."

   CLS

   aScript := { ;
      'cMyDatabase := "dontknow.dbf"', ;
      'USE ( cMyDatabase )', ;
      'GO TOP', ;
      '', ;
      '? MYFIELD, YOURFIELD', ;
      '', ;
      'WAIT "Press any key..."', ;
      '', ;
      'CLOSE ALL' }

   FOR j := 1 TO 2
      ? iif( j == 1, "Before", "After" ) + " __pp_Process()"
      ? "==================="
      ?
      FOR i := 1 TO Len( aScript )
         ? iif( j == 1, aScript[ i ], __pp_Process( l_pp, aScript[ i ] ) )
      NEXT
      ?
      WAIT "Press any key..."
      CLS
   NEXT

   RETURN
