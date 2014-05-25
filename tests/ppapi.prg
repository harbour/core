REQUEST __pp_StdRules

PROCEDURE Main()

   LOCAL j, aScript, cString

   LOCAL pPP := __pp_Init()

   CLS

   ? "Testing Harbour run-time preprocessing"
   ? "======================================"
   ?

   ? cString := "@ 10, 10 SAY 'Hello!'"
   ? __pp_Process( pPP, cString )
   ?

   ? cString := "? 'Hello mom'"
   ? __pp_Process( pPP, cString )
   ?

   ? cString := "SET RELATION TO Something INTO MySelf"
   ? __pp_Process( pPP, cString )
   ?

   ? cString := "SET RELATION ADDITIVE TO Something INTO YourSelf"
   ? __pp_Process( pPP, cString )
   ?

   ? cString := "#xcommand DEFAULT <v1> := <x1> => IF <v1> == NIL ; <v1> := <x1> ; END"
   IF __pp_AddRule( pPP, cString )
      ? "Rule added successfully!"
   ELSE
      ? "Rule addition failed..."
   ENDIF

   ? cString := "DEFAULT x := 100"
   ? __pp_Process( pPP, cString )
   ?

   WAIT
   CLS

   aScript := { ;
      "cMyDatabase := 'dontknow.dbf'", ;
      "USE ( cMyDatabase )", ;
      "GO TOP", ;
      "", ;
      "? MYFIELD, YOURFIELD", ;
      "", ;
      "WAIT", ;
      "", ;
      "CLOSE ALL" }

   FOR j := 1 TO 2
      ? iif( j == 1, "Before", "After" ), "__pp_Process()"
      ? "==================="
      ?
      FOR EACH cString IN aScript
         ? iif( j == 1, cString, __pp_Process( pPP, cString ) )
      NEXT
      ?
      WAIT
      CLS
   NEXT

   RETURN
