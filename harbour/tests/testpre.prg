/*
 * $Id$
 */

REQUEST __PP_STDRULES

PROCEDURE Main()

   LOCAL cString, l_pp
   LOCAL i, j, aScript

   CLS

   l_pp := __pp_init()

   QOut( "Testing Harbour run-time preprocessing" )
   QOut( "======================================" )
   QOut( "" )

   cString := "@ 10, 10 SAY 'Hello!'"
   QOut( cString )
   QOut( __pp_process( l_pp, cString ) )
   QOut( "" )

   cString := "? 'Hello mom'"
   QOut( cString )
   QOut( __pp_process( l_pp, cString ) )
   QOut( "" )

   cString := 'SET RELATION TO Something INTO MySelf'
   QOut( cString )
   QOut( __pp_process( l_pp, cString ) )
   QOut( "" )

   cString := 'SET RELATION ADDITIVE TO Something INTO YourSelf'
   QOut( cString )
   QOut( __pp_process( l_pp, cString ) )
   QOut( "" )

   cString := "#xcommand DEFAULT <v1> := <x1> => IF <v1> == NIL ; <v1> := <x1> ; END"
   QOut( cString )
   IF __pp_addRule( l_pp, cString )
      QOut( "Rule added successfully !" )
   ELSE
      QOut( "Rule addition failed ..." )
   ENDIF

   cString := 'DEFAULT x := 100'
   QOut( cString )
   QOut( __pp_process( l_pp, cString ) )
   QOut( "" )

   QOut( "Press <Enter>..." )
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
      QOut( iif( j == 1, "Before", "After" ) + " __pp_process()" )
      QOut( "===================" )
      QOut( "" )
      FOR i := 1 TO Len( aScript )

         ? iif( j == 1, aScript[ i ], __pp_process( l_pp, aScript[ i ] ) )

      NEXT
      QOut( "" )
      QOut( "Press <Enter> key..." )
      __Accept( "" )
      CLS
   NEXT

   RETURN
