/*
 * $Id$
 */

FUNCTION Main()

   LOCAL cString
   LOCAL i, j, aScript

   CLS

   QOut( "Testing Harbour run-time preprocessing" )
   QOut( "======================================" )
   QOut( "" )

   cString := "@ 10, 10 SAY 'Hello!'"
   QOut( cString )
   QOut( __Preprocess( cString ) )
   QOut( "" )

   cString := "? 'Hello mom'"
   QOut( cString )
   QOut( __Preprocess( cString ) )
   QOut( "" )

   cString := 'SET RELATION TO Something INTO MySelf'
   QOut( cString )
   QOut( __Preprocess( cString ) )
   QOut( "" )

   cString := 'SET RELATION ADDITIVE TO Something INTO YourSelf'
   QOut( cString )
   QOut( __Preprocess( cString ) )
   QOut( "" )

   cString := "#xcommand DEFAULT <v1> := <x1> => IF <v1> == NIL ; <v1> := <x1> ; END"
   QOut( cString )
   IF __ppAddRule( cString )
      QOut( "Rule added successfully !" )
   ELSE
      QOut( "Rule addition failed ..." )
   ENDIF

   cString := 'DEFAULT x := 100'
   QOut( cString )
   QOut( __Preprocess( cString ) )
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
      QOut( iif( j = 1, "Before", "After" ) + " __Preprocess()" )
      QOut( "===================" )
      QOut( "" )
      FOR i := 1 TO Len( aScript )

         ? iif( j = 1, aScript[ i ], __Preprocess( aScript[ i ] ) )

      NEXT
      QOut( "" )
      QOut( "Press <Enter> key..." )
      __Accept( "" )
      CLS
   NEXT

   RETURN NIL

EXIT PROCEDURE ExitTest()
   __PP_Free()
   RETURN
