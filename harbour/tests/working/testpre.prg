FUNCTION Main()

   LOCAL cString
   LOCAL i, j, aScript

   CLS

   qOut( "Testing Harbour run-time preprocessing" )
   qOut( "======================================" )
   qOut( "" )

   cString := "@ 10, 10 SAY 'Hello!'"
   qOut( cString )
   qOut( Preprocess( cString ) )
   qOut( "" )

   cString := "? 'Hello mom'"
   qOut( cString )
   qOut( Preprocess( cString ) )
   qOut( "" )

   cString := 'SET RELATION TO Something INTO MySelf'
   qOut( cString )
   qOut( Preprocess( cString ) )
   qOut( "" )

   cString := 'SET RELATION ADDITIVE TO Something INTO YourSelf'
   qOut( cString )
   qOut( Preprocess( cString ) )
   qOut( "" )

   cString := 'CLOSE ALL'
   qOut( cString )
   qOut( Preprocess( cString ) )
   qOut( "" )

   qOut( chr(13)+chr(10)+"Press <Enter>..." )
   __Accept( "" )

   CLS

   aScript := { 'cMyDatabase := "DONTKNOW.DBF"', ;
                'USE (cMyDatabase)', ;
                'GO TOP', ;
                '', ;
                '? MYFIELD, YOURFIELD', ;
                '', ;
                'WAIT "Press <Enter> key..."', ;
                '', ;
                'CLOSE ALL' }

   FOR j := 1 TO 2
      qOut( if( j = 1, "Before", "After" ) + " Preprocess()" )
      qOut( "===================" )
      qOut( "" )
      FOR i := 1 TO len( aScript )

         ? if( j = 1, aScript[i], Preprocess( aScript[i] ) )

      NEXT
      qOut( "" )
      qOut( "Press <Enter> key..." )
      __Accept( "" )
      CLS
   NEXT

   RETURN( NIL )
