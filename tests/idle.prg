PROCEDURE Main()

   CLS

   ? "DEFAULT IDLEREPEAT =", Set( _SET_IDLEREPEAT )
   ?
   ? "Idle Block should be displayed multiple times until key or 10 seconds elapsed!"
   WAIT
   ?

   hb_idleAdd( {|| QOut( "Idle Block" ) } )
   Inkey( 2 )

   Set( _SET_IDLEREPEAT, .F. )

   hb_idleAdd( {|| QOut( "Idle Block2" ) } )

   CLS
   ? "Idle Block and Block-2 should display ONCE! while waitning for key or 10 seconds elapsed!"
   ?
   Inkey( 2 )

   ?
   ? "Again - Idle Block and Block-2 should display ONCE! while waitning for key or 10 seconds elapsed!"
   ?
   Inkey( 2 )
   ?

   RETURN
