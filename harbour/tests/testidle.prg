// #include "set.ch" - Preset in pptable.c

PROCEDURE MAIN()

   LOCAL bIdle := {|| QOut( "Idle Block" ) }

   CLS

   ? _SET_IDLEREPEAT

   ? "DEFAULT IDLEREPEAT =", SET( _SET_IDLEREPEAT )
   ?
   ? "Idle Block should be displayed multiple times until key or 10 seconds elapsed!"
   ? "Press any key to begin..."
   ?
   Inkey(0)

   HB_IDLEADD( bIdle )
   Inkey( 2 )

   SET( _SET_IDLEREPEAT, .F. )

   CLS
   ? "Idle Block should display ONCE! while waitning for key or 10 seconds elapsed!"
   ?
   Inkey( 2 )

RETURN
