// Copyright 2000 Alejandro de Garate <alex_degarate hotmail com>

// Test SetMode() for Harbour

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

#define HB_VROW    1
#define HB_VCOL    2
#define HB_PROMPT  3

PROCEDURE Main()

   LOCAL nMode := 1, nRow
   LOCAL aVModes := { ;
      { 12, 40, " 12 x 40 " }, ;
      { 25, 40, " 25 x 40 " }, ;
      { 28, 40, " 28 x 40 " }, ;
      { 50, 40, " 50 x 40 " }, ;
      { 12, 80, " 12 x 80 " }, ;
      { 25, 80, " 25 x 80 " }, ;
      { 28, 80, " 28 x 80 " }, ;
      { 43, 80, " 43 x 80 " }, ;
      { 50, 80, " 50 x 80 " }, ;
      { 60, 80, " 60 x 80 " } }

   DO WHILE nMode != 0

      CLS
      @ 0, 0 SAY "Select the video mode you want to test."

      FOR nRow := 1 TO 5
         @ 2 + nRow, 10 PROMPT aVModes[ nRow ][ HB_PROMPT ]
      NEXT

      FOR nRow := 6 TO 10
         @ 2 + nRow - 5, 25 PROMPT aVModes[ nRow ][ HB_PROMPT ]
      NEXT
      MENU TO nMode

      IF nMode > 0

         IF SetMode( aVModes[ nMode ][ HB_VROW ], aVModes[ nMode ][ HB_VCOL ] )
            TESTBOX( aVModes[ nMode ][ HB_PROMPT ] )
         ELSE
            @ MaxRow(), 0 SAY "Video mode not supported on this system."
            Inkey( 0 )
         ENDIF
      ENDIF

   ENDDO

   RETURN

// Simple testing screen.

STATIC PROCEDURE TESTBOX( cMode )

   LOCAL nRow

   CLS
   @ 0, 0 TO MaxRow(), MaxCol() DOUBLE
   @ 0, 3 SAY cMode
   @ MaxRow(), 3 SAY " Press a key "

   @ 8, 0 SAY Replicate( "|         ", 20 )
   @ 9, 0 SAY Replicate( "0123456789", 20 )

   FOR nRow := 0 TO MaxRow()
      @ nRow, 18 SAY nRow
   NEXT

   @ 4, 2 SAY "MaxRow() == " + hb_ntos( MaxRow() )
   @ 5, 2 SAY "MaxCol() == " + hb_ntos( MaxCol() )
   Inkey( 0 )

   RETURN
