/*
 * $Id$
 */
* VIDEOTST.PRG
*
* Copyright 2000 Alejandro de Garate <alex_degarate@hotmail.com>
*
* Test SETMODE() for Harbour Project
*
#define HB_NOT_SUPPORTED  "Video mode not supported on this system.."
#define HB_VROW   1
#define HB_VCOL   2
#define HB_PROMPT 3

FUNCTION MAIN()

 LOCAL nMode:= 1, nRow, lSuccess
 LOCAL aVModes:= {;
             { 12, 40, " 12 x 40 " },;
             { 25, 40, " 25 x 40 " },;
             { 28, 40, " 28 x 40 " },;
             { 50, 40, " 50 x 40 " },;
             { 12, 80, " 12 x 80 " },;
             { 25, 80, " 25 x 80 " },;
             { 28, 80, " 28 x 80 " },;
             { 43, 80, " 43 x 80 " },;
             { 50, 80, " 50 x 80 " },;
             { 60, 80, " 60 x 80 " } }

 DO WHILE (nMode != 0)

    CLEAR SCREEN
    @ 0, 0 SAY "Select the video mode you want to test.."

    FOR nRow:= 1 To 5
        @ 2 + nRow, 10 PROMPT aVModes [nRow] [HB_PROMPT]
    NEXT

    FOR nRow:= 6 To 10
        @ 2 + nRow - 5, 25 PROMPT aVModes [nRow] [HB_PROMPT]
    NEXT
    MENU TO nMode

    If (nMode > 0)
       lSuccess = SETMODE( aVModes [nMode] [HB_VROW], aVModes [nMode] [HB_VCOL])

        IF lSuccess == .T.
            TESTBOX( aVModes [nMode] [HB_PROMPT] )
        ELSE
            @ MAXROW(), 0 SAY HB_NOT_SUPPORTED
            INKEY(0)
        ENDIF
    ENDIF

 ENDDO

 QUIT

 RETURN 0


 **************************
 PROCEDURE TESTBOX( cMode )
 ****************************************************************************
 * Simple testing screen..
 ****************************************************************************
  LOCAL nRow
  CLEAR SCREEN
  @ 0, 0 TO MAXROW(), MAXCOL() DOUBLE
  @ 0, 3 SAY cMode
  @ MAXROW(), 3 SAY " Press a key "

  @ 8, 0 SAY REPLICATE( "         ", 20)
  @ 9, 0 SAY REPLICATE( "0123456789", 20)

  FOR nRow:= 0 TO MAXROW()
      @ nRow, 18 SAY STR( nRow, 2)
  NEXT

  @ 4, 2 SAY "MaxRow() = " + STR( MAXROW(), 3)
  @ 5, 2 SAY "MaxCol() = " + STR( MAXCOL(), 3)
  INKEY(0)
  RETURN

