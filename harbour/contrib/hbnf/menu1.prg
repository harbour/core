/*
 * $Id$
 */

/*
 * File......: menu1.prg
 * Author....: Paul Ferrara
 * CIS ID....: 76702,556
 *
 * This is an original work by Paul Ferrara and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:42   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:12   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:40   GLENN
 * Nanforum Toolkit
 *
 */

/*
     For the sample program:

     Compile with "/n /dFT_TEST" SWITCHES AND LINK.

     PASS "MONO" OR "MONO" AS A COMMAND LINE PARAMETER TO FORCE MONO MODE.

     PASS "NOSNOW" OR "NOSNOW" AS A COMMAND LINE PARAMETER ON A CGA.

     PASS "VGA" OR "VGA" AS A COMMAND LINE PARAMETER FOR 50-LINE MODE.
 */

#define LEFTARROW  19
#define RIGHTARROW  4
#define ENTER      13
#define CTRLEND    23
#define CTRLHOME   29
#define HOME        1
#define END         6
#define TAB         9
#define SHIFTTAB  271
#define PGUP       18
#define PGDN        3
#define ESCAPE     27
#define HITTOP      1
#define HITBOTTOM   2
#define KEYEXCEPT   3
#define NEXTITEM    3
#define RESUME      2
#define MAKESELECT  1
#define ABORT       0
#define DISABLE     0
#define ENABLE      1
#define SCNONE      0
#define SCNORMAL    1

THREAD STATIC ACHOICES := {}, AVALIDKEYS := {}
THREAD STATIC NHPOS, NVPOS, NMAXROW, NMAXCOL

// BEGINNING OF DEMO PROGRAM
#IFDEF FT_TEST
   // DUMMY PROCEDURE NAME SO "CCMDLINE" WILL BE LOCAL
   PROCEDURE CALLMENU( cCmdLine )
   LOCAL sDosScrn, nDosRow, nDosCol, lColor

   // my approach to color variables
   // see colorchg.arc on NANFORUM
   STATIC cNormH, cNormN, cNormE, ;
          cWindH, cWindN, cWindE, ;
          cErrH, cErrN, cErrE

   // options on menu bar
   LOCAL aColors  := {}
   LOCAL aBar     := { " ENTER/EDIT ", " REPORTS ", " DISPLAY ", " MAINTENANCE ", " QUIT " }
   LOCAL aOptions[ LEN( aBar ) ]
   AEVAL( aBar, { |x,i| aOptions[i] := { {},{},{} } } )

   cCmdLine := iif( cCmdLine == NIL, "", cCmdLine )

   lColor := iif( "MONO" $ UPPER( cCmdLine ), .F., IsColor() )

   * Border, Box, Bar, Current, Unselected
   aColors := iif( lColor, {"W+/G", "N/G", "N/G", "N/W", "N+/G"}, ;
                           {"W+/N", "W+/N", "W/N", "N/W", "W/N"} )

   FT_FILL( aOptions[1], 'A. Execute A Dummy Procedure'        , {|| fubar()}, .t. )
   FT_FILL( aOptions[1], 'B. Enter Daily Charge/Credit Slips'  , {|| .t.}, .t. )
   FT_FILL( aOptions[1], 'C. Enter Payments On Accounts'       , {|| .t.}, .f. )
   FT_FILL( aOptions[1], 'D. Edit Daily Transactions'          , {|| .t.}, .t. )
   FT_FILL( aOptions[1], 'E. Enter/Update Member File'         , {|| .t.}, .t. )
   FT_FILL( aOptions[1], 'F. Update Code File'                 , {|| .t.}, .f. )
   FT_FILL( aOptions[1], 'G. Add/Update Auto Charge File'      , {|| .t.}, .t. )
   FT_FILL( aOptions[1], 'H. Post All Transactions To A/R File', {|| .t.}, .t. )
   FT_FILL( aOptions[1], 'I. Increment Next Posting Date'      , {|| .t.}, .t. )

   FT_FILL( aOptions[2], 'A. Print Member List'                , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'B. Print Active Auto Charges'        , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'C. Print Edit List'                  , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'D. Print Pro-Usage Report'           , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'E. Print A/R Transaction Report'     , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'F. Aging Report Preparation'         , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'G. Add Interest Charges'             , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'H. Print Aging Report'               , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'I. Print Monthly Statements'         , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'J. Print Mailing Labels'             , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'K. Print Transaction Totals'         , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'L. Print Transaction Codes File'     , {|| .t.}, .t. )
   FT_FILL( aOptions[2], 'M. Print No-Activity List'           , {|| .t.}, .t. )

   FT_FILL( aOptions[3], 'A. Transaction Totals Display'       , {|| .t.}, .t. )
   FT_FILL( aOptions[3], 'B. Display Invoice Totals'           , {|| .t.}, .t. )
   FT_FILL( aOptions[3], 'C. Accounts Receivable Display'      , {|| .t.}, .t. )

   FT_FILL( aOptions[4], 'A. Backup Database Files'            , {|| .t.}, .t. )
   FT_FILL( aOptions[4], 'B. Reindex Database Files'           , {|| .t.}, .t. )
   FT_FILL( aOptions[4], 'C. Set System Parameters'            , {|| .t.}, .t. )
   FT_FILL( aOptions[4], 'D. This EXITs Too'                   , {|| .f. }, .t. )

   FT_FILL( aOptions[5], 'A. Does Nothing'                     , {|| .t.}, .t. )
   FT_FILL( aOptions[5], 'B. Exit To DOS'                      , {|| .f. }, .t. )

   // main routine starts here
   SET SCOREBOARD OFF

   cNormH := iif( lColor, "W+/G", "W+/N" )
   cNormN := iif( lColor, "N/G" , "W/N"  )
   cNormE := iif( lColor, "N/W" , "N/W"  )
   cWindH := iif( lColor, "W+/B", "W+/N" )
   cWindN := iif( lColor, "W/B" , "W/N"  )
   cWindE := iif( lColor, "N/W" , "N/W"  )
   cErrH  := iif( lColor, "W+/R", "W+/N" )
   cErrN  := iif( lColor, "W/R" , "W/N"  )
   cErrE  := iif( lColor, "N/W" , "N/W"  )

   SAVE SCREEN TO sDosScrn
   nDosRow := ROW()
   nDosCol := COL()
   SETCOLOR( "w/n" )
   CLS
   NOSNOW( ( "NOSNOW" $ UPPER( cCmdLine ) ) )
   IF "VGA" $ UPPER( cCmdLine )
      SETMODE(50,80)
   ENDIF
   nMaxRow := MAXROW()
   SETBLINK(.f.)
   SETCOLOR( cWindN + "*" )
   CLEAR SCREEN
   SETCOLOR( cNormN )
   @ nMaxRow, 0
   @ nMaxRow, 0 SAY " FT_MENU1 1.0 ≥ "
   @ NMAXROW,16 SAY "WRITTEN BY PAUL FERRARA [76702,556] FOR NANFORUM.LIB"
   @ NMAXROW,69 SAY "≥ "+DTOC( DATE() )

   SETCOLOR( cErrH )
   @ nMaxRow-11, 23, nMaxRow-3, 56 BOX "⁄ƒø≥Ÿƒ¿≥ "
   @ nMaxRow- 9,23 SAY "√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
   SETCOLOR( cErrN )
   @ nMaxRow-10,33 SAY "Navigation Keys"
   @ nMaxRow- 8,25 SAY "LeftArrow   RightArrow   Alt-E"
   @ nMaxRow- 7,25 SAY "Home        End          Alt-R"
   @ nMaxRow- 6,25 SAY "Tab         Shift-Tab    Alt-D"
   @ nMaxRow- 5,25 SAY "PgUp        PgDn         Alt-M"
   @ nMaxRow- 4,25 SAY "Enter       ESCape       Alt-Q"
   SETCOLOR( cNormN )

   FT_MENU1( aBar, aOptions, aColors )

   SETCOLOR( "W/N" )
   SETCURSOR( SCNORMAL )
   SETBLINK(.t.)
   IF "VGA" $ UPPER( cCmdLine )
      SETMODE(25,80)
   ENDIF
   RESTORE SCREEN FROM sDosScrn
   SETPOS(nDosRow, nDosCol)
   QUIT

   FUNCTION fubar()
   LOCAL OldColor:= SETCOLOR( "W/N" )
   CLEAR SCREEN
   Qout( "Press Any Key" )
   INKEY(0)
   SETCOLOR( OldColor )
   RETURN .t.
#endif
// end of demo program

FUNCTION FT_MENU1( aBar, aOptions, aColors, nTopRow, lShadow )
   LOCAL nTtlUsed
   LOCAL sMainScrn, lCancMode, lLooping := .t.

   // column position for each item on the menu bar
   LOCAL aBarCol[LEN(aBar)]

   // inkey code for each item on menu bar
   LOCAL aBarKeys[ LEN( aBar ) ]

   // inkey codes for A - Z
   LOCAL aKeyCodes := { 286, 304, 302, 288, 274, 289, 290, 291, 279, ;
                        292, 293, 294, 306, 305, 280, 281, 272, 275, ;
                        287, 276, 278, 303, 273, 301, 277, 300 }

   // LEN() of widest array element for for each pulldown menu
   LOCAL aBarWidth[LEN(aBar)]

   // starting column for each box
   LOCAL aBoxLoc[LEN(aBar)]

   // last selection for each element
   LOCAL aLastSel[LEN(aBar)]

   // color memvars
   LOCAL cBorder  := aColors[1]
   LOCAL cBox     := aColors[2]
   LOCAL cBar     := aColors[3]
   LOCAL cCurrent := aColors[4]
   LOCAL cUnSelec := aColors[5]

   nMaxRow := MAXROW()
   nMaxCol := MAXCOL()

   // row for menu bar
   nTopRow := iif( nTopRow == NIL, 0, nTopRow )

   AFILL(aLastSel,1)
   aChoices := aOptions

   // this is the routine that calculates the position of each item
   // on the menu bar.

   aBarCol[1] := 0
   nTtlUsed := LEN( aBar[1] ) + 1
   AEVAL( aBar, ;
          {|x,i| HB_SYMBOL_UNUSED( x ), aBarcol[i]:= nTtlUsed,nTtlUsed+= (LEN(aBar[i]) +1 )}, ;
          2, LEN(aBar) -1 )

   // calculates widest element for each pulldown menu
   // see below for _ftWidest()
   AFILL(aBarWidth,1)
   AEVAL( aChoices, { |x,i| HB_SYMBOL_UNUSED( x ), _ftWidest( @i, aChoices, @aBarWidth ) } )

   // box location for each pulldown menu
   // see below for _ftLocat()
   AEVAL( aChoices, { |x,i| HB_SYMBOL_UNUSED( x ), _ftLocat( i, aBarCol, aBarWidth, @aBoxLoc, nMaxCol ) } )

   // valid keys for each pulldown menu
   // see below for _ftValKeys()
   AEVAL( aChoices,{|x,i| HB_SYMBOL_UNUSED( x ), AADD( aValidkeys,"" ),;
                          _ftValKeys( i,aChoices,@aValidKeys ) } )

   // display the menu bar
   SETCOLOR( cBar )
   @ nTopRow, 0
   AEVAL( aBar, { |x,i| HB_SYMBOL_UNUSED( x ), Devpos(nTopRow, aBarCol[i]), Devout(aBar[i]) })

   // store inkey code for each item on menu bar to aBarKeys
   AEVAL( aBarKeys, {|x,i| HB_SYMBOL_UNUSED( x ), aBarKeys[i] := ;
          aKeyCodes[ ASC( UPPER( LTRIM( aBar[i] ) ) ) - 64 ] } )

   // disable Alt-C and Alt-D
   lCancMode := SETCANCEL( .f. )
   AltD( DISABLE )

   // main menu loop
   SAVE SCREEN TO sMainScrn
   // which menu and which menu item
   nHpos := 1; nVpos := 1
   DO WHILE lLooping
      RESTORE SCREEN FROM sMainScrn
      SETCOLOR( cCurrent )
      @  nTopRow, aBarCol[nHpos] SAY aBar[nHpos]
      IF lShadow == NIL .OR. lShadow
         FT_SHADOW( nTopRow+1, aBoxLoc[nHpos], LEN(aChoices[nHpos,1])+nTopRow+2, aBarWidth[nHpos]+3+aBoxLoc[nHpos] )
      ENDIF
      SETCOLOR( cBorder )
      @  nTopRow+1, aBoxLoc[nHpos], LEN(aChoices[nHpos,1])+nTopRow+2, aBarWidth[nHpos]+3+aBoxLoc[nHpos] BOX "…Õª∫ºÕ»∫ "
      SETCOLOR( cBox +","+ cCurrent +",,,"+ cUnselec )
      nVpos := ACHOICE( nTopRow+2, aBoxLoc[nHpos]+2, LEN(aChoices[nHpos,1])+nTopRow+2, aBarWidth[nHpos]+1+aBoxLoc[nHpos], aChoices[nHpos,1], aChoices[nHpos,3], "__ftAcUdf", aLastSel[nHpos])
      DO CASE
      CASE LASTKEY() == RIGHTARROW .OR. LASTKEY() == TAB
         nHpos := iif( nHpos == LEN( aChoices ), 1, nHpos + 1 )
      CASE LASTKEY() == LEFTARROW .OR. LASTKEY() == SHIFTTAB
         nHpos := iif( nHpos == 1, LEN( aChoices ), nHpos - 1 )
      CASE LASTKEY() == ESCAPE
         lLooping := _ftBailOut( cBorder, cBox )
      CASE LASTKEY() == HOME
         nHpos := 1
      CASE LASTKEY() == END
         nHpos := LEN( aChoices )
      CASE LASTKEY() == ENTER
         aLastSel[nHpos] := nVpos
         IF aChoices[nHpos,2,nVpos] != NIL
            SETCANCEL( lCancMode )
            ALTD( ENABLE )
            lLooping := EVAL( aChoices[nHpos,2,nVpos] )
            ALTD( DISABLE )
            SETCANCEL( .f. )
         ENDIF
      CASE ASCAN( aBarKeys, LASTKEY() ) > 0
         nHpos := ASCAN( aBarKeys, LASTKEY() )
      ENDCASE
   ENDDO
   SETCANCEL( lCancMode )
   AltD( ENABLE )
   RESTORE SCREEN FROM sMainScrn
   RETURN NIL

FUNCTION __ftAcUdf( nMode )
   // ACHOICE() user function
   LOCAL nRtnVal := RESUME
   DO CASE
   CASE nMode == HITTOP
      KEYBOARD CHR( CTRLEND )
   CASE nMode == HITBOTTOM
      KEYBOARD CHR( CTRLHOME )
   CASE nMode == KEYEXCEPT
      IF UPPER( CHR( LASTKEY() ) ) $ aValidKeys[ nHpos ]
         IF aChoices[ nHpos, 3, AT( UPPER(CHR(LASTKEY())), aValidKeys[ nHpos ] )]
            KEYBOARD CHR( ENTER )
            nRtnVal := NEXTITEM
         ENDIF
      ELSE
         nRtnVal := MAKESELECT
      ENDIF
   ENDCASE
   RETURN nRtnVal

STATIC FUNCTION _ftWidest( i, aChoices, aBarWidth )
   AEVAL(aChoices[i,1],{|a,b| HB_SYMBOL_UNUSED( a ), aBarWidth[i] := ;
            MAX( aBarWidth[i],LEN(aChoices[i,1,b])) })
   RETURN NIL

STATIC FUNCTION _ftLocat( i, aBarCol, aBarWidth, aBoxLoc, nMaxCol )
   aBoxLoc[i] := iif( aBarCol[i] + aBarWidth[i] + 4 > nMaxCol + 1, ;
                 nMaxCol - 3 - aBarWidth[i], aBarCol[i] )
   RETURN NIL

STATIC FUNCTION _ftBailOut( cBorder, cBox )
   LOCAL cOldColor, sOldScreen, nKeyPress, nOldCursor
   nOldCursor := SETCURSOR( SCNONE )
   sOldScreen := SAVESCREEN(nMaxRow/2-1, 24, nMaxRow/2+2, 55)
   cOldColor := SETCOLOR( cBorder )
   FT_SHADOW( nMaxRow/2-1, 24, nMaxRow/2+2, 55 )
   @ nMaxRow/2-1, 24, nMaxRow/2+2, 55 BOX "…Õª∫ºÕ»∫ "
   SETCOLOR( cBox )
   @ nMaxRow/2,  26 SAY "Press ESCape To Confirm Exit"
   @ nMaxRow/2+1,27 SAY "Or Any Other Key To Resume"
   nKeyPress := INKEY(0)
   SETCOLOR( cOldColor )
   RESTSCREEN(nMaxRow/2-1, 24, nMaxRow/2+2, 55,sOldScreen )
   SETCURSOR( nOldCursor )
   RETURN !(nKeyPress == ESCAPE)

STATIC FUNCTION _ftValKeys( nNum,aChoices,aValidkeys )
   AEVAL( aChoices[nNum,1], {|x| aValidKeys[nNum] += LEFT( x, 1)} )
   RETURN NIL

FUNCTION FT_FILL( aArray, cMenuOption, bBlock, lAvailable )
   AADD( aArray[1], cMenuOption )
   AADD( aArray[2], bBlock )
   AADD( aArray[3], lAvailable )
   RETURN NIL
