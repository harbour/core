/*
 * File......: Popadder.prg
 * Author....: Keith A. Wire
 * CIS ID....: 73760,2427
 *
 * This is an original work by Keith A. Wire and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   03 Mar 1994 19:47:22   GLENN
 * Author made some enhancements and modifications.
 *
 *    Rev 1.3   19 Jan 1993 19:52:52   GLENN
 * Removed reference to K_SPACE, as this has been defined in Clipper
 * 5.2's INKEY.CH.
 *
 *    Rev 1.2   17 Aug 1991 15:44:30   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.1   15 Aug 1991 23:04:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   14 Jun 1991 17:37:54   GLENN
 * Initial revision.
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *      FT_Adder()
 *  $CATEGORY$
 *      Menus/Prompts
 *  $ONELINER$
 *      Pop up a simple calculator
 *  $SYNTAX$
 *      FT_Adder()
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      NIL .... but optionally places Total of calculation in active
 *               Get variable using oGet:VARPUT()
 *  $DESCRIPTION$
 *      PopAdder() gives you an adding machine inside your Clipper 5.2
 *      application. It has the basic functions add, subtract, multiply,
 *      and divide. You may move it from one side of the screen to the
 *      other. It even displays a scrollable tape, if you want it.
 *
 *
 *      There are a few HOT Keys while using the Adder:
 *
 *             <D>ecimals Ä change # of decimals
 *             <M>ove     Ä the Adder from right display to left
 *             <T>ape     Ä turn the Tape Display On or Off
 *             <S>croll   Ä the tape display
 *             <DEL> ÄÄÄÂÄÄ 1st Clear entry
 *                      ÀÄÄ 2nd Clear ADDER
 *             <ESC>      Ä Quit
 *             <F10>      Ä return a <TOTAL> to the active get
 *
 *
 *      A couple of notes about the adder:
 *
 *
 *      1.) It was designed to be used on an Enhanced keyboard with
 *          separate <DELETE> key. <DELETE> is used to clear the adder.
 *          However, it will still work on a Standard keyboard.
 *
 *      2.) You do not have to display the tape. You may turn it on
 *          at any time by pressing <T>. You may SCROLL back through
 *          the tape once there are more than 16 entries in the
 *          adder, by pressing <S>.
 *
 *      3.) To Quit the Adder just press <ESC>. To return your Total
 *          to the application press <F10>. The adder will place the
 *          Total in the active GET variable using oGet:VarPut(). The
 *          adder will only return a Total to a numerical GET!
 *
 *      4.) There are many support functions that you might find
 *          interesting. They are part of my personal library, but
 *          are necessary to the operation of the adder.
 *          You might want to pull these out to reduce the overall
 *          size of the adder. Many are worth at least a little
 *          time studying.
 *
 *      5.) To make FT_Adder a Hot key from inside your application
 *          at the beginning of your application add the line:
 *
 *                 SET KEY K_ALT_A  TO FT_Adder
 *
 *          This will make <ALT-A> a key "Hot" and permit you to
 *          Pop - Up the adder from anywhere in the application.
 *
 *      6.) If you use FT_INKEY(), you can even have active hotkeys
 *          in an INKEY().
 *
 *
 *
 *
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *     INKEY.CH, SETCURS.CH, ACHOICE.CH
 *  $END$
 */

#include 'inkey.ch'
#include 'setcurs.ch'
#include 'achoice.ch'

// Set up manifest constants to access the window colors in the array aWinColor
#define W_BORDER 1
#define W_ACCENT 2
#define W_PROMPT 3
#define W_SCREEN 4
#define W_TITLE  5
#define W_VARIAB 6
#define W_CURR   NIL

// Set up manifest constants to access the Standard screen colors in the array
//   aStdColor
#define STD_ACCENT   1
#define STD_ERROR    2
#define STD_PROMPT   3
#define STD_SCREEN   4
#define STD_TITLE    5
#define STD_VARIABLE 6
#define STD_BORDER   7

#define K_DECIM    46
#define K_EQUAL    13
#define K_PLUS     43
#define K_MINUS    45
#define K_MULTIPLY 42
#define K_DIVIDE   47
#define K_ZERO     48
#define B_DOUBLE 'ÉÍ»º¼ÍÈº '
#define B_SINGLE 'ÚÄ¿³ÙÄÀ³ '

#define CRLF CHR(13)+CHR(10)
#define nTotTran LEN(aTrans)

#command DEFAULT <p> TO <val> [,<pn> TO <valn>] =>         ;
                 <p>    := IF(<p>  == NIL, <val>,  <p>)    ;
                 [;<pn> := IF(<pn> == NIL, <valn>, <pn>)]

#command DISPMESSAGE <mess>,<t>,<l>,<b>,<r> =>                        ;
         _ftPushKeys(); KEYBOARD CHR(K_CTRL_PGDN)+CHR(K_CTRL_W)      ;;
         MEMOEDIT(<mess>,<t>,<l>,<b>,<r>, .F., NIL, (<r>)-(<l>)+1)   ;;
         _ftPopKeys()

#define ASHRINK(ar) ASIZE(ar,LEN(ar)-1)

/* This INKEY UDC was posted by Don Caton on NanForum... Thanks Don <g> */
#command FT_INKEY [ <secs> ] TO <var>                                    ;
         =>                                                              ;
         WHILE (.T.)                                                    ;;
            <var> := Inkey([ <secs> ])                                  ;;
            IF Setkey(<var>) # NIL                                      ;;
               Eval( Setkey(<var>), ProcName(), ProcLine(), #<var> )    ;;
            ELSE                                                        ;;
               EXIT                                                     ;;
            END                                                         ;;
         END


// Instead of using STATIC variables for these I'm using a LOCAL array
//   and passing aAdder[] all over the place.... Don't let this confuse
//   you. I wrote the Adder using the variable names & now let the
//   PreProcessor do all the work.
#define nTotal     aAdder[1]
#define nNumTotal  aAdder[2]
#define nSavTotal  aAdder[3]
#define cTotPict   aAdder[4]
#define lClAdder   aAdder[5]
#define lDecSet    aAdder[6]
#define nDecDigit  aAdder[7]
#define nMaxDeci   aAdder[8]
#define lMultDiv   aAdder[9]
#define nAddMode   aAdder[10]
#define lSubRtn    aAdder[11]
#define lTotalOk   aAdder[12]
#define lAddError  aAdder[13]
#define lTape      aAdder[14]
#define lNewNum    aAdder[15]
#define nSavSubTot aAdder[16]
#define lDivError  aAdder[17]
#define aTrans     aAdder[18]
#define nTopOS     aAdder[19]
#define nLeftOS    aAdder[20]
#define nAddSpace  aAdder[21]
#define nTapeSpace aAdder[22]
#define cTapeScr   aAdder[23]


// I still use a few of STATICS, but most are set to NIL when quiting...
STATIC lAdderOpen := .F.,                                                    ;
       aKeys, aWindow, nWinColor, aWinColor, aStdColor

#ifdef FT_TEST

  FUNCTION TEST

    LOCAL nSickHrs := 0,                                                     ;
          nPersHrs := 0,                                                     ;
          nVacaHrs := 0,                                                     ;
          GetList  := {}

    SET SCOREBOARD OFF
    _ftSetScrColor(STD_SCREEN,STD_VARIABLE)
    CLEAR SCREEN

    SET KEY K_ALT_A  TO FT_Adder        // Make <ALT-A> call FT_Adder

    * SIMPLE Sample of program data entry!

    @ 12,5 SAY 'Please enter the total Sick, Personal, and Vacation hours.'
    @ 15,22 SAY 'Sick hrs.'
    @ 15,40 SAY 'Pers. hrs.'
    @ 15,60 SAY 'Vaca. hrs.'
    @ 23,20 SAY 'Press <ALT-A> to Pop - Up the Adder.'
    @ 24,20 SAY 'Press <ESC> to Quit the adder Demo.'
    DO WHILE .T.                               // Get the sick, personal, & vaca
      @ 16,24 GET nSickHrs PICTURE '9999.999'  // Normally I have a VALID()
      @ 16,43 GET nPersHrs PICTURE '9999.999'  // to make sure the value is
      @ 16,63 GET nVacaHrs PICTURE '9999.999'  // within the allowable range.
      SET CURSOR ON                            // But, like I said it is a
      CLEAR TYPEAHEAD                          // SIMPLE example <g>.
      READ
      SET CURSOR OFF
      IF LASTKEY() == K_ESC                    // <ESC> - ABORT
        CLEAR TYPEAHEAD
        EXIT
      ENDIF
    ENDDO
    SET CURSOR ON

    SET KEY K_ALT_A                     // Reset <ALT-A>

  RETURN NIL
#endif



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: FT_Adder()            Docs: Keith A. Wire                  ³
  ³  Description: Pop Up Adder / Calculator with Tape Display                ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 11:18:40am            Time updated: ş11:18:40am            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: NIL                                                        ³
  ³        Notes: To make FT_Adder() pop up from any wait state in your      ³
  ³             : application just insert the line:                          ³
  ³             :   SET KEY K_ALT_A  TO FT_Adder                             ³
  ³             : at the top of your application                             ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
FUNCTION FT_Adder()

  LOCAL nOldDecim, cMoveTotSubTot, cTotal, lDone, nKey,                      ;
        oGet        := GetActive(),                                          ;
        nOldCurs    := SETCURSOR(SC_NONE),                                   ;
        nOldRow     := ROW(),                                                ;
        nOldCol     := COL(),                                                ;
        bOldF10     := SETKEY(K_F10, NIL),                                   ;
        nOldLastKey := LASTKEY(),                                            ;
        lShowRight  := .T.,                                                  ;
        aAdder      := ARRAY(23)

  // Must prevent recursive calls
  IF lAdderOpen
    RETURN NIL
  ELSE
    lAdderOpen := .T.
  ENDIF

  aTrans       := {'                  0.00 C '}
  nOldDecim    := SET(_SET_DECIMALS,9)
  cTotPict     := '999999999999999.99'
  cTapeScr     := ''
  nTotal       := nNumTotal := nSavTotal := nDecDigit := 0
  lDone        := .F.                   // Loop flag
  nKey         := 0
  nMaxDeci     := 2                     // Initial # of decimals
  nSavSubTot   := 0
  lNewNum      := .F.
  nAddMode     := 1                     // Start in ADD mode
  lMultDiv     := .F.                   // Start in ADD mode
  lClAdder     := .F.                   // Clear adder flag
  lDecSet      := .F.                   // Decimal ? - keyboard routine
  lSubRtn      := lTotalOk := lTape := lAddError := lDivError := .F.

  nTopOS       := INT((MAXROW()-24)/2)  // Using the TopOffSet & LeftOffSet
  nLeftOS      := INT((MAXCOL()-79)/2)  // the Adder will always be centered
  nAddSpace    := IF(lShowRight,40,0)+nLeftOS
  nTapeSpace   := IF(lShowRight,0,40)+nLeftOS

  // Set Up the STATIC variables
  aKeys      := {}
  aWindow    := {}
  nWinColor  := 0

  _ftAddScreen(aAdder)

  // Set the decimals to 2 & display a cleared adder
  _ftChangeDec(aAdder, 2)
  @ 4+nTopOS, 7+nAddSpace SAY nTotal PICTURE cTotPict

  DO WHILE ! lDone                      // Input key & test loop
    FT_INKEY 0 TO nKey
    DO CASE
      CASE UPPER(CHR(nKey)) $'1234567890.'
        _ftProcessNumb(aAdder, nKey)
      CASE nKey == K_PLUS               // <+> sign
        _ftAddSub(aAdder, nKey)
      CASE nKey == K_MINUS              // <-> sign
        _ftAddSub(aAdder, nKey)
      CASE nKey == K_MULTIPLY           // <*> sign
        _ftMultDiv(aAdder, nKey)
      CASE nKey == K_DIVIDE             // </> sign
        _ftMultDiv(aAdder, nKey)
      CASE nKey == K_RETURN             // <RTN> Total or Subtotal
        _ftAddTotal(aAdder)
      CASE nKey == K_ESC                // <ESC> Quit
        SET(_SET_DECIMALS,nOldDecim)
        SETCURSOR(nOldCurs)
        IF lTape
          RESTSCREEN(4+nTopOS,6+nTapeSpace,22+nTopOS,35+nTapeSpace,cTapeScr)
        ENDIF
        _ftPopWin()
        SETPOS(nOldRow,nOldCol)
        _ftSetLastKey(nOldLastKey)
        SETKEY(K_F10, bOldF10)
        lAdderOpen := .F.               // Reset the recursive flag
        lDone      := .T.
      CASE nKey == 68 .OR. nKey == 100  // <D> Change number of decimal places
        _ftChangeDec(aAdder)
      CASE nKey == 84 .OR. nKey == 116  // <T> Display Tape
        _ftDisplayTape(aAdder, nKey)
      CASE nKey == 77 .OR. nKey == 109  // <M> Move Adder
        IF lTape
          RESTSCREEN(4+nTopOS,6+nTapeSpace,22+nTopOS,35+nTapeSpace,cTapeScr)
        ENDIF
        IF LEFT(SAVESCREEN(6+nTopOS,26+nAddSpace,6+nTopOS,27+nAddSpace),1)   ;
              != ' '
          IF LEFT(SAVESCREEN(6+nTopOS,19+nAddSpace,6+nTopOS,20+nAddSpace),1) ;
              == 'S'
            cMoveTotSubTot := 'S'
          ELSE
            cMoveTotSubTot := 'T'
          ENDIF
        ELSE
          cMoveTotSubTot := ' '
        ENDIF
        cTotal := _ftCharOdd(SAVESCREEN( 4 + nTopOS, 8 + nAddSpace, 4 +      ;
                             nTopOS,25+nAddSpace))
        _ftPopWin()                     // Remove Adder
        lShowRight := !lShowRight
        nAddSpace  := IF(lShowRight,40,0)+nLeftOS
        nTapeSpace := IF(lShowRight,0,40)+nLeftOS
        _ftAddScreen(aAdder)
        _ftDispTotal(aAdder)
        IF lTape
          lTape := .F.
          _ftDisplayTape(aAdder, nKey)
        ENDIF
        @ 4+nTopOS, 8+nAddSpace SAY cTotal
        IF !EMPTY(cMoveTotSubTot)
          _ftSetWinColor(W_CURR,W_SCREEN)
          @ 6+nTopOS,18+nAddSpace SAY IF(cMoveTotSubTot=='T', '   <TOTAL>',  ;
                                                             '<SUBTOTAL>')
          _ftSetWinColor(W_CURR,W_PROMPT)
        ENDIF
      CASE (nKey == 83 .OR. nKey == 115) .AND. lTape  // <S> Scroll tape display
        IF nTotTran>16                  // We need to scroll
          SETCOLOR('GR+/W')
          @ 21+nTopOS,8+nTapeSpace SAY ' '+CHR(24)+CHR(25)+'-SCROLL  <ESC>-QUIT '
          SETCOLOR('N/W,W+/N')
          ACHOICE(5+nTopOS,7+nTapeSpace,20+nTopOS,32+nTapeSpace,aTrans,.T.,  ;
                  '_ftAdderTapeUDF',nTotTran,20)
          SETCOLOR('R+/W')
          @ 21+nTopOS,8+nTapeSpace TO 21+nTopOS,30+nTapeSpace
          _ftSetWinColor(W_CURR,W_PROMPT)
          CLEAR TYPEAHEAD
        ELSE
          _ftError('there are ' + IF(nTotTran > 0, 'only ' +                 ;
                   LTRIM(STR(nTotTran, 3, 0)), 'no') +                       ;
                   ' transactions entered so far.'   +                       ;
                   ' No need to scroll!')
        ENDIF
      CASE nKey == 7                    // Delete - Clear adder
        _ftClearAdder(aAdder)
      CASE nKey == K_F1                 // <F1> Help
        _ftAddHelp()
      CASE nKey == K_F10                // <F10> Quit - Return total
        IF lTotalOk                     // Did they finish the calculation
          IF oGet != NIL .AND. oGet:TYPE == 'N'
            SET(_SET_DECIMALS,nOldDecim)
            SETCURSOR(nOldCurs)
            IF lTape
              RESTSCREEN(4+nTopOS,6+nTapeSpace,22+nTopOS,35+nTapeSpace,cTapeScr)
            ENDIF
            _ftPopWin()
            SETPOS(nOldRow,nOldCol)
            _ftSetLastKey(nOldLastKey)
            SETKEY(K_F10, bOldF10)
            oGet:VARPUT(nSavTotal)
            lAdderOpen := .F.           // Reset the recursive flag
            lDone      := .T.
          ELSE
            _ftError('but I can not return the total from the '+             ;
                    'adder to this variable. You must quit the adder using'+ ;
                    ' the <ESC> key and then enter the total manually.')
          ENDIF
        ELSE
          _ftError('the calculation is not finished yet! You must have'+     ;
                  ' a TOTAL before you can return it to the program.')
        ENDIF
    ENDCASE
  ENDDO  (WHILE .T.  Data entry from keyboard)

// Reset the STATICS to NIL
aKeys := aWindow := aWinColor := aStdColor := NIL

RETURN NIL



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftAddScreen()        Docs: Keith A. Wire                  ³
  ³  Description: Display the Adder                                          ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 11:24:29am            Time updated: ş11:24:29am            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftAddScreen(aAdder)
  LOCAL nCol
  _ftPushWin(2+nTopOS,2+nAddSpace,22+nTopOS,30+nAddSpace,'   Adder   ',      ;
          '<F-1> for Help',,B_DOUBLE)
  nCol := 5+nAddSpace
  @  7+nTopOS, nCol SAY '      ÚÄÄÄ¿ ÚÄÄÄ¿ ÚÄÄÄ¿'
  @  8+nTopOS, nCol SAY '      ³   ³ ³   ³ ³   ³'
  @  9+nTopOS, nCol SAY '      ÀÄÄÄÙ ÀÄÄÄÙ ÀÄÄÄÙ'
  @ 10+nTopOS, nCol SAY 'ÚÄÄÄ¿ ÚÄÄÄ¿ ÚÄÄÄ¿ ÚÄÄÄ¿'
  @ 11+nTopOS, nCol SAY '³   ³ ³   ³ ³   ³ ³   ³'
  @ 12+nTopOS, nCol SAY 'ÀÄÄÄÙ ÀÄÄÄÙ ÀÄÄÄÙ ³   ³'
  @ 13+nTopOS, nCol SAY 'ÚÄÄÄ¿ ÚÄÄÄ¿ ÚÄÄÄ¿ ³   ³'
  @ 14+nTopOS, nCol SAY '³   ³ ³   ³ ³   ³ ³   ³'
  @ 15+nTopOS, nCol SAY 'ÀÄÄÄÙ ÀÄÄÄÙ ÀÄÄÄÙ ÀÄÄÄÙ'
  @ 16+nTopOS, nCol SAY 'ÚÄÄÄ¿ ÚÄÄÄ¿ ÚÄÄÄ¿ ÚÄÄÄ¿'
  @ 17+nTopOS, nCol SAY '³   ³ ³   ³ ³   ³ ³   ³'
  @ 18+nTopOS, nCol SAY 'ÀÄÄÄÙ ÀÄÄÄÙ ÀÄÄÄÙ ³   ³'
  @ 19+nTopOS, nCol SAY 'ÚÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄ¿ ³   ³'
  @ 20+nTopOS, nCol SAY '³         ³ ³   ³ ³   ³'
  @ 21+nTopOS, nCol SAY 'ÀÄÄÄÄÄÄÄÄÄÙ ÀÄÄÄÙ ÀÄÄÄÙ'
  _ftSetWinColor(W_CURR,W_TITLE)
  nCol := 7+nAddSpace
  @ 11+nTopOS, nCol SAY '7'
  @ 14+nTopOS, nCol SAY '4'
  @ 17+nTopOS, nCol SAY '1'
  nCol := 13+nAddSpace
  @  8+nTopOS,nCol SAY '/'
  @ 11+nTopOS,nCol SAY '8'
  @ 14+nTopOS,nCol SAY '5'
  @ 17+nTopOS,nCol SAY '2'
  nCol := 19+nAddSpace
  @  8+nTopOS,nCol SAY 'X'
  @ 11+nTopOS,nCol SAY '9'
  @ 14+nTopOS,nCol SAY '6'
  @ 17+nTopOS,nCol SAY '3'
  @ 20+nTopOS,nCol SAY '.'
  @ 20+nTopOS,10+nAddSpace SAY '0'
  nCol := 25+nAddSpace
  @  8+nTopOS,nCol SAY '-'
  @ 13+nTopOS,nCol SAY '+'
  @ 18+nTopOS,nCol SAY '='
  @ 19+nTopOS,nCol SAY ''
  _ftSetWinColor(W_CURR,W_PROMPT)
  @ 3+nTopOS, 6+nAddSpace, 5+nTopOS, 27+nAddSpace BOX B_DOUBLE
RETURN NIL



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftChangeDec()        Docs: Keith A. Wire                  ³
  ³  Description: Change the decimal position in the display                 ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 11:25:17am            Time updated: ş11:25:17am            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³             : nNumDec                                                    ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftChangeDec(aAdder, nNumDec)

  LOCAL cDefTotPict  := '9999999999999999999'

  IF nNumDec == NIL
    nNumDec := 0

    nNumDec := _ftQuest('How many decimals do you want to display?',         ;
                        nNumDec, '9', {|oGet| _ftValDeci(oGet)})

    cTotPict := _ftPosRepl(cDefTotPict, '.', 19 - ABS(nNumDec))

    cTotPict := RIGHT(_ftStuffComma(cTotPict), 19 )
    cTotPict := IIF(nNumDec==2 .OR. nNumDec==6, ' '+RIGHT(cTotPict,18),cTotPict)

    nMaxDeci := nNumDec

    IF lSubRtn
      _ftDispTotal(aAdder)
    ELSE
      _ftDispSubTot(aAdder)
    ENDIF

  ENDIF

RETURN NIL



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftDispTotal()        Docs: Keith A. Wire                  ³
  ³  Description: Display total number to Adder Window                       ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 11:25:58am            Time updated: ş11:25:58am            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftDispTotal(aAdder)

  LOCAL cTotStr

  IF nTotal>VAL(_ftCharRem(',',cTotPict))
    cTotStr   := _ftStuffComma(LTRIM(STR(nTotal)))
    @ 4+nTopOS, 8+nAddSpace SAY '****  ERROR  **** '
    _ftError('that number is to big to display! I believe the answer was ' + ;
              cTotStr+'.')
    lAddError := .T.
    _ftUpdateTrans(aAdder, .T., NIL)
    _ftClearAdder(aAdder)
    nTotal    := 0
    nNumTotal := 0
    lAddError := .F.
  ELSE
    @ 4+nTopOS, 7+nAddSpace SAY nTotal PICTURE cTotPict
  ENDIF

RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftDispSubTot()       Docs: Keith A. Wire                  ³
  ³  Description: Display subtotal number                                    ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 11:26:31am            Time updated: ş11:26:31am            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftDispSubTot(aAdder)

  LOCAL cStotStr

  IF nNumTotal>VAL(_ftCharRem(',',cTotPict))
    cStotStr  := _ftStuffComma(LTRIM(STR(nNumTotal)))
    @ 4+nTopOS, 8+nAddSpace SAY '****  ERROR  **** '
    _ftError('that number is to big to display! I believe the answer was ' + ;
              cStotStr+'.')
    lAddError := .T.
    _ftUpdateTrans(aAdder, .T.,nNumTotal)
    _ftClearAdder(aAdder)
    nTotal    := 0
    nNumTotal := 0
    lAddError := .F.
  ELSE
    @ 4+nTopOS, 7+nAddSpace SAY nNumTotal PICTURE cTotPict
  ENDIF
RETURN NIL



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftProcessNumb()      Docs: Keith A. Wire                  ³
  ³  Description: Act on NUMBER key pressed                                  ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 11:38:34am            Time updated: ş11:38:34am            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³             : nKey                                                       ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftProcessNumb(aAdder, nKey)
  LOCAL nNum
  _ftEraseTotSubTot(aAdder)
  lTotalOk  := .F.
  lClAdder  := .F.                      // Reset the Clear flag
  lAddError := .F.                      // Reset adder error flag

  IF nKey=46                            // Period (.) decimal point
    IF lDecSet                          // Has decimal already been set
      TONE(800, 1)
    ELSE
      lDecSet := .T.
    ENDIF
  ELSE                                  // It must be a number input
    lNewNum := .T.
    nNum := nKey-48
    IF lDecSet                          // Decimal set
      IF nDecDigit<nMaxDeci             // Check how many decimals are allowed
        nDecDigit := ++nDecDigit
        nNumTotal := nNumTotal+nNum/(10**nDecDigit)
      ENDIF
    ELSE
      nNumTotal := nNumTotal*10+nNum
    ENDIF
  ENDIF

  _ftDispSubTot(aAdder)

RETURN NIL





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftAddTotal()         Docs: Keith A. Wire                  ³
  ³  Description: Enter key - SUBTOTAL\TOTAL                                 ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:05:29pm            Time updated: ş12:05:29pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftAddTotal(aAdder)
  _ftEraseTotSubTot(aAdder)
  lDecSet   := .F.
  nDecDigit :=  0
  lClAdder  := .F.                      // Reset the Clear flag
  IF lSubRtn                            // If this was the second time they
    IF !lMultDiv
      _ftSetWinColor(W_CURR,W_SCREEN)
      @ 6+nTopOS, 18+nAddSpace SAY '   <TOTAL>'
      _ftSetWinColor(W_CURR,W_PROMPT)
      _ftUpdateTrans(aAdder, .T., NIL)
      _ftDispTotal(aAdder)
      lSubRtn   := .F.                  // pressed the total key reset everyting
      nSavTotal := nTotal
      nTotal    := 0
      lTotalOk  := .T.
    ENDIF
  ELSE                                  // This was the first time they pressed
    IF !lMultDiv .AND. LASTKEY() == K_RETURN // total key
      lSubRtn := .T.
    ENDIF
    IF _ftRoundIt(nTotal,nMaxDeci)!=0 .OR. _ftRoundIt(nNumTotal,nMaxDeci)!=0
      IF !lMultDiv
        _ftSetWinColor(W_CURR,W_SCREEN)
        @ 6+nTopOS, 18+nAddSpace SAY '<SUBTOTAL>'
        _ftSetWinColor(W_CURR,W_PROMPT)
      ENDIF
      IF _ftRoundIt(nNumTotal,nMaxDeci)!=0
        lSubRtn := .F.
        _ftUpdateTrans(aAdder, .F.,nNumTotal)
      ENDIF
      IF !lMultDiv
        lSubRtn := .T.                  // total key
      ENDIF
      IF nAddMode == 1                  // Add
        nTotal := nTotal+nNumTotal
      ELSEIF nAddMode == 2              // Subtract
        nTotal := nTotal-nNumTotal
      ELSEIF nAddMode == 3              // Multiply
        nTotal := nTotal*nNumTotal
      ELSEIF nAddMode == 4              // Divide
        nTotal := _ftDivide(aAdder, nTotal,nNumTotal)
        IF lDivError
          _ftError("you can't divide by ZERO!")
          lDivError := .F.
        ENDIF
      ENDIF
    ENDIF
    _ftDispTotal(aAdder)
    IF lMultDiv                         // This was a multiply or divide
      _ftSetWinColor(W_CURR,W_SCREEN)
      @ 6+nTopOS, 18+nAddSpace SAY '   <TOTAL>'
      _ftSetWinColor(W_CURR,W_PROMPT)
      lSubRtn := .F.                    // pressed total so key reset everything
      IF !lTotalOk                      // If you haven't printed total DO-IT
        lTotalOk := .T.
        _ftUpdateTrans(aAdder, .F., NIL)
      ENDIF
      nNumTotal := 0
      nSavTotal := nTotal
      nTotal    := 0
    ELSE
      IF !lTotalOk                      // If you haven't printed total DO-IT
        _ftUpdateTrans(aAdder, .F., NIL)
        nNumTotal := 0
      ENDIF
    ENDIF
  ENDIF

RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftAddSub()           Docs: Keith A. Wire                  ³
  ³  Description: Process + or - keypress                                    ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:06:13pm            Time updated: ş12:06:13pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³             : nKey                                                       ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftAddSub(aAdder, nKey)

  lMultDiv  := .F.
  _ftEraseTotSubTot(aAdder)
  lTotalOk  := .F.
  lDecSet   := .F.
  nDecDigit := 0
  lSubRtn   := .F.
  // They pressed the + or - key to process the previous total
  IF _ftRoundIt(nNumTotal,nMaxDeci)==0 .AND. _ftRoundIt(nTotal,nMaxDeci)==0
    nNumTotal := nSavTotal
    lNewNum   := .T.
  ENDIF
  IF nKey == K_PLUS                     // Add
    nAddMode := 1
    IF !lNewNum                         // They pressed + again to add the same
      nNumTotal := nSavSubTot           // number without re-entering
    ENDIF
    _ftUpdateTrans(aAdder, .F.,nNumTotal)
    nTotal     := nTotal+nNumTotal
    lNewNum    := .F.
    nSavSubTot := nNumTotal   // Save this number in case they just press + or -
    nNumTotal  := 0
  ELSEIF nKey == K_MINUS                // Subtract
    nAddMode := 2
    IF !lNewNum                         // They pressed + again to add the same
      nNumTotal := nSavSubTot           // number without re-entering
      lNewNum   := .T.
    ENDIF
    _ftUpdateTrans(aAdder, .F.,nNumTotal)
    nTotal     := nTotal-nNumTotal
    lNewNum    := .F.
    nSavSubTot := nNumTotal   // Save this number in case they just press + or -
    nNumTotal  := 0
  ENDIF

  _ftDispTotal(aAdder)

RETURN NIL





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftMultDiv()          Docs: Keith A. Wire                  ³
  ³  Description: Process * or / keypress                                    ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:06:43pm            Time updated: ş12:06:43pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³             : nKey                                                       ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftMultDiv(aAdder, nKey)

  lMultDiv  := .T.
  _ftEraseTotSubTot(aAdder)
  lTotalOk  := .F.
  lDecSet   := .F.
  nDecDigit := 0
  lSubRtn   := .F.
  // They pressed the + or - key to process the previous total
  IF _ftRoundIt(nNumTotal,nMaxDeci)==0 .AND. _ftRoundIt(nTotal,nMaxDeci)==0
    nNumTotal := nSavTotal
  ENDIF
  // Get the first number of the product or division
  IF _ftRoundIt(nTotal,nMaxDeci)==0
    IF nKey == K_MULTIPLY               // Setup mode
      nAddMode := 3
      _ftUpdateTrans(aAdder, .F.,nNumTotal)
    ELSEIF nKey == K_DIVIDE
      nAddMode := 4
      _ftUpdateTrans(aAdder, .F.,nNumTotal)
    ENDIF
    nTotal    := nNumTotal
    nNumTotal := 0
  ELSE
    IF nKey == K_MULTIPLY               // Multiply
      nAddMode  := 3
      _ftUpdateTrans(aAdder, .F.,nNumTotal)
      nTotal    := nTotal*nNumTotal
      nNumTotal := 0
    ELSEIF nKey == K_MULTIPLY           // Divide
      nAddMode := 4
      _ftUpdateTrans(aAdder, .F.,nNumTotal)
      nTotal:=_ftDivide(aAdder, nTotal,nNumTotal)
      IF lDivError
        _ftError("you can't divide by ZERO!")
        lDivError := .F.
      ENDIF
      nNumTotal := 0
    ENDIF
  ENDIF

  _ftDispTotal(aAdder)

RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftAddHelp            Docs: Keith A. Wire                  ³
  ³  Description: Help window                                                ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:07:07pm            Time updated: ş12:07:07pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftAddHelp

  LOCAL cMess := 'This Adder works like a desk top calculator. You may add,'+;
                 ' subtract, multiply, or divide. '           + CRLF + CRLF +;
                 'When adding or subtracting, the first entry is entered '  +;
                 'into the accumulator and each sucessive entry is '        +;
                 'subtotaled. When you press <ENTER> the SubTotal is also ' +;
                 'shown on the tape. The second time you press <ENTER> the '+;
                 'adder is Totaled. When multiplying or dividing the '      +;
                 '<ENTER> is a Total the first time pressed.' + CRLF + CRLF +;
                 'Hot Keys:'                                           +CRLF+;
                 '         <D>ecimals Ä change # of decimals'          +CRLF+;
                 '         <M>ove     Ä the Adder from right to left'  +CRLF+;
                 '         <T>ape     Ä turn Tape Display On or Off'   +CRLF+;
                 '         <S>croll   Ä the tape display'       + CRLF +CRLF+;
                 '         <DEL> ÄÄÄÂÄÄ 1st Clear entry'               +CRLF+;
                 '                  ÀÄÄ 2nd Clear ADDER'               +CRLF+;
                 '         <ESC>      Ä Quit'                          +CRLF+;
                 '         <F10>      Ä return a <TOTAL> to the active get'

   _ftPushMessage(cMess, .T., 'ADDER HELP', 'press any key to continue...',  ;
                  'QUIET')


RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftClearAdder()       Docs: Keith A. Wire                  ³
  ³  Description: Clear entry / Clear Adder                                  ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:07:33pm            Time updated: ş12:07:33pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftClearAdder(aAdder)

  _ftEraseTotSubTot(aAdder)
  lDecSet   := .F.
  nDecDigit := 0
  IF lClAdder                           // If it has alredy been pressed once
    nTotal    := 0                      // then we are clearing the total
    nSavTotal := 0
    _ftUpdateTrans(aAdder, .F., NIL)
    lClAdder  := .F.
    _ftDispTotal(aAdder)
  ELSE
    nNumTotal := 0                      // Just clearing the last entry
    lClAdder  := .T.
    _ftDispSubTot(aAdder)
  ENDIF
RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftUpdateTrans()      Docs: Keith A. Wire                  ³
  ³  Description: Update transactions array                                  ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:07:55pm            Time updated: ş12:07:55pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³             : lTypeTotal                                                 ³
  ³             : nAmount                                                    ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftUpdateTrans(aAdder, lTypeTotal, nAmount)

  LOCAL lUseTotal := (nAmount == NIL)

  nAmount := IF(nAmount==NIL,0,nAmount)
  IF lClAdder                     // Clear the adder (they pressed <DEL> twice
    AADD(aTrans,STR(0,22,nMaxDeci)+' C')
    IF lTape                            // If there is a tape Show Clear
      _ftDisplayTape(aAdder)
    ENDIF
    RETU NIL
  ENDIF

  IF lTypeTotal                         // If lTypeTotal=.T. Update from total
    AADD(aTrans,STR(IF(lUseTotal,nTotal,nAmount),22,nMaxDeci) )
    aTrans[nTotTran] := _ftStuffComma(aTrans[nTotTran], .T.) + ' *'+         ;
                                     IF(lAddError,'ER','')

  ELSE                            // If lTypeTotal=.F. Update from nNumTotal
    AADD(aTrans,STR(IF(lUseTotal,nTotal,nAmount),22,nMaxDeci))

    aTrans[nTotTran] := _ftStuffComma(aTrans[nTotTran], .T.) +               ;
      IF(lSubRtn,' S',IF(nAddMode==1,' +',IF(nAddMode==2,' -',IF             ;
      (lTotalOk,' =',IF(nAddMode==3,' X',' /'))))) + IF(lAddError,'ER','')

  ENDIF

  IF lTape
    _ftDisplayTape(aAdder)
  ENDIF

RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftEraseTotSubTot()   Docs: Keith A. Wire                  ³
  ³  Description: Clear the <TOTAL> & <SUBTOTAL> from Adder                  ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:08:14pm            Time updated: ş12:08:14pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftEraseTotSubTot(aAdder)
  _ftSetWinColor(W_CURR,W_SCREEN)
  @ 6+nTopOS, 18+nAddSpace SAY '          '
  _ftSetWinColor(W_CURR,W_PROMPT)
RETURN NIL



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftRoundIt()          Docs: Keith A. Wire                  ³
  ³  Description: Adder Rounding function                                    ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:09:00pm            Time updated: ş12:09:00pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: nNumber                                                    ³
  ³             : nPlaces                                                    ³
  ³ Return Value: INT@( ABS@(nNumber@) @* 10 @^ nPlaces @+ 0@.50 @+ 10 @^ - ;³
  ³             :    12 @) / 10 @^ nPlaces                                   ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftRoundIt(nNumber, nPlaces)
  nPlaces := IF( nPlaces == NIL, 0, nPlaces )
RETURN IF(nNumber < 0.0, -1.0, 1.0) *                                        ;
       INT( ABS(nNumber) * 10 ^ nPlaces + 0.50 + 10 ^ -12 ) / 10 ^ nPlaces




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftDivide()           Docs: Keith A. Wire                  ³
  ³  Description: Check divide by zero not allowed                           ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:10:41pm            Time updated: ş12:10:41pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³             : nNumerator                                                 ³
  ³             : nDenominator                                               ³
  ³ Return Value: @(nNumerator/nDenominator@)                                ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftDivide(aAdder, nNumerator,nDenominator)
  IF nDenominator==0.0
    lDivError := .T.
    RETU 0
  ELSE
    lDivError := .F.
  ENDIF
RETURN(nNumerator/nDenominator)



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftValDeci()          Docs: Keith A. Wire                  ³
  ³  Description: Validate the number of decimals                            ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:10:56pm            Time updated: ş12:10:56pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: oGet                                                       ³
  ³ Return Value: lRtnValue                                                  ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftValDeci(oGet)

  LOCAL lRtnValue := .T.

  IF oGet:VarGet() > 8
    _ftError('no more than 8 decimal places please!')
    lRtnValue := .F.
  ENDIF

RETURN lRtnValue



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftDisplayTape()      Docs: Keith A. Wire                  ³
  ³  Description: Display the Tape                                           ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:11:28pm            Time updated: ş12:11:28pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: aAdder                                                     ³
  ³             : nKey                                                       ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftDisplayTape(aAdder, nKey)
  LOCAL nDispTape, nTopTape := 1
  IF (nKey == 84 .OR. nKey == 116) .AND. lTape  // Stop displaying tape
    lTape := .F.
    RESTSCREEN(4+nTopOS,6+nTapeSpace,22+nTopOS,35+nTapeSpace,cTapeScr)
    RETU NIL
  ENDIF
  IF lTape                              // Are we in the display mode
    SETCOLOR('N/W')
    SCROLL(5+nTopOS,7+nTapeSpace,20+nTopOS,32+nTapeSpace,1)
    IF nTotTran>0                       // Any transactions been entered yet?
      @ 20+nTopOS,7+nTapeSpace SAY aTrans[nTotTran]
    ENDIF
    _ftSetWinColor(W_CURR,W_PROMPT)
  ELSE                                  // Start displaying tape
    lTape := .T.
    SETCOLOR('N/W')
    cTapeScr := SAVESCREEN(4+nTopOS,6+nTapeSpace,22+nTopOS,35+nTapeSpace)
    _ftShadow(22+nTopOS,8+nTapeSpace,22+nTopOS,35+nTapeSpace)
    _ftShadow(5+nTopOS,33+nTapeSpace,21+nTopOS,35+nTapeSpace)
    SETCOLOR('R+/W')
    @ 4+nTopOS,6+nTapeSpace,21+nTopOS,33+nTapeSpace BOX B_SINGLE
    SETCOLOR('GR+/W')
    @ 4+nTopOS,17+nTapeSpace SAY ' TAPE '
    SETCOLOR('N/W')
    IF nTotTran>15
      nTopTape := nTotTran-15
    ENDIF
    FOR nDispTape=nTotTran TO nTopTape STEP -1
      @ 20+nDispTape-nTotTran+nTopOS,7+nTapeSpace SAY aTrans[nDispTape]
    NEXT
  ENDIF
  _ftSetWinColor(W_CURR,W_PROMPT)
RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftSetLastKey()       Docs: Keith A. Wire                  ³
  ³  Description: Sets the LASTKEY() value to value of nLastKey              ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:12:00pm            Time updated: ş12:12:00pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: nLastKey                                                   ³
  ³ Return Value: NIL                                                        ³
  ³        Notes: I use this in most of my Pop-Up routines to reset the      ³
  ³             : original value of LASTKEY() when quitting.                 ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftSetLastKey(nLastKey)
  _ftPushKeys()
  KEYBOARD CHR(nLastKey)
  INKEY()
  _ftPopKeys()
RETURN NIL





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPushKeys           Docs: Keith A. Wire                  ³
  ³  Description: Push any keys in the Keyboard buffer on the array aKeys[]  ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:16:09pm            Time updated: ş12:16:09pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: NIL                                                        ³
  ³        Notes: Save any keys in the buffer... for FAST typists <g>.       ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPushKeys
  DO WHILE NEXTKEY() != 0
    AADD(aKeys,INKEY())
  ENDDO
RETURN NIL





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPopKeys            Docs: Keith A. Wire                  ³
  ³  Description: Restore the keyboard with any keystrokes that were saved   ³
  ³             :   with _ftPushKeys                                         ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:17:58pm            Time updated: ş12:17:58pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPopKeys
  LOCAL cKeys := ''
  IF LEN(aKeys) != 0
    AEVAL(aKeys, {|elem| cKeys += CHR(elem)})
  ENDIF
  KEYBOARD cKeys
  aKeys := {}
RETURN NIL


/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPushMessage()      Docs: Keith A. Wire                  ³
  ³  Description: Display a message on the screen in a window                ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:18:53pm            Time updated: ş12:18:53pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cMessage                                                   ³
  ³             : lWait                                                      ³
  ³             : cTitle                                                     ³
  ³             : cBotTitle                                                  ³
  ³             : xQuiet                                                     ³
  ³             : nTop                                                       ³
  ³ Return Value: NIL                                                        ³
  ³     See Also: _ftPopMessage                                              ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPushMessage(cMessage,lWait,cTitle,cBotTitle,xQuiet, nTop)
  LOCAL nMessLen, nNumRows, nWide, nLeft, nBottom, nRight, nKey, cOldDevic,  ;
        lOldPrint,                                                           ;
        cOldColor   := SETCOLOR(),                                           ;
        nOldLastkey := LASTKEY(),                                            ;
        nOldRow     := ROW(),                                                ;
        nOldCol     := COL(),                                                ;
        nOldCurs    := SETCURSOR(SC_NONE),                                   ;
        nWinColor   := IF(nWinColor == NIL, W_CURR, nWinColor)

  cOldDevic := SET(_SET_DEVICE, 'SCREEN')
  lOldPrint := SET(_SET_PRINTER, .F.)
  nMessLen  := LEN(cMessage)
  nWide     := IF(nMessLen>72,72,IF(nMessLen<12,12,nMessLen))
  nNumRows  := MLCOUNT(cMessage,nWide)

  // If they didn't say what the top row is, Center it on the screen
  DEFAULT nTop TO INT((MAXROW()-nNumRows)/2)

  nBottom   := nTop+nNumRows+2
  nLeft     := INT((MAXCOL()-nWide)/2)-3
  nRight    := nLeft+nWide+4
  lWait     := IF(lWait == NIL, .F., lWait)

  _ftPushWin(nTop,nLeft,nBottom,nRight,cTitle,cBotTitle,nWinColor)
  DISPMESSAGE cMessage,nTop+1,nLeft+2,nBottom-1,nRight-2

  IF xQuiet == NIL
    TONE(800, 1)
  ENDIF
  IF lWait
    FT_INKEY 0 TO nKey
    _ftPopMessage()
  ENDIF

  SETCURSOR(nOldCurs)
  SETCOLOR(cOldColor)
  SETPOS(nOldRow,nOldCol)
  SET(_SET_DEVICE, cOldDevic)
  SET(_SET_PRINTER, lOldPrint)
  _ftSetLastKey(nOldLastKey)
RETURN NIL



/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPopMessage         Docs: Keith A. Wire                  ³
  ³  Description: Pop off the Message Box                                    ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:24:22pm            Time updated: ş12:24:22pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: NIL                                                        ³
  ³     See Also: _ftPushMessage()                                           ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPopMessage
  _ftPopWin()
RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftQuest()            Docs: Keith A. Wire                  ³
  ³  Description: Push a Question Box on the Screen                          ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:25:32pm            Time updated: ş12:25:32pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cMessage                                                   ³
  ³             : xVarVal                                                    ³
  ³             : cPict                                                      ³
  ³             : bValid                                                     ³
  ³             : lNoESC                                                     ³
  ³             : nWinColor                                                  ³
  ³             : nTop                                                       ³
  ³ Return Value: xVarVal                                                    ³
  ³        Notes: This function will work for all Data Types                 ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftQuest(cMessage,xVarVal,cPict,bValid,lNoESC,nWinColor,nTop)

  LOCAL nOldRow, nOldCol, cOldColor, nMessLen, nWide, nNumRows, nBottom, nLeft
  LOCAL nRight, oNewGet, nNumMessRow, nLenLastRow, lGetOnNextLine, nOldCurs
  LOCAL cVarType := VALTYPE(xVarVal)
  LOCAL nVarLen  := IF(cVarType='C',LEN(xVarVal),IF(cVarType='D',8,          ;
                       IF(cVarType='L',1,IF(cVarType='N',IF(cPict=NIL,9,     ;
                       LEN(cPict)),0))))
  LOCAL nOldLastKey := LASTKEY()
  LOCAL GETLIST := {},                                                       ;
        cOldDevice  := SET(_SET_DEVICE, 'SCREEN'),                           ;
        lOldPrint   := SET(_SET_PRINTER, .F.)

  nOldRow   := ROW()
  nOldCol   := COL()
  nOldCurs  := SETCURSOR(SC_NONE)
  cOldColor := SETCOLOR()
  lNoESC    := IF(lNoESC==NIL,.F.,lNoESC)

  nMessLen  := LEN(cMessage)+nVarLen+1
  nWide     := IF(nMessLen>66,66,IF(nMessLen<12,12,nMessLen))

  nNumMessRow    := MLCOUNT(cMessage,nWide)
  nLenLastRow    := LEN(TRIM(MEMOLINE(cMessage,nWide,nNumMessRow)))
  lGetOnNextLine := (nLenLastRow + nVarLen) > nWide
  nNumRows       := nNumMessRow + IF(lGetOnNextLine,1,0)

  // Center it in the screen
  nTop        := IF(nTop=NIL,INT((MAXROW() - nNumRows)/2),nTop)
  nBottom     := nTop+nNumRows+1
  nLeft       := INT((MAXCOL()-nWide)/2)-4
  nRight      := nLeft+nWide+4

  _ftPushWin(nTop,nLeft,nBottom,nRight,'QUESTION ?',IF(VALTYPE(xVarVal)='C'  ;
          .AND. nVarLen>nWide,CHR(27)+' scroll '+ CHR(26),NIL),nWinColor)
  DISPMESSAGE cMessage,nTop+1,nLeft+2,nBottom-1,nRight-2

  oNewGet := GetNew( IF(lGetOnNextLine,Row()+1,Row()),                       ;
                     IF(lGetOnNextLine,nLeft+2,Col()+1),                     ;
                     {|x| IF(PCOUNT() > 0, xVarVal := x, xVarVal)},          ;
                     'xVarVal' )

  // If the input line is character & wider than window SCROLL
  IF lGetOnNextLine .AND. VALTYPE(xVarVal)='C' .AND. nVarLen>nWide
    oNewGet:Picture   := '@S'+LTRIM(STR(nWide,4,0))+IF(cPict=NIL,'',' '+cPict)
  ENDIF

  IF cPict != NIL                       // Use the picture they passed
    oNewGet:Picture   := cPict
  ELSE                                  // Else setup default pictures
    IF VALTYPE(xVarVal)='D'
      oNewGet:Picture   := '99/99/99'
    ELSEIF VALTYPE(xVarVal)='L'
      oNewGet:Picture   := 'Y'
    ELSEIF VALTYPE(xVarVal)='N'
      oNewGet:Picture   := '999999.99'  // Guess that they are inputting dollars
    ENDIF
  ENDIF

  oNewGet:PostBlock := IF(bValid=NIL,NIL,bValid)

  oNewGet:Display()

  SETCURSOR(SC_NORMAL)
  DO WHILE .T.                          // Loop so we can check for <ESC>
                                        // without reissuing the gets
    ReadModal({oNewGet})
    IF LASTKEY() == K_ESC .AND. lNoESC  // They pressed <ESC>
      _ftError('you cannot Abort! Please enter an answer.')
    ELSE
      EXIT
    ENDIF

  ENDDO

  _ftPopWin()

  SETCURSOR(nOldCurs)
  SETCOLOR(cOldColor)
  SETPOS(nOldRow,nOldCol)
  SET(_SET_DEVICE,  cOldDevice)
  SET(_SET_PRINTER, lOldPrint)
  _ftSetLastKey(nOldLastKey)
RETURN xVarVal




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftAdderTapeUDF()    Docs: Keith A. Wire                   ³
  ³  Description: User function for ACHOICE() when scrolling tape            ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:26:44pm            Time updated: ş12:26:44pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: mode                                                       ³
  ³             : cur_elem                                                   ³
  ³             : rel_pos                                                    ³
  ³ Return Value: nRtnVal                                                    ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
FUNCTION _ftAdderTapeUDF(mode,cur_elem,rel_pos)
  LOCAL nKey,nRtnVal
  STATIC ac_exit_ok := .F.
  DO CASE
    CASE mode == AC_EXCEPT
      nKey := LASTKEY()
      DO CASE
        CASE nKey == 30
          nRtnVal := AC_CONT
        CASE nKey == K_ESC
          KEYBOARD CHR(K_CTRL_PGDN)+CHR(K_RETURN)  // Go to last item
          ac_exit_ok := .T.
          nRtnVal := AC_CONT
        CASE ac_exit_ok
          nRtnVal := AC_ABORT
          ac_exit_ok := .F.
        OTHERWISE
          nRtnVal := AC_CONT
      ENDCASE
    OTHERWISE
      nRtnVal := AC_CONT
  ENDCASE
RETURN nRtnVal




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftError()            Docs: Keith A. Wire                  ³
  ³  Description: Display an ERROR message in a window                       ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:27:43pm            Time updated: ş12:27:43pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cMessage                                                   ³
  ³             : xDontReset                                                 ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftError(cMessage, xDontReset)
  LOCAL nOldRow,nOldCol,nOldCurs,nTop,nLeft,nBot,nRight,cOldColor,           ;
        nOldLastKey,cErrorScr,nMessLen,nWide,nNumRows,nKey,                  ;
        cOldDevic,lOldPrint,                                                 ;
        lResetLKey := IF(xDontReset==NIL, .T., .F.)

  nOldLastKey := LASTKEY()
  nOldRow  := ROW()
  nOldCol  := COL()
  nOldCurs := SETCURSOR(SC_NONE)
  cOldColor:= _ftSetSCRColor(STD_ERROR)
  cOldDevic := SET(_SET_DEVICE, 'SCREEN')
  lOldPrint := SET(_SET_PRINTER, .F.)
  cMessage := "I'm sorry but, " + cMessage
  nMessLen := LEN(cMessage)
  nWide    := IF(nMessLen>66,66,IF(nMessLen<12,12,nMessLen))
  nNumRows := MLCOUNT(cMessage,nWide)
  nTop     := INT((MAXROW() - nNumRows)/2)  // Center it in the screen
  nBot     := nTop+3+nNumRows
  nLeft    := INT((MAXCOL()-nWide)/2)-2
  nRight   := nLeft+nWide+4

  cErrorScr:=SAVESCREEN(nTop,nLeft,nBot+1,nRight+2)
  _ftShadow(nBot+1,nLeft+2,nBot+1,nRight+2,8)
  _ftShadow(nTop+1,nRight+1,nBot  ,nRight+2,8)
  @ nTop,nLeft,nBot,nRight BOX B_SINGLE
  @ nTop,nLeft+INT(nWide/2)-1 SAY ' ERROR '
  @ nBot-1,nLeft+INT(nWide-28)/2+3 SAY 'Press any key to continue...'
  DISPMESSAGE cMessage,nTop+1,nLeft+3,nBot-2,nRight-3
  TONE(70,5)
  FT_INKEY 0 TO nKey
  RESTSCREEN(nTop,nLeft,nBot+1,nRight+2,cErrorScr)
  SETCURSOR(nOldCurs)
  SETCOLOR(cOldColor)
  SETPOS(nOldRow,nOldCol)

  IF lResetLKey
    _ftSetLastKey(nOldLastKey)
  ENDIF

  SET(_SET_DEVICE, cOldDevic)
  SET(_SET_PRINTER, lOldPrint)

RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftStuffComma()       Docs: Keith A. Wire                  ³
  ³  Description: Stuff a Comma in a string                                  ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:28:19pm            Time updated: ş12:28:19pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cStrToStuff                                                ³
  ³             : lTrimStuffedStr                                            ³
  ³ Return Value: cStrToStuff                                                ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftStuffComma(cStrToStuff,lTrimStuffedStr)

  LOCAL nDecPosit, x,                                                        ;
        nOrLen := LEN(cStrToStuff)

  lTrimStuffedStr := IF(lTrimStuffedStr=NIL,.F.,lTrimStuffedStr)
  IF !('.' $ cStrToStuff)
    cStrToStuff := _ftPosIns(cStrToStuff,'.',IF('C'$cStrToStuff .OR.         ;
                   'E'$cStrToStuff .OR. '+'$cStrToStuff .OR. '-'$cStrToStuff ;
                   .OR. 'X'$cStrToStuff .OR. '*'$cStrToStuff .OR.            ;
                   ''$cStrToStuff .OR. '/'$cStrToStuff .OR. '='$cStrToStuff,;
                   LEN(cStrToStuff)-1,LEN(cStrToStuff)+1))

    IF ASC(cStrToStuff) == K_SPACE .OR. ASC(cStrToStuff) == K_ZERO
      cStrToStuff := SUBSTR(cStrToStuff, 2)
    ENDIF

  ENDIF
  nDecPosit := AT('.',cStrToStuff)

  IF LEN(LEFT(LTRIM(_ftCharRem('-',cStrToStuff)),                            ;
      AT('.',LTRIM(_ftCharRem('-',cStrToStuff)))-1))>3
    IF lTrimStuffedStr    // Do we trim the number each time we insert a comma
      FOR x=nDecPosit-3 TO 2+_ftCountLeft(cStrToStuff,' ') STEP -4
        cStrToStuff := SUBSTR(_ftPosIns(cStrToStuff,',',x),2)
      NEXT
    ELSE
      FOR x=nDecPosit-3 TO 2+_ftCountLeft(cStrToStuff,' ') STEP -3
        cStrToStuff := _ftPosIns(cStrToStuff,',',x)
      NEXT
    ENDIF
  ENDIF

RETURN cStrToStuff





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftSetSCRColor()      Docs: Keith A. Wire                  ³
  ³  Description: Set the standard screen colors to the color requested.     ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:28:48pm            Time updated: ş12:28:48pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: nStd                                                       ³
  ³             : nEnh                                                       ³
  ³             : nBord                                                      ³
  ³             : nBack                                                      ³
  ³             : nUnsel                                                     ³
  ³ Return Value: SETCOLOR(aStdColor[nStd] + ',' + aStdColor[nEnh] + ',' + ; ³
  ³             :   aStdColor[nBord] + ',' + aStdColor[nBack] + ',' +      ; ³
  ³             :   aStdColor[nUnsel])                                       ³
  ³     See Also: _ftSetWinColor()                                           ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftSetSCRColor(nStd,nEnh,nBord,nBack,nUnsel)

  IF EMPTY(aWinColor)
    _ftInitColors()
  ENDIF

  nStd  := IF(nStd   == NIL, 8,    nStd)
  nEnh  := IF(nEnh   == NIL, 8,    nEnh)
  nBord := IF(nBord  == NIL, 8,    nBord)
  nBack := IF(nBack  == NIL, 8,    nBack)
  nUnsel:= IF(nUnsel == NIL, nEnh, nUnsel)

RETURN SETCOLOR(aStdColor[nStd]+','+aStdColor[nEnh]+','+aStdColor[nBord]+','+;
  aStdColor[nBack]+','+aStdColor[nUnsel])




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPushWin()          Docs: Keith A. Wire                  ³
  ³  Description: Push a new window on the screen                            ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:34:38pm            Time updated: ş12:34:38pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: t                                                          ³
  ³             : l                                                          ³
  ³             : b                                                          ³
  ³             : r                                                          ³
  ³             : cTitle                                                     ³
  ³             : cBotTitle                                                  ³
  ³             : nWinColor                                                  ³
  ³ Return Value: NIL                                                        ³
  ³     See Also:                                                            ³
  ³        Notes: Push a new window on the screen in the position t,l,b,r    ³
  ³             :   and if cTitle is not NIL print the title for the window  ³
  ³             :   in centered in the top line of the box. Similarly do     ³
  ³             :   the same for cBotTitle. If nWinColor=NIL get the next    ³
  ³             :   window color and use it for all the colors. If           ³
  ³             :   cTypeBord=NIL use the single line border, else use the   ³
  ³             :   one they requested. Push the window coordinates, the     ³
  ³             :   color number, the SAVESCREEN() value, and whether they   ³
  ³             :   picked the window color they wanted to use. If           ³
  ³             :   lAutoWindow=.F. then the window color was incremented    ³
  ³             :   and we will will restore the color number when we pop    ³
  ³             :   the window off.                                          ³
  ³             :                                                            ³
  ³             :      nWinColor DEFAULT == _ftNextWinColor()                ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPushWin(t,l,b,r,cTitle,cBotTitle,nWinColor)

  LOCAL lAutoWindow := nWinColor==NIL

  nWinColor := IF(nWinColor=NIL,_ftNextWinColor(),nWinColor)
  AADD(aWindow,{t,l,b,r,nWinColor,SAVESCREEN(t,l,b+1,r+2),lAutoWindow})
  _ftShadow(b+1,l+2,b+1,r+2)
  _ftShadow(t+1,r+1,b,r+2)
  _ftSetWinColor(nWinColor,W_BORDER)
  @ t,l,b,r BOX B_SINGLE

  IF cTitle!=NIL
    _ftSetWinColor(nWinColor,W_TITLE)
    _ftWinTitle(cTitle)
  ENDIF

  IF cBotTitle!=NIL
    _ftSetWinColor(nWinColor,W_TITLE)
    _ftWinTitle(cBotTitle,'bot')
  ENDIF

  _ftSetWinColor(nWinColor,W_SCREEN,W_VARIAB)
  @ t+1,l+1 CLEAR TO b-1,r-1

RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPopWin             Docs: Keith A. Wire                  ³
  ³  Description: Pop a Window off the screen                                ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 12:52:34pm            Time updated: ş12:52:34pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: NIL                                                        ³
  ³        Notes: Pop the currently active window off the screen by restoring³
  ³             :   it from the aWindow Array and if they pushed a new window³
  ³             :   automatically selecting the color we will roll back the  ³
  ³             :   current window setting using _ftLastWinColor() and reset ³
  ³             :   the color to the color setting when window was pushed.   ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPopWin

  LOCAL nNumWindow:=LEN(aWindow)

  RESTSCREEN(aWindow[nNumWindow,1],aWindow[nNumWindow,2],                    ;
             aWindow[nNumWindow,3]+1,aWindow[nNumWindow,4]+2,                ;
             aWindow[nNumWindow,6])

  IF aWindow[nNumWindow,7]
    _ftLastWinColor()
  ENDIF

  ASHRINK(aWindow)

  IF !EMPTY(aWindow)
    _ftSetWinColor(W_CURR,W_SCREEN,W_VARIAB)
  ELSE
    _ftSetSCRColor(STD_SCREEN,STD_VARIABLE)
  ENDIF

RETURN NIL





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftSetWinColor()      Docs: Keith A. Wire                  ³
  ³  Description: Set the Color to the Window Colors requested               ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:37:32pm            Time updated: ş01:37:32pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: nWin                                                       ³
  ³             : nStd                                                       ³
  ³             : nEnh                                                       ³
  ³             : nBord                                                      ³
  ³             : nBack                                                      ³
  ³             : nUnsel                                                     ³
  ³ Return Value:SETCOLOR(aWinColor[nStd,nWin]+','+aWinColor[nEnh,nWin]+','+;³
  ³             :  aWinColor[nBord,nWin]+','+aWinColor[nBack,nWin]+','+     ;³
  ³             :  aWinColor[nUnsel,nWin])                                   ³
  ³     See Also: _ftSetSCRColor()                                           ³
  ³        Notes: If the window number is not passed use the currently active³
  ³             :   window number nWinColor.                                 ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftSetWinColor(nWin,nStd,nEnh,nBord,nBack,nUnsel)

  nWin  := IF(nWin   == NIL, nWinColor, nWin)
  nStd  := IF(nStd   == NIL, 7,         nStd)
  nEnh  := IF(nEnh   == NIL, 7,         nEnh)
  nBord := IF(nBord  == NIL, 7,         nBord)
  nBack := IF(nBack  == NIL, 7,         nBack)
  nUnsel:= IF(nUnsel == NIL, nEnh,      nUnsel)

RETURN SETCOLOR(aWinColor[nStd,nWin]+','+aWinColor[nEnh,nWin]+','+           ;
  aWinColor[nBord,nWin]+','+aWinColor[nBack,nWin]+','+aWinColor[nUnsel,nWin])






/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftShadow()           Docs: Keith A. Wire                  ³
  ³  Description: Create a shadow on the screen in the coordinates given     ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:40:56pm            Time updated: ş01:40:56pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: nTop                                                       ³
  ³             : nLeft                                                      ³
  ³             : nBottom                                                    ³
  ³             : nRight                                                     ³
  ³ Return Value: NIL                                                        ³
  ³     See Also: _ftPushWin()                                               ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftShadow( nTop, nLeft, nBottom, nRight )

  LOCAL theShadow := SAVESCREEN(nTop, nLeft, nBottom, nRight)

  RESTSCREEN( nTop, nLeft, nBottom, nRight,                                  ;
              TRANSFORM( theShadow,REPLICATE("X", LEN(theShadow)/2 ) ) )

RETURN NIL





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftLastWinColor       Docs: Keith A. Wire                  ³
  ³  Description: Decrement the active window color number and return the    ³
  ³             :   current value                                            ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:49:19pm            Time updated: ş01:49:19pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: nWinColor := IF(nWinColor=1,4,nWinColor-1)                 ³
  ³        Notes: If we are already on window #1 restart count by using # 4. ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftLastWinColor
RETURN nWinColor := IF(nWinColor=1,4,nWinColor-1)






/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftNextWinColor       Docs: Keith A. Wire                  ³
  ³  Description: Increment the active window color number and return the    ³
  ³             :   current value                                            ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:51:12pm            Time updated: ş01:51:12pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: nWinColor := (IF(nWinColor<4,nWinColor+1,1))               ³
  ³        Notes: If we are already on window #4 restart count by using # 1. ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftNextWinColor
  IF EMPTY(aWinColor)
    _ftInitColors()
  ENDIF

RETURN nWinColor := (IF(nWinColor<4,nWinColor+1,1))





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftWinTitle()         Docs: Keith A. Wire                  ³
  ³  Description: Print the top or bottom titles on the border of the        ³
  ³             :   currently active window.                                 ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:52:29pm            Time updated: ş01:52:29pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cTheTitle                                                  ³
  ³             : cTopOrBot                                                  ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftWinTitle(cTheTitle,cTopOrBot)

  LOCAL nCurWin  :=LEN(aWindow),                                             ;
        nLenTitle:=LEN(cTheTitle)

  @ aWindow[nCurWin,IF(cTopOrBot=NIL,1,3)],(aWindow[nCurWin,4]-              ;
    aWindow[nCurWin,2]-nLenTitle)/2+aWindow[nCurWin,2] SAY ' '+cTheTitle+' '

RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftInitColors         Docs: Keith A. Wire                  ³
  ³  Description: Initilize the colors for the Adder                         ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 02:59:58pm            Time updated: ş02:59:58pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: None                                                       ³
  ³ Return Value: NIL                                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftInitColors

  aWinColor := { {'GR+/BG','GR+/G', 'B+/RB', 'G+/R'} ,                       ;
                 {'R+/N',   'W+/RB','W+/BG','GR+/B'} ,                       ;
                 {'GR+/N', 'GR+/N','GR+/N', 'GR+/N'} ,                       ;
                 {  'B/BG','BG+/G', 'W+/RB','BG+/R'} ,                       ;
                 { 'W+/BG', 'W+/G','GR+/RB', 'W+/R'} ,                       ;
                 {'GR+/B', 'GR+/R', 'R+/B',  'W+/BG'},                       ;
                 {  'N/N',   'N/N',  'N/N',   'N/N'}   }

  aStdColor := { 'BG+*/RB' ,                                                 ;
                  'GR+/R'  ,                                                 ;
                  'GR+/N'  ,                                                 ;
                    'W/B'  ,                                                 ;
                  'GR+/N'  ,                                                 ;
                  'GR+/GR' ,                                                 ;
                 { 'W+/B',  'W/B','G+/B','R+/B',                             ;
                  'GR+/B','BG+/B','B+/B','G+/B'},                            ;
                    'N/N'    }
RETURN NIL




/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftCharOdd()          Docs: Keith A. Wire                  ³
  ³  Description: Remove all the even numbered characters in a string.       ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:41:50pm            Time updated: ş01:41:50pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cString                                                    ³
  ³ Return Value: STRTRAN(cString,'')                                       ³
  ³        Notes: Used for example to strip all the attribute characters     ³
  ³             : from a screen save.                                        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftCharOdd(cString)
  cString := TRANSFORM(cString,REPLICATE("X", LEN(cString)/2 ) )
RETURN STRTRAN(cString,'')





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPosRepl()          Docs: Keith A. Wire                  ³
  ³  Description: Replace the Character at nPosit in cString with cChar      ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:44:21pm            Time updated: ş01:44:21pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cString                                                    ³
  ³             : cChar                                                      ³
  ³             : nPosit                                                     ³
  ³ Return Value: STRTRAN(cString,'9',cChar,nPosit,1)+''                     ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPosRepl(cString,cChar,nPosit)
RETURN STRTRAN(cString,'9',cChar,nPosit,1)+''





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftCharRem()          Docs: Keith A. Wire                  ³
  ³  Description: Removes all occurances of cChar from cString.              ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:45:41pm            Time updated: ş01:45:41pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cChar                                                      ³
  ³             : cString                                                    ³
  ³ Return Value: STRTRAN(cString,cChar)                                     ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftCharRem(cChar,cString)
RETURN STRTRAN(cString,cChar)








/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftCountLeft()        Docs: Keith A. Wire                  ³
  ³  Description: Returns the number of spaces on the Left side of the String³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:47:00pm            Time updated: ş01:47:00pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cString                                                    ³
  ³ Return Value: LEN(cString)-LEN(LTRIM(cString))                           ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftCountLeft(cString)
RETURN LEN(cString)-LEN(LTRIM(cString))





/*ÚÄ Function ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³         Name: _ftPosIns()           Docs: Keith A. Wire                  ³
  ³  Description: Insert the Character cChar in cString at position nPosit   ³
  ³       Author: Keith A. Wire                                              ³
  ³ Date created: 10-03-93              Date updated: ş10-03-93              ³
  ³ Time created: 01:48:30pm            Time updated: ş01:48:30pm            ³
  ³    Copyright: None - Public Domain                                       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³    Arguments: cString                                                    ³
  ³             : cChar                                                      ³
  ³             : nPosit                                                     ³
  ³ Return Value: LEFT(cString,nPosit-1)+cChar+SUBSTR(cString,nPosit)        ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/
STATIC FUNCTION _ftPosIns(cString,cChar,nPosit)
RETURN LEFT(cString,nPosit-1)+cChar+SUBSTR(cString,nPosit)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      


