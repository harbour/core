/*
 * $Id$
 */

/*
 * File......: clrsel.prg
 * Author....: Dave Adams
 * CIS ID....: 72037,2654
 *
 * This is an original work by Dave Adams and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   17 Aug 1991 15:05:22   GLENN
 * Don Caton made corrected some spelling errors in the doc
 *
 *    Rev 1.1   15 Aug 1991 23:03:50   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   13 Jun 1991 15:21:46   GLENN
 * Initial revision.
 *
 */

/*
 * File Contents
 *
 *   FT_ClrSel( aClrs, lColour, cChr )         user selectable colour routine
 *   _ftHiLite( nRow, nCol, cStr, nLen )       re-hilite an achoice prompt
 *   _ftColours( aOpt, aClrPal, lColour )      control colour selection
 *   _ftShowIt( aOpt )                         show a sample of the colours
 *   _ftClrSel( aClrPal, cClr, nElem, aOpt)    pick a colour
 *   _ftClrPut( cClrStr, nElem, cClr )         place a clr element into str
 *   _ftDeskChar( aOpt )                       select desktop char
 *   _ftChr2Arr( cString, cDelim )             parse string into array
 *   _ftArr2Chr( aArray, cDelim )              create string from array
 *   _ftShowPal( aClrPal, cChr )               paint palette on screen
 *   _ftInitPal( aClrTab )                     create the palette
 *   _ftIdentArr( aArray1, aArray2 )           compare array contents
 *
 */

/*
 * Commentary
 *
 *  Thanks to Brian Loesgen for offering ideas and helping to tweak
 *  the code.
 *
 *
 */

*------------------------------------------------
// Pre-processor stuff

#include "box.ch"
#include "setcurs.ch"
#include "inkey.ch"

#define C_NAME   1
#define C_CLR    2
#define C_TYPE   3
#define C_CHAR   4

#translate Single( <t>, <l>, <b>, <r> ) =>;
           @ <t>, <l>, <b>, <r> BOX B_SINGLE

#translate Double( <t>, <l>, <b>, <r> ) =>;
           @ <t>, <l>, <b>, <r> BOX B_DOUBLE

#translate ClearS( <t>, <l>, <b>, <r> ) =>;
           @ <t>, <l> CLEAR TO <b>, <r>

#translate BkGrnd( <t>, <l>, <b>, <r>, <c> ) =>;
           DispBox( <t>, <l>, <b>, <r>, REPLICATE(<c>,9) )

#command DEFAULT <p> TO <val> [, <pn> TO <valn> ]  =>;
         <p> := iif( <p> == Nil, <val>, <p> );     ;
         [ <pn> := iif( <pn> == Nil, <valn>, <pn> ) ]

*------------------------------------------------
//  Demo of FT_ClrSel()

/*
 *     To run the sample program:
 *
 *     Compile :   Clipper ClrSel /n /m /w /dFT_TEST
 *     Link    :   Rtlink FILE ClrSel LIB NanFor [/PLL:Fullbase]
 *                                         .OR.  [/PLL:Base50]
 *
 *     ClrSel MONO      To force monochrome mode
 *     ClrSel NOSNOW    To prevent CGA snowstorms
 *     ClrSel EGA       43 line mode
 *     ClrSel VGA       50 line mode
 *
 */

#IFDEF FT_TEST

  FUNCTION Main( cVidMode )

  LOCAL nRowDos := ROW()
  LOCAL nColDos := COL()
  LOCAL lBlink  := SETBLINK( .F. )  // make sure it starts out .F.
  LOCAL aEnvDos := FT_SaveSets()
  LOCAL cScrDos := SAVESCREEN( 00, 00, MAXROW(), MAXCOL() )
  LOCAL lColour := .F.
  LOCAL aClrs   := {}

  DEFAULT cVidMode TO ""
  NOSNOW( ( "NOSNOW" $ UPPER( cVidMode ) ) )
  IF "VGA" $ UPPER( cVidMode )
     SETMODE( 50, 80 )
  ENDIF
  IF "EGA" $ UPPER( cVidMode )
     SETMODE( 43, 80 )
  ENDIF
  lColour := iif( "MONO" $ UPPER( cVidMode ), .F., IsColor() )

  SET SCOREBOARD Off
  SETCURSOR( SC_NONE )
  lBlink := SETBLINK( .F. )

  *.... a typical application might have the following different settings
  *     normally these would be stored in a .dbf/.dbv
  aClrs := {;
     { "Desktop",        "N/BG",                         "D", hb_UTF8ToStr( "▒" ) }, ;
     { "Title",          "N/W",                          "T"      }, ;
     { "Top Menu",       "N/BG,N/W,W+/BG,W+/N,GR+/N",    "M"      }, ;
     { "Sub Menu",       "W+/N*,GR+/N*,GR+/N*,W+/R,G+/R","M"      }, ;
     { "Standard Gets",  "W/B,  W+/N,,, W/N",            "G"      }, ;
     { "Nested Gets",    "N/BG, W+/N,,, W/N",            "G"      }, ;
     { "Help",           "N/G,  W+/N,,, W/N",            "W"      }, ;
     { "Error Messages", "W+/R*,N/GR*,,,N/R*",           "W"      }, ;
     { "Database Query", "N/BG, N/GR*,,,N+/BG",          "B"      }, ;
     { "Pick List",      "N/GR*,W+/B,,, BG/GR*",         "A"      }  ;
           }

  aClrs := FT_ClrSel( aClrs, lColour )

  *.... restore the DOS environment
  FT_RestSets( aEnvDos )
  RESTSCREEN( 00, 00, MAXROW(), MAXCOL(), cScrDos )
  SETPOS( nRowDos, nColDos )
  SETBLINK( .F. )  // doesn't appear to be reset from FT_RestSets

  RETURN Nil

#ENDIF

*------------------------------------------------
FUNCTION FT_ClrSel( aClrs, lColour, cChr )
// Colour selection routine
// Return -> the same array that was passed but with modified colours

LOCAL aClrOld := aClone( aClrs )
LOCAL aOptions
LOCAL nB, nT, nL, nR
LOCAL nChoice := 1
LOCAL nLen    := 0
LOCAL aPrompt := {}
LOCAL aClrPal
LOCAL aClrTab := { "N","B","G","BG","R","RB","GR","W" }
LOCAL aClrBW  := { "N","B","W" }
LOCAL nRowSav := ROW()
LOCAL nColSav := COL()
LOCAL aEnvSav := FT_SaveSets()
LOCAL cScrSav := SAVESCREEN( 00, 00, MAXROW(), MAXCOL() )

DEFAULT lColour TO IsColor()
DEFAULT cChr TO chr(254)+chr(254)
cChr := PadR( cChr, 2 )

SETCURSOR( SC_NONE )
SETCOLOR( iif( lColour, "GR+/N,,N/N", "W+/N,,N/N" ) )
CLS

*.... initialize the colour palette
aClrPal := _ftInitPal( iif( lColour, aClrTab, aClrBW ) )

*.... paint the colours on the screen
_ftShowPal( aClrPal, cChr )

*.... Determine length of longest name and make sure not greater than 20
aEval( aClrs, { |aOpt| nLen := MAX( nLen, LEN( aOpt[C_NAME] ) ) } )
nLen := MIN( MAX( nLen, 1 ), 20 ) + 2

*.... prepare an array for use with aChoice(); truncate names at 20 chrs.
aPrompt := ARRAY( LEN( aClrs ) )
aEval( aClrs,;
       { |aOpt,nE| aPrompt[nE] := " "+ SUBS(aOpt[C_NAME], 1, nLen-2) +" " };
     )

*.... determine co-ordinates for the achoice window
nT := MAX( INT( (18-LEN(aPrompt)) /2 )-1, 1 )
nB := MIN( nT + LEN(aPrompt) + 1, 17 )
nL := MAX( INT( (27-nLen) /2 )-2, 1 )
nR := MIN( nL + nLen + 3, 26 )

*.... set up the window for aChoice
SETCOLOR( iif( lColour, "N/W,W+/R", "N/W,W+/N" ) )
ClearS( nT, nL,   nB, nR )

*.... prompt for colour setting and modify
DO WHILE nChoice != 0
  Double( nT, nL+1, nB, nR-1 )
  nChoice := aChoice( nt+1, nL+2, nB-1, nR-2, aPrompt, , , nChoice )
  IF nChoice != 0
    _ftHiLite( ROW(), nL+2, aPrompt[ nChoice ], nLen )
    Single( nT, nL+1, nB, nR-1 )
    aClrs[ nChoice ] := _ftColours( aClrs[ nChoice ], aClrPal, lColour )
  ENDIF
ENDDO

aOptions := { "Save New Colours", "Restore Original" }
IF ! _ftIdentArr( aClrs, aClrOld )
  nChoice := ALERT( "Colors have been modified...", aOptions )
ELSE
  nChoice := 1
ENDIF

FT_RestSets( aEnvSav )
RESTSCREEN( 00, 00, MAXROW(), MAXCOL(), cScrSav )
SETPOS( nRowSav, nColSav )

RETURN iif( nChoice == 1, aClrs, aClrOld )

*------------------------------------------------
STATIC FUNCTION _ftHiLite( nRow, nCol, cStr, nLen )
// Highlight the current selected aChoice element
// Return -> Nil

LOCAL cClr := SETCOLOR()
LOCAL aClr := _ftChr2Arr( cClr )

SETCOLOR( aClr[ 2 ] )                  // enhanced colour
@ nRow, nCol SAY PadR( cStr, nLen )
SETCOLOR( cClr )

RETURN Nil

*------------------------------------------------
STATIC FUNCTION _ftColours( aOpt, aClrPal, lColour )
// Colour selection for specific type of colour setting
// Return -> aOpt with modified colour strings

LOCAL nB, nT, nL, nR
LOCAL nX
LOCAL aClrs   := {}
LOCAL cClr
LOCAL nChoice := 1
LOCAL aPrompt
LOCAL nLen    := 0
LOCAL cColour := SETCOLOR()
LOCAL cScrSav := SAVESCREEN( 18, 00, MAXROW(), MAXCOL() )

aSize( aOpt, 4 )                            // check incoming parameters
DEFAULT aOpt[ C_CHAR ] TO ""
DEFAULT aOpt[ C_TYPE ] TO "W"
aOpt[ C_CLR ]  := UPPER( aOpt[ C_CLR ] )    // need upper case
aOpt[ C_TYPE ] := UPPER( aOpt[ C_TYPE ] )

DEFAULT lColour TO IsColor()

*.... display appropriate prompts based on type of colour setting
nChoice := 1
DO CASE
   CASE aOpt[ C_TYPE ] == "D"
     aPrompt := { " Color ", " Character " }
   CASE aOpt[ C_TYPE ] == "M"
     aPrompt := { " Prompt ", " Message ", " HotKey ",;
                  " LightBar ", " LightBar HotKey " }
   CASE aOpt[ C_TYPE ] == "A" .OR.  aOpt[ C_TYPE ] == "B"
     aPrompt := { " Standard ", " Selected ", " Border ", " Unavailable " }
   OTHERWISE
     aPrompt := { " Standard ", " Selected ", " Border ", " Unselected " }
ENDCASE

IF !( aOpt[ C_TYPE ] == "T" )  // no prompt for titles
  *.... we need to know top,left,bottom,right for the prompt window
  aEval( aPrompt, { |cPrompt| nLen := MAX( nLen, LEN( cPrompt ) ) } )
  nLen := MAX( nLen, LEN( aOpt[ C_NAME ] ) + 2 )
  nT := iif( aOpt[ C_TYPE ] == "M", 18, 19 )
  nB := nT + LEN(aPrompt) + 1
  nL := MAX( INT( (27-nLen) /2 )-2, 1 )
  nR := MIN( nL + nLen + 3, 26 )

  *.... set up the window for prompt
  SETCOLOR( "N/W" )
  ClearS( nT, nL, nB, nR )
ENDIF

DO WHILE .T.

  *.... show sample window
  _ftShowIt( aOpt )

  IF !( aOpt[ C_TYPE ] == "T" )  // no prompt for titles
    SETCOLOR( iif( lColour, "N/W,W+/R,,,N/W", "N/W,W+/N,,,N/W" ) )
    Double( nT, nL+1, nB, nR-1 )
    @ nT, nL+2 SAY PadC( " "+ aOpt[C_NAME] +" ", nR -nL -3, hb_UTF8ToStr( "═" ) )
    FOR nX := 1 TO LEN( aPrompt )
      @ nX+nT, nL+2 PROMPT PadR( aPrompt[nX], nR -nL -3 )
    NEXT
    MENU TO nChoice

    DO CASE
       CASE nChoice == 0
         EXIT
       CASE nChoice == 2 .AND. aOpt[ C_TYPE ] == "D"
         *....  desktop character
         aOpt := _ftDeskChar( aOpt )
         LOOP
       CASE nChoice == 4 .AND. !( aOpt[ C_TYPE ] == "M" )
         nChoice := 5      // 4th color param is unused
    ENDCASE
  ENDIF

  *.... get the specific colour combination
  aClrs := _ftChr2Arr( aOpt[ C_CLR ] )   // place color string in an array
  aSize( aClrs, 5 )                      // make sure there are 5 settings
  *.... empty elements are made Nil so they can be defaulted
  aEval( aClrs, { |v,e| aClrs[e] := iif( EMPTY(v), Nil, ALLTRIM(v) ) } )
  DEFAULT aClrs[1] TO "W/N"
  DEFAULT aClrs[2] TO "N/W"   // place default colours into
  DEFAULT aClrs[3] TO "N/N"   // elements which are empty
  DEFAULT aClrs[4] TO "N/N"
  DEFAULT aClrs[5] TO "N/W"
  cClr := aClrs[ nChoice ]    // selected colour

  *.... allow change to specific part of colour string
  IF !( aOpt[ C_TYPE ] == "T" )
    Single( nT, nL+1, nB, nR-1 )
    @ nT, nL+2 SAY PadC( " "+ aOpt[C_NAME] +" ", nR -nL -3, hb_UTF8ToStr( "─" ) )
  ENDIF
  cClr := _ftClrSel( aClrPal, cClr, nChoice, aOpt )  //  selection routine
  aClrs[ nChoice ] := cClr               // put colour back in array
  aOpt[ C_CLR ] := _ftArr2Chr( aClrs )   // convert array to colour string

  IF aOpt[ C_TYPE ] == "T"
    EXIT
  ENDIF

ENDDO

*.... restore the lower 1/2 of screen, and colour
RESTSCREEN( 18, 00, MAXROW(), MAXCOL(), cScrSav )
SETCOLOR( cColour )

RETURN aOpt

*------------------------------------------------
STATIC FUNCTION _ftShowIt( aOpt )
// Show an example of the colour setting
// Return -> Nil

LOCAL aClr := _ftChr2Arr( aOpt[ C_CLR ] )

IF !( aOpt[ C_TYPE ] == "M" ) // no borders in menu colour selection
  SETCOLOR( aOpt[ C_CLR ] )  // this will set the border on VGA
ENDIF

DispBegin()
DO CASE

   CASE aOpt[ C_TYPE ] == "D"    // Desktop Background
     SETCOLOR( aClr[1] )
     BkGrnd( 19, 43, 22, 64, aOpt[ C_CHAR ] )

   CASE aOpt[ C_TYPE ] == "T"    // Title
     SETCOLOR( aClr[1] )
     @ 20,08 SAY PadC( "This is an example of how the text shall look", 63 )

   CASE aOpt[ C_TYPE ] == "M"    // Menus
     SETCOLOR( "W/N" )
     BkGrnd( 19, 41, 23, 66, CHR(177) )
     SETCOLOR( aClr[1] )
     Single( 19, 43, 22, 60 )
     @ 18,41 SAY "   Report  Inquiry  Quit  "
     @ 21,44 SAY    " eXit           "
     SETCOLOR( aClr[4] )
     @ 18,43 SAY    " Report "
     @ 20,44 SAY    " Product List   "
     SETCOLOR( aClr[3] )
     @ 18,52 SAY            "I"
     @ 18,61 SAY                     "Q"
     @ 21,46 SAY      "X"
     SETCOLOR( aClr[5] )
     @ 18,44 SAY     "R"
     @ 20,45 SAY     "P"
     SETCOLOR( aClr[2] )
     @ 24,41 SAY PadC( "Inventory Report", 26 )

   CASE aOpt[ C_TYPE ] == "G"    // Get windows
     SETCOLOR( aClr[1] )
     ClearS( 19, 41, 24, 66 )
     Single( 19, 42, 24, 65 )
     @ 20,43 SAY  "    Invoice Entry    "
     @ 21,42 SAY hb_UTF8ToStr( "├──────────────────────┤" )
     @ 22,43 SAY  "   Amount            "
     @ 23,43 SAY  "   Date              "
     SETCOLOR( aClr[2] )
     @ 22,53 SAY             "  199.95"
     SETCOLOR( aClr[5] )
     @ 23,53 SAY             "09/15/91"

   CASE aOpt[ C_TYPE ] == "W"    // Alert windows
     SETCOLOR( aClr[1] )
     ClearS( 18, 40, 24, 66 )
     Single( 18, 41, 24, 65 )
     @ 19,42 SAY  "                       "
     @ 20,42 SAY  "     Test Message      "
     @ 21,42 SAY  "                       "
     @ 22,41 SAY hb_UTF8ToStr( "├───────────────────────┤" )
     SETCOLOR( aClr[2] )
     @ 23,44 SAY  " Accept "
     SETCOLOR( aClr[5] )
     @ 23,55 SAY             " Reject "

   CASE aOpt[ C_TYPE ] == "B"    // browse windows
     SETCOLOR( aClr[1] )
     ClearS( 18, 37, 24, 70 )
     Single( 18, 38, 24, 69 )
     @ 19,39 SAY  " Cust   Name           Amount "
     @ 20,38 SAY hb_UTF8ToStr( "╞══════╤══════════════╤════════╡" )
     @ 21,39 SAY hb_UTF8ToStr(  "  312 │ Rick Shaw    │ 143.25 "  )
     @ 23,39 SAY hb_UTF8ToStr(  "      │              │        "  )
     @ 24,38 SAY hb_UTF8ToStr( "╘══════╧══════════════╧════════╛" )
     SETCOLOR( aClr[2] )
     @ 22,39 SAY hb_UTF8ToStr(  " 1005 │ Harry Pitts  │  78.95 "  )
     SETCOLOR( aClr[5] )
     @ 23,39 SAY  " 3162 "
     @ 23,46 SAY         " Barb Wire    "
     @ 23,61 SAY                        " 345.06 "

   CASE aOpt[ C_TYPE ] == "A"    // achoice type window
     SETCOLOR( aClr[1] )
     ClearS( 18, 42, 24, 64 )
     Single( 18, 43, 24, 63 )
     @ 19,44 SAY  " Daily Reports     "
     @ 21,44 SAY  " Quarterly Reports "
     @ 23,44 SAY  " Exit ...   <Esc>  "
     SETCOLOR( aClr[2] )
     @ 20,44 SAY  " Monthend Reports  "
     SETCOLOR( aClr[5] )
     @ 22,44 SAY  " Yearend Reports   "

ENDCASE
DispEnd()

RETURN Nil

*------------------------------------------------
STATIC FUNCTION _ftClrSel( aClrPal, cClr, nElem, aOpt )
// select the colour combination from aClrPal and place in cClr
// cClr is the current colour being modified
// Return -> selected colour combination

LOCAL nR
LOCAL nC     := 1
LOCAL lFound := .F.
LOCAL nKey
LOCAL nDim   := LEN( aClrPal )
LOCAL nTop    := 0
LOCAL nLeft   := 28
LOCAL nBottom := nTop  + nDim + 1
LOCAL nRight  := nLeft + ( nDim * 3 ) + 2

SETCOLOR( "GR+/N" )
Double( nTop, nLeft, nBottom, nRight )

SETCOLOR ( "W+/N" )

*.... find the starting row and column for the current colour
FOR nR := 1 TO nDim
  FOR nC := 1 TO nDim
    IF aClrPal[ nR, nC ] == ALLTRIM( cClr )
      lFound := .T. ;  EXIT
    ENDIF
  NEXT
  IF lFound ;  EXIT ;  ENDIF
NEXT

IF ! lFound
  nR := 1                         // black background
  nC := iif( nDim == 5, 3, 8 )    // white foreground
ENDIF

DO WHILE .T.

  *.... make sure array boundary not exceeded
  nR := iif( nR > nDim, 1, iif( nR == 0, nDim, nR ) )
  nC := iif( nC > nDim, 1, iif( nC == 0, nDim, nC ) )

  *.... place selected colour in the appropriate spot in clr string
  aOpt[ C_CLR ] := _ftClrPut( aOpt[ C_CLR ], nElem, aClrPal[ nR, nC ] )

  *.... show sample window
  _ftShowIt( aOpt )

  *.... highlight the colour palette element
  SETCOLOR ( "W+/N" )
  @ nR, nC*3+26 SAY ""
  @ nR, nC*3+29 SAY ""
  nKey := INKEY(0)
  @ nR, nC*3+26 SAY " "
  @ nR, nC*3+29 SAY " "

  *.... check key movement and modify co-ordinates
  DO CASE
     CASE nKey == K_ESC   ;  EXIT
     CASE nKey == K_ENTER ;  cClr := aClrPal[ nR, nC ] ;  EXIT
     CASE nKey == K_UP    ;  --nR
     CASE nKey == K_DOWN  ;  ++nR
     CASE nKey == K_LEFT  ;  --nC
     CASE nKey == K_RIGHT ;  ++nC
  ENDCASE

ENDDO

SETCOLOR( "GR+/N" )
Single( nTop, nLeft, nBottom, nRight )

RETURN cClr

*------------------------------------------------
STATIC FUNCTION _ftClrPut( cClrStr, nElem, cClr )
// Place a colour setting in the colour string
// Return -> modified colour string

LOCAL aClr := _ftChr2Arr( cClrStr )

aClr[ nElem ] := cClr

RETURN _ftArr2Chr( aClr )

*------------------------------------------------
STATIC FUNCTION _ftDeskChar( aOpt )
// Select the character to be used for the desktop background
// Return -> same array with new character

LOCAL aChar := { CHR(32), CHR(176), CHR(177), CHR(178) }
LOCAL cChar := aOpt[ C_CHAR ]
LOCAL cClr  := aOpt[ C_CLR ]
LOCAL nElem := aScan( aChar, cChar )
LOCAL n, nKey

IF nElem == 0            // this allows another character to be selected
  aAdd( aChar, cChar )   // but there is the possibility that it will
  nElem := 5             // not be available if they ever select another
ENDIF                    // char and store it. It's up to you to put it in

*.... draw the choices on the screen
SETCOLOR ( cClr )
FOR n := 1 TO LEN( aChar )
  @ n+18, 29 SAY REPL( aChar[n], 10 )
NEXT

n := nElem + 18
DO WHILE .T.
  *.... make sure boundary not exeeded
  n := iif( n > Len(aChar)+18, 19, iif( n < 19, Len(aChar)+18, n ) )

  *.... show sample window
  aOpt[ C_CHAR ] := aChar[ n-18 ] // place in array
  _ftShowIt( aOpt )

  SETCOLOR ( "W+/N" )
  @ n, 28 SAY ""
  @ n, 39 SAY ""
  nKey := INKEY(0)
  @ n, 28 SAY " "
  @ n, 39 SAY " "

  *.... check key movement and modify co-ordinates
  DO CASE
     CASE nKey == K_ESC   ;  aOpt[ C_CHAR ] := cChar ;  EXIT
     CASE nKey == K_ENTER ;  EXIT
     CASE nKey == K_UP    ;  --n
     CASE nKey == K_DOWN  ;  ++n
  ENDCASE

ENDDO

SETCOLOR ( "W+/N" )
ClearS( 18, 28, 23, 39 )

RETURN aOpt

*------------------------------------------------
STATIC FUNCTION _ftChr2Arr( cString, cDelim )
// Convert a chr string to an array
// Return -> array

LOCAL n, aArray := {}

DEFAULT cDelim  TO ","
DEFAULT cString TO ""  // this should really be passed
cString += cDelim

DO WHILE .T.
  IF EMPTY( cString ) ;  EXIT ;  ENDIF
  n := AT( cDelim, cString )
  AADD( aArray, iif( n == 1, "", LEFT( cString, n - 1 ) ) )
  cString := SUBS( cString, n + 1 )
ENDDO

RETURN aArray

*------------------------------------------------
STATIC FUNCTION _ftArr2Chr( aArray, cDelim )
// convert an array to a chr string
// Return -> string

LOCAL cString := ""

DEFAULT aArray TO {}
DEFAULT cDelim TO ","

AEVAL( aArray, { |v,e| cString += iif( e == 1, v, cDelim + v ) } )

RETURN cString

*------------------------------------------------
STATIC FUNCTION _ftShowPal( aClrPal, cChr )
// Paint the palette on the screen
// Return -> Nil

LOCAL nF,nB
LOCAL nTop    := 0
LOCAL nLeft   := 28
LOCAL nBottom := nTop  + LEN( aClrPal ) + 1
LOCAL nRight  := nLeft + ( LEN( aClrPal )*3 ) + 2

*.... Buffer the screen output
DispBegin()
Single( nTop, nLeft, nBottom, nRight )
FOR nF := 1 TO LEN( aClrPal )
  FOR nB := 1 TO  LEN( aClrPal[ nF ] )
    SETCOLOR( aClrPal[ nF, nB ] )
    @ nF, nB*3+27 SAY cChr
  NEXT
NEXT
DispEnd()

RETURN Nil

*------------------------------------------------
STATIC FUNCTION _ftInitPal( aClrTab )
// Initialise the colour palette based on the passed colour table aClrTab
// Load the palette with colours
// Return -> Colour pallette array

LOCAL nF,nB
LOCAL nDim    := LEN( aClrTab )
LOCAL aClrPal := ARRAY( nDim*2, nDim*2 )

FOR nF := 1 TO nDim*2
  FOR nB := 1 TO nDim*2
    aClrPal[ nF, nB ] :=;
      iif( nF <= nDim, aClrTab[ nF ], aClrTab[ nF-nDim ] +"+" ) +"/"+;
      iif( nB <= nDim, aClrTab[ nB ], aClrTab[ nB-nDim ] +"*" )
  NEXT
NEXT

RETURN aClrPal

*------------------------------------------------
STATIC FUNCTION _ftIdentArr( aArr1, aArr2 )
// Compares the contents of 2 arrays
// Return -> logical

LOCAL lIdentical := LEN(aArr1) == LEN(aArr2)
LOCAL n := 1

DO WHILE lIdentical .AND. n <= LEN(aArr1)
  IF VALTYPE( aArr1[n] ) == VALTYPE( aArr2[n] )
    lIdentical := iif( VALTYPE( aArr1[n] ) == "A",     ;
                       _ftIdentArr( aArr1[n], aArr2[n] ), ;
                       aArr1[n] == aArr2[n] )
  ELSE
    lIdentical := .f.
  ENDIF
  n++
ENDDO

RETURN lIdentical
