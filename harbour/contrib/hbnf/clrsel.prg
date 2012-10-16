/*
 * $Id$
 */

/*
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

//------------------------------------------------
// Pre-processor stuff

#include "setcurs.ch"
#include "inkey.ch"

#define C_NAME   1
#define C_CLR    2
#define C_TYPE   3
#define C_CHAR   4

#translate Single( <t>, <l>, <b>, <r> ) =>;
      hb_DispBox( <t>, <l>, <b>, <r>, hb_UTF8ToStrBox( "┌─┐│┘─└│" ) )

#translate Double( <t>, <l>, <b>, <r> ) =>;
      hb_DispBox( <t>, <l>, <b>, <r>, hb_UTF8ToStrBox( "╔═╗║╝═╚║" ) )

#translate BkGrnd( <t>, <l>, <b>, <r>, <c> ) =>;
      hb_DispBox( <t>, <l>, <b>, <r>, Replicate( <c>, 9 ) )

//------------------------------------------------
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

//------------------------------------------------

FUNCTION FT_ClrSel( aClrs, lColour, cChr )

   // Colour selection routine
   // Return -> the same array that was passed but with modified colours

   LOCAL aClrOld := AClone( aClrs )
   LOCAL aOptions
   LOCAL nB, nT, nL, nR
   LOCAL nChoice := 1
   LOCAL nLen    := 0
   LOCAL aPrompt := {}
   LOCAL aClrPal
   LOCAL aClrTab := { "N", "B", "G", "BG", "R", "RB", "GR", "W" }
   LOCAL aClrBW  := { "N", "B", "W" }
   LOCAL nRowSav := Row()
   LOCAL nColSav := Col()
   LOCAL aEnvSav := FT_SaveSets()
   LOCAL cScrSav := SaveScreen( 0, 0, MaxRow(), MaxCol() )

   __defaultNIL( @lColour, IsColor() )
   __defaultNIL( @cChr, hb_UTF8ToStr( "■■" ) )

   cChr := PadR( cChr, 2 )

   SetCursor( SC_NONE )
   SetColor( iif( lColour, "GR+/N,,N/N", "W+/N,,N/N" ) )
   CLS

   //.... initialize the colour palette
   aClrPal := _ftInitPal( iif( lColour, aClrTab, aClrBW ) )

   //.... paint the colours on the screen
   _ftShowPal( aClrPal, cChr )

   //.... Determine length of longest name and make sure not greater than 20
   AEval( aClrs, {| aOpt | nLen := Max( nLen, Len( aOpt[ C_NAME ] ) ) } )
   nLen := Min( Max( nLen, 1 ), 20 ) + 2

   //.... prepare an array for use with aChoice(); truncate names at 20 chrs.
   aPrompt := Array( Len( aClrs ) )
   AEval( aClrs, ;
      {| aOpt, nE | aPrompt[ nE ] := " " + SubStr( aOpt[ C_NAME ], 1, nLen - 2 ) + " " };
      )

   //.... determine co-ordinates for the achoice window
   nT := Max( Int( ( 18 - Len( aPrompt ) ) / 2 ) - 1, 1 )
   nB := Min( nT + Len( aPrompt ) + 1, 17 )
   nL := Max( Int( ( 27 - nLen ) / 2 ) - 2, 1 )
   nR := Min( nL + nLen + 3, 26 )

   //.... set up the window for aChoice
   SetColor( iif( lColour, "N/W,W+/R", "N/W,W+/N" ) )
   hb_Scroll( nT, nL, nB, nR )

   //.... prompt for colour setting and modify
   DO WHILE nChoice != 0
      Double( nT, nL + 1, nB, nR - 1 )
      nChoice := AChoice( nt + 1, nL + 2, nB - 1, nR - 2, aPrompt, , , nChoice )
      IF nChoice != 0
         _ftHiLite( Row(), nL + 2, aPrompt[ nChoice ], nLen )
         Single( nT, nL + 1, nB, nR - 1 )
         aClrs[ nChoice ] := _ftColours( aClrs[ nChoice ], aClrPal, lColour )
      ENDIF
   ENDDO

   aOptions := { "Save New Colours", "Restore Original" }
   IF ! _ftIdentArr( aClrs, aClrOld )
      nChoice := Alert( "Colors have been modified...", aOptions )
   ELSE
      nChoice := 1
   ENDIF

   FT_RestSets( aEnvSav )
   RestScreen( 0, 0, MaxRow(), MaxCol(), cScrSav )
   SetPos( nRowSav, nColSav )

   RETURN iif( nChoice == 1, aClrs, aClrOld )

//------------------------------------------------

// Highlight the current selected aChoice element
// Return -> NIL

STATIC FUNCTION _ftHiLite( nRow, nCol, cStr, nLen )

   LOCAL aClr := _ftChr2Arr( SetColor() )

   hb_DispOutAt( nRow, nCol, PadR( cStr, nLen ), aClr[ 2 ] ) // enhanced colour

   RETURN NIL

//------------------------------------------------

// Colour selection for specific type of colour setting
// Return -> aOpt with modified colour strings

STATIC FUNCTION _ftColours( aOpt, aClrPal, lColour )

   LOCAL nB, nT, nL, nR
   LOCAL nX
   LOCAL aClrs   := {}
   LOCAL cClr
   LOCAL nChoice := 1
   LOCAL aPrompt
   LOCAL nLen    := 0
   LOCAL cColour := SetColor()
   LOCAL cScrSav := SaveScreen( 18, 0, MaxRow(), MaxCol() )

   ASize( aOpt, 4 )                            // check incoming parameters
   __defaultNIL( @aOpt[ C_CHAR ], "" )
   __defaultNIL( @aOpt[ C_TYPE ], "W" )
   aOpt[ C_CLR ]  := Upper( aOpt[ C_CLR ] )    // need upper case
   aOpt[ C_TYPE ] := Upper( aOpt[ C_TYPE ] )

   __defaultNIL( @lColour, IsColor() )

   //.... display appropriate prompts based on type of colour setting
   nChoice := 1
   DO CASE
   CASE aOpt[ C_TYPE ] == "D"
      aPrompt := { " Color ", " Character " }
   CASE aOpt[ C_TYPE ] == "M"
      aPrompt := { " Prompt ", " Message ", " HotKey ", ;
         " LightBar ", " LightBar HotKey " }
   CASE aOpt[ C_TYPE ] == "A" .OR.  aOpt[ C_TYPE ] == "B"
      aPrompt := { " Standard ", " Selected ", " Border ", " Unavailable " }
   OTHERWISE
      aPrompt := { " Standard ", " Selected ", " Border ", " Unselected " }
   ENDCASE

   IF !( aOpt[ C_TYPE ] == "T" )  // no prompt for titles
      //.... we need to know top,left,bottom,right for the prompt window
      AEval( aPrompt, {| cPrompt | nLen := Max( nLen, Len( cPrompt ) ) } )
      nLen := Max( nLen, Len( aOpt[ C_NAME ] ) + 2 )
      nT := iif( aOpt[ C_TYPE ] == "M", 18, 19 )
      nB := nT + Len( aPrompt ) + 1
      nL := Max( Int( ( 27 - nLen ) / 2 ) - 2, 1 )
      nR := Min( nL + nLen + 3, 26 )

      //.... set up the window for prompt
      SetColor( "N/W" )
      hb_Scroll( nT, nL, nB, nR )
   ENDIF

   DO WHILE .T.

      //.... show sample window
      _ftShowIt( aOpt )

      IF !( aOpt[ C_TYPE ] == "T" )  // no prompt for titles
         SetColor( iif( lColour, "N/W,W+/R,,,N/W", "N/W,W+/N,,,N/W" ) )
         Double( nT, nL + 1, nB, nR - 1 )
         hb_DispOutAt( nT, nL + 2, PadC( " " + aOpt[ C_NAME ] + " ", nR - nL - 3, hb_UTF8ToStr( "═" ) ) )
         FOR nX := 1 TO Len( aPrompt )
            @ nX + nT, nL + 2 PROMPT PadR( aPrompt[ nX ], nR - nL - 3 )
         NEXT
         MENU TO nChoice

         DO CASE
         CASE nChoice == 0
            EXIT
         CASE nChoice == 2 .AND. aOpt[ C_TYPE ] == "D"
            //....  desktop character
            aOpt := _ftDeskChar( aOpt )
            LOOP
         CASE nChoice == 4 .AND. !( aOpt[ C_TYPE ] == "M" )
            nChoice := 5      // 4th color param is unused
         ENDCASE
      ENDIF

      //.... get the specific colour combination
      aClrs := _ftChr2Arr( aOpt[ C_CLR ] )   // place color string in an array
      ASize( aClrs, 5 )                      // make sure there are 5 settings
      //.... empty elements are made NIL so they can be defaulted
      AEval( aClrs, {| v, e | aClrs[ e ] := iif( Empty( v ), NIL, AllTrim( v ) ) } )
      __defaultNIL( @aClrs[ 1 ], "W/N" )
      __defaultNIL( @aClrs[ 2 ], "N/W" )  // place default colours into
      __defaultNIL( @aClrs[ 3 ], "N/N" )  // elements which are empty
      __defaultNIL( @aClrs[ 4 ], "N/N" )
      __defaultNIL( @aClrs[ 5 ], "N/W" )
      cClr := aClrs[ nChoice ]    // selected colour

      //.... allow change to specific part of colour string
      IF !( aOpt[ C_TYPE ] == "T" )
         Single( nT, nL + 1, nB, nR - 1 )
         hb_DispOutAt( nT, nL + 2, PadC( " " + aOpt[ C_NAME ] + " ", nR - nL - 3, hb_UTF8ToStr( "─" ) ) )
      ENDIF
      cClr := _ftClrSel( aClrPal, cClr, nChoice, aOpt )  //  selection routine
      aClrs[ nChoice ] := cClr               // put colour back in array
      aOpt[ C_CLR ] := _ftArr2Chr( aClrs )   // convert array to colour string

      IF aOpt[ C_TYPE ] == "T"
         EXIT
      ENDIF

   ENDDO

   //.... restore the lower 1/2 of screen, and colour
   RestScreen( 18, 00, MaxRow(), MaxCol(), cScrSav )
   SetColor( cColour )

   RETURN aOpt

//------------------------------------------------

// Show an example of the colour setting
// Return -> NIL

STATIC FUNCTION _ftShowIt( aOpt )

   LOCAL aClr := _ftChr2Arr( aOpt[ C_CLR ] )

   IF !( aOpt[ C_TYPE ] == "M" ) // no borders in menu colour selection
      SetColor( aOpt[ C_CLR ] )  // this will set the border on VGA
   ENDIF

   DispBegin()

   SWITCH aOpt[ C_TYPE ]

   CASE "D"    // Desktop Background
      SetColor( aClr[ 1 ] )
      BkGrnd( 19, 43, 22, 64, hb_UTF8ToStrBox( aOpt[ C_CHAR ] ) )
      EXIT

   CASE "T"    // Title
      SetColor( aClr[ 1 ] )
      hb_DispOutAt( 20, 08, PadC( "This is an example of how the text shall look", 63 ) )
      EXIT

   CASE "M"    // Menus
      SetColor( "W/N" )
      BkGrnd( 19, 41, 23, 66, hb_UTF8ToStrBox( "▒" ) )
      SetColor( aClr[ 1 ] )
      Single( 19, 43, 22, 60 )
      hb_DispOutAt( 18, 41, "   Report  Inquiry  Quit  " )
      hb_DispOutAt( 21, 44,    " eXit           " )
      SetColor( aClr[ 4 ] )
      hb_DispOutAt( 18, 43, " Report " )
      hb_DispOutAt( 20, 44, " Product List   " )
      SetColor( aClr[ 3 ] )
      hb_DispOutAt( 18, 52, "I" )
      hb_DispOutAt( 18, 61, "Q" )
      hb_DispOutAt( 21, 46, "X" )
      SetColor( aClr[ 5 ] )
      hb_DispOutAt( 18, 44, "R" )
      hb_DispOutAt( 20, 45, "P" )
      SetColor( aClr[ 2 ] )
      hb_DispOutAt( 24, 41, PadC( "Inventory Report", 26 ) )
      EXIT

   CASE "G"    // Get windows
      SetColor( aClr[ 1 ] )
      hb_Scroll( 19, 41, 24, 66 )
      Single( 19, 42, 24, 65 )
      hb_DispOutAt( 20, 43, "    Invoice Entry    " )
      hb_DispOutAt( 21, 42, hb_UTF8ToStr( "├──────────────────────┤" ) )
      hb_DispOutAt( 22, 43, "   Amount            " )
      hb_DispOutAt( 23, 43, "   Date              " )
      SetColor( aClr[ 2 ] )
      hb_DispOutAt( 22, 53, "  199.95" )
      SetColor( aClr[ 5 ] )
      hb_DispOutAt( 23, 53, "09/15/91" )
      EXIT

   CASE "W"    // Alert windows
      SetColor( aClr[ 1 ] )
      hb_Scroll( 18, 40, 24, 66 )
      Single( 18, 41, 24, 65 )
      hb_DispOutAt( 19, 42, "                       " )
      hb_DispOutAt( 20, 42, "     Test Message      " )
      hb_DispOutAt( 21, 42, "                       " )
      hb_DispOutAt( 22, 41, hb_UTF8ToStr( "├───────────────────────┤" ) )
      SetColor( aClr[ 2 ] )
      hb_DispOutAt( 23, 44, " Accept " )
      SetColor( aClr[ 5 ] )
      hb_DispOutAt( 23, 55, " Reject " )
      EXIT

   CASE "B"    // browse windows
      SetColor( aClr[ 1 ] )
      hb_Scroll( 18, 37, 24, 70 )
      Single( 18, 38, 24, 69 )
      hb_DispOutAt( 19, 39,                " Cust   Name           Amount " )
      hb_DispOutAt( 20, 38, hb_UTF8ToStr( "╞══════╤══════════════╤════════╡" ) )
      hb_DispOutAt( 21, 39, hb_UTF8ToStr(  "  312 │ Rick Shaw    │ 143.25 "  ) )
      hb_DispOutAt( 23, 39, hb_UTF8ToStr(  "      │              │        "  ) )
      hb_DispOutAt( 24, 38, hb_UTF8ToStr( "╘══════╧══════════════╧════════╛" ) )
      SetColor( aClr[ 2 ] )
      hb_DispOutAt( 22, 39, hb_UTF8ToStr(  " 1005 │ Harry Pitts  │  78.95 "  ) )
      SetColor( aClr[ 5 ] )
      hb_DispOutAt( 23, 39,                " 3162 " )
      hb_DispOutAt( 23, 46,                       " Barb Wire    " )
      hb_DispOutAt( 23, 61,                                      " 345.06 " )
      EXIT

   CASE "A"    // achoice type window
      SetColor( aClr[ 1 ] )
      hb_Scroll( 18, 42, 24, 64 )
      Single( 18, 43, 24, 63 )
      hb_DispOutAt( 19, 44, " Daily Reports     " )
      hb_DispOutAt( 21, 44, " Quarterly Reports " )
      hb_DispOutAt( 23, 44, " Exit ...   <Esc>  " )
      SetColor( aClr[ 2 ] )
      hb_DispOutAt( 20, 44, " Monthend Reports  " )
      SetColor( aClr[ 5 ] )
      hb_DispOutAt( 22, 44, " Yearend Reports   " )
      EXIT

   ENDSWITCH

   DispEnd()

   RETURN NIL

//------------------------------------------------

// select the colour combination from aClrPal and place in cClr
// cClr is the current colour being modified
// Return -> selected colour combination

STATIC FUNCTION _ftClrSel( aClrPal, cClr, nElem, aOpt )

   LOCAL nR
   LOCAL nC     := 1
   LOCAL lFound := .F.
   LOCAL nKey
   LOCAL nDim   := Len( aClrPal )
   LOCAL nTop    := 0
   LOCAL nLeft   := 28
   LOCAL nBottom := nTop  + nDim + 1
   LOCAL nRight  := nLeft + ( nDim * 3 ) + 2

   SetColor( "GR+/N" )
   Double( nTop, nLeft, nBottom, nRight )

   SetColor( "W+/N" )

   //.... find the starting row and column for the current colour
   FOR nR := 1 TO nDim
      FOR nC := 1 TO nDim
         IF aClrPal[ nR, nC ] == AllTrim( cClr )
            lFound := .T.
            EXIT
         ENDIF
      NEXT
      IF lFound
         EXIT
      ENDIF
   NEXT

   IF ! lFound
      nR := 1                         // black background
      nC := iif( nDim == 5, 3, 8 )    // white foreground
   ENDIF

   DO WHILE .T.

      //.... make sure array boundary not exceeded
      nR := iif( nR > nDim, 1, iif( nR == 0, nDim, nR ) )
      nC := iif( nC > nDim, 1, iif( nC == 0, nDim, nC ) )

      //.... place selected colour in the appropriate spot in clr string
      aOpt[ C_CLR ] := _ftClrPut( aOpt[ C_CLR ], nElem, aClrPal[ nR, nC ] )

      //.... show sample window
      _ftShowIt( aOpt )

      //.... highlight the colour palette element
      SetColor( "W+/N" )
      hb_DispOutAt( nR, nC * 3 + 26, "" ) /* LOW-ASCII "►" */
      hb_DispOutAt( nR, nC * 3 + 29, "" ) /* LOW-ASCII "◄" */
      nKey := Inkey( 0 )
      hb_DispOutAt( nR, nC * 3 + 26, " " )
      hb_DispOutAt( nR, nC * 3 + 29, " " )

      //.... check key movement and modify co-ordinates
      DO CASE
      CASE nKey == K_ESC   ; EXIT
      CASE nKey == K_ENTER ; cClr := aClrPal[ nR, nC ] ; EXIT
      CASE nKey == K_UP    ; --nR
      CASE nKey == K_DOWN  ; ++nR
      CASE nKey == K_LEFT  ; --nC
      CASE nKey == K_RIGHT ; ++nC
      ENDCASE

   ENDDO

   SetColor( "GR+/N" )
   Single( nTop, nLeft, nBottom, nRight )

   RETURN cClr

//------------------------------------------------

// Place a colour setting in the colour string
// Return -> modified colour string

STATIC FUNCTION _ftClrPut( cClrStr, nElem, cClr )

   LOCAL aClr := _ftChr2Arr( cClrStr )

   aClr[ nElem ] := cClr

   RETURN _ftArr2Chr( aClr )

//------------------------------------------------

// Select the character to be used for the desktop background
// Return -> same array with new character

STATIC FUNCTION _ftDeskChar( aOpt )

   LOCAL aChar := { " ", "░", "▒", "▓" }
   LOCAL cChar := aOpt[ C_CHAR ]
   LOCAL cClr  := aOpt[ C_CLR ]
   LOCAL nElem := hb_AScan( aChar, cChar, , , .T. )
   LOCAL n, nKey

   IF nElem == 0             // this allows another character to be selected
      AAdd( aChar, cChar )   // but there is the possibility that it will
      nElem := 5             // not be available if they ever select another
   ENDIF                     // char and store it. It's up to you to put it in

   //.... draw the choices on the screen
   SetColor( cClr )
   FOR n := 1 TO Len( aChar )
      hb_DispOutAt( n + 18, 29, Replicate( hb_UTF8ToStr( aChar[ n ] ), 10 ) )
   NEXT

   n := nElem + 18
   DO WHILE .T.
      //.... make sure boundary not exeeded
      n := iif( n > Len( aChar ) + 18, 19, iif( n < 19, Len( aChar ) + 18, n ) )

      //.... show sample window
      aOpt[ C_CHAR ] := aChar[ n - 18 ] // place in array
      _ftShowIt( aOpt )

      SetColor( "W+/N" )
      hb_DispOutAt( n, 28, "" ) /* LOW-ASCII "►" */
      hb_DispOutAt( n, 39, "" ) /* LOW-ASCII "◄" */
      nKey := Inkey( 0 )
      hb_DispOutAt( n, 28, " " )
      hb_DispOutAt( n, 39, " " )

      //.... check key movement and modify co-ordinates
      DO CASE
      CASE nKey == K_ESC   ; aOpt[ C_CHAR ] := cChar ; EXIT
      CASE nKey == K_ENTER ; EXIT
      CASE nKey == K_UP    ; --n
      CASE nKey == K_DOWN  ; ++n
      ENDCASE

   ENDDO

   SetColor( "W+/N" )
   hb_Scroll( 18, 28, 23, 39 )

   RETURN aOpt

//------------------------------------------------

// Convert a chr string to an array
// Return -> array

STATIC FUNCTION _ftChr2Arr( cString, cDelim )

   LOCAL n, aArray := {}

   __defaultNIL( @cDelim, "," )
   __defaultNIL( @cString, "" )  // this should really be passed

   cString += cDelim

   DO WHILE .T.
      IF Empty( cString )
         EXIT
      ENDIF
      n := At( cDelim, cString )
      AAdd( aArray, iif( n == 1, "", Left( cString, n - 1 ) ) )
      cString := SubStr( cString, n + 1 )
   ENDDO

   RETURN aArray

//------------------------------------------------

// convert an array to a chr string
// Return -> string

STATIC FUNCTION _ftArr2Chr( aArray, cDelim )

   LOCAL cString := ""

   __defaultNIL( @aArray, {} )
   __defaultNIL( @cDelim, "," )

   AEval( aArray, {| v, e | cString += iif( e == 1, v, cDelim + v ) } )

   RETURN cString

//------------------------------------------------

// Paint the palette on the screen
// Return -> NIL

STATIC FUNCTION _ftShowPal( aClrPal, cChr )

   LOCAL nF, nB
   LOCAL nTop    := 0
   LOCAL nLeft   := 28
   LOCAL nBottom := nTop  + Len( aClrPal ) + 1
   LOCAL nRight  := nLeft + ( Len( aClrPal ) * 3 ) + 2

   //.... Buffer the screen output
   DispBegin()
   Single( nTop, nLeft, nBottom, nRight )
   FOR nF := 1 TO Len( aClrPal )
      FOR nB := 1 TO  Len( aClrPal[ nF ] )
         SetColor( aClrPal[ nF, nB ] )
         hb_DispOutAt( nF, nB * 3 + 27, cChr )
      NEXT
   NEXT
   DispEnd()

   RETURN NIL

//------------------------------------------------

// Initialise the colour palette based on the passed colour table aClrTab
// Load the palette with colours
// Return -> Colour pallette array

STATIC FUNCTION _ftInitPal( aClrTab )

   LOCAL nF, nB
   LOCAL nDim    := Len( aClrTab )
   LOCAL aClrPal := Array( nDim * 2, nDim * 2 )

   FOR nF := 1 TO nDim * 2
      FOR nB := 1 TO nDim * 2
         aClrPal[ nF, nB ] := ;
            iif( nF <= nDim, aClrTab[ nF ], aClrTab[ nF - nDim ] + "+" ) + "/" + ;
            iif( nB <= nDim, aClrTab[ nB ], aClrTab[ nB - nDim ] + "*" )
      NEXT
   NEXT

   RETURN aClrPal

//------------------------------------------------

// Compares the contents of 2 arrays
// Return -> logical

STATIC FUNCTION _ftIdentArr( aArr1, aArr2 )

   LOCAL lIdentical := Len( aArr1 ) == Len( aArr2 )
   LOCAL n := 1

   DO WHILE lIdentical .AND. n <= Len( aArr1 )
      IF ValType( aArr1[ n ] ) == ValType( aArr2[ n ] )
         lIdentical := iif( HB_ISARRAY( aArr1[ n ] ), ;
            _ftIdentArr( aArr1[ n ], aArr2[ n ] ), ;
            aArr1[ n ] == aArr2[ n ] )
      ELSE
         lIdentical := .F.
      ENDIF
      n++
   ENDDO

   RETURN lIdentical
