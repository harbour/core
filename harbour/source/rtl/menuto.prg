/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PROMPT/MENU TO commands
 *
 * Released to Public Domain by Phil Barnett <philb@iag.net>
 * www - http://www.harbour-project.org
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    Changes for higher Clipper compatibility
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    __ATPROMPT() documentation
 *    __MENUTO() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: Recursive use is supported */

#include "color.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbmemvar.ch"
#include "setcurs.ch"

static s_aLevel   := {}
static s_nPointer := 1

/*  $DOC$
 *  $FUNCNAME$
 *      __AtPrompt() (@...PROMPT command)
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Display a menu item on screen and define a message
 *  $SYNTAX$
 *      __AtPrompt( <nRow>, <nCol>, <cPrompt>, [<xMsg>] ) --> .F.
 *
 *      or
 *
 *      @ <nRow>, <nCol> PROMPT <cPrompt> [MESSAGE <xMsg>]
 *  $ARGUMENTS$
 *      <nRow> is the row number to display the menu <cPrompt>. Value could
 *      range from zero to MAXROW().
 *
 *      <nCol> is the column number to display the menu <cPrompt>. Value
 *      could range from zero to MAXCOL().
 *
 *      <cPrompt> is the menu item character string to display.
 *
 *      <xMsg> define a message to display each time this menu item is
 *      highlighted. <xMsg> could be a character string or code block that
 *      is evaluated to a character string. If <xMsg> is not specified or
 *      got the wrong type, an empty string ("") would be used.
 *  $RETURNS$
 *      __AtPrompt() always return .F.
 *  $DESCRIPTION$
 *      With __AtPrompt() you define and display a menu item, each call to
 *      __AtPrompt() add another item to the menu, to start the menu itself
 *      you should call the __MenuTo() function (MENU TO command). You can
 *      define any row and column combination and they will be displayed at
 *      the order of definition. After each call to __AtPrompt(), the cursor
 *      is placed one column to the right of the last text displayed, and
 *      ROW() and COL() are updated.
 *
 *      @...PROMPT command is preprocessed into __AtPrompt() function during
 *      compile time.
 *  $EXAMPLES$
 *      // display a two line menu with status line at the bottom
 *      // let the user select favorite day
 *      SET MESSAGE TO 24 CENTER
 *      @ 10, 2 PROMPT "Sunday" MESSAGE "This is the 1st item"
 *      @ 11, 2 PROMPT "Monday" MESSAGE "Now we're on the 2nd item"
 *      MENU TO nChoice
 *      DO CASE
 *         CASE nChoice == 0           // user press Esc key
 *              QUIT
 *         CASE nChoice == 1           // user select 1st menu item
 *              ? "Guess you don't like Mondays"
 *         CASE nChoice == 2           // user select 2nd menu item
 *              ? "Just another day for some"
 *      ENDCASE
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      CA-Clipper array is limited to 4096 items, and therefor 4096 menu
 *      items are the maximum that could be defined per one menu, Harbour
 *      does not have this limit (not that you'll ever need that).
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      ACHOICE(), MENU TO, SET MESSAGE, SET INTENSITY, SET WRAP, __MenuTo()
 *  $END$
 */

function __AtPrompt( nRow, nCol, cPrompt, cMsg )

   if s_nPointer < 1
      s_nPointer := 1
   endif

   // add the current level empty array.
   do while len( s_aLevel ) < s_nPointer
      aadd( s_aLevel, {} )
   enddo

   // add to the static array
   aadd( s_aLevel[ s_nPointer ], { nRow, nCol, cPrompt, cMsg } )

   // put this prompt on the screen right now
   DispOutAt( nRow, nCol, cPrompt )

   return .f.

/*  $DOC$
 *  $FUNCNAME$
 *      __MenuTo() (MENU TO command)
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Invoked a menu defined by set of @...PROMPT
 *  $SYNTAX$
 *      __MenuTo( <bBlock>, <cVariable> ) --> nChoice
 *
 *      or
 *
 *      MENU TO <cVariable>
 *  $ARGUMENTS$
 *      <bBlock> is a set/get code block for variable named <cVariable>.
 *
 *      <cVariable> is a character string that contain the name of the
 *      variable to hold the menu choices, if this variable does not exist
 *      a PRIVATE variable with the name <cVariable> would be created to
 *      hold the result.
 *  $RETURNS$
 *      __MenuTo() return the number of select menu item, or 0 if there was
 *      no item to select from or if the user pressed the Esc key.
 *  $DESCRIPTION$
 *      __MenuTo() invoked the menu define by previous __AtPrompt() call
 *      and display a highlight bar that the user can move to select an
 *      option from the menu. If <cVariable> does not exist or not visible,
 *      a PRIVATE variable named <cVariable> is created and hold the current
 *      menu selection. If there is a variable named <cVariable>, its value
 *      is used to select the first highlighted item.
 *
 *      Menu prompts and messages are displayed in current Standard color,
 *      highlighted bar is displayed using current Enhanced color.
 *
 *      Pressing the arrow keys move the highlighted bar. When a menu item
 *      is highlighted the message associated with it is displayed on the
 *      line specified with SET MESSAGE. If SET WRAP is ON and the user
 *      press UP arrow while on the first selection the last menu item is
 *      highlighted, if the user press Down arrow while on the last item,
 *      the first item is highlighted.
 *
 *      Following are active keys that handled by __MenuTo():
 *      -----------------------------------------------------
 *
 *      Up             - Move to previous item
 *      Down           - Move to next item
 *      Left           - Move to previous item
 *      Right          - Move to next item
 *      Home           - Move to the first item
 *      End            - Move to the last item
 *      Page-Up        - Select menu item, return position
 *      Page-Down      - Select menu item, return position
 *      Enter          - Select menu item, return position
 *      Esc            - Abort selection, return 0
 *      First letter   - Select next menu with the same first letter,
 *                       return this item position.
 *
 *      upon exit the cursor is placed at MAXROW()-1, 0
 *      __MenuTo() can be nested without loosing the previous prompts.
 *
 *      MENU TO command is preprocessed into __MenuTo() function during
 *      compile time.
 *  $EXAMPLES$
 *      // display menu item on each screen corner and let user select one
 *      CLS
 *      SET MESSAGE TO MAXROW()/2 CENTER
 *      SET WRAP ON
 *      @ 0,         0           PROMPT "1. Upper left"   MESSAGE " One "
 *      @ 0,         MAXCOL()-16 PROMPT "2. Upper right"  MESSAGE " Two "
 *      @ MAXROW()-1,MAXCOL()-16 PROMPT "3. Bottom right" MESSAGE "Three"
 *      @ MAXROW()-1,0           PROMPT "4. Bottom left"  MESSAGE "Four "
 *      MENU TO nChoice
 *      SETPOS ( MAXROW()/2, MAXCOL()/2 - 10 )
 *      if nChoice == 0
 *         ?? "Esc was pressed"
 *      else
 *         ?? "Selected option is", nChoice
 *      endif
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      @...PROMPT, ACHOICE(), SET MESSAGE, SET INTENSITY, SET WRAP,
 *      __AtPrompt()
 *  $END$
 */

function __MenuTo( bBlock, cVariable )

   local nKey
   local y
   local q
   local n
   local lExit
   local nArrLen
   local xMsg
   local nMsgCol
   local nMsgRow
   local lMsgCenter
   local nSaveCursor
   local cSaveReadVar

   local lDeclared
   local bAction

   // Detect if a memvar was passed

   if __mvSCOPE( cVariable ) <= HB_MV_ERROR
      __mvPUBLIC( cVariable )
      lDeclared := .T.
   else
      lDeclared := .F.
      n := eval( bBlock )
   endif

   // if no prompts were defined, exit with 0

   if s_nPointer < 1 .or. s_nPointer > len( s_aLevel )

      n := 0

   else

      s_nPointer ++

      nArrLen := len( s_aLevel[ s_nPointer - 1 ] )

      // put choice in a valid range

      if !ISNUMBER( n ) .OR. n < 1
         n := 1
      endif

      if n > nArrLen
         n := nArrLen
      endif

      //

      nSaveCursor := setcursor( iif( Set( _SET_INTENSITY ), SC_NONE, NIL ) )
      cSaveReadVar := ReadVar( upper( cVariable ) )
      xMsg := ""
      nMsgCol := 0
      nMsgRow := set( _SET_MESSAGE )
      lMsgCenter := set( _SET_MCENTER )
      lExit := .F.


      do while n <> 0

         // should we display messages?
         if nMsgRow > 0

            if ! Empty( xMsg )
               DispOutAt( nMsgRow, nMsgCol, Space( Len( xMsg ) ) )
            endif

            xMsg := s_aLevel[ s_nPointer - 1, n, 4 ]

            // Code Block messages ( yes, they are documented! )
            if ISBLOCK( xMsg )
               xMsg := eval( xMsg )
            endif

            if !ISCHARACTER( xMsg )
               xMsg := ""
            endif

            if lMsgCenter
               nMsgCol := int( ( maxcol() - len( xMsg ) ) / 2 )
            endif

            DispOutAt( nMsgRow, nMsgCol, xMsg )

         endif

         // save the current row
         q := n

         if Set( _SET_INTENSITY )
            ColorSelect( CLR_ENHANCED )
         endif

         // highlight the prompt
         DispOutAt( s_aLevel[ s_nPointer - 1, n, 1 ],;
                    s_aLevel[ s_nPointer - 1, n, 2 ],;
                    s_aLevel[ s_nPointer - 1, n, 3 ] )

         if Set( _SET_INTENSITY )
            ColorSelect( CLR_STANDARD )
         endif

         if lExit
            exit
         endif

         nKey := 0
         do while nKey == 0

            // wait for a keystroke
            nKey := inkey( 0 )

            if ( bAction := setkey( nKey ) ) <> NIL

               eval( bBlock, n )
               eval( bAction, procname( 1 ), procline( 1 ), upper( cVariable ) )
               n := eval( bBlock )

               if n < 1
                  n := 1
               elseif n > nArrLen
                  n := nArrLen
               endif

               nKey := 0

            endif
         enddo

         // check for keystrokes
         do case
         case nKey == K_DOWN .or. nKey == K_RIGHT
            if ++n > nArrLen
               n := iif( Set( _SET_WRAP ), 1, nArrLen )
            endif
         case nKey == K_UP .or. nKey == K_LEFT
            if --n < 1
               n := iif( Set( _SET_WRAP ), nArrLen, 1 )
            endif
         case nKey == K_HOME
            n := 1
         case nKey == K_END
            n := nArrLen
         case nKey == K_ENTER .or. nKey == K_PGUP .or. nKey == K_PGDN
            lExit := .T.
         case nKey == K_ESC
            n := 0
         otherwise
            // did user hit a hot key?
            for y := 1 to nArrLen
               if upper( left( ltrim( s_aLevel[ s_nPointer - 1, y, 3 ] ), 1 ) ) == upper( chr( nKey ) )
                  n := y
                  lExit := .T.
                  exit
               endif
            next
         endcase

         if n <> 0
            DispOutAt( s_aLevel[ s_nPointer - 1, q, 1 ],;
                       s_aLevel[ s_nPointer - 1, q, 2 ],;
                       s_aLevel[ s_nPointer - 1, q, 3 ] )
         endif

      enddo

      ReadVar( cSaveReadVar )
      SetCursor( nSaveCursor )

      s_nPointer --
      asize( s_aLevel, s_nPointer - 1 )

   endif

   eval( bBlock, n )

   if lDeclared
      __mvXRELEASE( cVariable )
   endif

   SetPos( MaxRow() - 1, 0)

   return n

