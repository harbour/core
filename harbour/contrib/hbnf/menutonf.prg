/*
 * $Id$
 */

/*
 * File......: menuto.prg
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.5   16 Oct 1992 00:20:28   GLENN
 * Cleaned up documentation header.
 *
 *    Rev 1.4   16 Oct 1992 00:08:44   GLENN
 * Just making sure we had Ted's latest revision.
 *
 *    Rev 1.3   13 Oct 1992 20:45:46   GLENN
 * Complete rewrite by Ted Means, dumping assembler version for a
 * Clipper version.
 *
 *    Rev 1.2   15 Aug 1991 23:03:54   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:16   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:42   GLENN
 * Nanforum Toolkit
 *
 */

#include "setcurs.ch"
#include "inkey.ch"

#xcommand if <true> then <action> => ;
          if <true> ; <action> ; end

#xtranslate display( <row>, <col>, <stuff>, <color> ) => ;
            setpos( <row>, <col> ) ; dispout( <stuff>, <color> )

#xtranslate EnhColor( <colorspec> ) => ;
            substr( <colorspec>, at( ",", <colorspec> ) + 1 )

#xtranslate isOkay( <exp> ) => ;
            ( <exp> \> 0 .and. <exp> \<= nCount )

#xtranslate isBetween( <val>, <lower>, <upper> ) => ;
            ( <val> \>= <lower> .and. <val> \<= <upper> )

#define nTriggerInkey asc( upper( substr( cPrompt, nTrigger, 1 ) ) )
#define cTrigger substr( cPrompt, nTrigger, 1 )
#define nCurrent nMenu,nActive
#define nLast nMenu,nPrev

// These arrays hold information about each menu item

THREAD static aRow          := {{}}
THREAD static aCol          := {{}}
THREAD static aPrompt       := {{}}
THREAD static aColor        := {{}}
THREAD static aMsgRow       := {{}}
THREAD static aMsgCol       := {{}}
THREAD static aMessage      := {{}}
THREAD static aMsgColor     := {{}}
THREAD static aTrigger      := {{}}
THREAD static aTriggerInkey := {{}}
THREAD static aTriggerColor := {{}}
THREAD static aHome         := {{}}
THREAD static aEnd          := {{}}
THREAD static aUp           := {{}}
THREAD static aDown         := {{}}
THREAD static aLeft         := {{}}
THREAD static aRight        := {{}}
THREAD static aExecute      := {{}}
THREAD static nLevel        := 1

function FT_Prompt( nRow,    nCol,    cPrompt,  cColor,      ;
                    nMsgRow, nMsgCol, cMessage, cMsgColor,   ;
                    nTrigger, cTriggerColor, nHome, nEnd,    ;
                    nUp, nDown, nLeft, nRight, bExecute      )

// If the prompt color setting is not specified, use default

if cColor  == NIL then cColor  := setcolor()

// If no message is supplied, set message values to NIL

if cMessage == NIL

   nMsgRow := nMsgCol := cMsgColor := NIL

else

   // If message row not supplied, use the default

   if nMsgRow == NIL then nMsgRow := set( _SET_MESSAGE )

   // If message column not supplied, use the default

   if nMsgCol == NIL
      if set( _SET_MCENTER )
         nMsgCol := int( ( maxcol() + 1 - len( cPrompt ) ) / 2 )
      else
         nMsgCol := 0
      endif
   endif

   // If message color not specified, use the default

   if cMsgColor == NIL then cMsgColor := cColor
endif

// If trigger values not specifed, set the defaults

if nTrigger       == NIL then nTrigger      := 1
if cTriggerColor  == NIL then cTriggerColor := cColor

// Now add elements to the static arrays -- nLevel indicates the recursion
// level, which allows for nested menus.

aadd(          aRow[ nLevel ], nRow          )
aadd(          aCol[ nLevel ], nCol          )
aadd(       aPrompt[ nLevel ], cPrompt       )
aadd(        aColor[ nLevel ], cColor        )
aadd(       aMsgRow[ nLevel ], nMsgRow       )
aadd(       aMsgCol[ nLevel ], nMsgCol       )
aadd(      aMessage[ nLevel ], cMessage      )
aadd(     aMsgColor[ nLevel ], cMsgColor     )
aadd(      aTrigger[ nLevel ], nTrigger      )
aadd( aTriggerInkey[ nLevel ], nTriggerInkey )
aadd( aTriggerColor[ nLevel ], cTriggerColor )
aadd(         aHome[ nLevel ], nHome         )
aadd(          aEnd[ nLevel ], nEnd          )
aadd(           aUp[ nLevel ], nUp           )
aadd(         aDown[ nLevel ], nDown         )
aadd(         aLeft[ nLevel ], nLeft         )
aadd(        aRight[ nLevel ], nRight        )
aadd(      aExecute[ nLevel ], bExecute      )

// Now display the prompt for the sake of compatibility

dispbegin()
display( nRow, nCol, cPrompt, cColor )
display( nRow, nCol - 1 + nTrigger, cTrigger, cTriggerColor )
dispend()

return NIL

function FT_MenuTo( bGetSet, cReadVar, lCold )

local nMenu   := nLevel++
local nActive
local nCount  := len( aRow[ nMenu ] )
local lChoice := .F.
local nCursor := set( _SET_CURSOR,SC_NONE )
local nKey,bKey,nScan,lWrap,cScreen,nPrev

// Validate the incoming parameters and assign some reasonable defaults
// to prevent a crash later.

cReadVar := iif( cReadVar == NIL, "", upper( cReadVar ) )

if bGetSet == NIL then bGetSet := {|| 1}

// Eval the incoming getset block to initialize nActive, which indicates
// the menu prompt which is to be active when the menu is first displayed.
// If nActive is outside the appropriate limits, a value of 1 is assigned.

nActive := eval( bGetSet )

if ( nActive < 1 .or. nActive > nCount ) then nActive := 1

// Increment the recursion level in case a hotkey procedure
// calls FT_Prompt().  This will cause a new set of prompts
// to be created without disturbing the current set.

aadd(          aRow, {} )
aadd(          aCol, {} )
aadd(       aPrompt, {} )
aadd(        aColor, {} )
aadd(       aMsgRow, {} )
aadd(       aMsgCol, {} )
aadd(      aMessage, {} )
aadd(     aMsgColor, {} )
aadd(      aTrigger, {} )
aadd( aTriggerInkey, {} )
aadd( aTriggerColor, {} )
aadd(           aUp, {} )
aadd(         aDown, {} )
aadd(         aLeft, {} )
aadd(        aRight, {} )
aadd(      aExecute, {} )

// Loop until Enter or Esc is pressed

while .not. lChoice

   // Evaluate the getset block to update the target memory variable
   // in case it needs to be examined by a hotkey procedure.

   eval( bGetSet,nActive )

   // Get the current setting of SET WRAP so that the desired menu behavior
   // can be implemented.

   lWrap := set( _SET_WRAP )

   // If a message is to be displayed, save the current screen contents
   // and then display the message, otherwise set the screen buffer to NIL.

   dispbegin()

   if aMessage[ nCurrent ] != NIL
      cScreen := savescreen( aMsgRow[ nCurrent ], aMsgCol[ nCurrent ],  ;
                             aMsgRow[ nCurrent ], aMsgCol[ nCurrent ] + ;
                       len( aMessage[ nCurrent ] ) - 1 )

      display( aMsgRow[ nCurrent ],   aMsgCol[ nCurrent ], ;
              aMessage[ nCurrent ], aMsgColor[ nCurrent ]  )

   else
      cScreen := NIL
   endif

   // Display the prompt using the designated colors for the prompt and
   // the trigger character.

   display( aRow[ nCurrent ], aCol[ nCurrent ], ;
         aPrompt[ nCurrent ], EnhColor( aColor[ nCurrent ] ) )

   display( aRow[ nCurrent ], ;
            aCol[ nCurrent ] - 1 + aTrigger[ nCurrent ], ;
            substr( aPrompt[ nCurrent ], aTrigger[ nCurrent ], 1 ), ;
            EnhColor( aTriggerColor[ nCurrent ] ) )

   dispend()

   // Wait for a keystroke

   nKey := inkey( 0 )

   // If the key was an alphabetic char, convert to uppercase

   if isBetween( nKey,97,122 ) then nKey -= 32

   // Set nPrev to the currently active menu item

   nPrev := nActive

   do case

      // Check for a hotkey, and evaluate the associated block if present.

      case ( bKey := setkey( nKey ) ) != NIL
         eval( bKey, ProcName( 1 ), ProcLine( 1 ), cReadVar )

      // If Enter was pressed, either exit the menu or evaluate the
      // associated code block.

      case nKey == K_ENTER
         if aExecute[ nCurrent ] != NIL
            eval( aExecute[ nCurrent ] )
         else
            lChoice := .T.
         endif

      // If ESC was pressed, set the selected item to zero and exit.

      case nKey == K_ESC
         lChoice := .T.
         nActive := 0

      // If Home was pressed, go to the designated menu item.

      case nKey == K_HOME
         nActive := iif( aHome[ nCurrent ] == NIL, 1, aHome[ nCurrent ] )

      // If End was pressed, go to the designated menu item.

      case nKey == K_END
         nActive := iif( aEnd[ nCurrent ] == NIL, nCount, aEnd[ nCurrent ] )

      // If Up Arrow was pressed, go to the designated menu item.

      case nKey == K_UP
         if aUp[ nCurrent ] == NIL
            if --nActive < 1 then nActive := iif( lWrap, nCount, 1 )
         else
            if isOkay( aUp[ nCurrent ] ) then nActive := aUp[ nCurrent ]
         endif

      // If Down Arrow was pressed, go to the designated menu item.

      case nKey == K_DOWN
         if aDown[ nCurrent ] == NIL
            if ++nActive > nCount then nActive := iif( lWrap, 1, nCount )
         else
            if isOkay( aDown[ nCurrent ] ) then nActive := aDown[ nCurrent ]
         endif

      // If Left Arrow was pressed, go to the designated menu item.

      case nKey == K_LEFT
         if aLeft[ nCurrent ] == NIL
            if --nActive < 1 then nActive := iif( lWrap, nCount, 1 )
         else
            if isOkay( aLeft[ nCurrent ] ) then nActive := aLeft[ nCurrent ]
         endif

      // If Right Arrow was pressed, go to the designated menu item.

      case nKey == K_RIGHT
         if aRight[ nCurrent ] == NIL
            if ++nActive > nCount then nActive := iif( lWrap, 1, nCount )
         else
            if isOkay( aRight[ nCurrent ] ) then nActive := aRight[ nCurrent ]
         endif

      // If a trigger letter was pressed, handle it based on the COLD
      // parameter.

      case ( nScan := ascan( aTriggerInkey[ nMenu ], nKey ) ) > 0
         nActive := nScan
         if .not. lCold then FT_PutKey( K_ENTER )
   endcase

   // Erase the highlight bar in preparation for the next iteration

   if .not. lChoice
      dispbegin()
      display( aRow[ nLast ], aCol[ nLast ], ;
            aPrompt[ nLast ], aColor[ nLast ] )

      display( aRow[ nLast ], aCol[ nLast ] - 1 + aTrigger[ nLast ], ;
               substr( aPrompt[ nLast ], aTrigger[ nLast ], 1 ), ;
               aTriggerColor[ nLast ] )

      if cScreen != NIL then restscreen( aMsgRow[ nLast ], ;
                                         aMsgCol[ nLast ], ;
                                         aMsgRow[ nLast ], ;
                                         aMsgCol[ nLast ]  ;
                                         + len( aMessage[ nLast ] ) - 1, ;
                                         cScreen )
      dispend()
      endif
end

// Now that we're exiting, decrement the recursion level and erase all
// the prompt information for the current invocation.

nLevel--

asize(          aRow, nLevel )
asize(          aCol, nLevel )
asize(       aPrompt, nLevel )
asize(        aColor, nLevel )
asize(       aMsgRow, nLevel )
asize(       aMsgCol, nLevel )
asize(      aMessage, nLevel )
asize(     aMsgColor, nLevel )
asize(      aTrigger, nLevel )
asize( aTriggerInkey, nLevel )
asize( aTriggerColor, nLevel )
asize(           aUp, nLevel )
asize(         aDown, nLevel )
asize(         aLeft, nLevel )
asize(        aRight, nLevel )
asize(      aExecute, nLevel )

         aRow[ nLevel ] := {}
         aCol[ nLevel ] := {}
      aPrompt[ nLevel ] := {}
       aColor[ nLevel ] := {}
      aMsgRow[ nLevel ] := {}
      aMsgCol[ nLevel ] := {}
     aMessage[ nLevel ] := {}
    aMsgColor[ nLevel ] := {}
     aTrigger[ nLevel ] := {}
aTriggerInkey[ nLevel ] := {}
aTriggerColor[ nLevel ] := {}
          aUp[ nLevel ] := {}
        aDown[ nLevel ] := {}
        aLeft[ nLevel ] := {}
       aRight[ nLevel ] := {}
     aExecute[ nLevel ] := {}

set( _SET_CURSOR, nCursor )

eval( bGetSet, nActive )

return nActive
