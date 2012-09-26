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

#xtranslate display( <row>, <col>, <stuff>, <color> ) => ;
      SetPos( < row > , < col > ) ; DispOut( < stuff > , < color > )

#xtranslate EnhColor( <colorspec> ) => ;
      SubStr( < colorspec > , At( ",", < colorspec > ) + 1 )

#xtranslate isOkay( <exp> ) => ;
      ( < exp > \ > 0 .AND. < exp > \ <= nCount )

#xtranslate isBetween( <val>, <lower>, <upper> ) => ;
      ( < val > \ >= < lower > .AND. < val > \ <= < upper > )

#define nTriggerInkey hb_keyCode( Upper( SubStr( cPrompt, nTrigger, 1 ) ) )
#define cTrigger SubStr( cPrompt, nTrigger, 1 )
#define nCurrent nMenu,nActive
#define nLast nMenu,nPrev

// These arrays hold information about each menu item

THREAD STATIC aRow          := { {} }
THREAD STATIC aCol          := { {} }
THREAD STATIC aPrompt       := { {} }
THREAD STATIC aColor        := { {} }
THREAD STATIC aMsgRow       := { {} }
THREAD STATIC aMsgCol       := { {} }
THREAD STATIC aMessage      := { {} }
THREAD STATIC aMsgColor     := { {} }
THREAD STATIC aTrigger      := { {} }
THREAD STATIC aTriggerInkey := { {} }
THREAD STATIC aTriggerColor := { {} }
THREAD STATIC aHome         := { {} }
THREAD STATIC aEnd          := { {} }
THREAD STATIC aUp           := { {} }
THREAD STATIC aDown         := { {} }
THREAD STATIC aLeft         := { {} }
THREAD STATIC aRight        := { {} }
THREAD STATIC aExecute      := { {} }
THREAD STATIC nLevel        := 1

FUNCTION FT_Prompt( nRow,    nCol,    cPrompt,  cColor,      ;
      nMsgRow, nMsgCol, cMessage, cMsgColor,   ;
      nTrigger, cTriggerColor, nHome, nEnd,    ;
      nUp, nDown, nLeft, nRight, bExecute      )

// If the prompt color setting is not specified, use default

   IF cColor  == NIL
      cColor  := SetColor()
   ENDIF

// If no message is supplied, set message values to NIL

   IF cMessage == NIL

      nMsgRow := nMsgCol := cMsgColor := NIL

   ELSE

      // If message row not supplied, use the default

      IF nMsgRow == NIL
         nMsgRow := Set( _SET_MESSAGE )
      ENDIF

      // If message column not supplied, use the default

      IF nMsgCol == NIL
         IF Set( _SET_MCENTER )
            nMsgCol := Int( ( MaxCol() + 1 - Len( cPrompt ) ) / 2 )
         ELSE
            nMsgCol := 0
         ENDIF
      ENDIF

      // If message color not specified, use the default

      IF cMsgColor == NIL
         cMsgColor := cColor
      ENDIF
   ENDIF

// If trigger values not specifed, set the defaults

   IF nTrigger       == NIL
      nTrigger      := 1
   ENDIF
   IF cTriggerColor  == NIL
      cTriggerColor := cColor
   ENDIF

// Now add elements to the static arrays -- nLevel indicates the recursion
// level, which allows for nested menus.

   AAdd(          aRow[ nLevel ], nRow          )
   AAdd(          aCol[ nLevel ], nCol          )
   AAdd(       aPrompt[ nLevel ], cPrompt       )
   AAdd(        aColor[ nLevel ], cColor        )
   AAdd(       aMsgRow[ nLevel ], nMsgRow       )
   AAdd(       aMsgCol[ nLevel ], nMsgCol       )
   AAdd(      aMessage[ nLevel ], cMessage      )
   AAdd(     aMsgColor[ nLevel ], cMsgColor     )
   AAdd(      aTrigger[ nLevel ], nTrigger      )
   AAdd( aTriggerInkey[ nLevel ], nTriggerInkey )
   AAdd( aTriggerColor[ nLevel ], cTriggerColor )
   AAdd(         aHome[ nLevel ], nHome         )
   AAdd(          aEnd[ nLevel ], nEnd          )
   AAdd(           aUp[ nLevel ], nUp           )
   AAdd(         aDown[ nLevel ], nDown         )
   AAdd(         aLeft[ nLevel ], nLeft         )
   AAdd(        aRight[ nLevel ], nRight        )
   AAdd(      aExecute[ nLevel ], bExecute      )

// Now display the prompt for the sake of compatibility

   DispBegin()
   DISPLAY( nRow, nCol, cPrompt, cColor )
   DISPLAY( nRow, nCol - 1 + nTrigger, cTrigger, cTriggerColor )
   DispEnd()

   RETURN NIL

FUNCTION FT_MenuTo( bGetSet, cReadVar, lCold )

   LOCAL nMenu   := nLevel++
   LOCAL nActive
   LOCAL nCount  := Len( aRow[ nMenu ] )
   LOCAL lChoice := .F.
   LOCAL nCursor := Set( _SET_CURSOR, SC_NONE )
   LOCAL nKey, bKey, nScan, lWrap, cScreen, nPrev

   IF ! HB_ISLOGICAL( lCold )
      lCold := .F.
   ENDIF

// Validate the incoming parameters and assign some reasonable defaults
// to prevent a crash later.

   cReadVar := iif( cReadVar == NIL, "", Upper( cReadVar ) )

   IF bGetSet == NIL
      bGetSet := {|| 1 }
   ENDIF

// Eval the incoming getset block to initialize nActive, which indicates
// the menu prompt which is to be active when the menu is first displayed.
// If nActive is outside the appropriate limits, a value of 1 is assigned.

   nActive := Eval( bGetSet )

   IF ( nActive < 1 .OR. nActive > nCount )
      nActive := 1
   ENDIF

// Increment the recursion level in case a hotkey procedure
// calls FT_Prompt().  This will cause a new set of prompts
// to be created without disturbing the current set.

   AAdd(          aRow, {} )
   AAdd(          aCol, {} )
   AAdd(       aPrompt, {} )
   AAdd(        aColor, {} )
   AAdd(       aMsgRow, {} )
   AAdd(       aMsgCol, {} )
   AAdd(      aMessage, {} )
   AAdd(     aMsgColor, {} )
   AAdd(      aTrigger, {} )
   AAdd( aTriggerInkey, {} )
   AAdd( aTriggerColor, {} )
   AAdd(           aUp, {} )
   AAdd(         aDown, {} )
   AAdd(         aLeft, {} )
   AAdd(        aRight, {} )
   AAdd(      aExecute, {} )

// Loop until Enter or Esc is pressed

   WHILE ! lChoice

      // Evaluate the getset block to update the target memory variable
      // in case it needs to be examined by a hotkey procedure.

      Eval( bGetSet, nActive )

      // Get the current setting of SET WRAP so that the desired menu behavior
      // can be implemented.

      lWrap := Set( _SET_WRAP )

      // If a message is to be displayed, save the current screen contents
      // and then display the message, otherwise set the screen buffer to NIL.

      DispBegin()

      IF aMessage[ nCurrent ] != NIL
         cScreen := SaveScreen( aMsgRow[ nCurrent ], aMsgCol[ nCurrent ],  ;
            aMsgRow[ nCurrent ], aMsgCol[ nCurrent ] + ;
            Len( aMessage[ nCurrent ] ) - 1 )

         DISPLAY( aMsgRow[ nCurrent ],   aMsgCol[ nCurrent ], ;
            aMessage[ nCurrent ], aMsgColor[ nCurrent ]  )

      ELSE
         cScreen := NIL
      ENDIF

      // Display the prompt using the designated colors for the prompt and
      // the trigger character.

      DISPLAY( aRow[ nCurrent ], aCol[ nCurrent ], ;
         aPrompt[ nCurrent ], EnhColor( aColor[ nCurrent ] ) )

      DISPLAY( aRow[ nCurrent ], ;
         aCol[ nCurrent ] - 1 + aTrigger[ nCurrent ], ;
         SubStr( aPrompt[ nCurrent ], aTrigger[ nCurrent ], 1 ), ;
         EnhColor( aTriggerColor[ nCurrent ] ) )

      DispEnd()

      // Wait for a keystroke

      nKey := Inkey( 0 )

      // If the key was an alphabetic char, convert to uppercase

      IF isBetween( nKey, 97, 122 )
         nKey -= 32
      ENDIF

      // Set nPrev to the currently active menu item

      nPrev := nActive

      DO CASE

         // Check for a hotkey, and evaluate the associated block if present.

      CASE ( bKey := SetKey( nKey ) ) != NIL
         Eval( bKey, ProcName( 1 ), ProcLine( 1 ), cReadVar )

         // If Enter was pressed, either exit the menu or evaluate the
         // associated code block.

      CASE nKey == K_ENTER
         IF aExecute[ nCurrent ] != NIL
            Eval( aExecute[ nCurrent ] )
         ELSE
            lChoice := .T.
         ENDIF

         // If ESC was pressed, set the selected item to zero and exit.

      CASE nKey == K_ESC
         lChoice := .T.
         nActive := 0

         // If Home was pressed, go to the designated menu item.

      CASE nKey == K_HOME
         nActive := iif( aHome[ nCurrent ] == NIL, 1, aHome[ nCurrent ] )

         // If End was pressed, go to the designated menu item.

      CASE nKey == K_END
         nActive := iif( aEnd[ nCurrent ] == NIL, nCount, aEnd[ nCurrent ] )

         // If Up Arrow was pressed, go to the designated menu item.

      CASE nKey == K_UP
         IF aUp[ nCurrent ] == NIL
            if --nActive < 1
               nActive := iif( lWrap, nCount, 1 )
            ENDIF
         ELSE
            IF isOkay( aUp[ nCurrent ] )
               nActive := aUp[ nCurrent ]
            ENDIF
         ENDIF

         // If Down Arrow was pressed, go to the designated menu item.

      CASE nKey == K_DOWN
         IF aDown[ nCurrent ] == NIL
            if ++nActive > nCount
               nActive := iif( lWrap, 1, nCount )
            ENDIF
         ELSE
            IF isOkay( aDown[ nCurrent ] )
               nActive := aDown[ nCurrent ]
            ENDIF
         ENDIF

         // If Left Arrow was pressed, go to the designated menu item.

      CASE nKey == K_LEFT
         IF aLeft[ nCurrent ] == NIL
            if --nActive < 1
               nActive := iif( lWrap, nCount, 1 )
            ENDIF
         ELSE
            IF isOkay( aLeft[ nCurrent ] )
               nActive := aLeft[ nCurrent ]
            ENDIF
         ENDIF

         // If Right Arrow was pressed, go to the designated menu item.

      CASE nKey == K_RIGHT
         IF aRight[ nCurrent ] == NIL
            if ++nActive > nCount
               nActive := iif( lWrap, 1, nCount )
            ENDIF
         ELSE
            IF isOkay( aRight[ nCurrent ] )
               nActive := aRight[ nCurrent ]
            ENDIF
         ENDIF

         // If a trigger letter was pressed, handle it based on the COLD
         // parameter.

      CASE ( nScan := AScan( aTriggerInkey[ nMenu ], nKey ) ) > 0
         nActive := nScan
         IF ! lCold
            FT_PutKey( K_ENTER )
         ENDIF
      ENDCASE

      // Erase the highlight bar in preparation for the next iteration

      IF ! lChoice
         DispBegin()
         DISPLAY( aRow[ nLast ], aCol[ nLast ], ;
            aPrompt[ nLast ], aColor[ nLast ] )

         DISPLAY( aRow[ nLast ], aCol[ nLast ] - 1 + aTrigger[ nLast ], ;
            SubStr( aPrompt[ nLast ], aTrigger[ nLast ], 1 ), ;
            aTriggerColor[ nLast ] )

         IF cScreen != NIL
            RestScreen( aMsgRow[ nLast ], ;
               aMsgCol[ nLast ], ;
               aMsgRow[ nLast ], ;
               aMsgCol[ nLast ]  ;
               + Len( aMessage[ nLast ] ) - 1, ;
               cScreen )
         ENDIF
         DispEnd()
      ENDIF
   ENDDO

// Now that we're exiting, decrement the recursion level and erase all
// the prompt information for the current invocation.

   nLevel--

   ASize(          aRow, nLevel )
   ASize(          aCol, nLevel )
   ASize(       aPrompt, nLevel )
   ASize(        aColor, nLevel )
   ASize(       aMsgRow, nLevel )
   ASize(       aMsgCol, nLevel )
   ASize(      aMessage, nLevel )
   ASize(     aMsgColor, nLevel )
   ASize(      aTrigger, nLevel )
   ASize( aTriggerInkey, nLevel )
   ASize( aTriggerColor, nLevel )
   ASize(           aUp, nLevel )
   ASize(         aDown, nLevel )
   ASize(         aLeft, nLevel )
   ASize(        aRight, nLevel )
   ASize(      aExecute, nLevel )

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

   Set( _SET_CURSOR, nCursor )

   Eval( bGetSet, nActive )

   RETURN nActive
