/*
 * $Id$
 */

/*
   Harbour Project source code
   www - http://www.Harbour-Project.org

   A pure Clipper emulation of the PROMPT/MENU TO commands.

   Author: Phil Barnett <philb@iag.net>
   Released to Public Domain

   Changes for higher Clipper compatibility by:
   Victor Szel <info@szelvesz.hu>
*/

/* NOTE: Recursive use is supported */

#include "setcurs.ch"
#include "inkey.ch"
#include "color.ch"

static aLevel   := {}
static nPointer := 1

function __AtPrompt( nCol, nRow, cPrompt, cMsg )

   if nPointer < 1
      nPointer := 1
   endif

   // add the current level empty array.
   do while len( aLevel ) < nPointer
      aadd( aLevel, {} )
   enddo

   // add to the static array
   aadd( aLevel[ nPointer ], { nCol, nRow, cPrompt, cMsg } )

   // put this prompt on the screen right now
   setpos( nCol, nRow )
   dispout( cPrompt )

   return .f.

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

   local bOldError
   local lBlockError

   // Detect if a memver was passed

// TODO: Enable when implemented
//
// bOldError := errorblock( {|| break( NIL ), .F. } )
// lBlockError := .T.
// begin sequence
      n := eval( bBlock )
      lBlockError := .F.
// TODO: Enable when implemented
//
// end sequence
// errorblock( bOldError )

   if lBlockError
      __mvPUBLIC( cVariable )
   endif

   // if no prompts were defined, exit with 0

   if nPointer < 1 .or. nPointer > len( aLevel )

      n := 0

   else

      nPointer ++

      nArrLen := len( aLevel[ nPointer - 1 ] )

      // put choice in a valid range

      if !( ValType( n ) == "N" ) .OR. n < 1
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
               setpos( nMsgRow, nMsgCol )
               dispout( space( len( xMsg ) ) )
            endif

            xMsg := aLevel[ nPointer - 1, n, 4 ]

            // Code Block messages ( yes, they are documented! )
            if valtype( xMsg ) == 'B'
               xMsg := eval( xMsg )
            endif

            if valtype( xMsg ) <> 'C'
               xMsg := ""
            endif

            if lMsgCenter
               nMsgCol := int( ( maxcol() - len( xMsg ) ) / 2 )
            endif

            setpos( nMsgRow, nMsgCol )
            dispout( xMsg )

         endif

         // save the current row
         q := n

         if Set( _SET_INTENSITY )
            ColorSelect( CLR_ENHANCED )
         endif

         // highlight the prompt
         setpos( aLevel[ nPointer - 1, n, 1 ], aLevel[ nPointer - 1, n, 2 ] )
         dispout( aLevel[ nPointer - 1, n, 3 ] )

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
               if upper( left( ltrim( aLevel[ nPointer - 1, y, 3 ] ), 1 ) ) == upper( chr( nKey ) )
                  n := y
                  lExit := .T.
                  exit
               endif
            next
         endcase

         if n <> 0
            setpos( aLevel[ nPointer - 1, q, 1 ], aLevel[ nPointer - 1, q, 2 ] )
            dispout( aLevel[ nPointer - 1, q, 3 ] )
         endif

      enddo

      ReadVar( cSaveReadVar )
      SetCursor( nSaveCursor )

      nPointer --
      asize( aLevel, nPointer - 1 )

   endif

   eval( bBlock, n )

   if lBlockError
      release ( cVariable )
   endif

   SetPos( MaxRow() - 1, 0)

   return n

