/*
 * $Id$
 */

/*
   Harbour Project source code
   www - http://www.Harbour-Project.org

   A pure Clipper emulation of the PROMPT/MENU TO commands.

   Author: Phil Barnett <philb@iag.net>
   Released to Public Domain
*/

/* NOTE: Recursive use is acceptable */

#include "setcurs.ch"
#include "inkey.ch"
#include "color.ch"

static aLevel   := {}
static nPointer := 1

procedure __AtPrompt( nCol, nRow, cPrompt, cMsg )

   local nMsgLen := 0

   // gather message data
   if valtype( cMsg ) == 'C' .and. !empty( cMsg )
      if len( cMsg ) > maxcol() + 1
         cMsg := left( cMsg, maxcol() + 1 )                    // message too long to display
      endif
      nMsgLen := max( nMsgLen, len( cMsg ) )
   endif

   if nPointer < 1
      nPointer := 1
   endif

   // add the current level empty array.
   do while len( aLevel ) < nPointer
      aadd( aLevel, {} )
   enddo

   // add to the static array
   aadd( aLevel[ nPointer ], { nCol, nRow, cPrompt, cMsg, len( cPrompt ), nMsgLen } )

   // put this prompt on the screen right now
   setpos( nCol, nRow )
   dispout( cPrompt )

   return

function __MenuTo( bBlock, cVariable )

   local nKey
   local y
   local q
   local n           := eval( bBlock )
   local nArrLen     := len( aLevel[ nPointer ] )
   local nMsgRow     := set( _SET_MESSAGE )
   local lSetMCenter := set( _SET_MCENTER )
   local nMaxCol     := maxcol() + 1
   local lDoMessage  := nMsgRow > 0
   local lGotMessage := .f.
   local nMsgCol     := int( ( nMaxCol - aLevel[ nPointer ] [ len( aLevel ) ] [ 6 ] ) / 2 )
   local nSaveCsr    := setcursor( SC_NONE )

   ColorSelect( CLR_STANDARD )

   nPointer ++

   if len( aLevel[ nPointer - 1 ] ) == 0

      n := 0

   else

      if !( ValType( n ) == "N" ) .OR. n < 1
         n := 1
      elseif n > len( aLevel[ nPointer - 1 ] )
         n := len( aLevel[ nPointer - 1 ] )
      endif

      for y := 1 to nArrLen
         if valtype( aLevel[ nPointer - 1, y, 4 ] ) $ 'CB'
            lGotMessage := .t.
            exit
         endif
      next

      do while .t.

         // were there any messages?
         if lGotMessage .and. lDoMessage

            if valtype( aLevel[ nPointer - 1, n, 4 ] ) == 'B'
               // Code Block messages ( yes, they are documented! )

               eval( aLevel[ nPointer - 1, n, 4 ] )

            elseif valtype( aLevel[ nPointer - 1, n, 4 ] ) == 'C'
               // Character messages

               // set the display location
               if lSetMCenter

                  // erase the current message row
                  dispbox( nMsgRow, nMsgCol, ;
                           nMsgRow, nMsgCol + aLevel[ nPointer - 1, n, 6 ] )

                  setpos( nMsgRow, ( nMaxcol - len( aLevel[ nPointer - 1, n, 4 ] ) ) / 2 )

               else

                  // erase the current message row
                  dispbox( nMsgRow, 0, ;
                           nMsgRow, aLevel[ nPointer - 1, n, 6 ], '         ' )

                  setpos( nMsgRow, 0 )

               endif

               if len( aLevel[ nPointer - 1, n, 4 ] ) > 0

                  // display the message
                  dispout( aLevel[ nPointer - 1, n, 4 ] )

               endif

            endif

         endif

         // save the current row
         q := n

         ColorSelect( CLR_ENHANCED )

         // highlight the prompt
         setpos( aLevel[ nPointer - 1, n, 1 ], aLevel[ nPointer - 1, n, 2 ] )
         dispout( aLevel[ nPointer - 1, n, 3 ] )

         ColorSelect( CLR_STANDARD )

         setpos( aLevel[ nPointer - 1, n, 1 ], aLevel[ nPointer - 1, n, 2 ] )

         // wait for a keystroke
         nKey := inkey( 0 )

         // check for keystrokes
         do case
         case ( bAction := setkey( nKey ) ) <> NIL

            eval( bAction, procname( 1 ), procline( 1 ), Upper( cVariable ) )
            if empty( nextkey() )
               keyboard chr( 255 )
               inkey()
               nKey := 0
            endif

         case nKey == K_DOWN .or. nKey == K_RIGHT
            n ++
            if n > nArrLen
               if set( _SET_WRAP )
                  n := 1
               else
                  n := nArrLen
               endif
            endif
         case nKey == K_UP .or. nKey == K_LEFT
            n --
            if n < 1
               if set( _SET_WRAP )
                  n := nArrLen
               else
                  n := 1
               endif
            endif
         case nKey == K_ENTER .or. nKey == K_PGUP .or. nKey == K_PGDN
            exit
         case nKey == K_ESC
            n := 0
            exit
         case nKey == K_HOME
            n := 1
         case nKey == K_END
            n := nArrLen
         otherwise
            // did user hit a hot key?
            for y := 1 to nArrLen
               if upper( left( ltrim( aLevel[ nPointer - 1, y, 3 ] ), 1 ) ) == upper( chr( nKey ) )
                  n := y
                  exit
               endif
            next
         endcase

         // unhighlight the prompt
         setpos( aLevel[ nPointer - 1, q, 1 ], aLevel[ nPointer - 1, q, 2 ] )
         dispout( aLevel[ nPointer - 1, q, 3 ] )

      enddo

   endif

   nPointer --

   asize( aLevel, nPointer - 1 )

   setcursor( nSaveCsr )

   eval( bBlock, n )

   return n

