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

/* NOTE: Recursive use is supported. */

#include "color.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbmemvar.ch"
#include "setcurs.ch"
#xtranslate COLORARRAY(<x>) => &( '{"' + strtran(<x>, ',', '","') + '"}' )

static s_aLevel   := {}
static s_nPointer := 1

function __AtPrompt( nRow, nCol, cPrompt, cMsg, cColor )

   if s_nPointer < 1
      s_nPointer := 1
   endif

   // add the current level empty array.
   do while len( s_aLevel ) < s_nPointer
      aadd( s_aLevel, {} )
   enddo

   // add to the static array
   aadd( s_aLevel[ s_nPointer ], { nRow, nCol, cPrompt, cMsg, cColor} )

   // put this prompt on the screen right now
   DispOutAt( nRow, nCol, cPrompt, cColor )

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

   local lDeclared
   local bAction
   local nMouseClik

   local nPointer
   Local aColor

   // Detect if a memvar was passed

   if __mvSCOPE( cVariable ) <= HB_MV_ERROR
      __mvPUBLIC( cVariable )
      lDeclared := .T.
   else
      lDeclared := .F.
   endif

   n := eval( bBlock )

   // if no prompts were defined, exit with 0

   if s_nPointer < 1 .or. s_nPointer > len( s_aLevel )

      n := 0

   else

      s_nPointer ++
      nPointer := s_nPointer

      nArrLen := len( s_aLevel[ nPointer - 1 ] )

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

            xMsg := s_aLevel[ nPointer - 1, n, 4 ]

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

         if s_aLevel[ s_nPointer-1,n,5] <> nil
             aColor := COLORARRAY( s_aLevel[ s_nPointer-1,n,5] )
         endif

         if Set( _SET_INTENSITY )
            if( aColor <> Nil , aColor[ 2 ] , ColorSelect( CLR_ENHANCED ) )
         endif

         // highlight the prompt
         DispOutAt( s_aLevel[ nPointer - 1, n, 1 ],;
                    s_aLevel[ nPointer - 1, n, 2 ],;
                    s_aLevel[ nPointer - 1, n, 3 ],;
                    if(aColor <> NIL, acolor[ 2 ], NIL ) )

         if Set( _SET_INTENSITY )
            //ColorSelect( CLR_STANDARD )
            if( aColor<> Nil , aColor[ 1 ] , ColorSelect( CLR_STANDARD ) )
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
         case nKey == 1001
         case nKey == 1002 .OR. nKey == 1006
            if ( ( nMouseClik := hittest(s_aLevel[ nPointer-1 ], mrow(), mcol()) ) > 0 )
                n := nMouseClik
            endif
            if ( nKey == 1006 )
                lExit := .T.
            endif

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
               if upper( left( ltrim( s_aLevel[ nPointer - 1, y, 3 ] ), 1 ) ) == upper( chr( nKey ) )
                  n := y
                  lExit := .T.
                  exit
               endif
            next
         endcase

         if n <> 0
            DispOutAt( s_aLevel[ nPointer - 1, q, 1 ],;
                       s_aLevel[ nPointer - 1, q, 2 ],;
                       s_aLevel[ nPointer - 1, q, 3 ],;
                       if( aColor <> NIL , aColor[ 1 ] , nil ) )
         endif

      enddo

      ReadVar( cSaveReadVar )
      SetCursor( nSaveCursor )

      s_nPointer := nPointer
      s_nPointer --
      asize( s_aLevel, s_nPointer - 1 )

   endif

   eval( bBlock, n )

   if lDeclared
      __mvXRELEASE( cVariable )
   endif

   SetPos( MaxRow() - 1, 0)

   return n

static function HITTEST( aMenu, nMouseRow, nMouseCol )

   local nPos, nLen := Len(aMenu)
   for nPos := 1 to nLen
      if ( nMouseRow != aMenu[ nPos ][ 1 ] )
      elseif ( nMouseCol < aMenu[ nPos ][ 2 ] )
      elseif ( nMouseCol < aMenu[ nPos ][ 2 ] + Len(aMenu[ nPos ][ 3 ]) )
         return nPos
      endif
   next
   return 0
