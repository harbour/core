/*
 * $Id$
 */

// Harbour Class TBrowse and TBColumn sample

#include "inkey.ch"

function Main()

   local oBrowse := TBrowseNew( 5, 5, 16, 30 )
   local aTest   := { "This", "is", "a", "browse", "on", "an", "array", "test", "with", "a", "long", "data" }
   local n       := 1
   local nKey
   local lEnd    := .f.

   oBrowse:colorSpec     = "W+/B, N/BG"
   oBrowse:ColSep        = "³"
   oBrowse:HeadSep        = "ÑÍ"
   oBrowse:FootSep        = "ÏÍ"
   oBrowse:GoTopBlock    = { || n := 1 }
   oBrowse:GoBottomBlock = { || n := Len( aTest ) }
   oBrowse:SkipBlock     = { | nSkip, nPos | nPos := n,;
                             n := If( nSkip > 0, Min( Len( aTest ), n + nSkip ),;
                             Max( 1, n + nSkip )), n - nPos }

   oBrowse:AddColumn( TBColumnNew( "First",  { || n } ) )
   oBrowse:AddColumn( TBColumnNew( "Second", { || aTest[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Third",  { || aTest[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Forth",  { || aTest[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Fifth",  { || n } ) )
   oBrowse:GetColumn(1):Footing = 'First'
   oBrowse:GetColumn(2):Footing = 'Second'
   oBrowse:GetColumn(3):Footing = 'Third'
   oBrowse:GetColumn(4):Footing = 'Forth'
   oBrowse:GetColumn(5):Footing = 'Fifth'
   oBrowse:GetColumn(2):ColSep = 'º'
   oBrowse:GetColumn(3):ColSep = 'º'

   Alert( oBrowse:ClassName() )
   Alert( oBrowse:GetColumn( 1 ):ClassName() )

   oBrowse:Freeze = 1
   SetCursor( 0 )

   set color to "W+/B"
   @ 4,4,17,31 BOX "ÚÄ¿³ÙÄÀ³ "

   while ! lEnd

      oBrowse:ForceStable()

      nKey = InKey( 0 )

      do case
         case nKey == K_ESC
              SetPos( 17, 0 )
              lEnd = .t.

         case nKey == K_DOWN
              oBrowse:Down()

         case nKey == K_UP
              oBrowse:Up()

         case nKey == K_LEFT
              oBrowse:Left()

         case nKey == K_RIGHT
              oBrowse:Right()

         case nKey = K_PGDN
              oBrowse:pageDown()

         case nKey = K_PGUP
              oBrowse:pageUp()

         case nKey = K_CTRL_PGUP
              oBrowse:goTop()

         case nKey = K_CTRL_PGDN
              oBrowse:goBottom()

         case nKey = K_HOME
              oBrowse:home()

         case nKey = K_END
              oBrowse:end()

         case nKey = K_CTRL_LEFT
              oBrowse:panLeft()

         case nKey = K_CTRL_RIGHT
              oBrowse:panRight()

         case nKey = K_CTRL_HOME
              oBrowse:panHome()

         case nKey = K_CTRL_END
              oBrowse:panEnd()

      endcase
   end

return nil
