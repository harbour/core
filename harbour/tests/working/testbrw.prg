/*
 * $Id$
 */

// Harbour Class TBrowse and TBColumn sample

#include "inkey.ch"

function Main()

   local oBrowse := TBrowseNew( 5, 5, 15, 30 )
   local aTest   := { "This", "is", "a", "browse", "on", "an", "array", "test", "with", "a", "long", "data" }
   local n       := 1
   local nKey
   local lEnd    := .f.

   oBrowse:colorSpec     = "W+/B, N/BG"
   oBrowse:ColSep        = "³"
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

   Alert( oBrowse:ClassName() )
   Alert( oBrowse:GetColumn( 1 ):ClassName() )

   SetCursor( 0 )

   set color to "W+/B"
   @ 4,4,16,31 BOX "ÚÄ¿³ÙÄÀ³ "
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
      endcase
   end

return nil
