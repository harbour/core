/*
 * $Id$
 */

// Harbour Class TBrowse and TBColumn sample

#include "inkey.ch"

function Main()

   local oBrowse := TBrowseNew( 5, 5, 15, 30 )
   local aTest   := { "This", "is", "a", "browse", "on", "an", "array", "test" }
   local n       := 1
   local nKey
   local lEnd    := .f.

   oBrowse:colorSpec     = "W+/B, N/BG"
   oBrowse:GoTopBlock    = { || n := 1 }
   oBrowse:GoBottomBlock = { || n := Len( aTest ) }
   oBrowse:SkipBlock     = { | nSkip, nPos | nPos := n,;
                             n := If( nSkip > 0, Min( Len( aTest ), n + nSkip ),;
                             Max( 1, n + nSkip )), n - nPos }

   oBrowse:AddColumn( TBColumnNew( "First",  { || aTest[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Second", { || aTest[ n ] } ) )
   oBrowse:AddColumn( TBColumnNew( "Third",  { || aTest[ n ] } ) )

   Alert( oBrowse:ClassName() )
   Alert( oBrowse:GetColumn( 1 ):ClassName() )

   SetCursor( 0 )

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
      endcase
   end

return nil
