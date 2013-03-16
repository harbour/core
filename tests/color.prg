//
// This program demonstrates that SetColor() works with
// Set( _SET_COLOR ) and also that the internal representation
// of a clipper color string is properly converted back to a
// valid string.  It also shows that there can be any number of
// color pairs as in CA-Cl*pper
// This demo will be expanded over time.
//
// Written by Paul Tucker {ptucker@sympatico.ca>
// www - http://harbour-project.org
//
// This test program placed in the public domain
//

#include "color.ch"

PROCEDURE Main()

   LOCAL cColor := "r+/bg*,rg*/ng+,w/n,b+/w,rg/b,w+/n*"

   ? "original set ", Set( _SET_COLOR )
   ? "should match ", SetColor( cColor )
   ? "input 1      ", cColor
   ? "return       ", SetColor( "1/7,9/15" )
   ? "input 2      ", "1/7,9/15"
   ? "return       ", SetColor()
   ? "final set    ", Set( _SET_COLOR )

   SetColor( "BG/N, N/BG" )
   ? "Hello1"
   ColorSelect( CLR_ENHANCED )
   ? "Hello2"
   ColorSelect( CLR_STANDARD )
   ? "Hello3"
   ColorSelect( CLR_ENHANCED )
   SetColor( "GR/N, N/GR" )
   ? "Hello4"
   ColorSelect( CLR_ENHANCED )
   ? "Hello5"
   ColorSelect( CLR_STANDARD )
   ? "Hello6"
   SetColor( "B/N, N/B" )
   ? "Hello7"

   RETURN
