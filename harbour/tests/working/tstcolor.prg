//
// $Id$
//

//
// tstColor
//
// This program demonstrates that SetColor() works with
// Set( _SET_COLOR ) and also that the internal representation
// of a clipper color string is properly converted back to a
// valid string.  It also shows that there can be any number of
// color pairs as in CA-Clipper
// This demo will be expanded over time.
//
// Written by Paul Tucker {ptucker@sympatico.ca>
// www - http://www.harbour-project.org
//
// This test program placed in the public domain
//

#include "set.ch"
proc main
   Local cColor := "r+/bg*,rg*/ng+,w/n,b+/w,rg/b,w+/n*"

? set(_SET_COLOR )
? "original ", setcolor( cColor )
? "input    ", cColor
? "return   ", setcolor()
? set(_SET_COLOR )
