/*
 * $Id$
 */

*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊
*+
*+    Source Module => D:\SRC\PBMAKE\PRB_STAK.PRG
*+
*+    PBMake is a Clipper, C, ASM, Xbase++ make engine.
*+    
*+    Copyright(C) 1996-1999 by Phil Barnett.
*+       
*+    This program is free software; you can redistribute it and/or modify it
*+    under the terms of the GNU General Public License as published by the
*+    Free Software Foundation; either version 2 of the License, or (at your
*+    option) any later version.
*+    
*+    This program is distributed in the hope that it will be useful, but
*+    WITHOUT ANY WARRANTY; without even the implied warranty of
*+    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*+    General Public License for more details.
*+    
*+    You should have received a copy of the GNU General Public License along
*+    with this program; if not, write to the Free Software Foundation, Inc.,
*+    675 Mass Ave, Cambridge, MA 02139, USA.
*+    
*+    You can contact me at:
*+    
*+    Phil Barnett
*+    Box 944
*+    Plymouth, Florida  32768
*+    
*+    or
*+    
*+    philb@iag.net
*+    
*+
*+    Functions: Procedure PUTSCREEN()
*+               Procedure GETSCREEN()
*+
*+    Reformatted by Click! 2.03 on Mar-30-1999 at 11:19 pm
*+
*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊

#include "common.ch"
static SCR := {}
static CNT := 0

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Procedure PUTSCREEN()
*+
*+    Called from ( pbinit.prg   )   4 - procedure pbinit()
*+                ( pickarry.prg )   1 - function pickarry()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
procedure PUTSCREEN( T, L, b, r )

default T to 0
default L to 0
default b to maxrow()
default r to maxcol()

aadd( SCR, { T, L, b, r, savescreen( T, L, b, r ), row(), col(), set( _SET_CURSOR ), setcolor() } )
CNT ++

return

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Procedure GETSCREEN()
*+
*+    Called from ( pbinit.prg   )   4 - procedure pbinit()
*+                ( pickarry.prg )   1 - function pickarry()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
procedure GETSCREEN( OPTION )

default OPTION to 1

do case
case OPTION == 0
   CNT := 0
   SCR := {}
case OPTION == - 1
   asize( SCR, -- CNT )
otherwise
   if CNT > 0
      restscreen( SCR[ cnt, 1 ], SCR[ cnt, 2 ], SCR[ cnt, 3 ], SCR[ cnt, 4 ], SCR[ cnt, 5 ] )
      devpos( SCR[ cnt, 6 ], SCR[ cnt, 7 ] )
      set( _SET_CURSOR, SCR[ cnt, 8 ] )
      setcolor( SCR[ cnt, 9 ] )
      asize( SCR, -- CNT )
   endif
endcase

return

*+ EOF: PRB_STAK.PRG
