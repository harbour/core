/*
 * $Id$
 */

*+²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
*+
*+    Source Module => D:\SRC\PBMAKE\PICKARRY.PRG
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
*+    Functions: Function PICKARRY()
*+               Function Keys()
*+
*+    Reformatted by Click! 2.03 on Mar-30-1999 at 11:19 pm
*+
*+²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

static someitems

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function PICKARRY()
*+
*+    Called from ( makelink.prg )   1 - function makelink()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
function PICKARRY( T, L, b, r, IN_ARRAY, OUT_ARRAY )

local nChoice    := 1
local x
local NEW_ARRAY  := {}
local NUM_ELEMS  := len( IN_ARRAY )
local PAD_LEN    := ( r - 1 ) - ( L + 1 )
local lIsChecked

someitems := 0

putscreen()

@ T - 1, L - 1 clear to b + 1, r + 1
@ T - 1, L - 1 to b + 1, r + 1 double

for x := 1 to NUM_ELEMS
   IN_ARRAY[ X ]  := padr( '   ' + IN_ARRAY[ X ], PAD_LEN )
   OUT_ARRAY[ X ] := ' ' + OUT_ARRAY[ X ]
next

do while nChoice != 0
   nChoice := achoice(    T, L    , b      , r     , IN_ARRAY,     , 'keys'   , nChoice, 1 )

   if nChoice > 0

      lIsChecked := substr( IN_ARRAY[ nChoice ], 2, 1 ) == 'û'

      IN_ARRAY[ nChoice ]  := stuff( IN_ARRAY[ nChoice ], 2, 1, if( lIsChecked, ' ', 'û' ) )
      OUT_ARRAY[ nChoice ] := stuff( OUT_ARRAY[ nChoice ], 1, 1, if( lIsChecked, ' ', 'û' ) )

      if lIsChecked
         SOMEITEMS --
      else
         SOMEITEMS ++
      endif

      nChoice ++

   endif

enddo

for x := 1 to NUM_ELEMS
   if left( OUT_ARRAY[ X ], 1 ) == 'û'
      aadd( NEW_ARRAY, substr( OUT_ARRAY[ X ], 2 ) )
   endif
   IN_ARRAY[ X ] := substr( IN_ARRAY[ X ], 4 )
next

asize( OUT_ARRAY, len( NEW_ARRAY ) )
acopy( NEW_ARRAY, OUT_ARRAY )

getscreen()

return len( NEW_ARRAY )

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Keys()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
function Keys( MODE )

local RETVAL := 2
local THEKEY := lastkey()

if MODE = 1
   keyboard chr( 30 )
elseif MODE = 2
   keyboard chr( 31 )
elseif MODE = 3
   if THEKEY = 32
      RETVAL := 1
   elseif THEKEY = 27
      RETVAL := 0
   elseif THEKEY = 13 .and. SOMEITEMS < 1
      RETVAL := 1
      keyboard chr( 13 )
   elseif THEKEY = 13
      keyboard chr( 24 )
      RETVAL := 0
   endif
endif

return ( RETVAL )

*+ EOF: PICKARRY.PRG
