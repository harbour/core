/*
 * $Id$
 */

*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊
*+
*+    Source Module => D:\SRC\PBMAKE\PICKFILE.PRG
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
*+    Functions: Function pickfile()
*+
*+    Reformatted by Click! 2.03 on Mar-30-1999 at 11:19 pm
*+
*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊

/*
   PICKFILE.PRG

   Author     : Phil Barnett

   Written    : 18-Apr-93

   Function   : PICKFILE()

   Purpose    : Generic file picking routine that pops a picklist of files.

   Syntax     : PICKFILE( [FILESPEC] )

   Parameters : FILESPEC is a DOS filename. Wildcards permitted

   Returns    : Character file name of selected file or "" if nothing picked.

   Example    : yourfile := pickfile( '*.dbf' )

                if empty(yourfile)
                  ? 'You pressed Escape or No Matching File'
                else
                  ? 'The file you selected is: '+yourfile
                endif

   Released to Public Domain by Author.             

*/

#include "common.ch"
#include "box.ch"
#include "directry.ch"

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function pickfile()
*+
*+    Called from ( makelink.prg )   1 - function makelink()
*+                ( pbinit.prg   )   1 - procedure pbinit()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
function pickfile( cFileSpec )

local cOldString := savescreen( 8, 19, 16, 61 )
local aFiles     := directory( cFileSpec )
local aPickList  := {}
local cRetVal    := ''
local sel

default cFileSpec to '*.*'

dispbox( 8, 19, 16, 61, B_SINGLE + " ", "+W/R" )

if len( aFiles ) > 0

   aeval( aFiles, { | xx | aadd( aPickList, ;
                    pad( xx[ F_NAME ], 13 ) + ;
                    str( xx[ F_SIZE ], 8 ) + '  ' + ;
                    dtoc( xx[ F_DATE ] ) + '  ' + ;
                    xx[ F_TIME ] ) } )

   sel := achoice( 9, 20, 15, 60, aPickList )

   cRetVal := iif( lastkey() == 27, '', aFiles[ sel, 1 ] )

else

   achoice( 9, 20, 15, 60, { "No files match " + cFileSpec } )

endif

restscreen( 8, 19, 16, 61, cOldString )

return cRetVal

*+ EOF: PICKFILE.PRG
