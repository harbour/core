/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SETKEY() and related functions
 *
 * Copyright 1999 A White <awhite@user.rose.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * ChangeLog:
 *
 * V 1.1  A White       Fixed wrong parameter aClone() bug in SetKeySave()
 *                      Added SetKeyCheck()
 *                      Added SetKeyGet()
 * V 1.0  A White       Initial version, submitted to Harbour Projects
 *
 */

#include "common.ch"

// macro substitutions to access sub-array elements of aSetKeys[]
#define KEY        1
#define BLOCK      2
#define CONDITION  3

// holds array of hot-key id, code-block, activation-block
static s_aSetKeys := {}

Function SetKey( anKey, bBlock, bCondition )
   local nFound
   local bReturn
   local aKey
 
   if ISARRAY( anKey )
      aEval( anKey, {|x| setKey( x, bBlock, bCondition ) } )
 
   elseif ISNUMBER( anKey ) .and. anKey <> 0
      if ( nFound := aScan( s_aSetKeys, {|x| x[ KEY ] == anKey } ) ) == 0
         if ISBLOCK( bBlock )
            aAdd( s_aSetKeys, { anKey, bBlock, bCondition } )
        
         endif
        
      else
         aKey := s_aSetKeys[ nFound ]
        
         if aKey[ CONDITION ] == NIL .or. eval( aKey[ CONDITION ], anKey )
            bReturn := aKey[ BLOCK ]

         endif
        
         if ISBLOCK( bBlock )
            aKey[ BLOCK ]     := bBlock
            aKey[ CONDITION ] := bCondition
        
         elseif pcount() > 1 .and. bBlock == NIL
            aSize( aDel( s_aSetKeys, nFound ), len( s_aSetKeys ) - 1 )
        
         endif
        
      endif
     
   endif

return bReturn

Function HB_SetKeyGet( nKey, bCondition )
   local nFound
  
   if ISNUMBER( nKey ) .and. nKey <> 0
      if ( nFound := aScan( s_aSetKeys, {|x| x[ KEY ] == nKey } ) ) == 0
         bCondition := NIL
     
      else
         bCondition := s_aSetKeys[ nFound, CONDITION ]
         return        s_aSetKeys[ nFound, BLOCK ]
     
      endif
  
   endif

return NIL

Function HB_SetKeySave( OldKeys )
   local aReturn := aClone( s_aSetKeys )
  
   if pcount() != 0 .or. ISARRAY( OldKeys )
      if OldKeys == NIL
         s_aSetKeys := {}
     
      else
         s_aSetKeys := aClone( OldKeys )
     
      endif
  
   endif

return aReturn

Function HB_SetKeyCheck( nKey, p1, p2, p3 )
   local nFound
   local aKey
   local bBlock
  
   if ( nFound := aScan( s_aSetKeys, {|x| x[ KEY ] == nKey } ) ) > 0
      aKey   := s_aSetKeys[ nFound ]
      bBLock := aKey[ BLOCK ]
     
      if aKey[ CONDITION ] == NIL .or. eval( aKey[ CONDITION ], nKey )
       
         // is this overkill? if a code-block checks its own pcount(),
         // passing nil parameters would skew the count!
       
         do case
         case pcount() == 1 ; eval( bBlock, nKey )
         case pcount() == 2 ; eval( bBlock, p1, nKey )
         case pcount() == 3 ; eval( bBlock, p1, p2, nKey )
         otherwise          ; eval( bBlock, p1, p2, p3, nKey )
         end case
       
         return .t.
     
      endif
     
   endif

return .f.

