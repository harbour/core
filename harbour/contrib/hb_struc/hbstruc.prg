/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base Class for internal handling of class creation
 *
 * Copyright 2000 Ron Pinkas <Ron@Profit-Master.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbstruc.ch"

//----------------------------------------------------------------------------//

Function HB_Structure( cStructureName AS Char, aMembers AS Array OF Char )

  STRUCTURE HB_Structure cName AS Char, hId As Num

  DECLARE __ClsNew( ClassName AS Char, N As Num ) AS Num
  DECLARE __ClsAddMsg( H AS Num, Data AS Char, ID As Num, Type As Num ) AS Num
  DECLARE __ClsInst( H AS Num ) AS Structure HB_Structure

  LOCAL hStructure AS Num, nCounter AS Num, nMembers AS Num

  STATIC asStructures AS Array OF Structure HB_Structure := {}

  STATIC sStructure AS Stru HB_Structure

  LOCAL hSelf As Num

  cStructureName := Upper( cStructureName )

  hStructure := aScan( asStructures, { |aStructure| aStructure:cName == cStructureName } )

  IF aMembers == NIL
     IF hStructure == 0
        //hb_Structure( cStructureName, {} )
        RETURN NIL //hb_Structure( cStructureName )
     ELSE
        RETURN __ClsInst( asStructures[ hStructure ]:hId )
     ENDIF
  ELSE
     IF hStructure > 0
        // Duplicate declaration
        RETURN NIL
     ENDIF
  ENDIF

  nMembers := Len( aMembers )

  hStructure := __ClsNew( cStructureName, nMembers )

  FOR nCounter := 1 TO nMembers
     __clsAddMsg( hStructure, aMembers[nCounter], nCounter, 1 )
     __clsAddMsg( hStructure, '_' + aMembers[nCounter], nCounter, 1 )
  NEXT

  IF sStructure == NIL
     hSelf := __ClsNew( "HB_Structure", 2 )

     __clsAddMsg( hSelf, "cName", 1, 1 )
     __clsAddMsg( hSelf, "_cName", 1, 1 )
     __clsAddMsg( hSelf, "hID", 2, 1 )
     __clsAddMsg( hSelf, "_hID", 2, 1 )

     sStructure := __ClsInst( hSelf )
  ENDIF

  sStructure:cName := cStructureName
  sStructure:hId   := hStructure

  aAdd( asStructures, sStructure )

RETURN NIL //__clsInst( hStructure )

