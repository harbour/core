/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ADIR() function
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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

#include "common.ch"
#include "directry.ch"

FUNCTION aDir( cFileMask, aName, aSize, aDate, aTime, aAttr )

   LOCAL aDir
   LOCAL nDirLen
   LOCAL nDirPos

   LOCAL nNameLen, nSizeLen, nDateLen, nTimeLen, nAttrLen

   LOCAL aFileInfo

   // ; CA-Clipper would fail on this case.

   IF !ISCHARACTER( cFileMask )
      RETURN 0
   ENDIF

   // ;

   /* TODO: Some portable code should be added here, which would  */
   /*       use the drive + dir from Set( _SET_DEFAULT ) if there */
   /*       was none specified in cFileMask                       */

   // ;

   IF ISARRAY( aAttr )
      aDir := Directory( cFileMask, "HSD" )
   ELSE
      aDir := Directory( cFileMask )
   ENDIF

   IF ISARRAY( aName )
      nNameLen := Len( aName )
   ENDIF
   IF ISARRAY( aSize )
      nSizeLen := Len( aSize )
   ENDIF
   IF ISARRAY( aDate )
      nDateLen := Len( aDate )
   ENDIF
   IF ISARRAY( aTime )
      nTimeLen := Len( aTime )
   ENDIF
   IF ISARRAY( aAttr )
      nAttrLen := Len( aAttr )
   ENDIF

   // ;

   nDirLen := Len( aDir )

   FOR nDirPos := 1 TO nDirLen

      aFileInfo := aDir[ nDirPos ]

      IF nNameLen != NIL .AND. nNameLen >= nDirPos
         aName[ nDirPos ] := aFileInfo[ F_NAME ]
      ENDIF
      IF nSizeLen != NIL .AND. nSizeLen >= nDirPos
         aSize[ nDirPos ] := aFileInfo[ F_SIZE ]
      ENDIF
      IF nDateLen != NIL .AND. nDateLen >= nDirPos
         aDate[ nDirPos ] := aFileInfo[ F_DATE ]
      ENDIF
      IF nTimeLen != NIL .AND. nTimeLen >= nDirPos
         aTime[ nDirPos ] := aFileInfo[ F_TIME ]
      ENDIF
      IF nAttrLen != NIL .AND. nAttrLen >= nDirPos
         aAttr[ nDirPos ] := aFileInfo[ F_ATTR ]
      ENDIF

   NEXT

   RETURN nDirLen

