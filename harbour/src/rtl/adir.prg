/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ADIR() function
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.hu)
 * www - http://harbour-project.org
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

#include "common.ch"
#include "directry.ch"

FUNCTION ADir( cFileMask, aName, aSize, aDate, aTime, aAttr )

   LOCAL aDir
   LOCAL nDirLen
   LOCAL nDirPos

   LOCAL nNameLen, nSizeLen, nDateLen, nTimeLen, nAttrLen

   LOCAL aFileInfo

   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   // ; CA-Cl*pper would fail on this case.

   IF !ISCHARACTER( cFileMask )
      RETURN 0
   ENDIF

   // ; If no drive/dir specified, use the SET DEFAULT setting.

   hb_FNameSplit( cFileMask, @cDir, @cName, @cExt )

   IF Empty( cDir )
      cFileMask := hb_FNameMerge( __DefPath(), cName, cExt )
   ENDIF

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
