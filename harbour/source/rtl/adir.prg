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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    ADIR() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "directry.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      ADIR()
 *  $CATEGORY$
 *      Array
 *  $ONELINER$
 *      Fill pre-defined arrays with file/directory information
 *  $SYNTAX$
 *      ADIR( [<cFileMask>], [<aName>], [<aSize>], [<aDate>],
 *            [<aTime>], [<aAttr>] ) -> nDirEnries
 *  $ARGUMENTS$
 *      <cFileMask> File mask to include in the function return. It could
 *      contain path and standard wildcard characters as supported by your
 *      OS (like * and ?). If you omit <cFileMask> or if <cFileMask> contain
 *      no path, then the path from SET DEFAULT is used.
 *
 *      <aName> Array to fill with file name of files that meet <cFileMask>.
 *      Each element is a Character string and include the file name and
 *      extension without the path. The name is the long file name as
 *      reported by the OS and not necessarily the 8.3 uppercase name.
 *
 *      <aSize> Array to fill with file size of files that meet <cFileMask>.
 *      Each element is a Numeric integer and include the file size in Bytes.
 *      Directories are always zero in size.
 *
 *      <aDate> Array to fill with file last modification date of files that
 *      meet <cFileMask>. Each element is of type Date.
 *
 *      <aTime> Array to fill with file last modification time of files that
 *      meet <cFileMask>. Each element is a Character string in the format
 *      HH:mm:ss.
 *
 *      <aAttr> Array to fill with attribute of files that meet <cFileMask>.
 *      Each element is a Character string, see DIRECTORY() for information
 *      about attribute values. If you pass array to <aAttr>, the function
 *      is going to return files with normal, hidden, system and directory
 *      attributes. If <aAttr> is not specified or with type other than
 *      Array, only files with normal attribute would return.
 *  $RETURNS$
 *      ADIR() return the number of file entries that meet <cFileMask>
 *  $DESCRIPTION$
 *      ADIR() return the number of files and/or directories that match
 *      a specified skeleton, it also fill a series of given arrays with
 *      the name, size, date, time and attribute of those files. The passed
 *      arrays should pre-initialized to the proper size, see example below.
 *      In order to include hidden, system or directories <aAttr> must be
 *      specified.
 *
 *      ADIR() is a compatibility function, it is superseded by DIRECTORY()
 *      which return all the information in a multidimensional array.
 *  $EXAMPLES$
 *      LOCAL aName, aSize, aDate, aTime, aAttr, nLen, i
 *      nLen := ADIR( "*.JPG" )     // Number of JPG files in this directory
 *      IF nLen > 0
 *         aName := Array( nLen )   // make room to store the information
 *         aSize := Array( nLen )
 *         aDate := Array( nLen )
 *         aTime := Array( nLen )
 *         aAttr := Array( nLen )
 *         FOR i = 1 TO nLen
 *             ? aName[i], aSize[i], aDate[i], aTime[i], aAttr[i]
 *         NEXT
 *      ELSE
 *         ? "This directory is clean from smut"
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      <aName> is going to be fill with long file name and not necessarily
 *      the 8.3 uppercase name.
 *  
 *  $SEEALSO$
 *      ARRAY()  DIRECTORY() 'SET DEFAULT'
 *  $END$
 */

FUNCTION ADir( cFileMask, aName, aSize, aDate, aTime, aAttr )

   LOCAL aDir
   LOCAL nDirLen
   LOCAL nDirPos

   LOCAL nNameLen, nSizeLen, nDateLen, nTimeLen, nAttrLen

   LOCAL aFileInfo

   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   // ; CA-Clipper would fail on this case.

   IF !ISCHARACTER( cFileMask )
      RETURN 0
   ENDIF

   // ; If no drive/dir specified, use the SET DEFAULT setting.

   hb_FNameSplit( cFileMask, @cDir, @cName, @cExt )

   IF Empty( cDir )
      cFileMask := hb_FNameMerge( DefPath(), cName, cExt )
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

