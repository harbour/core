/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Lists selected functions and their location
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#include "directry.ch"

PROCEDURE Main( ... )

   WalkDir( hb_DirBase() + ".." + hb_ps(), { ... } )

   RETURN

STATIC PROCEDURE WalkDir( cDir, aContains )

   LOCAL aFile

   FOR EACH aFile IN Directory( cDir + hb_osFileMask(), "D" )
      IF aFile[ F_NAME ] == "." .OR. aFile[ F_NAME ] == ".."
      ELSEIF "D" $ aFile[ F_ATTR ]
         WalkDir( cDir + aFile[ F_NAME ] + hb_ps(), aContains )
      ELSEIF hb_FNameExt( aFile[ F_NAME ] ) == ".hbx"
         ProcessFile( cDir + aFile[ F_NAME ], aContains )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION PathSepToSelf( cFileName )
   RETURN StrTran( cFileName, iif( hb_ps() == "\", "/", "\" ), hb_ps() )

STATIC PROCEDURE ProcessFile( cFileName, aContains )

   LOCAL cDynamic
   LOCAL lFirst := .T.

   FOR EACH cDynamic IN __hb_extern_get_exception_list( cFileName )
      IF Empty( aContains ) .OR. AScan( aContains, {| tmp | Upper( tmp ) $ Upper( cDynamic ) } ) > 0
         IF lFirst
            lFirst := .F.
            OutStd( PathSepToSelf( cFileName ) + hb_eol() )
         ENDIF
         OutStd( "   " + cDynamic + "()" + hb_eol() )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION __hb_extern_get_exception_list( cInputName )

   LOCAL cFile
   LOCAL pRegex
   LOCAL tmp
   LOCAL aDynamic := {}

   IF ! Empty( cFile := MemoRead( cInputName ) ) .AND. ;
      ! Empty( pRegex := hb_regexComp( "^DYNAMIC ([a-zA-Z0-9_]*)$", .T., .T. ) )
      FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
         AAdd( aDynamic, tmp[ 2 ] )
      NEXT
   ENDIF

   RETURN aDynamic
