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

   LOCAL cRoot := hb_DirBase() + ".." + hb_ps()

   WalkDir( cRoot + "include", { ... }, "core" )
   WalkDir( cRoot + "contrib", { ... }, "contrib" )
   WalkDir( cRoot + "addons" , { ... }, "addon" )

   RETURN

STATIC PROCEDURE WalkDir( cDir, aContains, cComponent )

   LOCAL aFile

   cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )

   FOR EACH aFile IN hb_DirScan( cDir, "*.hbx" )
      ProcessFile( cDir + aFile[ F_NAME ], aContains, cComponent )
   NEXT

   RETURN

STATIC PROCEDURE ProcessFile( cFileName, aContains, cComponent )

   LOCAL cDynamic
   LOCAL lFirst := .T.

   FOR EACH cDynamic IN LoadHBX( cFileName )
      IF Empty( aContains ) .OR. AScan( aContains, {| tmp | Upper( tmp ) $ Upper( cDynamic ) } ) > 0
         IF lFirst
            lFirst := .F.
            IF cComponent == "core"
               cFileName := "(" + cComponent + ")"
            ELSE
               cFileName := cComponent + " - " + hb_FNameExtSet( hb_FNameName( hb_DirSepToOS( cFileName ) ), ".hbc" )
            ENDIF
            OutStd( cFileName + ": " + hb_eol() )
         ENDIF
         OutStd( "   " + cDynamic + "()" + hb_eol() )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION LoadHBX( cFileName )

   LOCAL cFile
   LOCAL pRegex
   LOCAL tmp
   LOCAL aDynamic := {}
   LOCAL cFilter

   IF ! Empty( cFile := hb_MemoRead( cFileName ) )

      FOR EACH cFilter IN { ;
         "^DYNAMIC ([a-zA-Z0-9_]*)$", ;
         "ANNOUNCE ([a-zA-Z0-9_]*)$" }

         IF ! Empty( pRegex := hb_regexComp( cFilter, .T., .T. ) )
            FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
               AAdd( aDynamic, tmp[ 2 ] )
            NEXT
         ENDIF
      NEXT
   ENDIF

   RETURN aDynamic
