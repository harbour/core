/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DIR() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#include "hbsetup.ch"

#include "directry.ch"
#include "fileio.ch"

#define _DIR_HEADER             1

PROCEDURE __Dir( cFileMask )
   LOCAL cPath
   LOCAL cName
   LOCAL cExt

   IF Empty( cFileMask )

      /* NOTE: Although Cl*pper has this string in the national language
               modul, it will not use it from here.
               This is hard wired to English. So this is a small 
               incompatibility */

#ifdef HB_C52_STRICT
      QOut( "Database Files    # Records    Last Update     Size" )
#else
      QOut( NationMsg( _DIR_HEADER ) )
#endif

      AEval( Directory( hb_FNameMerge( Set( _SET_DEFAULT ), "*", ".dbf" ) ),;
         {| aDirEntry | PutDbf( aDirEntry ) } )
   ELSE

      hb_FNameSplit( LTrim( cFileMask ), @cPath, @cName, @cExt )
      IF Empty( cPath )
         cPath := Set( _SET_DEFAULT )
      ENDIF

      AEval( Directory( hb_FNameMerge( cPath, cName, cExt ) ),;
         {| aDirEntry | PutNormal( aDirEntry ) } )
   ENDIF

   QOut()

   RETURN

STATIC PROCEDURE PutDBF( aDirEntry )
   LOCAL fhnd
   LOCAL buffer
   LOCAL nRecCount := 0
   LOCAL dLastUpdate := hb_SToD( "" )

   IF ( fhnd := FOpen( aDirEntry[ F_NAME ] ) ) != F_ERROR

      buffer := Replicate( Chr( 0 ), 8 )

      IF FRead( fhnd, @buffer, 8 ) == 8 .AND. ;
         ( Bin2W( Left( buffer, 1 ) ) == 3 .OR. ;
           Bin2W( Left( buffer, 1 ) ) == 131 )

         nRecCount := Bin2L( SubStr( buffer, 5, 4 ) )
         dLastUpdate := hb_SToD( StrZero( Bin2W( SubStr( buffer, 2, 1 ) ) + 1900, 4 ) +;
                                 StrZero( Bin2W( SubStr( buffer, 3, 1 ) ), 2 ) +;
                                 StrZero( Bin2W( SubStr( buffer, 4, 1 ) ), 2 ) )

      ENDIF

      FClose( fhnd )

   ENDIF

   QOut( PadR( aDirEntry[ F_NAME ], 15 ) +;
         Str( nRecCount, 12 ) + "    " +;
         DToC( dLastUpdate ) +;
         Str( aDirEntry[ F_SIZE ], 12 ) )

   RETURN

STATIC PROCEDURE PutNormal( aDirEntry )
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( aDirEntry[ F_NAME ], NIL, @cName, @cExt )

   QOut( PadR( cName, 8 ) + " " +;
         PadR( cExt, 3 ) + " " +;
         Str( aDirEntry[ F_SIZE ], 8 ) + "  " +;
         DToC( aDirEntry[ F_DATE ] ) )

   RETURN
