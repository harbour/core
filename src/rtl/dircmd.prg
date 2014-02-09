/*
 * Harbour Project source code:
 * __Dir() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "directry.ch"
#include "fileio.ch"

#define _DIR_HEADER             1

PROCEDURE __Dir( cFileMask )

   LOCAL cPath
   LOCAL cName
   LOCAL cExt

   IF Empty( cFileMask )

      /* NOTE: Although Cl*pper has this string in the national language
               module, it will not use it from there.
               This is hard wired to English. So this is a small
               incompatibility. */

#ifdef HB_CLP_STRICT
      QOut( "Database Files    # Records    Last Update     Size" )
#else
      QOut( __natMsg( _DIR_HEADER ) )
#endif

      AEval( Directory( hb_FNameMerge( Set( _SET_DEFAULT ), "*", ".dbf" ) ), ;
         {| aDirEntry | PutDbf( aDirEntry ) } )
   ELSE

      hb_FNameSplit( LTrim( cFileMask ), @cPath, @cName, @cExt )
      IF Empty( cPath )
         cPath := Set( _SET_DEFAULT )
      ENDIF

      AEval( Directory( hb_FNameMerge( cPath, cName, cExt ) ), ;
         {| aDirEntry | PutNormal( aDirEntry ) } )
   ENDIF

   QOut()

   RETURN

STATIC PROCEDURE PutDBF( aDirEntry )

   LOCAL fhnd
   LOCAL buffer
   LOCAL nRecCount := 0
   LOCAL dLastUpdate := hb_SToD()

   IF ( fhnd := FOpen( aDirEntry[ F_NAME ] ) ) != F_ERROR

      buffer := Space( 8 )

      IF FRead( fhnd, @buffer, 8 ) == 8 .AND. ;
         AScan( { 0x03, 0x06, 0x30, 0x31, 0x83, 0x86, 0xE5, 0xE6, 0xF5, 0xF6 }, ;
         hb_BCode( buffer ) ) != 0

         nRecCount := Bin2L( hb_BSubStr( buffer, 5, 4 ) )
         dLastUpdate := hb_SToD( ;
            StrZero( hb_BPeek( buffer, 2 ) + 1900, 4 ) + ;
            StrZero( hb_BPeek( buffer, 3 ), 2 ) + ;
            StrZero( hb_BPeek( buffer, 4 ), 2 ) )

      ENDIF

      FClose( fhnd )

   ENDIF

   QOut( ;
      PadR( aDirEntry[ F_NAME ], 15 ) + ;
      Str( nRecCount, 12 ) + "    " + ;
      DToC( dLastUpdate ) + ;
      Str( aDirEntry[ F_SIZE ], 12 ) )

   RETURN

STATIC PROCEDURE PutNormal( aDirEntry )

   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( aDirEntry[ F_NAME ], NIL, @cName, @cExt )

   /* strict MS-DOS like formatting, it does not play well with long
      file names which do not stick to 8.3 MS-DOS convention */

   QOut( ;
      PadR( cName, 8 ), ;
      PadR( SubStr( cExt, 2 ), 3 ), ;
      Str( aDirEntry[ F_SIZE ], 8 ), "", ;
      DToC( aDirEntry[ F_DATE ] ) )

   RETURN
