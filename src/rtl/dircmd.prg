/*
 * __Dir() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#pragma -gc0

#include "directry.ch"
#include "fileio.ch"

#define _DIR_HEADER  1

PROCEDURE __Dir( cFileMask )

   LOCAL cPath
   LOCAL cName
   LOCAL cExt

   IF Set( _SET_TRIMFILENAME )
      cFileMask := AllTrim( cFileMask )
   ENDIF

   IF HB_ISNULL( cFileMask )

      /* NOTE: Although Cl*pper has this string in the national language
               module, it will not use it from there.
               This is hard-wired to English. So this is a small
               incompatibility. */

#ifdef HB_CLP_STRICT
      QOut( "Database Files    # Records    Last Update     Size" )
#else
      QOut( __natMsg( _DIR_HEADER ) )
#endif

      AEval( hb_vfDirectory( hb_FNameMerge( Set( _SET_DEFAULT ), "*", ".dbf" ) ), ;
             {| aDirEntry | PutDbf( aDirEntry ) } )
   ELSE
      hb_FNameSplit( cFileMask, @cPath, @cName, @cExt )
      IF HB_ISNULL( cPath )
         cPath := Set( _SET_DEFAULT )
      ENDIF

      AEval( hb_vfDirectory( hb_FNameMerge( cPath, cName, cExt ) ), ;
             {| aDirEntry | PutNormal( aDirEntry ) } )
   ENDIF

   QOut()

   RETURN

#define _DBF_HEAD_MARK  hb_BChar( 0x03 ) + hb_BChar( 0x06 ) + ;
                        hb_BChar( 0x30 ) + hb_BChar( 0x31 ) + ;
                        hb_BChar( 0x83 ) + hb_BChar( 0x86 ) + ;
                        hb_BChar( 0xE5 ) + hb_BChar( 0xE6 ) + ;
                        hb_BChar( 0xF5 ) + hb_BChar( 0xF6 )

STATIC PROCEDURE PutDBF( aDirEntry )

   LOCAL hFile
   LOCAL buffer
   LOCAL nRecCount := 0
   LOCAL dLastUpdate := hb_SToD()

   IF ( hFile := hb_vfOpen( aDirEntry[ F_NAME ], FO_READ ) ) != NIL

      buffer := hb_vfReadLen( hFile, 8 )

      IF hb_BLen( buffer ) == 8 .AND. hb_BAt( hb_BLeft( buffer, 1 ), _DBF_HEAD_MARK ) > 0
         nRecCount := Bin2L( hb_BSubStr( buffer, 5, 4 ) )
         dLastUpdate := hb_Date( hb_BPeek( buffer, 2 ) + 1900, ;
                                 hb_BPeek( buffer, 3 ), ;
                                 hb_BPeek( buffer, 4 ) )
      ENDIF

      hb_vfClose( hFile )
   ENDIF

   QOut( ;
      hb_UPadR( aDirEntry[ F_NAME ], 15 ) + ;
      Str( nRecCount, 12 ), "  ", ;
      DToC( dLastUpdate ) + ;
      Str( aDirEntry[ F_SIZE ], 12 ) )

   RETURN

STATIC PROCEDURE PutNormal( aDirEntry )

   LOCAL cName, cExt

   hb_FNameSplit( aDirEntry[ F_NAME ],, @cName, @cExt )

   /* Strict MS-DOS like formatting, it does not play well with long
      filenames which do not stick to 8.3 MS-DOS convention */

   QOut( ;
      hb_UPadR( cName, 8 ), ;
      hb_UPadR( hb_USubStr( cExt, 2 ), 3 ), ;
      Str( aDirEntry[ F_SIZE ], 8 ), "", ;
      aDirEntry[ F_DATE ] )

   RETURN
