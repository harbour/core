/*
 * Harbour Project source code:
 * DirChange(), MakeDir(), DirRemove(), IsDisk(), DiskChange(), DiskName() functions
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( HB_DIRCREATE )
{
   if( HB_ISCHAR( 1 ) )
      hb_retni( hb_fsMkDir( hb_parc( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( F_ERROR );
}

HB_FUNC( HB_DIRDELETE )
{
   if( HB_ISCHAR( 1 ) )
      hb_retni( hb_fsRmDir( hb_parc( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( F_ERROR );
}

/* NOTE: Clipper 5.3 functions */

#ifdef HB_COMPAT_C53

HB_FUNC( DIRCHANGE )
{
   if( HB_ISCHAR( 1 ) )
      hb_retni( hb_fsChDir( hb_parc( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( F_ERROR );
}

/* NOTE: Clipper 5.3 NG incorrectly states that the name of this function is
         DirMake(), in reality it's not. */

HB_FUNC_TRANSLATE( MAKEDIR, HB_DIRCREATE )
HB_FUNC_TRANSLATE( DIRREMOVE, HB_DIRDELETE )

/* NOTE: Clipper 5.3 undocumented */

#ifdef HB_CLP_UNDOC

HB_FUNC( ISDISK )
{
   HB_BOOL fResult = HB_FALSE;
   const char * szDrive = hb_parc( 1 );

   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         fResult = hb_fsIsDrv( *szDrive - 'A' ) == 0;
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         fResult = hb_fsIsDrv( *szDrive - 'a' ) == 0;
   }
   hb_retl( fResult );
}

HB_FUNC( DISKCHANGE )
{
   HB_BOOL fResult = HB_FALSE;
   const char * szDrive = hb_parc( 1 );

   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         fResult = hb_fsChDrv( *szDrive - 'A' ) == 0;
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         fResult = hb_fsChDrv( *szDrive - 'a' ) == 0;
   }
   hb_retl( fResult );
}

HB_FUNC( DISKNAME )
{
#if defined( HB_OS_HAS_DRIVE_LETTER )
   char szDrive[ 1 ];

   szDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';
   hb_retclen( szDrive, 1 );
#else
   hb_retc_null();
#endif
}

#endif

#endif
