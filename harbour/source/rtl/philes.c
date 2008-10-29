/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The FileSys API (Harbour level)
 *
 * Copyright 1999 Manuel Ruiz <mrt@joca.es>
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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    CURDIR()
 *    HB_FLOCK()
 *    HB_FUNLOCK()
 *
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
 *    HB_FEOF()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapiitm.h"

HB_FUNC( FOPEN )
{
   if( ISCHAR( 1 ) )
   {
      hb_retnint( ( HB_NHANDLE ) hb_fsOpen( ( BYTE * ) hb_parc( 1 ),
                  ISNUM( 2 ) ? ( USHORT ) hb_parni( 2 ) : FO_READ | FO_COMPAT ) );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_fsSetFError( 0 );
      /* NOTE: Undocumented but existing Clipper Run-time error */
      hb_errRT_BASE( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( FCREATE )
{
   if( ISCHAR( 1 ) )
   {
      hb_retnint( ( HB_NHANDLE ) hb_fsCreate( ( BYTE * ) hb_parc( 1 ),
                  ISNUM( 2 ) ? hb_parni( 2 ) : FC_NORMAL ) );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( F_ERROR );
      hb_fsSetFError( 0 );
   }
}

HB_FUNC( HB_FCREATE )
{
   if( ISCHAR( 1 ) )
   {
      hb_retnint( ( HB_NHANDLE ) hb_fsCreateEx( ( BYTE * ) hb_parc( 1 ),
                  ISNUM( 2 ) ? hb_parni( 2 ) : FC_NORMAL,
                  ISNUM( 3 ) ? ( USHORT ) hb_parni( 3 ) : FO_COMPAT ) );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( F_ERROR );
      hb_fsSetFError( 0 );
   }
}

HB_FUNC( FREAD )
{
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   USHORT uiError = 0;
   ULONG ulRead = 0;

   if( ISNUM( 1 ) && pBuffer && ISBYREF( 2 ) && ISNUM( 3 ) )
   {
      ulRead = hb_parnl( 3 );

      /* NOTE: CA-Cl*pper determines the maximum size by calling _parcsiz()
               instead of _parclen(), this means that the maximum read length
               will be one more than the length of the passed buffer, because
               the terminating zero could be used if needed. [vszakats] */

      if( ulRead <= hb_parcsiz( 2 ) )
      {
         /* NOTE: Warning, the read buffer will be directly modified,
                  this is normal here ! [vszakats] */

         /* Unshare the item to avoid GPF on static buffers and changing
            other items which shares this buffer. [druzus] */
         pBuffer = hb_itemUnShareString( pBuffer );

         ulRead = hb_fsReadLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                  ( BYTE * ) hb_itemGetCPtr( pBuffer ),
                                  ulRead );
         uiError = hb_fsError();
      }
      else
         ulRead = 0;
   }

   hb_retnint( ulRead );
   hb_fsSetFError( uiError );
}

HB_FUNC( FWRITE )
{
   USHORT uiError = 0;

   if( ISNUM( 1 ) && ISCHAR( 2 ) )
   {
      hb_retnl( hb_fsWriteLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                 ( BYTE * ) hb_parc( 2 ),
                                 ISNUM( 3 ) ? ( ULONG ) hb_parnl( 3 ) : hb_parclen( 2 ) ) );
      uiError = hb_fsError();
   }
   else
      hb_retni( 0 );
   hb_fsSetFError( uiError );
}

HB_FUNC( FERROR )
{
   hb_retni( hb_fsGetFError() );
}

HB_FUNC( FCLOSE )
{
   USHORT uiError = 0;
   if( ISNUM( 1 ) )
   {
      hb_fsClose( hb_numToHandle( hb_parnint( 1 ) ) );
      uiError = hb_fsError();
      hb_retl( uiError == 0 );
   }
   else
      hb_retl( FALSE );
   hb_fsSetFError( uiError );
}

HB_FUNC( FERASE )
{
   USHORT uiError = 3;

   if( ISCHAR( 1 ) )
   {
      hb_retni( hb_fsDelete( ( BYTE * ) hb_parc( 1 ) ) ? 0 : F_ERROR );
      uiError = hb_fsError();
   }
   else
      hb_retni( F_ERROR );
   hb_fsSetFError( uiError );
}

HB_FUNC( FRENAME )
{
   USHORT uiError = 2;

   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      hb_retni( hb_fsRename( ( BYTE * ) hb_parc( 1 ),
                             ( BYTE * ) hb_parc( 2 ) ) ? 0 : F_ERROR );
      uiError = hb_fsError();
   }
   else
      hb_retni( F_ERROR );
   hb_fsSetFError( uiError );
}

HB_FUNC( FSEEK )
{
   USHORT uiError = 0;

   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      hb_retnint( hb_fsSeekLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                  hb_parnint( 2 ),
                                  ISNUM( 3 ) ? ( USHORT ) hb_parni( 3 ) : FS_SET ) );
      uiError = hb_fsError();
   }
   else
      hb_retni( 0 );

   hb_fsSetFError( uiError );
}

HB_FUNC( FREADSTR )
{
   USHORT uiError = 0;

   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      ULONG ulToRead = ( ULONG ) hb_parnl( 2 );

      if( ulToRead > 0 )
      {
         HB_FHANDLE fhnd = ( HB_FHANDLE ) hb_parni( 1 );
         BYTE * buffer = ( BYTE * ) hb_xgrab( ulToRead + 1 );
         ULONG ulRead;

         ulRead = hb_fsReadLarge( fhnd, buffer, ulToRead );
         uiError = hb_fsError();
         buffer[ ulRead ] = '\0';

         /* NOTE: Clipper will not return zero chars from this functions. */
         hb_retc_buffer( ( char * ) buffer );
      }
      else
         hb_retc( NULL );
   }
   else
      hb_retc( NULL );
   hb_fsSetFError( uiError );
}

/* NOTE: This function should not return the leading and trailing */
/*       (back)slashes. [vszakats] */

/* TODO: Xbase++ is able to change to the specified directory. */

HB_FUNC( CURDIR )
{
   BYTE byBuffer[ _POSIX_PATH_MAX + 1 ];

   hb_fsCurDirBuff( ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
      ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) : 0, byBuffer, _POSIX_PATH_MAX + 1 );

   hb_retc( ( char * ) byBuffer );
}

HB_FUNC( HB_FEOF )
{
   USHORT uiError = 6;

   if( ISNUM( 1 ) )
   {
      hb_retl( hb_fsEof( hb_numToHandle( hb_parnint( 1 ) ) ) );
      uiError = hb_fsError();
   }
   else
      hb_retl( TRUE );
   hb_fsSetFError( uiError );
}

HB_FUNC( HB_FCOMMIT )
{
   USHORT uiError = 6;

   if( ISNUM( 1 ) )
   {
      hb_fsCommit( hb_numToHandle( hb_parnint( 1 ) ) );
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
}

HB_FUNC( HB_FLOCK )
{
   USHORT uiError = 0;
   BOOL fResult = FALSE;

   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) )
   {
      fResult = hb_fsLockLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                ( HB_FOFFSET ) hb_parnint( 2 ),
                                ( HB_FOFFSET ) hb_parnint( 3 ),
                                FL_LOCK | ( ( USHORT ) hb_parni( 4 ) & ~FL_MASK ) );
      uiError = hb_fsError();
   }
   hb_fsSetFError( uiError );
   hb_retl( fResult );
}

HB_FUNC( HB_FUNLOCK )
{
   USHORT uiError = 0;
   BOOL fResult = FALSE;

   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) )
   {
      fResult = hb_fsLockLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                ( HB_FOFFSET ) hb_parnint( 2 ),
                                ( HB_FOFFSET ) hb_parnint( 3 ),
                                FL_UNLOCK );
      uiError = hb_fsError();
   }
   hb_fsSetFError( uiError );
   hb_retl( fResult );
}

HB_FUNC( HB_OSERROR )
{
   hb_retni( hb_fsOsError() );
}

HB_FUNC( HB_OSPATHSEPARATOR )
{
   hb_retc_const( HB_OS_PATH_DELIM_CHR_STRING );
}

HB_FUNC( HB_OSPATHLISTSEPARATOR )
{
   static const char s_ret[ 2 ] = { HB_OS_PATH_LIST_SEP_CHR, '\0' };
   hb_retc_const( s_ret );
}

HB_FUNC( HB_OSPATHDELIMITERS )
{
   hb_retc_const( HB_OS_PATH_DELIM_CHR_LIST );
}

HB_FUNC( HB_OSDRIVESEPARATOR )
{
#ifdef HB_OS_HAS_DRIVE_LETTER
   static const char s_ret[ 2 ] = { HB_OS_DRIVE_DELIM_CHR, '\0' };
   hb_retc_const( s_ret );
#else
   hb_retc_null();
#endif
}

HB_FUNC( HB_OSFILEMASK )
{
   hb_retc_const( HB_OS_ALLFILE_MASK );
}
