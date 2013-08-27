/*
 * Harbour Project source code
 * POSIX function errno handling
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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

#include "hbposix.h"
#include "hbstack.h"
#include "hbapierr.h"

typedef struct
{
   int iErrNo;
} HB_POSIXERRDATA, * PHB_POSIXERRDATA;

static HB_TSD_NEW( s_posix_errno, sizeof( HB_POSIXERRDATA ), NULL, NULL );

#define HB_POSIX_ERRNO  ( ( PHB_POSIXERRDATA ) hb_stackGetTSD( &s_posix_errno ) )

void hb_posix_save_errno( void )
{
   HB_POSIX_ERRNO->iErrNo = errno;
}

void hb_posix_set_errno( int iErrNo )
{
   HB_POSIX_ERRNO->iErrNo = iErrNo;
}

int hb_posix_get_errno( void )
{
   return HB_POSIX_ERRNO->iErrNo;
}

void hb_posix_param_error( void )
{
   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

void hb_posix_result( int iResult )
{
   if( iResult == -1 )
      hb_posix_save_errno();
   else
      hb_posix_set_errno( 0 );

   hb_retni( iResult );
}


HB_FUNC( POSIX_ERRNO )
{
   hb_retni( HB_POSIX_ERRNO->iErrNo );
}
