/*
 * Harbour Project source code:
 * OpenSSL API (SSL) - Harbour extensions
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbsocket.h"
#include "hbvm.h"

#include "hbssl.h"

HB_FUNC( HB_SSL_READ_ALL )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         int iMax        = hb_parnidef( 3, INT_MAX );
         int iTimeout    = hb_parnidef( 4, -1 );
         int iBufferSize = hb_parnidef( 5, 80 );

         int    iPos       = 0;
         int    iAllocated = 0;
         char * retval     = NULL;

         for(;; )
         {
            char buffer[ 1 ];
            int  iLen;
            int  sd = SSL_get_rfd( ssl );

            if( SSL_pending( ssl ) ||
                ( sd >= 0 && hb_socketSelectRead( ( HB_SOCKET ) sd, iTimeout ) ) )
            {
               iLen = SSL_read( ssl, buffer, 1 );

               if( iLen == SSL_ERROR_WANT_READ )
                  continue;
            }
            else
               break;

            if( iLen <= 0 )
            {
               if( retval )
                  hb_xfree( retval );

               hb_storc( NULL, 2 );
               hb_retni( iLen );
               return;
            }

            if( iPos == iAllocated )
            {
               iAllocated += iBufferSize;
               retval      = ( char * ) hb_xrealloc( retval, iAllocated );
            }

            retval[ iPos++ ] = buffer[ 0 ];

            if( iPos == iMax )
               break;
         }

         if( retval )
         {
            if( ! hb_storclen_buffer( retval, iPos, 2 ) )
               hb_xfree( retval );
         }
         else
            hb_storc( NULL, 2 );

         hb_retni( iPos );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_SSL_READ_LINE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         int iMax        = hb_parnidef( 3, INT_MAX );
         int iTimeout    = hb_parnidef( 4, -1 );
         int iBufferSize = hb_parnidef( 5, 80 );

         int    iPos       = 0;
         int    iAllocated = 0;
         char * retval     = NULL;

         for(;; )
         {
            char buffer[ 1 ];
            int  iLen;
            int  sd = SSL_get_rfd( ssl );

            if( SSL_pending( ssl ) ||
                ( sd >= 0 && hb_socketSelectRead( ( HB_SOCKET ) sd, iTimeout ) ) )
            {
               iLen = SSL_read( ssl, buffer, 1 );

               if( iLen == SSL_ERROR_WANT_READ )
                  continue;
            }
            else
               break;

            if( iLen <= 0 )
            {
               if( retval )
                  hb_xfree( retval );

               hb_storc( NULL, 2 );
               hb_retni( iLen );
               return;
            }
            else if( buffer[ 0 ] == '\r' )
               continue;
            else if( buffer[ 0 ] == '\n' )
               break;

            if( iPos == iAllocated )
            {
               iAllocated += iBufferSize;
               retval      = ( char * ) hb_xrealloc( retval, iAllocated );
            }

            retval[ iPos++ ] = buffer[ 0 ];

            if( iPos == iMax )
               break;
         }

         if( retval )
         {
            if( ! hb_storclen_buffer( retval, iPos, 2 ) )
               hb_xfree( retval );
         }
         else
            hb_storc( NULL, 2 );

         hb_retni( iPos );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
