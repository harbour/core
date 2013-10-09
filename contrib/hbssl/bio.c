/*
 * Harbour Project source code:
 * OpenSSL API (BIO) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
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

#include "hbssl.h"

void * hb_BIO_is( int iParam )
{
   return hb_parptr( iParam );
}

BIO * hb_BIO_par( int iParam )
{
   return ( BIO * ) hb_parptr( iParam );
}

static int hb_BIO_METHOD_is( int iParam )
{
   return HB_ISCHAR( iParam );
}

static BIO_METHOD * hb_BIO_METHOD_par( int iParam )
{
   BIO_METHOD * p;

   switch( hb_parni( iParam ) )
   {
      case HB_BIO_METHOD_S_NULL:        p = BIO_s_null();       break;
#ifndef OPENSSL_NO_FP_API
      case HB_BIO_METHOD_S_FILE:        p = BIO_s_file();       break;
#endif
      case HB_BIO_METHOD_S_MEM:         p = BIO_s_mem();        break;
      case HB_BIO_METHOD_S_SOCKET:      p = BIO_s_socket();     break;
      case HB_BIO_METHOD_S_CONNECT:     p = BIO_s_connect();    break;
      case HB_BIO_METHOD_S_ACCEPT:      p = BIO_s_accept();     break;
      case HB_BIO_METHOD_S_FD:          p = BIO_s_fd();         break;
#if 0 /* BIO_s_log() isn't exported via implibs on Windows at version 0.9.8k. [vszakats] */
#ifndef OPENSSL_SYS_OS2
      case HB_BIO_METHOD_S_LOG:         p = BIO_s_log();        break;
#endif
#endif
      case HB_BIO_METHOD_S_BIO:         p = BIO_s_bio();        break;
#ifndef OPENSSL_NO_DGRAM
      case HB_BIO_METHOD_S_DATAGRAM:    p = BIO_s_datagram();   break;
#endif
      case HB_BIO_METHOD_F_NULL:        p = BIO_f_null();       break;
      case HB_BIO_METHOD_F_BUFFER:      p = BIO_f_buffer();     break;
#ifdef OPENSSL_SYS_VMS
      case HB_BIO_METHOD_F_LINEBUFFER:  p = BIO_f_linebuffer(); break;
#endif
      case HB_BIO_METHOD_F_NBIO_TEST:   p = BIO_f_nbio_test();  break;
      default:                          p = NULL;
   }

   return p;
}

#if 0
/* NOTE: Unused yet. Commented to avoid warning */
static int hb_BIO_METHOD_ptr_to_id( const BIO_METHOD * p )
{
   int n;

   if(      p == BIO_s_null()       ) n = HB_BIO_METHOD_S_NULL;
#ifndef OPENSSL_NO_FP_API
   else if( p == BIO_s_file()       ) n = HB_BIO_METHOD_S_FILE;
#endif
   else if( p == BIO_s_mem()        ) n = HB_BIO_METHOD_S_MEM;
   else if( p == BIO_s_socket()     ) n = HB_BIO_METHOD_S_SOCKET;
   else if( p == BIO_s_connect()    ) n = HB_BIO_METHOD_S_CONNECT;
   else if( p == BIO_s_accept()     ) n = HB_BIO_METHOD_S_ACCEPT;
   else if( p == BIO_s_fd()         ) n = HB_BIO_METHOD_S_FD;
#if 0 /* BIO_s_log() isn't exported via implibs on Windows at version 0.9.8k. [vszakats] */
#ifndef OPENSSL_SYS_OS2
   else if( p == BIO_s_log()        ) n = HB_BIO_METHOD_S_LOG;
#endif
#endif
   else if( p == BIO_s_bio()        ) n = HB_BIO_METHOD_S_BIO;
#ifndef OPENSSL_NO_DGRAM
   else if( p == BIO_s_datagram()   ) n = HB_BIO_METHOD_S_DATAGRAM;
#endif
   else if( p == BIO_f_null()       ) n = HB_BIO_METHOD_F_NULL;
   else if( p == BIO_f_buffer()     ) n = HB_BIO_METHOD_F_BUFFER;
#ifdef OPENSSL_SYS_VMS
   else if( p == BIO_f_linebuffer() ) n = HB_BIO_METHOD_F_LINEBUFFER;
#endif
   else if( p == BIO_f_nbio_test()  ) n = HB_BIO_METHOD_F_NBIO_TEST;
   else                               n = HB_BIO_METHOD_UNSUPPORTED;

   return n;
}
#endif

HB_FUNC( BIO_NEW )
{
   if( hb_BIO_METHOD_is( 1 ) )
      hb_retptr( BIO_new( hb_BIO_METHOD_par( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio && hb_BIO_METHOD_is( 2 ) )
      hb_retni( BIO_set( bio, hb_BIO_METHOD_par( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_CLEAR_FLAGS )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      BIO_clear_flags( bio, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_FLAGS )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      BIO_set_flags( bio, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GET_FLAGS )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_get_flags( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_TEST_FLAGS )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
#if ! defined( HB_OPENSSL_OLD_OSX_ )
      hb_retni( BIO_test_flags( bio, hb_parni( 2 ) ) );
#else
      hb_retni( 0 );
#endif
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_FD )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_set_fd( bio, hb_parnl( 2 ), hb_parnidef( 3, BIO_NOCLOSE ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GET_FD )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retnl( BIO_get_fd( bio, NULL ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GET_RETRY_REASON )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_get_retry_reason( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_RETRY_SPECIAL )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      BIO_set_retry_special( bio );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_RETRY_READ )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      BIO_set_retry_read( bio );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_RETRY_WRITE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      BIO_set_retry_write( bio );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SHOULD_READ )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_should_read( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SHOULD_WRITE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_should_write( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SHOULD_IO_SPECIAL )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_should_io_special( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_RETRY_TYPE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_retry_type( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SHOULD_RETRY )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_should_retry( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_CTRL_PENDING )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retnint( BIO_ctrl_pending( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_CTRL_WPENDING )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retnint( BIO_ctrl_wpending( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_FLUSH )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_flush( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SEEK )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retnl( BIO_seek( bio, hb_parnl( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_TELL )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retnl( BIO_tell( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_RESET )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_reset( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_EOF )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_eof( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_CLOSE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_set_close( bio, hb_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GET_CLOSE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_get_close( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_NEW_SOCKET )
{
   if( HB_ISNUM( 1 ) )
      hb_retptr( BIO_new_socket( hb_parni( 1 ), hb_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_NEW_DGRAM )
{
#ifndef OPENSSL_NO_DGRAM
   if( HB_ISNUM( 1 ) )
      hb_retptr( BIO_new_dgram( hb_parni( 1 ), hb_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_errRT_BASE( EG_NOFUNC, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( BIO_NEW_FD )
{
   if( HB_ISNUM( 1 ) )
      hb_retptr( BIO_new_fd( hb_parnl( 1 ), hb_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_NEW_FILE )
{
   if( HB_ISCHAR( 1 ) )
      hb_retptr( BIO_new_file( hb_parc( 1 ), hb_parcx( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_NEW_MEM_BUF )
{
   if( HB_ISCHAR( 1 ) )
      /* NOTE: Discarding 'const' */
      hb_retptr( BIO_new_mem_buf( ( char * ) hb_parc( 1 ), ( int ) hb_parclen( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_READ )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
   {
      int size = HB_ISNUM( 3 ) ? hb_parni( 3 ) : ( int ) hb_parclen( 2 );

      if( size > 0 )
      {
         char * buffer = ( char * ) hb_xgrab( size + 1 );

         hb_retni( size = BIO_read( bio, buffer, size ) );

         if( ! hb_storclen( buffer, size, 2 ) )
            hb_xfree( buffer );
      }
      else
      {
         hb_retni( 0 );
         hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GETS )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
   {
      int size = HB_ISNUM( 3 ) ? hb_parni( 3 ) : ( int ) hb_parclen( 2 );

      if( size > 0 )
      {
         char * buffer = ( char * ) hb_xgrab( size + 1 );

         hb_retni( size = BIO_gets( bio, buffer, size ) );

         if( ! hb_storclen( buffer, size, 2 ) )
            hb_xfree( buffer );
      }
      else
      {
         hb_retni( 0 );
         hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_WRITE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
   {
      int size = ( int ) hb_parclen( 2 );

      if( HB_ISNUM( 3 ) )
      {
         int towrite = hb_parni( 3 );
         if( towrite >= 0 && towrite < size )
            size = towrite;
      }

      hb_retni( BIO_write( bio, hb_parcx( 2 ), size ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_PUTS )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_puts( bio, hb_parcx( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_VFREE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      BIO_vfree( bio );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_FREE )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_free( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_FREE_ALL )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      BIO_free_all( bio );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* ------------ connect ------------ */

HB_FUNC( BIO_NEW_CONNECT )
{
   if( HB_ISCHAR( 1 ) )
      /* NOTE: Discarding 'const' */
      hb_retptr( BIO_new_connect( ( char * ) hb_parc( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_NEW_ACCEPT )
{
   if( HB_ISCHAR( 1 ) )
      /* NOTE: Discarding 'const' */
      hb_retptr( BIO_new_accept( ( char * ) hb_parc( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_CONN_HOSTNAME )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio && HB_ISCHAR( 2 ) )
      hb_retnl( BIO_set_conn_hostname( bio, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_CONN_PORT )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio && HB_ISCHAR( 2 ) )
      hb_retnl( BIO_set_conn_port( bio, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_CONN_INT_PORT )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio && HB_ISNUM( 2 ) )
   {
      int port = hb_parni( 2 );
      hb_retnl( BIO_set_conn_port( bio, &port ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_SET_CONN_IP )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio && HB_ISCHAR( 2 ) && hb_parclen( 2 ) == 4 )
      hb_retnl( BIO_set_conn_ip( bio, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GET_CONN_HOSTNAME )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retc( BIO_get_conn_hostname( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GET_CONN_PORT )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retc( BIO_get_conn_port( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_GET_CONN_IP )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retc( BIO_get_conn_ip( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if 0
/* NOTE: Commented due to bugs in OpenSSL declaration. Bug report sent #1989 */
HB_FUNC( BIO_GET_CONN_INT_PORT )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retnl( BIO_get_conn_int_port( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
#endif

HB_FUNC( BIO_SET_NBIO )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retnl( BIO_set_nbio( bio, hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( BIO_DO_CONNECT )
{
   BIO * bio = hb_BIO_par( 1 );

   if( bio )
      hb_retni( BIO_do_connect( bio ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( ERR_LOAD_BIO_STRINGS )
{
   ERR_load_BIO_strings();
}

#if 0

#define BIO_set_url( b, url )                 BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 0, ( char * ) ( url ) )
#define BIO_set_proxies( b, p )               BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 1, ( char * ) ( p ) )
/* BIO_set_nbio(b,n) */
#define BIO_set_filter_bio( b, s )            BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 2, ( char * ) ( s ) )
/* BIO *BIO_get_filter_bio(BIO *bio); */
#define BIO_set_proxy_cb( b, cb )             BIO_callback_ctrl( b, BIO_C_SET_PROXY_PARAM, 3, ( void *( *cb )( ) ) )
#define BIO_set_proxy_header( b, sk )         BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 4, ( char * ) sk )
#define BIO_set_no_connect_return( b, bool )  BIO_int_ctrl( b, BIO_C_SET_PROXY_PARAM, 5, bool )

#define BIO_get_proxy_header( b, skp )        BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 0, ( char * ) skp )
#define BIO_get_proxies( b, pxy_p )           BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 1, ( char * ) ( pxy_p ) )
#define BIO_get_url( b, url )                 BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 2, ( char * ) ( url ) )
#define BIO_get_no_connect_return( b )        BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 5, NULL )

#define BIO_set_fp( b, fp, c )                BIO_ctrl( b, BIO_C_SET_FILE_PTR, c, ( char * ) fp )
#define BIO_get_fp( b, fpp )                  BIO_ctrl( b, BIO_C_GET_FILE_PTR, 0, ( char * ) fpp )

int   BIO_indent( BIO * b, int indent, int max );
long  BIO_ctrl( BIO * bp, int cmd, long larg, void * parg );
long  BIO_callback_ctrl( BIO * b, int cmd, void ( * fp )( struct bio_st *, int, const char *, int, long, long ) );
char * BIO_ptr_ctrl( BIO * bp, int cmd, long larg );
long  BIO_int_ctrl( BIO * bp, int cmd, long larg, int iarg );
BIO * BIO_push( BIO * b, BIO * append );
BIO * BIO_pop( BIO * b );
BIO * BIO_find_type( BIO * b, int bio_type );
BIO * BIO_next( BIO * b );
BIO * BIO_get_retry_BIO( BIO * bio, int * reason );
BIO * BIO_dup_chain( BIO * in );

int BIO_nread0( BIO * bio, char ** buf );
int BIO_nread( BIO * bio, char ** buf, int num );
int BIO_nwrite0( BIO * bio, char ** buf );
int BIO_nwrite( BIO * bio, char ** buf, int num );

BIO_METHOD *   BIO_s_mem( void );

BIO_set_mem_eof_return( BIO * b, int v )
long BIO_get_mem_data( BIO * b, char ** pp )
BIO_set_mem_buf( BIO * b, BUF_MEM * bm, int c )
BIO_get_mem_ptr( BIO * b, BUF_MEM * *pp )

BIO * BIO_new_mem_buf( void * buf, int len );

#endif
