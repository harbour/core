/*
 * Harbour Project source code:
 * POSIX function wrappers to get/set user and group IDs
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

HB_FUNC( POSIX_GETUID )
{
   hb_ret_uid( getuid() );
}

HB_FUNC( POSIX_GETEUID )
{
   hb_ret_uid( geteuid() );
}

HB_FUNC( POSIX_GETGID )
{
   hb_ret_uid( getgid() );
}

HB_FUNC( POSIX_GETEGID )
{
   hb_ret_uid( getegid() );
}

HB_FUNC( POSIX_SETUID )
{
   if( HB_ISNUM( 1 ) )
      hb_posix_result( setuid( hb_par_uid( 1 ) ) );
   else
      hb_posix_param_error();
}

HB_FUNC( POSIX_SETEUID )
{
   if( HB_ISNUM( 1 ) )
      hb_posix_result( seteuid( hb_par_uid( 1 ) ) );
   else
      hb_posix_param_error();
}

HB_FUNC( POSIX_SETGID )
{
   if( HB_ISNUM( 1 ) )
      hb_posix_result( setgid( hb_par_uid( 1 ) ) );
   else
      hb_posix_param_error();
}

HB_FUNC( POSIX_SETEGID )
{
   if( HB_ISNUM( 1 ) )
      hb_posix_result( setegid( hb_par_uid( 1 ) ) );
   else
      hb_posix_param_error();
}
