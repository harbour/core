/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Std applications entry point
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * along with this software; see the file COPYING.  If not, write to
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
#include "hbvm.h"

#if ! ( defined( HB_DYNLIB ) && defined( __WATCOMC__ ) )

HB_EXTERN_BEGIN
#if defined( __MINGW32__ )
int _CRT_glob = 0;
#elif defined( __DJGPP__ )

#include <crt0.h>

int _crt0_startup_flags = _CRT0_FLAG_USE_DOS_SLASHES;

char ** __crt0_glob_function( char * _arg )
{
   /* This function disables command line wildcard expansion. */
   HB_SYMBOL_UNUSED( _arg );

   return 0;
}
#endif

#if defined( __WATCOMC__ ) && ( defined( HB_OS_LINUX ) || defined( HB_OS_OS2 ) || defined( HB_OS_WIN ) )
void hb_forceLinkMainStd( void ) {}
#endif

HB_EXTERN_END

int main( int argc, char * argv[] )
{
   HB_TRACE( HB_TR_DEBUG, ( "main(%d, %p)", argc, argv ) );

#if defined( __DJGPP__ )
   __system_flags =
      __system_redirect |
      __system_allow_long_cmds |
      __system_emulate_command |
      __system_handle_null_commands |
      __system_emulate_chdir;
#endif

   hb_cmdargInit( argc, argv );
   hb_vmInit( HB_TRUE );
   return hb_vmQuit();
}

#endif
