/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * NETNAME() function
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *    Support for DJGPP/GCC/OS2 for netname
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

#include "hbapi.h"

#if defined(HB_OS_OS2) && defined(__GNUC__)

   #include "hb_io.h"

   /* 25/03/2004 - <maurilio.longo@libero.it>
      not needed anymore as of GCC 3.2.2 */

   #include <pwd.h>
   #include <sys/types.h>

   #if defined(__EMX__) && __GNUC__ * 1000 + __GNUC_MINOR__ < 3002
      #include <emx/syscalls.h>
      #define gethostname __gethostname
   #endif

   #define MAXGETHOSTNAME 256      /* should be enough for a host name */

#elif defined(HB_OS_DOS)

   #if defined(__DJGPP__) || defined(__RSX32__) || defined(__GNUC__)
      #include "hb_io.h"
      #include <sys/param.h>
   #endif

#elif defined(HB_OS_UNIX)

   #if !defined(__WATCOMC__)
      #include <pwd.h>
      #include <sys/types.h>
   #endif
   #include <unistd.h>
   #define MAXGETHOSTNAME 256      /* should be enough for a host name */

#endif

/* NOTE: Clipper will only return a maximum of 15 bytes from this function.
         And it will be padded with spaces. Harbour does the same in the
         DOS platform.
         [vszakats] */

HB_FUNC( NETNAME )
{
#if defined(HB_OS_UNIX) || ( defined(HB_OS_OS2) && defined(__GNUC__) )

   BOOL fGetUser = hb_parni( 1 ) == 1;
#  if defined(__WATCOMC__)
   char * pszValue = hb_getenv( fGetUser ? "USER" : "HOSTNAME" );
   hb_retc_buffer( pszValue );
#  else
   if( fGetUser )
   {
      struct passwd * pwd;
      pwd = getpwuid( getuid() );
      if( pwd )
         hb_retc( pwd->pw_name );
      else
         hb_retc_buffer( hb_getenv( "USER" ) );
   }
   else
   {
      char szValue[ MAXGETHOSTNAME + 1 ];
      szValue[ 0 ] = '\0';
      gethostname( szValue, MAXGETHOSTNAME );
      hb_retc( szValue );
   }
#  endif

#elif defined(HB_OS_DOS)

#  if defined(__DJGPP__) || defined(__RSX32__) || defined(__GNUC__)
      char szValue[ MAXGETHOSTNAME + 1 ];
      szValue[ 0 ] = '\0';

      gethostname( szValue, MAXGETHOSTNAME );

      hb_retc( szValue );
#  else
      union REGS regs;
      char szValue[ 16 ];
      szValue[ 0 ] = '\0';

      regs.HB_XREGS.ax = 0x5E00;

      {
         struct SREGS sregs;

         regs.HB_XREGS.dx = FP_OFF( szValue );
         sregs.ds = FP_SEG( szValue );

         HB_DOS_INT86X( 0x21, &regs, &regs, &sregs );
      }

      hb_retc( regs.h.ch == 0 ? "" : szValue );
#  endif

#elif defined(HB_OS_WIN_32)

   BOOL fGetUser = hb_parni( 1 ) == 1;
   DWORD ulLen = MAX_COMPUTERNAME_LENGTH + 1;
   char szValue[ MAX_COMPUTERNAME_LENGTH + 1 ];

   szValue[ 0 ] = '\0';
   if( fGetUser )
      GetUserName( szValue, &ulLen );
   else
      GetComputerName( szValue, &ulLen );
   hb_retc( szValue );

#else

   hb_retc( NULL );

#endif
}
