/*
* $Id$
*/

/*
* xHarbour Project source code:
* The Internet Protocol / TCP support
*
* Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
* www - http://www.xharbour.org
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
* As a special exception, xHarbour license gives permission for
* additional uses of the text contained in its release of xHarbour.
*
* The exception is that, if you link the xHarbour libraries with other
* files to produce an executable, this does not by itself cause the
* resulting executable to be covered by the GNU General Public License.
* Your use of that executable is in no way restricted on account of
* linking the xHarbour library code into it.
*
* This exception does not however invalidate any other reasons why
* the executable file might be covered by the GNU General Public License.
*
* This exception applies only to the code released with this xHarbour
* explicit exception.  If you add/copy code from other sources,
* as the General Public License permits, the above exception does
* not apply to the code that you add in this way.  To avoid misleading
* anyone as to the status of such modified files, you must delete
* this exception notice from them.
*
* If you write modifications of your own for xHarbour, it is your choice
* whether to permit this exception to apply to your modifications.
* If you do not wish that, delete this exception notice.
*
*/

#ifndef HB_INET_H_
#define HB_INET_H_

#include "hbdefs.h"
#include "hbvm.h"
#include "hbapierr.h"

#define  HB_SOCKET_SIGN   0xF0123A42

#if defined( HB_OS_DOS )
    #define HB_NO_DEFAULT_INET
#else
   #if defined( HB_OS_WIN_32 )
      #define _WINSOCKAPI_  /* Prevents inclusion of Winsock.h in Windows.h */
      #define HB_SOCKET_T SOCKET
      #include <winsock2.h>
      #include <windows.h>

      #define HB_INET_CLOSE( x )    closesocket( x )

      extern char *hstrerror( int i );
   #else

      #if defined( HB_OS_HPUX )
         #define _XOPEN_SOURCE_EXTENDED
      #endif

      #define HB_SOCKET_T int
      #include <unistd.h>
      #include <sys/types.h>
      #include <sys/socket.h>
      #include <netdb.h>
      #include <netinet/in.h>
      #include <arpa/inet.h>

      extern int h_errno;
      #define HB_INET_CLOSE( x )    close( x )
      #ifdef HB_OS_DARWIN
         #if !defined( socklen_t ) && !defined( _SOCKLEN_T )
            #define socklen_t int
         #endif
      #endif
      #include <errno.h>
   #endif

   typedef struct tag_HB_SOCKET_STRUCT
   {
       ULONG sign;
       HB_SOCKET_T com;
       char *errorDesc;
       int errorCode;
       struct sockaddr_in remote;
       LONG count;
       int timeout;
       int timelimit;
       PHB_ITEM caPeriodic;
       int iSndBufSize;
       int iRcvBufSize;
   } HB_SOCKET_STRUCT;

   #define HB_PARSOCKET( n )     ( ( HB_SOCKET_STRUCT * ) hb_parptrGC( hb_inetSocketFinalize, n ) )

   #define HB_SOCKET_ZERO_ERROR( s )  s->errorCode = 0; s->errorDesc = ""

   #if defined( HB_OS_WIN_32 )
       #define HB_SOCKET_SET_ERROR( s ) \
           s->errorCode = WSAGetLastError(); \
           s->errorDesc = strerror( s->errorCode );\
           WSASetLastError( 0 );

   #else
       #define HB_SOCKET_SET_ERROR( s ) s->errorCode = errno; s->errorDesc = strerror( errno )
   #endif

   #define HB_SOCKET_SET_ERROR1( s, code ) s->errorCode = code; s->errorDesc = strerror( code );
   #define HB_SOCKET_SET_ERROR2( s, code, desc ) s->errorCode = code; s->errorDesc = desc;

   #define HB_SOCKET_INIT( s, p ) \
   {\
      s = ( HB_SOCKET_STRUCT *) hb_gcAlloc( sizeof( HB_SOCKET_STRUCT ), hb_inetSocketFinalize );\
      p = hb_itemPutPtrGC( p, s );\
      HB_SOCKET_ZERO_ERROR( s );\
      s->sign = HB_SOCKET_SIGN;\
      s->com = 0;\
      s->count = 0;\
      s->timeout = -1;\
      s->timelimit = -1;\
      s->caPeriodic = NULL;\
   }

   #ifndef MSG_NOSIGNAL
       #define MSG_NOSIGNAL  0
   #endif

   #ifndef MSG_DONTWAIT
       /* #define MSG_DONTWAIT 0x80 */
       #define MSG_DONTWAIT    0
   #endif

   #ifndef MSG_WAITALL
       #define MSG_WAITALL 0
   #endif
#endif

#endif
