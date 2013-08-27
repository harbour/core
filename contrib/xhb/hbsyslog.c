/*
 * Harbour Project source code:
 * Event logging system
 *
 * Copyright 2004 Giancarlo Niccolai <gc -at- niccolai [dot] ws>
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
#include "hbwinuni.h"

#include "hblogdef.ch"

#if defined( HB_OS_WIN )

#  include "windows.h"

static HANDLE s_RegHandle;

#elif defined( HB_OS_UNIX ) && \
   ! defined( __WATCOMC__ ) && \
   ! defined( HB_OS_VXWORKS ) && \
   ! defined( HB_OS_SYMBIAN )

#  include <syslog.h>

#endif

HB_FUNC( HB_SYSLOGOPEN )
{
#if defined( HB_OS_WIN )

#  if ( WINVER >= 0x0400 ) && \
   ! ( defined( HB_OS_WIN_CE ) && defined( _MSC_VER ) && ( _MSC_VER <= 1310 ) )

   /* Ok, we compiled under NT, but we must not use this function
      when RUNNING on a win98. */
   if( hb_iswinnt() )
   {
      void * hSourceName;
      s_RegHandle = RegisterEventSource( NULL, HB_PARSTRDEF( 1, &hSourceName, NULL ) );
      hb_strfree( hSourceName );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
#  else
   s_RegHandle = NULL;
   hb_retl( HB_FALSE );
#  endif

#elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ ) && ! defined( HB_OS_VXWORKS )
   openlog( hb_parcx( 1 ), LOG_NDELAY | LOG_NOWAIT | LOG_PID, LOG_USER );
   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( HB_SYSLOGCLOSE )
{
#if defined( HB_OS_WIN )

#  if ( WINVER >= 0x0400 ) && \
   ! ( defined( HB_OS_WIN_CE ) && defined( _MSC_VER ) && ( _MSC_VER <= 1310 ) )

   if( hb_iswinnt() )
   {
      DeregisterEventSource( s_RegHandle );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
#  else
   hb_retl( HB_FALSE );
#  endif

#elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ ) && ! defined( HB_OS_VXWORKS )
   closelog();
   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( HB_SYSLOGMESSAGE )
{
#if defined( HB_OS_WIN )

#  if ( WINVER >= 0x0400 ) && \
   ! ( defined( HB_OS_WIN_CE ) && defined( _MSC_VER ) && ( _MSC_VER <= 1310 ) )
   if( hb_iswinnt() )
   {
      WORD    logval;
      void *  hMsg;
      LPCTSTR lpMsg = HB_PARSTRDEF( 1, &hMsg, NULL );
      switch( hb_parni( 2 ) )
      {
         case HB_LOG_CRITICAL: logval = EVENTLOG_ERROR_TYPE; break;
         case HB_LOG_ERROR: logval    = EVENTLOG_ERROR_TYPE; break;
         case HB_LOG_WARN: logval     = EVENTLOG_WARNING_TYPE; break;
         case HB_LOG_INFO: logval     = EVENTLOG_INFORMATION_TYPE; break;
         default: logval = EVENTLOG_AUDIT_SUCCESS;
      }
      hb_retl( ReportEvent( s_RegHandle,             /* event log handle */
                            logval,                  /* event type */
                            0,                       /* category zero */
                            ( DWORD ) hb_parnl( 3 ), /* event identifier */
                            NULL,                    /* no user security identifier */
                            1,                       /* one substitution string */
                            0,                       /* no data */
                            &lpMsg,                  /* pointer to string array */
                            NULL                     /* pointer to data */
                            ) ? HB_TRUE : HB_FALSE );

      hb_strfree( hMsg );
   }
   else
      hb_retl( HB_FALSE );
#  else
   hb_retl( HB_FALSE );
#  endif

#elif defined( HB_OS_UNIX ) && \
   ! defined( __WATCOMC__ ) && \
   ! defined( HB_OS_VXWORKS ) && \
   ! defined( HB_OS_SYMBIAN )

   int logval;

   switch( hb_parni( 2 ) )
   {
      case HB_LOG_CRITICAL: logval = LOG_CRIT; break;
      case HB_LOG_ERROR:    logval = LOG_ERR; break;
      case HB_LOG_WARN:     logval = LOG_WARNING; break;
      case HB_LOG_INFO:     logval = LOG_INFO; break;
      default:              logval = LOG_DEBUG;
   }

   syslog( logval, "[%lX]: %s", hb_parnl( 3 ), hb_parcx( 1 ) );
   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}
