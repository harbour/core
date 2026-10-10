/*
 * posix_kill() wrapper, additional unix_signalToNum(), unix_numToSignal()
 * posix_signal() wrapper, plus unix_sigactionGet()
 *
 * Copyright 2026 Aleksander Czajczynski (hb fki.pl)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

typedef struct
{
   char szName[ 8 ];   /* longest is "STKFLT"/"VTALRM"/"UNUSED" + \0 */
   int  iSignal;
} HB_SIGNAME;

static const HB_SIGNAME s_sigTable[] =
{
#ifdef SIGABRT
   { "ABRT", SIGABRT },
#endif
#ifdef SIGALRM
   { "ALRM", SIGALRM },
#endif
#ifdef SIGBUS
   { "BUS", SIGBUS },
#endif
#ifdef SIGCHLD
   { "CHLD", SIGCHLD },
#endif
#ifdef SIGCLD
   { "CLD", SIGCLD },
#endif
#ifdef SIGCONT
   { "CONT", SIGCONT },
#endif
#ifdef SIGEMT
   { "EMT", SIGEMT },
#endif
#ifdef SIGFPE
   { "FPE", SIGFPE },
#endif
#ifdef SIGHUP
   { "HUP", SIGHUP },
#endif
#ifdef SIGILL
   { "ILL", SIGILL },
#endif
#ifdef SIGINFO
   { "INFO", SIGINFO },
#endif
#ifdef SIGINT
   { "INT", SIGINT },
#endif
#ifdef SIGIO
   { "IO", SIGIO },
#endif
#ifdef SIGIOT
   { "IOT", SIGIOT },
#endif
#ifdef SIGKILL
   { "KILL", SIGKILL },
#endif
#ifdef SIGLOST
   { "LOST", SIGLOST },
#endif
#ifdef SIGPIPE
   { "PIPE", SIGPIPE },
#endif
#ifdef SIGPOLL
   { "POLL", SIGPOLL },
#endif
#ifdef SIGPROF
   { "PROF", SIGPROF },
#endif
#ifdef SIGPWR
   { "PWR", SIGPWR },
#endif
#ifdef SIGQUIT
   { "QUIT", SIGQUIT },
#endif
#ifdef SIGSEGV
   { "SEGV", SIGSEGV },
#endif
#ifdef SIGSTKFLT
   { "STKFLT", SIGSTKFLT },
#endif
#ifdef SIGSTOP
   { "STOP", SIGSTOP },
#endif
#ifdef SIGTSTP
   { "TSTP", SIGTSTP },
#endif
#ifdef SIGSYS
   { "SYS", SIGSYS },
#endif
#ifdef SIGTERM
   { "TERM", SIGTERM },
#endif
#ifdef SIGTRAP
   { "TRAP", SIGTRAP },
#endif
#ifdef SIGTTIN
   { "TTIN", SIGTTIN },
#endif
#ifdef SIGTTOU
   { "TTOU", SIGTTOU },
#endif
#ifdef SIGUNUSED
   { "UNUSED", SIGUNUSED },
#endif
#ifdef SIGURG
   { "URG", SIGURG },
#endif
#ifdef SIGUSR1
   { "USR1", SIGUSR1 },
#endif
#ifdef SIGUSR2
   { "USR2", SIGUSR2 },
#endif
#ifdef SIGVTALRM
   { "VTALRM", SIGVTALRM },
#endif
#ifdef SIGXCPU
   { "XCPU", SIGXCPU },
#endif
#ifdef SIGXFSZ
   { "XFSZ", SIGXFSZ },
#endif
#ifdef SIGWINCH
   { "WINCH", SIGWINCH },
#endif
   { "NULL", 0 },
   { "", 0 }
};

/* Converts "SIGINT", "sigint" or "INT" to the signal number.
 * Returns -1 if unknown, 0 is valid for kill() - allowed when empty arg. */
static int hb_signalFromName( const char * pSig )
{
   if( pSig )
   {
      const HB_SIGNAME * pEntry;

      if( hb_strnicmp( pSig, "SIG", 3 ) == 0 )
         pSig += 3;

      for( pEntry = s_sigTable; pEntry->szName[ 0 ]; ++pEntry )
      {
         if( hb_stricmp( pSig, pEntry->szName ) == 0 )
            return pEntry->iSignal;
      }
      return -1;
   }

   return 0; /* NULL signal, permission check only or PID validation */
}

/* posix_kill( <nPid>[, <cSignal> = SIGNULL ] ) */
HB_FUNC( POSIX_KILL )
{
   if( HB_ISNUM( 1 ) )
      hb_posix_result( kill( ( pid_t ) hb_parni( 1 ), hb_signalFromName( hb_parc( 2 ) ) ) );
   else
      hb_posix_param_error();
}

/* posix_signal( <cSignal>, .T. (ignore) / .F. (default)
   -> <lWasIgnored> / NIL ( error or custom handler - use posix_errno() ) */
HB_FUNC( POSIX_SIGNAL )
{
   int iSignal = -1;

#if 0
   /* intentionally ignoring non-portable signal numbers */
   if( HB_ISNUM( 1 ) )
      iSignal = hb_parni( 1 );
#endif
   if( HB_ISCHAR( 1 ) )
      iSignal = hb_signalFromName( hb_parc( 1 ) );

   if( iSignal > 0 && HB_ISLOG( 2 ) )
   {
      void ( * pPrev )( int );
      HB_BOOL iAction = hb_parl( 2 );

      pPrev = signal( iSignal, iAction ? SIG_IGN : SIG_DFL );

      if( pPrev == SIG_ERR )
      {
         hb_posix_save_errno();
         hb_ret();
         return;
      }
      else if( pPrev == SIG_IGN )
         hb_retl( HB_TRUE );
      else if( pPrev == SIG_DFL )
         hb_retl( HB_FALSE );
      else
         hb_ret();
      hb_posix_set_errno( 0 );
   }
   else
      hb_posix_set_errno( ENOSYS );
}

/* unix_sigactionGet( <cSignal> )
 * -> <lIsIgnored> / NIL ( error or custom handler - use posix_errno() )
 *
 * not enabled for "Unix98", please retest with HP-UX, AIX, Solaris, etc.
 * implement minimum system revision checks if you need them */
HB_FUNC( UNIX_SIGACTIONGET )
{
#ifdef HB_OS_LINUX
   struct sigaction sa;
   int iSignal = hb_signalFromName( hb_parc( 1 ) );

   if( iSignal > 0 )
   {
      if( sigaction( iSignal, NULL, &sa ) == 0 )
      {
         if( sa.sa_handler == SIG_IGN )
            hb_retl( HB_TRUE );
         else if( sa.sa_handler == SIG_DFL )
            hb_retl( HB_FALSE );
         else
            hb_ret(); /* custom handler */
         hb_posix_set_errno( 0 );
      }
      else
         hb_posix_save_errno();
   }
   else
      hb_posix_set_errno( ENOSYS );
#else
   hb_posix_set_errno( ENOSYS );
   hb_ret();
#endif
}

/* unix_signalToNum( <cSignal> )
   -> <iSignal> */
HB_FUNC( UNIX_SIGNALTONUM )
{
   if( HB_ISCHAR( 1 ) )
      hb_retni( hb_signalFromName( hb_parc( 1 ) ) );
   else
      hb_retni( -1 );
}

/* signal number to name without "SIG" prefix, NULL if unknown */
static const char * hb_signalToName( int iSignal )
{
   const HB_SIGNAME * pEntry;

   for( pEntry = s_sigTable; pEntry->szName[ 0 ]; ++pEntry )
   {
      if( pEntry->iSignal == iSignal )
         return pEntry->szName;
   }

   return NULL;
}

/* unix_numToSignal( <iSignal>[, <lShort> = .F. ] )
   -> <cSignal> */
HB_FUNC( UNIX_NUMTOSIGNAL )
{
   const char * pName = HB_ISNUM( 1 ) ? hb_signalToName( hb_parni( 1 ) ) : NULL;

   if( pName )
   {
      /* short form unix_numToSignal( n, .T. ) */
      if( hb_parl( 2 ) )
         hb_retc( pName );
      else
      {
         char szSig[ 16 ];

         hb_snprintf( szSig, sizeof( szSig ), "SIG%s", pName );
         hb_retc( szSig );
      }
   }
   else
      hb_retc_null();
}
