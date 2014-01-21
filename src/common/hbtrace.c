/*
 * Harbour Project source code:
 * Tracing functions.
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
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

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbapi.h"
#include "hbapifs.h"
#include "hb_io.h"
#include "hbtrace.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#elif defined( HB_OS_UNIX ) && \
   ! defined( __WATCOMC__ ) && \
   ! defined( HB_OS_VXWORKS ) && \
   ! defined( HB_OS_SYMBIAN ) && \
   ! defined( HB_OS_QNX_BB10 )
   #include <syslog.h>
#endif

#ifndef va_copy
#  ifdef __va_copy
#     define va_copy( dst, src )  __va_copy( dst, src )
#  else
#     define va_copy( dst, src )  ( ( dst ) = ( src ) )
#  endif
#endif

static int s_enabled = 1;
static int s_level   = -1;
static int s_flush   = -1;
static int s_sysout  = -1;
static const char * s_mode = "w";

static FILE * s_fp = NULL;

static const char * s_slevel[ HB_TR_LAST ] =
{
   "HB_TR_ALWAYS",
   "HB_TR_FATAL",
   "HB_TR_ERROR",
   "HB_TR_WARNING",
   "HB_TR_INFO",
   "HB_TR_DEBUG"
};

int hb_tracestate( int new_state )
{
   int old_state = s_enabled;

   if( new_state == 0 ||
       new_state == 1 )
      s_enabled = new_state;

   return old_state;
}

int hb_tracelevel( int new_level )
{
   int old_level = hb_tr_level();

   if( new_level >= HB_TR_ALWAYS &&
       new_level <  HB_TR_LAST )
      s_level = new_level;

   return old_level;
}

const char * hb_tracemode( const char * szNewMode )
{
   const char * szPrevMode = s_mode;

   if( szNewMode )
   {
      switch( *szNewMode )
      {
         case 'a':
            s_mode = "a";
            break;
         case 'w':
            s_mode = "w";
            break;
      }
   }

   return szPrevMode;
}

HB_BOOL hb_tracefile( const char * szFile )
{
   if( szFile && *szFile )
   {
      FILE * fp = hb_fopen( szFile, s_mode );

      if( fp )
      {
         if( s_fp != NULL && s_fp != stderr )
            fclose( s_fp );
         s_fp = fp;
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

int hb_traceflush( int new_flush )
{
   int old_flush = HB_MAX( s_flush, 0 );

   if( new_flush == 0 ||
       new_flush == 1 )
      s_flush = new_flush;

   return old_flush;
}

int hb_tracesysout( int new_sysout )
{
   int old_sysout = HB_MAX( s_sysout, 0 );

   if( new_sysout == 0 ||
       new_sysout == 1 )
      s_sysout = new_sysout;

   return old_sysout;
}

int hb_tr_level( void )
{
   if( s_level == -1 )
   {
      char env[ HB_PATH_MAX ];
      int enabled = s_enabled;

      /* protection against recursive or concurrent calls */
      s_enabled = 0;

      s_level = HB_TR_DEFAULT;

      if( s_fp == NULL )
      {
         if( hb_getenv_buffer( "HB_TR_OUTPUT", env, sizeof( env ) ) &&
             env[ 0 ] != '\0' )
         {
            s_fp = hb_fopen( env, s_mode );

            if( s_fp == NULL )
               s_fp = stderr;
         }
         else
            s_fp = stderr;
      }

      if( hb_getenv_buffer( "HB_TR_LEVEL", env, sizeof( env ) ) &&
          env[ 0 ] != '\0' )
      {
         int i;

         for( i = 0; i < HB_TR_LAST; ++i )
         {
            if( hb_stricmp( env, s_slevel[ i ] ) == 0 ||
                hb_stricmp( env, s_slevel[ i ] + 6 ) == 0 )
            {
               s_level = i;
               break;
            }
         }
      }

      if( s_sysout < 0 )
      {
         s_sysout = ( hb_getenv_buffer( "HB_TR_SYSOUT", env, sizeof( env ) ) &&
                      env[ 0 ] != '\0' ) ? 1 : 0;
      }

      if( s_flush < 0 )
      {
         s_flush = ( hb_getenv_buffer( "HB_TR_FLUSH", env, sizeof( env ) ) &&
                     env[ 0 ] != '\0' ) ? 1 : 0;
      }

      s_enabled = enabled;
   }

   return s_level;
}

static void hb_tracelog_( int level, const char * file, int line, const char * proc,
                          const char * fmt, va_list ap )
{
   const char * pszLevel;

   /*
    * Clean up the file, so that instead of showing
    *
    *   ../../../foo/bar/baz.c
    *
    * we just show
    *
    *   foo/bar/baz.c
    */
   if( file )
   {
      while( *file == '.' || *file == '/' || *file == '\\' )
         file++;
   }
   else
      file = "";

   pszLevel = ( level >= HB_TR_ALWAYS && level <= HB_TR_LAST ) ?
              s_slevel[ level ] : "(\?\?\?)";

   if( s_sysout > 0 )
   {
#if ( defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE ) ) || \
      ( defined( HB_OS_UNIX ) && \
      ! defined( __WATCOMC__ ) && \
      ! defined( HB_OS_VXWORKS ) && \
      ! defined( HB_OS_SYMBIAN ) && \
      ! defined( HB_OS_QNX_BB10 ) )

      char message[ 1024 ];

      va_list vargs;
      va_copy( vargs, ap );

      /* NOTE: This is protection against recursive call to trace engine when
               there is more than 16 parameters in format string */
      if( hb_xtraced() && hb_printf_params( fmt ) > 16 )
         hb_snprintf( message, sizeof( message ), "more then 16 parameters in message '%s'", fmt );
      else
         hb_vsnprintf( message, sizeof( message ), fmt, vargs );

      va_end( vargs );

#  if defined( HB_OS_WIN )
      {
         union
         {
            char  psz[ 1024 ];
            TCHAR lp[ 1024 ];
         } buf;

         /* We add \n at the end of the buffer to make WinDbg display look readable. */
         if( proc )
            hb_snprintf( buf.psz, sizeof( buf.psz ), "%s:%d:%s() %s %s\n",
                         file, line, proc, pszLevel, message );
         else
            hb_snprintf( buf.psz, sizeof( buf.psz ), "%s:%d: %s %s\n",
                         file, line, pszLevel, message );

         #if defined( UNICODE )
         MultiByteToWideChar( CP_ACP, 0, ( LPCSTR ) memcpy( message, buf.psz, sizeof( message ) ), -1,
                              buf.lp, HB_SIZEOFARRAY( buf.lp ) );
         buf.lp[ HB_SIZEOFARRAY( buf.lp ) - 1 ] = 0;
         #endif
         OutputDebugString( buf.lp );
      }
#  else
      {
         int slevel;

         switch( level )
         {
            case HB_TR_ALWAYS:  slevel = LOG_ALERT;   break;
            case HB_TR_FATAL:   slevel = LOG_CRIT;    break;
            case HB_TR_ERROR:   slevel = LOG_ERR;     break;
            case HB_TR_WARNING: slevel = LOG_WARNING; break;
            case HB_TR_INFO:    slevel = LOG_INFO;    break;
            case HB_TR_DEBUG:   slevel = LOG_DEBUG;   break;
            default:            slevel = LOG_DEBUG;
         }

         if( proc )
            syslog( slevel, "%s:%d:%s() %s %s", file, line, proc,
                    pszLevel, message );
         else
            syslog( slevel, "%s:%d: %s %s", file, line, pszLevel, message );
      }
#  endif
#endif
   }

   /*
    * Print file and line.
    */
   if( proc )
      fprintf( s_fp, "%s:%d:%s(): %s ", file, line, proc, pszLevel );
   else
      fprintf( s_fp, "%s:%d: %s ", file, line, pszLevel );

   /*
    * Print the name and arguments for the function.
    */
   vfprintf( s_fp, fmt, ap );

   /*
    * Print a new-line.
    */
   fprintf( s_fp, "\n" );

   if( s_flush > 0 )
      fflush( s_fp );
}

void hb_tracelog( int level, const char * file, int line, const char * proc,
                  const char * fmt, ... )
{
   /*
    * If tracing is disabled, do nothing.
    */
   if( s_enabled && level <= hb_tr_level() )
   {
      va_list ap;
      va_start( ap, fmt );
      hb_tracelog_( level, file, line, proc, fmt, ap );
      va_end( ap );
   }
}

void hb_tr_trace( const char * fmt, ... )
{
   /*
    * If tracing is disabled, do nothing.
    */
   if( s_enabled )
   {
      PHB_TRACEINFO pTrace = hb_traceinfo();

      va_list ap;
      va_start( ap, fmt );
      hb_tracelog_( pTrace->level, pTrace->file, pTrace->line, pTrace->proc, fmt, ap );
      va_end( ap );

      /*
       * Reset file and line.
       */
      pTrace->level = -1;
      /* NOTE: resetting file name/line number will cause that we will unable
       * to report the location of code that allocated unreleased memory blocks
       * See hb_xalloc/hb_xgrab in src/vm/fm.c
       */
      if( hb_tr_level() < HB_TR_DEBUG )
      {
         pTrace->file = "";
         pTrace->line = -1;
      }
   }
}

void hb_tr_stealth( const char * fmt, ... )
{
   if( s_enabled )
   {
      PHB_TRACEINFO pTrace = hb_traceinfo();

      va_list ap;
      va_start( ap, fmt );
      hb_tracelog_( pTrace->level, pTrace->file, pTrace->line, pTrace->proc, fmt, ap );
      va_end( ap );
   }
}
