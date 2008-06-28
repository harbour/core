/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Exception handlers
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbvm.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "hbset.h"

#if defined(HB_OS_WIN_32) && !defined(HB_WINCE)

LONG WINAPI hb_win32ExceptionHandler( struct _EXCEPTION_POINTERS * ExceptionInfo )
{
   char msg[ ( HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ) * 32 ];
   char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
   char * ptr;
   USHORT uiLine;
   int iLevel;

   FILE * hLog = hb_fopen( hb_setGetCPtr( HB_SET_HBOUTLOG ), "a+" );

   if( hLog )
   {
      char szTime[ 9 ];
      int iYear, iMonth, iDay;
      
      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_dateTimeStr( szTime );
         
      fprintf( hLog, HB_I_("Application Exception - %s\n"), hb_cmdargARGV()[0] );
      fprintf( hLog, HB_I_("Terminated at: %04d.%02d.%02d %s\n"), iYear, iMonth, iDay, szTime );
      if( *hb_setGetCPtr( HB_SET_HBOUTLOGINFO ) )
         fprintf( hLog, HB_I_("Info: %s\n"), hb_setGetCPtr( HB_SET_HBOUTLOGINFO ) );
   }

   HB_SYMBOL_UNUSED( ExceptionInfo );

   msg[ 0 ] = '\0';
   ptr = msg;
   iLevel = 0;

   while( hb_procinfo( iLevel++, buffer, &uiLine, NULL ) )
   {
      snprintf( ptr, sizeof( msg ) - ( ptr - msg ), 
                HB_I_("Called from %s(%hu)\n"), buffer, uiLine );

      if( hLog )
         fwrite( ptr, sizeof( *ptr ), strlen( ptr ), hLog );

      ptr += strlen( ptr );
   }

   /* GUI */
   {
      LPTSTR lpStr = HB_TCHAR_CONVTO( msg );
      MessageBox( NULL, lpStr, TEXT( HB_I_("Application Exception") ), MB_ICONSTOP );
      HB_TCHAR_FREE( lpStr );
   }

   if( hLog )
   {
      fprintf( hLog, "------------------------------------------------------------------------\n");
      fclose( hLog );
   }

   return EXCEPTION_CONTINUE_SEARCH; /* EXCEPTION_EXECUTE_HANDLER; */
}

#endif

#if defined(HB_OS_OS2)

static EXCEPTIONREGISTRATIONRECORD s_regRec;    /* Exception Registration Record */

ULONG _System hb_os2ExceptionHandler( PEXCEPTIONREPORTRECORD       p1,
                                      PEXCEPTIONREGISTRATIONRECORD p2,
                                      PCONTEXTRECORD               p3,
                                      PVOID                        pv )
{
   HB_SYMBOL_UNUSED( p1 );
   HB_SYMBOL_UNUSED( p2 );
   HB_SYMBOL_UNUSED( p3 );
   HB_SYMBOL_UNUSED( pv );

   /* Don't print stack trace if inside unwind, normal process termination or process killed or
      during debugging */
   if( p1->ExceptionNum != XCPT_UNWIND && p1->ExceptionNum < XCPT_BREAKPOINT )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
      USHORT uiLine;
      int iLevel = 0;

      fprintf( stderr, HB_I_("\nException %lx at address %p \n"), p1->ExceptionNum, p1->ExceptionAddress );

      while( hb_procinfo( iLevel++, buffer, &uiLine, NULL ) )
         fprintf( stderr, HB_I_("Called from %s(%hu)\n"), buffer, uiLine );
   }

   return XCPT_CONTINUE_SEARCH;          /* Exception not resolved... */
}

#endif

void hb_vmSetExceptionHandler( void )
{
#if defined(HB_OS_WIN_32) && !defined(HB_WINCE)
   {
      LPTOP_LEVEL_EXCEPTION_FILTER ef = SetUnhandledExceptionFilter( hb_win32ExceptionHandler );
      HB_SYMBOL_UNUSED( ef );
   }
#elif defined(HB_OS_OS2) /* Add OS2TermHandler to this thread's chain of exception handlers */
   {
      APIRET rc;                             /* Return code                   */

      memset( &s_regRec, 0, sizeof( s_regRec ) );
      s_regRec.ExceptionHandler = ( ERR ) hb_os2ExceptionHandler;
      rc = DosSetExceptionHandler( &s_regRec );
      if( rc != NO_ERROR )
         hb_errInternal( HB_EI_ERRUNRECOV, "Unable to setup exception handler (DosSetExceptionHandler())", NULL, NULL );
   }
#endif
}

void hb_vmUnsetExceptionHandler( void )
{
#if defined(HB_OS_OS2) /* Add OS2TermHandler to this thread's chain of exception handlers */
   {
      APIRET rc;                             /* Return code                   */

      /* I don't do any check on return code since harbour is exiting in any case */
      rc = DosUnsetExceptionHandler( &s_regRec );
      HB_SYMBOL_UNUSED( rc );
   }
#endif
}
