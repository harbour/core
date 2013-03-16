/*
 * Harbour Project source code:
 * Debug Functions
 *
 * Copyright 2007-2008 Francesco Saverio Giudice <info / at /fsgiudice.com>
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
#include "hbapiitm.h"
#include "hbapifs.h"

#include "xhb.h"

static HB_BOOL s_bToOutputDebug = HB_TRUE;
static HB_BOOL s_bToLogFile     = HB_TRUE;

static HB_BOOL s_bEmptyLogFile = HB_TRUE;

HB_BOOL hb_ToOutDebugOnOff( HB_BOOL bOnOff )
{
   HB_BOOL bOld = s_bToOutputDebug;

   s_bToOutputDebug = bOnOff;
   return bOld;
}

void hb_ToOutDebug( const char * sTraceMsg, ... )
{
   if( sTraceMsg && s_bToOutputDebug )
   {
      char    buffer[ 1024 ];
      va_list ap;

      va_start( ap, sTraceMsg );
      hb_vsnprintf( buffer, sizeof( buffer ), sTraceMsg, ap );
      va_end( ap );

      hb_OutDebug( ( const char * ) buffer, strlen( buffer ) );
   }
}

HB_BOOL hb_ToLogFileOnOff( HB_BOOL bOnOff )
{
   HB_BOOL bOld = s_bToLogFile;

   s_bToLogFile = bOnOff;
   return bOld;
}

HB_BOOL hb_EmptyLogFile( HB_BOOL bOnOff )
{
   HB_BOOL bOld = s_bEmptyLogFile;

   s_bEmptyLogFile = bOnOff;
   return bOld;
}

void hb_ToLogFile( const char * sFile, const char * sTraceMsg, ... )
{
   if( s_bToLogFile )
   {
      FILE * hFile;

      if( sFile == NULL )
      {
         if( s_bEmptyLogFile )
         {
            s_bEmptyLogFile = HB_FALSE;

            /* Empty the file if it exists. */
            hFile = hb_fopen( "logfile.log", "w" );
         }
         else
            hFile = hb_fopen( "logfile.log", "a" );
      }
      else
         hFile = hb_fopen( sFile, "a" );

      if( hFile )
      {
         va_list ap;

         va_start( ap, sTraceMsg );
         vfprintf( hFile, sTraceMsg, ap );
         va_end( ap );

         fclose( hFile );
      }
   }
}
