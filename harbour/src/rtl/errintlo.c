/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Error API (internal error)
 *
 * Copyright 1999-2004 Viktor Szakats (harbour syenar.net)
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
#include "hbapilng.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbstack.h"

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */

void hb_errInternalRaw( HB_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char buffer[ 8192 ];
   char file[ HB_PATH_MAX ];
   const char * szFile, * szInfo;
   HB_BOOL fStack, fLang;
   HB_USHORT uiLine;
   int iLevel;
   FILE * hLog;

   HB_TRACE( HB_TR_DEBUG, ( "hb_errInternal(%d, %s, %s, %s)", errCode, szText, szPar1, szPar2 ) );

   if( szPar1 == NULL )
      szPar1 = "";

   if( szPar2 == NULL )
      szPar2 = "";

   fStack = hb_stackId() != NULL;
   fLang = fStack && hb_langID() != NULL;

   szFile = fStack ? hb_setGetCPtr( HB_SET_HBOUTLOG ) : NULL;
   if( ! szFile )
      szFile = "hb_out.log";

   hLog = hb_fopen( szFile, "a+" );
   if( hLog )
   {
      char szTime[ 9 ];
      int  iYear, iMonth, iDay;

      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_dateTimeStr( szTime );

      fprintf( hLog, "Application Internal Error - %s\n", hb_cmdargARGVN( 0 ) );
      fprintf( hLog, "Terminated at: %04d.%02d.%02d %s\n", iYear, iMonth, iDay, szTime );
      szInfo = fStack ? hb_setGetCPtr( HB_SET_HBOUTLOGINFO ) : NULL;
      if( szInfo && *szInfo )
         fprintf( hLog, "Info: %s\n", szInfo );
   }

   hb_conOutErr( hb_conNewLine(), 0 );
   if( fLang )
      hb_snprintf( buffer, sizeof( buffer ), hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR ), errCode );
   else
      hb_snprintf( buffer, sizeof( buffer ), "Unrecoverable error %d: ", errCode );

   hb_conOutErr( buffer, 0 );
   if( hLog )
      fprintf( hLog, "%s", buffer );

   if( ! szText && fLang )
      szText = hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR + errCode - 9000 );

   if( szText )
      hb_snprintf( buffer, sizeof( buffer ), szText, szPar1, szPar2 );
   else
      buffer[ 0 ] = '\0';

   hb_conOutErr( buffer, 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
   if( hLog )
      fprintf( hLog, "%s\n", buffer );

   if( fStack && hb_stackTotalItems() )
   {
      iLevel = 0;
      while( hb_procinfo( iLevel++, buffer, &uiLine, file ) )
      {
         char msg[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

         hb_snprintf( msg, sizeof( msg ), "Called from %s(%hu)%s%s\n", buffer, uiLine, *file ? " in " : "", file );

         hb_conOutErr( msg, 0 );
         if( hLog )
            fprintf( hLog, "%s", msg );
      }
   }

   if( hLog )
   {
      fprintf( hLog, "------------------------------------------------------------------------\n" );
      fclose( hLog );
   }
}
