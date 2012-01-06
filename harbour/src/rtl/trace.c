/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Clipper tracing API.
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * Copyright 1999 Gonzalo A. Diethelm <gonzalo.diethelm@iname.com>
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
#include "hbapiitm.h"
#include "hbtrace.h"

static int s_traceLogLevel = HB_TR_DEFAULT;

static void hb_trace_message( char * buffer, HB_SIZE nSize, int iParam, int iCount )
{
   int iFirst = iParam;

   buffer[ 0 ] = '\0';

   while( iParam <= iCount && nSize > 1 )
   {
      char * pszString;
      HB_SIZE nLen;
      HB_BOOL fFree;

      if( iParam > iFirst )
      {
         *buffer++ = ' ';
         --nSize;
      }
      pszString = hb_itemString( hb_param( iParam, HB_IT_ANY ), &nLen, &fFree );
      hb_strncpy( buffer, pszString, nSize );
      nLen = strlen( buffer );
      nSize -= nLen;
      buffer += nLen;
      if( fFree )
         hb_xfree( pszString );
      iParam++;
   }
}

HB_FUNC( HB_TRACESTATE )
{
   hb_retl( hb_tracestate( HB_ISLOG( 1 ) ? hb_parl( 1 ) :
                                           hb_parnidef( 1, -1 ) ) );
}

HB_FUNC( HB_TRACESYSOUT )
{
   hb_retl( hb_tracesysout( HB_ISLOG( 1 ) ? hb_parl( 1 ) :
                                            hb_parnidef( 1, -1 ) ) );
}

HB_FUNC( HB_TRACEFLUSH )
{
   hb_retl( hb_traceflush( HB_ISLOG( 1 ) ? hb_parl( 1 ) :
                                           hb_parnidef( 1, -1 ) ) );
}

HB_FUNC( HB_TRACEMODE )
{
   hb_retc( hb_tracemode( hb_parc( 1 ) ) );
}

HB_FUNC( HB_TRACEFILE )
{
   hb_retl( hb_tracefile( hb_parc( 1 ) ) );
}

HB_FUNC( HB_TRACELEVEL )
{
   hb_retni( hb_tracelevel( hb_parnidef( 1, -1 ) ) );
}

HB_FUNC( HB_TRACELOGLEVEL )
{
   int iOldLevel = s_traceLogLevel, iLevel;

   if( HB_ISNUM( 1 ) )
   {
      iLevel = hb_parni( 1 );
      if( iLevel >= HB_TR_ALWAYS && iLevel < HB_TR_LAST )
         s_traceLogLevel = iLevel;
   }
   hb_retni( iOldLevel );
}

HB_FUNC( HB_TRACELOG )
{
   char message[ 1024 ];
   char procname[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
   char file[ HB_PATH_MAX ];
   HB_USHORT line;

   hb_trace_message( message, sizeof( message ) - 1, 1, hb_pcount() );
   hb_procinfo( 1, procname, &line, file );
   hb_tracelog( s_traceLogLevel, file, line, procname, "%s", message );
}

HB_FUNC( HB_TRACELOGAT )
{
   if( HB_ISNUM( 1 ) )
   {
      int iLevel = hb_parni( 1 );

      if( iLevel <= hb_tr_level() )
      {
         char message[ 1024 ];
         char procname[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
         char file[ HB_PATH_MAX ];
         HB_USHORT line;

         hb_trace_message( message, sizeof( message ) - 1, 2, hb_pcount() );
         hb_procinfo( 1, procname, &line, file );
         hb_tracelog( iLevel, file, line, procname, "%s", message );
      }
   }
}

HB_FUNC( HB_TRACESTRING )
{
   int iPCount = hb_pcount();

   if( iPCount > 0 )
   {
      char message[ 1024 ];

      hb_trace_message( message, sizeof( message ) - 1, 1, iPCount );

      HB_TRACE(HB_TR_ALWAYS, ("%s", message) );
   }
}
