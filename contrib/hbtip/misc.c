/*
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * Copyright 1999-2017 Viktor Szakats (vszakats.net/harbour)
 *    (tip_TimeStamp() rework, cleanups)
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbdate.h"

/* Internet timestamp based on:
   https://tools.ietf.org/html/rfc822
   https://tools.ietf.org/html/rfc2822 */
HB_FUNC( TIP_TIMESTAMP )
{
   static const char * s_days[]   = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
   static const char * s_months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

   char szRet[ 64 ];
   int  iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
   long lOffset;

   /* FIXME: wrong result is returned when empty dates it's passed */

   if( HB_ISDATE( 1 ) )
   {
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );

      /* For compatibility, Seconds() value */
      if( HB_ISNUM( 2 ) )
         hb_timeDecode( ( long ) ( hb_parnd( 2 ) * 1000 ),
                        &iHour, &iMinute, &iSecond, &iMSec );
      else
         iHour = iMinute = iSecond = 0;
   }
   else if( HB_ISDATETIME( 1 ) )
      hb_timeStampUnpack( hb_partd( 1 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
   else
      hb_timeStampGetLocal( &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );

   lOffset = hb_timeStampUTCOffset( iYear, iMonth, iDay, iHour, iMinute, iSecond );

   hb_snprintf( szRet, sizeof( szRet ), "%s, %d %s %d %02d:%02d:%02d %+03d%02d",
                s_days[ hb_dateDOW( iYear, iMonth, iDay ) - 1 ],
                iDay, s_months[ iMonth - 1 ], iYear,
                iHour, iMinute, iSecond,
                ( int ) ( lOffset / 3600 ),
                ( int ) ( ( lOffset % 3600 ) / 60 ) );

   hb_retc( szRet );
}

HB_FUNC( TIP_HTMLSPECIALCHARS )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ nLen = hb_parclen( 1 );

      if( nLen )
      {
         const char * pszData = hb_parc( 1 );
         char *       pszRet;
         HB_ISIZ      nPos    = 0;
         HB_ISIZ      nPosRet = 0;

         while( nLen && HB_ISSPACE( pszData[ nLen - 1 ] ) )
            nLen--;

         /* Giving maximum final length possible */
         pszRet = ( char * ) hb_xgrab( nLen * 6 + 1 );

         while( nPos < nLen )
         {
            HB_BYTE cElem = ( HB_BYTE ) pszData[ nPos ];

            if( cElem == '&' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'a';
               pszRet[ nPosRet++ ] = 'm';
               pszRet[ nPosRet++ ] = 'p';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '<' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'l';
               pszRet[ nPosRet++ ] = 't';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '>' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'g';
               pszRet[ nPosRet++ ] = 't';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '"' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'q';
               pszRet[ nPosRet++ ] = 'u';
               pszRet[ nPosRet++ ] = 'o';
               pszRet[ nPosRet++ ] = 't';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\'' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = '#';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = '3';
               pszRet[ nPosRet++ ] = '9';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\r' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = '#';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = '1';
               pszRet[ nPosRet++ ] = '3';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\n' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = '#';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = '1';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem >= ' ' )
            {
               pszRet[ nPosRet++ ] = cElem;
            }

            nPos++;
         }

         hb_retclen_buffer( ( char * ) hb_xrealloc( pszRet, nPosRet + 1 ), nPosRet );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( TIP_CRLF )
{
   hb_retc_const( "\r\n" );
}

HB_FUNC( TIP_JSONSPECIALCHARS )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ nLen = hb_parclen( 1 );

      if( nLen )
      {
         const char * pszData = hb_parc( 1 );
         char *       pszRet;
         HB_ISIZ      nPos    = 0;
         HB_ISIZ      nPosRet = 0;

         while( nLen && HB_ISSPACE( pszData[ nLen - 1 ] ) )
            nLen--;

         /* Giving maximum final length possible */
         pszRet = ( char * ) hb_xgrab( nLen * 2 + 1 );

         while( nPos < nLen )
         {
            HB_BYTE cElem = ( HB_BYTE ) pszData[ nPos ];

            if( cElem == '"' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = '"';
            }
            else if( cElem == '\\' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = '\\';
            }
            else if( cElem == '/' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = '/';
            }
            else if( cElem == '\b' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'b';
            }
            else if( cElem == '\f' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'f';
            }
            else if( cElem == '\r' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'r';
            }
            else if( cElem == '\n' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'n';
            }
            else if( cElem == '\t' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 't';
            }
            else if( cElem >= ' ' )
            {
               pszRet[ nPosRet++ ] = cElem;
            }

            nPos++;
         }

         hb_retclen_buffer( ( char * ) hb_xrealloc( pszRet, nPosRet + 1 ), nPosRet );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
