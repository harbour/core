/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date API (C level)
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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
/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    hb_timeFormat()
 *    hb_timeUnformat()
 *    hb_timeStampFormat()
 *    hb_timeStampUnformat()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbdate.h"
#include "hbset.h"

char * hb_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat )
{
   /*
    * NOTE: szFormattedDate must point to a buffer of at least 11 bytes.
    *       szDateFormat must point to a buffer holding the date format to use.
    */
   int format_count, digit_count, size;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateFormat(%s, %p, %s)", szDate, szFormattedDate, szDateFormat ) );

   /*
    * Determine the maximum size of the formatted date string
    */
   size = ( int ) strlen( szDateFormat );
   if( size > 10 )
      size = 10;

   if( szDate && strlen( szDate ) == 8 ) /* A valid date is always 8 characters */
   {
      const char * szPtr;
      int digit;
      HB_BOOL used_d, used_m, used_y;

      format_count = 0;
      used_d = used_m = used_y = HB_FALSE;
      szPtr = szDateFormat;

      while( format_count < size )
      {
         digit = HB_TOUPPER( ( HB_UCHAR ) *szPtr );
         szPtr++;
         digit_count = 1;
         while( HB_TOUPPER( ( HB_UCHAR ) *szPtr ) == digit && format_count < size )
         {
            szPtr++;
            if( format_count + digit_count < size )
               digit_count++;
         }
         switch( digit )
         {
            case 'D':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_d && format_count < size )
                     {
                        /* szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
                  case 3:
                     if( ! used_d && format_count < size )
                     {
                        /* szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
                  case 2:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
                  default:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 7 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedDate[ format_count++ ] = ( char ) digit;
               }
               used_d = HB_TRUE;
               break;

            case 'M':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_m && format_count < size )
                     {
                        /* szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
                  case 3:
                     if( ! used_m && format_count < size )
                     {
                        /* szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
                  case 2:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
                  default:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 5 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedDate[ format_count++ ] = ( char ) digit;
               }
               used_m = HB_TRUE;
               break;

            case 'Y':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 0 ];
                        digit_count--;
                     }

                  case 3:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 1 ];
                        digit_count--;
                     }

                  case 2:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 2 ];
                        digit_count--;
                     }

                  default:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 3 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size )
                        szFormattedDate[ format_count++ ] = ( char ) digit;
               }
               used_y = HB_TRUE;
               break;

            default:
               while( digit_count-- > 0 && format_count < size )
                  szFormattedDate[ format_count++ ] = ( char ) digit;
         }
      }
   }
   else
   {
      /* Not a valid date string, so return a blank date with separators */
      format_count = size; /* size is either 8 or 10 */
      hb_strncpy( szFormattedDate, szDateFormat, size );

      for( digit_count = 0; digit_count < size; digit_count++ )
      {
         switch( szFormattedDate[ digit_count ] )
         {
            case 'D':
            case 'd':
            case 'M':
            case 'm':
            case 'Y':
            case 'y':
               szFormattedDate[ digit_count ] = ' ';
         }
      }
   }

   szFormattedDate[ format_count ] = '\0';

   return szFormattedDate;
}

static int hb_dateUnformatRaw( const char * szDate, const char * szDateFormat, long * plDate )
{
   int d_value = 0, m_value = 0, y_value = 0;
   int iSize = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateUnformatRaw(%s, %s, %p)", szDate, szDateFormat, plDate ) );

   if( szDate )
   {
      int d_pos = 0, m_pos = 0, y_pos = 0;
      int count, digit, non_digit, size, used;

      if( ! szDateFormat )
         szDateFormat = hb_setGetDateFormat();
      size = ( int ) strlen( szDateFormat );

      for( count = used = 0; count < size && used < 3; count++ )
      {
         switch( szDateFormat[ count ] )
         {
            case 'D':
            case 'd':
               if( d_pos == 0 )
               {
                  ++used;
                  if( m_pos == 0 && y_pos == 0 )
                     d_pos = 1;
                  else if( m_pos == 0 || y_pos == 0 )
                     d_pos = 2;
                  else
                     d_pos = 3;
               }
               break;
            case 'M':
            case 'm':
               if( m_pos == 0 )
               {
                  ++used;
                  if( d_pos == 0 && y_pos == 0 )
                     m_pos = 1;
                  else if( d_pos == 0 || y_pos == 0 )
                     m_pos = 2;
                  else
                     m_pos = 3;
               }
               break;
            case 'Y':
            case 'y':
               if( y_pos == 0 )
               {
                  ++used;
                  if( m_pos == 0 && d_pos == 0 )
                     y_pos = 1;
                  else if( m_pos == 0 || d_pos == 0 )
                     y_pos = 2;
                  else
                     y_pos = 3;
               }
         }
      }

      /* If there are non-digits at the start of the date field,
         they are not to be treated as date field separators */
      non_digit = 1;
      size = ( int ) strlen( szDate );
      for( count = used = 0; count < size; count++ )
      {
         digit = szDate[ count ];
         if( HB_ISDIGIT( digit ) )
         {
            /* Process the digit for the current date field */
            if( d_pos == 1 )
               d_value = ( d_value * 10 ) + digit - '0';
            else if( m_pos == 1 )
               m_value = ( m_value * 10 ) + digit - '0';
            else if( y_pos == 1 )
               y_value = ( y_value * 10 ) + digit - '0';
            /* Treat the next non-digit as a date field separator */
            non_digit = 0;
         }
         else
         {
            /* Process the non-digit */
            if( non_digit == 0 )
            {
               /* Only move to the next date field on the first
                  consecutive non-digit that is encountered */
               non_digit = 1;
               d_pos--;
               m_pos--;
               y_pos--;
               if( ++used >= 3 )
                  break;
            }
         }
      }
      iSize = count;

      if( y_value >= 0 && y_value < 100 )
      {
         count = hb_setGetEpoch();
         digit = count / 100;
         count %= 100;

         if( y_value >= count )
            y_value += ( digit * 100 );
         else
            y_value += ( ( digit * 100 ) + 100 );
      }
   }

   *plDate = hb_dateEncode( y_value, m_value, d_value );

   return iSize;
}

long hb_dateUnformat( const char * szDate, const char * szDateFormat )
{
   long lDate;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateFormat(%s, %s)", szDate, szDateFormat ) );

   hb_dateUnformatRaw( szDate, szDateFormat, &lDate );

   return lDate;
}


/* time modifiers:
 *    H - hour
 *    M - minutes
 *    S - seconds
 *    F - fractional part of seconds
 *    P - PM/AM marker
 * maximal size of time pattern:
 *    16 for "hh:mm:ss:ffff pp"
 * always safe buffer size is 17 (+1 for 0)
 */
char * hb_timeFormat( char * szBuffer, const char * szTimeFormat, long lMilliSec )
{
   char * szTimeBuffer;
   int iHour, iMinutes, iSeconds, iMSec, iPM, i12;
   int size, i, ch, count, value, digits, skip;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeFormat(%p, %s, %ld)", szBuffer, szTimeFormat, lMilliSec ) );

   hb_timeDecode( lMilliSec, &iHour, &iMinutes, &iSeconds, &iMSec );
   szTimeBuffer = szBuffer;

   size = ( int ) hb_strnlen( szTimeFormat, 16 );
   iPM = i12 = 0;
   for( i = 0; i < size; ++i )
   {
      if( HB_TOUPPER( szTimeFormat[ i ] ) == 'P' )
      {
         if( iHour >= 12 )
         {
            iPM = 1;
            iHour -= 12;
         }
         if( iHour == 0 )
            iHour += 12;
         if( iHour < 10 )
            i12 = 1;
         break;
      }
   }

   i = 0;
   while( i < size )
   {
      count = -i;
      ch = HB_TOUPPER( szTimeFormat[ i ] );
      ++i;
      while( ch == HB_TOUPPER( szTimeFormat[ i ] ) && i < size )
         ++i;
      count += i;
      switch( ch )
      {
         case 'H':
            value = iHour;
            if( count == 2 && value >= 0 )
            {
               if( i12 )
               {
                  *szTimeBuffer++ = ' ';
                  --count;
               }
               digits = count;
            }
            else
               digits = 1;
            iHour = -1;
            break;
         case 'M':
            value = iMinutes;
            iMinutes = -1;
            digits = count > 2 ? 1 : count;
            break;
         case 'S':
            value = iSeconds;
            iSeconds = -1;
            digits = count > 2 ? 1 : count;
            break;
         case 'F':
            value = iMSec;
            iMSec = -1;
            digits = count > 4 ? 1 : count;
            switch( digits )
            {
               case 4:
                  value *= 10;
                  break;
               case 2:
                  value = ( value + 5 ) / 10;
                  break;
               case 1:
                  value = ( value + 50 ) / 100;
                  break;
            }
            break;
         case 'P':
            if( iPM >= 0 )
            {
               *szTimeBuffer++ = iPM ? 'P' : 'A';
               if( --count )
               {
                  *szTimeBuffer++ = 'M';
                  --count;
               }
               iPM = -1;
            }
         default:
            digits = value = 0;
      }
      if( digits && value >= 0 )
      {
         skip = digits;
         count -= digits;
         do
         {
            szTimeBuffer[ --digits ] = ( char ) ( '0' + value % 10 );
            value /= 10;
         }
         while( digits );
         szTimeBuffer += skip;
      }
      while( count-- )
         *szTimeBuffer++ = ( char ) ch;
   }

   *szTimeBuffer = '\0';

   return szBuffer;
}

/*
 * maximal size of time pattern:
 *    16 for "hh:mm:ss:ffff pp"
 * total maximal size of formatted timestamp value: 10 + 16 = 26
 * always safe buffer size is: 27
 */
char * hb_timeStampFormat( char * szBuffer,
                           const char * szDateFormat, const char * szTimeFormat,
                           long lJulian, long lMilliSec )
{
   char szDate[ 9 ], * szTimeBuffer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampFormat(%p, %s, %s, %ld, %ld)", szBuffer, szDateFormat, szTimeFormat, lJulian, lMilliSec ) );

   hb_dateDecStr( szDate, lJulian );
   hb_dateFormat( szDate, szBuffer, szDateFormat );
   szTimeBuffer = szBuffer + strlen( szBuffer );
   if( *szBuffer )
      *szTimeBuffer++ = ' ';
   hb_timeFormat( szTimeBuffer, szTimeFormat, lMilliSec );

   return szBuffer;
}

long hb_timeUnformat( const char * szTime, const char * szTimeFormat )
{
   int iHour, iMinutes, iSeconds, iMSec, iPM;
   int size, i, count, prec, * pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeUnformat(%s, %s)", szTime, szTimeFormat ) );

   if( ! szTime )
      return 0;

   if( ! szTimeFormat )
      szTimeFormat = hb_setGetTimeFormat();

   size = ( int ) hb_strnlen( szTime, hb_strnlen( szTimeFormat, 16 ) );
   iHour = iMinutes = iSeconds = iMSec = iPM = -1;
   prec = 0;
   for( i = count = 0; i < size && szTime[ count ]; ++i )
   {
      switch( szTimeFormat[ i ] )
      {
         case 'H':
         case 'h':
            pValue = &iHour;
            break;
         case 'M':
         case 'm':
            pValue = &iMinutes;
            break;
         case 'S':
         case 's':
            pValue = &iSeconds;
            break;
         case 'F':
         case 'f':
            pValue = &iMSec;
            break;
         case 'P':
         case 'p':
            if( iPM < 0 )
            {
               while( szTime[ count ] && ! HB_ISDIGIT( szTime[ count ] ) &&
                      szTime[ count ] != 'P' && szTime[ count ] != 'p' &&
                      szTime[ count ] != 'A' && szTime[ count ] != 'a' )
                  ++count;
               if     ( szTime[ count ] == 'P' || szTime[ count ] == 'p' )
                  iPM = 1;
               else if( szTime[ count ] == 'A' || szTime[ count ] == 'a' )
                  iPM = 0;
            }
         default:
            pValue = NULL;
      }
      if( pValue && *pValue < 0 )
      {
         *pValue = 0;
         while( szTime[ count ] && ! HB_ISDIGIT( szTime[ count ] ) )
            ++count;
         while( HB_ISDIGIT( szTime[ count ] ) )
         {
            *pValue = *pValue * 10 + ( szTime[ count ] - '0' );
            ++count;
            if( pValue == &iMSec )
               ++prec;
         }
      }
   }
   if( iHour < 0 )
      iHour = 0;
   if( iMinutes < 0 )
      iMinutes = 0;
   if( iSeconds < 0 )
      iSeconds = 0;
   if( iMSec < 0 )
      iMSec = 0;
   else if( iMSec > 0 )
   {
      if( prec > 3 )
      {
         do
         {
            iMSec /= 10;
         }
         while( --prec > 3 );
      }
      else
      {
         while( prec++ < 3 )
            iMSec *= 10;
      }
   }
   if( iPM > 0 )
   {
      if( iHour == 0 )
         iHour = 24;    /* wrong time */
      else if( iHour != 12 )
         iHour += 12;
   }
   else if( iPM == 0 )
   {
      if( iHour == 0 )
         iHour = 24;    /* wrong time */
      else if( iHour == 12 )
         iHour = 0;
   }

   return hb_timeEncode( iHour, iMinutes, iSeconds, iMSec );
}

void hb_timeStampUnformat( const char * szDateTime,
                           const char * szDateFormat, const char * szTimeFormat,
                           long * plJulian, long * plMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampUnformat(%s, %s, %s, %p, %p)", szDateTime, szDateFormat, szTimeFormat, plJulian, plMilliSec ) );

   if( szDateTime )
   {
      int size;

      if( ! szDateFormat )
         szDateFormat = hb_setGetDateFormat();
      size = hb_dateUnformatRaw( szDateTime, szDateFormat, plJulian );
      *plMilliSec = hb_timeUnformat( szDateTime + size, szTimeFormat );
   }
   else
      *plJulian = *plMilliSec = 0;
}
