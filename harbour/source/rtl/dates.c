/*
 * $Id$
 */

#include <extend.h>
#include <set.h>
#include <ctype.h>
#include <time.h>

#ifndef _STRICT_CLIPPER_COMPATIBILITY
   #define _OPTIMIZE_DTOS
#endif

long hb_dateEncode( long lDay, long lMonth, long lYear )
{
   long lFactor = ( lMonth < 3 ) ? -1: 0;

   return ( 1461 * ( lFactor + 4800 + lYear ) / 4 ) +
          ( ( lMonth - 2 - ( lFactor * 12 ) ) * 367 ) / 12 -
          ( 3 * ( ( lYear + 4900 + lFactor ) / 100 ) / 4 ) +
          lDay - 32075;
}

void hb_dateDecode( long julian, long * plDay, long * plMonth, long * plYear )
{
  long U, V, W, X;

  julian += 68569;
  W = ( julian * 4 ) / 146097;
  julian -= ( ( 146097 * W ) + 3 ) / 4;
  X = 4000 * ( julian + 1 ) / 1461001;
  julian -= ( ( 1461 * X ) / 4 ) - 31;
  V = 80 * julian / 2447;
  U = V / 11;
  * plDay   = julian - ( 2447 * V / 80 );
  * plMonth = V + 2 - ( U * 12 );
  * plYear  = X + U + ( W - 49 ) * 100;
}

HARBOUR CTOD( void )
{
   BOOL bValid = FALSE;
   char * szDate = _parc( 1 );
   int d_value = 0, m_value = 0, y_value = 0;
   int d_pos = 0, m_pos = 0, y_pos = 0;
   int count, digit, size = strlen (hb_set.HB_SET_DATEFORMAT);
   char szDateFormat[ 9 ];

   if( szDate )
   {
      for( count = 0; count < size; count++)
      {
         switch (hb_set.HB_SET_DATEFORMAT [count])
         {
            case 'D':
            case 'd':
               if (d_pos == 0)
               {
                  if (m_pos == 0 && y_pos == 0) d_pos = 1;
                  else if (m_pos == 0 || y_pos == 0) d_pos = 2;
                  else d_pos = 3;
               }
               break;
            case 'M':
            case 'm':
               if (m_pos == 0)
               {
                  if (d_pos == 0 && y_pos == 0) m_pos = 1;
                  else if (d_pos == 0 || y_pos == 0) m_pos = 2;
                  else m_pos = 3;
               }
               break;
            case 'Y':
            case 'y':
               if (y_pos == 0)
               {
                  if (m_pos == 0 && d_pos == 0) y_pos = 1;
                  else if (m_pos == 0 || d_pos == 0) y_pos = 2;
                  else y_pos = 3;
               }
         }
      }
      size = strlen (szDate);
      for( count = 0; count < size; count++)
      {
         digit = szDate [count];
         if (isdigit (digit))
         {
            if (d_pos == 1)
            {
               d_value = (d_value * 10) + digit - '0';
            }
            else if (m_pos == 1)
            {
               m_value = (m_value * 10) + digit - '0';
            }
            else if (y_pos == 1)
            {
               y_value = (y_value * 10) + digit - '0';
            }
         }
         else
         {
            d_pos--;
            m_pos--;
            y_pos--;
         }
      }
      if (y_value < 100)
      {
         count = hb_set.HB_SET_EPOCH % 100;
         digit = hb_set.HB_SET_EPOCH / 100;
         if (y_value >= count) y_value += (digit * 100);
         else y_value += ((digit * 100) + 100);
      }
   }
   /* Perform date validation before converting to date */
   if (szDate && m_value >= 1 && m_value <= 12 && d_value >= 1
   && y_value >= 0 & y_value <= 2999)
   {
      /* Month, year, and lower day limits are easy, 
         but upper day limit is dependent upon month and leap year */
      BOOL bLeapYear = FALSE;
      int d_limit [12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
      if( y_value % 4 == 0 )
      {
         /* Check for leap year (every year that is evenly divisible by 4,
            except for centuries, which must be evenly divisible by 400 */
         if( y_value % 100 == 0 )
         {
            if( y_value % 400 == 0) bLeapYear = TRUE; /* Leap century */
         }
         else bLeapYear = TRUE; /* Leap year */
         
         if( bLeapYear ) d_limit[ 1 ] = 29;
      }
      if( d_value <= d_limit[ m_value - 1 ] ) bValid = TRUE;
   }
   if( bValid )
   {
      sprintf (szDateFormat, "%04i%02i%02i", y_value, m_value, d_value);
      _retds( szDateFormat );
   }
   else _retds( "" );
}

char * hb_dtoc (char * szDate, char * szDateFormat)
{
   /*
    * NOTE: szDateFormat must point to a buffer of at least 11 bytes
     */
   int digit, digit_count, format_count, size;
   BOOL used_d, used_m, used_y;
   char *szPtr;

   /*
     * Determine the maximum size of the formatted date string
     */
   size = strlen (hb_set.HB_SET_DATEFORMAT);
   if (size > 10) size = 10;

   if( szDate && szDateFormat && strlen( szDate ) == 8 ) /* A valid date is always 8 characters */
   {
      format_count = 0;
      used_d = used_m = used_y = FALSE;
      szPtr = hb_set.HB_SET_DATEFORMAT;
      while (format_count < size)
      {
         digit = toupper (*szPtr);
         szPtr++;
         digit_count = 1;
         while (toupper (*szPtr) == digit && format_count < size)
         {
            szPtr++;
            if (format_count + digit_count < size) digit_count++;
         }
         switch (digit)
         {
            case 'D':
               switch (digit_count)
               {
                  case 4:
                     if (!used_d && format_count < size)
                     {
                        szDateFormat [format_count++] = '0';
                        digit_count--;
                     }
                  case 3:
                     if (!used_d && format_count < size)
                     {
                        szDateFormat [format_count++] = '0';
                        digit_count--;
                     }
                  case 2:
                     if (!used_d && format_count < size)
                     {
                        szDateFormat [format_count++] = szDate [6];
                        digit_count--;
                     }
                  default:
                     if (!used_d && format_count < size) 
                     {
                        szDateFormat [format_count++] = szDate [7];
                        digit_count--;
                     }
                     while (digit_count-- > 0 && format_count < size) szDateFormat [format_count++] = digit;
               }
               used_d = TRUE;
               break;
            case 'M':
               switch (digit_count)
               {
                  case 4:
                     if (!used_m && format_count < size) 
                     {
                        szDateFormat [format_count++] = '0';
                        digit_count--;
                     }
                  case 3:
                     if (!used_m && format_count < size) 
                     {
                        szDateFormat [format_count++] = '0';
                        digit_count--;
                     }
                  case 2:
                     if (!used_m && format_count < size) 
                     {
                        szDateFormat [format_count++] = szDate [4];
                        digit_count--;
                     }
                  default:
                     if (!used_m && format_count < size) 
                     {
                        szDateFormat [format_count++] = szDate [5];
                        digit_count--;
                     }
                     while (digit_count-- > 0 && format_count < size) szDateFormat [format_count++] = digit;
               }
               used_m = TRUE;
               break;
            case 'Y':
               switch (digit_count)
               {
                  case 4:
                     if (!used_y && format_count < size) 
                     {
                        szDateFormat [format_count++] = szDate [0];
                        digit_count--;
                     }
                  case 3:
                     if (!used_y && format_count < size) 
                     {
                        szDateFormat [format_count++] = szDate [1];
                        digit_count--;
                     }
                  case 2:
                     if (!used_y && format_count < size) 
                     {
                        szDateFormat [format_count++] = szDate [2];
                        digit_count--;
                     }
                  default:
                     if (!used_y && format_count < size) 
                     {
                        szDateFormat [format_count++] = szDate [3];
                        digit_count--;
                     }
                     while (digit_count-- > 0 && format_count < size) szDateFormat [format_count++] = digit;
               }
               used_y = TRUE;
               break;
            default:
               while (digit_count-- > 0 && format_count < size) szDateFormat [format_count++] = digit;
         }
      }
   }
   else 
   {
      /* Not a valid date string, so return a blank date */
      format_count = size;
      strncpy( szDateFormat, "          ", format_count );
   }
   szDateFormat [format_count] = 0;
   return (szDateFormat);
}

HARBOUR DTOC( void )
{
   char * szDate = _pards( 1 );
   char szDateFormat[ 11 ];
   _retc( hb_dtoc (szDate, szDateFormat) );
}

/* QUESTION: Should we drop error checkings to make it faster ? */
/*           This function may be called many times in a real world */
/*           application, for example in index creation. */
/*           Clipper does these checks, anyway. */
HARBOUR DTOS( void )
{
#ifndef _OPTIMIZE_DTOS
   if( _pcount() == 1 )
   {
      if( ISDATE( 1 ) )
      {
#endif
         _retc( _pards( 1 ) );
#ifndef _OPTIMIZE_DTOS
      }
      else
      {
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: DTOS");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: DTOS");
      _errLaunch(pError);
      _errRelease(pError);
   }
#endif
}

HARBOUR STOD( void )
{
   _retds((ISCHAR(1) && _parclen(1) == 8) ? _parc(1) : "        ");
}

HARBOUR DAY( void )
{
   PITEM pDate = _param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
      _retni( lDay );
   }
   else
   {
      PITEM pError = _errNew();
      _errPutDescription(pError, "Error BASE/1114  Argument error: DAY");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR MONTH( void )
{
   PITEM pDate = _param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
      _retni( lMonth );
   }
   else
   {
      PITEM pError = _errNew();
      _errPutDescription(pError, "Error BASE/1113  Argument error: MONTH");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR YEAR( void )
{
   PITEM pDate = _param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
      _retni( lYear );
   }
   else
   {
      PITEM pError = _errNew();
      _errPutDescription(pError, "Error BASE/1112  Argument error: YEAR");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR TIME( void )
{
   if( _pcount() == 0 )
   {
      time_t t;
      struct tm *oTime;
      char szTime[9];

      time(&t);
      oTime = localtime(&t);
      sprintf(szTime, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min,
oTime->tm_sec);
      _retclen(szTime, 8);
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: TIME");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

