/*
 * $Id$
 */

#include <extend.h>
#include <set.h>
#include <ctype.h>
#include <time.h>

long greg2julian( long lDay, long lMonth, long lYear )
{
   long lFactor = ( lMonth < 3 ) ? -1: 0;

   return ( 1461 * ( lFactor + 4800 + lYear ) / 4 ) +
          ( ( lMonth - 2 - ( lFactor * 12 ) ) * 367 ) / 12 -
          ( 3 * ( ( lYear + 4900 + lFactor ) / 100 ) / 4 ) +
          lDay - 32075;
}

void julian2greg( long julian, long * plDay, long * plMonth, long * plYear )
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
   char * szDate = _parc( 1 );
   int d_value = 0, m_value = 0, y_value = 0;
   int d_pos = 0, m_pos = 0, y_pos = 0;
   int count, digit, size = strlen (hb_set.HB_SET_DATEFORMAT);
   char szDateFormat[ 9 ];

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
   sprintf (szDateFormat, "%04i%02i%02i", y_value, m_value, d_value);
   _retds( szDateFormat );
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
   size = min (10, strlen (hb_set.HB_SET_DATEFORMAT));

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
   szDateFormat [format_count] = 0;
   return (szDateFormat);
}

HARBOUR DTOC( void )
{
   char * szDate = _pards( 1 );
   char szDateFormat[ 11 ];
   _retc( hb_dtoc (szDate, szDateFormat) );
}

HARBOUR DTOS( void )
{
   _retc( _pards( 1 ) );
}

HARBOUR STOD( void )
{
   _retds( _parc( 1 ) );
}

HARBOUR DAY( void )
{
   PITEM pDate = _param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      julian2greg( pDate->value.lDate, &lDay, &lMonth, &lYear );
      _retni( lDay );
   }
   else
   {
      /* TODO: generate a proper error object and raise an error */
      printf( "not a valid date item from Day()\n" );
      exit( 1 );
   }
}

HARBOUR MONTH( void )
{
   PITEM pDate = _param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      julian2greg( pDate->value.lDate, &lDay, &lMonth, &lYear );
      _retni( lMonth );
   }
   else
   {
      /* TODO: generate a proper error object and raise an error */
      printf( "not a valid date item from Day()\n" );
      exit( 1 );
   }
}

HARBOUR YEAR( void )
{
   PITEM pDate = _param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      julian2greg( pDate->value.lDate, &lDay, &lMonth, &lYear );
      _retni( lYear );
   }
   else
   {
      /* TODO: generate a proper error object and raise an error */
      printf( "not a valid date item from Day()\n" );
      exit( 1 );
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

