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
   int count, digit;
   char szDateFormat[ 9 ];

   for( count = 0; count < strlen (HB_set._SET_DATEFORMAT); count++)
   {
      switch (HB_set._SET_DATEFORMAT [count])
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
   for( count = 0; count < strlen (szDate); count++)
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
      count = HB_set._SET_EPOCH % 100;
      digit = HB_set._SET_EPOCH / 100;
      if (y_value >= count) y_value += (digit * 100);
      else y_value += ((digit * 100) + 100);
   }
   sprintf (szDateFormat, "%04i%02i%02i", y_value, m_value, d_value);
   _retds( szDateFormat );
}

char * hb_dtoc (char * szDate, char * szDateFormat)
{
   /* NOTE: szDateFormat must point to a buffer of at least 11 bytes */
   int d_digits = 0, m_digits = 0, y_digits = 0;
   int d_pos = 0, m_pos = 0, y_pos = 0;
   int add_sep, count, digit, delim_1, delim_2, delim_count;
   char szTemp [5];

   delim_count = 0;
   delim_1 = delim_2 = '.';
   for( count = 0; count < strlen (HB_set._SET_DATEFORMAT); count++)
   {
      digit = HB_set._SET_DATEFORMAT [count];
      switch (digit)
      {
         case 'D':
         case 'd':
            d_digits++;
            if (d_pos == 0)
            {
               if (m_pos == 0 && y_pos == 0) d_pos = 1;
               else if (m_pos == 0 || y_pos == 0) d_pos = 2;
               else d_pos = 3;
            }
            break;
         case 'M':
         case 'm':
            m_digits++;
            if (m_pos == 0)
            {
               if (d_pos == 0 && y_pos == 0) m_pos = 1;
               else if (d_pos == 0 || y_pos == 0) m_pos = 2;
               else m_pos = 3;
            }
            break;
         case 'Y':
         case 'y':
            y_digits++;
            if (y_pos == 0)
            {
               if (m_pos == 0 && d_pos == 0) y_pos = 1;
               else if (m_pos == 0 || d_pos == 0) y_pos = 2;
               else y_pos = 3;
            }
            break;
         default:
            delim_count++;
            if (delim_count == 1) delim_1 = digit;
            else if (delim_count == 2) delim_2 = digit;
            break;
      }
   }
   *szDateFormat = 0;
   for (count = 0; count < 3; count++)
   {
      /* Insert a converted date element. */
      add_sep = 0;
      if (d_pos == 1 && d_digits > 0)
      {
         add_sep = 1;
         while (d_digits > 2)
         {
            strcat (szDateFormat, "0");
            d_digits--;
         }
         if (d_digits == 1)
         {
            szTemp [0] = szDate [7];
            szTemp [1] = 0;
         }
         else
         {
            szTemp [0] = szDate [6];
            szTemp [1] = szDate [7];
            szTemp [2] = 0;
         }
         strcat (szDateFormat, szTemp);
      }
      if (m_pos == 1 && m_digits > 0)
      {
         add_sep = 1;
         while (m_digits > 2)
         {
            strcat (szDateFormat, "0");
            m_digits--;
         }
         if (m_digits == 1)
         {
            szTemp [0] = szDate [5];
            szTemp [1] = 0;
         }
         else
         {
            szTemp [0] = szDate [4];
            szTemp [1] = szDate [5];
            szTemp [2] = 0;
         }
         strcat (szDateFormat, szTemp);
      }
      if (y_pos == 1 && y_digits > 0)
      {
         add_sep = 1;
         while (y_digits > 4)
         {
            strcat (szDateFormat, "0");
            y_digits--;
         }
         if (y_digits == 1)
         {
            szTemp [0] = szDate [3];
            szTemp [1] = 0;
         }
         else if (y_digits == 2)
         {
            szTemp [0] = szDate [2];
            szTemp [1] = szDate [3];
            szTemp [2] = 0;
         }
         else if (y_digits == 3)
         {
            szTemp [0] = szDate [1];
            szTemp [1] = szDate [2];
            szTemp [2] = szDate [3];
            szTemp [3] = 0;
         }
         else
         {
            szTemp [0] = szDate [0];
            szTemp [1] = szDate [1];
            szTemp [2] = szDate [2];
            szTemp [3] = szDate [3];
            szTemp [4] = 0;
         }
         strcat (szDateFormat, szTemp);
      }
      /* Insert a date field separator. */
      if (add_sep && delim_1)
      {
         szTemp [0] = delim_1;
         szTemp [1] = 0;
         strcat (szDateFormat, szTemp);
         delim_1 = 0;
      }
      else if (add_sep && delim_2)
      {
         szTemp [0] = delim_2;
         szTemp [1] = 0;
         strcat (szDateFormat, szTemp);
         delim_2 = 0;
      }
      /* Get ready for the next date element. */
      d_pos--;
      m_pos--;
      y_pos--;
   }
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

