/*
 * $Id$
 */

#include <hbsetup.h>
#include <extend.h>
#include <errorapi.h>
#include <set.h>
#include <ctype.h>
#include <time.h>
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__)
  #include <dos.h>
#endif

#ifndef HB_STRICT_CLIPPER_COMPATIBILITY
   #define HB_OPTIMIZE_DTOS
#endif

extern STACK stack;

/* In msgxxx.c modules */
extern char *hb_monthsname[];
extern char *hb_daysname[];

HARBOUR HB_CDOW( void );
HARBOUR HB_CMONTH( void );
HARBOUR HB_CTOD( void );
HARBOUR HB_DATE( void );
HARBOUR HB_DAY( void );
HARBOUR HB_DOW( void );
HARBOUR HB_DTOC( void );
HARBOUR HB_DTOS( void );
HARBOUR HB_MONTH( void );
HARBOUR HB_SECONDS( void );
HARBOUR HB_STOD( void );
HARBOUR HB_TIME( void );
HARBOUR HB_YEAR( void );

static SYMBOL symbols[] = {
{ "HB_CDOW",    FS_PUBLIC, HB_CDOW,    0 },
{ "HB_CMONTH",  FS_PUBLIC, HB_CMONTH,  0 },
{ "HB_CTOD",    FS_PUBLIC, HB_CTOD,    0 },
{ "HB_DATE",    FS_PUBLIC, HB_DATE,    0 },
{ "HB_DAY",     FS_PUBLIC, HB_DAY,     0 },
{ "HB_DOW",     FS_PUBLIC, HB_DOW,     0 },
{ "HB_DTOC",    FS_PUBLIC, HB_DTOC,    0 },
{ "HB_DTOS",    FS_PUBLIC, HB_DTOS,    0 },
{ "HB_MONTH",   FS_PUBLIC, HB_MONTH,   0 },
{ "HB_SECONDS", FS_PUBLIC, HB_SECONDS, 0 },
{ "HB_STOD",    FS_PUBLIC, HB_STOD,    0 },
{ "HB_TIME",    FS_PUBLIC, HB_TIME,    0 },
{ "HB_YEAR",    FS_PUBLIC, HB_YEAR,    0 }
};

void Dates__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}

double hb__seconds( void )
{
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__)
   struct time t;
   gettime( &t );
   return( ( ( t.ti_hour * 3600 ) + ( t.ti_min * 60 ) + t.ti_sec ) + t.ti_hund / 100.0 );
#else
   time_t t;
   struct tm *oTime;

   time(&t);
   oTime = localtime(&t);

   return( ( oTime->tm_hour * 3600 ) + ( oTime->tm_min * 60 ) + oTime->tm_sec );
#endif
}

char *hb_cmonth( int month )
{
   if( month >= 1 && month <= 12 )
      return hb_monthsname[ month - 1 ];

   return "";
}

char *hb_cdow( int day )
{
   if( day >= 1 && day <= 7 )
      return hb_daysname[ day - 1 ];

   return "";
}

long hb_dateEncode( long lDay, long lMonth, long lYear )
{
   BOOL bValid = FALSE;
   long lFactor = ( lMonth < 3 ) ? -1: 0;

   /* Perform date validation */
   if (lMonth >= 1 && lMonth <= 12 && lDay >= 1
   && lYear >= 1 && lYear <= 2999)
   {
      /* Month, year, and lower day limits are simple,
         but upper day limit is dependent upon month and leap year */
      int aiDayLimit [12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
      if( (( lYear % 4 == 0 && lYear % 100 != 0 ) || lYear % 400 == 0 ) )
         aiDayLimit[ 1 ] = 29;
      if( lDay <= (long)aiDayLimit[ (int)lMonth - 1 ] ) bValid = TRUE;
   }
   if( bValid )
      return ( 1461 * ( lFactor + 4800 + lYear ) / 4 ) +
             ( ( lMonth - 2 - ( lFactor * 12 ) ) * 367 ) / 12 -
             ( 3 * ( ( lYear + 4900 + lFactor ) / 100 ) / 4 ) +
             lDay - 32075;
   else return ( 0 );
}

void hb_dateDecode( long julian, long * plDay, long * plMonth, long * plYear )
{
  long U, V, W, X;

   if( julian > 0 )
   {
      julian += 68569;
      W = ( julian * 4 ) / 146097;
      julian -= ( ( 146097 * W ) + 3 ) / 4;
      X = 4000 * ( julian + 1 ) / 1461001;
      julian -= ( ( 1461 * X ) / 4 ) - 31;
      V = 80 * julian / 2447;
      U = V / 11;
      if( plDay )   * plDay   = julian - ( 2447 * V / 80 );
      if( plMonth ) * plMonth = V + 2 - ( U * 12 );
      if( plYear )  * plYear  = X + U + ( W - 49 ) * 100;
   }
   else
   {
      * plDay = 0;
      * plMonth = 0;
      * plYear = 0;
   }
}

HARBOUR HB_CTOD( void )
{
   char * szDate = hb_parc( 1 );
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
         else if( digit != ' ' )
         {
            d_pos--;
            m_pos--;
            y_pos--;
         }
      }
      if (y_value > 0 && y_value < 100)
      {
         count = hb_set.HB_SET_EPOCH % 100;
         digit = hb_set.HB_SET_EPOCH / 100;
         if (y_value >= count) y_value += (digit * 100);
         else y_value += ((digit * 100) + 100);
      }
   }
   sprintf (szDateFormat, "%04i%02i%02i", y_value, m_value, d_value);
   hb_retds( szDateFormat );
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
      /* Not a valid date string, so return a blank date with separators */
      format_count = size; /* size is either 8 or 10 */
      strncpy( szDateFormat, hb_set.HB_SET_DATEFORMAT, size );
      for (digit_count = 0; digit_count < size; digit_count++)
         switch (szDateFormat [digit_count])
         {
            case 'D':
            case 'd':
            case 'M':
            case 'm':
            case 'Y':
            case 'y':
               szDateFormat [digit_count] = ' ';
         }
   }
   szDateFormat [format_count] = 0;
   return (szDateFormat);
}

HARBOUR HB_DTOC( void )
{
   char * szDate = hb_pards( 1 );
   char szDateFormat[ 11 ];
   hb_retc( hb_dtoc (szDate, szDateFormat) );
}

/* QUESTION: Should we drop error checkings to make it faster ? */
/*           This function may be called many times in a real world */
/*           application, for example in index creation. */
/*           Clipper does these checks, anyway. */
HARBOUR HB_DTOS( void )
{
#ifndef HB_OPTIMIZE_DTOS
   if( hb_pcount() == 1 )
   {
      if( ISDATE( 1 ) )
      {
#endif
         hb_retc( hb_pards( 1 ) );
#ifndef HB_OPTIMIZE_DTOS
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: DTOS");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: DTOS");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
#endif
}

HARBOUR HB_STOD( void )
{
   hb_retds((ISCHAR(1) && hb_parclen(1) == 8) ? hb_parc(1) : "        ");
}

HARBOUR HB_DAY( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
      hb_retni( lDay );
      stack.Return.wLength = 3;
   }
   else
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Error BASE/1114  Argument error: DAY");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

HARBOUR HB_MONTH( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
      hb_retni( lMonth );
      stack.Return.wLength = 3;
   }
   else
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Error BASE/1113  Argument error: MONTH");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

HARBOUR HB_YEAR( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
      hb_retni( lYear );
      stack.Return.wLength = 5;
   }
   else
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Error BASE/1112  Argument error: YEAR");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

HARBOUR HB_TIME( void )
{
   if( hb_pcount() == 0 )
   {
      time_t t;
      struct tm *oTime;
      char szTime[9];

      time(&t);
      oTime = localtime(&t);
      sprintf(szTime, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min,
oTime->tm_sec);
      hb_retclen(szTime, 8);
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: TIME");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

HARBOUR HB_DATE( void )
{
   if( hb_pcount() == 0 )
   {
      time_t t;
      struct tm *oTime;
      char szTime[9];

      time(&t);
      oTime = localtime(&t);
      sprintf(szTime, "%04d%02d%02d", oTime->tm_year + 1900, oTime->tm_mon + 1,
oTime->tm_mday);
      hb_retds(szTime);
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: DATE");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}
long hb_dow( long d, long m, long y )
{

   if( m < 3 )
      {
         m += 13;
         y--;
      }
   else
      m++;

   return ( ( d + 26 * m / 10 + y + y / 4 - y / 100 + y / 400 + 6 ) % 7 + 1 );
}

HARBOUR HB_DOW( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      if( pDate->value.lDate )
      {
         hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
         hb_retni( hb_dow( lDay, lMonth, lYear ) );
      }
      else
         hb_retni( 0 );
      stack.Return.wLength = 3;
   }
   else
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Error BASE/1115  Argument error: DOW");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

HARBOUR HB_CMONTH( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( hb_pcount() )
   {
     if( pDate )
     {
        hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
        hb_retc( hb_cmonth( lMonth ) );
     }
     else
     {
        PHB_ITEM pError = hb_errNew();
        hb_errPutDescription(pError, "Error BASE/1116  Argument error: CMONTH");
        hb_errLaunch(pError);
        hb_errRelease(pError);
     }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: CMONTH");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

HARBOUR HB_CDOW( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( hb_pcount() )
   {
     if( pDate )
     {
        hb_dateDecode( pDate->value.lDate, &lDay, &lMonth, &lYear );
        hb_retc( hb_cdow( hb_dow( lDay, lMonth, lYear ) ) );
     }
     else
     {
        PHB_ITEM pError = hb_errNew();
        hb_errPutDescription(pError, "Error BASE/1117  Argument error: CDOW");
        hb_errLaunch(pError);
        hb_errRelease(pError);
     }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: CDOW");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

HARBOUR HB_SECONDS( void )
{
   if( hb_pcount() == 0 )
      hb_retnd( hb__seconds() );
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: SECONDS");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}
