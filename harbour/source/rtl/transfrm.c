/*
 * $Id$
 */

#include <extend.h>
#include <ctoharb.h>
#include <ctype.h>

/*                                                                          */
/*  Transform( xValue, cPicture )                                           */
/*                                                                          */
/*  Date : 29/04/1999                                                       */
/*                                                                          */

extern STACK stack;

void StackPop( void );                          /* TOFIX: Should go away    */

/* Function flags                                                           */

#define PF_LEFT    0x0001   /* @B */
#define PF_CREDIT  0x0002   /* @C */
#define PF_DEBIT   0x0004   /* @D */
#define PF_ZERO    0x0008   /* @0 */
#define PF_PARNEG  0x0010   /* @( */
#define PF_REMAIN  0x0020   /* @R */
#define PF_UPPER   0x0040   /* @! */
#define PF_DATE    0x0080   /* @D */
#define PF_BRITISH 0x0100   /* @E */
#define PF_EXCHANG 0x0100   /* @E. Also means exchange . and , */
#define PF_EMPTY   0x0200   /* @Z */
#define PF_NUMDATE 0x0400   /* Internal flag. Ignore decimal dot            */

/* Date settings                                                            */

#define DF_CENTOFF   0
#define DF_CENTURY   1

#define DF_DMY 0
#define DF_MDY 1
#define DF_YMD 2
#define DF_EOT 3                                /* End of table for Century */

BYTE bCentury     = DF_CENTURY;                 /* Century on               */

/* Multiplication factors for different formats. */

long lFactDay  [] = { 10000,   100,     1, 1000000,   10000,       1 };
long lFactMonth[] = {   100, 10000,   100,   10000, 1000000,     100 };
long lFactYear [] = {     1,     1, 10000,       1,       1,   10000 };

char *szDatePict   =   "DD/MM/YYYY";               /* TODO:Drop SET DATE    */
char *szBritish[]  = { "DD/MM/YY", "DD/MM/YYYY" }; /* For @E                */

/*
   PictFunc -> Analyze function flags and return binary flags bits

   szPict  : Pointer to the picture
   lPicLen : Pointer to the length.  Changed during execution.
*/
int PictFunc( char **szPict, long *lPicLen )
{
   int  bDone     = FALSE;
   int  iPicFlags = 0;

   char *szPic    = *szPict;

   szPic++;
   (*lPicLen)--;
   while( *lPicLen && !bDone )
   {
      switch( toupper(*szPic) )
      {
         case ' ':                              /* End of function string   */
            bDone = TRUE;
            break;
         case '!':
            iPicFlags |= PF_UPPER;
            break;
         case '(':
            iPicFlags |= PF_PARNEG;
            break;
         case '0':
            iPicFlags |= PF_ZERO;
            break;
         case 'B':
            iPicFlags |= PF_LEFT;
            break;
         case 'C':
            iPicFlags |= PF_CREDIT;
            break;
         case 'D':
            iPicFlags |= PF_DATE;
            break;
         case 'E':
            iPicFlags |= PF_BRITISH;
            break;
         case 'R':
            iPicFlags |= PF_REMAIN;
            break;
         case 'X':
            iPicFlags |= PF_DEBIT;
            break;
         case 'Z':
            iPicFlags |= PF_EMPTY;
            break;
      }
      szPic++;
      (*lPicLen)--;
   }
   return( iPicFlags );
}

/*
    NumPicture -> Handle a numeric picture.

    This function is ALSO called by DatePicture.

    szPic       : Picture
    lPic        : Length of picture
    iPicFlags   : Function flags. NUM_DATE tells whether its a number or date
    dValue      : Number to picture
    lRetSize    : The size of the returned string is passed here !
*/
char *NumPicture( char *szPic, long lPic, int iPicFlags, double dValue,
                  long *lRetSize )
{
   int   iWidth;                                /* Width of string          */
   int   iDecimals;                             /* Number of decimals       */
   int   i;
   int   iCount;

   char *szRet;
   char *szStr;
   char  cPic;

   PITEM pItem  = NULL;                         /* Suppress warning         */

   BYTE  bFound = FALSE;
   BYTE  bEmpty;                                /* Suppress empty string    */

   double dPush;

   iCount = 0;

   szRet  = (char *) _xgrab( lPic+4 );          /* Grab enough              */
   *szRet = 0;
   for( i=0; i<lPic && !bFound; i++ )           /* Count number in front    */
   {
      if( szPic[i] == '.' )
         bFound = !(iPicFlags & PF_NUMDATE);    /* Exit when numeric        */
      else if( szPic[i] == '9' || szPic[i] == '#' ||
               szPic[i] == '$' || szPic[i] == '*' )
         iCount++;
   }
   iWidth = iCount;

   if( bFound )                                 /* Did we find a dot        */
   {
      iDecimals = 0;
      iWidth++;                                 /* Also adjust iWidth       */
      for( ; i<lPic; i++ )
      {
         if( szPic[i] == '9' || szPic[i] == '#' ||
             szPic[i] == '$' || szPic[i] == '*' )
         {
            iWidth++;
            iDecimals++;
         }
      }
   }
   else
      iDecimals = 0;

   if( ( iPicFlags & (PF_DEBIT + PF_PARNEG) ) && ( dValue < 0 ) )
      dPush = -dValue;                          /* Always push absolute val */
   else
      dPush = dValue;

   bEmpty = !dPush && ( iPicFlags & PF_EMPTY ); /* Suppress 0               */

   PushSymbol ( GetDynSym( "STR" )->pSymbol );  /* Push STR function        */
   PushNil    ();                               /* Function call. No object */

   PushDouble ( dPush );                        /* Push value to transform  */
   PushInteger( iWidth );                       /* Push numbers width       */
   PushInteger( iDecimals );                    /* Push decimals            */
   Function( 3 );                               /* 3 Parameters             */
   pItem = &stack.Return;
   if( IS_STRING( pItem ) )                     /* Is it a string           */
   {
      szStr  = pItem->value.szText;
      iCount = 0;

      if( iPicFlags & PF_ZERO )                 /* Pad with Zero's          */
      {
         for( i=0; szStr[i] == ' ' && i < iWidth; i++ )
            szStr[i] = '0';
      }
      if( bEmpty && pItem->wLength )            /* Suppress empty value     */
      {
         szStr[pItem->wLength - 1] = ' ';
      }

      if( iPicFlags & PF_LEFT )                 /* Left align               */
      {
         for( i=0; szStr[i] == ' ' && i < iWidth; i++ );
                                                /* Find first non-space     */

         if( i && i != iWidth )                 /* Any found or end of str  */
         {
            memcpy(szStr, szStr+i, iWidth-i);
            for( i = iWidth-i; i < iWidth; i++ )
               szStr[i] = ' ';                  /* Pad with spaces          */
         }
      }

      for( i=0; i < lPic; i++ )
      {
         cPic = szPic[i];
         if( cPic == '9' || cPic == '#' )
            szRet[i] = szStr[iCount++];         /* Just copy                */
         else if( cPic == '.' )
         {
            if( iPicFlags & PF_NUMDATE )        /* Dot in date              */
               szRet[i] = cPic;
            else                                /* Dot in number            */
            {
               if( iPicFlags & PF_EXCHANG )     /* Exchange . and ,         */
               {
                  szRet[i] = ',';
                  iCount++;
               }
               else
                  szRet[i] = szStr[iCount++];
            }
         }
         else if( cPic == '$' || cPic == '*' )
         {
            if( szStr[iCount] == ' ' )
            {
               szRet[i] = cPic;
               iCount++;
            }
            else
               szRet[i] = szStr[iCount++];
         }
         else if( cPic == ',' )                 /* Comma                    */
         {
            if( iCount && isdigit(szStr[iCount-1]) ) /* May we place it     */
            {
               if( iPicFlags & PF_EXCHANG )
                  szRet[i] = '.';
               else
                  szRet[i] = ',';
            }
            else
               szRet[i] = ' ';
         }
         else
            szRet[i] = cPic;
      }
      if( (iPicFlags & PF_CREDIT) && (dValue >= 0) )
      {
         szRet[i++] = ' ';
         szRet[i++] = 'C';
         szRet[i++] = 'R';
      }

      if( (iPicFlags & PF_DEBIT) && (dValue < 0) )
      {
         szRet[i++] = ' ';
         szRet[i++] = 'D';
         szRet[i++] = 'B';
      }

      if( (iPicFlags & PF_PARNEG) && (dValue < 0) )
      {
         if( isdigit(*szRet) )                  /* Overflow                 */
         {
            for( iCount = 1; iCount < i; iCount++ )
            {
               if( isdigit( szRet[iCount] ) )
                  szRet[iCount] = '*';
            }
         }
         *szRet     = '(';
         szRet[i++] = ')';
      }

      *lRetSize = i;
      szRet[i]  = 0;
   }
   else
   {
      printf( "\nNUMPICTURE: STR does not return string" );
      _exit(1);
   }
   return(szRet);
}


/*
    NumDefault -> Handle default numerics.

    dValue      : Number to picture
    lRetSize    : The size of the returned string is passed here !
*/
PITEM NumDefault( double dValue )
{                                               /* Default number           */
                                                /* TODO: Change to str call */
   PushSymbol ( GetDynSym( "STR" )->pSymbol );  /* Push STR function         */
   PushNil    ();                               /* Function call. No object  */

   PushDouble ( dValue );                       /* Push value to transform   */
   Function   ( 1 );                            /* 1 Parameter               */
   StackPop   ();                               /* Pop return value          */
   if( stack.pPos->wType != IT_STRING )         /* Is it a string            */
   {
      printf( "\nNUMDEFAULT: STR does not return string" );
      _exit(1);
   }
   return( stack.pPos );
}


/*
    DatePicture -> Handle dates.

    lDate       : Date to handle
    iPicFlags   : Function flags
    lRetSize    : The size of the returned string is passed here !
*/
char *DatePicture( long lDate, int iPicFlags, long *lRetSize )
{
   BYTE  bFormat;

   int   n;
   int   iLenPic;                               /* Length picture           */

   char *szDateFormat;                          /* Date format to be used   */
   char *szIntPicture;                          /* Internal picture used    */
   char *szResult;
   char  c;

   long  lDay;
   long  lMonth;
   long  lYear;

   double dIn;

   if( iPicFlags & PF_BRITISH )
   {
      bFormat      = DF_DMY;                    /* Just use british         */
      szDateFormat = szBritish[ bCentury ];
   }
   else
   {
      szDateFormat = szDatePict;                /* Analyze date format      */
      c = toupper( *szDateFormat );
      if( c == 'D' )
         bFormat = DF_DMY;
      else if ( c == 'M' )
         bFormat = DF_MDY;
      else if ( c == 'Y' )
         bFormat = DF_YMD;
      else                                      /* QUESTION: Error ?        */
         bFormat = DF_DMY;
   }

   if( lDate <= 0 )                             /* Missing date             */
   {
      lDay   = 0;
      lMonth = 0;
      lYear  = 0;
      iPicFlags |= PF_EMPTY;                    /* Suppress empty           */
   }
   else
   {
      iPicFlags |= PF_ZERO;                     /* Pad with zeros           */
      hb_dateDecode( lDate, &lDay, &lMonth, &lYear );
                                                /* Calculate d/m/y          */
   }
   iLenPic = strlen( szDateFormat );
   szIntPicture = (char *) _xgrab( iLenPic+1 );
   for( n = 0; n < iLenPic; n++ )               /* Create internal picture  */
   {
      c = toupper(szDateFormat[n]);
      if( c == 'D' || c == 'M' || c == 'Y' )    /* Change format markers    */
      {
         szIntPicture[n] = lDay ? '9' : ' ';    /* Empty date -> No picture */
      }
      else
         szIntPicture[n] = szDateFormat[n];     /* Copy the pattern         */
   }
   szIntPicture[n] = 0;                         /* Close the string         */

   iPicFlags |= PF_NUMDATE;                     /* Internal date flag       */

   if( bCentury )
      bFormat += DF_EOT;                        /* Use the second part      */

/*                                                                          */
/* Transfer the date to a number. Example :                                 */
/*                                                                          */
/*   bFormat == DMY    12/05/1925 => 12051925   d*1M + m*10K + y            */
/*   bFormat == YMD    1998.05.25 => 19980525   d    + m*100 + y*10K        */
/*                                                                          */

   dIn  = ( (double) lDay   ) * lFactDay  [ bFormat ];
   dIn += ( (double) lMonth ) * lFactMonth[ bFormat ];
   if( iLenPic == 8 )                           /* 2 digit year. Y2K?       */
      dIn += ( (double) (lYear % 100) ) * lFactYear [ bFormat ];
   else                                         /* 4 digit year             */
      dIn += ( (double) lYear ) * lFactYear [ bFormat ];

   szResult = NumPicture( szIntPicture, iLenPic, iPicFlags, dIn, lRetSize );
                                                /* And give to NumPicture   */
   _xfree( szIntPicture );

   return( szResult );
}


HARBOUR TRANSFORM( void )
{
   PITEM pPic       = _param( 2, IT_STRING);    /* Picture string           */
   PITEM pExp       = _param( 1, IT_ANY );      /* Input parameter          */

   char *szPic      = pPic->value.szText;
   char *szTemp;
   char *szResult;
   char *szExp;

   PITEM pItem;

   long  lPic       = pPic->wLength;
   long  lPicStart  = 0;                        /* Start of template        */
   long  lExpPos    = 0;
   long  lResultPos = 0;

   int   iPicFlags  = 0;                        /* Function flags           */
   int   n;

   BYTE  bDone      = FALSE;

   if( lPic )
   {
      if( pPic->wLength )
      {
         if( *szPic == '@' )                    /* Function marker found    */
         {
            iPicFlags = PictFunc( &szPic, &lPic ); /* Get length of function*/
            lPicStart = pPic->wLength - lPic;   /* Get start of template    */
         }

         switch( pExp->wType & ~IT_BYREF )
         {
            case IT_STRING:
            {
               szExp = pExp->value.szText;
               szResult = (char *)_xgrab( ( (lPic-lPicStart) > pExp->wLength) ?
                             (lPic-lPicStart) + 1 : pExp->wLength + 1 );
                                                /* Grab enough              */
               szPic += lPicStart;              /* Skip functions           */

               if( iPicFlags & PF_UPPER )       /* Function : @!            */
               {
                  szTemp = szExp;               /* Convert to upper         */
                  for( n = pExp->wLength; n ; n--)
                  {
                     *szTemp = toupper( *szTemp );
                     szTemp++;
                  }
               }

               if( lPic )                       /* Template string          */
               {
                  while( lPic && lExpPos < pExp->wLength )
                  {                             /* Analyze picture mask     */
                     switch( *szPic )
                     {
                        case '!':               /* Upper                    */
                        {
                           szResult[lResultPos++] = toupper(szExp[lExpPos++]);
                           break;
                        }
                        case 'L':               /* Ignored                  */
                        case 'Y':
                        case '*':
                        case '$':
                        case '.':
                        case ',':
                           break;

                        case '#':               /* Out the character        */
                        case '9':
                        case 'A':
                        case 'N':
                        case 'X':
                        case ' ':
                        {
                           szResult[lResultPos++] = szExp[lExpPos++];
                           break;
                        }

                        default:                /* Other choices            */
                        {
                           szResult[lResultPos++] = *szPic;
                           lExpPos++;
                        }
                     }
                     szPic++;
                     lPic--;
                  }
               }
               else if( iPicFlags & (PF_UPPER + PF_REMAIN) )
               {                                /* Without template         */
                  for( n = pExp->wLength; n; n--)
                    szResult[lResultPos++] = *szExp++;
               }

               if( ( iPicFlags & PF_REMAIN ) && lPic )  /* Any chars left   */
               {
                  for( n = lPic; n; n--)
                     szResult[lResultPos++] = *szPic; /* Export remainder   */
               }                                
               _retclen(szResult, lResultPos);
               _xfree(szResult);
               break;
            }

            case IT_LOGICAL:
            {
               szExp      =  pExp->value.szText;
               szResult   =  (char *) _xgrab( lPic + 1 );
                                                /* That's all folks        */
               szPic      += lPicStart;         /* Skip functions           */
               lResultPos =  1;

               if( lPic )                       /* Template string          */
               {
                  switch( *szPic )
                  {
                     case 'Y':                  /* Yes/No                   */
                     {
                        *szResult = pExp->value.iLogical ? 'Y' : 'N';
                        szPic++;
                        lPic--;
                        bDone = TRUE;           /* Logical written          */
                        break;
                     }

                     case '#':
                     case 'L':                  /* True/False               */
                     {
                        *szResult = pExp->value.iLogical ? 'T' : 'F';
                        szPic++;
                        lPic--;
                        bDone = TRUE;
                        break;
                     }

                     default:
                     {
                       *szResult = *szPic++;
                       lPic--;
                     }
                  }
               }
               if( ( iPicFlags & PF_REMAIN ) && lPic ) /* Any chars left    */
               {
                  for( n = lPic; n; n--)        /* Copy remainder           */
                     szResult[lResultPos++] = *szPic++;
                  if( !bDone )                  /* Logical written ?        */
                     szResult[lResultPos++] = pExp->value.iLogical ? 'T' : 'F';
               }
               _retclen( szResult, lResultPos );
               _xfree( szResult );
               break;
            }
            case IT_INTEGER:
            {
               szResult = NumPicture( szPic + lPicStart, lPic, iPicFlags,
                       (double) pExp->value.iNumber, &lResultPos );
               _retclen( szResult, lResultPos );
               _xfree( szResult );
               break;
            }
            case IT_LONG:
            {
               szResult = NumPicture( szPic + lPicStart, lPic, iPicFlags,
                       (double) pExp->value.lNumber, &lResultPos );
               _retclen( szResult, lResultPos );
               _xfree( szResult );
               break;
            }
            case IT_DOUBLE:
            {
               szResult = NumPicture( szPic + lPicStart, lPic, iPicFlags,
                       (double) pExp->value.dNumber, &lResultPos );
               _retclen( szResult, lResultPos);
               _xfree( szResult );
               break;
            }
            case IT_DATE:
            {                   /* Date is currently British; Century is on */
               szResult = DatePicture( pExp->value.lDate, iPicFlags, &lResultPos );
               _retclen( szResult, lResultPos );
               _xfree( szResult );
               break;
            }
            default:
            {
               PITEM pError = _errNew();

               _errPutDescription(pError, "Argument error: TRANSFORM");
               _errLaunch(pError);
               _errRelease(pError);
               _retc("");
            }
         }
      }
      else
      {
         PITEM pError = _errNew();

         _errPutDescription(pError, "Argument error: TRANSFORM");
         _errLaunch(pError);
         _errRelease(pError);
         _retc("");
      }
   }
   else                                         /* No picture supplied      */
   {
      switch( pExp->wType & ~IT_BYREF )         /* Default behaviour        */
      {
         case IT_STRING:
         {
            _retclen( pExp->value.szText, pExp->wLength);
            break;
         }
         case IT_LOGICAL:
         {
            _retclen( pExp->value.iLogical ? "T" : "F", 1);
            break;
         }
         case IT_INTEGER:
         {
            pItem = NumDefault( (double) pExp->value.iNumber );
            _retclen( pItem->value.szText, pItem->wLength );
            ItemRelease( pItem );
            break;
         }
         case IT_LONG:
         {
            pItem = NumDefault( (double) pExp->value.lNumber );
            _retclen( pItem->value.szText, pItem->wLength );
            ItemRelease( pItem );
            break;
         }
         case IT_DOUBLE:
         {
            pItem = NumDefault( (double) pExp->value.dNumber );
            _retclen( pItem->value.szText, pItem->wLength );
            ItemRelease( pItem );
            break;
         }
         case IT_DATE:
         {
            szResult = DatePicture( pExp->value.lDate, iPicFlags, &lResultPos );
            _retclen( szResult, lResultPos );
            _xfree( szResult );
            break;
         }
         default:
         {
           PITEM pError = _errNew();

           _errPutDescription(pError, "Argument error: TRANSFORM");
           _errLaunch(pError);
           _errRelease(pError);
           _retc("");
         }
      }
   }
}


