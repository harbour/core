/*
 * $Id$
 */

#include <extend.h>
#include <ctype.h>
#include <set.h>

extern STACK stack;

#define HB_ISSPACE(c) ((c) == 9 || (c) == 10 || (c) == 13 || (c) == 32)

BOOL hb_strempty( char * szText, ULONG ulLen )
{
   BOOL bRetVal = TRUE;

   while( ulLen-- )
   {
      char c = szText[ulLen];

      if( !HB_ISSPACE( c ) )
      {
         bRetVal = FALSE;
         break;
      }
   }

   return bRetVal;
}

/* determines if first char of string is letter */
/* TEST: QOUT( "isalpha( 'hello' ) = ", isalpha( 'hello' ) ) */
/* TEST: QOUT( "isalpha( '12345' ) = ", isalpha( '12345' ) ) */
HARBOUR ISALPHA( void )
{
   _retl(isalpha(*_parc(1)));
}

/* determines if first char of string is digit */
/* TEST: QOUT( "isdigit( '12345' ) = ", isdigit( '12345' ) ) */
/* TEST: QOUT( "isdigit( 'abcde' ) = ", isdigit( 'abcde' ) ) */
HARBOUR ISDIGIT( void )
{
   _retl(isdigit(*_parc(1)));
}

/* determines if first char of string is upper-case */
/* TEST: QOUT( "isupper( 'Abcde' ) = ", isupper( 'Abcde' ) ) */
/* TEST: QOUT( "isupper( 'abcde' ) = ", isupper( 'abcde' ) ) */
HARBOUR ISUPPER( void )
{
   _retl(isupper(*_parc(1)));
}

/* determines if first char of string is lower-case */
/* TEST: QOUT( "islower( 'abcde' ) = ", islower( 'abcde' ) ) */
/* TEST: QOUT( "islower( 'Abcde' ) = ", islower( 'Abcde' ) ) */
HARBOUR ISLOWER( void )
{
   _retl(islower(*_parc(1)));
}

/* trims from the left, and returns a new pointer to szText */
/* also returns the new length in lLen */
char *hb_strLTrim( char *szText, ULONG *lLen )
{
   while( *lLen && HB_ISSPACE(*szText) )
   {
      szText++;
      (*lLen)--;
   }
   return szText;
}

/* trims leading spaces from a string */
/* TEST: QOUT( "ltrim( '  hello world  ' ) = '" + ltrim( '  hello world  ' ) + "'" ) */
HARBOUR LTRIM( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);

      if( pText )
      {
         ULONG lLen = pText->wLength;
         char *szText = hb_strLTrim(pText->value.szText, &lLen);

         _retclen(szText, lLen);
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: LTRIM");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: LTRIM");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns szText and the new length in lLen */
ULONG hb_strRTrimLen( char *szText, ULONG lLen )
{
   while( lLen && szText[lLen - 1] == ' ' )
      lLen--;
   return lLen;
}

/* trims trailing spaces from a string */
/* TEST: QOUT( "rtrim( '  hello world  ' ) = '" + rtrim( '  hello world  ' ) + "'" ) */
HARBOUR RTRIM( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);
      if( pText )
         _retclen(pText->value.szText, hb_strRTrimLen(pText->value.szText, pText->wLength));
      else
         /* Clipper doesn't error */
         _retc("");
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: RTRIM");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* synonymn for RTRIM */
HARBOUR TRIM( void )
{
   RTRIM();
}

/* trims leading and trailing spaces from a string */
/* TEST: QOUT( "alltrim( '  hello world  ' ) = '" + alltrim( '  hello world  ' ) + "'" ) */
HARBOUR ALLTRIM( void )
{
   if( _pcount() > 0 )
   {
      char *szText = _parc(1);
      ULONG lLen = hb_strRTrimLen(szText, _parclen(1));

      szText = hb_strLTrim(szText, &lLen);

      _retclen(szText, lLen);
   }
   else
      /* Clipper doesn't error */
      _retc("");
}

/* right-pads a string with spaces or supplied character */
/* TEST: QOUT( "padr( 'hello', 10 ) = '" + padr( 'hello', 10 ) + "'" ) */
HARBOUR PADR( void )
{
   char *szText = _parc(1);

   if( _pcount() > 1 )
   {
      long lLen = _parnl(2);

      if( lLen > (long)_parclen(1) )
      {
         char *szResult = (char *)_xgrab(lLen + 1);
         long lPos;
         char cPad;

         memcpy(szResult, szText, _parclen(1));

         cPad = ( _pcount() > 2? *(_parc(3)): ' ' );

         for( lPos = _parclen(1); lPos < lLen; lPos++ )
            szResult[lPos] = cPad;

         _retclen(szResult, lLen);
         _xfree(szResult);
      }
      else
         _retclen(szText, (long)_parclen(1) );
   }
   else
      _retc("");
}

/* synonymn for PADR */
HARBOUR PAD( void )
{
   PADR();
}

/* left-pads a string with spaces or supplied character */
/* TEST: QOUT( "padl( 'hello', 10 ) = '" + padl( 'hello', 10 ) + "'" ) */
HARBOUR PADL( void )
{
   char *szText = _parc(1);

   if( _pcount() > 1 )
   {
      long lLen = _parnl(2);

      if( lLen > (long) _parclen(1) )
      {
         char *szResult = (char *)_xgrab(lLen + 1);
         long lPos = lLen - _parclen(1);
         char cPad;

         memcpy(szResult + lPos, szText, _parclen(1));

         cPad = (_pcount() > 2? *(_parc(3)): ' ');

         for(; lPos > 0; lPos--)
         {
            szResult[lPos - 1] = cPad;
         }

         _retclen(szResult, lLen);
         _xfree(szResult);
      }
      else
         _retclen(szText, (long)_parclen(1) );
   }
   else
      _retc("");
}

/* centre-pads a string with spaces or supplied character */
/* TEST: QOUT( "padc( 'hello', 10 ) = '" + padc( 'hello', 10 ) + "'" ) */
HARBOUR PADC( void )
{
   char *szText = _parc(1);

   if( _pcount() > 1 )
   {
      long lLen = _parnl(2);

      if( lLen > (long) _parclen(1) )
      {
         char *szResult = (char *)_xgrab(lLen + 1);
         char cPad;
         long w, lPos = (lLen - _parclen(1)) / 2;

         memcpy(szResult + lPos, szText, _parclen(1) + 1);

         cPad = ( _pcount() > 2? *_parc(3): ' ' );

         for( w = 0; w < lPos; w++ )
            szResult[w] = cPad;

         for( w = _parclen(1) + lPos; w < lLen; w++ )
            szResult[w] = cPad;

         szResult[lLen] = 0;

         _retclen(szResult, lLen);
         _xfree(szResult);
      }
      else
         _retclen(szText, (long)_parclen(1) );
   }
   else
      _retc("");
}

ULONG hb_strAt(char *szSub, long lSubLen, char *szText, long lLen)
{
   if( lSubLen )
   {
      if( lLen >= lSubLen )
      {
         long lPos = 0, lSubPos = 0;

         while( lPos < lLen && lSubPos < lSubLen )
         {
            if( *(szText + lPos) == *(szSub + lSubPos) )
            {
               lSubPos++;
               lPos++;
            }
            else if( lSubPos )
               lSubPos = 0;
            else
               lPos++;
         }
         return (lSubPos < lSubLen? 0: lPos - lSubLen + 1);
      }
      else
         return 0;
   }
   else
      return 1;
}

/* locates a substring in a string */
/* TEST: QOUT( "at( 'cde', 'abcdefgfedcba' ) = '" + at( 'cde', 'abcdefgfedcba' ) + "'" ) */
HARBOUR AT( void )
{
   PHB_ITEM pSub = _param(1, IT_ANY);
   PHB_ITEM pText = _param(2, IT_ANY);

   if( pText && pSub )
   {
      if( IS_STRING( pText ) && IS_STRING( pSub ) )
      {
         _retnl( hb_strAt(pSub->value.szText, pSub->wLength, pText->value.szText, pText->wLength) );
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: AT");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: AT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* locates a substring in a string starting at the end */
/* TEST: QOUT( "rat( 'cde', 'abcdefgfedcba' ) = '" + rat( 'cde', 'abcdefgfedcba' ) + "'" ) */
HARBOUR RAT( void )
{
   long lSubLen = _parclen(1);

   if( lSubLen )
   {
      long lPos = _parclen(2) - lSubLen;
      if( lPos < 0 )
         _retni(0);
      else
      {
         char *szSub = _parc(1);
         char *szText = _parc(2);
         int bFound = 0;

         while( lPos >= 0 && !bFound )
         {
            if( *(szText + lPos) == *szSub )
               bFound = !memcmp(szSub, szText + lPos, lSubLen);
            lPos--;
         }
         _retnl( bFound? lPos + 2: 0 );
      }
   }
   else
      /* This function never seems to raise an error */
      _retni(0);
}

/* converts an ASCII code to a character value */
HARBOUR CHR( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pAsc = _param(1, IT_NUMERIC);

      if( pAsc )
      {
         char chr[2];

         /* Believe it or not, clipper does this! */
         chr[0] = _parnl(1) % 256;
         chr[1] = 0;
         _retclen(chr, 1);
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: CHR");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: CHR");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* converts a character value to an ASCII code */
HARBOUR ASC(void)
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);

      if( pText )
      {
         if( pText->wLength > 0 )
            _retni(*(pText->value.szText));
         else
            _retni(0);
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: ASC");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: ASC");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns the left-most n characters in string */
HARBOUR LEFT( void )
{
   if( _pcount() == 2 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);

      if( pText )
      {
         PHB_ITEM pLen = _param(2, IT_NUMERIC);

         if( pLen )
         {
            long lLen = _parnl(2);

            if( lLen > pText->wLength )
               lLen = pText->wLength;

            else if( lLen < 0 )
               lLen = 0;

            _retclen(pText->value.szText, lLen);
         }
         else
         {
            PHB_ITEM pError = _errNew();
            _errPutDescription(pError, "Argument error: LEFT");
            _errLaunch(pError);
            _errRelease(pError);
         }
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: LEFT");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: LEFT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns the right-most n characters in string */
HARBOUR RIGHT( void )
{
   if( _pcount() == 2 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);

      if( pText )
      {
         PHB_ITEM pLen = _param(2, IT_NUMERIC);

         if( pLen )
         {
            long lLen = _parnl(2);

            if( lLen > pText->wLength )
               lLen = pText->wLength;

            else if( lLen < 0 )
               lLen = 0;

            _retclen(pText->value.szText + pText->wLength - lLen, lLen);
         }
         else
         {
            PHB_ITEM pError = _errNew();
            _errPutDescription(pError, "Argument error: RIGHT");
            _errLaunch(pError);
            _errRelease(pError);
         }
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: RIGHT");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: RIGHT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns l characters from n characters into string */
HARBOUR SUBSTR( void )
{
   if( _pcount() > 1 && _pcount() < 4 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);
      PHB_ITEM pPos = _param(2, IT_NUMERIC);

      if( pText && pPos )
      {
         long lPos = _parnl(2);

         if( lPos < 0 )
         {
            lPos += pText->wLength;
            if( lPos < 0 )
               lPos = 0;
         }
         else if( lPos )
         {
            lPos--;
         }

         if( lPos < pText->wLength )
         {
            PHB_ITEM pLen = _param(3, IT_NUMERIC);
            long lLen;

            if( pLen )
            {
               lLen = _parnl(3);

               if( lLen > pText->wLength - lPos )
                  lLen = pText->wLength - lPos;
            }
            else
               lLen = pText->wLength - lPos;

            if( lLen > 0 )
               _retclen(pText->value.szText + lPos, lLen);
            else
               _retc("");
         }
         else
            _retc("");
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: SUBSTR");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: SUBSTR");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* converts szText to lower case. Does not create a new string! */
char *hb_strLower(char *szText, long lLen)
{
   long i;
   for( i = 0; i < lLen; i++ )
      szText[i] = tolower(szText[i]);
   return szText;
}

/* converts string to lower case */
HARBOUR LOWER( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);

      if( pText )
      {
         long lLen = pText->wLength;

         _retclen(hb_strLower(pText->value.szText, lLen), lLen);
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: LOWER");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: LOWER");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* converts szText to upper case. Does not create a new string! */
char *hb_strUpper(char *szText, long lLen)
{
   long i;
   for( i = 0; i < lLen; i++ )
      szText[i] = toupper(szText[i]);
   return szText;
}

/* converts string to upper case */
HARBOUR UPPER( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);

      if( pText )
      {
         long lLen = pText->wLength;

         _retclen(hb_strUpper(pText->value.szText, lLen), lLen);
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: LOWER");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: LOWER");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns n copies of given string */
/* TEST: QOUT( "replicate( 'abc', 5 ) = " + replicate( 'abc', 5 ) ) */
HARBOUR REPLICATE( void )
{
   if( _pcount() == 2 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);
      PHB_ITEM pTimes = _param(2, IT_NUMERIC);

      if( pText && pTimes )
      {
         long lTimes = _parnl(2);

         if( lTimes > 0 )
         {
            char *szText = pText->value.szText;
            long lLen = pText->wLength;
            char *szResult = (char *)_xgrab((lLen * lTimes) + 1);
            char *szPtr = szResult;
            long i;

            for( i = 0; i < lTimes; i++ )
            {
               memcpy(szPtr, szText, lLen);
               szPtr += lLen;
            }
            _retclen(szResult, lLen * lTimes);
            _xfree(szResult);
         }
         else
            _retc("");
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: REPLICATE");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: REPLICATE");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns n copies of a single space */
/* TEST: QOUT( "space( 5 ) = '" + space( 5 ) + "'" ) */
HARBOUR SPACE( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pLen = _param(1, IT_NUMERIC);

      if( pLen )
      {
         long lLen = _parnl(1);

         if( lLen > 0 )
         {
            char *szResult = (char *)_xgrab(lLen + 1);

            memset(szResult, ' ', lLen);
            _retclen(szResult, lLen);
            _xfree(szResult);
         }
         else
            _retc("");
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: SPACE");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: SPACE");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* replaces characters in a string */
HARBOUR STUFF( void )
{
   PHB_ITEM pText;

   pText = _param(1, IT_STRING);
   if( pText )
   {
      char *szText = pText->value.szText;
      PHB_ITEM pPos, pDel, pInsert;
      long lPos, lDel, lInsert, lTotalLen;
      char *szInsert;

      pPos = _param(2, IT_NUMERIC);
      lPos = (pPos? pPos->value.lNumber - 1: 0);
      if( lPos > pText->wLength )
         lPos = pText->wLength;

      pDel = _param(3, IT_NUMERIC);
      if( pDel )
      {
         lDel = pDel->value.lNumber;
         if( lDel > pText->wLength - lPos )
            lDel = pText->wLength - lPos;
      }
      else
         lDel = 0;

      pInsert = _param(4, IT_STRING);
      if( pInsert )
      {
         szInsert = pInsert->value.szText;
         lInsert = pInsert->wLength;
      }
      else
      {
         szInsert = ""; /* shouldn't matter that we don't allocate */
         lInsert = 0;
      }

      if( (lTotalLen = pText->wLength + lInsert - lDel) > 0 )
      {
         char *szResult = (char *)_xgrab(lTotalLen + 1);

         memcpy(szResult, szText, lPos);
         memcpy(szResult + lPos, szInsert, lInsert);
         memcpy(szResult + lPos + lInsert, szText + lPos + lDel,
                pText->wLength - (lPos + lDel));

         szResult[lTotalLen] = 0;
         _retclen(szResult, lTotalLen);
         _xfree(szResult);
      }
      else
         _retc("");
   }
   else
      _retc("");
}

/* replaces lots of characters in a string */
HARBOUR STRTRAN( void )
{
   PHB_ITEM pText = _param(1, IT_STRING);

   if( pText )
   {
      PHB_ITEM pSeek = _param(2, IT_STRING);
      if( pSeek )
      {
         char *szText = pText->value.szText;
         if( pSeek->wLength && pSeek->wLength <= pText->wLength )
         {
            char *szSeek = pSeek->value.szText;
            PHB_ITEM pStart = _param(4, IT_NUMERIC);
            char *szReplace;
            long iStart;

            iStart = (pStart? _parnl(4): 1);
            if( !iStart )
            {
               /* Clipper seems to work this way */
               _retc("");
            }
            else if( iStart > 0 )
            {
               PHB_ITEM pReplace = _param(3, IT_STRING);
               PHB_ITEM pCount = _param(5, IT_NUMERIC);
               long iReplace;
               long iCount, bAll;

               if( pReplace )
               {
                  szReplace = pReplace->value.szText;
                  iReplace = pReplace->wLength;
               }
               else
               {
                  szReplace = ""; /* shouldn't matter that we don't allocate */
                  iReplace = 0;
               }

               if( pCount )
               {
                  iCount = pCount->value.lNumber;
                  bAll = 0;
               }
               else
               {
                  iCount = 0;
                  bAll = 1;
               }

               if( bAll || iCount > 0 )
               {
                  long iFound = 0;
                  long iReplaced = 0;
                  long i = 0;
                  long iLength = pText->wLength;

                  while( i < pText->wLength )
                  {
                     if( (bAll || iReplaced < iCount) && !memcmp(szText + i, szSeek, pSeek->wLength) )
                     {
                        iFound++;
                        if( iFound >= iStart )
                        {
                           iReplaced++;
                           iLength = iLength - pSeek->wLength + iReplace;
                           i += pSeek->wLength;
                        }
                        else
                           i++;
                     }
                     else
                        i++;
                  }

                  if( iFound )
                  {
                     char *szResult = (char *)_xgrab(iLength + 1);
                     char *szPtr = szResult;

                     iFound = 0;
                     i = 0;
                     while( i < pText->wLength )
                     {
                        if( iReplaced && !memcmp(szText + i, szSeek, pSeek->wLength) )
                        {
                           iFound++;
                           if( iFound >= iStart )
                           {
                              iReplaced--;
                              memcpy(szPtr, szReplace, iReplace);
                              szPtr += iReplace;
                              i += pSeek->wLength;
                           }
                           else
                           {
                              *szPtr = szText[i];
                              szPtr++;
                              i++;
                           }
                        }
                        else
                        {
                           *szPtr = szText[i];
                           szPtr++;
                           i++;
                        }
                     }
                     _retclen(szResult, iLength);
                     _xfree(szResult);
                  }
                  else
                     _retclen(szText, pText->wLength);
               }
                else
                  _retclen(szText, pText->wLength);
            }
            else
               _retclen(szText, pText->wLength);
         }
         else
            _retclen(szText, pText->wLength);
      }
      else
         _retc("");
   }
   else
      _retc("");
}

/* returns the numeric value of a character string representation of a number  */
double hb_strVal( char *szText )
{
   return atof(szText);
}

/* returns the numeric value of a character string representation of a number  */
HARBOUR VAL( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pText = _param(1, IT_STRING);

      if( pText )
      {
         int nWidth, nDec = 0;
         char * ptr = strchr( pText->value.szText, '.' );
         if( ptr )
         {
            nWidth = ptr - pText->value.szText;
            nDec = strlen( ptr + 1 );
         }
         else nWidth = strlen( pText->value.szText );
         _retnd(hb_strVal(pText->value.szText));
         stack.Return.wLength = nWidth;
         stack.Return.wDec    = nDec;
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: VAL");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: VAL");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* converts a numeric to a string with optional width & precision.
   This function should be used by any function that wants to format numeric
   data for displaying, printing, or putting in a database.
   
   Note: The caller is responsible for calling _xfree to free the results buffer,
         but ONLY if the return value is not a NULL pointer!
*/
char * hb_str( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec )
{
   char * szResult = 0;

   if( pNumber )
   {
      /* Default to the width and number of decimals specified by the item,
         with a limit of 9 decimal places */
      int iWidth = pNumber->wLength;
      int iDec   = pNumber->wDec;

      if( iDec > 9 )
         iDec = 9;
      if( hb_set_fixed )
         iDec = hb_set.HB_SET_DECIMALS;

      if( pWidth )
      {
         /* If the width parameter is specified, override the default value
            and set the number of decimals to zero */
         if( IS_INTEGER( pWidth ) )
            iWidth = pWidth->value.iNumber;
         else if( IS_LONG( pWidth ) )
            iWidth = (int) pWidth->value.lNumber;
         else if( IS_DOUBLE( pWidth ) )
            iWidth = (int) pWidth->value.dNumber;

         if( iWidth < 1 )
            iWidth = 10;                   /* If 0 or negative, use default */
         iDec = 0;
      }

      if( pDec )
      {
         /* This function does not include the decimal places in the width,
            so the width must be adjusted downwards, if the decimal places
            parameter is greater than 0  */
         if( IS_INTEGER( pDec ) )
            iDec = pDec->value.iNumber;
         else if( IS_LONG( pDec ) )
            iDec = (int) pDec->value.lNumber;
         else if( IS_DOUBLE( pDec ) )
            iDec = (int) pDec->value.dNumber;

         if( iDec < 0 )
            iDec = 0;
         else if( iDec > 0 )
            iWidth -= (iDec + 1);
      }

      if( iWidth )
      {
         /* We at least have a width value */
         int iBytes;
         int iSize = (iDec ? iWidth + 1 + iDec : iWidth);
         
         /* Be paranoid and use a large amount of padding */
         szResult = (char *)_xgrab( iWidth + iDec + 64 );

         if( IS_DOUBLE( pNumber ) || iDec != 0 )
         {
            double dNumber;

            if( IS_INTEGER( pNumber ) )
               dNumber = (double) pNumber->value.iNumber;
            else if( IS_LONG( pNumber ) )
               dNumber = (double) pNumber->value.lNumber;
            else if( IS_DOUBLE( pNumber ) )
               dNumber = pNumber->value.dNumber;

            if( iDec > 0 )
               iBytes = sprintf( szResult, "%*.*f", iSize, iDec, dNumber );
            else
               iBytes = sprintf( szResult, "%*ld", iWidth, (long)dNumber );
         }
         else switch( pNumber->wType & ~IT_BYREF )
         {
            case IT_LONG:
                 iBytes = sprintf( szResult, "%*li", iWidth, pNumber->value.lNumber );
                 break;

            case IT_INTEGER:
                 iBytes = sprintf( szResult, "%*i", iWidth, pNumber->value.iNumber );
                 break;

            default:
                 iBytes = 0;
                 *szResult = 0;
                 break;
         }
         /* Set to asterisks in case of overflow (same reason for paranoia) */
         if( iBytes > iSize )
         {
            memset( szResult, '*', iSize );
            szResult[ iSize ] = 0;
         }
      }
   }
   return( szResult );
}

/* converts a numeric to a string with optional width & precision.
   calls hb_str() after validating parameters
*/
HARBOUR STR( void )
{
   if( _pcount() > 0 && _pcount() < 4 )
   {
      BOOL bValid = TRUE;
      PHB_ITEM pNumber = _param( 1, IT_NUMERIC );
      PHB_ITEM pWidth  = 0;
      PHB_ITEM pDec    = 0;

      if( !pNumber )
         bValid = FALSE;
      else
      {
         if( _pcount() > 1 )
         {
            pWidth = _param( 2, IT_NUMERIC );
            if( !pWidth)
               bValid = FALSE;
         }
         if( _pcount() > 2 )
         {
            pDec = _param( 3, IT_NUMERIC );
            if( !pDec )
               bValid = FALSE;
         }
      }
      if( bValid )
      {
         char * szResult = hb_str( pNumber, pWidth, pDec );

         if( szResult )
         {
            _retc( szResult );
            _xfree( szResult );
         }
         else _retc( "" );
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: STR");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: STR");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

