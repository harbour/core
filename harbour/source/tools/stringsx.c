/*
 * $Id$
 */

#include <extend.h>
#include <ctype.h>

/* TODO: search this file for TODO and find 'em! */

/* debug function to dump the ASCII values of an entire string */
HARBOUR STRDUMP( void )
{
   char *szText = _parc(1);
   long i, lLength = _parclen(1);
   for( i = 0; i < lLength; i++ )
      printf("%d ", szText[i]);
   printf("\n");
}

char *hb_strtoken(char *szText, long lText, long lIndex, char cDelimiter, long *lLen)
{
   long wStart, wEnd = 0, wCounter = 0;

   do
   {
      wStart = wEnd;

      if( cDelimiter != ' ' )
      {
         if( szText[wStart] == cDelimiter )
            wStart++;
      }
      else
      {
         while( wStart < lText && szText[wStart] == cDelimiter )
            wStart++;
      }

      if( wStart < lText && szText[wStart] != cDelimiter )
      {
         wEnd = wStart + 1;

         while( wEnd < lText && szText[wEnd] != cDelimiter )
            wEnd++;
      }
      else
         wEnd = wStart;
   } while( wCounter++ < lIndex - 1 && wEnd < lText );

   if( wCounter < lIndex )
   {
      *lLen = 0;
      return "";
   }
   else
   {
      *lLen = wEnd - wStart;
      return szText + wStart;
   }
}

/* returns the nth occurence of a substring within a token-delimited string */
HARBOUR STRTOKEN( void )
{
   char *szText;
   long lIndex = _parnl(2);
   char cDelimiter = *_parc(3);
   long lLen;

   if( !cDelimiter )
     cDelimiter = ' ';

   szText = hb_strtoken(_parc(1), _parclen(1), lIndex, cDelimiter, &lLen);

   _stornl(lLen, 4);
   _retclen(szText, lLen);
}

HARBOUR ROT13( void )
{
   PITEM pText = _param(1, IT_STRING);

   if( pText )
   {
      char *szText = pText->value.szText;
      long i, lLen = pText->wLength;
      char *szResult = _xgrab(lLen + 1);

      for( i = 0; i < lLen; i++ )
      {
         char c = szText[i];
         if( (c >= 'A' && c <= 'M') || (c >= 'a' && c <= 'm') )
            c += 13;
         else if( (c >= 'N' && c <= 'Z') || (c >= 'n' && c <= 'z') )
            c -= 13;

         szResult[i] = c;
      }
      _retclen(szResult, lLen);
      _xfree(szResult);
   }
   else
      _retc("");
}

