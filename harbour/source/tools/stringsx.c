/*
 * $Id$
 */

#include <extend.h>
#include <ctype.h>

/* TODO: search this file for TODO and find 'em! */

/* debug function to dump the ASCII values of an entire string */
HARBOUR HB_STRDUMP( void )
{
   char *szText = hb_parc(1);
   long i, lLength = hb_parclen(1);
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
HARBOUR HB_STRTOKEN( void )
{
   char *szText;
   long lIndex = hb_parnl(2);
   char cDelimiter = *hb_parc(3);
   long lLen;

   if( !cDelimiter )
     cDelimiter = ' ';

   szText = hb_strtoken(hb_parc(1), hb_parclen(1), lIndex, cDelimiter, &lLen);

   hb_stornl(lLen, 4);
   hb_retclen(szText, lLen);
}

HARBOUR HB_ROT13( void )
{
   PHB_ITEM pText = hb_param(1, IT_STRING);

   if( pText )
   {
      char *szText = pText->value.szText;
      long i, lLen = pText->wLength;
      char *szResult = (char*)hb_xgrab(lLen + 1);

      for( i = 0; i < lLen; i++ )
      {
         char c = szText[i];
         if( (c >= 'A' && c <= 'M') || (c >= 'a' && c <= 'm') )
            c += 13;
         else if( (c >= 'N' && c <= 'Z') || (c >= 'n' && c <= 'z') )
            c -= 13;

         szResult[i] = c;
      }
      hb_retclen(szResult, lLen);
      hb_xfree(szResult);
   }
   else
      hb_retc("");
}

