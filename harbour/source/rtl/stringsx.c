/*
 * $Id$
 */

#include "extend.h"

char *hb_strtoken(char *szText,
		  long lText,
		  long lIndex,
		  char cDelimiter,
		  long *lLen)
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
