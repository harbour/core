/*
 * $Id$
 */

#include "extend.h"

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

HARBOUR HB_ROT13( void )
{
  if( ISCHAR(1) )
    {
      char *szText = hb_parc( 1 );
      ULONG i, lLen = hb_parclen( 1 );
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
