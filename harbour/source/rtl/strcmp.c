/*
 * $Id$
 */

#include <extend.h>
#include <set.h>

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int hb_itemStrCmp( PITEM pFirst, PITEM pSecond, BOOL bForceExact )
{
   char *szFirst   = pFirst->value.szText;
   char *szSecond  = pSecond->value.szText;
   long lLenFirst  = pFirst->wLength;	/* TODO: change ITEM.wLength from WORD to long */
   long lLenSecond = pSecond->wLength;	/* TODO: change ITEM.wLength from WORD to long */
   long lMinLen    = lLenFirst < lLenSecond ? lLenFirst : lLenSecond;
   long lCounter;
   int  iRet = 0;                       /* Current status               */

   if( lMinLen )                        /* One of the strings is empty  */
   {
      for( lCounter = 0; lCounter < lMinLen && !iRet; lCounter++ )
      {
         if( *szFirst != *szSecond )    /* Difference found             */
            iRet = (*szFirst < *szSecond) ? -1 : 1;
         else                           /* TODO : #define some constants*/
         {
           szFirst++;
           szSecond++;
         }
      }
/* printf ("\nhb_itemStrCmp: iRet = %d, lCounter = %ld, lLenFirst = %ld, lLenSecond = %ld", iRet, lCounter, lLenFirst, lLenSecond); */
      if( hb_set.HB_SET_EXACT || bForceExact || lLenSecond > lCounter )
      {  /* Force an exact comparison */
         if( !iRet && lLenFirst != lLenSecond )
                                        /* If length is different !     */
            iRet = (lLenFirst < lLenSecond) ? -1 : 1;
      }
/* printf ("\n; hb_set.HB_SET_EXACT = %d, bForceExact = %d, iRet = %d.\n", hb_set.HB_SET_EXACT, bForceExact, iRet); */
   }
   else
   {
      if( lLenFirst != lLenSecond )     /* Both empty ?                 */
         iRet = (lLenFirst < lLenSecond) ? -1 : 1;
      else
         iRet = 0;                      /* Both empty => Equal !        */
   }
   return(iRet);
}

