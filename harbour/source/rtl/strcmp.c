/*
 * $Id$
 */

#include <extend.h>
#include <set.h>

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact )
{
   char *szFirst   = pFirst->value.szText;
   char *szSecond  = pSecond->value.szText;
   long lLenFirst  = pFirst->wLength;	/* TODO: change HB_ITEM.wLength from WORD to long */
   long lLenSecond = pSecond->wLength;	/* TODO: change HB_ITEM.wLength from WORD to long */
   long lMinLen;
   long lCounter;
   int  iRet = 0;                       /* Current status               */

   if (hb_set.HB_SET_EXACT && !bForceExact)
   {					/* SET EXACT ON and not using == */
                                        /* Don't include trailing spaces */
      while( lLenFirst > 0 && szFirst[ lLenFirst - 1 ] == ' ') lLenFirst--;
      while( lLenSecond > 0 && szSecond[ lLenSecond - 1 ] == ' ') lLenSecond--;
   }
   lMinLen = lLenFirst < lLenSecond ? lLenFirst : lLenSecond;
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
      if( hb_set.HB_SET_EXACT || bForceExact || lLenSecond > lCounter )
      {  /* Force an exact comparison */
         if( !iRet && lLenFirst != lLenSecond )
                                        /* If length is different !     */
               iRet = (lLenFirst < lLenSecond) ? -1 : 1;
      }
   }
   else
   {
      if( lLenFirst != lLenSecond )     /* Both empty ?                 */
      {
         if( hb_set.HB_SET_EXACT || bForceExact )
            iRet = (lLenFirst < lLenSecond) ? -1 : 1;
         else
            iRet = (lLenSecond == 0) ? 0 : -1;
      }
      else
         iRet = 0;                      /* Both empty => Equal !        */
   }
   return(iRet);
}

