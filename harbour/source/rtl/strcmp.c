#include <extend.h>

int OurStrCmp( PITEM pFirst, PITEM pSecond ) /* Check whether two strings
                                                are equal (0), smaller (-1),
                                                or greater (1) */
{
   char *szFirst   = pFirst->value.szText;
   char *szSecond  = pSecond->value.szText;
   long wLenFirst  = pFirst->wLength;
   long wLenSecond = pSecond->wLength;
   long wMinLen    = wLenFirst < wLenSecond ? wLenFirst : wLenSecond;
   long wCounter;           /* TODO : Should change w* to l* later on ...    */
                            /* TODO : Same applies to sz*. Any suggestions ? */
   int  iRet = 0;                       /* Current status               */

   if( wMinLen )                        /* One of the strings is empty  */
   {
      for( wCounter = 0; wCounter < wMinLen && !iRet; wCounter++ )
      {
         if( *szFirst != *szSecond )    /* Difference found             */
            iRet = (*szFirst < *szSecond) ? -1 : 1;
         else                           /* TODO : #define some constants*/
         {
           szFirst++;
           szSecond++;
         }
      }
      if( !iRet && wLenFirst != wLenSecond )
                                        /* If length is different !     */
         iRet = (wLenFirst < wLenSecond) ? -1 : 1;
   }
   else
   {
      if( wLenFirst != wLenSecond )     /* Both empty ?                 */
         iRet = (wLenFirst < wLenSecond) ? -1 : 1;
      else
         iRet = 0;                      /* Both empty => Equal !        */
   }
   return(iRet);
}

