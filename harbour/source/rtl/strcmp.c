/*
 * $Id$
 */

#include <hbsetup.h>
#include <ctype.h>
#include <extend.h>
#include <set.h>

int hb_stricmp( const char *s1, const char *s2 )
{
   int rc = 0, c1, c2;
   USHORT l1, l2, count;
   l1 = strlen( s1 );
   l2 = strlen( s2 );
   if( l1 < l2 ) count = l1;
   else count = l2;
   while( rc == 0 && count > 0 )
   {
      count--;
      c1 = toupper( *s1++ );
      c2 = toupper( *s2++ );
      if( c1 != c2 ) rc = ( c1 < c2 ? -1 : 1 );
   }
   if( rc == 0 && l1 != l2 )
   {
      if( l1 < l2 ) rc = -1;
      else rc = 1;
   }
   return rc;
}

/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact )
{
   char *szFirst   = pFirst->item.asString.value;
   char *szSecond  = pSecond->item.asString.value;
   ULONG lLenFirst  = pFirst->item.asString.length;  
   ULONG lLenSecond = pSecond->item.asString.length; 
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
