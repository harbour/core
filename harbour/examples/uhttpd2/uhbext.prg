/*
 * $Id$
 */

/************************************************************
*
* Functions candidates to be a part of Harbour's core or RTL
*
*************************************************************/


FUNC HGetDef(aHash, xKey, xDefault)
RETURN IIF(HB_HHasKey(aHash, xKey), aHash[ xKey ], xDefault)


FUNC split(cSeparator, cString)
LOCAL aRet := {}, nI

  DO WHILE (nI := AT(cSeparator, cString)) > 0
    AADD(aRet, LEFT(cString, nI - 1))
    cString := SUBSTR(cString, nI + LEN(cSeparator))
  ENDDO
  AADD(aRet, cString)
RETURN aRet


FUNC join(cSeparator, aData)
LOCAL cRet := "", nI

  FOR nI := 1 TO LEN(aData)
    IF nI > 1;  cRet += cSeparator
    ENDIF
    IF     VALTYPE(aData[nI]) $ "CM";  cRet += aData[nI]
    ELSEIF VALTYPE(aData[nI]) == "N";  cRet += LTRIM(STR(aData[nI]))
    ELSEIF VALTYPE(aData[nI]) == "D";  cRet += IF(!EMPTY(aData[nI]), DTOC(aData[nI]), "")
    ELSE
    ENDIF
  NEXT
RETURN cRet


#pragma begindump
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbthread.h"

typedef struct _HB_MUTEX
{
   int                  lock_count;
   int                  lockers;
   int                  waiters;
   PHB_ITEM             events;
   HB_THREAD_ID         owner;
   HB_RAWCRITICAL_T     mutex;
   HB_RAWCOND_T         cond_l;
   HB_RAWCOND_T         cond_w;
   BOOL                 fSync;
   struct _HB_MUTEX *   pNext;
   struct _HB_MUTEX *   pPrev;
}
HB_MUTEX, * PHB_MUTEX;


HB_FUNC( HB_MUTEXWAITERSCOUNT )
{
   PHB_MUTEX  pItem = ( PHB_MUTEX ) hb_param( 1, HB_IT_POINTER );

   if( pItem )
      hb_retni( ( ( PHB_MUTEX ) hb_itemGetPtr( pItem ) )->waiters );
   else
      hb_ret();
}

#pragma enddump
