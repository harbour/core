/*
 * $Id$
 */

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
