/*
 * $Id$
 */

// ; Donated to the public domain by
//   Viktor Szakats (harbour syenar.hu)

#include "hbmemory.ch"

FUNCTION Main()

   ? "HB_MEM_CHAR      " , MEMORY( HB_MEM_CHAR       )
   ? "HB_MEM_BLOCK     " , MEMORY( HB_MEM_BLOCK      )
   ? "HB_MEM_RUN       " , MEMORY( HB_MEM_RUN        )
   ? "HB_MEM_VM        " , MEMORY( HB_MEM_VM         )
   ? "HB_MEM_EMS       " , MEMORY( HB_MEM_EMS        )
   ? "HB_MEM_FM        " , MEMORY( HB_MEM_FM         )
   ? "HB_MEM_FMSEGS    " , MEMORY( HB_MEM_FMSEGS     )
   ? "HB_MEM_SWAP      " , MEMORY( HB_MEM_SWAP       )
   ? "HB_MEM_CONV      " , MEMORY( HB_MEM_CONV       )
   ? "HB_MEM_EMSUSED   " , MEMORY( HB_MEM_EMSUSED    )
   ? "HB_MEM_USED      " , MEMORY( HB_MEM_USED       )
   ? "HB_MEM_USEDMAX   " , MEMORY( HB_MEM_USEDMAX    )
   ? "HB_MEM_STACKITEMS" , MEMORY( HB_MEM_STACKITEMS )
   ? "HB_MEM_STACK     " , MEMORY( HB_MEM_STACK      )

   RETURN NIL
