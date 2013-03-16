// ; Donated to the public domain by
//   Viktor Szakats (harbour syenar.net)

#include "hbmemory.ch"

PROCEDURE Main()

   ? "HB_MEM_CHAR      ", Memory( HB_MEM_CHAR       )
   ? "HB_MEM_BLOCK     ", Memory( HB_MEM_BLOCK      )
   ? "HB_MEM_RUN       ", Memory( HB_MEM_RUN        )
   ? "HB_MEM_VM        ", Memory( HB_MEM_VM         )
   ? "HB_MEM_EMS       ", Memory( HB_MEM_EMS        )
   ? "HB_MEM_FM        ", Memory( HB_MEM_FM         )
   ? "HB_MEM_FMSEGS    ", Memory( HB_MEM_FMSEGS     )
   ? "HB_MEM_SWAP      ", Memory( HB_MEM_SWAP       )
   ? "HB_MEM_CONV      ", Memory( HB_MEM_CONV       )
   ? "HB_MEM_EMSUSED   ", Memory( HB_MEM_EMSUSED    )
   ? "HB_MEM_USED      ", Memory( HB_MEM_USED       )
   ? "HB_MEM_USEDMAX   ", Memory( HB_MEM_USEDMAX    )
   ? "HB_MEM_STACKITEMS", Memory( HB_MEM_STACKITEMS )
   ? "HB_MEM_STACK     ", Memory( HB_MEM_STACK      )

   RETURN
