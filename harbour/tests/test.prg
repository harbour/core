#include "setcurs.ch"

#define ALTD_DISABLE   0
#define ALTD_ENABLE    1

static s_oDebugger
static s_lExit := .F.

function AltD( nAction )
   static s_lEnabled := .t.

   do case
      case nAction == nil
           if s_lEnabled
              s_lExit := .f.
              __dbgEntry( ProcLine( 2 ) )
           endif

      case nAction == ALTD_DISABLE
           s_lEnabled := .f.

      case nAction == ALTD_ENABLE
           s_lEnabled := .t.
   endcase

return nil
