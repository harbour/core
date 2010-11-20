/* 
 * $Id$
 */

#include <math.h>

#include "hbapi.h"
#include "hbapierr.h"

#define WRAP( hbfunc, mfunc )                                                                 \
   HB_FUNC( hbfunc )                                                                          \
   {                                                                                          \
      if( HB_ISNUM( 1 ) )                                                                     \
      {                                                                                       \
         double dArg, dResult;                                                                \
         dArg = hb_parnd( 1 );                                                                \
         dResult = mfunc( dArg );                                                             \
         hb_retnd( dResult );                                                                 \
      }                                                                                       \
      else                                                                                    \
         hb_errRT_BASE_SubstR( EG_ARG, 1097, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS ); \
   }

WRAP( SIN, sin )
WRAP( COS, cos )
