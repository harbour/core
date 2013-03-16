/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.2   15 Aug 1991 23:07:12   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.1   14 Jun 1991 19:54:58   GLENN
 *  Minor edit to file header
 *
 *     Rev 1.0   01 Apr 1991 01:03:52   GLENN
 *  Nanforum Toolkit
 *
 */

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( FT_RMDIR )
{
   hb_retl( HB_ISCHAR( 1 ) && hb_fsRmDir( hb_parc( 1 ) ) );
}
