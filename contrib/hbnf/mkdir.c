/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.2   15 Aug 1991 23:06:58   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.1   14 Jun 1991 19:54:44   GLENN
 *  Minor edit to file header
 *
 *     Rev 1.0   01 Apr 1991 01:03:32   GLENN
 *  Nanforum Toolkit
 *
 *
 */

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( FT_MKDIR )
{
   hb_retl( HB_ISCHAR( 1 ) && hb_fsMkDir( hb_parc( 1 ) ) );
}
