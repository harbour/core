/*
 * $Id$
 */

/*
 * Author....: Clayton Neff
 * CIS ID....:
 *
 * This is an original work by Clayton Neff and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:08:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:58   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:03:00   GLENN
 * Nanforum Toolkit
 *
 */

#include "hbapi.h"

HB_FUNC( FT_STOD )
{
   hb_retds( hb_parclen( 1 ) >= 8 ? hb_parc( 1 ) : NULL );
}
