/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: asciisum.c
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Net/Node..: 050/069
 * User Name.: Andy Leighton
 * Date......: $Date$
 * Revision..: $Revision$
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_ASCIISUM )
{
	char *str;
   int  len, i;
	long ascSum = 0;

	str = hb_parc(1);
   len = hb_parclen(1);

   for (i = 0; i <= len; i++,str++) {
      ascSum += *str;
	}

   hb_retnl(ascSum);
}
