/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: charmix.c
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Net/Node..: 050/069
 * User Name.: Andy Leighton
 * Date......: 24/05/93
 * Revision..: 1.00
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HARBOUR HB_GT_CHARMIX( void )
{
  char *s1, *s2, *s3;
  int l1, l2, i, pos3;

  if (ISCHAR(1) && ISCHAR(2)) {
    s1  = hb_parc(1);
    s2  = hb_parc(2);
    l1  = hb_parclen(1);
    l2  = hb_parclen(2);
    pos3 = 0;

    s3  = (char*)hb_xgrab(l1 + l2 + 1);   /* grab us some mem to work with */

    for (i = 0; i < l1; i++) {
      s3[pos3++] = s1[i];

      if (i < l2)
        s3[pos3++] = s2[i];
    }

    if (l2 > l1)
      for (; i < l2; i++)
        s3[pos3++] = s2[i];

    s3[pos3] = '\0';
    hb_retclen(s3, l1 + l2);
    hb_xfree(s3);                     /* free alloc'ed mem */
  } else {
    hb_retc((char *) NULL);           /* parameter mismatch - error NullStr */
  }
}
