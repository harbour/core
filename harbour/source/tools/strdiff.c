/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strdiff.c
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

#include "extend.h"

HARBOUR HB_GT_STRDIFF( void )
{
  char *s1, *s2;
  int pos, len;

  if (ISCHAR(1) && ISCHAR(2)) {
    s1  = hb_parc(1);
    s2  = hb_parc(2);
    len = hb_parclen(2);

    /*
       loop through comparing both strings

       NOTE: pos starts at 1, so as to return a string index
             for CLIPPER
    */

    for (pos = 1; (pos <= len) && (*s1 == *s2); s2++, s1++)
      pos++;

    if (pos > len)                  /* strings match exactly!!! */
      hb_retc((char *) NULL);
    else
      hb_retc(s2);
  } else {
    hb_ret();                         /* parameter mismatch - error return NIL */
  }
}
