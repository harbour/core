/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: ascpos.c
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Net/Node..: 050/069
 * User Name.: Andy Leighton
 * Date......: 23/05/93
 * Revision..: 1.00
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_ASCPOS()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Return the ascii value of a specified character in a string
 *  $SYNTAX$
 *      GT_Ascpos(<cStr>, <nPos>) --> nAscVal
 *  $ARGUMENTS$
 *      <cStr>  - The string
 *      <nPos>  - The position in <cStr>
 *  $RETURNS$
 *      nAscVal - The ascii value of substr(<cStr>, <nPos>, 1)
 *  $DESCRIPTION$
 *      Return the ascii value of a specified character in a string
 *      Equivalent (but much faster) to
 *          asc(substr(cStr, nPos, 1)
 *
 *      NOTE:
 *         invalid parameters will return -1
 *         nPos > len(cStr)   will return -2
 *
 *      This last behaviour is different to the Funcky function of the
 *      same name.  I changed the behaviour because some of the strings
 *      I process contain embedded NULs.
 *  $EXAMPLES$
 *       ? gt_ascpos("the cat sat on the mat", 3) // prints e
 *  $END$
 */

#include "extend.h"

HARBOUR HB_GT_ASCPOS( void )
{
  char *s;
  ULONG p;

  if (ISCHAR(1) && ISNUM(2)) {
    s = hb_parc(1);
    p = hb_parni(2);
    p--;                            /* decrement p to adjust for c strings */
                                    /* starting at position 0 */

    if (p > hb_parclen(1))            /* oh oh p > length of passed string */
      hb_retni(-2);                   /* error -2 */
    else
      hb_retni((int) s[p]);           /* return ascii code of appropriate */
                                    /* character in string */
  } else {
    hb_retni(-1);                     /* parameter mismatch - error -1 */
  }
}
