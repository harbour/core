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
 *
 * Modification history:
 * ---------------------
 *
 * $Log$
 * Revision 1.4  1999/06/17 07:09:24  dholm
 * See ChangeLog entry 19990617-02:00 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.3  1999/06/12 00:21:55  gdiet
 * ChangeLogTag:Fri Jun 11 19:14:22 1999  Gonzalo A. Diethelm  <Gonzalo.Diethelm@jda.cl>
 *
 * Revision 1.2  1999/06/09 18:06:03  dholm
 * See ChangeLog entry 19990609-12:55 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.1  1999/06/02 06:21:33  ajahja
 * Adding GT Library
 *
 *
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
 *       ? gt_ascpos("the cat sat on the mat", 3)
 *                                   // prints e
 *  $END$
 */

#include <extend.h>

HARBOUR HB_GT_ASCPOS( void )
{
  char *s;
  int  p;

  if (ISCHAR(1) && ISNUM(2)) {
    s = hb_parc(1);
    p = hb_parni(2);
    p--;                            // decrement p to adjust for c strings
                                    // starting at position 0

    if (p > hb_parclen(1))            // oh oh p > length of passed string
      hb_retni(-2);                   // error -2
    else
      hb_retni((int) s[p]);           // return ascii code of appropriate
                                    // character in string
  } else {
    hb_retni(-1);                     // parameter mismatch - error -1
  }
}
