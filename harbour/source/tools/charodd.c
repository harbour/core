/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: charodd.c
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

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_CHARODD()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Return a string of all the characters in odd positions
 *  $SYNTAX$
 *      GT_CharOdd(<cStr>) --> cRet
 *  $ARGUMENTS$
 *      <cStr>   - A character string to extract chars from
 *  $RETURNS$
 *      cRet     - A string of all the chars in odd positions
 *  $DESCRIPTION$
 *      Return a string consisting of all the characters in odd
 *      positions in <cStr1>.
 *
 *      NOTE:
 *         invalid parameters will return ""
 *  $EXAMPLES$
 *
 *      ? gt_CharOdd("abcdefghijklm")             // prints "acegikm"
 *
 *  $END$
 */

#include "extend.h"

HARBOUR HB_GT_CHARODD( void )
{
  char *s1, *s2;
  int len, i;

  if (ISCHAR(1)) {
    s1  = hb_parc(1);
    len = hb_parclen(1);

    s2  = (char *)hb_xgrab(len / 2);  /* grab us some mem to work with */

    for (i = 0; i <= len; i += 2)
      s2[i/2] = s1[i] & 0x7f;

    hb_retclen(s2, len);
    hb_xfree(s2);                     /* free alloc'ed mem */
  } else {
    hb_retc((char *) NULL);           /* parameter mismatch - error NullStr */
  }
}
