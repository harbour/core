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

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_CHARMIX()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Amalgamate two strings to form the return value
 *  $SYNTAX$
 *      GT_CharMix(<cStr1>, <cStr2>) --> cRet
 *  $ARGUMENTS$
 *      <cStr1>  - A character string to mix
 *      <cStr2>  - A character string to mix with
 *  $RETURNS$
 *      cRet     - A string consisting of all the characters in <cStr1>
 *                 mixed with all the characters in <cStr2>
 *  $DESCRIPTION$
 *      Return a string consisting of all the characters in <cStr1>
 *      mixed with the characters from <cStr2>.
 *
 *      NOTE:
 *         invalid parameters will return ""
 *  $EXAMPLES$
 *
 *      ? gt_CharMix("abc", "123")               // prints "a1b2c3"
 *      ? gt_CharMix("abcde", "123")             // prints "a1b2c3de"
 *      ? gt_CharMix("abc", "12345")             // prints "a1b2c345"
 *
 *  $END$
 */

#include <extend.h>

HARBOUR HB_GT_CHARMIX( void )
{
  char *s1, *s2, *s3;
  int l1, l2, i, pos;

  if (ISCHAR(1) && ISCHAR(2)) {
    s1  = hb_parc(1);
    s2  = hb_parc(2);
    l1  = hb_parclen(1);
    l2  = hb_parclen(2);
    pos = 0;

    s3  = (char*)hb_xgrab(l1 + l2);   /* grab us some mem to work with */

    for (i = 0; i < l1; i++) {
      s3[pos++] = s1[i];

      if (i < l2)
        s3[pos++] = s2[i];
    }

    if (l2 > l1)
      for (; i < l2; i++)
        s3[pos++] = s2[i];

    s3[pos] = '\0';
    hb_retc(s3);
    hb_xfree(s3);                     /* free alloc'ed mem */
  } else {
    hb_retc((char *) NULL);           /* parameter mismatch - error NullStr */
  }
}
