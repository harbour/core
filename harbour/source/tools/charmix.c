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
 *
 * Modification history:
 * ---------------------
 *
 * $Log$
 * Revision 1.4  1999/06/12 00:21:55  gdiet
 * ChangeLogTag:Fri Jun 11 19:14:22 1999  Gonzalo A. Diethelm  <Gonzalo.Diethelm@jda.cl>
 *
 * Revision 1.3  1999/06/09 18:06:04  dholm
 * See ChangeLog entry 19990609-12:55 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.2  1999/06/02 19:53:28  dholm
 * See ChangeLog entry 19990602-14:50 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.1  1999/06/02 06:49:38  ajahja
 * Adding GT Library
 *
 *
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
    s1  = _parc(1);
    s2  = _parc(2);
    l1  = _parclen(1);
    l2  = _parclen(2);
    pos = 0;

    s3  = (char*)_xgrab(l1 + l2);   // grab us some mem to work with

    for (i = 0; i < l1; i++) {
      s3[pos++] = s1[i];

      if (i < l2)
        s3[pos++] = s2[i];
    }

    if (l2 > l1)
      for (; i < l2; i++)
        s3[pos++] = s2[i];

    s3[pos] = '\0';
    _retc(s3);
    _xfree(s3);                     // free alloc'ed mem
  } else {
    _retc((char *) NULL);           // parameter mismatch - error NullStr
  }
}
