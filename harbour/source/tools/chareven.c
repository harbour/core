/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: chareven.c
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
 * Revision 1.3  1999/06/09 18:06:03  dholm
 * See ChangeLog entry 19990609-12:55 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.2  1999/06/02 19:53:28  dholm
 * See ChangeLog entry 19990602-14:50 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.1  1999/06/02 06:32:42  ajahja
 * Adding GT Library
 *
 *
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_CHAREVEN()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Return a string of all the characters in even positions
 *  $SYNTAX$
 *      GT_CharEven(<cStr>) --> cRet
 *  $ARGUMENTS$
 *      <cStr>   - A character string to extract chars from
 *  $RETURNS$
 *      cRet     - A string of all the chars in even positions
 *  $DESCRIPTION$
 *      Return a string consisting of all the characters in even
 *      positions in <cStr1>.
 *
 *      NOTE:
 *         invalid parameters will return ""
 *  $EXAMPLES$
 *
 *      ? gt_CharEven("abcdefghijklm")             // prints "bdfhjl"
 *
 *  $END$
 */

#include <extend.h>

HARBOUR HB_GT_CHAREVEN( void )
{
  char *s1, *s2;
  int len, i;

  if (ISCHAR(1)) {
    s1  = _parc(1);
    len = _parclen(1);

    s2  = (char *)_xgrab(len / 2);  // grab us some mem to work with

    for (i = 1; i <= len; i += 2)
      s2[(i - 1)/2] = s1[i] & 0x7f;

    _retc(s2);
    _xfree(s2);                     // free alloc'ed mem
  } else {
    _retc((char *) NULL);           // parameter mismatch - error NullStr
  }
}
