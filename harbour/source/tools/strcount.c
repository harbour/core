/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strcount.c
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
 * Revision 1.3  1999/06/12 00:21:57  gdiet
 * ChangeLogTag:Fri Jun 11 19:14:22 1999  Gonzalo A. Diethelm  <Gonzalo.Diethelm@jda.cl>
 *
 * Revision 1.2  1999/06/09 18:06:05  dholm
 * See ChangeLog entry 19990609-12:55 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.1  1999/06/02 06:49:39  ajahja
 * Adding GT Library
 *
 *
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_STRCOUNT()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Count the number of times a substring appears in a string
 *  $SYNTAX$
 *      GT_StrCount(<cChrs>, <cStr>) --> nFreq
 *  $ARGUMENTS$
 *      <cChrs> - The substring to find the frequence of
 *      <cStr>  - The string in which to find the character
 *  $RETURNS$
 *      nFreq   - The number of times <cChrs> occurs in <cStr>
 *  $DESCRIPTION$
 *      GT_StrCount() counts how many times a specified substring
 *      appears in a string.
 *      If the substring does NOT appear in <cStr> this function
 *      will return 0.
 *      If the substring is a single character use GT_ChrCount() as
 *      it will be faster.
 *
 *      NOTE:
 *         invalid parameters will return -1
 *  $EXAMPLES$
 *
 *      ? GT_StrCount("the", "the cat sat on the mat")      // prints 2
 *
 *  $END$
 */

#include <extend.h>

HARBOUR HB_GT_STRCOUNT( void )
{
  char *s1, *s2;
  int count, p1, p2, l1, l2;
  int match = 1;

  if (ISCHAR(1) && ISCHAR(2)) {
    s1  = _parc(1);
    s2  = _parc(2);
    l1  = _parclen(1);
    l2  = _parclen(2);

    /* loop through s2 matching passed character (s1) with
       each character of s1 */

    for (count = 0, p2 = 0; p2 <= (l2 - l1); p2++) {
      match = 1;

      for (p1 = 0; p1 < l1; p1++) {
        if (s1[p1] != s2[p2 + p1])
          match = 0;
      }
      if (match)
        count++;
    }

    _retni(count);                  // return result
  } else {
    _retni(-1);                     // parameter mismatch - error -1
  }
}
