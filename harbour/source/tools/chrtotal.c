/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: chrtotal.c
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
 * Revision 1.3  1999/06/12 00:21:56  gdiet
 * ChangeLogTag:Fri Jun 11 19:14:22 1999  Gonzalo A. Diethelm  <Gonzalo.Diethelm@jda.cl>
 *
 * Revision 1.2  1999/06/09 18:06:04  dholm
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
 *      GT_CHRTOTAL()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Find number of times a set of characters appears in a string
 *  $SYNTAX$
 *      GT_ChrTotal(<cChrs>, <cStr>) --> nTotOcc
 *  $ARGUMENTS$
 *      <cChrs> - The set of characters
 *      <cStr>  - The string to search
 *  $RETURNS$
 *      nTotOcc - The number of times the characters specified in
 *                <cChrs> appears in <cStr>
 *  $DESCRIPTION$
 *      Returns the numnber of occurrences of characters belonging
 *      to the set <cChrs> in the string <cStr>.  If no characters
 *      in <cChrs> appears in <cStr> GT_ChrTotal() will return 0.
 *
 *      NOTE:
 *         invalid parameters will return -1
 *  $EXAMPLES$
 *
 *       local cStr1 := "the cat sat on the mat"
 *
 *       ? GT_ChrTotal("tae", cStr1)            // prints 10
 *       ? GT_ChrTotal("zqw", cStr1)            // prints  0
 *  $END$
 */

#include <extend.h>

HARBOUR HB_GT_CHRTOTAL( void )
{
  char *s1, *s2;
  int count, p1, p2, l2, l1;

  if (ISCHAR(1) && ISCHAR(2)) {
    s1  = _parc(1);
    s2  = _parc(2);
    l2  = _parclen(2);
    l1  = _parclen(1);

    for (count = 0, p2 = 0; p2 < l2; p2++)
      for (p1 = 0; p1 < l1; p1++)
        if (s1[p1] == s2[p2])
          count++;                    // increment counter

    _retni(count);                  // return result
  } else {
    _retni(-1);                     // parameter mismatch - error -1
  }
}
