/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: chrcount.c
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
 *      GT_CHRCOUNT()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Count the number of times a character appears in a string
 *  $SYNTAX$
 *      GT_ChrCount(<cChr>, <cStr>) --> nFreq
 *  $ARGUMENTS$
 *      <cChr>  - The character to find the frequence of
 *      <cStr>  - The string in which to find the character
 *  $RETURNS$
 *      nFreq   - The number of times <cChr> occurs in <cStr>
 *  $DESCRIPTION$
 *      GT_ChrCount() counts how many times a specified character
 *      appears in a string.
 *
 *      NOTE:
 *         invalid parameters will return -1
 *  $EXAMPLES$
 *
 *      ? GT_ChrCount("t", "the cat sat on the mat")      // prints 4
 *
 *  $END$
 */

#include <extend.h>

HARBOUR HB_GT_CHRCOUNT( void )
{
  char *s1, *s2;
  int count, pos2, len;

  if (ISCHAR(1) && ISCHAR(2)) {
    s1  = hb_parc(1);
    s2  = hb_parc(2);
    len = hb_parclen(2);

    /* loop through s2 matching passed character (s1) with
       each character of s1 */
    for (count = 0, pos2 = 1; pos2 <= len; s2++, pos2++)
      if (*s1 == *s2)               /* character matches s1 */
        count++;                    /* increment counter */

    hb_retni(count);                  /* return result */
  } else {
    hb_retni(-1);                     /* parameter mismatch - error -1 */
  }
}
