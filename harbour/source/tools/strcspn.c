/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strcspn.c
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
 *      GT_STRCSPN()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Return length of prefix in string of chars NOT in set.
 *  $SYNTAX$
 *      GT_strcspn(<cString>, <cSet>) --> nLength
 *  $ARGUMENTS$
 *      <cString> - The string to find the prefix in
 *      <cSet>    - The set of characters
 *  $RETURNS$
 *      nLength   - The length of a string upto a character in the set
 *  $DESCRIPTION$
 *      Return the number of characters in the leading segment of a
 *      string that consists solely of characters NOT in the set.
 *  $EXAMPLES$
 *
 *      ? GT_strcspn("this is a test", "as ")      // prints 3
 *      ? GT_strcspn("this is a test", "elnjpq")   // prints 11
 *
 *  $END$
 */


#include <extend.h>

HARBOUR HB_GT_STRCSPN( void )
{
  char *string;
  char *cset;
  int  l1, l2;
  int  p1, p2;

  if (ISCHAR(1) && ISCHAR(2)) {
    string = hb_parc(1);
    cset   = hb_parc(2);
    l1     = hb_parclen(1);
    l2     = hb_parclen(2);

    for (p1 = 0; p1 < l1; ++p1) {
      for (p2 = 0; (p2 < l2) && (string[p1] != cset[p2]); ++p2)
         ;

      if (p2 < l2)
         break;
    }
    hb_retni(p1);
  } else {
    hb_retni(-1);                     /* parameter mismatch - error -1 */
  }
}

