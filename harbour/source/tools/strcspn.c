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
 *
 * Modification history:
 * ---------------------
 *
 * $Log$
 * Revision 1.3  1999/06/12 00:21:58  gdiet
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
    string = _parc(1);
    cset   = _parc(2);
    l1     = _parclen(1);
    l2     = _parclen(2);

    for (p1 = 0; p1 < l1; ++p1) {
      for (p2 = 0; (p2 < l2) && (string[p1] != cset[p2]); ++p2)
         ;

      if (p2 < l2)
         break;
    }
    _retni(p1);
  } else {
    _retni(-1);                     // parameter mismatch - error -1
  }
}

