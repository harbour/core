/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: chrfirst.c
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
 * Revision 1.1  1999/06/02 06:49:38  ajahja
 * Adding GT Library
 *
 *
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_CHRFIRST()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Find which character occurs first in a string
 *  $SYNTAX$
 *      GT_ChrFirst(<cChars>, <cStr>) --> nAsc
 *  $ARGUMENTS$
 *      <cChars> - The set of characters to find
 *      <cStr>   - The input string
 *  $RETURNS$
 *      nAsc     - The ASCII value of the first character in <cChars>
 *                 which appears first in <cStr>
 *  $DESCRIPTION$
 *      Return the ascii value of a character in <cChars>
 *      which appears first in <cStr>.
 *  $EXAMPLES$
 *
 *      ? chr(GT_ChrFirst("sa ", "This is a test"))  // prints "s"
 *      ? chr(GT_ChrFirst("et",  "This is a test"))   // prints "t"
 *
 *  $END$
 */

#include <extend.h>

HARBOUR
GT_ChrFirst()
{
  char *string;
  char *cset;
  int l1, l2;
  int p1, p2;

  if (ISCHAR(1) && ISCHAR(2)) {
    string = _parc(2);
    cset   = _parc(1);
    l1     = _parclen(2);
    l2     = _parclen(1);
    p1     = p2 = 0;

    do {
      for (p2 = 0; (p2 < l2) && (cset[p2] != string[p1]); ++p2)
         ;
      if (p2 < l2) {
         _retni(string[p1]);
         break;
      }
    } while (p1++ < l1);

    if (p2 >= l2)
      _retni(0);

  } else {
    _retni(-1);               // parameter mismatch - error NullStr
  }
}
