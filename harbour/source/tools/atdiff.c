/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: atdiff.c
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
 * Revision 1.1  1999/06/02 06:18:45  ajahja
 * Adding GT Library
 *
 *
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_ATDIFF()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Return the position where two strings begin to differ
 *  $SYNTAX$
 *      GT_AtDiff(<cStr1>, <cStr2>) --> nPos
 *  $ARGUMENTS$
 *      <cStr1>  - A character string to compare
 *      <cStr2>  - The string to compare with
 *  $RETURNS$
 *      nPos     - The position in <cStr2> where <cStr1> begins to differ
 *  $DESCRIPTION$
 *      Return the position in <cStr2> where <cStr1> begins to differ.
 *      If the strings differ in the first character GT_AtDiff() will
 *      return 1.  If the two strings are identical (or identical upto
 *      the last character in <cStr2>) the function will return 0.
 *
 *      NOTE:
 *         invalid parameters will return -1
 *  $EXAMPLES$
 *      ? gt_atDiff("the cat", "the rat")          // prints 5
 *      ? gt_atDiff("the cat", "the ")             // prints 0
 *
 *  $END$
 */


#include <extend.h>

HARBOUR
gt_atdiff()
{
  char *s1, *s2;
  int pos, len;

  if (ISCHAR(1) && ISCHAR(2)) {
    s1  = _parc(1);
    s2  = _parc(2);
    len = _parclen(2);

    /*
       loop through comparing both strings

       NOTE: pos starts at 1, so as to return a string index
             for CLIPPER
    */

    for (pos = 1; (pos <= len) && (*s1 == *s2); s2++, s1++)
      pos++;

    if (pos > len)                  // strings match exactly!!!
      _retni(0);
    else
      _retni(pos);
  } else {
    _retni(-1);                     // parameter mismatch - error -1
  }
}
