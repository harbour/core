/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strdiff.c
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
 * Revision 1.1  1999/06/02 06:49:39  ajahja
 * Adding GT Library
 *
 *
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_STRDIFF()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Return a string where it begins to differ from another
 *  $SYNTAX$
 *      GT_StrDiff(<cStr1>, <cStr2>) --> cRet
 *  $ARGUMENTS$
 *      <cStr1>  - A character string to compare
 *      <cStr2>  - The string to compare with
 *  $RETURNS$
 *      cRet     - A string beginning at the position in <cStr2> where
 *                 <cStr1> begins to differ from <cStr1>
 *  $DESCRIPTION$
 *      Return a string beginning at the position in <cStr2> where
 *      <cStr1> begins to differ from <cStr1>. If the two strings are
 *      identical (or identical upto the last character in <cStr2>)
 *      the function will return "".
 *
 *      NOTE:
 *         invalid parameters will return ""
 *  $EXAMPLES$
 *      ? gt_strDiff("the cat", "the rat")          // prints "rat"
 *      ? gt_strDiff("the cat", "the ")             // prints ""
 *
 *  $END$
 */

#include <extend.h>

HARBOUR
GT_strdiff()
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
      _retc((char *) NULL);
    else
      _retc(s2);
  } else {
    _ret();                         // parameter mismatch - error return NIL
  }
}
