/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strexpan.c
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
 * Revision 1.3  1999/06/09 18:06:05  dholm
 * See ChangeLog entry 19990609-12:55 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.2  1999/06/02 19:53:28  dholm
 * See ChangeLog entry 19990602-14:50 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.1  1999/06/02 06:49:39  ajahja
 * Adding GT Library
 *
 *
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_STREXPAND()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Insert fillers between characters in a passed string
 *  $SYNTAX$
 *      GT_StrExpand(<cStr>, [<nNum>], [<cChar>]) --> cRet
 *  $ARGUMENTS$
 *      <cStr1>  - A character string to insert chars into
 *      <nNum>   - The number of fill characters to insert (default 1)
 *      <cChar>  - The fill chararacter (default space)
 *  $RETURNS$
 *      cRet     - The input string with fill characters inserted between
 *                 every character in the original.
 *  $DESCRIPTION$
 *      Inserts fill characters into a string.
 *
 *      NOTE:
 *         invalid parameters will return ""
 *  $EXAMPLES$
 *
 *      ? gt_strexpand("abc")                    // prints "a b c"
 *      ? gt_strexpand("abc", 2)                 // prints "a  b  c"
 *      ? gt_strexpand("abc", 2, 'þ')            // prints "aþþbþþc"
 *
 *  $END$
 */


#include <extend.h>

HARBOUR GT_STREXPAND( void )
{
  char *in, *out;
  int  nIns = 1;
  char *insert = " ";
  int  len;
  int  i, j, p;

  if (ISCHAR(1) && (ISNUM(2) || _pcount() < 2) && (ISCHAR(3) || _pcount() < 3)) {
    in  = _parc(1);
    len = _parclen(1);

    if (ISNUM(2))
      nIns = _parni(2);

    if (ISCHAR(3))
      insert = _parc(3);

    out = (char *)_xgrab(len * (nIns + 1));    // alloc us some memory

    for (i = 0, p = 0; i < len; i++) { // loop thru input
      out[p++] = in[i];                // insert a character from input

      if (i < (len - 1)) {             // do not insert fill chars on last
                                       // char of input
         for (j = 1; j <= nIns; j++)   // insert the fill characters
            out[p++] = insert[0];
      }
    }
    out[p] = '\0';                     // Add terminating NUL

    _retc(out);
    _xfree(out);                       // free alloc'ed mem
  } else {
    _retc((char *) NULL);              // parameter mismatch - error NullStr
  }
}

