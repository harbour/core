/*
 * $Id$
 */

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

HARBOUR HB_GT_STREXPAND( void )
{
  char *in, *out;
  int  nIns = 1;
  char *insert = " ";
  int  len;
  int  i, j, p;

  if (ISCHAR(1) && (ISNUM(2) || hb_pcount() < 2) && (ISCHAR(3) || hb_pcount() < 3)) {
    in  = hb_parc(1);
    len = hb_parclen(1);

    if (ISNUM(2))
      nIns = hb_parni(2);

    if (ISCHAR(3))
      insert = hb_parc(3);

    out = (char *)hb_xgrab(len * (nIns + 1));    /* alloc us some memory */

    for (i = 0, p = 0; i < len; i++) { /* loop thru input */
      out[p++] = in[i];                /* insert a character from input */

      if (i < (len - 1)) {             /* do not insert fill chars on last */
                                       /* char of input */
         for (j = 1; j <= nIns; j++)   /* insert the fill characters */
            out[p++] = insert[0];
      }
    }
    out[p] = '\0';                     /* Add terminating NUL */

    hb_retc(out);
    hb_xfree(out);                       /* free alloc'ed mem */
  } else {
    hb_retc((char *) NULL);              /* parameter mismatch - error NullStr */
  }
}

