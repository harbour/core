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
 * Revision 1.4  1999/06/17 07:09:25  dholm
 * See ChangeLog entry 19990617-02:00 EDT David G. Holm <dholm@jsd-llc.com>
 *
 * Revision 1.3  1999/06/12 00:21:56  gdiet
 * ChangeLogTag:Fri Jun 11 19:14:22 1999  Gonzalo A. Diethelm  <Gonzalo.Diethelm@jda.cl>
 *
 * Revision 1.2  1999/06/09 18:06:04  dholm
 * See ChangeLog entry 19990609-12:55 EDT David G. Holm <dholm@jsd-llc.com>
 *
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

HARBOUR HB_GT_CHRFIRST( void )
{
  char *string;
  char *cset;
  int l1, l2;
  int p1, p2;

  if (ISCHAR(1) && ISCHAR(2)) {
    string = hb_parc(2);
    cset   = hb_parc(1);
    l1     = hb_parclen(2);
    l2     = hb_parclen(1);
    p1     = p2 = 0;

    do {
      for (p2 = 0; (p2 < l2) && (cset[p2] != string[p1]); ++p2)
         ;
      if (p2 < l2) {
         hb_retni(string[p1]);
         break;
      }
    } while (p1++ < l1);

    if (p2 >= l2)
      hb_retni(0);

  } else {
    hb_retni(-1);               // parameter mismatch - error NullStr
  }
}
