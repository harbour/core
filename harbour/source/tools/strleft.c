/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strleft.c
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
 * Revision 1.1  1999/06/02 06:49:40  ajahja
 * Adding GT Library
 *
 *
 */

/*
 *  $DOC$
 *  $FUNCNAME$
 *      GT_STRLEFT()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Find length of prefix of a string
 *  $SYNTAX$
 *      GT_StrLeft(<cStr>, <cChars>) --> nLen
 *  $ARGUMENTS$
 *      <cStr>   - The input string
 *      <cChars> - The set of characters to find
 *  $RETURNS$
 *      nLen     - The length of the prefix found.
 *  $DESCRIPTION$
 *      Return the length of the leading segment in the passed string
 *      <cStr> that consists solely of the characters in the character
 *      set <cChars>.
 *
 *      If no characters in the the search set are found, the function
 *      shall return 0
 *  $EXAMPLES$
 *
 *      ? GT_StrLeft("this is a test", "hsit ")       // prints 8
 *      ? GT_StrLeft("this is a test", "hit a")       // prints 3
 *      ? GT_StrLeft("this is a test", "zxy")         // prints 0
 *
 *  $END$
 */


#include <extend.h>


HARBOUR HB_GT_STRLEFT( void )
{
  char *string;
  char *cset;
  int l1, l2;
  int p1, p2;

  if (ISCHAR(1) && ISCHAR(2)) {
    string = _parc(1);
    cset   = _parc(2);
    l1     = _parclen(1);
    l2     = _parclen(2);
    p1     = p2 = 0;

    for (p1 = 0; p1 < l1; p1++) {
      for (p2 = 0; p2 < l2 && cset[p2] != string[p1]; p2++)
         ;

      if (p2 == l2)
         break;
    }
    _retni(p1);

  } else {
    _retni(-1);               // parameter mismatch - error NullStr
  }
}

