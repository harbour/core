/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strpbrk.c
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
 * Revision 1.2  1999/06/09 18:06:06  dholm
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
 *      GT_STRPBRK()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Return string after 1st char from a set
 *  $SYNTAX$
 *      GT_StrpBrk(<cStr>, <cSet>) --> cString
 *  $ARGUMENTS$
 *      <cStr>   - The input string
 *      <cSet>   - The set of characters to find
 *  $RETURNS$
 *      cString  - The input string after the first occurance of any
 *                 character from <cSet>
 *  $DESCRIPTION$
 *      Return a string after the first occurance of any character from
 *      the input set <cSet>.
 *  $EXAMPLES$
 *
 *      ? GT_Strpbrk("This is a test", "sa ")  // prints "s is a test"
 *      ? GT_Strpbrk("This is a test", "et")   // prints "test"
 *
 *  $END$
 */


#include <extend.h>


HARBOUR HB_GT_STRPBRK( void )
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

    do {
      for (p2 = 0; (p2 < l2) && (cset[p2] != string[p1]); ++p2)
         ;
      if (p2 < l2) {
         _retc(string + p1);
         break;
      }
    } while (p1++ < l1);

    if (p2 >= l2)
      _retc((char *) NULL);

  } else {
    _retc((char *) NULL);               // parameter mismatch - error NullStr
  }
}

