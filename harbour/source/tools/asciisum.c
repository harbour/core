/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: asciisum.c
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Net/Node..: 050/069
 * User Name.: Andy Leighton
 * Date......: $Date$
 * Revision..: $Revision$
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 * $Log$
 * Revision 1.2  1999/06/09 18:06:03  dholm
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
 *      GT_ASCIISUM()
 *  $CATEGORY$
 *      String
 *  $ONELINER$
 *      Sum the ascii values in a string.
 *  $SYNTAX$
 *      GT_AsciiSum(<cStr>) --> nSum
 *  $ARGUMENTS$
 *      <cStr>  - The string to sum
 *  $RETURNS$
 *      nSum    - The sum of all ascii values in <cStr>.
 *  $DESCRIPTION$
 *      Sum the ascii value of every character in the passed string
 *      and return the result.
 *  $EXAMPLES$
 *  $END$
 */

#include <extend.h>

HARBOUR GT_ASCIISUM( void )
{
	char *str;
   int  len, i;
	long ascSum = 0;

	str = _parc(1);
   len = _parclen(1);

   for (i = 0; i <= len; i++,str++) {
      ascSum += *str;
	}

   _retnl(ascSum);
}
