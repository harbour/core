/*
 * $Id$
 */

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

#include "extend.h"

HARBOUR HB_GT_ASCIISUM( void )
{
	char *str;
   int  len, i;
	long ascSum = 0;

	str = hb_parc(1);
   len = hb_parclen(1);

   for (i = 0; i <= len; i++,str++) {
      ascSum += *str;
	}

   hb_retnl(ascSum);
}
