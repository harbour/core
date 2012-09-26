/*
 * $Id$
 */

/*
 * File......: bytenot.prg
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:00   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   10 May 1991 23:54:40   GLENN
 * Documentation correction.  The "oneliner" said two characters were NOTted,
 * but this function just takes one byte.
 *
 *    Rev 1.0   01 Apr 1991 01:00:54   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BYTENOT( cByte )

   LOCAL nCounter, cNewByte

   IF HB_ISSTRING( cByte )
      cNewByte := Chr( 0 )
      FOR nCounter := 0 TO 7           // test each bit position
         IF ! FT_ISBIT( cByte, nCounter )
            cNewByte := FT_BITSET( cNewByte, nCounter )
         ENDIF
      NEXT
   ELSE
      cNewByte := NIL
   ENDIF

   RETURN cNewByte
