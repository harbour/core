/*
 * $Id$
 */

/*
 * File......: sinkey.prg
 * Author....: Greg Lief
 * CIS ID....: 72460,1760
 *
 * This is an original work by Greg Lief and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:06:10   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:02   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:18   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_SINKEY( waittime )

   LOCAL key, cblock

   DO CASE

      /* if no WAITTIME passed, go straight through */
   CASE PCount() == 0
      key := Inkey()

     /* dig this... if you pass inkey(NIL), it is identical to INKEY(0)!
        therefore, I allow you to pass FT_SINKEY(NIL) -- hence this mild bit
        of convolution */

   CASE waittime == NIL .AND. PCount() == 1
      key := Inkey( 0 )

   OTHERWISE
      key := Inkey( waittime )

   ENDCASE

   cblock := SetKey( key )

   IF cblock != NIL

      // run the code block associated with this key and pass it the
      // name of the previous procedure and the previous line number

      Eval( cblock, ProcName( 1 ), ProcLine( 1 ), NIL )

   ENDIF

   RETURN key
