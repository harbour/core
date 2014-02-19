/*
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

FUNCTION ft_SInkey( waittime )

   LOCAL nKey, cBlock

   DO CASE
   CASE PCount() == 0 /* if no waittime passed, go straight through */
      nKey := Inkey()
      /* dig this... if you pass Inkey( NIL ), it is identical to Inkey( 0 )!
         therefore, I allow you to pass ft_SInkey( NIL ) -- hence this mild bit
         of convolution */
   CASE waittime == NIL .AND. PCount() == 1
      nKey := Inkey( 0 )
   OTHERWISE
      nKey := Inkey( waittime )
   ENDCASE

   IF ( cBlock := SetKey( nKey ) ) != NIL
      /* run the code block associated with this key and pass it the
         name of the previous procedure and the previous line number */
      Eval( cBlock, ProcName( 1 ), ProcLine( 1 ), NIL )
   ENDIF

   RETURN nKey
