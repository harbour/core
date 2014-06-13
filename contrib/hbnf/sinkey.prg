/*
 * This is an original work by Greg Lief and is placed in the
 * public domain.
 *
 * Modification history:
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

/* NOTE: Harbour accepts a second parameter, like Inkey() */
FUNCTION ft_SInkey( ... )

   LOCAL nKey, cBlock

   IF ( cBlock := SetKey( nKey := Inkey( ... ) ) ) != NIL
      /* Run the codeblock associated with this key and pass it the
         name of the previous procedure and the previous line number */
      Eval( cBlock, ProcName( 1 ), ProcLine( 1 ), NIL )
   ENDIF

   RETURN nKey
