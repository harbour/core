/*
 * File......: SINKEY.PRG
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SINKEY()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Replacement for INKEY() that tests for SET KEY procedures
 *  $SYNTAX$
 *     FT_SINKEY( [ <nWaitTime> ] ) -> nKey
 *  $ARGUMENTS$
 *     <nWaitTime> is the number of seconds to wait.  If zero,
 *     FT_SINKEY() will wait indefinitely for a keypress.  If not
 *     passed, FT_SINKEY() does not wait for a keypress.  If NIL,
 *     it is treated the same as 0.
 *  $RETURNS$
 *     The INKEY() value of the key pressed.
 *  $DESCRIPTION$
 *     FT_SINKEY() is similar to the function provided by Nantucket in
 *     KEYBOARD.PRG, with one significant difference: you can pass NIL
 *     to INKEY(), which will be treated as a zero (i.e., wait indefinitely
 *     for keypress).  Therefore, it is necessary to differentiate between
 *     an explicit NIL and one that is a result of a formal parameter NOT
 *     being received.
 *
 *     FT_SINKEY() differs from the standard INKEY() in that it will
 *     respond to any keys set with SET KEY TO or SetKey().
 *  $EXAMPLES$
 *     SetKey( K_F1, {|n,l,r| Help(n,l,r) } )
 *     nKey := FT_SINKEY(0)       // Help() will be called if F1 pressed
 *  $END$
 */



FUNCTION FT_SINKEY(waittime)
  LOCAL key, cblock

  DO CASE

     /* if no WAITTIME passed, go straight through */
     CASE pcount() == 0
        key := inkey()

     /* dig this... if you pass inkey(NIL), it is identical to INKEY(0)!
        therefore, I allow you to pass FT_SINKEY(NIL) -- hence this mild bit
        of convolution */

     CASE waittime == NIL .AND. Pcount() == 1
        key := inkey(0)

     OTHERWISE
        key := inkey(waittime)

  ENDCASE

  cblock := Setkey(key)

  IF cblock != NIL

     // run the code block associated with this key and pass it the
     // name of the previous procedure and the previous line number

     Eval(cblock, Procname(1), Procline(1), NIL)

  ENDIF

RETURN key
