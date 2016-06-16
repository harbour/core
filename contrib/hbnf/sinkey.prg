/* This is an original work by Greg Lief and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:06:10   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:53:02   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:02:18   GLENN
   Nanforum Toolkit
 */

#include "inkey.ch"

FUNCTION ft_SInkey( nTimeOut, nMask )  // HB_EXTENSION: Harbour accepts a 2nd parameter, like Inkey()

   LOCAL nKey, nKeyStd, bBlock

   SWITCH PCount()
   CASE 0    ; nKey := Inkey(, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) ; EXIT
   CASE 1    ; nKey := Inkey( nTimeOut, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) ; EXIT
   OTHERWISE ; nKey := Inkey( nTimeOut, hb_bitOr( hb_defaultValue( nMask, Set( _SET_EVENTMASK ) ), HB_INKEY_EXT ) )
   ENDSWITCH

   nKeyStd := hb_keyStd( nKey )

   IF ( bBlock := SetKey( nKey ) ) != NIL .OR. ;
      ( bBlock := SetKey( nKeyStd ) ) != NIL
      // Run the codeblock associated with this key and pass it the
      // name of the previous procedure and the previous line number
      Eval( bBlock, ProcName( 1 ), ProcLine( 1 ), NIL )
   ENDIF

   RETURN nKeyStd
