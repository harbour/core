/*
 * $Id$
 */
/*
   sx_Encrypt(), sx_Decrypt()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cString := "Harbour Power"
   LOCAL cEncrypted

   ? 'cEncrypted:=sx_Encrypt( cString, "password" )=', cEncrypted := sx_Encrypt( cString, "password" )
   ? 'sx_Decrypt( cEncrypted, "password" )=', sx_Decrypt( cEncrypted, "password" )
   ?
   ? 'cString=', cString
   ? 'cString="Harbour Power"', cString := "Harbour Power"
   ?
   ? 'cEncrypted:=sx_Encrypt( cString, "password" )=', cEncrypted := sx_Encrypt( cString, "password" )
   ? 'sx_Decrypt( cEncrypted, "nopass" )=', sx_Decrypt( cEncrypted, "nopass" )
