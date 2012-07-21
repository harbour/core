/*
 * $Id$
 */
/*
  Test sx_Encrypt()/sx_Decrypt()
*/

#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cname := "Harbour Power"
   LOCAL c

   ?
   ? 'local cname := "Harbour Power"'
   ? 'local c'
   ?
   ? 'sx_encrypt( cname, "password" ) = ', c := sx_encrypt( cname, "password" )
   ? 'sx_decrypt( c, "password" )     = ', sx_decrypt( c, "password" )
   ?
   ? 'sx_decrypt( c, "wrongpass" )    = ', sx_decrypt( c, "wrongpass" )
