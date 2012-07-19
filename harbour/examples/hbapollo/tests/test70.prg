/*
 * $Id$
 */
/*
   sx_DBFEncrypt(), sx_DbfDecrypt()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   USE "test/test" ALIAS TESTME
   sx_Copyfile( "newtest" )    // Copy DBF to newtest
   USE "newtest" EXCLUSIVE     // Open Exclusive
   sx_DBFEncrypt( "harbour" )  // Encrypt with password
   sx_SetPassword()            // Reset password to test encryption
   BROWSE
   sx_DBFDecrypt( "harbour" )  // Decrypt DBF with password
   BROWSE
