/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 */

#require "hbssl"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cString
   LOCAL bio
   LOCAL bioe

   SSL_init()

   ? ERR_load_PEM_strings()
   ? OpenSSL_add_all_algorithms()

   bioe := BIO_new_fd( 1, HB_BIO_NOCLOSE )

   ? PEM_READ_BIO_RSAPRIVATEKEY( "privkey.pem", {| lWrite | Output(  "Callback (block)", lWrite, hb_eol() ), "test" } )
   ? ; ERR_print_errors( bioe )
   ? PEM_READ_BIO_RSAPRIVATEKEY( "privkey.pem", @cb_function() )
   ? ; ERR_print_errors( bioe )
   ? PEM_READ_BIO_RSAPRIVATEKEY( "privkey.pem", "test" )
   ? ; ERR_print_errors( bioe )
   ? PEM_READ_BIO_RSAPUBLICKEY( "privkey.pem", {| lWrite | Output(  "Callback (block)", lWrite, hb_eol() ), "test" } )
   ? ; ERR_print_errors( bioe )
   ? PEM_READ_BIO_RSAPUBLICKEY( "privkey.pem", "test" )
   ? ; ERR_print_errors( bioe )

#pragma __cstream|cString:=%s
-----BEGIN RSA PRIVATE KEY-----
Proc-Type: 4,ENCRYPTED
DEK-Info: DES-EDE3-CBC,7EDF8C06409FC8D1

w2i5vWLCOvrExPC4+FMwGQBwxXdGE7FY5Jgr6UstEN+b6l7UVSdRXEkT/Ng6RDZF
jViFPKBSrTAzEnvyNesqPBZMwRPKSEZSj+XcS/dHoz7hrbFTNBzKmDL8CJ67k2Lw
4UwtTtmTkU6L++NTfP6ImvxfaQYCkFK9D42qB7pzAAA27aGZMJCotECqVrekeCed
Tx7EMa7Vf2ToGTsvHBphy7Nwe/8Kijdb0wQIj0ZQWGa2vkYjICauAz9vfOv1xaXe
+H90c6xhVqRCv0uum3pGkqsrdJm0mBnr1gstFEDI+S1Lwr80WWBSQjCBsytfxc8j
dNXG8qXm59/n01vfHaZTtStd6mVa+eJwNRMHP0pisoxCTBPTtF/LOaZvOlbYB+r4
Hbxs5Bp0+YZp9RIipA3uagvtcTk7oHzx6v72amd3egli/DL1OY/ZjvVzHe9/dh6K
LZE2mpAHXtnHe8Rlg4CSPMvyFgN2OZXfbc64FjZcglldIoLwhr7kuLzb0zWv8sOz
jOO+uKNzyjDl5R2ay9YCANUpWVGOrpRrU6C/TzbltcxyEVKk8riKAKCsYf+De3Ee
SwPgouYGV8RgfzAwhwesibA1By1cLS/alCESH+9P2R39VHPB7MzjLc8FyNe3xAQJ
VGue9TIkMgy7RW1VFWLcLsCArTEAl83bv+BQ+YaPp9aLNq8bL5vfU2od0R7LXIOe
jH09fWcojNNLfmZU0Jzy7viUiScTtNqpqoH0qPI1hkisvELqXKhW1Lpkr56Ij8IL
B0NDIZKbaPJHHPb9Ne7nQECzv0/kzmAley9UMTZ1M7fq6KYemR0LsA==
-----END RSA PRIVATE KEY-----
#pragma __endtext

   ? PEM_READ_BIO_RSAPRIVATEKEY( bio := BIO_new_mem_buf( cString ), {| lWrite | QOut( "Callback", lWrite, hb_eol() ), "test" } )
   ? ; ERR_print_errors( bioe )
   BIO_free( bio )
   ? PEM_READ_BIO_RSAPRIVATEKEY( bio := BIO_new_mem_buf( cString ), "test" )
   ? ; ERR_print_errors( bioe )
   BIO_free( bio )
   ? PEM_READ_BIO_RSAPRIVATEKEY( bio := BIO_new_mem_buf( cString ), "<wrong>" )
   ? ; ERR_print_errors( bioe )
   BIO_free( bio )

   BIO_free( bioe )

   RETURN

STATIC FUNCTION cb_function( lWrite )

   ? "Callback (func)", lWrite

   RETURN "test"

STATIC FUNCTION Output( ... )

   ? ...

   RETURN NIL
