/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 */

#include "simpleio.ch"

#include "hbssl.ch"

PROCEDURE Main()

   LOCAL ctx
   LOCAL result
   LOCAL encrypted
   LOCAL decrypted

   LOCAL cKey := "key"

   LOCAL a, iv, pub, bioe, tmp

   LOCAL all := {;
       @PEM_READ_BIO_PRIVATEKEY()      ,;
       @PEM_READ_BIO_PUBKEY()          ,; //
       @PEM_READ_BIO_RSAPRIVATEKEY()   ,;
       @PEM_READ_BIO_RSAPUBLICKEY()    ,;
       @PEM_READ_BIO_RSA_PUBKEY()      ,; //
       @PEM_READ_BIO_DSAPRIVATEKEY()   ,;
       @PEM_READ_BIO_DSA_PUBKEY()      ,;
       @PEM_READ_BIO_DSAPARAMS()       ,;
       @PEM_READ_BIO_DHPARAMS()        ,;
       @PEM_READ_BIO_X509()            ,;
       @PEM_READ_BIO_X509_AUX()        ,;
       @PEM_READ_BIO_X509_REQ()        ,;
       @PEM_READ_BIO_X509_CRL()        ,;
       @PEM_READ_BIO_PKCS7()           }

   OpenSSL_add_all_ciphers()

   ctx := hb_EVP_CIPHER_CTX_create()
   EVP_CIPHER_CTX_init( ctx )

   EVP_EncryptInit( ctx, "AES-192-OFB", cKey )
   ? EVP_CIPHER_CTX_cipher( ctx )
   ? EVP_CIPHER_block_size( EVP_CIPHER_CTX_cipher( ctx ) )

   encrypted := ""
   result := ""
   EVP_EncryptUpdate( ctx, @result, "sample text" )
   encrypted += result
   EVP_EncryptFinal( ctx, @result )
   encrypted += result
   ? "ENCRYTPTED", ">" + hb_StrToHex( encrypted ) + "<"
   ? ">" + encrypted + "<"

   EVP_CIPHER_CTX_init( ctx )
   EVP_CIPHER_CTX_cleanup( ctx )

   EVP_DecryptInit( ctx, "AES-192-OFB", cKey )

   decrypted := ""
   result := ""
   EVP_DecryptUpdate( ctx, @result, encrypted )
   decrypted += result
   EVP_DecryptFinal( ctx, @result )
   decrypted += result
   ? "DECRYTPTED", ">" + decrypted + "<"

   ? ERR_load_PEM_strings()
   ? OpenSSL_add_all_algorithms()

   EVP_CIPHER_CTX_init( ctx )
   EVP_CIPHER_CTX_cleanup( ctx )

   ? "()()()()()()()"
// bioe := BIO_new_fd( 1, HB_BIO_NOCLOSE )
// FOR EACH tmp IN all
//    ? tmp:__enumIndex(), tmp:exec( "pubkey.pem", "test" )
//    ? ; ERR_print_errors( bioe )
// NEXT

   ? pub := PEM_READ_BIO_PUBKEY( "pubkey.pem", "test" )

   ? "EVP_SealInit", EVP_SealInit( ctx, "AES-192-OFB", @a, @iv, { pub } )
   ? ValType( a ), Len( a )
   ? ValType( a[ 1 ] ), ">" + hb_StrToHex( a[ 1 ] ) + "<"
   ? ValType( iv ), ">" + hb_StrToHex( iv ) + "<"

// BIO_free( bioe )

   EVP_cleanup()

   RETURN
