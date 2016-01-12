/* Copyright 2009-2016 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

PROCEDURE Main()

   LOCAL ctx
   LOCAL result
   LOCAL encrypted
   LOCAL decrypted

   LOCAL cKey := "key"

   LOCAL a, iv, pub
   LOCAL bioe
   LOCAL tmp

   LOCAL all := {;
       @PEM_READ_BIO_PRIVATEKEY()      , ;
       @PEM_READ_BIO_PUBKEY()          , ; //
       @PEM_READ_BIO_RSAPRIVATEKEY()   , ;
       @PEM_READ_BIO_RSAPUBLICKEY()    , ;
       @PEM_READ_BIO_RSA_PUBKEY()      , ; //
       @PEM_READ_BIO_DSAPRIVATEKEY()   , ;
       @PEM_READ_BIO_DSA_PUBKEY()      , ;
       @PEM_READ_BIO_DSAPARAMS()       , ;
       @PEM_READ_BIO_DHPARAMS()        , ;
       @PEM_READ_BIO_X509()            , ;
       @PEM_READ_BIO_X509_AUX()        , ;
       @PEM_READ_BIO_X509_REQ()        , ;
       @PEM_READ_BIO_X509_CRL()        , ;
       @PEM_READ_BIO_PKCS7()           }

   SSL_init()

   OpenSSL_add_all_ciphers()

   ? "encrypt"

   ctx := EVP_CIPHER_CTX_new()

   ? EVP_EncryptInit( ctx, "AES256", cKey, hb_rand32( 40 ) )
   ? EVP_CIPHER_CTX_cipher( ctx )
   ? EVP_CIPHER_block_size( EVP_CIPHER_CTX_cipher( ctx ) )

   encrypted := ""
   result := ""
   ? EVP_EncryptUpdate( ctx, @result, "sample text" )
   encrypted += result
   ? EVP_EncryptFinal( ctx, @result )
   encrypted += result
   ? "ENCRYPTED", ">" + hb_StrToHex( encrypted ) + "<"

   ? "decrypt"

   ctx := EVP_CIPHER_CTX_new()

   ? EVP_DecryptInit( ctx, "AES256", cKey )

   decrypted := ""
   result := ""
   ? EVP_DecryptUpdate( ctx, @result, encrypted )
   decrypted += result
   /* TOFIX: this fails sometimes */
   ? EVP_DecryptFinal( ctx, @result )
   decrypted += result
   ? "DECRYPTED", ">" + decrypted + "<"

   ? ERR_load_PEM_strings()
   ? OpenSSL_add_all_algorithms()

   ctx := EVP_CIPHER_CTX_new()

   ? Replicate( "=", 15 )
   bioe := BIO_new_fd( hb_GetStdOut(), HB_BIO_NOCLOSE )
   FOR EACH tmp IN all
      ? tmp:__enumIndex(), pub := tmp:exec( "pubkey.pem", "test" )
      IF ! Empty( pub )
         ? "EVP_PKEY_free()", EVP_PKEY_free( pub )
      ENDIF
      ? ; ERR_print_errors( bioe )
   NEXT
   BIO_free( bioe )

   ? pub := PEM_READ_BIO_PUBKEY( "pubkey.pem", "test" )

   ? "EVP_SealInit()", EVP_SealInit( ctx, "AES256", @a, @iv, { pub } )
   ? ValType( a ), iif( HB_ISSTRING( a ), hb_BLen( a ), NIL )
   ? ValType( a[ 1 ] ), ">" + hb_StrToHex( a[ 1 ] ) + "<"
   ? ValType( iv ), ">" + hb_StrToHex( iv ) + "<"

   ? "EVP_PKEY_free()", EVP_PKEY_free( pub )

   ctx := NIL

   EVP_cleanup()

   RETURN
