/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

PROCEDURE Main()

   LOCAL bio

   SSL_init()

   ? bio := BIO_new_fd( 1, HB_BIO_NOCLOSE )
   ? "BIO_write()", BIO_write( bio, "Hello world!" + hb_eol() )
   ? "BIO_flush()", BIO_flush( bio )
   ? "BIO_free()", BIO_free( bio )

   ? bio := BIO_new_file( "bio_test.txt", "a+" )
   ? "BIO_write()", BIO_write( bio, "Hello world!" + hb_eol() )
   ? "BIO_flush()", BIO_flush( bio )
   ? "BIO_free()", BIO_free( bio )

   RETURN
