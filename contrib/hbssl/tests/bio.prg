/* Copyright 2009-2016 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

PROCEDURE Main()

   LOCAL bio
   LOCAL buffer

   SSL_init()

   ? bio := BIO_new_fd( hb_GetStdOut(), HB_BIO_NOCLOSE )
   ? "BIO_write()", BIO_write( bio, "Hello world!" + hb_eol() )
   ? "BIO_flush()", BIO_flush( bio )
   ? "BIO_free()", BIO_free( bio )

   ? bio := BIO_new_mem_buf( "This is a test string" )
   ? "BIO_read()", BIO_read( bio, @buffer, 10 )
   ? ">" + buffer + "<"
   ? "BIO_flush()", BIO_flush( bio )
   ? "BIO_free()", BIO_free( bio )

   RETURN
