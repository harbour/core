/* Copyright 2009-2016 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

PROCEDURE Main()

   LOCAL bio
   LOCAL buffer

   SSL_init()

   ? "hbssl dynamic:", ! hb_SSL_STATIC()
   ? "hbssl Applink support:", hb_SSL_APPLINK()

   ? bio := BIO_new_mem_buf( "This is a test string" )
   ? "BIO_read()", BIO_read( bio, @buffer, 10 )
   ? ">" + buffer + "<"
   ? "BIO_flush()", BIO_flush( bio )

   ? bio := BIO_new_fd( hb_GetStdOut(), HB_BIO_NOCLOSE )
   ? "BIO_write()", BIO_write( bio, "Hello world!" + hb_eol() )
   ? "BIO_flush()", BIO_flush( bio )

   RETURN
