/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 */

#include "simpleio.ch"

#include "hbssl.ch"

PROCEDURE Main()

   LOCAL bio

   SSL_INIT()

   ? bio := BIO_new_fd( 1, HB_BIO_NOCLOSE )
   ? "BIO_WRITE", BIO_write( bio, "Hello world!" + hb_eol() )
   ? "BIO_FLUSH", BIO_flush( bio )
   ? "BIO_FREE", BIO_free( bio )

   ? bio := BIO_new_file( "bio_test.txt", "a+" )
   ? "BIO_WRITE", BIO_write( bio, "Hello world!" + hb_eol() )
   ? "BIO_FLUSH", BIO_flush( bio )
   ? "BIO_FREE", BIO_free( bio )

   RETURN
