/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

#include "hbsocket.ch"

#define CRLF  ( hb_BChar( 13 ) + hb_BChar( 10 ) )

PROCEDURE Main()

   LOCAL ssl_ctx
   LOCAL ssl
   LOCAL cipher

   LOCAL socket
   LOCAL buffer

   LOCAL bits
   LOCAL tmp

   ? "---"

   socket := hb_socketOpen()
   ? "hb_socketConnect()", hb_socketConnect( socket, { HB_SOCKET_AF_INET, hb_socketResolveAddr( "example.org" ), 80 } )
   ? "hb_socketGetError()", hb_socketGetError( socket )
   ? "hb_socketGetFD()", hb_socketGetFD( socket )
   ? "hb_socketSend()", hb_socketSend( socket, "GET / HTTP/1.1" + CRLF + "Host: " + "example.org" + CRLF + CRLF )
   ? "hb_socketGetError()", hb_socketGetError( socket )
   buffer := Space( 1024 )
   ? "hb_socketRecv()", hb_socketRecv( socket, @buffer,,, 500 )
   ? "BUFFER", ">" + AllTrim( buffer ) + "<"
   ? "hb_socketClose()", hb_socketClose( socket )

   ? "---"

   socket := hb_socketOpen()
   ? hb_socketConnect( socket, { HB_SOCKET_AF_INET, hb_socketResolveAddr( "example.org" ), 443 } )
   ? hb_socketGetError( socket )

   //

   SSL_init()

   ? OpenSSL_version()
   ? OpenSSL_version( HB_OPENSSL_VERSION )
   ? OpenSSL_version( HB_OPENSSL_CFLAGS )
   ? OpenSSL_version( HB_OPENSSL_BUILT_ON )
   ? OpenSSL_version( HB_OPENSSL_PLATFORM )
   ? OpenSSL_version( HB_OPENSSL_DIR )

   ? "RAND_seed()", RAND_seed( "some entropy" )

   ? "SSL_CTX_new()", ssl_ctx := SSL_CTX_new()

   ? "SSL_new()", ssl := SSL_new( ssl_ctx )

   ? "SSL_version()", SSL_version( ssl )
   ? "SSL_get_version()", SSL_get_version( ssl )

   ? "hb_socketGetFD()", hb_socketGetFD( socket )

   ? "SSL_set_fd()", SSL_set_fd( ssl, hb_socketGetFD( socket ) )
   ? "SSL_connect()", tmp := SSL_connect( ssl )
   ? "SSL_get_error()", SSL_get_error( ssl, tmp )

   FOR EACH cipher IN SSL_get_ciphers( ssl )
      ? "SSL_CIPHER_get_name()", SSL_CIPHER_get_name( cipher )
      ? "SSL_CIPHER_get_version()", SSL_CIPHER_get_version( cipher )
      ? "SSL_CIPHER_get_bits()", SSL_CIPHER_get_bits( cipher, @bits ), bits
      ? "SSL_CIPHER_description()", ">" + SSL_CIPHER_description( cipher ) + "<"
      ? Replicate( "- ", 15 )
   NEXT

   ? "SSL_get_cipher_bits()", SSL_get_cipher_bits( ssl, @bits ), bits
   ? "SSL_get_cipher_list()", SSL_get_cipher_list( ssl )
   ? "SSL_get_cipher_name()", SSL_get_cipher_name( ssl )
   ? "SSL_get_cipher_version()", SSL_get_cipher_version( ssl )

   ? "SSL_get_current_cipher()", cipher := SSL_get_current_cipher( ssl )
   ? "SSL_CIPHER_get_name()", SSL_CIPHER_get_name( cipher )
   ? "SSL_CIPHER_get_version()", SSL_CIPHER_get_version( cipher )
   ? "SSL_CIPHER_get_bits()", SSL_CIPHER_get_bits( cipher, @bits ), bits
   ? "SSL_CIPHER_description()", SSL_CIPHER_description( cipher )

   ? "SSL_write()", tmp := SSL_write( ssl, "GET / HTTP/1.1" + CRLF + "Host: " + "example.org" + CRLF + CRLF )
   ? "SSL_get_error()", SSL_get_error( ssl, tmp )
   buffer := Space( 1024 )
   ? "SSL_read()", tmp := SSL_read( ssl, @buffer )
   ? "SSL_get_error()", SSL_get_error( ssl, tmp )
   ? buffer

   ? hb_socketClose( socket )

   RETURN
