/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

#define CRLF Chr( 13 ) + Chr( 10 )

PROCEDURE Main()

   LOCAL ssl_ctx
   LOCAL ssl
   LOCAL cipher

   LOCAL socket
   LOCAL buffer

   LOCAL bits
   LOCAL tmp

   // TODO: use hb_socket*() API instead of hb_inet*()

   hb_inetInit()

   ? "-------"

   socket := hb_inetCreate()
   ? "INETTIMEOUT", hb_inetTimeout( socket, 500 )
   ? "INETCONN", hb_inetConnect( "www.fortify.net", 80, socket )
   ? "INETERR", hb_inetErrorCode( socket )
   ? "INETFD", hb_inetFD( socket )
   ? "INETSEND", hb_inetSend( socket, "GET / http/1.1" + CRLF + "Host: " + "www.fortify.net" + CRLF + CRLF )
   ? "INETERR", hb_inetErrorCode( socket )
   buffer := Space( 1024 )
   ? "INETRECVALL", hb_inetRecvAll( socket, @buffer, Len( buffer ) )
   ? "BUFFER", ">" + AllTrim( buffer ) + "<"
   ? "INETCLOSE", hb_inetClose( socket )

   ? "-------"

   socket := hb_inetCreate()
   ? hb_inetTimeout( socket, 2500 )
   ? hb_inetConnect( "www.fortify.net", 443, socket )
   ? hb_inetErrorCode( socket )

   //

   SSL_init()

   ? SSLeay_version()
   ? SSLeay_version( HB_SSLEAY_VERSION  )
   ? SSLeay_version( HB_SSLEAY_CFLAGS   )
   ? SSLeay_version( HB_SSLEAY_BUILT_ON )
   ? SSLeay_version( HB_SSLEAY_PLATFORM )
   ? SSLeay_version( HB_SSLEAY_DIR      )

   ? "RAND_SEED", RAND_seed( "some entropy" )

   ? "SSL_CTX_NEW", ssl_ctx := SSL_CTX_new()

   ? "SSL_NEW", ssl := SSL_new( ssl_ctx )

   ? "SSL_VERSION", SSL_version( ssl )
   ? "SSL_GET_VERSION", SSL_get_version( ssl )

   ? "INET FD", hb_inetFD( socket )

   ? "SSL_SET_FD", SSL_set_fd( ssl, hb_inetFD( socket ) )
   ? "SSL_CONNECT", tmp := SSL_connect( ssl )
   ? "SSL_GET_ERROR", SSL_get_error( ssl, tmp )

   tmp := SSL_get_ciphers( ssl )
   FOR EACH cipher IN tmp
      ? "SSL_CIPHER_GET_NAME", SSL_CIPHER_get_name( cipher )
      ? "SSL_CIPHER_GET_VERSION", SSL_CIPHER_get_version( cipher )
      ? "SSL_CIPHER_GET_BITS", SSL_CIPHER_get_bits( cipher, @bits ), bits
      ? "SSL_CIPHER_DESCRIPTION", ">" + SSL_CIPHER_description( cipher ) + "<"
      ? "- - - - - - - - - - - - - - -"
   NEXT

   ? "SSL_GET_CIPHER_BITS", SSL_get_cipher_bits( ssl, @bits ), bits
   ? "SSL_GET_CIPHER_LIST", SSL_get_cipher_list( ssl )
   ? "SSL_GET_CIPHER_NAME", SSL_get_cipher_name( ssl )
   ? "SSL_GET_CIPHER_VERSION", SSL_get_cipher_version( ssl )

   ? "SSL_GET_CURRENT_CIPHER", cipher := SSL_get_current_cipher( ssl )
   ? "SSL_CIPHER_GET_NAME", SSL_CIPHER_get_name( cipher )
   ? "SSL_CIPHER_GET_VERSION", SSL_CIPHER_get_version( cipher )
   ? "SSL_CIPHER_GET_BITS", SSL_CIPHER_get_bits( cipher, @bits ), bits
   ? "SSL_CIPHER_DESCRIPTION", SSL_CIPHER_description( cipher )

   ? "SSL_WRITE", tmp := SSL_write( ssl, "GET / http/1.1" + CRLF + "Host: " + "www.fortify.net" + CRLF + CRLF )
   ? "SSL_GET_ERROR", SSL_get_error( ssl, tmp )
   buffer := Space( 1024 )
   ? "SSL_READ", tmp := SSL_read( ssl, @buffer )
   ? "SSL_GET_ERROR", SSL_get_error( ssl, tmp )
   ? buffer

   ? hb_inetClose( socket )

   RETURN
