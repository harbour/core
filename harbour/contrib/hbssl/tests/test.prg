/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour syenar.hu)
 * www - http://harbour-project.org
 */

#include "simpleio.ch"

#include "hbssl.ch"

PROCEDURE Main()
   LOCAL ssl_ctx
   LOCAL ssl
   LOCAL cipher

   LOCAL socket
   LOCAL buffer

   LOCAL bits
   LOCAL tmp

   //

   hb_inetInit()

   ? "-------"

   socket := hb_inetCreate()
   ? "INETTIMEOUT", hb_inetTimeout( socket, 500 )
   ? "INETCONN", hb_inetConnect( "www.fortify.net", 80, socket )
   ? "INETERR", hb_inetErrorCode( socket )
   ? "INETFD", hb_inetFD( socket )
   ? "INETSEND", hb_inetSend( socket, "GET / http/1.1" + hb_inetCRLF() + "Host: " + "www.syenar.hu" + hb_inetCRLF() + hb_inetCRLF() )
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

   SSL_INIT()

   ? SSLEAY_VERSION()
   ? SSLEAY_VERSION( HB_SSLEAY_VERSION  )
   ? SSLEAY_VERSION( HB_SSLEAY_CFLAGS   )
   ? SSLEAY_VERSION( HB_SSLEAY_BUILT_ON )
   ? SSLEAY_VERSION( HB_SSLEAY_PLATFORM )
   ? SSLEAY_VERSION( HB_SSLEAY_DIR      )

   ? "RAND_SEED", RAND_seed( "some entropy" )

   ? "SSL_CTX_NEW", ssl_ctx := SSL_CTX_NEW()

   ? "SSL_NEW", ssl := SSL_NEW( ssl_ctx )

   ? "SSL_VERSION", SSL_VERSION( ssl )
   ? "SSL_GET_VERSION", SSL_GET_VERSION( ssl )

   ? "INET FD", hb_inetFD( socket )

   ? "SSL_SET_FD", SSL_SET_FD( ssl, hb_inetFD( socket ) )
   ? "SSL_CONNECT", tmp := SSL_CONNECT( ssl )
   ? "SSL_GET_ERROR", SSL_GET_ERROR( ssl, tmp )

   tmp := SSL_get_ciphers( ssl )
   FOR EACH cipher IN tmp
      ? "SSL_CIPHER_GET_NAME"   , SSL_CIPHER_GET_NAME( cipher )
      ? "SSL_CIPHER_GET_VERSION", SSL_CIPHER_GET_VERSION( cipher )
      ? "SSL_CIPHER_GET_BITS"   , SSL_CIPHER_GET_BITS( cipher, @bits ), bits
      ? "SSL_CIPHER_DESCRIPTION", ">" + SSL_CIPHER_DESCRIPTION( cipher ) + "<"
      ? "- - - - - - - - - - - - - - -"
   NEXT

   ? "SSL_GET_CIPHER_BITS"    , SSL_GET_CIPHER_BITS( ssl, @bits ), bits
   ? "SSL_GET_CIPHER_LIST"    , SSL_GET_CIPHER_LIST( ssl )
   ? "SSL_GET_CIPHER_NAME"    , SSL_GET_CIPHER_NAME( ssl )
   ? "SSL_GET_CIPHER_VERSION" , SSL_GET_CIPHER_VERSION( ssl )

   ? "SSL_GET_CURRENT_CIPHER", cipher := SSL_GET_CURRENT_CIPHER( ssl )
   ? "SSL_CIPHER_GET_NAME"   , SSL_CIPHER_GET_NAME( cipher )
   ? "SSL_CIPHER_GET_VERSION", SSL_CIPHER_GET_VERSION( cipher )
   ? "SSL_CIPHER_GET_BITS"   , SSL_CIPHER_GET_BITS( cipher, @bits ), bits
   ? "SSL_CIPHER_DESCRIPTION", SSL_CIPHER_DESCRIPTION( cipher )

   ? "SSL_WRITE", tmp := SSL_WRITE( ssl, "GET / http/1.1" + hb_inetCRLF() + "Host: " + "www.fortify.net" + hb_inetCRLF() + hb_inetCRLF() )
   ? "SSL_GET_ERROR", SSL_GET_ERROR( ssl, tmp )
   buffer := Space( 1024 )
   ? "SSL_READ", tmp := SSL_READ( ssl, @buffer )
   ? "SSL_GET_ERROR", SSL_GET_ERROR( ssl, tmp )
   ? buffer

   ? hb_inetClose( socket )

   RETURN
