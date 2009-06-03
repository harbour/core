/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 */

#include "hbssl.ch"

PROCEDURE Main()
   LOCAL ssl_ctx
   LOCAL ssl
   LOCAL cipher

   LOCAL socket
   LOCAL buffer := Space( 1000 )

   LOCAL bits

   //

   hb_inetInit()
   socket := hb_inetCreate()
   ? hb_inetTimeout( socket, 500 )
   ? hb_inetConnect( "www.harbour-project.org", 443, socket )

   //

   SSL_INIT()

   ? SSLEAY_VERSION()
   ? SSLEAY_VERSION( HB_SSLEAY_VERSION  )
   ? SSLEAY_VERSION( HB_SSLEAY_CFLAGS   )
   ? SSLEAY_VERSION( HB_SSLEAY_BUILT_ON )
   ? SSLEAY_VERSION( HB_SSLEAY_PLATFORM )
   ? SSLEAY_VERSION( HB_SSLEAY_DIR      )

   ? "SSL_RAND_SEED", SSL_RAND_seed( "some entropy" )

   ? "SSL_CTX_NEW", ssl_ctx := SSL_CTX_NEW()

   ? "SSL_NEW", ssl := SSL_NEW( ssl_ctx )

   ? "SSL_VERSION", SSL_VERSION( ssl )
   ? "SSL_GET_VERSION", SSL_GET_VERSION( ssl )

   ? "SSL_SET_FD", SSL_SET_FD( ssl, hb_inetFD( socket ) )
   ? "SSL_CONNECT", SSL_CONNECT( ssl )

   ? "SSL_GET_CIPHER_BITS"    , SSL_GET_CIPHER_BITS( ssl, @bits ), bits
   ? "SSL_GET_CIPHER_LIST"    , SSL_GET_CIPHER_LIST( ssl )
   ? "SSL_GET_CIPHER_NAME"    , SSL_GET_CIPHER_NAME( ssl )
   ? "SSL_GET_CIPHER_VERSION" , SSL_GET_CIPHER_VERSION( ssl )

   ? "SSL_GET_CURRENT_CIPHER", cipher := SSL_GET_CURRENT_CIPHER( ssl )
   ? "SSL_CIPHER_GET_NAME"   , SSL_CIPHER_GET_NAME( cipher )
   ? "SSL_CIPHER_GET_VERSION", SSL_CIPHER_GET_VERSION( cipher )
   ? "SSL_CIPHER_GET_BITS"   , SSL_CIPHER_GET_BITS( cipher, @bits ), bits
   ? "SSL_CIPHER_DESCRIPTION", SSL_CIPHER_DESCRIPTION( cipher )

   ? "SSL_WRITE", SSL_WRITE( ssl, "GET / http/1.1" )
   ? "SSL_READ", SSL_READ( ssl, @buffer )

   ? buffer

   ? hb_inetClose( socket )

   RETURN
