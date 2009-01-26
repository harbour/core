/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats <harbour 01 syenar hu>
 * www - http://www.harbour-project.org
 */

#include "hbssl.ch"

PROCEDURE Main()
   LOCAL ssl_ctx
   LOCAL ssl

   SSL_INIT()

   ? SSLEAY_VERSION()
   ? SSLEAY_VERSION( HB_SSLEAY_VERSION  )
   ? SSLEAY_VERSION( HB_SSLEAY_CFLAGS   )
   ? SSLEAY_VERSION( HB_SSLEAY_BUILT_ON )
   ? SSLEAY_VERSION( HB_SSLEAY_PLATFORM )
   ? SSLEAY_VERSION( HB_SSLEAY_DIR      )

   SSL_RAND_seed( "some entropy" )

   ssl_ctx := SSL_CTX_NEW()

   ? ssl_ctx

   ssl := SSL_NEW( ssl_ctx )

   ? ssl
   ? SSL_VERSION( ssl )
   ? SSL_GET_VERSION( ssl )

   RETURN
