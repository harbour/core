/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 */

#include "simpleio.ch"

#include "hbssl.ch"

PROCEDURE Main()

   LOCAL ctx
   LOCAL digest

   OpenSSL_add_all_digests()

   ctx := EVP_MD_CTX_create()
   EVP_MD_CTX_init( ctx )

   EVP_DigestInit_ex( ctx, HB_EVP_MD_SHA256 )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "SHA256", ">" + hb_StrToHex( digest ) + "<"

   EVP_MD_CTX_cleanup( ctx )

   EVP_DigestInit_ex( ctx, HB_EVP_MD_RIPEMD160 )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "RIPEMD160", ">" + hb_StrToHex( digest ) + "<"

   EVP_cleanup()

   RETURN
