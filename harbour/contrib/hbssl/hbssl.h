/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API - C header.
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HBSSL_H_
#define HBSSL_H_

#include "hbapi.h"

#if defined( HB_OS_WIN )
   #if ! defined( HB_OPENSSL_STATIC )
      #define OPENSSL_OPT_WINDLL
   #endif
#endif

#include <openssl/ssl.h>

#include "hbssl.ch"

#if OPENSSL_VERSION_NUMBER < 0x0090806fL
   #ifndef OPENSSL_NO_SEED
      #define OPENSSL_NO_SEED
   #endif
#endif

HB_EXTERN_BEGIN

extern const SSL_METHOD * hb_ssl_method_id_to_ptr( int n );

extern void *             hb_BIO_is( int iParam );
extern BIO *              hb_BIO_par( int iParam );

extern void *             hb_SSL_CTX_is( int iParam );
extern SSL_CTX *          hb_SSL_CTX_par( int iParam );

extern void *             hb_SSL_is( int iParam );
extern SSL *              hb_SSL_par( int iParam );

extern void *             hb_SSL_SESSION_is( int iParam );
extern SSL_SESSION *      hb_SSL_SESSION_par( int iParam );

extern void *             hb_X509_is( int iParam );
extern X509 *             hb_X509_par( int iParam );
extern void               hb_X509_ret( X509 * x509, HB_BOOL fRelease );

extern int                hb_EVP_MD_is( int iParam );
extern const EVP_MD *     hb_EVP_MD_par( int iParam );

extern int                hb_EVP_CIPHER_is( int iParam );
extern const EVP_CIPHER * hb_EVP_CIPHER_par( int iParam );

extern void *             hb_EVP_PKEY_is( int iParam );
extern EVP_PKEY *         hb_EVP_PKEY_par( int iParam );

extern char *             hb_openssl_strdup( const char * pszText );

HB_EXTERN_END

#endif /* HBSSL_H_ */
