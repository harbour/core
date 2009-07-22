/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (PEM) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "hbapierr.h"

#include "hbssl.h"

/* ---------------------------------------------------------------------------- */
/* Callbacks */

int hb_ssl_pem_password_cb( char * buf, int size, int rwflag, void * userdata )
{
   HB_SYMBOL_UNUSED( buf );
   HB_SYMBOL_UNUSED( size );
   HB_SYMBOL_UNUSED( rwflag );
   HB_SYMBOL_UNUSED( userdata );

   return 0;
}

HB_FUNC( ERR_LOAD_PEM_STRINGS )
{
   ERR_load_PEM_strings();
}

#if 0

EVP_PKEY * PEM_read_bio_PrivateKey(BIO *bp, EVP_PKEY **x, pem_password_cb *cb, void *u);
EVP_PKEY * PEM_read_bio_PUBKEY(BIO *bp, EVP_PKEY **x, pem_password_cb *cb, void *u);
RSA *      PEM_read_bio_RSAPrivateKey(BIO *bp, RSA **x, pem_password_cb *cb, void *u);
RSA *      PEM_read_bio_RSAPublicKey(BIO *bp, RSA **x, pem_password_cb *cb, void *u);
RSA *      PEM_read_bio_RSA_PUBKEY(BIO *bp, RSA **x, pem_password_cb *cb, void *u);
DSA *      PEM_read_bio_DSAPrivateKey(BIO *bp, DSA **x, pem_password_cb *cb, void *u);
DSA *      PEM_read_bio_DSA_PUBKEY(BIO *bp, DSA **x, pem_password_cb *cb, void *u);
DSA *      PEM_read_bio_DSAparams(BIO *bp, DSA **x, pem_password_cb *cb, void *u);
DH *       PEM_read_bio_DHparams(BIO *bp, DH **x, pem_password_cb *cb, void *u);
X509 *     PEM_read_bio_X509(BIO *bp, X509 **x, pem_password_cb *cb, void *u);
X509 *     PEM_read_bio_X509_AUX(BIO *bp, X509 **x, pem_password_cb *cb, void *u);
X509_REQ * PEM_read_bio_X509_REQ(BIO *bp, X509_REQ **x, pem_password_cb *cb, void *u);
X509_CRL * PEM_read_bio_X509_CRL(BIO *bp, X509_CRL **x, pem_password_cb *cb, void *u);
PKCS7 *    PEM_read_bio_PKCS7(BIO *bp, PKCS7 **x, pem_password_cb *cb, void *u);

int        PEM_write_bio_PrivateKey(BIO *bp, EVP_PKEY *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);
int        PEM_write_bio_PKCS8PrivateKey(BIO *bp, EVP_PKEY *x, const EVP_CIPHER *enc, char *kstr, int klen, pem_password_cb *cb, void *u);
int        PEM_write_bio_PKCS8PrivateKey_nid(BIO *bp, EVP_PKEY *x, int nid, char *kstr, int klen, pem_password_cb *cb, void *u);
int        PEM_write_bio_PUBKEY(BIO *bp, EVP_PKEY *x);
int        PEM_write_bio_RSAPrivateKey(BIO *bp, RSA *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);
int        PEM_write_bio_RSAPublicKey(BIO *bp, RSA *x);
int        PEM_write_bio_RSA_PUBKEY(BIO *bp, RSA *x);
int        PEM_write_bio_DSAPrivateKey(BIO *bp, DSA *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);
int        PEM_write_bio_DSA_PUBKEY(BIO *bp, DSA *x);
int        PEM_write_bio_DSAparams(BIO *bp, DSA *x);
int        PEM_write_bio_DHparams(BIO *bp, DH *x);
int        PEM_write_bio_X509(BIO *bp, X509 *x);
int        PEM_write_bio_X509_AUX(BIO *bp, X509 *x);
int        PEM_write_bio_X509_REQ(BIO *bp, X509_REQ *x);
int        PEM_write_bio_X509_REQ_NEW(BIO *bp, X509_REQ *x);
int        PEM_write_bio_X509_CRL(BIO *bp, X509_CRL *x);
int        PEM_write_bio_PKCS7(BIO *bp, PKCS7 *x);

#endif
