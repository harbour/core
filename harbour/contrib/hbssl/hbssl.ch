/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API - Harbour header.
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

#ifndef HBSSL_CH_
#define HBSSL_CH_

/* NOTE: This file is also used by C code. */

#define HB_SSL_CTX_NEW_METHOD_UNKNOWN                       ( -2 )
#define HB_SSL_CTX_NEW_METHOD_DEFAULT                       ( -1 )
#define HB_SSL_CTX_NEW_METHOD_SSLV2                         0
#define HB_SSL_CTX_NEW_METHOD_SSLV2_SERVER                  1
#define HB_SSL_CTX_NEW_METHOD_SSLV2_CLIENT                  2
#define HB_SSL_CTX_NEW_METHOD_SSLV3                         3
#define HB_SSL_CTX_NEW_METHOD_SSLV3_SERVER                  4
#define HB_SSL_CTX_NEW_METHOD_SSLV3_CLIENT                  5
#define HB_SSL_CTX_NEW_METHOD_TLSV1                         6
#define HB_SSL_CTX_NEW_METHOD_TLSV1_SERVER                  7
#define HB_SSL_CTX_NEW_METHOD_TLSV1_CLIENT                  8
#define HB_SSL_CTX_NEW_METHOD_SSLV23                        9
#define HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER                 10
#define HB_SSL_CTX_NEW_METHOD_SSLV23_CLIENT                 11

#define HB_SSLEAY_VERSION                                   0
#define HB_SSLEAY_CFLAGS                                    1
#define HB_SSLEAY_BUILT_ON                                  2
#define HB_SSLEAY_PLATFORM                                  3
#define HB_SSLEAY_DIR                                       4

#define HB_SSL_ERROR_NONE                                   0
#define HB_SSL_ERROR_SSL                                    1
#define HB_SSL_ERROR_WANT_READ                              2
#define HB_SSL_ERROR_WANT_WRITE                             3
#define HB_SSL_ERROR_WANT_X509_LOOKUP                       4
#define HB_SSL_ERROR_SYSCALL                                5
#define HB_SSL_ERROR_ZERO_RETURN                            6
#define HB_SSL_ERROR_WANT_CONNECT                           7
#define HB_SSL_ERROR_WANT_ACCEPT                            8

#define HB_SSL_OP_MICROSOFT_SESS_ID_BUG                     0x00000001
#define HB_SSL_OP_NETSCAPE_CHALLENGE_BUG                    0x00000002
#define HB_SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG          0x00000008
#define HB_SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG               0x00000010
#define HB_SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER                0x00000020
#define HB_SSL_OP_MSIE_SSLV2_RSA_PADDING                    0x00000040
#define HB_SSL_OP_SSLEAY_080_CLIENT_DH_BUG                  0x00000080
#define HB_SSL_OP_TLS_D5_BUG                                0x00000100
#define HB_SSL_OP_TLS_BLOCK_PADDING_BUG                     0x00000200
#define HB_SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS               0x00000800
#define HB_SSL_OP_ALL                                       0x00000FFF
#define HB_SSL_OP_NO_QUERY_MTU                              0x00001000
#define HB_SSL_OP_COOKIE_EXCHANGE                           0x00002000
#define HB_SSL_OP_NO_TICKET                                 0x00004000
#define HB_SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION    0x00010000
#define HB_SSL_OP_SINGLE_ECDH_USE                           0x00080000
#define HB_SSL_OP_SINGLE_DH_USE                             0x00100000
#define HB_SSL_OP_EPHEMERAL_RSA                             0x00200000
#define HB_SSL_OP_CIPHER_SERVER_PREFERENCE                  0x00400000
#define HB_SSL_OP_TLS_ROLLBACK_BUG                          0x00800000
#define HB_SSL_OP_NO_SSLv2                                  0x01000000
#define HB_SSL_OP_NO_SSLv3                                  0x02000000
#define HB_SSL_OP_NO_TLSv1                                  0x04000000
#define HB_SSL_OP_PKCS1_CHECK_1                             0x08000000
#define HB_SSL_OP_PKCS1_CHECK_2                             0x10000000
#define HB_SSL_OP_NETSCAPE_CA_DN_BUG                        0x20000000
#define HB_SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG           0x40000000

#define HB_SSL_MODE_ENABLE_PARTIAL_WRITE                    1
#define HB_SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER              2
#define HB_SSL_MODE_AUTO_RETRY                              4
#define HB_SSL_MODE_NO_AUTO_CHAIN                           8

#define HB_SSL_SENT_SHUTDOWN                                1
#define HB_SSL_RECEIVED_SHUTDOWN                            2

#define HB_SSL_FILETYPE_PEM                                 1
#define HB_SSL_FILETYPE_ASN1                                2

#define HB_BIO_NOCLOSE                                      0x00
#define HB_BIO_CLOSE                                        0x01

#endif /* HBSSL_CH_ */
