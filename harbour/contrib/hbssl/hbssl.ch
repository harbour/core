/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API - Harbour header.
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

#define HB_X509_V_OK                                        0
#define HB_X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT             2
#define HB_X509_V_ERR_UNABLE_TO_GET_CRL                     3
#define HB_X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE      4
#define HB_X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE       5
#define HB_X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY    6
#define HB_X509_V_ERR_CERT_SIGNATURE_FAILURE                7
#define HB_X509_V_ERR_CRL_SIGNATURE_FAILURE                 8
#define HB_X509_V_ERR_CERT_NOT_YET_VALID                    9
#define HB_X509_V_ERR_CERT_HAS_EXPIRED                      10
#define HB_X509_V_ERR_CRL_NOT_YET_VALID                     11
#define HB_X509_V_ERR_CRL_HAS_EXPIRED                       12
#define HB_X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD        13
#define HB_X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD         14
#define HB_X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD        15
#define HB_X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD        16
#define HB_X509_V_ERR_OUT_OF_MEM                            17
#define HB_X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT           18
#define HB_X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN             19
#define HB_X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY     20
#define HB_X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE       21
#define HB_X509_V_ERR_CERT_CHAIN_TOO_LONG                   22
#define HB_X509_V_ERR_CERT_REVOKED                          23
#define HB_X509_V_ERR_INVALID_CA                            24
#define HB_X509_V_ERR_PATH_LENGTH_EXCEEDED                  25
#define HB_X509_V_ERR_INVALID_PURPOSE                       26
#define HB_X509_V_ERR_CERT_UNTRUSTED                        27
#define HB_X509_V_ERR_CERT_REJECTED                         28
#define HB_X509_V_ERR_SUBJECT_ISSUER_MISMATCH               29
#define HB_X509_V_ERR_AKID_SKID_MISMATCH                    30
#define HB_X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH           31
#define HB_X509_V_ERR_KEYUSAGE_NO_CERTSIGN                  32
#define HB_X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER              33
#define HB_X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION          34
#define HB_X509_V_ERR_KEYUSAGE_NO_CRL_SIGN                  35
#define HB_X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION      36
#define HB_X509_V_ERR_INVALID_NON_CA                        37
#define HB_X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED            38
#define HB_X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE         39
#define HB_X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED        40
#define HB_X509_V_ERR_INVALID_EXTENSION                     41
#define HB_X509_V_ERR_INVALID_POLICY_EXTENSION              42
#define HB_X509_V_ERR_NO_EXPLICIT_POLICY                    43
#define HB_X509_V_ERR_UNNESTED_RESOURCE                     44
#define HB_X509_V_ERR_APPLICATION_VERIFICATION              50

#define HB_EVP_MD_UNSUPPORTED                               ( -1 )
#define HB_EVP_MD_MD_NULL                                   0
#define HB_EVP_MD_MD2                                       1
#define HB_EVP_MD_MD4                                       2
#define HB_EVP_MD_MD5                                       3
#define HB_EVP_MD_SHA                                       4
#define HB_EVP_MD_SHA1                                      5
#define HB_EVP_MD_DSS                                       6
#define HB_EVP_MD_DSS1                                      7
#define HB_EVP_MD_ECDSA                                     8
#define HB_EVP_MD_SHA224                                    9
#define HB_EVP_MD_SHA256                                    10
#define HB_EVP_MD_SHA384                                    11
#define HB_EVP_MD_SHA512                                    12
#define HB_EVP_MD_MDC2                                      13
#define HB_EVP_MD_RIPEMD160                                 14

#define HB_EVP_CIPHER_UNSUPPORTED                           ( -1 )
#define HB_EVP_CIPHER_ENC_NULL                              0
#define HB_EVP_CIPHER_DES_ECB                               1
#define HB_EVP_CIPHER_DES_EDE                               2
#define HB_EVP_CIPHER_DES_EDE3                              3
#define HB_EVP_CIPHER_DES_EDE_ECB                           4
#define HB_EVP_CIPHER_DES_EDE3_ECB                          5
#define HB_EVP_CIPHER_DES_CFB64                             6
#define HB_EVP_CIPHER_DES_CFB                               7
#define HB_EVP_CIPHER_DES_CFB1                              8
#define HB_EVP_CIPHER_DES_CFB8                              9
#define HB_EVP_CIPHER_DES_EDE_CFB64                         10
#define HB_EVP_CIPHER_DES_EDE_CFB                           11
#define HB_EVP_CIPHER_DES_EDE3_CFB64                        12
#define HB_EVP_CIPHER_DES_EDE3_CFB                          13
#define HB_EVP_CIPHER_DES_EDE3_CFB1                         14
#define HB_EVP_CIPHER_DES_EDE3_CFB8                         15
#define HB_EVP_CIPHER_DES_OFB                               16
#define HB_EVP_CIPHER_DES_EDE_OFB                           17
#define HB_EVP_CIPHER_DES_EDE3_OFB                          18
#define HB_EVP_CIPHER_DES_CBC                               19
#define HB_EVP_CIPHER_DES_EDE_CBC                           20
#define HB_EVP_CIPHER_DES_EDE3_CBC                          21
#define HB_EVP_CIPHER_DESX_CBC                              22
#define HB_EVP_CIPHER_RC4                                   23
#define HB_EVP_CIPHER_RC4_40                                24
#define HB_EVP_CIPHER_IDEA_ECB                              25
#define HB_EVP_CIPHER_IDEA_CFB64                            26
#define HB_EVP_CIPHER_IDEA_CFB                              27
#define HB_EVP_CIPHER_IDEA_OFB                              28
#define HB_EVP_CIPHER_IDEA_CBC                              29
#define HB_EVP_CIPHER_RC2_ECB                               30
#define HB_EVP_CIPHER_RC2_CBC                               31
#define HB_EVP_CIPHER_RC2_40_CBC                            32
#define HB_EVP_CIPHER_RC2_64_CBC                            33
#define HB_EVP_CIPHER_RC2_CFB64                             34
#define HB_EVP_CIPHER_RC2_CFB                               35
#define HB_EVP_CIPHER_RC2_OFB                               36
#define HB_EVP_CIPHER_BF_ECB                                37
#define HB_EVP_CIPHER_BF_CBC                                38
#define HB_EVP_CIPHER_BF_CFB64                              39
#define HB_EVP_CIPHER_BF_CFB                                40
#define HB_EVP_CIPHER_BF_OFB                                41
#define HB_EVP_CIPHER_CAST5_ECB                             42
#define HB_EVP_CIPHER_CAST5_CBC                             43
#define HB_EVP_CIPHER_CAST5_CFB64                           44
#define HB_EVP_CIPHER_CAST5_CFB                             45
#define HB_EVP_CIPHER_CAST5_OFB                             46
#define HB_EVP_CIPHER_RC5_32_12_16_CBC                      47
#define HB_EVP_CIPHER_RC5_32_12_16_ECB                      48
#define HB_EVP_CIPHER_RC5_32_12_16_CFB64                    49
#define HB_EVP_CIPHER_RC5_32_12_16_CFB                      50
#define HB_EVP_CIPHER_RC5_32_12_16_OFB                      51
#define HB_EVP_CIPHER_AES_128_ECB                           52
#define HB_EVP_CIPHER_AES_128_CBC                           53
#define HB_EVP_CIPHER_AES_128_CFB1                          54
#define HB_EVP_CIPHER_AES_128_CFB8                          55
#define HB_EVP_CIPHER_AES_128_CFB128                        56
#define HB_EVP_CIPHER_AES_128_CFB                           57
#define HB_EVP_CIPHER_AES_128_OFB                           58
#define HB_EVP_CIPHER_AES_192_ECB                           59
#define HB_EVP_CIPHER_AES_192_CBC                           60
#define HB_EVP_CIPHER_AES_192_CFB1                          61
#define HB_EVP_CIPHER_AES_192_CFB8                          62
#define HB_EVP_CIPHER_AES_192_CFB128                        63
#define HB_EVP_CIPHER_AES_192_CFB                           64
#define HB_EVP_CIPHER_AES_192_OFB                           65
#define HB_EVP_CIPHER_AES_256_ECB                           66
#define HB_EVP_CIPHER_AES_256_CBC                           67
#define HB_EVP_CIPHER_AES_256_CFB1                          68
#define HB_EVP_CIPHER_AES_256_CFB8                          69
#define HB_EVP_CIPHER_AES_256_CFB128                        70
#define HB_EVP_CIPHER_AES_256_CFB                           71
#define HB_EVP_CIPHER_AES_256_OFB                           72
#define HB_EVP_CIPHER_CAMELLIA_128_ECB                      73
#define HB_EVP_CIPHER_CAMELLIA_128_CBC                      74
#define HB_EVP_CIPHER_CAMELLIA_128_CFB1                     75
#define HB_EVP_CIPHER_CAMELLIA_128_CFB8                     76
#define HB_EVP_CIPHER_CAMELLIA_128_CFB128                   77
#define HB_EVP_CIPHER_CAMELLIA_128_CFB                      78
#define HB_EVP_CIPHER_CAMELLIA_128_OFB                      79
#define HB_EVP_CIPHER_CAMELLIA_192_ECB                      80
#define HB_EVP_CIPHER_CAMELLIA_192_CBC                      81
#define HB_EVP_CIPHER_CAMELLIA_192_CFB1                     82
#define HB_EVP_CIPHER_CAMELLIA_192_CFB8                     83
#define HB_EVP_CIPHER_CAMELLIA_192_CFB128                   84
#define HB_EVP_CIPHER_CAMELLIA_192_CFB                      85
#define HB_EVP_CIPHER_CAMELLIA_192_OFB                      86
#define HB_EVP_CIPHER_CAMELLIA_256_ECB                      87
#define HB_EVP_CIPHER_CAMELLIA_256_CBC                      88
#define HB_EVP_CIPHER_CAMELLIA_256_CFB1                     89
#define HB_EVP_CIPHER_CAMELLIA_256_CFB8                     90
#define HB_EVP_CIPHER_CAMELLIA_256_CFB128                   91
#define HB_EVP_CIPHER_CAMELLIA_256_CFB                      92
#define HB_EVP_CIPHER_CAMELLIA_256_OFB                      93
#define HB_EVP_CIPHER_SEED_ECB                              94
#define HB_EVP_CIPHER_SEED_CBC                              95
#define HB_EVP_CIPHER_SEED_CFB128                           96
#define HB_EVP_CIPHER_SEED_CFB                              97
#define HB_EVP_CIPHER_SEED_OFB                              98

#define HB_BIO_METHOD_UNSUPPORTED                           ( -1 )
#define HB_BIO_METHOD_S_NULL                                0
#define HB_BIO_METHOD_S_FILE                                1
#define HB_BIO_METHOD_S_MEM                                 2
#define HB_BIO_METHOD_S_SOCKET                              3
#define HB_BIO_METHOD_S_CONNECT                             4
#define HB_BIO_METHOD_S_ACCEPT                              5
#define HB_BIO_METHOD_S_FD                                  6
#define HB_BIO_METHOD_S_LOG                                 7
#define HB_BIO_METHOD_S_BIO                                 8
#define HB_BIO_METHOD_S_DATAGRAM                            10
#define HB_BIO_METHOD_F_NULL                                50
#define HB_BIO_METHOD_F_BUFFER                              51
#define HB_BIO_METHOD_F_LINEBUFFER                          52
#define HB_BIO_METHOD_F_NBIO_TEST                           53

#endif /* HBSSL_CH_ */
