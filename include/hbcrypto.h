/*
 * FIPS 180-2 SHA-224/256/384/512 implementation
 * Last update: 2007-02-02
 * Issue date:  2005-04-30
 * HMAC-SHA-224/256/384/512 implementation
 * Last update: 2005-06-15
 * Issue date:  2005-06-15
 *
 * Copyright (C) 2005, 2007 Olivier Gay <olivier.gay@a3.epfl.ch>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the project nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE PROJECT AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE PROJECT OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef HB_CRYPTO_H_
#define HB_CRYPTO_H_

#include "hbapi.h"

#define HB_SHA224_DIGEST_SIZE  (  224 / 8 )
#define HB_SHA256_DIGEST_SIZE  (  256 / 8 )
#define HB_SHA384_DIGEST_SIZE  (  384 / 8 )
#define HB_SHA512_DIGEST_SIZE  (  512 / 8 )

#define HB_SHA256_BLOCK_SIZE   (  512 / 8 )
#define HB_SHA512_BLOCK_SIZE   ( 1024 / 8 )
#define HB_SHA384_BLOCK_SIZE   HB_SHA512_BLOCK_SIZE
#define HB_SHA224_BLOCK_SIZE   HB_SHA256_BLOCK_SIZE

HB_EXTERN_BEGIN

typedef struct {
   HB_SIZE tot_len;
   HB_SIZE len;
   unsigned char block[ 2 * HB_SHA256_BLOCK_SIZE ];
   HB_U32 h[ 8 ];
} hb_sha256_ctx;

typedef struct {
   HB_SIZE tot_len;
   HB_SIZE len;
   unsigned char block[ 2 * HB_SHA512_BLOCK_SIZE ];
   HB_U64 h[ 8 ];
} hb_sha512_ctx;

typedef hb_sha512_ctx hb_sha384_ctx;
typedef hb_sha256_ctx hb_sha224_ctx;

extern HB_EXPORT void hb_sha224_init( hb_sha224_ctx * ctx );
extern HB_EXPORT void hb_sha224_update( hb_sha224_ctx * ctx,
                                        const void * message,
                                        HB_SIZE len );
extern HB_EXPORT void hb_sha224_final( hb_sha224_ctx * ctx,
                                       unsigned char * digest );
extern HB_EXPORT void hb_sha224( const void * message,
                                 HB_SIZE len,
                                 unsigned char * digest );

extern HB_EXPORT void hb_sha256_init( hb_sha256_ctx * ctx );
extern HB_EXPORT void hb_sha256_update( hb_sha256_ctx * ctx,
                                       const void *message,
                                       HB_SIZE len );
extern HB_EXPORT void hb_sha256_final( hb_sha256_ctx * ctx,
                                       unsigned char * digest );
extern HB_EXPORT void hb_sha256( const void * message,
                                 HB_SIZE len,
                                 unsigned char * digest );

extern HB_EXPORT void hb_sha384_init( hb_sha384_ctx * ctx );
extern HB_EXPORT void hb_sha384_update( hb_sha384_ctx * ctx,
                                        const void *message,
                                        HB_SIZE len );
extern HB_EXPORT void hb_sha384_final( hb_sha384_ctx * ctx,
                                       unsigned char * digest );
extern HB_EXPORT void hb_sha384( const void * message,
                                 HB_SIZE len,
                                 unsigned char * digest );

extern HB_EXPORT void hb_sha512_init( hb_sha512_ctx * ctx );
extern HB_EXPORT void hb_sha512_update( hb_sha512_ctx * ctx,
                                        const void *message,
                                        HB_SIZE len );
extern HB_EXPORT void hb_sha512_final( hb_sha512_ctx * ctx,
                                       unsigned char * digest );
extern HB_EXPORT void hb_sha512( const void * message,
                                 HB_SIZE len,
                                 unsigned char * digest );

typedef struct {
   hb_sha224_ctx ctx_inside;
   hb_sha224_ctx ctx_outside;

   /* for hmac_reinit */
   hb_sha224_ctx ctx_inside_reinit;
   hb_sha224_ctx ctx_outside_reinit;

   unsigned char block_ipad[ HB_SHA224_BLOCK_SIZE ];
   unsigned char block_opad[ HB_SHA224_BLOCK_SIZE ];
} hb_hmac_sha224_ctx;

typedef struct {
   hb_sha256_ctx ctx_inside;
   hb_sha256_ctx ctx_outside;

   /* for hmac_reinit */
   hb_sha256_ctx ctx_inside_reinit;
   hb_sha256_ctx ctx_outside_reinit;

   unsigned char block_ipad[ HB_SHA256_BLOCK_SIZE ];
   unsigned char block_opad[ HB_SHA256_BLOCK_SIZE ];
} hb_hmac_sha256_ctx;

typedef struct {
   hb_sha384_ctx ctx_inside;
   hb_sha384_ctx ctx_outside;

   /* for hmac_reinit */
   hb_sha384_ctx ctx_inside_reinit;
   hb_sha384_ctx ctx_outside_reinit;

   unsigned char block_ipad[ HB_SHA384_BLOCK_SIZE ];
   unsigned char block_opad[ HB_SHA384_BLOCK_SIZE ];
} hb_hmac_sha384_ctx;

typedef struct {
   hb_sha512_ctx ctx_inside;
   hb_sha512_ctx ctx_outside;

   /* for hmac_reinit */
   hb_sha512_ctx ctx_inside_reinit;
   hb_sha512_ctx ctx_outside_reinit;

   unsigned char block_ipad[ HB_SHA512_BLOCK_SIZE ];
   unsigned char block_opad[ HB_SHA512_BLOCK_SIZE ];
} hb_hmac_sha512_ctx;

extern HB_EXPORT void hb_hmac_sha224_init( hb_hmac_sha224_ctx * ctx,
                                           const void * key,
                                           HB_SIZE key_size );
extern HB_EXPORT void hb_hmac_sha224_reinit( hb_hmac_sha224_ctx * ctx );
extern HB_EXPORT void hb_hmac_sha224_update( hb_hmac_sha224_ctx * ctx,
                                             const void * message,
                                             HB_SIZE message_len );
extern HB_EXPORT void hb_hmac_sha224_final( hb_hmac_sha224_ctx * ctx,
                                            unsigned char * mac,
                                            HB_SIZE mac_size );
extern HB_EXPORT void hb_hmac_sha224( const void * key,
                                      HB_SIZE key_size,
                                      const void * message,
                                      HB_SIZE message_len,
                                      unsigned char * mac,
                                      HB_SIZE mac_size );

extern HB_EXPORT void hb_hmac_sha256_init( hb_hmac_sha256_ctx * ctx,
                                           const void * key,
                                           HB_SIZE key_size );
extern HB_EXPORT void hb_hmac_sha256_reinit( hb_hmac_sha256_ctx * ctx );
extern HB_EXPORT void hb_hmac_sha256_update( hb_hmac_sha256_ctx * ctx,
                                             const void * message,
                                             HB_SIZE message_len );
extern HB_EXPORT void hb_hmac_sha256_final( hb_hmac_sha256_ctx * ctx,
                                            unsigned char * mac,
                                            HB_SIZE mac_size );
extern HB_EXPORT void hb_hmac_sha256( const void * key,
                                      HB_SIZE key_size,
                                      const void * message,
                                      HB_SIZE message_len,
                                      unsigned char * mac,
                                      HB_SIZE mac_size );

extern HB_EXPORT void hb_hmac_sha384_init( hb_hmac_sha384_ctx * ctx,
                                           const void * key,
                                           HB_SIZE key_size );
extern HB_EXPORT void hb_hmac_sha384_reinit( hb_hmac_sha384_ctx * ctx );
extern HB_EXPORT void hb_hmac_sha384_update( hb_hmac_sha384_ctx * ctx,
                                             const void * message,
                                             HB_SIZE message_len );
extern HB_EXPORT void hb_hmac_sha384_final( hb_hmac_sha384_ctx * ctx,
                                            unsigned char * mac,
                                            HB_SIZE mac_size );
extern HB_EXPORT void hb_hmac_sha384( const void * key,
                                      HB_SIZE key_size,
                                      const void * message,
                                      HB_SIZE message_len,
                                      unsigned char * mac,
                                      HB_SIZE mac_size );

extern HB_EXPORT void hb_hmac_sha512_init( hb_hmac_sha512_ctx * ctx,
                                           const void * key,
                                           HB_SIZE key_size );
extern HB_EXPORT void hb_hmac_sha512_reinit( hb_hmac_sha512_ctx * ctx );
extern HB_EXPORT void hb_hmac_sha512_update( hb_hmac_sha512_ctx * ctx,
                                             const void * message,
                                             HB_SIZE message_len );
extern HB_EXPORT void hb_hmac_sha512_final( hb_hmac_sha512_ctx * ctx,
                                            unsigned char * mac,
                                            HB_SIZE mac_size );
extern HB_EXPORT void hb_hmac_sha512( const void * key,
                                      HB_SIZE key_size,
                                      const void * message,
                                      HB_SIZE message_len,
                                      unsigned char * mac,
                                      HB_SIZE mac_size );

HB_EXTERN_END

#endif /* HB_CRYPTO_H_ */
