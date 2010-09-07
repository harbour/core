/*
 * $Id$
 */

/*-
 * HMAC-SHA-224/256/384/512 implementation
 * Last update: 06/15/2005
 * Issue date:  06/15/2005
 *
 * Copyright (C) 2005 Olivier Gay <olivier.gay@a3.epfl.ch>
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

#include <string.h>

#include "sha2hmac.h"

/* HMAC-SHA-224 functions */

void hb_hmac_sha224_init(hmac_sha224_ctx *ctx, const void *keyv,
                      unsigned int key_size)
{
    unsigned int fill;
    unsigned int num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    unsigned char *key_used;
    unsigned char key_temp[SHA224_DIGEST_SIZE];
    unsigned int i;

    if (key_size == SHA224_BLOCK_SIZE) {
        key_used = ( unsigned char * ) key;
        num = SHA224_BLOCK_SIZE;
    } else {
        if (key_size > SHA224_BLOCK_SIZE){
            key_used = key_temp;
            num = SHA224_DIGEST_SIZE;
            hb_sha224(key, key_size, key_used);
        } else { /* key_size > SHA224_BLOCK_SIZE */
            key_used = ( unsigned char * ) key;
            num = key_size;
        }
        fill = SHA224_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha224_init(&ctx->ctx_inside);
    hb_sha224_update(&ctx->ctx_inside, ctx->block_ipad, SHA224_BLOCK_SIZE);

    hb_sha224_init(&ctx->ctx_outside);
    hb_sha224_update(&ctx->ctx_outside, ctx->block_opad,
                  SHA224_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(sha224_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(sha224_ctx));
}

void hb_hmac_sha224_reinit(hmac_sha224_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(sha224_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(sha224_ctx));
}

void hb_hmac_sha224_update(hmac_sha224_ctx *ctx, const void *message,
                        unsigned int message_len)
{
    hb_sha224_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha224_final(hmac_sha224_ctx *ctx, unsigned char *mac,
                       unsigned int mac_size)
{
    unsigned char digest_inside[SHA224_DIGEST_SIZE];
    unsigned char mac_temp[SHA224_DIGEST_SIZE];

    hb_sha224_final(&ctx->ctx_inside, digest_inside);
    hb_sha224_update(&ctx->ctx_outside, digest_inside, SHA224_DIGEST_SIZE);
    hb_sha224_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha224(const void *key, unsigned int key_size,
          const void *message, unsigned int message_len,
          unsigned char *mac, unsigned mac_size)
{
    hmac_sha224_ctx ctx;

    hb_hmac_sha224_init(&ctx, key, key_size);
    hb_hmac_sha224_update(&ctx, message, message_len);
    hb_hmac_sha224_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-256 functions */

void hb_hmac_sha256_init(hmac_sha256_ctx *ctx, const void *keyv,
                      unsigned int key_size)
{
    unsigned int fill;
    unsigned int num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    unsigned char *key_used;
    unsigned char key_temp[SHA256_DIGEST_SIZE];
    unsigned int i;

    if (key_size == SHA256_BLOCK_SIZE) {
        key_used = ( unsigned char * ) key;
        num = SHA256_BLOCK_SIZE;
    } else {
        if (key_size > SHA256_BLOCK_SIZE){
            key_used = key_temp;
            num = SHA256_DIGEST_SIZE;
            hb_sha256(key, key_size, key_used);
        } else { /* key_size > SHA256_BLOCK_SIZE */
            key_used = ( unsigned char * ) key;
            num = key_size;
        }
        fill = SHA256_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha256_init(&ctx->ctx_inside);
    hb_sha256_update(&ctx->ctx_inside, ctx->block_ipad, SHA256_BLOCK_SIZE);

    hb_sha256_init(&ctx->ctx_outside);
    hb_sha256_update(&ctx->ctx_outside, ctx->block_opad,
                  SHA256_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(sha256_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(sha256_ctx));
}

void hb_hmac_sha256_reinit(hmac_sha256_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(sha256_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(sha256_ctx));
}

void hb_hmac_sha256_update(hmac_sha256_ctx *ctx, const void *message,
                        unsigned int message_len)
{
    hb_sha256_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha256_final(hmac_sha256_ctx *ctx, unsigned char *mac,
                       unsigned int mac_size)
{
    unsigned char digest_inside[SHA256_DIGEST_SIZE];
    unsigned char mac_temp[SHA256_DIGEST_SIZE];

    hb_sha256_final(&ctx->ctx_inside, digest_inside);
    hb_sha256_update(&ctx->ctx_outside, digest_inside, SHA256_DIGEST_SIZE);
    hb_sha256_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha256(const void *key, unsigned int key_size,
          const void *message, unsigned int message_len,
          unsigned char *mac, unsigned mac_size)
{
    hmac_sha256_ctx ctx;

    hb_hmac_sha256_init(&ctx, key, key_size);
    hb_hmac_sha256_update(&ctx, message, message_len);
    hb_hmac_sha256_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-384 functions */

void hb_hmac_sha384_init(hmac_sha384_ctx *ctx, const void *keyv,
                      unsigned int key_size)
{
    unsigned int fill;
    unsigned int num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    unsigned char *key_used;
    unsigned char key_temp[SHA384_DIGEST_SIZE];
    unsigned int i;

    if (key_size == SHA384_BLOCK_SIZE) {
        key_used = ( unsigned char * ) key;
        num = SHA384_BLOCK_SIZE;
    } else {
        if (key_size > SHA384_BLOCK_SIZE){
            key_used = key_temp;
            num = SHA384_DIGEST_SIZE;
            hb_sha384(key, key_size, key_used);
        } else { /* key_size > SHA384_BLOCK_SIZE */
            key_used = ( unsigned char * ) key;
            num = key_size;
        }
        fill = SHA384_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha384_init(&ctx->ctx_inside);
    hb_sha384_update(&ctx->ctx_inside, ctx->block_ipad, SHA384_BLOCK_SIZE);

    hb_sha384_init(&ctx->ctx_outside);
    hb_sha384_update(&ctx->ctx_outside, ctx->block_opad,
                  SHA384_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(sha384_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(sha384_ctx));
}

void hb_hmac_sha384_reinit(hmac_sha384_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(sha384_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(sha384_ctx));
}

void hb_hmac_sha384_update(hmac_sha384_ctx *ctx, const void *message,
                        unsigned int message_len)
{
    hb_sha384_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha384_final(hmac_sha384_ctx *ctx, unsigned char *mac,
                       unsigned int mac_size)
{
    unsigned char digest_inside[SHA384_DIGEST_SIZE];
    unsigned char mac_temp[SHA384_DIGEST_SIZE];

    hb_sha384_final(&ctx->ctx_inside, digest_inside);
    hb_sha384_update(&ctx->ctx_outside, digest_inside, SHA384_DIGEST_SIZE);
    hb_sha384_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha384(const void *key, unsigned int key_size,
          const void *message, unsigned int message_len,
          unsigned char *mac, unsigned mac_size)
{
    hmac_sha384_ctx ctx;

    hb_hmac_sha384_init(&ctx, key, key_size);
    hb_hmac_sha384_update(&ctx, message, message_len);
    hb_hmac_sha384_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-512 functions */

void hb_hmac_sha512_init(hmac_sha512_ctx *ctx, const void *keyv,
                      unsigned int key_size)
{
    unsigned int fill;
    unsigned int num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    unsigned char *key_used;
    unsigned char key_temp[SHA512_DIGEST_SIZE];
    unsigned int i;

    if (key_size == SHA512_BLOCK_SIZE) {
        key_used = ( unsigned char * ) key;
        num = SHA512_BLOCK_SIZE;
    } else {
        if (key_size > SHA512_BLOCK_SIZE){
            key_used = key_temp;
            num = SHA512_DIGEST_SIZE;
            hb_sha512(key, key_size, key_used);
        } else { /* key_size > SHA512_BLOCK_SIZE */
            key_used = ( unsigned char * ) key;
            num = key_size;
        }
        fill = SHA512_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha512_init(&ctx->ctx_inside);
    hb_sha512_update(&ctx->ctx_inside, ctx->block_ipad, SHA512_BLOCK_SIZE);

    hb_sha512_init(&ctx->ctx_outside);
    hb_sha512_update(&ctx->ctx_outside, ctx->block_opad,
                  SHA512_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(sha512_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(sha512_ctx));
}

void hb_hmac_sha512_reinit(hmac_sha512_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(sha512_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(sha512_ctx));
}

void hb_hmac_sha512_update(hmac_sha512_ctx *ctx, const void *message,
                        unsigned int message_len)
{
    hb_sha512_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha512_final(hmac_sha512_ctx *ctx, unsigned char *mac,
                       unsigned int mac_size)
{
    unsigned char digest_inside[SHA512_DIGEST_SIZE];
    unsigned char mac_temp[SHA512_DIGEST_SIZE];

    hb_sha512_final(&ctx->ctx_inside, digest_inside);
    hb_sha512_update(&ctx->ctx_outside, digest_inside, SHA512_DIGEST_SIZE);
    hb_sha512_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha512(const void *key, unsigned int key_size,
          const void *message, unsigned int message_len,
          unsigned char *mac, unsigned mac_size)
{
    hmac_sha512_ctx ctx;

    hb_hmac_sha512_init(&ctx, key, key_size);
    hb_hmac_sha512_update(&ctx, message, message_len);
    hb_hmac_sha512_final(&ctx, mac, mac_size);
}
