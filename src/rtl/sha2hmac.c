/*-
 * HMAC-SHA-224/256/384/512 implementation
 * Last update: 2005-06-15
 * Issue date:  2005-06-15
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

#include "hbapi.h"
#include "hbcrypto.h"

/* HMAC-SHA-224 functions */

void hb_hmac_sha224_init(hb_hmac_sha224_ctx *ctx, const void *keyv,
                         HB_SIZE key_size)
{
    HB_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[HB_SHA224_DIGEST_SIZE];
    HB_SIZE i;

    if (key_size == HB_SHA224_BLOCK_SIZE) {
        key_used = key;
        num = HB_SHA224_BLOCK_SIZE;
    } else {
        HB_SIZE fill;
        if (key_size > HB_SHA224_BLOCK_SIZE){
            hb_sha224(key, key_size, key_temp);
            key_used = key_temp;
            num = HB_SHA224_DIGEST_SIZE;
        } else { /* key_size > HB_SHA224_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = HB_SHA224_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha224_init(&ctx->ctx_inside);
    hb_sha224_update(&ctx->ctx_inside, ctx->block_ipad, HB_SHA224_BLOCK_SIZE);

    hb_sha224_init(&ctx->ctx_outside);
    hb_sha224_update(&ctx->ctx_outside, ctx->block_opad,
                     HB_SHA224_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(hb_sha224_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(hb_sha224_ctx));
}

void hb_hmac_sha224_reinit(hb_hmac_sha224_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(hb_sha224_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(hb_sha224_ctx));
}

void hb_hmac_sha224_update(hb_hmac_sha224_ctx *ctx, const void *message,
                           HB_SIZE message_len)
{
    hb_sha224_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha224_final(hb_hmac_sha224_ctx *ctx, unsigned char *mac,
                          HB_SIZE mac_size)
{
    unsigned char digest_inside[HB_SHA224_DIGEST_SIZE];
    unsigned char mac_temp[HB_SHA224_DIGEST_SIZE];

    hb_sha224_final(&ctx->ctx_inside, digest_inside);
    hb_sha224_update(&ctx->ctx_outside, digest_inside, HB_SHA224_DIGEST_SIZE);
    hb_sha224_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha224(const void *key, HB_SIZE key_size,
          const void *message, HB_SIZE message_len,
          unsigned char *mac, HB_SIZE mac_size)
{
    hb_hmac_sha224_ctx ctx;

    hb_hmac_sha224_init(&ctx, key, key_size);
    hb_hmac_sha224_update(&ctx, message, message_len);
    hb_hmac_sha224_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-256 functions */

void hb_hmac_sha256_init(hb_hmac_sha256_ctx *ctx, const void *keyv,
                         HB_SIZE key_size)
{
    HB_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[HB_SHA256_DIGEST_SIZE];
    HB_SIZE i;

    if (key_size == HB_SHA256_BLOCK_SIZE) {
        key_used = key;
        num = HB_SHA256_BLOCK_SIZE;
    } else {
        HB_SIZE fill;
        if (key_size > HB_SHA256_BLOCK_SIZE){
            hb_sha256(key, key_size, key_temp);
            key_used = key_temp;
            num = HB_SHA256_DIGEST_SIZE;
        } else { /* key_size > HB_SHA256_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = HB_SHA256_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha256_init(&ctx->ctx_inside);
    hb_sha256_update(&ctx->ctx_inside, ctx->block_ipad, HB_SHA256_BLOCK_SIZE);

    hb_sha256_init(&ctx->ctx_outside);
    hb_sha256_update(&ctx->ctx_outside, ctx->block_opad,
                     HB_SHA256_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(hb_sha256_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(hb_sha256_ctx));
}

void hb_hmac_sha256_reinit(hb_hmac_sha256_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(hb_sha256_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(hb_sha256_ctx));
}

void hb_hmac_sha256_update(hb_hmac_sha256_ctx *ctx, const void *message,
                           HB_SIZE message_len)
{
    hb_sha256_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha256_final(hb_hmac_sha256_ctx *ctx, unsigned char *mac,
                          HB_SIZE mac_size)
{
    unsigned char digest_inside[HB_SHA256_DIGEST_SIZE];
    unsigned char mac_temp[HB_SHA256_DIGEST_SIZE];

    hb_sha256_final(&ctx->ctx_inside, digest_inside);
    hb_sha256_update(&ctx->ctx_outside, digest_inside, HB_SHA256_DIGEST_SIZE);
    hb_sha256_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha256(const void *key, HB_SIZE key_size,
          const void *message, HB_SIZE message_len,
          unsigned char *mac, HB_SIZE mac_size)
{
    hb_hmac_sha256_ctx ctx;

    hb_hmac_sha256_init(&ctx, key, key_size);
    hb_hmac_sha256_update(&ctx, message, message_len);
    hb_hmac_sha256_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-384 functions */

void hb_hmac_sha384_init(hb_hmac_sha384_ctx *ctx, const void *keyv,
                         HB_SIZE key_size)
{
    HB_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[HB_SHA384_DIGEST_SIZE];
    HB_SIZE i;

    if (key_size == HB_SHA384_BLOCK_SIZE) {
        key_used = key;
        num = HB_SHA384_BLOCK_SIZE;
    } else {
        HB_SIZE fill;
        if (key_size > HB_SHA384_BLOCK_SIZE){
            hb_sha384(key, key_size, key_temp);
            key_used = key_temp;
            num = HB_SHA384_DIGEST_SIZE;
        } else { /* key_size > HB_SHA384_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = HB_SHA384_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha384_init(&ctx->ctx_inside);
    hb_sha384_update(&ctx->ctx_inside, ctx->block_ipad, HB_SHA384_BLOCK_SIZE);

    hb_sha384_init(&ctx->ctx_outside);
    hb_sha384_update(&ctx->ctx_outside, ctx->block_opad,
                     HB_SHA384_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(hb_sha384_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(hb_sha384_ctx));
}

void hb_hmac_sha384_reinit(hb_hmac_sha384_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(hb_sha384_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(hb_sha384_ctx));
}

void hb_hmac_sha384_update(hb_hmac_sha384_ctx *ctx, const void *message,
                           HB_SIZE message_len)
{
    hb_sha384_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha384_final(hb_hmac_sha384_ctx *ctx, unsigned char *mac,
                          HB_SIZE mac_size)
{
    unsigned char digest_inside[HB_SHA384_DIGEST_SIZE];
    unsigned char mac_temp[HB_SHA384_DIGEST_SIZE];

    hb_sha384_final(&ctx->ctx_inside, digest_inside);
    hb_sha384_update(&ctx->ctx_outside, digest_inside, HB_SHA384_DIGEST_SIZE);
    hb_sha384_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha384(const void *key, HB_SIZE key_size,
          const void *message, HB_SIZE message_len,
          unsigned char *mac, HB_SIZE mac_size)
{
    hb_hmac_sha384_ctx ctx;

    hb_hmac_sha384_init(&ctx, key, key_size);
    hb_hmac_sha384_update(&ctx, message, message_len);
    hb_hmac_sha384_final(&ctx, mac, mac_size);
}

/* HMAC-SHA-512 functions */

void hb_hmac_sha512_init(hb_hmac_sha512_ctx *ctx, const void *keyv,
                         HB_SIZE key_size)
{
    HB_SIZE num;

    const unsigned char *key = ( const unsigned char * ) keyv;
    const unsigned char *key_used;
    unsigned char key_temp[HB_SHA512_DIGEST_SIZE];
    HB_SIZE i;

    if (key_size == HB_SHA512_BLOCK_SIZE) {
        key_used = key;
        num = HB_SHA512_BLOCK_SIZE;
    } else {
        HB_SIZE fill;
        if (key_size > HB_SHA512_BLOCK_SIZE){
            hb_sha512(key, key_size, key_temp);
            key_used = key_temp;
            num = HB_SHA512_DIGEST_SIZE;
        } else { /* key_size > HB_SHA512_BLOCK_SIZE */
            key_used = key;
            num = key_size;
        }
        fill = HB_SHA512_BLOCK_SIZE - num;

        memset(ctx->block_ipad + num, 0x36, fill);
        memset(ctx->block_opad + num, 0x5c, fill);
    }

    for (i = 0; i < num; i++) {
        ctx->block_ipad[i] = key_used[i] ^ 0x36;
        ctx->block_opad[i] = key_used[i] ^ 0x5c;
    }

    hb_sha512_init(&ctx->ctx_inside);
    hb_sha512_update(&ctx->ctx_inside, ctx->block_ipad, HB_SHA512_BLOCK_SIZE);

    hb_sha512_init(&ctx->ctx_outside);
    hb_sha512_update(&ctx->ctx_outside, ctx->block_opad,
                     HB_SHA512_BLOCK_SIZE);

    /* for hmac_reinit */
    memcpy(&ctx->ctx_inside_reinit, &ctx->ctx_inside,
           sizeof(hb_sha512_ctx));
    memcpy(&ctx->ctx_outside_reinit, &ctx->ctx_outside,
           sizeof(hb_sha512_ctx));
}

void hb_hmac_sha512_reinit(hb_hmac_sha512_ctx *ctx)
{
    memcpy(&ctx->ctx_inside, &ctx->ctx_inside_reinit,
           sizeof(hb_sha512_ctx));
    memcpy(&ctx->ctx_outside, &ctx->ctx_outside_reinit,
           sizeof(hb_sha512_ctx));
}

void hb_hmac_sha512_update(hb_hmac_sha512_ctx *ctx, const void *message,
                           HB_SIZE message_len)
{
    hb_sha512_update(&ctx->ctx_inside, message, message_len);
}

void hb_hmac_sha512_final(hb_hmac_sha512_ctx *ctx, unsigned char *mac,
                          HB_SIZE mac_size)
{
    unsigned char digest_inside[HB_SHA512_DIGEST_SIZE];
    unsigned char mac_temp[HB_SHA512_DIGEST_SIZE];

    hb_sha512_final(&ctx->ctx_inside, digest_inside);
    hb_sha512_update(&ctx->ctx_outside, digest_inside, HB_SHA512_DIGEST_SIZE);
    hb_sha512_final(&ctx->ctx_outside, mac_temp);
    memcpy(mac, mac_temp, mac_size);
}

void hb_hmac_sha512(const void *key, HB_SIZE key_size,
          const void *message, HB_SIZE message_len,
          unsigned char *mac, HB_SIZE mac_size)
{
    hb_hmac_sha512_ctx ctx;

    hb_hmac_sha512_init(&ctx, key, key_size);
    hb_hmac_sha512_update(&ctx, message, message_len);
    hb_hmac_sha512_final(&ctx, mac, mac_size);
}
