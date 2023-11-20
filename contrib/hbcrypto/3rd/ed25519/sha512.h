#ifndef SHA512_H
#define SHA512_H

#include "hbdefs.h"

#include "fixedint.h"

/* state */
#if defined( HB_USE_CORE_SHA512 )
#  include "hbcrypto.h"
#  define sha512_context hb_sha512_ctx
#  define sha512_init    hb_sha512_init
#  define sha512_final   hb_sha512_final
#  define sha512_update  hb_sha512_update
#  define sha512         hb_sha512
#else
#  define uint64_t       HB_U64
typedef struct sha512_context_ {
    uint64_t  length, state[8];
    size_t curlen;
    unsigned char buf[128];
} sha512_context;


int sha512_init(sha512_context * md);
int sha512_final(sha512_context * md, unsigned char *out);
int sha512_update(sha512_context * md, const unsigned char *in, size_t inlen);
int sha512(const unsigned char *message, size_t message_len, unsigned char *out);
#endif

#endif
