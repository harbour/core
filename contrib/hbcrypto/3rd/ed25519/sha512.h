#ifndef SHA512_H
#define SHA512_H

#include "hbcrypto.h"

#include "fixedint.h"

/* state */
#define sha512_context hb_sha512_ctx
#define sha512_init    hb_sha512_init
#define sha512_final   hb_sha512_final
#define sha512_update  hb_sha512_update
#define sha512         hb_sha512

#endif
