/*
 * $Id$
 */

/*
 * Portable ARC4 PRNG, based on arc4random.c from Libevent.
 * Harbour adaptation Copyright 2011 Tamas TEVESZ <ice@extreme.hu>
 */

/*
 * Portable arc4random.c based on arc4random.c from OpenBSD.
 * Portable version by Chris Davis, adapted for Libevent by Nick Mathewson
 * Copyright (c) 2010 Chris Davis, Niels Provos, and Nick Mathewson
 */

/*
 * Copyright (c) 1996, David Mazieres <dm@uun.org>
 * Copyright (c) 2008, Damien Miller <djm@openbsd.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Arc4 random number generator for OpenBSD.
 *
 * This code is derived from section 17.1 of Applied Cryptography,
 * second edition, which describes a stream cipher allegedly
 * compatible with RSA Labs "RC4" cipher (the actual description of
 * which is a trade secret). The same algorithm is used as a stream
 * cipher called "arcfour" in Tatu Ylonen's ssh package.
 *
 * Here the stream cipher has been modified always to include the time
 * when initializing the state.  That makes it impossible to
 * regenerate the same random sequence twice, so this can't be used
 * for encryption, but will generate good random numbers.
 *
 * RC4 is a registered trademark of RSA Laboratories.
 */

/*
 * Bit of a guesswork; possibly needs to be extended to other platforms,
 * but on UNIX-ish systems, seeding will fall back to using the
 * /dev/random-variants.
 */

#ifndef __ARC4_H__
#define __ARC4_H__

#include "hbdefs.h"

#ifdef __cplusplus
extern "C" {
#endif

HB_U32 hb_arc4random( void );
void hb_arc4random_buf( void * _buf, HB_SIZE n );
HB_U32 hb_arc4random_uniform( HB_U32 upper_bound );

#ifdef __cplusplus
}
#endif

#endif /* __ARC4_H__ */
