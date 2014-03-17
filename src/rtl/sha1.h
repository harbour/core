/*
 * sha.h
 *
 * Originally taken from the public domain SHA1 implementation
 * written by by Steve Reid <steve@edmweb.com>
 *
 * Modified by Aaron D. Gifford <agifford@infowest.com>
 *
 * NO COPYRIGHT - THIS IS 100% IN THE PUBLIC DOMAIN
 *
 * The original unmodified version is available at:
 *    http://www.nic.funet.fi/pub/crypt/hash/sha/sha1.c
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR(S) OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef __SHA1_H__
#define __SHA1_H__

#include "hbdefs.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Define this if your machine is LITTLE_ENDIAN, otherwise #undef it: */
/* Disabled to avoid conflicts with the same macro defined in system header
 * files in some OS-es. HB_LITTLE_ENDIAN used instead.
 */
/* #define LITTLE_ENDIAN */

/* Make sure you define these types for your architecture: */
typedef unsigned int sha1_quadbyte;     /* 4 byte type */
typedef unsigned char sha1_byte;        /* single byte type */

/*
 * Be sure to get the above definitions right.  For instance, on my
 * x86 based FreeBSD box, I define LITTLE_ENDIAN and use the type
 * "unsigned long" for the quadbyte.  On FreeBSD on the Alpha, however,
 * while I still use LITTLE_ENDIAN, I must define the quadbyte type
 * as "unsigned int" instead.
 */

#define SHA1_BLOCK_LENGTH       64
#define SHA1_DIGEST_LENGTH      20

/* The SHA1 structure: */
typedef struct _SHA_CTX {
    sha1_quadbyte   state[5];
    sha1_quadbyte   count[2];
    sha1_byte       buffer[SHA1_BLOCK_LENGTH];
} SHA_CTX;

#ifndef NOPROTO
void hb_SHA1_Init(SHA_CTX *context);
void hb_SHA1_Update(SHA_CTX *context, const void *data, unsigned int len);
void hb_SHA1_Final(sha1_byte digest[SHA1_DIGEST_LENGTH], SHA_CTX* context);
#else
void hb_SHA1_Init();
void hb_SHA1_Update();
void hb_SHA1_Final();
#endif

#ifdef  __cplusplus
}
#endif

#endif
