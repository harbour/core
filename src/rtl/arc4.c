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

#include "arc4.h"
#include "hbdate.h"
#include "hbthread.h"

/* XXX: Check and possibly extend this to other Unix-like platforms */
#if ( defined( HB_OS_BSD ) && ! defined( HB_OS_DARWIN ) ) || \
   ( defined( HB_OS_LINUX ) && ! defined ( HB_OS_ANDROID ) && ! defined ( __WATCOMC__ ) )
#  define HAVE_SYS_SYSCTL_H
#  define HAVE_DECL_CTL_KERN
#  define HAVE_DECL_KERN_RANDOM
#  if defined( HB_OS_LINUX )
#     define HAVE_DECL_RANDOM_UUID
#  endif
#endif

#if defined( HB_OS_WIN )
#  if ! defined( __TINYC__ )
#     include <wincrypt.h>
#     include <process.h>
#  endif
#elif defined( HB_OS_DOS ) || defined( HB_OS_OS2 )
#  include <sys/types.h>
#  include <process.h>
#else
#  if ! defined( __WATCOMC__ )
#     include <sys/param.h>
#  endif
#  include <sys/time.h>
#  include <sys/types.h>
#  ifdef HAVE_SYS_SYSCTL_H
#     include <sys/sysctl.h>
#     if ! defined( HB_OS_LINUX ) && defined( KERN_ARND )
#        define HAVE_DECL_KERN_ARND
#     endif
#  endif
#  include <fcntl.h>
#  include <unistd.h>
#endif

#include <limits.h>
#include <stdlib.h>
#include <string.h>

/* Add platform entropy 32 bytes (256 bits) at a time. */
#define ADD_ENTROPY          32

/* Re-seed from the platform RNG after generating this many bytes. */
#define BYTES_BEFORE_RESEED  1600000

struct arc4_stream
{
   HB_U8 i;
   HB_U8 j;
   HB_U8 s[ 256 ];
};

#if ! defined( HB_OS_UNIX )
#  define NO_PID_CHECK
#else
static pid_t arc4_stir_pid;
#endif

static int rs_initialized;
static struct arc4_stream rs;
static HB_I32 arc4_count;

static HB_CRITICAL_NEW( arc4_lock );
#define _ARC4_LOCK()    hb_threadEnterCriticalSection( &arc4_lock )
#define _ARC4_UNLOCK()  hb_threadLeaveCriticalSection( &arc4_lock )

#if defined( __BORLANDC__ ) && defined( _HB_INLINE_ )
#undef _HB_INLINE_
#define _HB_INLINE_
#endif

static _HB_INLINE_ HB_U8 arc4_getbyte( void );

static _HB_INLINE_ void arc4_init( void )
{
   int n;

   for( n = 0; n < 256; ++n )
      rs.s[ n ] = ( HB_U8 ) n;

   rs.i = rs.j = 0;
}

static _HB_INLINE_ void arc4_addrandom( const HB_U8 * dat, int datlen )
{
   int   n;
   HB_U8 si;

   rs.i--;
   for( n = 0; n < 256; ++n )
   {
      rs.i         = ( rs.i + 1 );
      si           = rs.s[ rs.i ];
      rs.j         = rs.j + si + dat[ n % datlen ];
      rs.s[ rs.i ] = rs.s[ rs.j ];
      rs.s[ rs.j ] = si;
   }
   rs.j = rs.i;
}

#if defined( HB_OS_UNIX )
static HB_ISIZ read_all( int fd, HB_U8 * buf, size_t count )
{
   HB_SIZE numread = 0;
   HB_ISIZ result;

   while( numread < count )
   {
      result = read( fd, buf + numread, count - numread );

      if( result < 0 )
         return -1;
      else if( result == 0 )
         break;

      numread += result;
   }

   return ( HB_ISIZ ) numread;
}
#endif /* HB_OS_UNIX */

#if defined( HB_OS_WIN ) && ! defined( __DMC__ ) && ! defined( __TINYC__ )

#define TRY_SEED_MS_CRYPTOAPI
static int arc4_seed_win( void )
{
   /* This is adapted from Tor's crypto_seed_rng() */
   static int        provider_set = 0;
   static HCRYPTPROV provider;
   unsigned char     buf[ ADD_ENTROPY ];

   if( ! provider_set &&
       ! CryptAcquireContext( &provider, NULL, NULL, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT ) &&
       GetLastError() != ( DWORD ) NTE_BAD_KEYSET )
      return -1;

   provider_set = 1;

   if( ! CryptGenRandom( provider, sizeof( buf ), buf ) )
      return -1;

   arc4_addrandom( buf, sizeof( buf ) );
   memset( buf, 0, sizeof( buf ) );

   return 0;
}
#endif /* HB_OS_WIN */

#if defined( HAVE_SYS_SYSCTL_H )

#if defined( HAVE_DECL_CTL_KERN ) && defined( HAVE_DECL_KERN_RANDOM ) && \
   defined( HAVE_DECL_RANDOM_UUID )

#define TRY_SEED_SYSCTL_LINUX
static int arc4_seed_sysctl_linux( void )
{
   /*
    * Based on code by William Ahern, this function tries to use the
    * RANDOM_UUID sysctl to get entropy from the kernel. This can work
    * even if /dev/urandom is inaccessible for some reason (e.g., we're
    * running in a chroot).
    */
   int          mib[] = { CTL_KERN, KERN_RANDOM, RANDOM_UUID };
   HB_U8        buf[ ADD_ENTROPY ];
   size_t       len, n;
   unsigned int i;
   int          any_set;

   memset( buf, 0, sizeof( buf ) );

   for( len = 0; len < sizeof( buf ); len += n )
   {
      n = sizeof( buf ) - len;

      if( sysctl( mib, 3, &buf[ len ], &n, NULL, 0 ) != 0 )
         return -1;
   }

   /* make sure that the buffer actually got set. */
   for( i = 0, any_set = 0; i < sizeof( buf ); ++i )
      any_set |= buf[ i ];

   if( ! any_set )
      return -1;

   arc4_addrandom( buf, sizeof( buf ) );
   memset( buf, 0, sizeof( buf ) );

   return 0;
}
#endif /* HAVE_DECL_CTL_KERN && HAVE_DECL_KERN_RANDOM && HAVE_DECL_RANDOM_UUID */

#if defined( HAVE_DECL_CTL_KERN ) && defined( HAVE_DECL_KERN_ARND )

#define TRY_SEED_SYSCTL_BSD
static int arc4_seed_sysctl_bsd( void )
{
   /*
    * Based on code from William Ahern and from OpenBSD, this function
    * tries to use the KERN_ARND syscall to get entropy from the kernel.
    * This can work even if /dev/urandom is inaccessible for some reason
    * (e.g., we're running in a chroot).
    */
   int    mib[] = { CTL_KERN, KERN_ARND };
   HB_U8  buf[ ADD_ENTROPY ];
   size_t len, n;
   int    i, any_set;

   memset( buf, 0, sizeof( buf ) );

   len = sizeof( buf );
   if( sysctl( mib, 2, buf, &len, NULL, 0 ) == -1 )
   {
      for( len = 0; len < sizeof( buf ); len += sizeof( unsigned ) )
      {
         n = sizeof( unsigned );

         if( n + len > sizeof( buf ) )
            n = len - sizeof( buf );

         if( sysctl( mib, 2, &buf[ len ], &n, NULL, 0 ) == -1 )
            return -1;
      }
   }

   /* make sure that the buffer actually got set. */
   for( i = any_set = 0; i < ( int ) sizeof( buf ); ++i )
      any_set |= buf[ i ];

   if( ! any_set )
      return -1;

   arc4_addrandom( buf, sizeof( buf ) );
   memset( buf, 0, sizeof( buf ) );

   return 0;
}
#endif   /* HAVE_DECL_CTL_KERN && HAVE_DECL_KERN_ARND */

#endif   /* defined( HAVE_SYS_SYSCTL_H ) */

#if defined( HB_OS_LINUX )

#define TRY_SEED_PROC_SYS_KERNEL_RANDOM_UUID
static _HB_INLINE_ int hex_char_to_int( char c )
{
   switch( c )
   {
      case '0':           return 0;
      case '1':           return 1;
      case '2':           return 2;
      case '3':           return 3;
      case '4':           return 4;
      case '5':           return 5;
      case '6':           return 6;
      case '7':           return 7;
      case '8':           return 8;
      case '9':           return 9;
      case 'A': case 'a': return 10;
      case 'B': case 'b': return 11;
      case 'C': case 'c': return 12;
      case 'D': case 'd': return 13;
      case 'E': case 'e': return 14;
      case 'F': case 'f': return 15;
   }

   return -1;
}

static int arc4_seed_proc_sys_kernel_random_uuid( void )
{
   /*
    * Occasionally, somebody will make /proc/sys accessible in a chroot,
    * but not /dev/urandom.  Let's try /proc/sys/kernel/random/uuid.
    * Its format is stupid, so we need to decode it from hex.
    */
   int   fd;
   char  buf[ 128 ];
   HB_U8 entropy[ 64 ];
   int   bytes, n, i, nybbles;

   for( bytes = 0; bytes < ADD_ENTROPY; )
   {
      fd = open( "/proc/sys/kernel/random/uuid", O_RDONLY, 0 );
      if( fd < 0 )
         return -1;

      n = read( fd, buf, sizeof( buf ) );
      close( fd );

      if( n <= 0 )
         return -1;

      memset( entropy, 0, sizeof( entropy ) );
      for( i = nybbles = 0; i < n; ++i )
      {
         if( HB_ISXDIGIT( buf[ i ] ) )
         {
            int nyb = hex_char_to_int( buf[ i ] );

            if( nybbles & 1 )
               entropy[ nybbles / 2 ] |= nyb;
            else
               entropy[ nybbles / 2 ] |= nyb << 4;

            ++nybbles;
         }
      }
      if( nybbles < 2 )
         return -1;

      arc4_addrandom( entropy, nybbles / 2 );
      bytes += nybbles / 2;
   }

   memset( entropy, 0, sizeof( entropy ) );
   memset( buf, 0, sizeof( buf ) );

   return 0;
}
#endif /* HB_OS_LINUX */

#if defined( HB_OS_UNIX )

#define TRY_SEED_URANDOM
static int arc4_seed_urandom( void )
{
   /* This is adapted from Tor's crypto_seed_rng() */
   static const char * filenames[] = {
      "/dev/srandom",
      "/dev/urandom",
      "/dev/random",
      NULL
   };
   HB_U8   buf[ ADD_ENTROPY ];
   int     fd, i;
   HB_SIZE n;

   for( i = 0; filenames[ i ]; ++i )
   {
      fd = open( filenames[ i ], O_RDONLY, 0 );
      if( fd < 0 )
         continue;

      n = read_all( fd, buf, sizeof( buf ) );
      close( fd );

      if( n != sizeof( buf ) )
         return -1;

      arc4_addrandom( buf, sizeof( buf ) );
      memset( buf, 0, sizeof( buf ) );

      return 0;
   }

   return -1;
}
#endif /* HB_OS_UNIX */

static int arc4_seed_rand( void )
{
   HB_SIZE i;
   HB_U8   buf[ ADD_ENTROPY ];

   srand( ( unsigned ) hb_dateMilliSeconds() );

   for( i = 0; i < sizeof( buf ); i++ )
      buf[ i ] = ( HB_U8 ) ( rand() % 256 );  /* not biased */

   arc4_addrandom( buf, sizeof( buf ) );
   memset( buf, 0, sizeof( buf ) );

   return 0;
}

static void arc4_seed( void )
{
   int ok = 0;

   /*
    * We try every method that might work, and don't give up even if one
    * does seem to work. There's no real harm in over-seeding, and if
    * one of these sources turns out to be broken, that would be bad.
    */

#if defined( TRY_SEED_MS_CRYPTOAPI )
   if( arc4_seed_win() == 0 )
      ok = 1;
#endif

#if defined( TRY_SEED_URANDOM )
   if( arc4_seed_urandom() == 0 )
      ok = 1;
#endif

#if defined( TRY_SEED_PROC_SYS_KERNEL_RANDOM_UUID )
   if( arc4_seed_proc_sys_kernel_random_uuid() == 0 )
      ok = 1;
#endif

#if defined( TRY_SEED_SYSCTL_LINUX )
   /*
    * Apparently Linux is deprecating sysctl, and spewing warning
    * messages when you try to use it. To avoid dmesg spamming,
    * only try this if no previous method worked.
    */
   if( ! ok && arc4_seed_sysctl_linux() == 0 )
      ok = 1;
#endif

#if defined( TRY_SEED_SYSCTL_BSD )
   if( arc4_seed_sysctl_bsd() == 0 )
      ok = 1;
#endif

   /*
    * If nothing else worked or there is no specific seeding
    * method for the current platform, fall back to rand().
    * In case an existing platform-specific method had a
    * (transient) failure, it will be re-tried at the next
    * seeding cycle.
    */
   if( ! ok )
      arc4_seed_rand();
}

static void arc4_stir( void )
{
   int i;

   if( ! rs_initialized )
   {
      arc4_init();
      rs_initialized = 1;
   }

   arc4_seed();

   /*
    * Discard early keystream, as per recommendations in
    * "Weaknesses in the Key Scheduling Algorithm of RC4" by
    * Scott Fluhrer, Itsik Mantin, and Adi Shamir.
    * http://www.wisdom.weizmann.ac.il/~itsik/RC4/Papers/Rc4_ksa.ps
    *
    * Ilya Mironov's "(Not So) Random Shuffles of RC4" suggests that
    * we drop at least 2*256 bytes, with 12*256 as a conservative
    * value.
    *
    * RFC4345 says to drop 6*256.
    *
    * At least some versions of this code drop 4*256, in a mistaken
    * belief that "words" in the Fluhrer/Mantin/Shamir paper refers
    * to processor words.
    *
    * We add another sect to the cargo cult, and choose 12*256.
    */
   for( i = 0; i < 12 * 256; i++ )
      ( void ) arc4_getbyte();

   arc4_count = BYTES_BEFORE_RESEED;
}

static void arc4_stir_if_needed( void )
{
#if defined( NO_PID_CHECK )
   if( arc4_count <= 0 || ! rs_initialized )
      arc4_stir();
#else
   pid_t pid = getpid();

   if( arc4_count <= 0 || ! rs_initialized || arc4_stir_pid != pid )
   {
      arc4_stir_pid = pid;
      arc4_stir();
   }
#endif
}

static _HB_INLINE_ HB_U8 arc4_getbyte( void )
{
   HB_U8 si, sj;

   rs.i         = rs.i + 1;
   si           = rs.s[ rs.i ];
   rs.j         = rs.j + si;
   sj           = rs.s[ rs.j ];
   rs.s[ rs.i ] = sj;
   rs.s[ rs.j ] = si;

   return rs.s[ ( si + sj ) & 0xff ];
}

static _HB_INLINE_ HB_U32 arc4_getword( void )
{
   HB_U32 val;

   val  = arc4_getbyte() << 24;
   val |= arc4_getbyte() << 16;
   val |= arc4_getbyte() << 8;
   val |= arc4_getbyte();

   return val;
}

#if 0
/*
 * These two are part of the original arc4random API, but Harbour does not
 * make use of either of them.
 */
void arc4random_stir( void )
{
   _ARC4_LOCK();
   arc4_stir();
   _ARC4_UNLOCK();
}

void arc4random_addrandom( const unsigned char * dat, int datlen )
{
   int j;

   _ARC4_LOCK();
   if( ! rs_initialized )
      arc4_stir();

   for( j = 0; j < datlen; j += 256 )
   {
      /*
       * arc4_addrandom() ignores all but the first 256 bytes of
       * its input.  We want to make sure to look at ALL the
       * data in 'dat', just in case the user is doing something
       * crazy like passing us all the files in /var/log.
       */
      arc4_addrandom( dat + j, datlen - j );
   }
   _ARC4_UNLOCK();
}
#endif

HB_U32 hb_arc4random( void )
{
   HB_U32 val;

   _ARC4_LOCK();

   arc4_count -= 4;
   arc4_stir_if_needed();
   val = arc4_getword();

   _ARC4_UNLOCK();

   return val;
}

void hb_arc4random_buf( void * _buf, HB_SIZE n )
{
   HB_U8 * buf = ( HB_U8 * ) _buf;

   _ARC4_LOCK();

   arc4_stir_if_needed();

   while( n-- )
   {
      if( --arc4_count <= 0 )
         arc4_stir();

      buf[ n ] = arc4_getbyte();
   }

   _ARC4_UNLOCK();
}

/*
 * Calculate a uniformly distributed random number less than upper_bound
 * avoiding "modulo bias".
 *
 * Uniformity is achieved by generating new random numbers until the one
 * returned is outside the range [0, 2**32 % upper_bound).  This
 * guarantees the selected random number will be inside
 * [2**32 % upper_bound, 2**32) which maps back to [0, upper_bound)
 * after reduction modulo upper_bound.
 */
HB_U32 hb_arc4random_uniform( HB_U32 upper_bound )
{
   HB_U32 r, min;

   if( upper_bound < 2 )
      return 0;

#if ( HB_U32_MAX > 0xffffffffUL )
   min = 0x100000000UL % upper_bound;
#else
   /* Calculate (2**32 % upper_bound) avoiding 64-bit math */
   if( upper_bound > 0x80000000 )
   {
      /* 2**32 - upper_bound */
      min = 1 + ~upper_bound;
   }
   else
   {
      /* (2**32 - (x * 2)) % x == 2**32 % x when x <= 2**31 */
      min = ( ( 0xffffffff - ( upper_bound * 2 ) ) + 1 ) % upper_bound;
   }
#endif

   /*
    * This could theoretically loop forever but each retry has
    * p > 0.5 (worst case, usually far better) of selecting a
    * number inside the range we need, so it should rarely need
    * to re-roll.
    */
   for( ;; )
   {
      r = hb_arc4random();
      if( r >= min )
         break;
   }

   return r % upper_bound;
}
