/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   07 Feb 1994 20:11:50   GLENN
 * Ted re-wrote to make it CPMI compliant.
 *
 *    Rev 1.2   15 Aug 1991 23:08:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:52   GLENN
 * Nanforum Toolkit
 *
 *
 */

#include "hbapi.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
   #include <conio.h>

   #if defined( __DJGPP__ )
      #include <sys/farptr.h>
      #include <dpmi.h>
   #endif

   #if ! defined( MK_FP )
      #define MK_FP( seg, off ) \
   ( ( void FAR * ) ( ( ( unsigned long ) ( seg ) << 4 ) | ( unsigned ) ( off ) ) )
   #endif

   #if defined( __WATCOMC__ )
      #define outportb  outp
      #define inportb   inp
   #endif

   #if defined( __WATCOMC__ ) && defined( __386__ )
      #define HB_PEEK_BYTE( s, o )     ( *( ( HB_UCHAR * ) ( ( ( s ) << 4 ) | ( o ) ) ) )
      #define HB_POKE_BYTE( s, o, b )  ( *( ( HB_UCHAR * ) ( ( ( s ) << 4 ) | ( o ) ) ) = ( HB_UCHAR ) ( b ) )
   #elif defined( __DJGPP__ )
      #define HB_PEEK_BYTE( s, o )     _farpeekb( ( s ), ( o ) )
      #define HB_POKE_BYTE( s, o, b )  _farpokeb( ( s ), ( o ), ( b ) )
   #else
      #define HB_PEEK_BYTE( s, o )     ( *( ( HB_UCHAR FAR * ) MK_FP( ( s ), ( o ) ) ) )
      #define HB_POKE_BYTE( s, o, b )  ( *( ( HB_UCHAR FAR * ) MK_FP( ( s ), ( o ) ) ) = ( HB_UCHAR ) ( b ) )
   #endif
#endif

HB_FUNC( FT_PEEK )
{
#if defined( HB_OS_DOS )
   {
      HB_U16 nSeg = ( HB_U16 ) hb_parni( 1 );
      HB_U16 nOff = ( HB_U16 ) hb_parni( 2 );

      hb_retni( HB_PEEK_BYTE( nSeg, nOff ) );
   }
#else
   hb_retni( -1 );
#endif
}

HB_FUNC( FT_POKE )
{
#if defined( HB_OS_DOS )
   {
      HB_U16 nSeg = ( HB_U16 ) hb_parni( 1 );
      HB_U16 nOff = ( HB_U16 ) hb_parni( 2 );

      HB_POKE_BYTE( nSeg, nOff, ( HB_U8 ) hb_parni( 3 ) );

      hb_retl( HB_TRUE );
   }
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( FT_INP )
{
#if defined( HB_OS_DOS )
   hb_retni( ( HB_U8 ) inportb( ( HB_U8 ) hb_parni( 1 ) ) );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( FT_OUTP )
{
#if defined( HB_OS_DOS )
   outportb( ( HB_U8 ) hb_parni( 1 ),
             ( HB_U8 ) hb_parni( 2 ) );

   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}
