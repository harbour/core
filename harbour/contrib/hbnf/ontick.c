/*
 * $Id$
 */

/*
 * File......: ontick.c
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.0   01 Jan 1995 03:01:00   TED
 * Initial release
 */

#include "hbapi.h"
#include "hbapiitm.h"

#include "cpmi.h"

typedef union
{
   long far * Address;
   struct
   {
      unsigned int Offset;
      unsigned int Segment;
   } Pointer;
} LONGPTR;

void cdecl _evLow( unsigned int, void *, unsigned int );

static long far Ticks = 0;
static long far Interval = 1;
static PHB_ITEM far codeBlock;
static char inProgress = 0;

static void cdecl TickTock( void )
{
   auto unsigned int ProtMode = cpmiIsProtected();
   auto LONGPTR Timer;
   auto HB_EVALINFO eval;

   if ( inProgress ) return;

   inProgress = 1;

   if ( ProtMode )
   {
      Timer.Pointer.Segment = cpmiProtectedPtr( ( long * ) ( 0x0000046C ), sizeof( long ) );
      Timer.Pointer.Offset  = 0;

      if ( Timer.Pointer.Segment == 0 ) goto Exit;
   }
   else
      Timer.Address = ( long * ) ( 0x0000046C );

   if ( *Timer.Address >= ( Ticks + Interval ) ||
      ( *Timer.Address < Ticks ) )
   {
      Ticks = *Timer.Address;

      hb_evalNew( &eval, codeBlock );

      hb_itemRelease( hb_evalLaunch( &eval ) );
   }

   if ( ProtMode ) cpmiFreeSelector( Timer.Pointer.Segment );

   Exit: inProgress = 0;

   return;
}

CLIPPER FT_OnTick( void )
{
   if ( hb_itemType( codeBlock ) == BLOCK ) hb_itemRelease( codeBlock );

   codeBlock = hb_itemParam( 1 );

   if ( hb_itemType( codeBlock ) == BLOCK )
   {
      Interval = hb_parnl( 2 );

      _evLow( 5, TickTock, HB_TRUE );
   }
   else
      _evLow( 5, TickTock, HB_FALSE );

   return;
}
