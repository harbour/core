/*
 * File......: ONTICK.C
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_OnTick()
 *  $CATEGORY$
 *     Event
 *  $ONELINER$
 *     Evaluate a designated code block at a designated interval.
 *  $SYNTAX$
 *     FT_OnTick( bCode, nInterval )
 *  $ARGUMENTS$
 *     <bCode> is the code block to evaluate.
 *     <nInterval> is the number of clock ticks to wait between
 *                 evaluations of the code block.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     This function effectively allows you to run tasks in the background
 *     by transparently and periodically calling a designated routine.
 *
 *     To halt the execution of the background function, call FT_OnTick()
 *     with no arguments.
 *
 *     This function makes heavy use of several undocumented internal
 *     routines.  If this fact makes you uncomfortable then don't use
 *     this function, you quivering sack of cowardly slime.
 *  $EXAMPLES$
 *
 *     // Set up a self-updating on-screen clock
 *
 *     FT_OnTick( "CLOCK", 9 )
 *
 *     procedure Clock
 *
 *     local nRow := Row()
 *     local nCol := Col()
 *
 *     @ 0, 0 say Time()
 *
 *     SetPos( nRow, nCol )
 *
 *     return
 *
 *  $SEEALSO$
 *  $END$
 */

#include <extend.api>
#include <item.api>
#include <cpmi.h>

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
void cdecl _bcopy( void *, void *, unsigned int );

static long far Ticks = 0;
static long far Interval = 1;
static ITEM far codeBlock;
static char inProgress = 0;

static void cdecl TickTock( void )
{
   auto unsigned int ProtMode = cpmiIsProtected();
   auto LONGPTR Timer;
   auto EVALINFO eval;

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

      _evalNew( &eval, codeBlock );

      _itemRelease( _evalLaunch( &eval ) );
   }

   if ( ProtMode ) cpmiFreeSelector( Timer.Pointer.Segment );

   Exit: inProgress = 0;

   return;
}


CLIPPER FT_OnTick( void )
{
   if ( _itemType( codeBlock ) == BLOCK ) _itemRelease( codeBlock );

   codeBlock = _itemParam( 1 );

   if ( _itemType( codeBlock ) == BLOCK )
   {
      Interval = _parnl( 2 );

      _evLow( 5, TickTock, TRUE );
   }
   else
      _evLow( 5, TickTock, FALSE );

   return;
}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
