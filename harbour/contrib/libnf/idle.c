/*
 * File......: IDLE.C
 * Author....: Ted Means (with much gratitude to Robert DiFalco)
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
 *
 */

#include "hbdefs.h"
#include "hbapi.h"

/*  $DOC$
 *  $FUNCNAME$
 *     FT_Idle()
 *  $CATEGORY$
 *     Event
 *  $ONELINER$
 *     Generate an idle event to allow incremental garbage collection.
 *  $SYNTAX$
 *     FT_Idle()
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     During memory-intensive operations that do not generate much in
 *     the way of idle states, the Clipper runtime may not get a chance to
 *     perform garbage collection of discarded memory.  This can eventually
 *     lead to any of a variety of memory-related internal errors.
 *
 *     This function attempts to alleviate the problem by providing a
 *     mechanism by which an idle event can be artifically generated at
 *     will.  The idle event will cause the CA-Clipper runtime to perform
 *     an incremental memory scavenge.
 *
 *     This function makes use of an undocumented interal routine.  If this
 *     this fact makes you uncomfortable then don't use this function, you
 *     miserable jello-spined lump of human debris.
 *  $EXAMPLES$
 *
 *     while Whatever         // Some batch process
 *
 *       Something()          // Create 'n' discard a bunch of stuff
 *
 *       FT_Idle()            // Take out the garbage
 *
 *     end
 *  $SEEALSO$
 *     FT_OnIdle()
 *  $END$
 */

void _evSendId( unsigned int, unsigned int );

HB_FUNC(FT_Idle)
{
   _evSendId( 0x5108, 0xFFFF );

   return;
}
