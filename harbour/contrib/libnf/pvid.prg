/*
 * File......: PVID.PRG
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   16 Oct 1992 00:05:22   GLENN
 * Just making sure we had Ted's most current revision.
 *
 *    Rev 1.0   22 Aug 1992 16:51:32   GLENN
 * Initial revision.
 */

#include "set.ch"

#define PV_ROW     1
#define PV_COL     2
#define PV_COLOR   3
#define PV_IMAGE   4
#define PV_CURSOR  5
#define PV_BLINK   6
#define PV_NOSNOW  7
#define PV_MAXROW  8
#define PV_MAXCOL  9
#define PV_SCORE  10

static aVideo := {}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_PUSHVID()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Save current video states on internal stack.
 *  $SYNTAX$
 *     FT_PushVid() -> <nStackSize>
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     The current size of the internal stack (i.e. the number of times
 *     FT_PushVid() has been called).
 *  $DESCRIPTION$
 *     Menus, picklists, browses, and other video-intensive items often
 *     require you to save certain video states -- screen image, cursor
 *     position, and so forth.  Constantly saving and restoring these items
 *     can get very tedious.  This function attempts to alleviate this
 *     problem.  When called, it saves the cursor position, color setting,
 *     screen image, cursor style, blink setting, scoreboard setting, snow
 *     setting, and maximum row and column to a series of static arrays.  All
 *     that is needed to restore the saved settings is a call to FT_PopVid().
 *  $EXAMPLES$
 *     FT_PushVid()          // Save the current video states
 *  $SEEALSO$
 *     FT_PopVid()
 *  $END$
 */

function FT_PushVid()

AAdd( aVideo, { row(), ;
                col(), ;
                setcolor(), ;
                savescreen( 0, 0, maxrow(), maxcol() ), ;
                set( _SET_CURSOR ), ;
                setblink(), ;
                nosnow(), ;
                maxrow() + 1, ;
                maxcol() + 1, ;
                set( _SET_SCOREBOARD ) } )

return len( aVideo )



/*  $DOC$
 *  $FUNCNAME$
 *     FT_POPVID()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Restore previously saved video states.
 *  $SYNTAX$
 *     FT_PopVid() -> <nStackSize>
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     The number of items remaining in the internal stack.
 *  $DESCRIPTION$
 *     This is the complementary function to FT_PushVid().  At some time
 *     after saving the video states it will probably be necessary to restore
 *     them.  This is done by restoring the settings from the last call to
 *     FT_PushVid().  The number of items on the internal stack is then
 *     reduced by one.  Note that the use of stack logic means that items on
 *     the stack are retrieved in Last In First Out order.
 *  $EXAMPLES$
 *     FT_PopVid()          // Restore video states
 *  $SEEALSO$
 *     FT_PushVid()
 *  $END$
 */

function FT_PopVid()

local nNewSize := len( aVideo ) - 1
local aBottom  := ATail( aVideo )

if nNewSize >= 0
   setmode( aBottom[ PV_MAXROW ], aBottom[ PV_MAXCOL ] )
   set( _SET_CURSOR, aBottom[ PV_CURSOR ] )
   nosnow( aBottom[ PV_NOSNOW ] )
   setblink( aBottom[ PV_BLINK ] )
   restscreen( 0, 0, maxrow(), maxcol(), aBottom[ PV_IMAGE ] )
   setcolor( aBottom[ PV_COLOR ] )
   setpos( aBottom[ PV_ROW ], aBottom[ PV_COL ] )
   set( _SET_SCOREBOARD, aBottom[ PV_SCORE ] )

   aSize( aVideo, nNewSize )
endif

return len( aVideo )


