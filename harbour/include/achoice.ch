/*
 * $Id$
 */

#ifndef _ACHOICE_CH
#define _ACHOICE_CH

/* User callback status codes */
#define AC_IDLE         0       /* Idle                                        */
#define AC_HITTOP       1       /* Attempt to move above the first item        */
#define AC_HITBOTTOM    2       /* Attempt to move below the last item         */
#define AC_EXCEPT       3       /* Keystroke exception                         */
#define AC_NOITEM       4       /* There's no selectable item                  */

/* User callback return codes */
#define AC_ABORT        0       /* Abort ACHOICE() and return zero             */
#define AC_SELECT       1       /* Select current item and return it's index   */
#define AC_CONT         2       /* Continue ACHOICE()                          */
#define AC_GOTO         3       /* Search first chars for the last pressed key */
#define AC_REDRAW       4       /* Redraw ACHOICE()                            */

#endif /* _ACHOICE_CH */
