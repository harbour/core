/*
 * $Id$
 */

#ifndef _DBEDIT_CH
#define _DBEDIT_CH

/* User function entry modes */
#define DE_IDLE         0       /* Idle                                    */
#define DE_HITTOP       1       /* Attempt to cursor past top of file      */
#define DE_HITBOTTOM    2       /* Attempt to cursor past bottom of file   */
#define DE_EMPTY        3       /* No records in work area                 */
#define DE_EXCEPT       4       /* Key exception                           */

/* User function return codes */
#define DE_ABORT        0       /* Abort DBEDIT()                          */
#define DE_CONT         1       /* Continue DBEDIT()                       */
#define DE_REFRESH      2       /* Force reread/redisplay of all data rows */

#endif /* _DBEDIT_CH */
