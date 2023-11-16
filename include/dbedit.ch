/*
 * Header file for dbEdit() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef _DBEDIT_CH
#define _DBEDIT_CH

/* User function entry modes */
#define DE_IDLE         0       /* Idle */
#define DE_HITTOP       1       /* Attempt to cursor past top of file */
#define DE_HITBOTTOM    2       /* Attempt to cursor past bottom of file */
#define DE_EMPTY        3       /* No records in work area */
#define DE_EXCEPT       4       /* Key exception */

/* User function return codes */
#define DE_ABORT        0       /* Abort dbEdit() */
#define DE_CONT         1       /* Continue dbEdit() */
#define DE_REFRESH      2       /* Force reread/redisplay of all data rows */
#ifdef HB_CLP_UNDOC
   #define DE_APPEND       3       /* Undocumented CA-Cl*pper append mode in DBEdit */
#endif

#endif /* _DBEDIT_CH */
