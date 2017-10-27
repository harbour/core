/*
 * Header file for debugger
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* NOTE: This file is also used by C code. */

#ifndef HB_DEBUGGER_CH_
#define HB_DEBUGGER_CH_

/* Mode of __dbgEntry() calls (the first parameter) */
#define HB_DBG_MODULENAME     1  /* 2nd argument is a module name */
#define HB_DBG_LOCALNAME      2  /* 2nd argument is a local var name */
#define HB_DBG_STATICNAME     3  /* 2nd arg is a static var name */
#define HB_DBG_ENDPROC        4  /* exit from a procedure */
#define HB_DBG_SHOWLINE       5  /* show current line */
#define HB_DBG_GETENTRY       6  /* initialize C __dbgEntry() function pointer */
#define HB_DBG_ACTIVATE       7  /* activate debugger interface */
#define HB_DBG_VMQUIT         8  /* call internal debugger destructors */


/* Information structure stored in aCallStack */
#define HB_DBG_CS_MODULE      1  /* module name (.prg file) */
#define HB_DBG_CS_FUNCTION    2  /* function name */
#define HB_DBG_CS_LINE        3  /* start line */
#define HB_DBG_CS_LEVEL       4  /* eval stack level of the function */
#define HB_DBG_CS_LOCALS      5  /* an array with local variables */
#define HB_DBG_CS_STATICS     6  /* an array with static variables */
#define HB_DBG_CS_LEN         6

/* Information structure stored in aModules */
#define HB_DBG_MOD_NAME       1  /* module name */
#define HB_DBG_MOD_STATICS    2  /* module static variables */
#define HB_DBG_MOD_GLOBALS    3  /* module global variables */
#define HB_DBG_MOD_EXTGLOBALS 4  /* module extern global variables */
#define HB_DBG_MOD_LEN        4

/* Information structure stored in:
 *    aCallStack[ n ][ HB_DBG_CS_LOCALS | HB_DBG_CS_STATICS ]
 *    aModules[ n ][ HB_DBG_MOD_STATICS | HB_DBG_MOD_GLOBALS | HB_DBG_MOD_EXTGLOBALS ]
 */
#define HB_DBG_VAR_NAME          1  /* variable name */
#define HB_DBG_VAR_INDEX         2  /* index */
#define HB_DBG_VAR_TYPE          3  /* type of variable: "L", "S", "G" */
#define HB_DBG_VAR_FRAME         4  /* eval stack level of the function or static frame */
#define HB_DBG_VAR_LEN           4

/* Information structure stored in __dbgGetWatchPoints() array */
#define HB_DBG_WP_EXPR           1  /* expression */
#define HB_DBG_WP_ISTRACE        2  /* .T. -> tracepoint, .F. -> watchpoint */
#define HB_DBG_WP_VALID          3  /* is valid expression? */
#define HB_DBG_WP_RESULT         4  /* expression result */
#define HB_DBG_WP_LEN            4

/* Information structure stored in __dbgGetBreakPoints() array */
#define HB_DBG_BP_LINE           1  /* line number */
#define HB_DBG_BP_MODULE         2  /* module name */
#define HB_DBG_BP_FUNC           3  /* function name */
#define HB_DBG_BP_LEN            3

/* Information structure stored in __dbgGetSETs() array */
#define HB_DBG_SET_POS           1  /* expression */
#define HB_DBG_SET_NAME          2  /* .T. -> tracepoint, .F. -> watchpoint */
#define HB_DBG_SET_VALUE         3  /* is valid expression? */
#define HB_DBG_SET_LEN           3

#endif /* HB_DEBUGGER_CH_ */
