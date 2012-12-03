/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for MemoEdit() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#ifndef _MEMOEDIT_CH
#define _MEMOEDIT_CH

/* User callback status modes */
#define ME_IDLE         0       /* Idle, all keys processed        */
#define ME_UNKEY        1       /* Unknown key, memo unaltered     */
#define ME_UNKEYX       2       /* Unknown key, memo altered       */
#define ME_INIT         3       /* Initialization mode             */
#ifndef HB_CLP_STRICT
#define ME_REQUEST      4       /* Memoedit requests an input from */
                                /* the user function, e.g. after   */
                                /* ME_PASTE                        */ /* Xbase++ extension */
#endif

/* User callback return codes */
#define ME_DEFAULT      0       /* Perform default action          */
#define ME_IGNORE       32      /* Ignore unknown key              */
#define ME_DATA         33      /* Treat unknown key as data       */
#define ME_TOGGLEWRAP   34      /* Toggle word-wrap mode           */
#define ME_TOGGLESCROLL 35      /* Toggle scrolling mode           */
#define ME_WORDRIGHT    100     /* Perform word-right operation    */
#define ME_BOTTOMRIGHT  101     /* Perform bottom-right operation  */
#ifndef HB_CLP_STRICT
#define ME_PASTE        110     /* Paste string into buffer        */ /* Xbase++ extension */
#endif

/* NOTE: Return codes 1-31 cause MemoEdit() to perform the */
/*       edit action corresponding to the key whose value is returned. */

#endif /* _MEMOEDIT_CH */
