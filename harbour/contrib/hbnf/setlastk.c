/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FT_LASTKEY()
 *
 * Copyright 1999-2008 Viktor Szakats (harbour.01 syenar.hu)
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
 * along with this software; see the file COPYING.  If not, write to
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

/*
* File......: setlastk.asm
* Author....: Ted Means
* CIS ID....: 73067,3332
*
* This is an original work by Ted Means and is placed in the
* public domain.
*
* Modification history:
* ---------------------
*
*     Rev 1.0   01 Jul 1992 01:23:06   GLENN
*  Initial revision.
*
*/

/*  $DOC$
 *  $FUNCNAME$
 *     FT_LASTKEY()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Force LastKey() to return a programmer-defined value.
 *  $SYNTAX$
 *     FT_LastKey( <nKey> ) -> NIL
 *  $ARGUMENTS$
 *     <nKey> is the Inkey() value of the desired key.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     It is occasionally useful to force LastKey() to return a known value.
 *     This is easily accomplishing by using the KEYBOARD command, but this
 *     has undesireable side effects (the keyboard buffer is cleared, and
 *     the keystroke is processed whether you needed it to be or not).  This
 *     function accomplishes the same task but without the side effects.  It
 *     does so by directly modifying the memory location where Clipper stores
 *     the LastKey() value.
 *
 *     Some highly unorthodox programming techniques, not to mention rather
 *     strange use of Clipper internals, was necessary to make this function
 *     work.  If this makes you uncomfortable, then don't use this function,
 *     you worthless crybaby.
 *  $EXAMPLES$
 *     keyboard chr( K_ESC )
 *
 *     ? lastkey()  // returns 27
 *
 *     FT_LastKey( K_F1 )
 *
 *     ? lastkey()  // now returns 28
 *  $END$
 */

#include "hbapi.h"

HB_FUNC_EXTERN( HB_SETLASTKEY );

HB_FUNC( FT_LASTKEY )
{
   HB_FUNC_EXEC( HB_SETLASTKEY )
}
