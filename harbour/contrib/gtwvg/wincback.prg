/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                      A Contribution from Andy Wos
 *                                   .
 *                            A Big Thank You
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"

/*----------------------------------------------------------------------*/
/*
 * Calback pointer interface
 * ( to be used with _winccback.c )
 * 6 June 2004, 13 June 2004
 * 5 April 2005 (added VOID return option)
 */
/*----------------------------------------------------------------------*/
/*
 * Universal 32 bit callback procedure interface
 *
 * Syntax:
 *   AsCallback( <@MyFunc()>, , [<nQtyParams>], [<xCargo>], [<lVoid>] )  => pProcPtr
 *   or
 *   AsCallback( <bBlock>, , [<nQtyParams>], [<xCargo>], [<lVoid>] )  => pProcPtr
 *   or
 *   AsCallback( <MethPtr>, <oObject>, [<nQtyParams>], [<xCargo>], [<lVoid>] )  => pProcPtr
 *   or
 *   you can replace <@MyFunc()> or <MethPtr> with the acual name, eg: "MYFUNC", MYMETH"
 *
 * Returns: <pProcPtr> - true callback function pointer, or 0 on failure
 *
 *
 * Note: Must call FreeCallback( <pProcPtr> ), when no longer needed
 *       or memory leak will occur on the application exit.
 *       All parameters are treated as LONG (32bit) values.
 *       Also the return value is treated as a numerical LONG (32bit), unless <lVoid> is TRUE.
 *
 *       [<nQtyParams>] - optional number of formal parameters (not incl. <xCargo>)
 *                        as passed by the caller, eg. Windows, the default is 4.
 *                        The number of parameters must be exactly as specified.
 *                        Min value is 0, and the maximum is 10.
 *
 *       [<xCargo>]     - any extra value to pass to the callback procedure
 *                        (if you intend to change it, it has to be passed by
 *                        reference). It will be passed to the procedure, after
 *                        all formal parameters.
 *
 *       [<lVoid>]      - an optional indication that the callback function is not to return
 *                        any value (void) to the calling process (default FALSE).
 *
 * [<cDebug>] - debugging only (not to be used in normal operation)
 *              if passed by reference, it returns the actual generated
 *              assembler code for inspection and debugging
 */
FUNCTION HB_AsCallback( pbcFunc, oObj, nNumParam, xCargo, lVoid, cDebug )
   LOCAL pCallback

   HB_SYMBOL_UNUSED( xCargo )

   pCallback := _AsCallback( pbcFunc, oObj, nNumParam, lVoid, @cDebug )

   RETURN pCallback
/*----------------------------------------------------------------------*/
/*
 * Syntax: FreeCallback( < nProcPtr > ) => lSuccess
 *
 * Note: The callback procedure must have been obtained
 *       through the AsCallback() function
 *       You should not call this function from within
 *       the callback function itself !!!!!!
 *       If it is a window procedure, the window must be
 *       destroyed first, or it must be "unsubclassed"
 */
FUNCTION HB_FreeCallback( pCallback )

   _FreeCallback( pCallback )

   RETURN .T.
/*----------------------------------------------------------------------*/
/*
 * free all oustanding unreleased callback pointers on exit from the application
 * could be changed to EXIT PROCEDURE
 */
PROCEDURE _ExitCallbacks

   _FreeAllCallbacks()

   RETURN
/*----------------------------------------------------------------------*/
