/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TBrowse() CA-Cl*pper 5.3 functions
 *
 * Copyright 2007 {list of individual authors and e-mail addresses}
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

#ifdef HB_COMPAT_C53

#include "button.ch"
#include "tbrowse.ch"

FUNCTION TApplyKey( nKey, oBrowse )
   RETURN oBrowse:applyKey( nKey )

FUNCTION TBAddCol() /* TODO */
   RETURN NIL

FUNCTION TBBBlock() /* TODO */
   RETURN NIL

FUNCTION TBClose() /* TODO */
   RETURN NIL

FUNCTION TBCreate() /* TODO */
   RETURN NIL

FUNCTION TBDelCol() /* TODO */
   RETURN NIL

FUNCTION TBDisplay() /* TODO */
   RETURN NIL

FUNCTION TBEditCell() /* TODO */
   RETURN NIL

FUNCTION TBFBlock() /* TODO */
   RETURN NIL

FUNCTION TBGoBot() /* TODO */
   RETURN NIL

FUNCTION TBGoTop() /* TODO */
   RETURN NIL

FUNCTION TBInsCol() /* TODO */
   RETURN NIL

FUNCTION TBModal() /* TODO */
   RETURN NIL

FUNCTION TBSBlock() /* TODO */
   RETURN NIL

FUNCTION TBSkip() /* TODO */
   RETURN NIL

#endif
