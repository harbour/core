/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ft_SetVcur()/FT_GETVCUR()
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:03:30   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:12   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   14 Jun 1991 17:59:18   GLENN
 * Documentation change (minor), and checked for compatibility with new
 * ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:28   GLENN
 * Nanforum Toolkit
 *
 */

#include "setcurs.ch"

/* NOTE: In Harbour video pages are ignored. */

PROCEDURE ft_SetVcur( nPage, nRow, nCol )

   HB_SYMBOL_UNUSED( nPage )

   SetPos( nRow, nCol )

   RETURN

/* NOTE: In Harbour video pages are ignored. */

FUNCTION ft_GetVCur( nPage )

   LOCAL nTop, nBot

   HB_SYMBOL_UNUSED( nPage )

   SWITCH SetCursor()
   CASE SC_NORMAL
      nTop := 6
      nBot := 7
      EXIT
   CASE SC_INSERT
      nTop := 4
      nBot := 7
      EXIT
   CASE SC_SPECIAL1
      nTop := 0
      nBot := 7
      EXIT
   CASE SC_SPECIAL2
      nTop := 0
      nBot := 3
      EXIT
   OTHERWISE
      nTop := nBot := 0
   ENDSWITCH

   RETURN { nTop, nBot, Row(), Col() }
