/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for IBM OS/2 Presentation Manager
 * Class HBButton
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * Copyright 2001 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.harbour-project.org
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

#include "common.ch"
#include "hbclass.ch"
#include "os2pm.ch"


// NOTE: Be sure they are the same
#define WS_CHILD     WS_DISABLED //0x40000000


CLASS HBButton FROM HBWinControl

   DATA      OnClick   PROPERTY

   METHOD    New( oContainer )

ENDCLASS


METHOD New( oContainer ) CLASS HBButton

   ::hWnd = WinCreateWindow(oContainer:hWndClient,;         /* Parent window  */
                            WC_BUTTON,;                     /* Class window   */
                            "",;                            /* Button text    */
                            WS_VISIBLE + WS_TABSTOP+;       /* Visible style  */
                            BS_PUSHBUTTON + BS_AUTOSIZE,;   /* Button style   */
                            0, 0,;                          /* x, y           */
                            80, 30,;                        /* cx, cy         */
                            oContainer:hWndClient,;         /* Owner window   */
                            HWND_TOP,;                      /* Top of z-order */
                            ::GetNewId(),;                  /* Identifier     */
                            nil,;                           /* Control data   */
                            nil)                            /* parameters     */

   // NOTE: There is no need to modify it later since I can give it an initial
   //       size during window creartion
   //::Width  = 80
   //::Height = 25

return Self
