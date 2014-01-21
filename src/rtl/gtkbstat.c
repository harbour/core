/*
 * Harbour Project source code:
 *    Low level keyboard shift state functions common to some GT drivers
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


/* NOTE: User programs should never call this layer directly! */


#include "hbgtcore.h"

#if defined( HB_OS_WIN )

#include <windows.h>
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

int hb_gt_winapi_getKbdState( void )
{
   BYTE kbState[ 256 ];
   int iKbdState = 0;

   if( GetKeyboardState( kbState ) )
   {
      if( kbState[ VK_SHIFT    ] & 0x80 ) iKbdState |= HB_GTI_KBD_SHIFT;
      if( kbState[ VK_CONTROL  ] & 0x80 ) iKbdState |= HB_GTI_KBD_CTRL;
      if( kbState[ VK_MENU     ] & 0x80 ) iKbdState |= HB_GTI_KBD_ALT;
      if( kbState[ VK_LWIN     ] & 0x80 ) iKbdState |= HB_GTI_KBD_LWIN;
      if( kbState[ VK_RWIN     ] & 0x80 ) iKbdState |= HB_GTI_KBD_RWIN;
      if( kbState[ VK_APPS     ] & 0x80 ) iKbdState |= HB_GTI_KBD_MENU;
      if( kbState[ VK_SCROLL   ] & 0x01 ) iKbdState |= HB_GTI_KBD_SCROLOCK;
      if( kbState[ VK_NUMLOCK  ] & 0x01 ) iKbdState |= HB_GTI_KBD_NUMLOCK;
      if( kbState[ VK_CAPITAL  ] & 0x01 ) iKbdState |= HB_GTI_KBD_CAPSLOCK;
      if( kbState[ VK_INSERT   ] & 0x01 ) iKbdState |= HB_GTI_KBD_INSERT;

      if( kbState[ VK_LSHIFT   ] & 0x80 ) iKbdState |= HB_GTI_KBD_LSHIFT;
      if( kbState[ VK_RSHIFT   ] & 0x80 ) iKbdState |= HB_GTI_KBD_RSHIFT;
      if( kbState[ VK_LCONTROL ] & 0x80 ) iKbdState |= HB_GTI_KBD_LCTRL;
      if( kbState[ VK_RCONTROL ] & 0x80 ) iKbdState |= HB_GTI_KBD_RCTRL;
      if( kbState[ VK_LMENU    ] & 0x80 ) iKbdState |= HB_GTI_KBD_LALT;
      if( kbState[ VK_RMENU    ] & 0x80 ) iKbdState |= HB_GTI_KBD_RALT;
   }

   return iKbdState;
}

void hb_gt_winapi_setKbdState( int iKbdState )
{
   BYTE kbState[ 256 ];

   if( GetKeyboardState( kbState ) )
   {
      kbState[ VK_SHIFT ]   = ( iKbdState & HB_GTI_KBD_SHIFT ) ? 0x80 : 0;
      kbState[ VK_CONTROL ] = ( iKbdState & HB_GTI_KBD_CTRL ) ? 0x80 : 0;
      kbState[ VK_MENU ]    = ( iKbdState & HB_GTI_KBD_ALT ) ? 0x80 : 0;
      kbState[ VK_LWIN ]    = ( iKbdState & HB_GTI_KBD_LWIN ) ? 0x80 : 0;
      kbState[ VK_RWIN ]    = ( iKbdState & HB_GTI_KBD_RWIN ) ? 0x80 : 0;
      kbState[ VK_APPS ]    = ( iKbdState & HB_GTI_KBD_MENU ) ? 0x80 : 0;
      kbState[ VK_SCROLL ]  = ( iKbdState & HB_GTI_KBD_SCROLOCK ) ? 0x01 : 0;
      kbState[ VK_NUMLOCK ] = ( iKbdState & HB_GTI_KBD_NUMLOCK ) ? 0x01 : 0;
      kbState[ VK_CAPITAL ] = ( iKbdState & HB_GTI_KBD_CAPSLOCK ) ? 0x01 : 0;
      kbState[ VK_INSERT ]  = ( iKbdState & HB_GTI_KBD_INSERT ) ? 0x01 : 0;

      kbState[ VK_LSHIFT ]   = ( iKbdState & HB_GTI_KBD_LSHIFT ) ? 0x80 : 0;
      kbState[ VK_RSHIFT ]   = ( iKbdState & HB_GTI_KBD_RSHIFT ) ? 0x80 : 0;
      kbState[ VK_LCONTROL ] = ( iKbdState & HB_GTI_KBD_LCTRL ) ? 0x80 : 0;
      kbState[ VK_RCONTROL ] = ( iKbdState & HB_GTI_KBD_RCTRL ) ? 0x80 : 0;
      kbState[ VK_LMENU ]    = ( iKbdState & HB_GTI_KBD_LALT ) ? 0x80 : 0;
      kbState[ VK_RMENU ]    = ( iKbdState & HB_GTI_KBD_RALT ) ? 0x80 : 0;

      SetKeyboardState( kbState );
   }
}

#endif /* HB_OS_WIN */
