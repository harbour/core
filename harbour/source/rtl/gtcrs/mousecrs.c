/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Mouse subsystem based on ncurses
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

#include <curses.h>

#include "hbapigt.h"
#include "inkey.ch"   /* included also in a harbour code */

/* C callable low-level interface */

struct HB_mouse_event
{
   USHORT key;
   USHORT row;
   USHORT col;
   long time;
};
#define HB_MOUSE_QUEUE_LEN 8
static struct HB_mouse_event s_event_queue[ HB_MOUSE_QUEUE_LEN ];
static int s_last_event;

/* this is called from key handling when ncurses reports a mouse event
 * under xterm
 * m_event points into:
 * m_event[ 0 ] = buttons state
 *  bits 0-1:     
 *   00 = button1 pressed (left on 2-buttons mouse)
 *   01 = button2 pressed (ignored on 2 button mouse)
 *   10 = button3 pressed (right on 2-buttons mouse)
 *   11 = button released
 *  bit 2:
 *   1 = shift was down
 *  bit 3:
 *   1 = meta key was down
 *  bit 4:
 *   1 = control key was down
 *
 * m_event[ 1 ] = column position of a pointer + 32
 * m_event[ 2 ] = row position of a pointer + 32
 */
int hb_mouse_xevent( char *m_event, HB_inkey_enum eventmask )
{
   int ch = 0;
   
   if( ( m_event[ 0 ] & 0x03 ) == 0x03 )
   {
      /* some button was released - 
       * xterm dosen't report which then use the last one!
       */
      if( s_event_queue[ s_last_event ].key == K_RBUTTONDOWN )
      {
         /* the last pressed button was the right one */
         if( eventmask & INKEY_RUP )
            ch = K_RBUTTONUP;
      }
      else if( s_event_queue[ s_last_event ].key == K_LBUTTONDOWN )
      {
         if( eventmask & INKEY_LUP )
            ch = K_LBUTTONUP;
      }
      else
      {
         /* ignore it - it is release after a double click */
      }
      --s_last_event;
   }
   else if( m_event[ 0 ] & 0x20 )
   {
      /* some button was pressed */
      if( s_last_event < HB_MOUSE_QUEUE_LEN )
         ++s_last_event;
      s_event_queue[ s_last_event ].col = m_event[ 1 ] - 33;
      s_event_queue[ s_last_event ].row = m_event[ 2 ] - 33;
      
      if( m_event[ 0 ] & 0x02 )
      {
         /* button2 (right) button was pressed */
         if( m_event[ 3 ] )
         {
            /* two press sequences = double-click is signaled */
            s_event_queue[ s_last_event ].key = K_RDBLCLK;
            ch = K_RDBLCLK;
         }
         else
         {
            s_event_queue[ s_last_event ].key = K_RBUTTONDOWN;
            if( eventmask & INKEY_RDOWN )
               ch = K_RBUTTONDOWN;
         }
      }
      else
      {
         /* button1 (left) was pressed */
         if( m_event[ 3 ] )
         {
            /* double-click is signaled */
            s_event_queue[ s_last_event ].key = K_LDBLCLK;
            ch = K_LDBLCLK;
         }
         else
         {
            s_event_queue[ s_last_event ].key = K_LBUTTONDOWN;
            if( eventmask & INKEY_LDOWN )
               ch = K_LBUTTONDOWN;
         }
      }      
   }
   
   return ch;
}   

/* this reads a single mouse event
 */
int hb_mouse_key( void )
{
   return 0;
}

void hb_mouse_Init( void )
{
   mmask_t mm;
   
   mousemask( BUTTON1_PRESSED | BUTTON1_RELEASED | BUTTON1_DOUBLE_CLICKED |
              BUTTON2_PRESSED | BUTTON2_RELEASED | BUTTON2_DOUBLE_CLICKED |
              REPORT_MOUSE_POSITION,
              &mm );
   mouseinterval( 500 );
   s_last_event = -1;
}

void hb_mouse_Exit( void )
{
   hb_mouse_Hide();   /* do not leave mouse hidden */
}

BOOL hb_mouse_IsPresent( void )
{
   return FALSE;
}

void hb_mouse_Show( void )
{
   hb_mouse_Init();
}

void hb_mouse_Hide( void )
{
   mmask_t mm;
   
   mousemask( 0, &mm );
}

int hb_mouse_Col( void )
{
   if( s_last_event >= 0 )
      return s_event_queue[ s_last_event ].col;
   else
      return 0;
}

int hb_mouse_Row( void )
{
   if( s_last_event >= 0 )
      return s_event_queue[ s_last_event ].row;
   else
      return 0;
}

void hb_mouse_SetPos( int iRow, int iCol )
{
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
}

BOOL hb_mouse_IsButtonPressed( int iButton )
{
   if( s_last_event >= 0 )
   {
      USHORT uKey;
      USHORT i;
      
      uKey = ( iButton == 1 ) ? INKEY_LDOWN : INKEY_RDOWN;
      i = 0;
      while( i <= s_last_event )
      {
         if( s_event_queue[ i++ ].key & uKey )
            return TRUE;
      }
   }
   return FALSE;
}

int hb_mouse_CountButton( void )
{
   return tigetnum( "btns" );
}

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   int r, c;

   getmaxyx( stdscr, r, c );

   *piTop = *piLeft = 0;
   *piBottom = r;
   *piRight = c;
}

