/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Keyboard API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    HB_KEYPUT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_DOSPROCESS
#define INCL_NOPMAPI

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbinkey.ch"
#include "inkey.ch"

#include <time.h>
#if defined( HB_OS_UNIX )
  #include <sys/times.h>
#endif

static int *  s_inkeyBuffer = 0; /* Harbour keyboard buffer (empty if head == tail)     */
static int    s_inkeyHead;       /* Harbour keyboard buffer head pointer (next insert)  */
static int    s_inkeyTail;       /* Harbour keyboard buffer tail pointer (next extract) */
static int    s_inkeyLast;       /* Last key extracted from Harbour keyboard buffer     */
static BOOL   s_inkeyPoll;       /* Flag to override no polling when TYPEAHEAD is 0     */
static int    s_inkeyForce;      /* Variable to hold keyboard input when TYPEAHEAD is 0 */
static HB_inkey_enum s_eventmask;

static int hb_inkeyFetch( void ) /* Extract the next key from the keyboard buffer */
{
   int key;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyFetch()"));

   hb_inkeyPoll();
   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is set */
      if( s_inkeyHead == s_inkeyTail ) key = 0;    /* Keyboard buffer is empty */
      else
      {                                            /* Keyboard buffer is not empty */
         s_inkeyLast = s_inkeyBuffer[ s_inkeyTail++ ];
         if( s_inkeyTail >= hb_set.HB_SET_TYPEAHEAD )
            s_inkeyTail = 0;
         key = s_inkeyLast;
      }
   }
   else
      key = s_inkeyLast = s_inkeyForce;           /* Typeahead support is disabled */
   s_inkeyForce = 0;

   return key;
}

int hb_inkey( BOOL bWait, double dSeconds, HB_inkey_enum event_mask )
{
   int key;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkey(%d, %lf, %d)", (int) bWait, dSeconds, (int) event_mask));

   s_eventmask = event_mask;                   /* Set current input event mask */
   s_inkeyPoll = TRUE;                         /* Force polling */

   /* Wait for input events if requested */
   if( bWait )
   {
      if( ( dSeconds * CLOCKS_PER_SEC ) < 1 )  /* Wait forever ? */
      {
         /* There is no point in waiting forever for no input events! */
         if( ( event_mask & ( INKEY_ALL + INKEY_RAW ) ) != 0 )
         {
            while( hb_inkeyNext( event_mask ) == 0 )
            {
               hb_idleState();
            }
            hb_idleReset();
         }
      }
      else
      {
#if defined( HB_OS_UNIX )
         /* NOTE: clock() returns a time used by a program - if it is suspended
          * then this time will be zero
         */
         clock_t end_clock;
         struct tms tm;

         end_clock = times( &tm ) + ( clock_t ) ( dSeconds * 100 );
         while( hb_inkeyNext( event_mask ) == 0 && (times( &tm ) < end_clock) )
#else
         clock_t end_clock = clock() + ( clock_t ) ( dSeconds * CLOCKS_PER_SEC );

         while( hb_inkeyNext( event_mask ) == 0 && clock() < end_clock )
#endif
         {
            hb_idleState();
         }
         hb_idleReset();
      }
   }

   key = hb_inkeyFetch();            /* Get the current input event or 0 */

   s_inkeyPoll = FALSE;                        /* Stop forced polling */
   s_eventmask = hb_set.HB_SET_EVENTMASK;      /* Restore original input event mask */

   return hb_inkeyTranslate( key, event_mask );
}

int hb_inkeyLast( HB_inkey_enum event_mask )      /* Return the value of the last key that was extracted */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast()"));

   hb_inkeyPoll();

   return hb_inkeyTranslate( s_inkeyLast, event_mask );
}

int hb_inkeyNext( HB_inkey_enum event_mask )      /* Return the next key without extracting it */
{
   int key = s_inkeyForce;    /* Assume that typeahead support is disabled */

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext()"));

   hb_inkeyPoll();

   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is enabled */
      if( s_inkeyHead == s_inkeyTail )
         key = 0;
      else
         key = s_inkeyBuffer[ s_inkeyTail ];    /* Next key */
   }

   return hb_inkeyTranslate( key, event_mask );
}

void hb_inkeyPoll( void )     /* Poll the console keyboard to stuff the Harbour buffer */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPoll()"));

   if( hb_set.HB_SET_TYPEAHEAD || s_inkeyPoll )
   {
      int ch = hb_gtReadKey( s_eventmask );

      switch( ch )
      {
         case HB_BREAK_FLAG:        /* Check for Ctrl+Break */
            if( !hb_set.HB_SET_CANCEL ) ch = 0; /* Ignore if cancel disabled */
         case HB_K_ALT_C:           /* Check for extended Alt+C */
         case K_ALT_C:              /* Check for normal Alt+C */
            if( hb_set.HB_SET_CANCEL )
            {
               ch = 3;              /* Pretend it's a Ctrl+C */
               hb_vmRequestCancel();/* Request cancellation */
            }
            break;
         case HB_K_ALT_D:           /* Check for extended Alt+D */
         case K_ALT_D:              /* Check for normal Alt+D */
            if( hb_set.HB_SET_DEBUG )
            {
               ch = 0;              /* Make the keystroke disappear */
               hb_vmRequestDebug(); /* Request the debugger */
            }
      }

      hb_inkeyPut( ch );
   }
}

void hb_inkeyReset( BOOL allocate )     /* Reset the keyboard buffer */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyReset(%d)", (int) allocate));

   /* Reset the buffer head and tail pointers, the last key value,
      and the polling override flag */
   s_inkeyHead = 0;
   s_inkeyTail = 0;
   s_inkeyLast = 0;
   s_inkeyPoll = FALSE;
   s_inkeyForce = 0;

   /* The allocate flag allows the same function to be used to reset the
      buffer or to reset and allocate, reallocate, or free the buffer */
   if( allocate )
   {
      /* If the buffer already exists, free it */
      if( s_inkeyBuffer )
         hb_xfree( s_inkeyBuffer );

      /* Always allocate a new buffer, unless it's being freed from hb_setRelease() */
      if( hb_set.HB_SET_TYPEAHEAD > -1 )
      {
         /* The buffer min and max are determined by SET(HB_SET_TYPEAHEAD, but it
            can also set the typeahead to 0 to disable polling, in which case the
            minimum buffer size (which is 16) must still be allocated, because
            even when polling is disabled, calling INKEY() or NEXTKEY() will
            temporarily re-enable polling */
         s_inkeyBuffer = ( int * ) hb_xgrab( sizeof( int )
            * ( hb_set.HB_SET_TYPEAHEAD == 0 ? 16 : hb_set.HB_SET_TYPEAHEAD ) );
      }
   }
}

HB_FUNC( INKEY )
{
   USHORT uiPCount = hb_pcount();

   hb_retni( hb_inkey( uiPCount == 1 || ( uiPCount > 1 && ISNUM( 1 ) ),
                       hb_parnd( 1 ),
                       ISNUM( 2 ) ? ( HB_inkey_enum ) hb_parni( 2 ) : hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( __KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   hb_inkeyReset( FALSE );

   if( ISCHAR( 1 ) )
   {
      ULONG size = hb_parclen( 1 );

      if( size != 0 )
      {
         /* Stuff the string */
         BYTE * fPtr = ( BYTE * ) hb_parc( 1 );

         if( size >= ( ULONG ) hb_set.HB_SET_TYPEAHEAD )
         {
            /* Have to allow for a zero size typehead buffer */
            if( hb_set.HB_SET_TYPEAHEAD )
               size = ( ULONG ) ( hb_set.HB_SET_TYPEAHEAD - 1 );
            else
               size = 0;
         }

         while( size-- )
         {
            int ch = *fPtr++;
            if( ch == 59 )
               ch = 13; /* Convert ";" to CR, like Clipper does */
            hb_inkeyPut( ch );
         }
      }
   }
}

void hb_inkeyPut( int ch )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPut(%d)", ch));

   if( ch )
   {
      if( hb_set.HB_SET_TYPEAHEAD )
      {
         /* Proper typeahead support is set */
         int head = s_inkeyHead;

         s_inkeyBuffer[ head++ ] = ch;
         if( head >= hb_set.HB_SET_TYPEAHEAD ) head = 0;
         if( head != s_inkeyTail ) s_inkeyHead = head;
         else /* TODO: Add error sound */ ;
      }
      else
         s_inkeyForce = ch; /* Typeahead support is disabled */
   }
}

#ifdef HB_EXTENSION

HB_FUNC( HB_KEYPUT )
{
   if( ISNUM( 1 ) )
      hb_inkeyPut( hb_parni( 1 ) );
}

#endif

HB_FUNC( NEXTKEY )
{
   hb_retni( hb_inkeyNext( ISNUM( 1 ) ? ( HB_inkey_enum ) hb_parni( 1 ) : hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( LASTKEY )
{
   hb_retni( hb_inkeyTranslate( s_inkeyLast, ( HB_inkey_enum ) hb_inkeyNext( ISNUM( 1 ) ? ( HB_inkey_enum ) hb_parni( 1 ) : hb_set.HB_SET_EVENTMASK ) ) );
}

int hb_inkeyTranslate( int key, HB_inkey_enum event_mask )
{
   if( key && hb_gtExtendedKeySupport() && ! ( event_mask & HB_INKEY_EXTENDED ) )
   {
      /* Translate the Harbour extended key codes to
         Clipper-compatible key codes */
      switch( key ) {
      case HB_K_ALT_A:
         key = K_ALT_A;
         break;
      case HB_K_ALT_B:
         key = K_ALT_B;
         break;
      case HB_K_ALT_C:
         key = K_ALT_C;
         break;
      case HB_K_ALT_D:
         key = K_ALT_D;
         break;
      case HB_K_ALT_E:
         key = K_ALT_E;
         break;
      case HB_K_ALT_F:
         key = K_ALT_F;
         break;
      case HB_K_ALT_G:
         key = K_ALT_G;
         break;
      case HB_K_ALT_H:
         key = K_ALT_H;
         break;
      case HB_K_ALT_I:
         key = K_ALT_I;
         break;
      case HB_K_ALT_J:
         key = K_ALT_J;
         break;
      case HB_K_ALT_K:
         key = K_ALT_K;
         break;
      case HB_K_ALT_L:
         key = K_ALT_L;
         break;
      case HB_K_ALT_M:
         key = K_ALT_M;
         break;
      case HB_K_ALT_N:
         key = K_ALT_N;
         break;
      case HB_K_ALT_O:
         key = K_ALT_O;
         break;
      case HB_K_ALT_P:
         key = K_ALT_P;
         break;
      case HB_K_ALT_Q:
         key = K_ALT_Q;
         break;
      case HB_K_ALT_R:
         key = K_ALT_R;
         break;
      case HB_K_ALT_S:
         key = K_ALT_S;
         break;
      case HB_K_ALT_T:
         key = K_ALT_T;
         break;
      case HB_K_ALT_U:
         key = K_ALT_U;
         break;
      case HB_K_ALT_V:
         key = K_ALT_V;
         break;
      case HB_K_ALT_W:
         key = K_ALT_W;
         break;
      case HB_K_ALT_X:
         key = K_ALT_X;
         break;
      case HB_K_ALT_Y:
         key = K_ALT_Y;
         break;
      case HB_K_ALT_Z:
         key = K_ALT_Z;
         break;
      case HB_K_CTRL_A:
         key = K_CTRL_A;
         break;
      case HB_K_CTRL_B:
         key = K_CTRL_B;
         break;
      case HB_K_CTRL_C:
         key = K_CTRL_C;
         break;
      case HB_K_CTRL_D:
         key = K_CTRL_D;
         break;
      case HB_K_CTRL_E:
         key = K_CTRL_E;
         break;
      case HB_K_CTRL_F:
         key = K_CTRL_F;
         break;
      case HB_K_CTRL_G:
         key = K_CTRL_G;
         break;
      case HB_K_CTRL_H:
         key = K_CTRL_H;
         break;
      case HB_K_CTRL_I:
         key = K_CTRL_I;
         break;
      case HB_K_CTRL_J:
         key = K_CTRL_J;
         break;
      case HB_K_CTRL_K:
         key = K_CTRL_K;
         break;
      case HB_K_CTRL_L:
         key = K_CTRL_L;
         break;
      case HB_K_CTRL_M:
         key = K_CTRL_M;
         break;
      case HB_K_CTRL_N:
         key = K_CTRL_N;
         break;
      case HB_K_CTRL_O:
         key = K_CTRL_O;
         break;
      case HB_K_CTRL_P:
         key = K_CTRL_P;
         break;
      case HB_K_CTRL_Q:
         key = K_CTRL_Q;
         break;
      case HB_K_CTRL_R:
         key = K_CTRL_R;
         break;
      case HB_K_CTRL_S:
         key = K_CTRL_S;
         break;
      case HB_K_CTRL_T:
         key = K_CTRL_T;
         break;
      case HB_K_CTRL_U:
         key = K_CTRL_U;
         break;
      case HB_K_CTRL_V:
         key = K_CTRL_V;
         break;
      case HB_K_CTRL_W:
         key = K_CTRL_W;
         break;
      case HB_K_CTRL_X:
         key = K_CTRL_X;
         break;
      case HB_K_CTRL_Y:
         key = K_CTRL_Y;
         break;
      case HB_K_CTRL_Z:
         key = K_CTRL_Z;
         break;
      case HB_K_CTRL_LEFT_SQUARE:
         key = K_ESC;
         break;
      case HB_K_CTRL_BACK_SLASH:
         key = K_F1;
         break;
      case HB_K_CTRL_RIGHT_SQUARE:
         key = K_CTRL_HOME;
         break;
      case HB_K_CTRL_HAT:
         key = K_CTRL_PGDN;
         break;
      case HB_K_CTRL_UNDERSCORE:
         key = K_CTRL_PGUP;
         break;
      case HB_K_SPACE:
         key = K_SPACE;
         break;
      case HB_K_ALT_1:
         key = K_ALT_1;
         break;
      case HB_K_ALT_2:
         key = K_ALT_2;
         break;
      case HB_K_ALT_3:
         key = K_ALT_3;
         break;
      case HB_K_ALT_4:
         key = K_ALT_4;
         break;
      case HB_K_ALT_5:
         key = K_ALT_5;
         break;
      case HB_K_ALT_6:
         key = K_ALT_6;
         break;
      case HB_K_ALT_7:
         key = K_ALT_7;
         break;
      case HB_K_ALT_8:
         key = K_ALT_8;
         break;
      case HB_K_ALT_9:
         key = K_ALT_9;
         break;
      case HB_K_ALT_0:
         key = K_ALT_0;
         break;
      case HB_K_ALT_EQUAL:
         key = 269;
         break;
      case HB_K_CTRL_1:
         key = -99;
         break;
      case HB_K_CTRL_2:
         key = 259;
         break;
      case HB_K_CTRL_3:
         key = -99;
         break;
      case HB_K_CTRL_4:
         key = -99;
         break;
      case HB_K_CTRL_5:
         key = -99;
         break;
      case HB_K_CTRL_6:
         key = K_CTRL_PGDN;
         break;
      case HB_K_CTRL_7:
         key = -99;
         break;
      case HB_K_CTRL_8:
         key = -99;
         break;
      case HB_K_CTRL_9:
         key = -99;
         break;
      case HB_K_CTRL_0:
         key = -99;
         break;
      case HB_K_CTRL_SEMI_COLON:
         key = -99;
         break;
      case HB_K_CTRL_COMMA:
         key = -99;
         break;
      case HB_K_CTRL_PERIOD:
         key = -99;
         break;
      case HB_K_CTRL_SLASH:
         key = -99;
         break;
      case HB_K_CTRL_MINUS:
         key = K_CTRL_PGUP;
         break;
      case HB_K_CTRL_PLUS:
         key = -99;
         break;
      case HB_K_CTRL_LEFT_CURLY:
         key = K_ESC;
         break;
      case HB_K_CTRL_RIGHT_CURLY:
         key = K_CTRL_HOME;
         break;
      case HB_K_ALT_BACKSPACE:
         key = 270;
         break;
      case HB_K_CTRL_BACKSPACE:
         key = 127;
         break;
      case HB_K_ALT_ENTER:
         key = K_ALT_ENTER;
         break;
      case HB_K_SHIFT_ENTER:
         key = K_ENTER;
         break;
      case HB_K_ALT_ESC:
         key = K_ALT_ESC;
         break;
      case HB_K_CTRL_ESC:
         key = K_ESC;
         break;
      case HB_K_SHIFT_TAB:
         key = K_SH_TAB;
         break;
      case HB_K_ALT_TAB:
         key = K_ALT_TAB;
         break;
      case HB_K_CTRL_TAB:
         key = K_CTRL_TAB;
         break;
      case HB_K_F1:
         key = K_F1;
         break;
      case HB_K_F2:
         key = K_F2;
         break;
      case HB_K_F3:
         key = K_F3;
         break;
      case HB_K_F4:
         key = K_F4;
         break;
      case HB_K_F5:
         key = K_F5;
         break;
      case HB_K_F6:
         key = K_F6;
         break;
      case HB_K_F7:
         key = K_F7;
         break;
      case HB_K_F8:
         key = K_F8;
         break;
      case HB_K_F9:
         key = K_F9;
         break;
      case HB_K_F10:
         key = K_F10;
         break;
      case HB_K_F11:
         key = K_F11;
         break;
      case HB_K_F12:
         key = K_F12;
         break;
      case HB_K_ALT_F1:
         key = K_ALT_F1;
         break;
      case HB_K_ALT_F2:
         key = K_ALT_F2;
         break;
      case HB_K_ALT_F3:
         key = K_ALT_F3;
         break;
      case HB_K_ALT_F4:
         key = K_ALT_F4;
         break;
      case HB_K_ALT_F5:
         key = K_ALT_F5;
         break;
      case HB_K_ALT_F6:
         key = K_ALT_F6;
         break;
      case HB_K_ALT_F7:
         key = K_ALT_F7;
         break;
      case HB_K_ALT_F8:
         key = K_ALT_F8;
         break;
      case HB_K_ALT_F9:
         key = K_ALT_F9;
         break;
      case HB_K_ALT_F10:
         key = K_ALT_F10;
         break;
      case HB_K_ALT_F11:
         key = K_ALT_F11;
         break;
      case HB_K_ALT_F12:
         key = K_ALT_F12;
         break;
      case HB_K_CTRL_F1:
         key = K_CTRL_F1;
         break;
      case HB_K_CTRL_F2:
         key = K_CTRL_F2;
         break;
      case HB_K_CTRL_F3:
         key = K_CTRL_F3;
         break;
      case HB_K_CTRL_F4:
         key = K_CTRL_F4;
         break;
      case HB_K_CTRL_F5:
         key = K_CTRL_F5;
         break;
      case HB_K_CTRL_F6:
         key = K_CTRL_F6;
         break;
      case HB_K_CTRL_F7:
         key = K_CTRL_F7;
         break;
      case HB_K_CTRL_F8:
         key = K_CTRL_F8;
         break;
      case HB_K_CTRL_F9:
         key = K_CTRL_F9;
         break;
      case HB_K_CTRL_F10:
         key = K_CTRL_F10;
         break;
      case HB_K_CTRL_F11:
         key = K_CTRL_F11;
         break;
      case HB_K_CTRL_F12:
         key = K_CTRL_F12;
         break;
      case HB_K_SHIFT_F1:
         key = K_SH_F1;
         break;
      case HB_K_SHIFT_F2:
         key = K_SH_F2;
         break;
      case HB_K_SHIFT_F3:
         key = K_SH_F3;
         break;
      case HB_K_SHIFT_F4:
         key = K_SH_F4;
         break;
      case HB_K_SHIFT_F5:
         key = K_SH_F5;
         break;
      case HB_K_SHIFT_F6:
         key = K_SH_F6;
         break;
      case HB_K_SHIFT_F7:
         key = K_SH_F7;
         break;
      case HB_K_SHIFT_F8:
         key = K_SH_F8;
         break;
      case HB_K_SHIFT_F9:
         key = K_SH_F9;
         break;
      case HB_K_SHIFT_F10:
         key = K_SH_F10;
         break;
      case HB_K_SHIFT_F11:
         key = K_SH_F11;
         break;
      case HB_K_SHIFT_F12:
         key = K_SH_F12;
         break;
      case HB_KP_MINUS:
         key = '-';
         break;
      case HB_KP_ALT_MINUS:
         key = KP_ALT_MINUS;
         break;
      case HB_KP_CTRL_MINUS:
         key = KP_CTRL_MINUS;
         break;
      case HB_KP_SHIFT_MINUS:
         key = '-';
         break;
      case HB_KP_PLUS:
         key = '+';
         break;
      case HB_KP_ALT_PLUS:
         key = KP_ALT_PLUS;
         break;
      case HB_KP_CTRL_PLUS:
         key = KP_CTRL_PLUS;
         break;
      case HB_KP_SHIFT_PLUS:
         key = '+';
         break;
      case HB_KP_SLASH:
         key = '/';
         break;
      case HB_KP_ALT_SLASH:
         key = KP_ALT_SLASH;
         break;
      case HB_KP_CTRL_SLASH:
         key = KP_CTRL_SLASH;
         break;
      case HB_KP_SHIFT_SLASH:
         key = '/';
         break;
      case HB_KP_STAR:
         key = '*';
         break;
      case HB_KP_ALT_STAR:
         key = KP_ALT_ASTERISK;
         break;
      case HB_KP_CTRL_STAR:
         key = KP_CTRL_ASTERISK;
         break;
      case HB_KP_SHIFT_STAR:
         key = '*';
         break;
      case HB_K_HOME:
         key = K_HOME;
         break;
      case HB_K_UP:
         key = K_UP;
         break;
      case HB_K_PG_UP:
         key = K_PGUP;
         break;
      case HB_K_LEFT:
         key = K_LEFT;
         break;
      case HB_K_RIGHT:
         key = K_RIGHT;
         break;
      case HB_K_END:
         key = K_END;
         break;
      case HB_K_DOWN:
         key = K_DOWN;
         break;
      case HB_K_PG_DN:
         key = K_PGDN;
         break;
      case HB_K_INS:
         key = K_INS;
         break;
      case HB_K_DEL:
         key = K_DEL;
         break;
      case HB_K_ALT_HOME:
         key = K_ALT_HOME;
         break;
      case HB_K_ALT_UP:
         key = K_ALT_UP;
         break;
      case HB_K_ALT_PG_UP:
         key = K_ALT_PGUP;
         break;
      case HB_K_ALT_LEFT:
         key = K_ALT_LEFT;
         break;
      case HB_K_ALT_RIGHT:
         key = K_ALT_RIGHT;
         break;
      case HB_K_ALT_END:
         key = K_ALT_END;
         break;
      case HB_K_ALT_PG_DN:
         key = K_ALT_PGDN;
         break;
      case HB_K_ALT_INS:
         key = K_ALT_INS;
         break;
      case HB_K_ALT_DEL:
         key = K_ALT_DEL;
         break;
      case HB_K_CTRL_HOME:
         key = K_CTRL_HOME;
         break;
      case HB_K_CTRL_UP:
         key = K_CTRL_UP;
         break;
      case HB_K_CTRL_PG_UP:
         key = K_CTRL_PGUP;
         break;
      case HB_K_CTRL_LEFT:
         key = K_CTRL_LEFT;
         break;
      case HB_K_CTRL_RIGHT:
         key = K_CTRL_RIGHT;
         break;
      case HB_K_CTRL_END:
         key = K_CTRL_END;
         break;
      case HB_K_CTRL_DOWN:
         key = K_CTRL_DOWN;
         break;
      case HB_K_CTRL_PG_DN:
         key = K_CTRL_PGDN;
         break;
      case HB_K_CTRL_INS:
         key = K_CTRL_INS;
         break;
      case HB_K_CTRL_DEL:
         key = K_CTRL_DEL;
         break;
      case HB_K_SHIFT_HOME:
         key = K_HOME;
         break;
      case HB_K_SHIFT_UP:
         key = K_UP;
         break;
      case HB_K_SHIFT_PG_UP:
         key = K_PGUP;
         break;
      case HB_K_SHIFT_LEFT:
         key = K_LEFT;
         break;
      case HB_K_SHIFT_RIGHT:
         key = K_RIGHT;
         break;
      case HB_K_SHIFT_END:
         key = K_END;
         break;
      case HB_K_SHIFT_DOWN:
         key = K_DOWN;
         break;
      case HB_K_SHIFT_PG_DN:
         key = K_PGDN;
         break;
      case HB_K_SHIFT_INS:
         key = K_INS;
         break;
      case HB_K_SHIFT_DEL:
         key = K_DEL;
         break;
      case HB_KP_HOME:
         key = K_HOME;
         break;
      case HB_KP_UP:
         key = K_UP;
         break;
      case HB_KP_PG_UP:
         key = K_PGUP;
         break;
      case HB_KP_LEFT:
         key = K_LEFT;
         break;
      case HB_KP_5:
         key = K_UP;
         break;
      case HB_KP_RIGHT:
         key = K_RIGHT;
         break;
      case HB_KP_END:
         key = K_END;
         break;
      case HB_KP_DOWN:
         key = K_DOWN;
         break;
      case HB_KP_PG_DN:
         key = K_PGDN;
         break;
      case HB_KP_INS:
         key = K_INS;
         break;
      case HB_KP_DEL:
         key = K_DEL;
         break;
      case HB_KP_ALT_HOME:
         key = K_ALT_HOME;
         break;
      case HB_KP_ALT_UP:
         key = K_ALT_UP;
         break;
      case HB_KP_ALT_PG_UP:
         key = K_ALT_PGUP;
         break;
      case HB_KP_ALT_LEFT:
         key = K_ALT_LEFT;
         break;
      case HB_KP_ALT_5:
         key = K_ALT_5;
         break;
      case HB_KP_ALT_RIGHT:
         key = K_ALT_RIGHT;
         break;
      case HB_KP_ALT_END:
         key = K_ALT_END;
         break;
      case HB_KP_ALT_PG_DN:
         key = K_ALT_PGDN;
         break;
      case HB_KP_ALT_INS:
         key = K_ALT_INS;
         break;
      case HB_KP_ALT_DEL:
         key = K_ALT_DEL;
         break;
      case HB_KP_CTRL_HOME:
         key = K_CTRL_HOME;
         break;
      case HB_KP_CTRL_UP:
         key = K_CTRL_UP;
         break;
      case HB_KP_CTRL_PG_UP:
         key = K_CTRL_PGUP;
         break;
      case HB_KP_CTRL_LEFT:
         key = K_CTRL_LEFT;
         break;
      case HB_KP_CTRL_5:
         key = KP_CTRL_5;
         break;
      case HB_KP_CTRL_RIGHT:
         key = K_CTRL_RIGHT;
         break;
      case HB_KP_CTRL_END:
         key = K_CTRL_END;
         break;
      case HB_KP_CTRL_DOWN:
         key = K_CTRL_DOWN;
         break;
      case HB_KP_CTRL_PG_DN:
         key = K_CTRL_PGDN;
         break;
      case HB_KP_CTRL_INS:
         key = K_CTRL_INS;
         break;
      case HB_KP_CTRL_DEL:
         key = K_CTRL_DEL;
         break;
      case HB_KP_SHIFT_HOME:
         key = K_HOME;
         break;
      case HB_KP_SHIFT_UP:
         key = K_UP;
         break;
      case HB_KP_SHIFT_PG_UP:
         key = K_PGUP;
         break;
      case HB_KP_SHIFT_LEFT:
         key = K_LEFT;
         break;
      case HB_KP_SHIFT_5:
         key = '5';
         break;
      case HB_KP_SHIFT_RIGHT:
         key = K_RIGHT;
         break;
      case HB_KP_SHIFT_END:
         key = K_END;
         break;
      case HB_KP_SHIFT_DOWN:
         key = K_DOWN;
         break;
      case HB_KP_SHIFT_PG_DN:
         key = K_PGDN;
         break;
      case HB_KP_SHIFT_INS:
         key = K_INS;
         break;
      case HB_KP_SHIFT_DEL:
         key = K_DEL;
         break;
      case HB_KP_ENTER:
         key = K_ENTER;
         break;
      case HB_KP_ALT_ENTER:
         key = K_ALT_ENTER;
         break;
      case HB_KP_CTRL_ENTER:
         key = K_CTRL_ENTER;
         break;
      case HB_KP_SHIFT_ENTER:
         key = K_ENTER;
         break;
      }
   }
   if( key == -99 )
   {
      /* Ignore this key code by extracting it from the input buffer
         and discarding it. */
      hb_inkeyFetch();
      key = 0;
   }
   return key;
}
