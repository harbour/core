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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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
#include "inkey.ch"

#include <time.h>

static int *  s_inkeyBuffer = 0; /* Harbour keyboard buffer (empty if head == tail)     */
static int    s_inkeyHead;       /* Harbour keyboard buffer head pointer (next insert)  */
static int    s_inkeyTail;       /* Harbour keyboard buffer tail pointer (next extract) */
static int    s_inkeyLast;       /* Last key extracted from Harbour keyboard buffer     */
static BOOL   s_inkeyPoll;       /* Flag to override no polling when TYPEAHEAD is 0     */
static int    s_inkeyForce;      /* Variable to hold keyboard input when TYPEAHEAD is 0 */
static HB_inkey_enum s_eventmask;

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
            while( hb_inkeyNext() == 0 )
               hb_idleState();
         }
      }
      else
      {
         clock_t end_clock = clock() + ( clock_t ) ( dSeconds * CLOCKS_PER_SEC );

         while( hb_inkeyNext() == 0 && clock() < end_clock )
            hb_idleState();
      }
   }

   key = hb_inkeyGet();                        /* Get the current input event or 0 */

   s_inkeyPoll = FALSE;                        /* Stop forced polling */
   s_eventmask = hb_set.HB_SET_EVENTMASK;      /* Restore original input event mask */

   return key;
}

int hb_inkeyGet( void )       /* Extract the next key from the keyboard buffer */
{
   int key;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyGet()"));

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
      key = s_inkeyLast = s_inkeyForce; /* Typeahead support is disabled */

   s_inkeyForce = 0;

   return key;
}

int hb_inkeyLast( void )      /* Return the value of the last key that was extracted */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast()"));

   hb_inkeyPoll();

   return s_inkeyLast;
}

int hb_inkeyNext( void )      /* Return the next key without extracting it */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext()"));

   hb_inkeyPoll();

   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is enabled */
      if( s_inkeyHead == s_inkeyTail )
         return 0;                               /* No key */
      else
         return s_inkeyBuffer[ s_inkeyTail ];    /* Next key */
   }

   return s_inkeyForce; /* Typeahead support is disabled */
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
            if( ! hb_set.HB_SET_CANCEL ) /* If cancel is disabled, */
               ch = 0;                   /* then ignore the keystroke */
                                    /* In either case, handle like Alt+C */
         case K_ALT_C:              /* Alt+C was pressed */
            hb_vmRequestCancel();
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
   hb_retni( hb_inkeyNext() );
}

HB_FUNC( LASTKEY )
{
   hb_retni( s_inkeyLast );
}

