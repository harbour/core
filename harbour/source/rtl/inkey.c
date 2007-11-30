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
 * Copyright 2002 Walter Negro <anegro@overnet.com.ar>
 *    hb_inkeySetLast()
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 *    HB_SETLASTKEY()
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz>
 *    HB_SETINKEYBEFOREBLOCK()
 *    HB_SETINKEYAFTERBLOCK()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#if defined( HB_OS_OS2 )
#  define INCL_DOSPROCESS
#  define INCL_NOPMAPI
#endif

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbdate.h"
#include "inkey.ch"

static int    s_defaultKeyBuffer[ HB_DEFAULT_INKEY_BUFSIZE + 1 ];

static int *  s_inkeyBuffer = s_defaultKeyBuffer;
static int    s_inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
static int    s_inkeyHead = 0;
static int    s_inkeyTail = 0;
static int    s_iLastPut = 0;

static BYTE * s_StrBuffer = NULL;
static ULONG  s_StrBufferSize;
static ULONG  s_StrBufferPos;

static int    s_inkeyLast = 0;

static PHB_ITEM s_inKeyBlockBefore = NULL;
static PHB_ITEM s_inKeyBlockAfter  = NULL;


static int hb_inkeyFilter( int iKey, int iEventMask )
{
   int iMask;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyFilter(%d,%d)", iKey, iEventMask));

   switch( iKey )
   {
      case K_MOUSEMOVE:
      case K_MMLEFTDOWN:
      case K_MMRIGHTDOWN:
      case K_MMMIDDLEDOWN:
      case K_NCMOUSEMOVE:
         iMask = INKEY_MOVE;
         break;
      case K_LBUTTONDOWN:
      case K_LDBLCLK:
         iMask = INKEY_LDOWN;
         break;
      case K_LBUTTONUP:
         iMask = INKEY_LUP;
         break;
      case K_RBUTTONDOWN:
      case K_RDBLCLK:
         iMask = INKEY_RDOWN;
         break;
      case K_RBUTTONUP:
         iMask = INKEY_RUP;
         break;
      case K_MBUTTONDOWN:
      case K_MBUTTONUP:
      case K_MDBLCLK:
         iMask = INKEY_MMIDDLE;
         break;
      case K_MWFORWARD:
      case K_MWBACKWARD:
         iMask = INKEY_MWHEEL;
         break;
      default:
         iMask = INKEY_KEYBOARD;
         break;
   }

   if( ( iMask & iEventMask ) == 0 )
      return 0;

   return iKey;
}

/* drop the next key in keyboard buffer */
static void hb_inkeyPop( void )
{
   if( s_StrBuffer )
   {
      if( ++s_StrBufferPos >= s_StrBufferSize )
      {
         hb_xfree( s_StrBuffer );
         s_StrBuffer = NULL;
      }
   }
   else if( s_inkeyHead != s_inkeyTail )
   {
      if( ++s_inkeyTail >= s_inkeyBufferSize )
         s_inkeyTail = 0;
   }
}

/* Put the key into keyboard buffer */
HB_EXPORT void hb_inkeyPut( int iKey )
{
   int iHead = s_inkeyHead;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPut(%d)", iKey));

   if( iKey == K_MOUSEMOVE )
   {
      /*
       * Clipper does not store in buffer repeated mouse movement
       * IMHO it's good idea to reduce unnecessary inkey buffer
       * overloading so I also implemented it, [druzus]
       */
      if( s_iLastPut == iKey && s_inkeyHead != s_inkeyTail )
         return;
   }

   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   s_inkeyBuffer[ iHead++ ] = s_iLastPut = iKey;
   if( iHead >= s_inkeyBufferSize )
   {
      iHead = 0;
   }

   if( iHead != s_inkeyTail )
   {
      s_inkeyHead = iHead;
   }
}

static BOOL hb_inkeyNextCheck( int iEventMask, int * iKey )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_inkeyNextCheck(%p)", iKey) );

   if( s_StrBuffer )
   {
      *iKey = s_StrBuffer[ s_StrBufferPos ];
   }
   else if( s_inkeyHead != s_inkeyTail )
   {
      *iKey = hb_inkeyFilter( s_inkeyBuffer[ s_inkeyTail ], iEventMask );
   }
   else
   {
      return FALSE;
   }

   if( *iKey == 0 )
   {
      hb_inkeyPop();
      return FALSE;
   }

   return TRUE;
}

static void hb_inkeyPollDo( void )
{
   int iKey;

   HB_TRACE( HB_TR_DEBUG, ("hb_inkeyPollDo()") );

   iKey = hb_gtReadKey( INKEY_ALL );

   if( iKey )
   {
      switch( iKey )
      {
         case HB_BREAK_FLAG:           /* Check for Ctrl+Break */
         case K_ALT_C:                 /* Check for normal Alt+C */
            if( hb_set.HB_SET_CANCEL )
            {
               hb_vmRequestCancel();   /* Request cancellation */
               return;
            }
            break;
         case K_ALT_D:                 /* Check for Alt+D */
            if( hb_set.HB_SET_DEBUG )
            {
               hb_vmRequestDebug();    /* Request the debugger */
               return;
            }
      }
      hb_inkeyPut( iKey );
   }
}

/* Poll the console keyboard to stuff the Harbour buffer */
HB_EXPORT void hb_inkeyPoll( void )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_inkeyPoll()") );

   /*
    * Clipper 5.3 always poll events without respecting
    * hb_set.HB_SET_TYPEAHEAD when CL5.2 only when it's non zero.
    * IMHO keeping CL5.2 behavior will be more accurate for xharbour
    * because it allow to control it by user what some times could be
    * necessary due to different low level GT behavior on some platforms
    */
   if( hb_set.HB_SET_TYPEAHEAD )
   {
      hb_inkeyPollDo();
   }
}

/* Return the next key without extracting it */
HB_EXPORT int hb_inkeyNext( int iEventMask )
{
   int iKey = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext(%d)", iEventMask));

   hb_inkeyPoll();
   hb_inkeyNextCheck( iEventMask, &iKey );

   return iKey;
}

/* Wait for keyboard input */
HB_EXPORT int hb_inkey( BOOL fWait, double dSeconds, int iEventMask )
{
   HB_ULONG end_timer = 0;
   BOOL fPop;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkey(%d, %f, %d)", (int) fWait, dSeconds, iEventMask));

   /* Wait forever ?, Use fixed value 100 for strict Clipper compatibility */
   if( fWait && dSeconds * 100 >= 1 )
   {
      end_timer = hb_dateMilliSeconds() + ( HB_ULONG ) ( dSeconds * 1000 );
   }

   do
   {
      hb_inkeyPollDo();
      fPop = hb_inkeyNextCheck( iEventMask, &s_inkeyLast );

      if( fPop )
      {
         break;
      }

      /* immediately break if a VM request is pending. */
      if( !fWait || hb_vmRequestQuery() != 0 )
      {
         return 0;
      }

      hb_idleState();
   }
   while( end_timer == 0 || end_timer > hb_dateMilliSeconds() );

   hb_idleReset();

   if( fPop )
   {
      hb_inkeyPop();
      return s_inkeyLast;
   }

   return 0;
}

/* Return the value of the last key that was extracted */
HB_EXPORT int hb_inkeyLast( int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast(%d)", iEventMask));

   hb_inkeyPoll();

   return hb_inkeyFilter( s_inkeyLast, iEventMask );
}

/* Force a value to s_inkeyLast and return previous value */
HB_EXPORT int hb_inkeySetLast( int iKey )
{
   int iLast = s_inkeyLast;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetLast(%d)", iKey));

   s_inkeyLast = iKey;

   return iLast;
}

/* Set text into inkey buffer */
HB_EXPORT void hb_inkeySetText( const char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetText(%s,%lu)", szText, ulLen));

   if( s_StrBuffer )
   {
      hb_xfree( s_StrBuffer );
      s_StrBuffer = NULL;
   }

   if( szText && ulLen )
   {
      s_StrBuffer = ( BYTE * ) hb_xgrab( ulLen );
      memcpy( s_StrBuffer, szText, ulLen );
      s_StrBufferSize = ulLen;
      s_StrBufferPos = 0;
   }
}

/* Reset the keyboard buffer */
HB_EXPORT void hb_inkeyReset( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyReset()"));

   if( s_StrBuffer )
   {
      hb_xfree( s_StrBuffer );
      s_StrBuffer = NULL;
   }

   s_inkeyHead = 0;
   s_inkeyTail = 0;

   if( hb_set.HB_SET_TYPEAHEAD != s_inkeyBufferSize )
   {
      if( s_inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE )
      {
         hb_xfree( s_inkeyBuffer );
      }
      if( hb_set.HB_SET_TYPEAHEAD > HB_DEFAULT_INKEY_BUFSIZE )
      {
         s_inkeyBufferSize = hb_set.HB_SET_TYPEAHEAD;
         s_inkeyBuffer = ( int * ) hb_xgrab( s_inkeyBufferSize * sizeof( int ) );
      }
      else
      {
         s_inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
         s_inkeyBuffer = s_defaultKeyBuffer;
      }
   }
}

/* reset inkey pool to default state and free any allocated resources */
HB_EXPORT void hb_inkeyExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyExit()"));

   if( s_inKeyBlockBefore )
   {
      hb_itemRelease( s_inKeyBlockBefore );
      s_inKeyBlockBefore = NULL;
   }
   if( s_inKeyBlockAfter )
   {
      hb_itemRelease( s_inKeyBlockAfter );
      s_inKeyBlockAfter = NULL;
   }

   if( s_StrBuffer )
   {
      hb_xfree( s_StrBuffer );
      s_StrBuffer = NULL;
   }
   if( s_inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE )
   {
      hb_xfree( s_inkeyBuffer );
      s_inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
      s_inkeyBuffer = s_defaultKeyBuffer;
   }
}

HB_EXPORT void hb_inkeySetCancelKeys( int CancelKey, int CancelKeyEx )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetCancelKeys(%d,%d)", CancelKey, CancelKeyEx));

/*
   s_InkeyAltC = CancelKey;
   s_InkeyAltCEx = CancelKeyEx;
*/
   HB_SYMBOL_UNUSED( CancelKey );
   HB_SYMBOL_UNUSED( CancelKeyEx );
}

HB_FUNC( INKEY )
{
   USHORT uiPCount = hb_pcount();
   PHB_ITEM pKey = NULL;
   int iKey;

   if( s_inKeyBlockBefore )
      hb_vmEvalBlock( s_inKeyBlockBefore );

   do
   {
      iKey = hb_inkey( uiPCount == 1 || ( uiPCount > 1 && ISNUM( 1 ) ),
                       hb_parnd( 1 ),
                       ISNUM( 2 ) ? hb_parni( 2 ) : hb_set.HB_SET_EVENTMASK );

      if( iKey == 0 || !s_inKeyBlockAfter )
         break;

      pKey = hb_itemPutNI( pKey, iKey );
      iKey = hb_itemGetNI( hb_vmEvalBlockV( s_inKeyBlockAfter, 1, pKey ) );
      hb_inkeySetLast( iKey );
   }
   while( iKey == 0 );

   if( pKey )
      hb_itemRelease( pKey );

   hb_retni( iKey );
}

/* temporary disabled */
#if 0
HB_FUNC( HB_SETINKEYBEFOREBLOCK )
{
   if( s_inKeyBlockBefore )
      hb_itemReturn( s_inKeyBlockBefore );

   if( hb_pcount() > 0 )
   {
      PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

      if( pBlock )
         pBlock = hb_itemNew( pBlock );

      if( s_inKeyBlockBefore )
         hb_itemRelease( s_inKeyBlockBefore );
      s_inKeyBlockBefore = pBlock;
   }
}

HB_FUNC( HB_SETINKEYAFTERBLOCK )
{
   if( s_inKeyBlockAfter )
      hb_itemReturn( s_inKeyBlockAfter );

   if( hb_pcount() > 0 )
   {
      PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

      if( pBlock )
         pBlock = hb_itemNew( pBlock );

      if( s_inKeyBlockAfter )
         hb_itemRelease( s_inKeyBlockAfter );
      s_inKeyBlockAfter = pBlock;
   }
}
#endif

HB_FUNC( __KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   hb_inkeyReset();

   if( ISCHAR( 1 ) )
      hb_inkeySetText( hb_parc( 1 ), hb_parclen( 1 ) );
}

HB_FUNC( HB_KEYPUT )
{
   if( ISNUM( 1 ) )
      hb_inkeyPut( hb_parni( 1 ) );
   else if( ISCHAR( 1 ) )
   {
      PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
      char * szText = hb_itemGetCPtr( pText );
      ULONG ulLen = hb_itemGetCLen( pText ), ulIndex;

      for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
         hb_inkeyPut( szText[ ulIndex ] );
   }
   else if( ISARRAY( 1 ) )
   {
      PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
      ULONG ulElements = hb_arrayLen( pArray ), ulIndex;

      for( ulIndex = 1; ulIndex <= ulElements; ulIndex++ )
      {
         if( hb_arrayGetType( pArray, ulIndex ) & HB_IT_NUMERIC )
            hb_inkeyPut( hb_arrayGetNI( pArray, ulIndex ) );
      }
   }
}

HB_FUNC( NEXTKEY )
{
   hb_retni( hb_inkeyNext( ISNUM( 1 ) ? hb_parni( 1 ) : hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( LASTKEY )
{
   hb_retni( hb_inkeyLast( ISNUM( 1 ) ? hb_parni( 1 ) : INKEY_ALL ) );
}

HB_FUNC( HB_SETLASTKEY )
{
   if( ISNUM(1) )
      hb_retni( hb_inkeySetLast( hb_parni( 1 ) ) );
}
