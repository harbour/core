/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Mouse API
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    API proposal
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbgtcore.h"

static int    s_iLeftButton = 0;
static int    s_iRightButton = 1;

/* NOTE: Mouse initialization is called directly from low level GT driver
 * because it possible that mouse subsystem can depend on the terminal
 * (for example, mouse subsystem cannot be initialized before ncurses
 * driver is initialized).
*/
/* C callable interface */

HB_EXPORT BOOL hb_mouseIsPresent( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseIsPresent()"));

   return hb_mouse_IsPresent();
}

HB_EXPORT BOOL hb_mouseGetCursor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseGetCursor()"));

   return hb_mouse_GetCursor();
}

HB_EXPORT void hb_mouseSetCursor( BOOL fVisible )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetCursor(%d)", (int) fVisible));

   hb_mouse_SetCursor( fVisible );
}

HB_EXPORT int hb_mouseCol( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseCol()"));

   return hb_mouse_Col();
}

HB_EXPORT int hb_mouseRow( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseRow()"));

   return hb_mouse_Row();
}

HB_EXPORT void hb_mouseGetPos( int * piRow, int * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetPos(%p, %p)", piRow, piCol));

   hb_mouse_GetPos( piRow, piCol );
}

HB_EXPORT void hb_mouseSetPos( int iRow, int iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetPos(%d, %d)", iRow, iCol));

   hb_mouse_SetPos( iRow, iCol );
}

HB_EXPORT void hb_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetBounds(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight));

   hb_mouse_SetBounds( iTop, iLeft, iBottom, iRight );
}

HB_EXPORT void hb_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetBounds(%p, %p, %p, %p)", piTop, piLeft, piBottom, piRight));

   hb_mouse_GetBounds( piTop, piLeft, piBottom, piRight );
}

HB_EXPORT int hb_mouseStorageSize( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseStorageSize()"));

   return hb_mouse_StorageSize();
}

HB_EXPORT void hb_mouseSaveState( BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSaveState(%p)", pBuffer));

   hb_mouse_SaveState( pBuffer );
}

HB_EXPORT void hb_mouseRestoreState( BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseRestoreState(%p)", pBuffer));

   hb_mouse_RestoreState( pBuffer );
}

HB_EXPORT int hb_mouseGetDoubleClickSpeed( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseGetDoubleClickSpeed()"));

   return hb_mouse_GetDoubleClickSpeed();
}

HB_EXPORT void hb_mouseSetDoubleClickSpeed( int iSpeed )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetDoubleClickSpeed(%d)", iSpeed));

   hb_mouse_SetDoubleClickSpeed( iSpeed );
}

HB_EXPORT int hb_mouseCountButton( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseCountButton()"));

   return hb_mouse_CountButton();
}

HB_EXPORT BOOL hb_mouseButtonState( int iButton )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseButtonState(%d)", iButton));

   return hb_mouse_ButtonState( iButton );
}

HB_EXPORT BOOL hb_mouseButtonPressed( int iButton, int * piRow, int * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseButtonPressed(%d,%p,%p)", iButton, piRow, piCol));

   return hb_mouse_ButtonPressed( iButton, piRow, piCol );
}

HB_EXPORT BOOL hb_mouseButtonReleased( int iButton, int * piRow, int * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseButtonReleased(%d,%p,%p)", iButton, piRow, piCol));

   return hb_mouse_ButtonReleased( iButton, piRow, piCol );
}

HB_EXPORT int hb_mouseReadKey( int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseReadKey(%d)", iEventMask));

   return hb_mouse_ReadKey( iEventMask );
}


/* HARBOUR callable interface */

#ifdef HB_COMPAT_C53

HB_FUNC( MPRESENT )
{
   hb_retl( hb_mouseIsPresent() );
}

HB_FUNC( MHIDE )
{
   hb_mouseSetCursor( FALSE );
}

HB_FUNC( MSHOW )
{
   hb_mouseSetCursor( TRUE );
}

HB_FUNC( MSETCURSOR )
{
   hb_retl( hb_mouseGetCursor() );

   if( ISLOG( 1 ) )
      hb_mouseSetCursor( hb_parl( 1 ) );
}

HB_FUNC( MROW )
{
   if( ISLOG( 1 ) )
   {
      int iRow, iCol;

      hb_mouseGetPos( &iRow, &iCol );
      hb_retni( iRow );
   }
   else
      hb_retni( hb_mouseRow() );
}

HB_FUNC( MCOL )
{
   if( ISLOG( 1 ) )
   {
      int iRow, iCol;

      hb_mouseGetPos( &iRow, &iCol );
      hb_retni( iCol );
   }
   else
      hb_retni( hb_mouseCol() );
}

HB_FUNC( MSETPOS )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_mouseSetPos( hb_parni( 1 ), hb_parni( 2 ) );
}

HB_FUNC( MRIGHTDOWN )
{
   hb_retl( hb_mouseButtonState( s_iRightButton ) );
}

HB_FUNC( MLEFTDOWN )
{
   hb_retl( hb_mouseButtonState( s_iLeftButton ) );
}

HB_FUNC( MDBLCLK )
{
   hb_retni( hb_mouseGetDoubleClickSpeed() );

   if( ISNUM( 1 ) )
   {
      hb_mouseSetDoubleClickSpeed( hb_parni( 1 ) );
   }
}

HB_FUNC( MSAVESTATE )
{
   int iLen = hb_mouseStorageSize();

   if( iLen > 0 )
   {
      BYTE * pBuffer = ( BYTE * ) hb_xgrab( iLen + 1 );

      hb_mouseSaveState( pBuffer );
      hb_retclen_buffer( ( char * ) pBuffer, iLen );
   }
   else
      hb_retc( NULL );
}

HB_FUNC( MRESTSTATE )
{
   if( ISCHAR( 1 ) && hb_parclen( 1 ) == ( ULONG ) hb_mouseStorageSize() )
   {
      hb_mouseRestoreState( ( BYTE * ) hb_parc( 1 ) );
   }
}

HB_FUNC( MSETBOUNDS )
{
   hb_mouseSetBounds( hb_parni( 1 ), /* Defaults to zero on bad type */
                      hb_parni( 2 ), /* Defaults to zero on bad type */
                      ISNUM( 3 ) ? hb_parni( 3 ) : hb_gtMaxRow(),
                      ISNUM( 4 ) ? hb_parni( 4 ) : hb_gtMaxCol() );
}

#endif
