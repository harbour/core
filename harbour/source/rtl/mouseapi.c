/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Mouse API
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    API interface proposal
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"
#include "itemapi.h"
#include "mouseapi.h"
#include "gtapi.h"

/* TODO: This level should make sure that if there's no mouse present 
         the functions don't call the low level function, but return some
         default values. */

static BOOL   s_bPresent;
static BOOL   s_bVisible;
static USHORT s_uiDoubleClickSpeed; /* In milliseconds */
static int    s_iLeftButton;
static int    s_iRightButton;

/* C callable interface */

void hb_mouseInit( void )
{
   HB_TRACE(("hb_mouseInit()"));

   hb_mouse_Init();

   s_bPresent = hb_mouse_IsPresent();

   hb_mouseSetCursor( FALSE );

   s_uiDoubleClickSpeed = 168;
   s_iLeftButton = 1;                      /* TOFIX: */
   s_iRightButton = hb_mouseCountButton(); /* TOFIX: */
}

void hb_mouseExit( void )
{
   HB_TRACE(("hb_mouseExit()"));

   hb_mouse_Exit();
}

BOOL hb_mouseIsPresent( void )
{
   HB_TRACE(("hb_mouseIsPresent()"));

   return hb_mouse_IsPresent();
}

BOOL hb_mouseGetCursor( void )
{
   HB_TRACE(("hb_mouseGetCursor()"));

   return s_bVisible;
}

void hb_mouseSetCursor( BOOL bVisible )
{
   HB_TRACE(("hb_mouseSetCursor(%d)", (int) bVisible));

   if( bVisible )
   {
      hb_mouse_Show();
      s_bVisible = TRUE;
   }
   else
   {
      hb_mouse_Hide();
      s_bVisible = FALSE;
   }
}

int hb_mouseCol( void )
{
   HB_TRACE(("hb_mouseCol()"));

   return hb_mouse_Col();
}

int hb_mouseRow( void )
{
   HB_TRACE(("hb_mouseRow()"));

   return hb_mouse_Row();
}

void hb_mouseSetPos( int iRow, int iCol )
{
   HB_TRACE(("hb_mouseSetPos(%d, %d)", iRow, iCol));

   hb_mouse_SetPos( iRow, iCol );
}

BOOL hb_mouseIsButtonPressed( int iButton )
{
   HB_TRACE(("hb_mouseIsButtonPressed(%d)", iButton));

   return hb_mouse_IsButtonPressed( iButton );
}

int hb_mouseCountButton( void )
{
   HB_TRACE(("hb_mouseCountButton()"));

   return hb_mouse_CountButton();
}

void hb_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_TRACE(("hb_mouseSetBounds(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight));

   hb_mouse_SetBounds( iTop, iLeft, iBottom, iRight );
}

void hb_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   HB_TRACE(("hb_mouseSetBounds(%p, %p, %p, %p)", piTop, piLeft, piBottom, piRight));

   hb_mouse_GetBounds( piTop, piLeft, piBottom, piRight );
}

/* HARBOUR callable interface */

#ifdef HB_COMPAT_C53

HARBOUR MPRESENT( void )
{
   hb_retl( hb_mouseIsPresent() );
}

HARBOUR MHIDE( void )
{
   hb_mouseSetCursor( FALSE );
}

HARBOUR MSHOW( void )
{
   hb_mouseSetCursor( TRUE );
}

HARBOUR MSETCURSOR( void )
{
   hb_retl( hb_mouseGetCursor() );

   if( ISLOG( 1 ) )
      hb_mouseSetCursor( hb_parl( 1 ) );
}

HARBOUR MROW( void )
{
   hb_retni( hb_mouseRow() );   
}

HARBOUR MCOL( void )
{
   hb_retni( hb_mouseCol() );   
}

HARBOUR MSETPOS( void )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_mouseSetPos( hb_parni( 1 ), hb_parni( 2 ) );
}

HARBOUR MRIGHTDOWN( void )
{
   hb_retl( hb_mouseIsButtonPressed( s_iRightButton ) );
}

HARBOUR MLEFTDOWN( void )
{
   hb_retl( hb_mouseIsButtonPressed( s_iLeftButton ) );
}

HARBOUR MDBLCLK( void )
{
   hb_retni( s_uiDoubleClickSpeed );

   if( ISNUM( 1 ) )
   {
      int uiDoubleClickSpeed = hb_parni( 1 );

      if( uiDoubleClickSpeed > 0 )
         s_uiDoubleClickSpeed = uiDoubleClickSpeed;
   }
}

HARBOUR MSAVESTATE( void )
{
   int iTop, iLeft, iBottom, iRight;

   USHORT uiPos;
   USHORT uiLen = sizeof( int ) * 2 + 
                  sizeof( BOOL ) +
                  sizeof( int ) * 4;

   BYTE * pBuffer = ( BYTE * ) hb_xgrab( uiLen );
  
   hb_mouseGetBounds( &iTop, &iLeft, &iBottom, &iRight );

   uiPos = 0;
   *( pBuffer + uiPos ) = hb_mouseRow();
   uiPos += sizeof( int );
   *( pBuffer + uiPos ) = hb_mouseCol();
   uiPos += sizeof( int );
   *( pBuffer + uiPos ) = s_bVisible;
   uiPos += sizeof( BOOL );
   *( pBuffer + uiPos ) = iTop;
   uiPos += sizeof( int );
   *( pBuffer + uiPos ) = iLeft;
   uiPos += sizeof( int );
   *( pBuffer + uiPos ) = iBottom;
   uiPos += sizeof( int );
   *( pBuffer + uiPos ) = iRight;

   hb_retclen( (char *) pBuffer, uiLen );

   hb_xfree( pBuffer );
}

HARBOUR MRESTSTATE( void )
{
   USHORT uiLen = sizeof( int ) * 2 + 
                  sizeof( BOOL ) +
                  sizeof( int ) * 4;

   if( ISCHAR( 1 ) && hb_parclen( 1 ) == uiLen )
   {
      int iRow, iCol;
      int iTop, iLeft, iBottom, iRight;
      BOOL bVisible;

      USHORT uiPos;

      BYTE * pBuffer = ( BYTE * ) hb_parc( 1 );

      uiPos = 0;
      iRow = *( pBuffer + uiPos );
      uiPos += sizeof( int );
      iCol = *( pBuffer + uiPos );
      uiPos += sizeof( int );
      bVisible = *( pBuffer + uiPos );
      uiPos += sizeof( BOOL );
      iTop = *( pBuffer + uiPos );
      uiPos += sizeof( int );
      iLeft = *( pBuffer + uiPos );
      uiPos += sizeof( int );
      iBottom = *( pBuffer + uiPos );
      uiPos += sizeof( int );
      iRight = *( pBuffer + uiPos );

      hb_mouseSetPos( iRow, iCol );
      hb_mouseSetBounds( iTop, iLeft, iBottom, iRight );
      hb_mouseSetCursor( bVisible );
   }
}

HARBOUR MSETBOUNDS( void )
{
   int iTop    = ISNUM( 1 ) ? hb_parni( 1 ) : 0;
   int iLeft   = ISNUM( 2 ) ? hb_parni( 2 ) : 0; 
   int iBottom = ISNUM( 3 ) ? hb_parni( 3 ) : hb_gtMaxRow();
   int iRight  = ISNUM( 4 ) ? hb_parni( 4 ) : hb_gtMaxCol();

   hb_mouseSetBounds( iTop, iLeft, iBottom, iRight );
}

#endif

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible function */

HARBOUR HB_NUMBUTTONS( void )
{
   hb_retni( hb_mouseCountButton() );
}

/* NOTE: XBase++ compatible function */

HARBOUR HB_SETMOUSE( void )
{
   hb_retl( hb_mouseGetCursor() );

   if( ISLOG( 1 ) )
      hb_mouseSetCursor( hb_parl( 1 ) );

   {
      PHB_ITEM pRow = hb_param( 2, IT_NUMERIC );
      PHB_ITEM pCol = hb_param( 3, IT_NUMERIC );

      if( pRow || pCol )
      {
         hb_mouseSetPos( pRow ? hb_itemGetNI( pRow ) : hb_mouseRow() ,
                         pCol ? hb_itemGetNI( pCol ) : hb_mouseCol() );
      }
   }
}

#endif
