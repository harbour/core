/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Mouse API
 *
 * Copyright 1999-2009 Viktor Szakats (harbour syenar.hu)
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

#include "hbapigt.h"

/* HARBOUR callable interface */

#define M_BUTTON_LEFT   0
#define M_BUTTON_RIGHT  1
#define M_BUTTON_MIDDLE 2

#ifdef HB_COMPAT_C53

HB_FUNC( MPRESENT )
{
   hb_retl( hb_mouseIsPresent() );
}

HB_FUNC( MHIDE )
{
   hb_mouseSetCursor( HB_FALSE );
}

HB_FUNC( MSHOW )
{
   hb_mouseSetCursor( HB_TRUE );
}

HB_FUNC( MSETCURSOR )
{
   hb_retl( hb_mouseGetCursor() );

   if( HB_ISLOG( 1 ) )
      hb_mouseSetCursor( hb_parl( 1 ) );
}

HB_FUNC( MROW )
{
   if( hb_parl( 1 ) )
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
   if( hb_parl( 1 ) )
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
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
      hb_mouseSetPos( hb_parni( 1 ), hb_parni( 2 ) );
}

HB_FUNC( MLEFTDOWN )
{
   hb_retl( hb_mouseButtonState( M_BUTTON_LEFT ) );
}

HB_FUNC( MRIGHTDOWN )
{
   hb_retl( hb_mouseButtonState( M_BUTTON_RIGHT ) );
}

HB_FUNC( MDBLCLK )
{
   hb_retni( hb_mouseGetDoubleClickSpeed() );

   if( HB_ISNUM( 1 ) )
      hb_mouseSetDoubleClickSpeed( hb_parni( 1 ) );
}

HB_FUNC( MSAVESTATE )
{
   int iLen = hb_mouseStorageSize();

   if( iLen > 0 )
   {
      void * pBuffer = hb_xgrab( iLen + 1 );

      hb_mouseSaveState( pBuffer );
      hb_retclen_buffer( ( char * ) pBuffer, iLen );
   }
   else
      hb_retc_null();
}

HB_FUNC( MRESTSTATE )
{
   if( hb_parclen( 1 ) == ( HB_SIZE ) hb_mouseStorageSize() )
      hb_mouseRestoreState( hb_parc( 1 ) );
}

HB_FUNC( MSETBOUNDS )
{
   hb_mouseSetBounds( hb_parni( 1 ), /* Defaults to zero on bad type */
                      hb_parni( 2 ), /* Defaults to zero on bad type */
                      HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_gtMaxRow(),
                      HB_ISNUM( 4 ) ? hb_parni( 4 ) : hb_gtMaxCol() );
}

#endif

HB_FUNC( HB_MMIDDLEDOWN )
{
   hb_retl( hb_mouseButtonState( M_BUTTON_MIDDLE ) );
}
