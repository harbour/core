/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __XSAVESCREEN()/__XRESTSCREEN() functions
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    Rewritten in C
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapigt.h"

/* NOTE: In original CA-Cl*pper 5.x these functions are written in Clipper
         [vszakats] */

static SHORT s_iRow;
static SHORT s_iCol;
static void * s_pBuffer = NULL;

void hb_conXSaveRestRelease( void )
{
   if( s_pBuffer )
   {
      hb_xfree( s_pBuffer );
      s_pBuffer = NULL;
   }
}

HB_FUNC( __XSAVESCREEN )
{
   if( s_pBuffer != NULL )
      hb_xfree( s_pBuffer );

   hb_gtGetPos( &s_iRow, &s_iCol );

   {
      USHORT uiSize;
      hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &uiSize );
      s_pBuffer = hb_xgrab( uiSize );
   }

   hb_gtSave( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), s_pBuffer );
}

/* NOTE: There's no check about the screen size on restore, so this will
         fail if the user has changed the screen resolution between calling 
         save and restore.
         [vszakats] */

HB_FUNC( __XRESTSCREEN )
{
   if( s_pBuffer != NULL )
   {
      hb_gtRest( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), s_pBuffer );
      hb_xfree( s_pBuffer );
      s_pBuffer = NULL;

      hb_gtSetPosContext( s_iRow, s_iCol, HB_GT_SET_POS_AFTER );
   }
}

