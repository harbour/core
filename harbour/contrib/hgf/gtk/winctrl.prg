/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Gtk
 * Class HBWinControl
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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
 * Additional Copyright notes :
 *  adapted for Hgf Gtk by Marek Paliwoda <paliwoda@inetia.pl>
 */

/* ********************************************************************* */

#include "common.ch"
#include "hbclass.ch"
#include "harbgtk.ch"

/* ********************************************************************* */

CLASS HBWinControl FROM HBPersistent

//PROTECTED:
/* should be PROTECTED */
EXPORTED:
    /* NOTE: because I didn't find a way to set Top and Left or Width */
    /* and Height individualy, and because Widget geometry is unknown */
    /* until it is realized I had to introduce these vars             */
    DATA      YPos  INIT 0
    DATA      XPos  INIT 0
    DATA      YSize INIT 1
    DATA      XSize INIT 1

EXPORTED:
    DATA      hWnd
    DATA      nId
    DATA      Container

    CLASSDATA nInitId   INIT 1

    ACCESS    Caption() INLINE ""          PROPERTY     // should be virtual for Gtk
    ASSIGN    Caption( cNewCaption ) INLINE cNewCaption // should be virtual for Gtk

    ACCESS    Top()    PROPERTY
    ASSIGN    Top( nNewTop ) INLINE ::YPos := nNewTop, ;
                  hb_GtkWidgetSetPos( ::hWnd, ::YPos, ::XPos )

    ACCESS    Left()   PROPERTY
    ASSIGN    Left( nNewLeft ) INLINE ::XPos := nNewLeft, ;
                  hb_GtkWidgetSetPos( ::hWnd, ::YPos, ::XPos )

    ACCESS    Height() PROPERTY
    ASSIGN    Height( nNewHeight ) INLINE ::YSize := nNewHeight, ;
                  hb_GtkWidgetSetSize( ::hWnd, ::YSize, ::XSize )

    ACCESS    Width()  PROPERTY
    ASSIGN    Width( nNewWidth ) INLINE ::XSize := nNewWidth,  ;
                  hb_GtkWidgetSetSize( ::hWnd, ::YSize, ::XSize )

    METHOD    GetNewID() INLINE ::nId := ::nInitId++

    METHOD    Show() INLINE hb_GtkShowWidget( ::hWnd )
ENDCLASS

/* ********************************************************************* */

ACCESS Top() CLASS HBWinControl
    LOCAL PosY := hb_GtkWidgetGetGeometry( ::hWnd, 1 )
    IF PosY <> HGF_GTK_WIDGET_GEOMETRY_UNKNOWN
        ::YPos := PosY
    ENDIF
RETURN ::YPos

/* ********************************************************************* */

ACCESS Left() CLASS HBWinControl
    LOCAL PosX := hb_GtkWidgetGetGeometry( ::hWnd, 2 )
    IF PosX <> HGF_GTK_WIDGET_GEOMETRY_UNKNOWN
        ::XPos := PosX
    ENDIF
RETURN ::XPos

/* ********************************************************************* */

ACCESS Height() CLASS HBWinControl
    LOCAL SizeY := hb_GtkWidgetGetGeometry( ::hWnd, 3 )
    IF SizeY <> HGF_GTK_WIDGET_GEOMETRY_UNKNOWN
        ::YSize := SizeY
    ENDIF
RETURN ::YSize

/* ********************************************************************* */

ACCESS Width() CLASS HBWinControl
    LOCAL SizeX := hb_GtkWidgetGetGeometry( ::hWnd, 4 )
    IF SizeX <> HGF_GTK_WIDGET_GEOMETRY_UNKNOWN
        ::XSize := SizeX
    ENDIF
RETURN ::XSize

/* ********************************************************************* */
