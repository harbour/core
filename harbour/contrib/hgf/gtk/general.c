/*
 * $"Id"$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Gtk
 *
 * Copyright 2001 Marek Paliwoda <paliwoda@inetia.pl>
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

/* ********************************************************************* */

#include "harbgtk.h"

/* ********************************************************************* */

#include "shared.ch"

/* ********************************************************************* */

HB_FUNC( HB_GTKSHOWWIDGET )
{
    GtkWidget *Widget;
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );

    if( hWnd )
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
    else
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 1 ) );

    if( Widget )
        gtk_widget_show( Widget );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKDESTROYWIDGET )
{
    GtkWidget *Widget;
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );

    if( hWnd )
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
    else
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 1 ) );

    if( Widget )
        gtk_widget_destroy( Widget );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKWIDGETSETPOS )
{
    GtkWidget *Widget;
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );

    if( hWnd )
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
    else
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 1 ) );

    if( Widget )
        gtk_widget_set_uposition( Widget,  hb_parni( 3 ), hb_parni( 2 ) );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKWIDGETSETSIZE )
{
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );
    GtkWidget *Widget;

    if( hWnd )
    {
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
        if( Widget )
            gtk_window_set_default_size( GTK_WINDOW( Widget ), hb_parni( 3 ), hb_parni( 2 ) );
    }
    else
    {
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 1 ) );
        if( Widget )
            gtk_widget_set_usize( Widget,  hb_parni( 3 ), hb_parni( 2 ) );
    }
}

/* ********************************************************************* */

HB_FUNC( HB_GTKWIDGETGETGEOMETRY )
{
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );
    GtkWidget *Widget;

    if( hWnd )
    {
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );

        if( Widget && GTK_WIDGET_REALIZED( Widget ) )
        {
            gint x, y, w, h;
            gdk_window_get_origin( Widget->window, &x, &y );
            gdk_window_get_size( Widget->window, &w, &h );

            switch ( hb_parni( 2 ) )
            {
                case 1:   hb_retni( y );  break;
                case 2:   hb_retni( x );  break;
                case 3:   hb_retni( h );  break;
                case 4:   hb_retni( w );  break;
                default:  hb_retni( HGF_GTK_WIDGET_GEOMETRY_UNKNOWN );
            }
        }
        else
            hb_retni( HGF_GTK_WIDGET_GEOMETRY_UNKNOWN );
    }
    else
    {
        Widget = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 1 ) );

        if( Widget && GTK_WIDGET_REALIZED( Widget ) )
            switch ( hb_parni( 2 ) )
            {
                case 1:  hb_retni( Widget->allocation.y );      break;
                case 2:  hb_retni( Widget->allocation.x );      break;
                case 3:  hb_retni( Widget->allocation.height ); break;
                case 4:  hb_retni( Widget->allocation.width );  break;
                default: hb_retni( HGF_GTK_WIDGET_GEOMETRY_UNKNOWN );
            }
        else
            hb_retni( HGF_GTK_WIDGET_GEOMETRY_UNKNOWN );
    }
}

/* ********************************************************************* */
