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

/* "delete_event" event handler - will be used for VALID clause in the future */
static gint DeleteEventWindowCallback( GtkWidget *Widget, GdkEventAny *Event, gpointer Data )
{
    gint DeleteAllowed = FALSE;

    /* the code needs to be added here in the future */

    return ( DeleteAllowed );
}

/* ********************************************************************* */

/* "destroy" signal handler */
static gint DestroyWindowCallback( GtkWidget *Widget, gpointer Data )
{
    gint DestroyAllowed = FALSE;

    /* the code needs to be added here in the future */

    if( !DestroyAllowed )
    {
        /* the code needs to be added here in the future */

        if( Widget == gtk_widget_get_toplevel( Widget ) )
            gtk_main_quit();
    }

    return ( DestroyAllowed );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKWINDOWCREATE )
{
    GtkWidget *MainWin, *VBox, *MoveBar, *MenuBar,
              *ScrlWin, *LayoutW, *Current;
    GtkTooltips *ToolTip = NULL;

    /* for the future enhancements */
    gint YSize=400, XSize=500;

    MainWin = gtk_window_new( GTK_WINDOW_TOPLEVEL );
    gtk_window_set_default_size( GTK_WINDOW( MainWin ), XSize, YSize );
    gtk_window_set_policy( GTK_WINDOW( MainWin ), TRUE, TRUE, FALSE );

    VBox = gtk_vbox_new( FALSE, 1 );
    gtk_container_add( GTK_CONTAINER( MainWin ), VBox );

    /* for the future enhancements */
    Current = VBox;

    LayoutW = gtk_layout_new( NULL, NULL );
    gtk_layout_set_size( GTK_LAYOUT( LayoutW ), XSize, YSize );
    gtk_box_pack_start( GTK_BOX( Current ), LayoutW, TRUE, TRUE, 0 );

    /* for the future signal handling */
    gtk_signal_connect
    (
        GTK_OBJECT( MainWin ),
        "delete_event",
        GTK_SIGNAL_FUNC( ( GtkSignalFunc ) DeleteEventWindowCallback ),
        ( gpointer ) NULL
    );

    /* for the future signal handling */
    gtk_signal_connect
    (
        GTK_OBJECT( MainWin ),
        "destroy",
        GTK_SIGNAL_FUNC( ( GtkSignalFunc ) DestroyWindowCallback ),
        ( gpointer ) NULL
    );

    /* for the future enhancements */
    ScrlWin = MoveBar = MenuBar = NULL;

    {
        PHB_ITEM ReturnArray, ArrayItem;

        ReturnArray = hb_itemArrayNew( 7 );
        ArrayItem = hb_itemNew( NULL );

        hb_itemPutNL( ArrayItem, GPOINTER_TO_UINT( MainWin ) );
        hb_itemArrayPut( ReturnArray, 1, ArrayItem );

        hb_itemPutNL( ArrayItem, GPOINTER_TO_UINT( VBox ) );
        hb_itemArrayPut( ReturnArray, 2, ArrayItem );

        hb_itemPutNL( ArrayItem, GPOINTER_TO_UINT( ScrlWin ) );
        hb_itemArrayPut( ReturnArray, 3, ArrayItem );

        hb_itemPutNL( ArrayItem, GPOINTER_TO_UINT( LayoutW ) );
        hb_itemArrayPut( ReturnArray, 4, ArrayItem );

        hb_itemPutNL( ArrayItem, GPOINTER_TO_UINT( MoveBar ) );
        hb_itemArrayPut( ReturnArray, 5, ArrayItem );

        hb_itemPutNL( ArrayItem, GPOINTER_TO_UINT( MenuBar ) );
        hb_itemArrayPut( ReturnArray, 6, ArrayItem );

        hb_itemPutNL( ArrayItem, GPOINTER_TO_UINT( ToolTip ) );
        hb_itemArrayPut( ReturnArray, 7, ArrayItem );

        hb_itemReturn( ReturnArray );
        hb_itemRelease( ReturnArray );
        hb_itemRelease( ArrayItem );
    }
}

/* ********************************************************************* */

HB_FUNC( HB_GTKWINGETTEXT )
{
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );

    if( hWnd )
    {
        GtkWindow *Win = ( GtkWindow * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
        hb_retc( ( char * )Win->title );
    }
    else
        hb_retc( "" );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKWINSETTEXT )
{
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );

    if( hWnd )
    {
        gchar *Title = hb_parc( 2 ); /* either tilte or "" */
        GtkWindow *Win = ( GtkWindow * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
        gtk_window_set_title( Win, Title );
    }
}

/* ********************************************************************* */

HB_FUNC( HB_GTKSHOWMODAL )
{
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );

    if( hWnd )
    {
        GtkWidget *Win = ( GtkWidget * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
        gtk_widget_show_all( Win );
        /* NOTE : this does not make any sense. It is only an example */
        gtk_main();
    }
}

/* ********************************************************************* */
