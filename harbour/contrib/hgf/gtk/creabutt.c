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

static gint ButtonClick( GtkWidget *Widget, gpointer Data )
{
    GtkWidget *Form = ( GtkWidget * )gtk_object_get_data( GTK_OBJECT( Widget ), "Form" );
    return( CallHarbour( Form, Widget, HGF_EV_CLICK, GPOINTER_TO_INT( Data ), ( PHB_ITEM )NULL ) );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKBUTTONCREATE )
{
    PHB_ITEM hWnd = hb_param( 1, HB_IT_ARRAY  );

    if( hWnd )
    {
        GtkWindow *Form = ( GtkWindow * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 1 ) );
        GtkLayout *LayO = ( GtkLayout * )GUINT_TO_POINTER( hb_arrayGetNL( hWnd, 4 ) );
        gint ButtID = ( gint )hb_parni( 2 );

        GtkWidget *Button = gtk_button_new_with_label( "" );
        gtk_object_set_data( GTK_OBJECT( Button ), "Form", ( gpointer )Form );

        gtk_signal_connect
        (
            GTK_OBJECT( Button ),
            "clicked",
            GTK_SIGNAL_FUNC( ( GtkSignalFunc ) ButtonClick ),
            GINT_TO_POINTER( ButtID )
        );

        if( LayO )
            gtk_layout_put( LayO, Button, 0, 0 );

        hb_retnl( GPOINTER_TO_UINT( Button ) );
    }
    else
        hb_retnl( GPOINTER_TO_UINT( NULL ) );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKBUTTONGETTEXT )
{
    GtkButton *Button = ( GtkButton * )GPOINTER_TO_UINT( hb_parnl( 1 ) );
    if( Button )
    {
        GtkLabel *Label = ( GtkLabel * )( GTK_BIN( Button )->child );
        if( Label )
            hb_retc( ( char * )Label->label );
        else
            hb_retc( "" );
    }
    else
        hb_retc( "" );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKBUTTONSETTEXT )
{
    GtkButton *Button = ( GtkButton * )GPOINTER_TO_UINT( hb_parnl( 1 ) );
    if( Button )
    {
        GtkLabel *Label = ( GtkLabel * )( GTK_BIN( Button )->child );
        if( Label )
            gtk_label_set_text( Label, hb_parc( 2 ) );
    }
}

/* ********************************************************************* */
