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

HB_FUNC( HB_GTKCREATEMENU )
{
    GtkWidget *Menu = gtk_menu_new();
    hb_retnl( GPOINTER_TO_UINT( Menu ) );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKCREATEMENUBAR )
{
    GtkBox *Box = ( GtkBox * )GUINT_TO_POINTER( hb_parnl( 1 ) );
    if( Box )
    {
        GtkWidget *MenuBar = gtk_menu_bar_new();
        gtk_menu_bar_set_shadow_type( GTK_MENU_BAR( MenuBar ), GTK_SHADOW_OUT );
        gtk_box_pack_start( Box, MenuBar, FALSE, FALSE, 0 );
        gtk_box_reorder_child( Box, MenuBar, 0 );

        hb_retnl( GPOINTER_TO_UINT( MenuBar ) );
    }
    else
        hb_retnl( GPOINTER_TO_UINT( NULL ) );
}

/* ********************************************************************* */

static void ActivateMenuItem( GtkWidget *Widget, gpointer Data )
{
    GtkWidget *Form = ( GtkWidget * )gtk_object_get_data( GTK_OBJECT( Widget ), "Form" );
    if( !( GTK_MENU_ITEM( Widget )->submenu ) )
        CallHarbour( Form, Widget, HGF_EV_MENU, GPOINTER_TO_INT( Data ), ( PHB_ITEM )NULL );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKBARADDMENUITEM )
{
    GtkMenuBar *Bar = ( GtkMenuBar * )GUINT_TO_POINTER( hb_parnl( 1 ) );

    if( Bar )
    {
        gchar *Caption = ( gchar * )hb_parc( 2 );
        gint ItemID = ( gint )hb_parni( 3 );
        gboolean Enabled = ( gboolean )hb_parl( 4 );
        GtkWidget *Form = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 5 ) );

        GtkWidget *Item = gtk_menu_item_new_with_label( Caption );

        gtk_widget_set_sensitive( Item, Enabled );
        gtk_object_set_data( GTK_OBJECT( Item ), "Form", ( gpointer )Form );

        gtk_signal_connect
        (
            GTK_OBJECT( Item ),
            "activate",
            GTK_SIGNAL_FUNC( ( GtkSignalFunc ) ActivateMenuItem ),
            GINT_TO_POINTER( ItemID )
        );

        gtk_menu_bar_append( GTK_MENU_BAR( Bar ), Item );
        hb_retnl( GPOINTER_TO_UINT( Item ) );
    }
    else
        hb_retnl( GPOINTER_TO_UINT( NULL ) );
}

/* ********************************************************************* */

HB_FUNC( HB_GTKADDMENUITEM )
{
    GtkWidget *Menu = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 1 ) );
    GtkMenuItem *Curr = ( GtkMenuItem * )GUINT_TO_POINTER( hb_parnl( 2 ) );

    if( !Menu )
    {
        Menu = gtk_menu_new();
        gtk_menu_item_set_submenu( Curr, Menu );
        hb_stornl( GPOINTER_TO_UINT( Menu ), 1 );
    }

    if( Curr )
    {
        GtkWidget *Item = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 3 ) );
        gchar *Caption = ( gchar * )hb_parc( 4 );
        gint ItemID = ( gint )hb_parni( 5 );
        gboolean Enabled = ( gboolean )hb_parl( 6 );
        GtkWidget *Form = ( GtkWidget * )GUINT_TO_POINTER( hb_parnl( 7 ) );

        if( !Item )
            Item = gtk_menu_item_new_with_label( Caption );

        gtk_widget_set_sensitive( Item, Enabled );
        gtk_object_set_data( GTK_OBJECT( Item ), "Form", ( gpointer )Form );

        gtk_signal_connect
        (
            GTK_OBJECT( Item ),
            "activate",
            GTK_SIGNAL_FUNC( ( GtkSignalFunc ) ActivateMenuItem ),
            GINT_TO_POINTER( ItemID )
        );

        gtk_menu_append( GTK_MENU( Menu ), Item );
        hb_retnl( GPOINTER_TO_UINT( Item ) );
    }
    else
        hb_retnl( GPOINTER_TO_UINT( NULL ) );
}

/* ********************************************************************* */
