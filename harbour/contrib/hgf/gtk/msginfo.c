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

static void GtkMsgInfoCallback( GtkWidget *Widget, gpointer Data )
{
    if( GTK_IS_WIDGET( ( GtkWidget * ) Data ) )
        gtk_widget_destroy( ( GtkWidget * ) Data );

    gtk_main_quit();
}

/* ********************************************************************* */

HB_FUNC( MSGINFO )
{
    gchar *Message = g_strdelimit(
                                     ( gchar * ) hb_parc( 1 ),
                                     MultiLineDelimiters, '\n'
                                 );
    GtkWidget *InfoWin = gtk_dialog_new();
    GtkWidget *Label = gtk_label_new( Message );
    GtkWidget *BtnOK = gtk_button_new_with_label( "Ok" );

    gtk_window_set_title( GTK_WINDOW( InfoWin ), "Message" );
    gtk_window_set_position( GTK_WINDOW( InfoWin ), GTK_WIN_POS_CENTER );
    gtk_container_border_width( GTK_CONTAINER( InfoWin ), 0 );
    gtk_container_border_width
    (
        GTK_CONTAINER( GTK_BOX( GTK_DIALOG( InfoWin )->vbox ) ), 5
    );

    gtk_window_set_modal( GTK_WINDOW( InfoWin ), TRUE );
    gtk_window_set_policy( GTK_WINDOW( InfoWin ), FALSE, FALSE, FALSE );

    gtk_signal_connect
    (
        GTK_OBJECT( InfoWin ),
        "destroy",
        GTK_SIGNAL_FUNC( ( GtkSignalFunc ) GtkMsgInfoCallback ),
        NULL
    );

    gtk_signal_connect
    (
        GTK_OBJECT( BtnOK ),
        "clicked",
        GTK_SIGNAL_FUNC( ( GtkSignalFunc ) GtkMsgInfoCallback ),
        ( gpointer ) InfoWin
    );

    gtk_box_pack_start
    (
        GTK_BOX( GTK_BOX( GTK_DIALOG( InfoWin )->vbox ) ),
        Label, TRUE, TRUE, 10
    );

    gtk_box_pack_start
    (
        GTK_BOX( GTK_BOX( GTK_DIALOG( InfoWin )->action_area ) ),
        BtnOK, TRUE, TRUE, 10
    );

    gtk_widget_show_all( InfoWin );
    gtk_main();
}

/* ********************************************************************* */
