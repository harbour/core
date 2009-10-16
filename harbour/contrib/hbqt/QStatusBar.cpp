/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QStatusBar>


/*
 * QStatusBar ( QWidget * parent = 0 )
 * virtual ~QStatusBar ()
 */

HB_FUNC( QT_QSTATUSBAR )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   QPointer< QStatusBar > pObj = NULL;

   pObj = ( QStatusBar* ) new QStatusBar( hbqt_par_QWidget( 1 ) ) ;

   p->ph = pObj;
   p->type = 1001;
   hb_retptrGC( p );
}
/*
 * void addPermanentWidget ( QWidget * widget, int stretch = 0 )
 */
HB_FUNC( QT_QSTATUSBAR_ADDPERMANENTWIDGET )
{
   hbqt_par_QStatusBar( 1 )->addPermanentWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
}

/*
 * void addWidget ( QWidget * widget, int stretch = 0 )
 */
HB_FUNC( QT_QSTATUSBAR_ADDWIDGET )
{
   hbqt_par_QStatusBar( 1 )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
}

/*
 * QString currentMessage () const
 */
HB_FUNC( QT_QSTATUSBAR_CURRENTMESSAGE )
{
   hb_retc( hbqt_par_QStatusBar( 1 )->currentMessage().toAscii().data() );
}

/*
 * int insertPermanentWidget ( int index, QWidget * widget, int stretch = 0 )
 */
HB_FUNC( QT_QSTATUSBAR_INSERTPERMANENTWIDGET )
{
   hb_retni( hbqt_par_QStatusBar( 1 )->insertPermanentWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parni( 4 ) ) );
}

/*
 * int insertWidget ( int index, QWidget * widget, int stretch = 0 )
 */
HB_FUNC( QT_QSTATUSBAR_INSERTWIDGET )
{
   hb_retni( hbqt_par_QStatusBar( 1 )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parni( 4 ) ) );
}

/*
 * bool isSizeGripEnabled () const
 */
HB_FUNC( QT_QSTATUSBAR_ISSIZEGRIPENABLED )
{
   hb_retl( hbqt_par_QStatusBar( 1 )->isSizeGripEnabled() );
}

/*
 * void removeWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSTATUSBAR_REMOVEWIDGET )
{
   hbqt_par_QStatusBar( 1 )->removeWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * void setSizeGripEnabled ( bool )
 */
HB_FUNC( QT_QSTATUSBAR_SETSIZEGRIPENABLED )
{
   hbqt_par_QStatusBar( 1 )->setSizeGripEnabled( hb_parl( 2 ) );
}

/*
 * void clearMessage ()
 */
HB_FUNC( QT_QSTATUSBAR_CLEARMESSAGE )
{
   hbqt_par_QStatusBar( 1 )->clearMessage();
}

/*
 * void showMessage ( const QString & message, int timeout = 0 )
 */
HB_FUNC( QT_QSTATUSBAR_SHOWMESSAGE )
{
   hbqt_par_QStatusBar( 1 )->showMessage( hbqt_par_QString( 2 ), hb_parni( 3 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
