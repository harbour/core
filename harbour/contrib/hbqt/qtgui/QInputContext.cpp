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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum StandardFormat { PreeditFormat, SelectionFormat }
 */

/*
 *  Constructed[ 11/13 [ 84.62% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  virtual QList<QAction *> actions ()
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // virtual bool x11FilterEvent ( QWidget * keywidget, XEvent * event )
 */

#include <QtCore/QPointer>

#include <QtGui/QInputContext>
#include <QtGui/QTextFormat>

/* QInputContext ( QObject * parent = 0 )
 * virtual ~QInputContext ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QInputContext > pq;
} QGC_POINTER_QInputContext;

QT_G_FUNC( hbqt_gcRelease_QInputContext )
{
   HB_SYMBOL_UNUSED( Cargo );
}

void * hbqt_gcAllocate_QInputContext( void * pObj, bool bNew )
{
   QGC_POINTER_QInputContext * p = ( QGC_POINTER_QInputContext * ) hb_gcAllocate( sizeof( QGC_POINTER_QInputContext ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QInputContext;

   if( bNew )
   {
      new( & p->pq ) QPointer< QInputContext >( ( QInputContext * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QInputContext              ph=%p", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QINPUTCONTEXT )
{
}

/*
 * virtual bool filterEvent ( const QEvent * event )
 */
HB_FUNC( QT_QINPUTCONTEXT_FILTEREVENT )
{
   hb_retl( hbqt_par_QInputContext( 1 )->filterEvent( hbqt_par_QEvent( 2 ) ) );
}

/*
 * virtual QFont font () const
 */
HB_FUNC( QT_QINPUTCONTEXT_FONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QInputContext( 1 )->font() ), true ) );
}

/*
 * virtual QString identifierName () = 0
 */
HB_FUNC( QT_QINPUTCONTEXT_IDENTIFIERNAME )
{
   hb_retc( hbqt_par_QInputContext( 1 )->identifierName().toAscii().data() );
}

/*
 * virtual bool isComposing () const = 0
 */
HB_FUNC( QT_QINPUTCONTEXT_ISCOMPOSING )
{
   hb_retl( hbqt_par_QInputContext( 1 )->isComposing() );
}

/*
 * virtual QString language () = 0
 */
HB_FUNC( QT_QINPUTCONTEXT_LANGUAGE )
{
   hb_retc( hbqt_par_QInputContext( 1 )->language().toAscii().data() );
}

/*
 * virtual void mouseHandler ( int x, QMouseEvent * event )
 */
HB_FUNC( QT_QINPUTCONTEXT_MOUSEHANDLER )
{
   hbqt_par_QInputContext( 1 )->mouseHandler( hb_parni( 2 ), hbqt_par_QMouseEvent( 3 ) );
}

/*
 * virtual void reset () = 0
 */
HB_FUNC( QT_QINPUTCONTEXT_RESET )
{
   hbqt_par_QInputContext( 1 )->reset();
}

/*
 * void sendEvent ( const QInputMethodEvent & event )
 */
HB_FUNC( QT_QINPUTCONTEXT_SENDEVENT )
{
   hbqt_par_QInputContext( 1 )->sendEvent( *hbqt_par_QInputMethodEvent( 2 ) );
}

/*
 * QTextFormat standardFormat ( StandardFormat s ) const
 */
HB_FUNC( QT_QINPUTCONTEXT_STANDARDFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextFormat( new QTextFormat( hbqt_par_QInputContext( 1 )->standardFormat( ( QInputContext::StandardFormat ) hb_parni( 2 ) ) ), true ) );
}

/*
 * virtual void update ()
 */
HB_FUNC( QT_QINPUTCONTEXT_UPDATE )
{
   hbqt_par_QInputContext( 1 )->update();
}

/*
 * virtual void widgetDestroyed ( QWidget * widget )
 */
HB_FUNC( QT_QINPUTCONTEXT_WIDGETDESTROYED )
{
   hbqt_par_QInputContext( 1 )->widgetDestroyed( hbqt_par_QWidget( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
