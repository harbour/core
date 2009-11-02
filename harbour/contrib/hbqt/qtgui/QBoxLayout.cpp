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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Direction { LeftToRight, RightToLeft, TopToBottom, BottomToTop }
 */

#include <QtCore/QPointer>

#include <QtGui/QBoxLayout>


/*
 * QBoxLayout ( Direction dir, QWidget * parent = 0 )
 * ~QBoxLayout ()
 */

QT_G_FUNC( release_QBoxLayout )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QBoxLayout                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QBoxLayout * ) ph )->~QBoxLayout();
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "  Object Name Missing: QBoxLayout" );  OutputDebugString( str );
#endif
      }
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QBoxLayout" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QBOXLAYOUT )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QBoxLayout > pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QBoxLayout                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   pObj = ( QBoxLayout* ) new QBoxLayout( ( QBoxLayout::Direction ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QBoxLayout;

   hb_retptrGC( p );
}
/*
 * void addLayout ( QLayout * layout, int stretch = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDLAYOUT )
{
   hbqt_par_QBoxLayout( 1 )->addLayout( hbqt_par_QLayout( 2 ), hb_parni( 3 ) );
}

/*
 * void addSpacerItem ( QSpacerItem * spacerItem )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSPACERITEM )
{
   hbqt_par_QBoxLayout( 1 )->addSpacerItem( hbqt_par_QSpacerItem( 2 ) );
}

/*
 * void addSpacing ( int size )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSPACING )
{
   hbqt_par_QBoxLayout( 1 )->addSpacing( hb_parni( 2 ) );
}

/*
 * void addStretch ( int stretch = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSTRETCH )
{
   hbqt_par_QBoxLayout( 1 )->addStretch( hb_parni( 2 ) );
}

/*
 * void addStrut ( int size )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSTRUT )
{
   hbqt_par_QBoxLayout( 1 )->addStrut( hb_parni( 2 ) );
}

/*
 * void addWidget ( QWidget * widget, int stretch = 0, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDWIDGET )
{
   hbqt_par_QBoxLayout( 1 )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), ( Qt::Alignment ) hb_parni( 4 ) );
}

/*
 * Direction direction () const
 */
HB_FUNC( QT_QBOXLAYOUT_DIRECTION )
{
   hb_retni( ( QBoxLayout::Direction ) hbqt_par_QBoxLayout( 1 )->direction() );
}

/*
 * void insertLayout ( int index, QLayout * layout, int stretch = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTLAYOUT )
{
   hbqt_par_QBoxLayout( 1 )->insertLayout( hb_parni( 2 ), hbqt_par_QLayout( 3 ), hb_parni( 4 ) );
}

/*
 * void insertSpacerItem ( int index, QSpacerItem * spacerItem )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTSPACERITEM )
{
   hbqt_par_QBoxLayout( 1 )->insertSpacerItem( hb_parni( 2 ), hbqt_par_QSpacerItem( 3 ) );
}

/*
 * void insertSpacing ( int index, int size )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTSPACING )
{
   hbqt_par_QBoxLayout( 1 )->insertSpacing( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void insertStretch ( int index, int stretch = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTSTRETCH )
{
   hbqt_par_QBoxLayout( 1 )->insertStretch( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void insertWidget ( int index, QWidget * widget, int stretch = 0, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTWIDGET )
{
   hbqt_par_QBoxLayout( 1 )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
}

/*
 * virtual void invalidate ()
 */
HB_FUNC( QT_QBOXLAYOUT_INVALIDATE )
{
   hbqt_par_QBoxLayout( 1 )->invalidate();
}

/*
 * void setDirection ( Direction direction )
 */
HB_FUNC( QT_QBOXLAYOUT_SETDIRECTION )
{
   hbqt_par_QBoxLayout( 1 )->setDirection( ( QBoxLayout::Direction ) hb_parni( 2 ) );
}

/*
 * void setSpacing ( int spacing )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSPACING )
{
   hbqt_par_QBoxLayout( 1 )->setSpacing( hb_parni( 2 ) );
}

/*
 * void setStretch ( int index, int stretch )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCH )
{
   hbqt_par_QBoxLayout( 1 )->setStretch( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * bool setStretchFactor ( QWidget * widget, int stretch )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCHFACTOR )
{
   hb_retl( hbqt_par_QBoxLayout( 1 )->setStretchFactor( hbqt_par_QWidget( 2 ), hb_parni( 3 ) ) );
}

/*
 * bool setStretchFactor ( QLayout * layout, int stretch )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCHFACTOR_1 )
{
   hb_retl( hbqt_par_QBoxLayout( 1 )->setStretchFactor( hbqt_par_QLayout( 2 ), hb_parni( 3 ) ) );
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QBOXLAYOUT_SPACING )
{
   hb_retni( hbqt_par_QBoxLayout( 1 )->spacing() );
}

/*
 * int stretch ( int index ) const
 */
HB_FUNC( QT_QBOXLAYOUT_STRETCH )
{
   hb_retni( hbqt_par_QBoxLayout( 1 )->stretch( hb_parni( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
