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

#include <QtCore/QPointer>

#include <QtGui/QLayoutItem>


/*
 * QLayoutItem ( Qt::Alignment alignment = 0 )
 * virtual ~QLayoutItem ()
 */

QT_G_FUNC( release_QLayoutItem )
{
   HB_SYMBOL_UNUSED( Cargo );
}

HB_FUNC( QT_QLAYOUTITEM )
{
}
/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QLAYOUTITEM_ALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QLayoutItem( 1 )->alignment() );
}

/*
 * QSizePolicy::ControlTypes controlTypes () const
 */
HB_FUNC( QT_QLAYOUTITEM_CONTROLTYPES )
{
   hb_retni( ( QSizePolicy::ControlTypes ) hbqt_par_QLayoutItem( 1 )->controlTypes() );
}

/*
 * virtual Qt::Orientations expandingDirections () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_EXPANDINGDIRECTIONS )
{
   hb_retni( ( Qt::Orientations ) hbqt_par_QLayoutItem( 1 )->expandingDirections() );
}

/*
 * virtual QRect geometry () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_GEOMETRY )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QLayoutItem( 1 )->geometry() ) ) );
}

/*
 * virtual bool hasHeightForWidth () const
 */
HB_FUNC( QT_QLAYOUTITEM_HASHEIGHTFORWIDTH )
{
   hb_retl( hbqt_par_QLayoutItem( 1 )->hasHeightForWidth() );
}

/*
 * virtual int heightForWidth ( int w ) const
 */
HB_FUNC( QT_QLAYOUTITEM_HEIGHTFORWIDTH )
{
   hb_retni( hbqt_par_QLayoutItem( 1 )->heightForWidth( hb_parni( 2 ) ) );
}

/*
 * virtual void invalidate ()
 */
HB_FUNC( QT_QLAYOUTITEM_INVALIDATE )
{
   hbqt_par_QLayoutItem( 1 )->invalidate();
}

/*
 * virtual bool isEmpty () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_ISEMPTY )
{
   hb_retl( hbqt_par_QLayoutItem( 1 )->isEmpty() );
}

/*
 * virtual QLayout * layout ()
 */
HB_FUNC( QT_QLAYOUTITEM_LAYOUT )
{
   hb_retptr( ( QLayout* ) hbqt_par_QLayoutItem( 1 )->layout() );
}

/*
 * virtual QSize maximumSize () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_MAXIMUMSIZE )
{
   hb_retptrGC( gcAllocate_QSize( new QSize( hbqt_par_QLayoutItem( 1 )->maximumSize() ) ) );
}

/*
 * virtual int minimumHeightForWidth ( int w ) const
 */
HB_FUNC( QT_QLAYOUTITEM_MINIMUMHEIGHTFORWIDTH )
{
   hb_retni( hbqt_par_QLayoutItem( 1 )->minimumHeightForWidth( hb_parni( 2 ) ) );
}

/*
 * virtual QSize minimumSize () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_MINIMUMSIZE )
{
   hb_retptrGC( gcAllocate_QSize( new QSize( hbqt_par_QLayoutItem( 1 )->minimumSize() ) ) );
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUTITEM_SETALIGNMENT )
{
   hbqt_par_QLayoutItem( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * virtual void setGeometry ( const QRect & r ) = 0
 */
HB_FUNC( QT_QLAYOUTITEM_SETGEOMETRY )
{
   hbqt_par_QLayoutItem( 1 )->setGeometry( *hbqt_par_QRect( 2 ) );
}

/*
 * virtual QSize sizeHint () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_SIZEHINT )
{
   hb_retptrGC( gcAllocate_QSize( new QSize( hbqt_par_QLayoutItem( 1 )->sizeHint() ) ) );
}

/*
 * virtual QSpacerItem * spacerItem ()
 */
HB_FUNC( QT_QLAYOUTITEM_SPACERITEM )
{
   hb_retptr( ( QSpacerItem* ) hbqt_par_QLayoutItem( 1 )->spacerItem() );
}

/*
 * virtual QWidget * widget ()
 */
HB_FUNC( QT_QLAYOUTITEM_WIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QLayoutItem( 1 )->widget() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
