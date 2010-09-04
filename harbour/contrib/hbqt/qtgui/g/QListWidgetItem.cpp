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
 * www - http://harbour-project.org
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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ItemType { Type, UserType }
 */

#include <QtCore/QPointer>

#include <QtGui/QListWidgetItem>


/*
 * QListWidgetItem ( QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QString & text, QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QIcon & icon, const QString & text, QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QListWidgetItem & other )
 * virtual ~QListWidgetItem ()
 */

typedef struct
{
   QListWidgetItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QListWidgetItem;

HBQT_GC_FUNC( hbqt_gcRelease_QListWidgetItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QListWidgetItem   /.\\", p->ph ) );
         delete ( ( QListWidgetItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QListWidgetItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QListWidgetItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QListWidgetItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QListWidgetItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QListWidgetItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QListWidgetItem;
   p->type = HBQT_TYPE_QListWidgetItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QListWidgetItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QListWidgetItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLISTWIDGETITEM )
{
   QListWidgetItem * pObj = NULL;

   pObj = new QListWidgetItem( hbqt_par_QListWidget( 1 ), hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( void * ) pObj, true ) );
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_BACKGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_BACKGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_CHECKSTATE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::CheckState ) ( p )->checkState() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_CHECKSTATE FP=hb_retni( ( Qt::CheckState ) ( p )->checkState() ); p is NULL" ) );
   }
}

/*
 * virtual QListWidgetItem * clone () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_CLONE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->clone(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_CLONE FP=hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->clone(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QVariant data ( int role ) const
 */
HB_FUNC( QT_QLISTWIDGETITEM_DATA )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_DATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_FLAGS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::ItemFlags ) ( p )->flags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_FLAGS FP=hb_retni( ( Qt::ItemFlags ) ( p )->flags() ); p is NULL" ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_FONT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_FOREGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_FOREGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) ); p is NULL" ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_ICON )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_ISHIDDEN )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isHidden() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_ISHIDDEN FP=hb_retl( ( p )->isHidden() ); p is NULL" ) );
   }
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_ISSELECTED )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isSelected() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_ISSELECTED FP=hb_retl( ( p )->isSelected() ); p is NULL" ) );
   }
}

/*
 * QListWidget * listWidget () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_LISTWIDGET )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidget( ( p )->listWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_LISTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QListWidget( ( p )->listWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QLISTWIDGETITEM_READ )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->read( *hbqt_par_QDataStream( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_READ FP=( p )->read( *hbqt_par_QDataStream( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETBACKGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETBACKGROUND FP=( p )->setBackground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETCHECKSTATE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETCHECKSTATE FP=( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setData ( int role, const QVariant & value )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETDATA )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETDATA FP=( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETFLAGS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETFLAGS FP=( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETFONT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETFOREGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setForeground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETFOREGROUND FP=( p )->setForeground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHidden ( bool hide )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETHIDDEN )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setHidden( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETHIDDEN FP=( p )->setHidden( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETICON )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETICON FP=( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setSelected ( bool select )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETSELECTED )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setSelected( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETSELECTED FP=( p )->setSelected( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeHint ( const QSize & size )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETSIZEHINT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setSizeHint( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETSIZEHINT FP=( p )->setSizeHint( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETSTATUSTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setStatusTip( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETSTATUSTIP FP=( p )->setStatusTip( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETTEXT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setText( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETTEXT FP=( p )->setText( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextAlignment ( int alignment )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETTEXTALIGNMENT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setTextAlignment( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETTEXTALIGNMENT FP=( p )->setTextAlignment( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETTOOLTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setToolTip( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETTOOLTIP FP=( p )->setToolTip( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWhatsThis ( const QString & whatsThis )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETWHATSTHIS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setWhatsThis( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SETWHATSTHIS FP=( p )->setWhatsThis( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSize sizeHint () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_SIZEHINT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_STATUSTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->statusTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_STATUSTIP FP=hb_retc( ( p )->statusTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TEXT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int textAlignment () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TEXTALIGNMENT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->textAlignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_TEXTALIGNMENT FP=hb_retni( ( p )->textAlignment() ); p is NULL" ) );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TOOLTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->toolTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_TOOLTIP FP=hb_retc( ( p )->toolTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int type () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TYPE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_TYPE FP=hb_retni( ( p )->type() ); p is NULL" ) );
   }
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_WHATSTHIS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->whatsThis().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_WHATSTHIS FP=hb_retc( ( p )->whatsThis().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual void write ( QDataStream & out ) const
 */
HB_FUNC( QT_QLISTWIDGETITEM_WRITE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->write( *hbqt_par_QDataStream( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLISTWIDGETITEM_WRITE FP=( p )->write( *hbqt_par_QDataStream( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
