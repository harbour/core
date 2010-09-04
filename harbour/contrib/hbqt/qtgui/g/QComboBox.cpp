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
 *  enum InsertPolicy { NoInsert, InsertAtTop, InsertAtCurrent, InsertAtBottom, ..., InsertAlphabetically }
 *  enum SizeAdjustPolicy { AdjustToContents, AdjustToContentsOnFirstShow, AdjustToMinimumContentsLength, AdjustToMinimumContentsLengthWithIcon }
 */

#include <QtCore/QPointer>

#include <QtGui/QComboBox>


/*
 * QComboBox ( QWidget * parent = 0 )
 * ~QComboBox ()
 */

typedef struct
{
   QPointer< QComboBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QComboBox;

HBQT_GC_FUNC( hbqt_gcRelease_QComboBox )
{
   QComboBox  * ph = NULL ;
   HBQT_GC_T_QComboBox * p = ( HBQT_GC_T_QComboBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QComboBox   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QComboBox   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QComboBox          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QComboBox    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QComboBox    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QComboBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QComboBox * p = ( HBQT_GC_T_QComboBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QComboBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QComboBox >( ( QComboBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QComboBox;
   p->type = HBQT_TYPE_QComboBox;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QComboBox  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QComboBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCOMBOBOX )
{
   QComboBox * pObj = NULL;

   pObj =  new QComboBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QComboBox( ( void * ) pObj, true ) );
}

/*
 * void addItem ( const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_ADDITEM )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->addItem( QComboBox::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ADDITEM FP=( p )->addItem( QComboBox::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) ); p is NULL" ) );
   }
}

/*
 * void addItem ( const QIcon & icon, const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_ADDITEM_1 )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->addItem( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QComboBox::tr( hb_parc( 3 ) ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QVariant( 4 ) : QVariant() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ADDITEM_1 FP=( p )->addItem( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QComboBox::tr( hb_parc( 3 ) ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QVariant( 4 ) : QVariant() ) ); p is NULL" ) );
   }
}

/*
 * void addItems ( const QStringList & texts )
 */
HB_FUNC( QT_QCOMBOBOX_ADDITEMS )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->addItems( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ADDITEMS FP=( p )->addItems( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * QCompleter * completer () const
 */
HB_FUNC( QT_QCOMBOBOX_COMPLETER )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCompleter( ( p )->completer(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_COMPLETER FP=hb_retptrGC( hbqt_gcAllocate_QCompleter( ( p )->completer(), false ) ); p is NULL" ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QCOMBOBOX_COUNT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QCOMBOBOX_CURRENTINDEX )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->currentIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_CURRENTINDEX FP=hb_retni( ( p )->currentIndex() ); p is NULL" ) );
   }
}

/*
 * QString currentText () const
 */
HB_FUNC( QT_QCOMBOBOX_CURRENTTEXT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retc( ( p )->currentText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_CURRENTTEXT FP=hb_retc( ( p )->currentText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool duplicatesEnabled () const
 */
HB_FUNC( QT_QCOMBOBOX_DUPLICATESENABLED )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retl( ( p )->duplicatesEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_DUPLICATESENABLED FP=hb_retl( ( p )->duplicatesEnabled() ); p is NULL" ) );
   }
}

/*
 * int findData ( const QVariant & data, int role = Qt::UserRole, Qt::MatchFlags flags = Qt::MatchExactly | Qt::MatchCaseSensitive ) const
 */
HB_FUNC( QT_QCOMBOBOX_FINDDATA )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->findData( *hbqt_par_QVariant( 2 ), hb_parnidef( 3, Qt::UserRole ), ( HB_ISNUM( 4 ) ? ( Qt::MatchFlags ) hb_parni( 4 ) : ( Qt::MatchFlags ) Qt::MatchExactly | Qt::MatchCaseSensitive ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_FINDDATA FP=hb_retni( ( p )->findData( *hbqt_par_QVariant( 2 ), hb_parnidef( 3, Qt::UserRole ), ( HB_ISNUM( 4 ) ? ( Qt::MatchFlags ) hb_parni( 4 ) : ( Qt::MatchFlags ) Qt::MatchExactly | Qt::MatchCaseSensitive ) ) ); p is NULL" ) );
   }
}

/*
 * int findText ( const QString & text, Qt::MatchFlags flags = Qt::MatchExactly | Qt::MatchCaseSensitive ) const
 */
HB_FUNC( QT_QCOMBOBOX_FINDTEXT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->findText( QComboBox::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( Qt::MatchFlags ) hb_parni( 3 ) : ( Qt::MatchFlags ) Qt::MatchExactly | Qt::MatchCaseSensitive ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_FINDTEXT FP=hb_retni( ( p )->findText( QComboBox::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( Qt::MatchFlags ) hb_parni( 3 ) : ( Qt::MatchFlags ) Qt::MatchExactly | Qt::MatchCaseSensitive ) ) ); p is NULL" ) );
   }
}

/*
 * bool hasFrame () const
 */
HB_FUNC( QT_QCOMBOBOX_HASFRAME )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retl( ( p )->hasFrame() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_HASFRAME FP=hb_retl( ( p )->hasFrame() ); p is NULL" ) );
   }
}

/*
 * virtual void hidePopup ()
 */
HB_FUNC( QT_QCOMBOBOX_HIDEPOPUP )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->hidePopup();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_HIDEPOPUP FP=( p )->hidePopup(); p is NULL" ) );
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QCOMBOBOX_ICONSIZE )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ICONSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * void insertItem ( int index, const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTITEM )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->insertItem( hb_parni( 2 ), QComboBox::tr( hb_parc( 3 ) ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QVariant( 4 ) : QVariant() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_INSERTITEM FP=( p )->insertItem( hb_parni( 2 ), QComboBox::tr( hb_parc( 3 ) ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QVariant( 4 ) : QVariant() ) ); p is NULL" ) );
   }
}

/*
 * void insertItem ( int index, const QIcon & icon, const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTITEM_1 )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->insertItem( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QIcon( 3 ) : QIcon( hbqt_par_QString( 3 ) ) ), QComboBox::tr( hb_parc( 4 ) ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QVariant( 5 ) : QVariant() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_INSERTITEM_1 FP=( p )->insertItem( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QIcon( 3 ) : QIcon( hbqt_par_QString( 3 ) ) ), QComboBox::tr( hb_parc( 4 ) ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QVariant( 5 ) : QVariant() ) ); p is NULL" ) );
   }
}

/*
 * void insertItems ( int index, const QStringList & list )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTITEMS )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->insertItems( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_INSERTITEMS FP=( p )->insertItems( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) ); p is NULL" ) );
   }
}

/*
 * InsertPolicy insertPolicy () const
 */
HB_FUNC( QT_QCOMBOBOX_INSERTPOLICY )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( QComboBox::InsertPolicy ) ( p )->insertPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_INSERTPOLICY FP=hb_retni( ( QComboBox::InsertPolicy ) ( p )->insertPolicy() ); p is NULL" ) );
   }
}

/*
 * void insertSeparator ( int index )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTSEPARATOR )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->insertSeparator( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_INSERTSEPARATOR FP=( p )->insertSeparator( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool isEditable () const
 */
HB_FUNC( QT_QCOMBOBOX_ISEDITABLE )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retl( ( p )->isEditable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ISEDITABLE FP=hb_retl( ( p )->isEditable() ); p is NULL" ) );
   }
}

/*
 * QVariant itemData ( int index, int role = Qt::UserRole ) const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMDATA )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->itemData( hb_parni( 2 ), hb_parnidef( 3, Qt::UserRole ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ITEMDATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->itemData( hb_parni( 2 ), hb_parnidef( 3, Qt::UserRole ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMDELEGATE )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ITEMDELEGATE FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate(), false ) ); p is NULL" ) );
   }
}

/*
 * QIcon itemIcon ( int index ) const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMICON )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->itemIcon( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ITEMICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->itemIcon( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString itemText ( int index ) const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMTEXT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retc( ( p )->itemText( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ITEMTEXT FP=hb_retc( ( p )->itemText( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QLineEdit * lineEdit () const
 */
HB_FUNC( QT_QCOMBOBOX_LINEEDIT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLineEdit( ( p )->lineEdit(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_LINEEDIT FP=hb_retptrGC( hbqt_gcAllocate_QLineEdit( ( p )->lineEdit(), false ) ); p is NULL" ) );
   }
}

/*
 * int maxCount () const
 */
HB_FUNC( QT_QCOMBOBOX_MAXCOUNT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->maxCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_MAXCOUNT FP=hb_retni( ( p )->maxCount() ); p is NULL" ) );
   }
}

/*
 * int maxVisibleItems () const
 */
HB_FUNC( QT_QCOMBOBOX_MAXVISIBLEITEMS )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->maxVisibleItems() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_MAXVISIBLEITEMS FP=hb_retni( ( p )->maxVisibleItems() ); p is NULL" ) );
   }
}

/*
 * int minimumContentsLength () const
 */
HB_FUNC( QT_QCOMBOBOX_MINIMUMCONTENTSLENGTH )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->minimumContentsLength() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_MINIMUMCONTENTSLENGTH FP=hb_retni( ( p )->minimumContentsLength() ); p is NULL" ) );
   }
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QCOMBOBOX_MODEL )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_MODEL FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) ); p is NULL" ) );
   }
}

/*
 * int modelColumn () const
 */
HB_FUNC( QT_QCOMBOBOX_MODELCOLUMN )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( p )->modelColumn() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_MODELCOLUMN FP=hb_retni( ( p )->modelColumn() ); p is NULL" ) );
   }
}

/*
 * void removeItem ( int index )
 */
HB_FUNC( QT_QCOMBOBOX_REMOVEITEM )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->removeItem( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_REMOVEITEM FP=( p )->removeItem( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QModelIndex rootModelIndex () const
 */
HB_FUNC( QT_QCOMBOBOX_ROOTMODELINDEX )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->rootModelIndex() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_ROOTMODELINDEX FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->rootModelIndex() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setCompleter ( QCompleter * completer )
 */
HB_FUNC( QT_QCOMBOBOX_SETCOMPLETER )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setCompleter( hbqt_par_QCompleter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETCOMPLETER FP=( p )->setCompleter( hbqt_par_QCompleter( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDuplicatesEnabled ( bool enable )
 */
HB_FUNC( QT_QCOMBOBOX_SETDUPLICATESENABLED )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setDuplicatesEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETDUPLICATESENABLED FP=( p )->setDuplicatesEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEditable ( bool editable )
 */
HB_FUNC( QT_QCOMBOBOX_SETEDITABLE )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setEditable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETEDITABLE FP=( p )->setEditable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFrame ( bool )
 */
HB_FUNC( QT_QCOMBOBOX_SETFRAME )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setFrame( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETFRAME FP=( p )->setFrame( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QCOMBOBOX_SETICONSIZE )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETICONSIZE FP=( p )->setIconSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setInsertPolicy ( InsertPolicy policy )
 */
HB_FUNC( QT_QCOMBOBOX_SETINSERTPOLICY )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setInsertPolicy( ( QComboBox::InsertPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETINSERTPOLICY FP=( p )->setInsertPolicy( ( QComboBox::InsertPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItemData ( int index, const QVariant & value, int role = Qt::UserRole )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMDATA )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setItemData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ), hb_parnidef( 4, Qt::UserRole ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETITEMDATA FP=( p )->setItemData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ), hb_parnidef( 4, Qt::UserRole ) ); p is NULL" ) );
   }
}

/*
 * void setItemDelegate ( QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMDELEGATE )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETITEMDELEGATE FP=( p )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItemIcon ( int index, const QIcon & icon )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMICON )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setItemIcon( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QIcon( 3 ) : QIcon( hbqt_par_QString( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETITEMICON FP=( p )->setItemIcon( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QIcon( 3 ) : QIcon( hbqt_par_QString( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setItemText ( int index, const QString & text )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMTEXT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setItemText( hb_parni( 2 ), QComboBox::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETITEMTEXT FP=( p )->setItemText( hb_parni( 2 ), QComboBox::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setLineEdit ( QLineEdit * edit )
 */
HB_FUNC( QT_QCOMBOBOX_SETLINEEDIT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setLineEdit( hbqt_par_QLineEdit( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETLINEEDIT FP=( p )->setLineEdit( hbqt_par_QLineEdit( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaxCount ( int max )
 */
HB_FUNC( QT_QCOMBOBOX_SETMAXCOUNT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setMaxCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETMAXCOUNT FP=( p )->setMaxCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaxVisibleItems ( int maxItems )
 */
HB_FUNC( QT_QCOMBOBOX_SETMAXVISIBLEITEMS )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setMaxVisibleItems( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETMAXVISIBLEITEMS FP=( p )->setMaxVisibleItems( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumContentsLength ( int characters )
 */
HB_FUNC( QT_QCOMBOBOX_SETMINIMUMCONTENTSLENGTH )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setMinimumContentsLength( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETMINIMUMCONTENTSLENGTH FP=( p )->setMinimumContentsLength( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QCOMBOBOX_SETMODEL )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETMODEL FP=( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setModelColumn ( int visibleColumn )
 */
HB_FUNC( QT_QCOMBOBOX_SETMODELCOLUMN )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setModelColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETMODELCOLUMN FP=( p )->setModelColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRootModelIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QCOMBOBOX_SETROOTMODELINDEX )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setRootModelIndex( *hbqt_par_QModelIndex( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETROOTMODELINDEX FP=( p )->setRootModelIndex( *hbqt_par_QModelIndex( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeAdjustPolicy ( SizeAdjustPolicy policy )
 */
HB_FUNC( QT_QCOMBOBOX_SETSIZEADJUSTPOLICY )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setSizeAdjustPolicy( ( QComboBox::SizeAdjustPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETSIZEADJUSTPOLICY FP=( p )->setSizeAdjustPolicy( ( QComboBox::SizeAdjustPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setValidator ( const QValidator * validator )
 */
HB_FUNC( QT_QCOMBOBOX_SETVALIDATOR )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setValidator( hbqt_par_QValidator( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETVALIDATOR FP=( p )->setValidator( hbqt_par_QValidator( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setView ( QAbstractItemView * itemView )
 */
HB_FUNC( QT_QCOMBOBOX_SETVIEW )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setView( hbqt_par_QAbstractItemView( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETVIEW FP=( p )->setView( hbqt_par_QAbstractItemView( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void showPopup ()
 */
HB_FUNC( QT_QCOMBOBOX_SHOWPOPUP )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->showPopup();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SHOWPOPUP FP=( p )->showPopup(); p is NULL" ) );
   }
}

/*
 * SizeAdjustPolicy sizeAdjustPolicy () const
 */
HB_FUNC( QT_QCOMBOBOX_SIZEADJUSTPOLICY )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retni( ( QComboBox::SizeAdjustPolicy ) ( p )->sizeAdjustPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SIZEADJUSTPOLICY FP=hb_retni( ( QComboBox::SizeAdjustPolicy ) ( p )->sizeAdjustPolicy() ); p is NULL" ) );
   }
}

/*
 * virtual const QValidator * validator () const
 */
HB_FUNC( QT_QCOMBOBOX_VALIDATOR )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QValidator( ( void * ) ( p )->validator(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_VALIDATOR FP=hb_retptrGC( hbqt_gcAllocate_QValidator( ( void * ) ( p )->validator(), false ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemView * view () const
 */
HB_FUNC( QT_QCOMBOBOX_VIEW )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemView( ( p )->view(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_VIEW FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemView( ( p )->view(), false ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QCOMBOBOX_CLEAR )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * void clearEditText ()
 */
HB_FUNC( QT_QCOMBOBOX_CLEAREDITTEXT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->clearEditText();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_CLEAREDITTEXT FP=( p )->clearEditText(); p is NULL" ) );
   }
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QCOMBOBOX_SETCURRENTINDEX )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setCurrentIndex( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETCURRENTINDEX FP=( p )->setCurrentIndex( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEditText ( const QString & text )
 */
HB_FUNC( QT_QCOMBOBOX_SETEDITTEXT )
{
   QComboBox * p = hbqt_par_QComboBox( 1 );
   if( p )
      ( p )->setEditText( QComboBox::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMBOBOX_SETEDITTEXT FP=( p )->setEditText( QComboBox::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
