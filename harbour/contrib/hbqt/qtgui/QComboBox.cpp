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

#include "hbapi.h"
#include "../hbqt.h"

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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QComboBox > pq;
} QGC_POINTER_QComboBox;

QT_G_FUNC( hbqt_gcRelease_QComboBox )
{
   QGC_POINTER_QComboBox * p = ( QGC_POINTER_QComboBox * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QComboBox   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QComboBox * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QComboBox   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QComboBoxph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QComboBox    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QComboBox    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QComboBox( void * pObj, bool bNew )
{
   QGC_POINTER_QComboBox * p = ( QGC_POINTER_QComboBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QComboBox ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QComboBox;

   if( bNew )
   {
      new( & p->pq ) QPointer< QComboBox >( ( QComboBox * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QComboBox                  ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QCOMBOBOX )
{
   void * pObj = NULL;

   pObj = ( QComboBox* ) new QComboBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QComboBox( pObj, true ) );
}

/*
 * void addItem ( const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_ADDITEM )
{
   hbqt_par_QComboBox( 1 )->addItem( QComboBox::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) );
}

/*
 * void addItem ( const QIcon & icon, const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_ADDITEM_1 )
{
   hbqt_par_QComboBox( 1 )->addItem( QIcon( hbqt_par_QString( 2 ) ), QComboBox::tr( hb_parc( 3 ) ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QVariant( 4 ) : QVariant() ) );
}

/*
 * void addItems ( const QStringList & texts )
 */
HB_FUNC( QT_QCOMBOBOX_ADDITEMS )
{
   hbqt_par_QComboBox( 1 )->addItems( *hbqt_par_QStringList( 2 ) );
}

/*
 * QCompleter * completer () const
 */
HB_FUNC( QT_QCOMBOBOX_COMPLETER )
{
   hb_retptrGC( hbqt_gcAllocate_QCompleter( hbqt_par_QComboBox( 1 )->completer(), false ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QCOMBOBOX_COUNT )
{
   hb_retni( hbqt_par_QComboBox( 1 )->count() );
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QCOMBOBOX_CURRENTINDEX )
{
   hb_retni( hbqt_par_QComboBox( 1 )->currentIndex() );
}

/*
 * QString currentText () const
 */
HB_FUNC( QT_QCOMBOBOX_CURRENTTEXT )
{
   hb_retc( hbqt_par_QComboBox( 1 )->currentText().toAscii().data() );
}

/*
 * bool duplicatesEnabled () const
 */
HB_FUNC( QT_QCOMBOBOX_DUPLICATESENABLED )
{
   hb_retl( hbqt_par_QComboBox( 1 )->duplicatesEnabled() );
}

/*
 * int findData ( const QVariant & data, int role = Qt::UserRole, Qt::MatchFlags flags = Qt::MatchExactly | Qt::MatchCaseSensitive ) const
 */
HB_FUNC( QT_QCOMBOBOX_FINDDATA )
{
   hb_retni( hbqt_par_QComboBox( 1 )->findData( *hbqt_par_QVariant( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : Qt::UserRole ), ( HB_ISNUM( 4 ) ? ( Qt::MatchFlags ) hb_parni( 4 ) : ( Qt::MatchFlags ) Qt::MatchExactly | Qt::MatchCaseSensitive ) ) );
}

/*
 * int findText ( const QString & text, Qt::MatchFlags flags = Qt::MatchExactly | Qt::MatchCaseSensitive ) const
 */
HB_FUNC( QT_QCOMBOBOX_FINDTEXT )
{
   hb_retni( hbqt_par_QComboBox( 1 )->findText( QComboBox::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( Qt::MatchFlags ) hb_parni( 3 ) : ( Qt::MatchFlags ) Qt::MatchExactly | Qt::MatchCaseSensitive ) ) );
}

/*
 * bool hasFrame () const
 */
HB_FUNC( QT_QCOMBOBOX_HASFRAME )
{
   hb_retl( hbqt_par_QComboBox( 1 )->hasFrame() );
}

/*
 * virtual void hidePopup ()
 */
HB_FUNC( QT_QCOMBOBOX_HIDEPOPUP )
{
   hbqt_par_QComboBox( 1 )->hidePopup();
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QCOMBOBOX_ICONSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QComboBox( 1 )->iconSize() ), true ) );
}

/*
 * void insertItem ( int index, const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTITEM )
{
   hbqt_par_QComboBox( 1 )->insertItem( hb_parni( 2 ), QComboBox::tr( hb_parc( 3 ) ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QVariant( 4 ) : QVariant() ) );
}

/*
 * void insertItem ( int index, const QIcon & icon, const QString & text, const QVariant & userData = QVariant() )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTITEM_1 )
{
   hbqt_par_QComboBox( 1 )->insertItem( hb_parni( 2 ), QIcon( hbqt_par_QString( 3 ) ), QComboBox::tr( hb_parc( 4 ) ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QVariant( 5 ) : QVariant() ) );
}

/*
 * void insertItems ( int index, const QStringList & list )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTITEMS )
{
   hbqt_par_QComboBox( 1 )->insertItems( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) );
}

/*
 * InsertPolicy insertPolicy () const
 */
HB_FUNC( QT_QCOMBOBOX_INSERTPOLICY )
{
   hb_retni( ( QComboBox::InsertPolicy ) hbqt_par_QComboBox( 1 )->insertPolicy() );
}

/*
 * void insertSeparator ( int index )
 */
HB_FUNC( QT_QCOMBOBOX_INSERTSEPARATOR )
{
   hbqt_par_QComboBox( 1 )->insertSeparator( hb_parni( 2 ) );
}

/*
 * bool isEditable () const
 */
HB_FUNC( QT_QCOMBOBOX_ISEDITABLE )
{
   hb_retl( hbqt_par_QComboBox( 1 )->isEditable() );
}

/*
 * QVariant itemData ( int index, int role = Qt::UserRole ) const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QComboBox( 1 )->itemData( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : Qt::UserRole ) ) ), true ) );
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMDELEGATE )
{
   hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( hbqt_par_QComboBox( 1 )->itemDelegate(), false ) );
}

/*
 * QIcon itemIcon ( int index ) const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QComboBox( 1 )->itemIcon( hb_parni( 2 ) ) ), true ) );
}

/*
 * QString itemText ( int index ) const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMTEXT )
{
   hb_retc( hbqt_par_QComboBox( 1 )->itemText( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QLineEdit * lineEdit () const
 */
HB_FUNC( QT_QCOMBOBOX_LINEEDIT )
{
   hb_retptrGC( hbqt_gcAllocate_QLineEdit( hbqt_par_QComboBox( 1 )->lineEdit(), false ) );
}

/*
 * int maxCount () const
 */
HB_FUNC( QT_QCOMBOBOX_MAXCOUNT )
{
   hb_retni( hbqt_par_QComboBox( 1 )->maxCount() );
}

/*
 * int maxVisibleItems () const
 */
HB_FUNC( QT_QCOMBOBOX_MAXVISIBLEITEMS )
{
   hb_retni( hbqt_par_QComboBox( 1 )->maxVisibleItems() );
}

/*
 * int minimumContentsLength () const
 */
HB_FUNC( QT_QCOMBOBOX_MINIMUMCONTENTSLENGTH )
{
   hb_retni( hbqt_par_QComboBox( 1 )->minimumContentsLength() );
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QCOMBOBOX_MODEL )
{
   hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( hbqt_par_QComboBox( 1 )->model(), false ) );
}

/*
 * int modelColumn () const
 */
HB_FUNC( QT_QCOMBOBOX_MODELCOLUMN )
{
   hb_retni( hbqt_par_QComboBox( 1 )->modelColumn() );
}

/*
 * void removeItem ( int index )
 */
HB_FUNC( QT_QCOMBOBOX_REMOVEITEM )
{
   hbqt_par_QComboBox( 1 )->removeItem( hb_parni( 2 ) );
}

/*
 * QModelIndex rootModelIndex () const
 */
HB_FUNC( QT_QCOMBOBOX_ROOTMODELINDEX )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QComboBox( 1 )->rootModelIndex() ), true ) );
}

/*
 * void setCompleter ( QCompleter * completer )
 */
HB_FUNC( QT_QCOMBOBOX_SETCOMPLETER )
{
   hbqt_par_QComboBox( 1 )->setCompleter( hbqt_par_QCompleter( 2 ) );
}

/*
 * void setDuplicatesEnabled ( bool enable )
 */
HB_FUNC( QT_QCOMBOBOX_SETDUPLICATESENABLED )
{
   hbqt_par_QComboBox( 1 )->setDuplicatesEnabled( hb_parl( 2 ) );
}

/*
 * void setEditable ( bool editable )
 */
HB_FUNC( QT_QCOMBOBOX_SETEDITABLE )
{
   hbqt_par_QComboBox( 1 )->setEditable( hb_parl( 2 ) );
}

/*
 * void setFrame ( bool )
 */
HB_FUNC( QT_QCOMBOBOX_SETFRAME )
{
   hbqt_par_QComboBox( 1 )->setFrame( hb_parl( 2 ) );
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QCOMBOBOX_SETICONSIZE )
{
   hbqt_par_QComboBox( 1 )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setInsertPolicy ( InsertPolicy policy )
 */
HB_FUNC( QT_QCOMBOBOX_SETINSERTPOLICY )
{
   hbqt_par_QComboBox( 1 )->setInsertPolicy( ( QComboBox::InsertPolicy ) hb_parni( 2 ) );
}

/*
 * void setItemData ( int index, const QVariant & value, int role = Qt::UserRole )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMDATA )
{
   hbqt_par_QComboBox( 1 )->setItemData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : Qt::UserRole ) );
}

/*
 * void setItemDelegate ( QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMDELEGATE )
{
   hbqt_par_QComboBox( 1 )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) );
}

/*
 * void setItemIcon ( int index, const QIcon & icon )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMICON )
{
   hbqt_par_QComboBox( 1 )->setItemIcon( hb_parni( 2 ), QIcon( hbqt_par_QString( 3 ) ) );
}

/*
 * void setItemText ( int index, const QString & text )
 */
HB_FUNC( QT_QCOMBOBOX_SETITEMTEXT )
{
   hbqt_par_QComboBox( 1 )->setItemText( hb_parni( 2 ), QComboBox::tr( hb_parc( 3 ) ) );
}

/*
 * void setLineEdit ( QLineEdit * edit )
 */
HB_FUNC( QT_QCOMBOBOX_SETLINEEDIT )
{
   hbqt_par_QComboBox( 1 )->setLineEdit( hbqt_par_QLineEdit( 2 ) );
}

/*
 * void setMaxCount ( int max )
 */
HB_FUNC( QT_QCOMBOBOX_SETMAXCOUNT )
{
   hbqt_par_QComboBox( 1 )->setMaxCount( hb_parni( 2 ) );
}

/*
 * void setMaxVisibleItems ( int maxItems )
 */
HB_FUNC( QT_QCOMBOBOX_SETMAXVISIBLEITEMS )
{
   hbqt_par_QComboBox( 1 )->setMaxVisibleItems( hb_parni( 2 ) );
}

/*
 * void setMinimumContentsLength ( int characters )
 */
HB_FUNC( QT_QCOMBOBOX_SETMINIMUMCONTENTSLENGTH )
{
   hbqt_par_QComboBox( 1 )->setMinimumContentsLength( hb_parni( 2 ) );
}

/*
 * void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QCOMBOBOX_SETMODEL )
{
   hbqt_par_QComboBox( 1 )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
}

/*
 * void setModelColumn ( int visibleColumn )
 */
HB_FUNC( QT_QCOMBOBOX_SETMODELCOLUMN )
{
   hbqt_par_QComboBox( 1 )->setModelColumn( hb_parni( 2 ) );
}

/*
 * void setRootModelIndex ( const QModelIndex & index )
 */
HB_FUNC( QT_QCOMBOBOX_SETROOTMODELINDEX )
{
   hbqt_par_QComboBox( 1 )->setRootModelIndex( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * void setSizeAdjustPolicy ( SizeAdjustPolicy policy )
 */
HB_FUNC( QT_QCOMBOBOX_SETSIZEADJUSTPOLICY )
{
   hbqt_par_QComboBox( 1 )->setSizeAdjustPolicy( ( QComboBox::SizeAdjustPolicy ) hb_parni( 2 ) );
}

/*
 * void setValidator ( const QValidator * validator )
 */
HB_FUNC( QT_QCOMBOBOX_SETVALIDATOR )
{
   hbqt_par_QComboBox( 1 )->setValidator( hbqt_par_QValidator( 2 ) );
}

/*
 * void setView ( QAbstractItemView * itemView )
 */
HB_FUNC( QT_QCOMBOBOX_SETVIEW )
{
   hbqt_par_QComboBox( 1 )->setView( hbqt_par_QAbstractItemView( 2 ) );
}

/*
 * virtual void showPopup ()
 */
HB_FUNC( QT_QCOMBOBOX_SHOWPOPUP )
{
   hbqt_par_QComboBox( 1 )->showPopup();
}

/*
 * SizeAdjustPolicy sizeAdjustPolicy () const
 */
HB_FUNC( QT_QCOMBOBOX_SIZEADJUSTPOLICY )
{
   hb_retni( ( QComboBox::SizeAdjustPolicy ) hbqt_par_QComboBox( 1 )->sizeAdjustPolicy() );
}

/*
 * virtual const QValidator * validator () const
 */
HB_FUNC( QT_QCOMBOBOX_VALIDATOR )
{
   hb_retptr( ( QValidator* ) hbqt_par_QComboBox( 1 )->validator() );
}

/*
 * QAbstractItemView * view () const
 */
HB_FUNC( QT_QCOMBOBOX_VIEW )
{
   hb_retptrGC( hbqt_gcAllocate_QAbstractItemView( hbqt_par_QComboBox( 1 )->view(), false ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QCOMBOBOX_CLEAR )
{
   hbqt_par_QComboBox( 1 )->clear();
}

/*
 * void clearEditText ()
 */
HB_FUNC( QT_QCOMBOBOX_CLEAREDITTEXT )
{
   hbqt_par_QComboBox( 1 )->clearEditText();
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QCOMBOBOX_SETCURRENTINDEX )
{
   hbqt_par_QComboBox( 1 )->setCurrentIndex( hb_parni( 2 ) );
}

/*
 * void setEditText ( const QString & text )
 */
HB_FUNC( QT_QCOMBOBOX_SETEDITTEXT )
{
   hbqt_par_QComboBox( 1 )->setEditText( QComboBox::tr( hb_parc( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
