/*
 * $Id$
 */
   
/* 
 * Harbour Project source code:
 * QT wrapper main header
 * 
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


/*
 *  Constructed[ 42/53 [ 79.25% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  void addItem ( const QString & text, const QVariant & userData = QVariant() )
 *  void addItem ( const QIcon & icon, const QString & text, const QVariant & userData = QVariant() )
 *  void addItems ( const QStringList & texts )
 *  void insertItem ( int index, const QString & text, const QVariant & userData = QVariant() )
 *  void insertItem ( int index, const QIcon & icon, const QString & text, const QVariant & userData = QVariant() )
 *  void insertItems ( int index, const QStringList & list )
 *  QVariant itemData ( int index, int role = Qt::UserRole ) const
 *  QIcon itemIcon ( int index ) const
 *  QModelIndex rootModelIndex () const
 *  void setItemData ( int index, const QVariant & value, int role = Qt::UserRole )
 *  void setRootModelIndex ( const QModelIndex & index )
 */ 


#include <QtGui/QComboBox>


/*
 * QComboBox ( QWidget * parent = 0 )
 * ~QComboBox ()
 */
HB_FUNC( QT_QCOMBOBOX )
{
   hb_retptr( ( QComboBox* ) new QComboBox( hbqt_par_QWidget( 1 ) ) );
}

/*
 * QCompleter * completer () const
 */
HB_FUNC( QT_QCOMBOBOX_COMPLETER )
{
   hb_retptr( ( QCompleter* ) hbqt_par_QComboBox( 1 )->completer(  ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QCOMBOBOX_COUNT )
{
   hb_retni( hbqt_par_QComboBox( 1 )->count(  ) );
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QCOMBOBOX_CURRENTINDEX )
{
   hb_retni( hbqt_par_QComboBox( 1 )->currentIndex(  ) );
}

/*
 * QString currentText () const
 */
HB_FUNC( QT_QCOMBOBOX_CURRENTTEXT )
{
   hb_retc( hbqt_par_QComboBox( 1 )->currentText( ).toLatin1().data() );
}

/*
 * bool duplicatesEnabled () const
 */
HB_FUNC( QT_QCOMBOBOX_DUPLICATESENABLED )
{
   hb_retl( hbqt_par_QComboBox( 1 )->duplicatesEnabled(  ) );
}

/*
 * bool hasFrame () const
 */
HB_FUNC( QT_QCOMBOBOX_HASFRAME )
{
   hb_retl( hbqt_par_QComboBox( 1 )->hasFrame(  ) );
}

/*
 * virtual void hidePopup ()
 */
HB_FUNC( QT_QCOMBOBOX_HIDEPOPUP )
{
   hbqt_par_QComboBox( 1 )->hidePopup(  );
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QCOMBOBOX_ICONSIZE )
{
   hbqt_ret_QSize( hbqt_par_QComboBox( 1 )->iconSize(  ) );
}

/*
 * InsertPolicy insertPolicy () const
 */
HB_FUNC( QT_QCOMBOBOX_INSERTPOLICY )
{
   hb_retni( hbqt_par_QComboBox( 1 )->insertPolicy(  ) );
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
   hb_retl( hbqt_par_QComboBox( 1 )->isEditable(  ) );
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMDELEGATE )
{
   hb_retptr( ( QAbstractItemDelegate* ) hbqt_par_QComboBox( 1 )->itemDelegate(  ) );
}

/*
 * QString itemText ( int index ) const
 */
HB_FUNC( QT_QCOMBOBOX_ITEMTEXT )
{
   hb_retc( hbqt_par_QComboBox( 1 )->itemText( hb_parni( 2 )).toLatin1().data() );
}

/*
 * QLineEdit * lineEdit () const
 */
HB_FUNC( QT_QCOMBOBOX_LINEEDIT )
{
   hb_retptr( ( QLineEdit* ) hbqt_par_QComboBox( 1 )->lineEdit(  ) );
}

/*
 * int maxCount () const
 */
HB_FUNC( QT_QCOMBOBOX_MAXCOUNT )
{
   hb_retni( hbqt_par_QComboBox( 1 )->maxCount(  ) );
}

/*
 * int maxVisibleItems () const
 */
HB_FUNC( QT_QCOMBOBOX_MAXVISIBLEITEMS )
{
   hb_retni( hbqt_par_QComboBox( 1 )->maxVisibleItems(  ) );
}

/*
 * int minimumContentsLength () const
 */
HB_FUNC( QT_QCOMBOBOX_MINIMUMCONTENTSLENGTH )
{
   hb_retni( hbqt_par_QComboBox( 1 )->minimumContentsLength(  ) );
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QCOMBOBOX_MODEL )
{
   hb_retptr( ( QAbstractItemModel* ) hbqt_par_QComboBox( 1 )->model(  ) );
}

/*
 * int modelColumn () const
 */
HB_FUNC( QT_QCOMBOBOX_MODELCOLUMN )
{
   hb_retni( hbqt_par_QComboBox( 1 )->modelColumn(  ) );
}

/*
 * void removeItem ( int index )
 */
HB_FUNC( QT_QCOMBOBOX_REMOVEITEM )
{
   hbqt_par_QComboBox( 1 )->removeItem( hb_parni( 2 ) );
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
   hbqt_par_QComboBox( 1 )->setIconSize( hbqt_const_QSize( 2 ) );
}

/*
 * void setInsertPolicy ( InsertPolicy policy )
 */
HB_FUNC( QT_QCOMBOBOX_SETINSERTPOLICY )
{
   hbqt_par_QComboBox( 1 )->setInsertPolicy( ( QComboBox::InsertPolicy ) hb_parni( 2 ) );
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
   hbqt_par_QComboBox( 1 )->setItemText( hb_parni( 2 ), hbqt_par_QString( 3 ) );
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
   hbqt_par_QComboBox( 1 )->showPopup(  );
}

/*
 * SizeAdjustPolicy sizeAdjustPolicy () const
 */
HB_FUNC( QT_QCOMBOBOX_SIZEADJUSTPOLICY )
{
   hb_retni( hbqt_par_QComboBox( 1 )->sizeAdjustPolicy(  ) );
}

/*
 * const QValidator * validator () const
 */
HB_FUNC( QT_QCOMBOBOX_VALIDATOR )
{
   hb_retptr( ( QValidator* ) hbqt_par_QComboBox( 1 )->validator(  ) );
}

/*
 * QAbstractItemView * view () const
 */
HB_FUNC( QT_QCOMBOBOX_VIEW )
{
   hb_retptr( ( QAbstractItemView* ) hbqt_par_QComboBox( 1 )->view(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

