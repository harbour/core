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

#if QT_VERSION >= 0x040500

#include <QtGui/QFormLayout>

/*----------------------------------------------------------------------*/
/*
QFormLayout ( QWidget * parent = 0 )
*/
HB_FUNC( QT_QFORMLAYOUT )
{
   QFormLayout *formlayout = NULL;
   QWidget* parent = ( QWidget * ) hb_parptr( 1 );
   formlayout = new QFormLayout( parent );
   hb_retptr( ( QFormLayout * ) formlayout);
}

/*
void addRow ( QWidget * label, QWidget * field )
*/
HB_FUNC( QT_QFORMLAYOUT_ADDROW_1 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QWidget * par2 = ( QWidget * ) hb_parptr( 2 );
   QWidget * par3 = ( QWidget * ) hb_parptr( 3 );
   par1->addRow( par2, par3 );
}

/*
void addRow ( QWidget * label, QLayout * field )
*/
HB_FUNC( QT_QFORMLAYOUT_ADDROW_2 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QWidget * par2 = ( QWidget * ) hb_parptr( 2 );
   QLayout * par3 = ( QLayout * ) hb_parptr( 3 );
   par1->addRow( par2, par3 );
}

/*
void addRow ( const QString & labelText, QWidget * field )
*/
HB_FUNC( QT_QFORMLAYOUT_ADDROW_3 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QString par2 = hb_parc( 2 );
   QWidget * par3 = ( QWidget * ) hb_parptr( 3 );
   par1->addRow( par2, par3 );
}

/*
void addRow ( const QString & labelText, QLayout * field )
*/
HB_FUNC( QT_QFORMLAYOUT_ADDROW_4 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QString par2 = hb_parc( 2 );
   QLayout * par3 = ( QLayout *) hb_parptr( 3 );
   par1->addRow( par2, par3 );
}

/*
void addRow ( QWidget * widget )
*/
HB_FUNC( QT_QFORMLAYOUT_ADDROW_5 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QWidget * par2 = ( QWidget * ) hb_parptr( 2 );
   par1->addRow( par2 );
}

/*
void addRow ( QLayout * layout )
*/
HB_FUNC( QT_QFORMLAYOUT_ADDROW_6 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QLayout * par2 = ( QLayout * ) hb_parptr( 2 );
   par1->addRow( par2 );
}

/*
FieldGrowthPolicy fieldGrowthPolicy () const
*/
HB_FUNC( QT_QFORMLAYOUT_FIELDGROWTHPOLICY )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->fieldGrowthPolicy();
   hb_retni( i );
}

/*
Qt::Alignment formAlignment () const
*/
HB_FUNC( QT_QFORMLAYOUT_FORMALIGNMENT )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->formAlignment();
   hb_retni( i );
}

/*
int horizontalSpacing () const
*/
HB_FUNC( QT_QFORMLAYOUT_HORIZONTALSPACING )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->horizontalSpacing();
   hb_retni( i );
}

/*
void insertRow ( int row, QWidget * label, QWidget * field )
*/
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_1 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   QWidget * par3 = ( QWidget * ) hb_parptr( 3 );
   QWidget * par4 = ( QWidget * ) hb_parptr( 4 );
   par1->insertRow( par2, par3, par4 );
}

/*
void insertRow ( int row, QWidget * label, QLayout * field )
*/
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_2 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   QWidget * par3 = ( QWidget * ) hb_parptr( 3 );
   QLayout * par4 = ( QLayout * ) hb_parptr( 4 );
   par1->insertRow( par2, par3, par4 );
}

/*
void insertRow ( int row, const QString & labelText, QWidget * field )
*/
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_3 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   QString par3 = hb_parc( 3 );
   QWidget * par4 = ( QWidget * ) hb_parptr( 4 );
   par1->insertRow( par2, par3, par4 );
}

/*
void insertRow ( int row, const QString & labelText, QLayout * field )
*/
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_4 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   QString par3 = hb_parc( 3 );
   QLayout * par4 = ( QLayout * ) hb_parptr( 4 );
   par1->insertRow( par2, par3, par4 );
}

/*
void insertRow ( int row, QWidget * widget )
*/
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_5 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   QWidget * par3 = ( QWidget * ) hb_parptr( 3 );
   par1->insertRow( par2, par3 );
}

/*
void insertRow ( int row, QLayout * layout )
*/
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_6 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   QLayout * par3 = ( QLayout * ) hb_parptr( 3 );
   par1->insertRow( par2, par3 );
}

/*
QLayoutItem * itemAt ( int row, ItemRole role ) const
*/
HB_FUNC( QT_QFORMLAYOUT_ITEMAT )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   int par3 = hb_parni( 3 );
   QLayoutItem * ptr = par1->itemAt( par2, ( QFormLayout::ItemRole ) par3 );
   hb_retptr( ( QLayoutItem * ) ptr );
}

/*
Qt::Alignment labelAlignment () const
*/
HB_FUNC( QT_QFORMLAYOUT_LABELALIGNMENT )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->labelAlignment();
   hb_retni( i );
}

/*
QWidget * labelForField ( QWidget * field ) const
*/
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD_1 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QWidget * par2 = ( QWidget * ) hb_parptr( 2 );
   QWidget * ptr = par1->labelForField( par2 );
   hb_retptr( ( QWidget * ) ptr );
}

/*
QWidget * labelForField ( QLayout * field ) const
*/
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD_2 )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   QLayout * par2 = ( QLayout * ) hb_parptr( 2 );
   QWidget * ptr = par1->labelForField( par2 );
   hb_retptr( ( QWidget * ) ptr );
}

/*
int rowCount () const
*/
HB_FUNC( QT_QFORMLAYOUT_ROWCOUNT )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->rowCount();
   hb_retni( i );
}

/*
RowWrapPolicy rowWrapPolicy () const
*/
HB_FUNC( QT_QFORMLAYOUT_ROWWRAPPOLICY )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->rowWrapPolicy();
   hb_retni( i );
}

/*
void setFieldGrowthPolicy ( FieldGrowthPolicy policy )
*/
HB_FUNC( QT_QFORMLAYOUT_SETFIELDGROWTHPOLICY )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setFieldGrowthPolicy( ( QFormLayout::FieldGrowthPolicy ) par2 );
}

/*
void setFormAlignment ( Qt::Alignment alignment )
*/
HB_FUNC( QT_QFORMLAYOUT_SETFORMALIGNMENT )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setFormAlignment( ( Qt::Alignment ) par2 );
}

/*
void setHorizontalSpacing ( int spacing )
*/
HB_FUNC( QT_QFORMLAYOUT_SETHORIZONTALSPACING )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setHorizontalSpacing( par2 );
}

/*
void setItem ( int row, ItemRole role, QLayoutItem * item )
*/
HB_FUNC( QT_QFORMLAYOUT_SETITEM )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   int par3 = hb_parni( 3 );
   QLayout * par4 = ( QLayout * ) hb_parptr( 4 );
   par1->setItem( par2, ( QFormLayout::ItemRole ) par3, par4 );
}

/*
void setLabelAlignment ( Qt::Alignment alignment )
*/
HB_FUNC( QT_QFORMLAYOUT_SETLABELALIGNMENT )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setLabelAlignment( ( Qt::Alignment ) par2 );
}

/*
void setLayout ( int row, ItemRole role, QLayout * layout )
*/
HB_FUNC( QT_QFORMLAYOUT_SETLAYOUT )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   int par3 = hb_parni( 3 );
   QLayout * par4 = ( QLayout * ) hb_parptr( 4 );
   par1->setLayout( par2, ( QFormLayout::ItemRole ) par3, par4 );
}

/*
void setRowWrapPolicy ( RowWrapPolicy policy )
*/
HB_FUNC( QT_QFORMLAYOUT_SETROWWRAPPOLICY )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setRowWrapPolicy( ( QFormLayout::RowWrapPolicy ) par2 );
}

/*
void setSpacing ( int spacing )
*/
HB_FUNC( QT_QFORMLAYOUT_SETSPACING )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setSpacing( par2 );
}

/*
void setVerticalSpacing ( int spacing )
*/
HB_FUNC( QT_QFORMLAYOUT_SETVERTICALSPACING )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   par1->setVerticalSpacing( par2 );
}

/*
void setWidget ( int row, ItemRole role, QWidget * widget )
*/
HB_FUNC( QT_QFORMLAYOUT_SETWIDGET )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int par2 = hb_parni( 2 );
   int par3 = hb_parni( 3 );
   QWidget * par4 = ( QWidget * ) hb_parptr( 4 );
   par1->setWidget( par2,  (QFormLayout::ItemRole) par3, par4 );
}

/*
int spacing () const
*/
HB_FUNC( QT_QFORMLAYOUT_SPACING )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->spacing();
   hb_retni( i );
}

/*
int verticalSpacing () const
*/
HB_FUNC( QT_QFORMLAYOUT_VERTICALSPACING )
{
   QFormLayout * par1 = ( QFormLayout * ) hb_parptr( 1 );
   int i = par1->verticalSpacing();
   hb_retni( i );
}

/*----------------------------------------------------------------------*/
#endif
