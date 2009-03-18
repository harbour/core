/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 {list of individual authors and e-mail addresses}
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

#include "hbapi.h"

#include "hbqt.h"

/*
QDialog ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
*/
HB_FUNC( QT_QDIALOG )
{
   hb_retptr( new QDialog( hbqt_par_QWidget( 1 ), hbqt_par_WindowFlags( 2 ) ) );
}

/*
bool isModal () const
void setModal ( bool modal )
*/
HB_FUNC( QT_QDIALOG_ISMODAL )
{
   hb_retl( hbqt_par_QDialog( 1 )->isModal() );
}

HB_FUNC( QT_QDIALOG_SETMODAL )
{
   hbqt_par_QDialog( 1 )->setModal( hb_parl( 2 ) );
}

/*
bool isSizeGripEnabled () const
void setSizeGripEnabled ( bool )
*/
HB_FUNC( QT_QDIALOG_ISSIZEGRIPENABLED )
{
   hb_retl( hbqt_par_QDialog( 1 )->isSizeGripEnabled() );
}

HB_FUNC( QT_QDIALOG_SETSIZEGRIPENABLED )
{
   hbqt_par_QDialog( 1 )->setSizeGripEnabled( hb_parl( 2 ) );
}

/*
void QDialog::accept ()   [virtual slot]
*/
HB_FUNC( QT_QDIALOG_ACCEPT )
{
   hbqt_par_QDialog( 1 )->accept();
}

/*
void QDialog::done ( int r )   [virtual slot]
*/
HB_FUNC( QT_QDIALOG_DONE )
{
   hbqt_par_QDialog( 1 )->done( hb_parni( 2 ) );
}

/*
int QDialog::exec ()   [slot]
*/
HB_FUNC( QT_QDIALOG_EXEC )
{
   hb_retni( hbqt_par_QDialog( 1 )->exec() );
}

/*
void QDialog::open ()   [slot]
*/
HB_FUNC( QT_QDIALOG_OPEN )
{
   hbqt_par_QDialog( 1 )->open();
}

/*
void QDialog::reject ()   [virtual slot]
*/
HB_FUNC( QT_QDIALOG_REJECT )
{
   hbqt_par_QDialog( 1 )->reject();
}

/*
int QDialog::result () const
void QDialog::setResult ( int i )
*/
HB_FUNC( QT_QDIALOG_RESULT )
{
   hb_retni( hbqt_par_QDialog( 1 )->result() );
}

HB_FUNC( QT_QDIALOG_SETRESULT )
{
   hbqt_par_QDialog( 1 )->setResult( hb_parni( 2 ) );
}
