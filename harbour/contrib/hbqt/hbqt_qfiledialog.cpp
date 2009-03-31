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
 *  Constructed[ 36/47 [ 76.60% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  QDir directory () const
 *  QStringList history () const
 *  QStringList nameFilters () const
 *  bool restoreState ( const QByteArray & state )
 *  QByteArray saveState () const
 *  QStringList selectedFiles () const
 *  void setDirectory ( const QDir & directory )
 *  void setHistory ( const QStringList & paths )
 *  void setNameFilters ( const QStringList & filters )
 *  void setSidebarUrls ( const QList<QUrl> & urls )
 *  QList<QUrl> sidebarUrls () const
 */ 


#include <QtGui/QFileDialog>


/*
 * QFileDialog ( QWidget * parent, Qt::WindowFlags flags )
 * QFileDialog ( QWidget * parent = 0, const QString & caption = QString(), const QString & directory = QString(), const QString & filter = QString() )
 * ~QFileDialog ()
 */
HB_FUNC( QT_QFILEDIALOG )
{
   hb_retptr( ( QFileDialog* ) new QFileDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) );
}

/*
 * AcceptMode acceptMode () const
 */
HB_FUNC( QT_QFILEDIALOG_ACCEPTMODE )
{
   hb_retni( hbqt_par_QFileDialog( 1 )->acceptMode(  ) );
}

/*
 * bool confirmOverwrite () const
 */
HB_FUNC( QT_QFILEDIALOG_CONFIRMOVERWRITE )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->confirmOverwrite(  ) );
}

/*
 * QString defaultSuffix () const
 */
HB_FUNC( QT_QFILEDIALOG_DEFAULTSUFFIX )
{
   hb_retc( hbqt_par_QFileDialog( 1 )->defaultSuffix( ).toLatin1().data() );
}

/*
 * FileMode fileMode () const
 */
HB_FUNC( QT_QFILEDIALOG_FILEMODE )
{
   hb_retni( hbqt_par_QFileDialog( 1 )->fileMode(  ) );
}

/*
 * QDir::Filters filter () const
 */
HB_FUNC( QT_QFILEDIALOG_FILTER )
{
   hb_retni( hbqt_par_QFileDialog( 1 )->filter(  ) );
}

/*
 * QFileIconProvider * iconProvider () const
 */
HB_FUNC( QT_QFILEDIALOG_ICONPROVIDER )
{
   hb_retptr( ( QFileIconProvider* ) hbqt_par_QFileDialog( 1 )->iconProvider(  ) );
}

/*
 * bool isNameFilterDetailsVisible () const
 */
HB_FUNC( QT_QFILEDIALOG_ISNAMEFILTERDETAILSVISIBLE )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->isNameFilterDetailsVisible(  ) );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QFILEDIALOG_ISREADONLY )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->isReadOnly(  ) );
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QFILEDIALOG_ITEMDELEGATE )
{
   hb_retptr( ( QAbstractItemDelegate* ) hbqt_par_QFileDialog( 1 )->itemDelegate(  ) );
}

/*
 * QString labelText ( DialogLabel label ) const
 */
HB_FUNC( QT_QFILEDIALOG_LABELTEXT )
{
   hb_retc( hbqt_par_QFileDialog( 1 )->labelText( ( QFileDialog::DialogLabel ) hb_parni( 2 )).toLatin1().data() );
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QFILEDIALOG_OPEN )
{
   hbqt_par_QFileDialog( 1 )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
}

/*
 * Options options () const
 */
HB_FUNC( QT_QFILEDIALOG_OPTIONS )
{
   hb_retni( hbqt_par_QFileDialog( 1 )->options(  ) );
}

/*
 * QAbstractProxyModel * proxyModel () const
 */
HB_FUNC( QT_QFILEDIALOG_PROXYMODEL )
{
   hb_retptr( ( QAbstractProxyModel* ) hbqt_par_QFileDialog( 1 )->proxyModel(  ) );
}

/*
 * bool resolveSymlinks () const
 */
HB_FUNC( QT_QFILEDIALOG_RESOLVESYMLINKS )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->resolveSymlinks(  ) );
}

/*
 * void selectFile ( const QString & filename )
 */
HB_FUNC( QT_QFILEDIALOG_SELECTFILE )
{
   hbqt_par_QFileDialog( 1 )->selectFile( hbqt_par_QString( 2 ) );
}

/*
 * void selectNameFilter ( const QString & filter )
 */
HB_FUNC( QT_QFILEDIALOG_SELECTNAMEFILTER )
{
   hbqt_par_QFileDialog( 1 )->selectNameFilter( hbqt_par_QString( 2 ) );
}

/*
 * QString selectedNameFilter () const
 */
HB_FUNC( QT_QFILEDIALOG_SELECTEDNAMEFILTER )
{
   hb_retc( hbqt_par_QFileDialog( 1 )->selectedNameFilter( ).toLatin1().data() );
}

/*
 * void setAcceptMode ( AcceptMode mode )
 */
HB_FUNC( QT_QFILEDIALOG_SETACCEPTMODE )
{
   hbqt_par_QFileDialog( 1 )->setAcceptMode( ( QFileDialog::AcceptMode ) hb_parni( 2 ) );
}

/*
 * void setConfirmOverwrite ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETCONFIRMOVERWRITE )
{
   hbqt_par_QFileDialog( 1 )->setConfirmOverwrite( hb_parl( 2 ) );
}

/*
 * void setDefaultSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QFILEDIALOG_SETDEFAULTSUFFIX )
{
   hbqt_par_QFileDialog( 1 )->setDefaultSuffix( hbqt_par_QString( 2 ) );
}

/*
 * void setDirectory ( const QString & directory )
 */
HB_FUNC( QT_QFILEDIALOG_SETDIRECTORY )
{
   hbqt_par_QFileDialog( 1 )->setDirectory( hbqt_par_QString( 2 ) );
}

/*
 * void setFileMode ( FileMode mode )
 */
HB_FUNC( QT_QFILEDIALOG_SETFILEMODE )
{
   hbqt_par_QFileDialog( 1 )->setFileMode( ( QFileDialog::FileMode ) hb_parni( 2 ) );
}

/*
 * void setFilter ( QDir::Filters filters )
 */
HB_FUNC( QT_QFILEDIALOG_SETFILTER )
{
   hbqt_par_QFileDialog( 1 )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
}

/*
 * void setIconProvider ( QFileIconProvider * provider )
 */
HB_FUNC( QT_QFILEDIALOG_SETICONPROVIDER )
{
   hbqt_par_QFileDialog( 1 )->setIconProvider( hbqt_par_QFileIconProvider( 2 ) );
}

/*
 * void setItemDelegate ( QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QFILEDIALOG_SETITEMDELEGATE )
{
   hbqt_par_QFileDialog( 1 )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) );
}

/*
 * void setLabelText ( DialogLabel label, const QString & text )
 */
HB_FUNC( QT_QFILEDIALOG_SETLABELTEXT )
{
   hbqt_par_QFileDialog( 1 )->setLabelText( ( QFileDialog::DialogLabel ) hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setNameFilter ( const QString & filter )
 */
HB_FUNC( QT_QFILEDIALOG_SETNAMEFILTER )
{
   hbqt_par_QFileDialog( 1 )->setNameFilter( hbqt_par_QString( 2 ) );
}

/*
 * void setNameFilterDetailsVisible ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETNAMEFILTERDETAILSVISIBLE )
{
   hbqt_par_QFileDialog( 1 )->setNameFilterDetailsVisible( hb_parl( 2 ) );
}

/*
 * void setOption ( Option option, bool on = true )
 */
HB_FUNC( QT_QFILEDIALOG_SETOPTION )
{
   hbqt_par_QFileDialog( 1 )->setOption( ( QFileDialog::Option ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setOptions ( Options options )
 */
HB_FUNC( QT_QFILEDIALOG_SETOPTIONS )
{
   hbqt_par_QFileDialog( 1 )->setOptions( ( QFileDialog::Options ) hb_parni( 2 ) );
}

/*
 * void setProxyModel ( QAbstractProxyModel * proxyModel )
 */
HB_FUNC( QT_QFILEDIALOG_SETPROXYMODEL )
{
   hbqt_par_QFileDialog( 1 )->setProxyModel( hbqt_par_QAbstractProxyModel( 2 ) );
}

/*
 * void setReadOnly ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETREADONLY )
{
   hbqt_par_QFileDialog( 1 )->setReadOnly( hb_parl( 2 ) );
}

/*
 * void setResolveSymlinks ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETRESOLVESYMLINKS )
{
   hbqt_par_QFileDialog( 1 )->setResolveSymlinks( hb_parl( 2 ) );
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QFILEDIALOG_SETVIEWMODE )
{
   hbqt_par_QFileDialog( 1 )->setViewMode( ( QFileDialog::ViewMode ) hb_parni( 2 ) );
}

/*
 * bool testOption ( Option option ) const
 */
HB_FUNC( QT_QFILEDIALOG_TESTOPTION )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->testOption( ( QFileDialog::Option ) hb_parni( 2 ) ) );
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QFILEDIALOG_VIEWMODE )
{
   hb_retni( hbqt_par_QFileDialog( 1 )->viewMode(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

