/*
 * $Id$
 */

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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum AcceptMode { AcceptOpen, AcceptSave }
 *  enum DialogLabel { LookIn, FileName, FileType, Accept, Reject }
 *  enum FileMode { AnyFile, ExistingFile, Directory, ExistingFiles, DirectoryOnly }
 *  enum Option { ShowDirsOnly, DontResolveSymlinks, DontConfirmOverwrite, DontUseNativeDialog, ..., DontUseSheet }
 *  flags Options
 *  enum ViewMode { Detail, List }
 */

/*
 *  Constructed[ 45/51 [ 88.24% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setSidebarUrls ( const QList<QUrl> & urls )
 *  QList<QUrl> sidebarUrls () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // void open ( QObject * receiver, const char * member )
 *  //QString getOpenFileName ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), const QString & filter = QString(), QString * selectedFilter = 0, Options options = 0 )
 *  //QStringList getOpenFileNames ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), const QString & filter = QString(), QString * selectedFilter = 0, Options options = 0 )
 *  //QString getSaveFileName ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), const QString & filter = QString(), QString * selectedFilter = 0, Options options = 0 )
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
 * DESTRUCTOR
 */
HB_FUNC( QT_QFILEDIALOG_DESTROY )
{
   hbqt_par_QFileDialog( 1 )->~QFileDialog();
}

/*
 * AcceptMode acceptMode () const
 */
HB_FUNC( QT_QFILEDIALOG_ACCEPTMODE )
{
   hb_retni( ( QFileDialog::AcceptMode ) hbqt_par_QFileDialog( 1 )->acceptMode() );
}

/*
 * bool confirmOverwrite () const
 */
HB_FUNC( QT_QFILEDIALOG_CONFIRMOVERWRITE )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->confirmOverwrite() );
}

/*
 * QString defaultSuffix () const
 */
HB_FUNC( QT_QFILEDIALOG_DEFAULTSUFFIX )
{
   hb_retc( hbqt_par_QFileDialog( 1 )->defaultSuffix().toLatin1().data() );
}

/*
 * QDir directory () const
 */
HB_FUNC( QT_QFILEDIALOG_DIRECTORY )
{
   hb_retptr( new QDir( hbqt_par_QFileDialog( 1 )->directory() ) );
}

/*
 * FileMode fileMode () const
 */
HB_FUNC( QT_QFILEDIALOG_FILEMODE )
{
   hb_retni( ( QFileDialog::FileMode ) hbqt_par_QFileDialog( 1 )->fileMode() );
}

/*
 * QDir::Filters filter () const
 */
HB_FUNC( QT_QFILEDIALOG_FILTER )
{
   hb_retni( ( QDir::Filters ) hbqt_par_QFileDialog( 1 )->filter() );
}

/*
 * QStringList history () const
 */
HB_FUNC( QT_QFILEDIALOG_HISTORY )
{
   hb_retptr( new QStringList( hbqt_par_QFileDialog( 1 )->history() ) );
}

/*
 * QFileIconProvider * iconProvider () const
 */
HB_FUNC( QT_QFILEDIALOG_ICONPROVIDER )
{
   hb_retptr( ( QFileIconProvider* ) hbqt_par_QFileDialog( 1 )->iconProvider() );
}

/*
 * bool isNameFilterDetailsVisible () const
 */
HB_FUNC( QT_QFILEDIALOG_ISNAMEFILTERDETAILSVISIBLE )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->isNameFilterDetailsVisible() );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QFILEDIALOG_ISREADONLY )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->isReadOnly() );
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QFILEDIALOG_ITEMDELEGATE )
{
   hb_retptr( ( QAbstractItemDelegate* ) hbqt_par_QFileDialog( 1 )->itemDelegate() );
}

/*
 * QString labelText ( DialogLabel label ) const
 */
HB_FUNC( QT_QFILEDIALOG_LABELTEXT )
{
   hb_retc( hbqt_par_QFileDialog( 1 )->labelText( ( QFileDialog::DialogLabel ) hb_parni( 2 ) ).toLatin1().data() );
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QFILEDIALOG_NAMEFILTERS )
{
   hb_retptr( new QStringList( hbqt_par_QFileDialog( 1 )->nameFilters() ) );
}

/*
 * Options options () const
 */
HB_FUNC( QT_QFILEDIALOG_OPTIONS )
{
   hb_retni( ( QFileDialog::Options ) hbqt_par_QFileDialog( 1 )->options() );
}

/*
 * QAbstractProxyModel * proxyModel () const
 */
HB_FUNC( QT_QFILEDIALOG_PROXYMODEL )
{
   hb_retptr( ( QAbstractProxyModel* ) hbqt_par_QFileDialog( 1 )->proxyModel() );
}

/*
 * bool resolveSymlinks () const
 */
HB_FUNC( QT_QFILEDIALOG_RESOLVESYMLINKS )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->resolveSymlinks() );
}

/*
 * bool restoreState ( const QByteArray & state )
 */
HB_FUNC( QT_QFILEDIALOG_RESTORESTATE )
{
   hb_retl( hbqt_par_QFileDialog( 1 )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * QByteArray saveState () const
 */
HB_FUNC( QT_QFILEDIALOG_SAVESTATE )
{
   hb_retptr( new QByteArray( hbqt_par_QFileDialog( 1 )->saveState() ) );
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
 * QStringList selectedFiles () const
 */
HB_FUNC( QT_QFILEDIALOG_SELECTEDFILES )
{
   hb_retptr( new QStringList( hbqt_par_QFileDialog( 1 )->selectedFiles() ) );
}

/*
 * QString selectedNameFilter () const
 */
HB_FUNC( QT_QFILEDIALOG_SELECTEDNAMEFILTER )
{
   hb_retc( hbqt_par_QFileDialog( 1 )->selectedNameFilter().toLatin1().data() );
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
 * void setDirectory ( const QDir & directory )
 */
HB_FUNC( QT_QFILEDIALOG_SETDIRECTORY_1 )
{
   hbqt_par_QFileDialog( 1 )->setDirectory( *hbqt_par_QDir( 2 ) );
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
 * void setHistory ( const QStringList & paths )
 */
HB_FUNC( QT_QFILEDIALOG_SETHISTORY )
{
   hbqt_par_QFileDialog( 1 )->setHistory( *hbqt_par_QStringList( 2 ) );
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
 * void setNameFilters ( const QStringList & filters )
 */
HB_FUNC( QT_QFILEDIALOG_SETNAMEFILTERS )
{
   hbqt_par_QFileDialog( 1 )->setNameFilters( *hbqt_par_QStringList( 2 ) );
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
   hb_retni( ( QFileDialog::ViewMode ) hbqt_par_QFileDialog( 1 )->viewMode() );
}

/*
 * QString getExistingDirectory ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), Options options = ShowDirsOnly )
 */
HB_FUNC( QT_QFILEDIALOG_GETEXISTINGDIRECTORY )
{
   hb_retc( hbqt_par_QFileDialog( 1 )->getExistingDirectory( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ), hbqt_par_QString( 4 ), ( HB_ISNUM( 5 ) ? ( QFileDialog::Options ) hb_parni( 5 ) : ( QFileDialog::Options ) QFileDialog::ShowDirsOnly ) ).toLatin1().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

