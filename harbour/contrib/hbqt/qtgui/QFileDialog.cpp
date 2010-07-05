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

#include "../hbqt.h"

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
 *  Constructed[ 46/51 [ 90.20% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setSidebarUrls ( const QList<QUrl> & urls )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // void open ( QObject * receiver, const char * member )
 *  //QString getOpenFileName ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), const QString & filter = QString(), QString * selectedFilter = 0, Options options = 0 )
 *  //QStringList getOpenFileNames ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), const QString & filter = QString(), QString * selectedFilter = 0, Options options = 0 )
 *  //QString getSaveFileName ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), const QString & filter = QString(), QString * selectedFilter = 0, Options options = 0 )
 */

#include <QtCore/QPointer>

#include <QtGui/QFileDialog>
#include <QtCore/QUrl>

/*
 * QFileDialog ( QWidget * parent, Qt::WindowFlags flags )
 * QFileDialog ( QWidget * parent = 0, const QString & caption = QString(), const QString & directory = QString(), const QString & filter = QString() )
 * ~QFileDialog ()
 */

typedef struct
{
   QPointer< QFileDialog > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
} QGC_POINTER_QFileDialog;

QT_G_FUNC( hbqt_gcRelease_QFileDialog )
{
   QFileDialog  * ph = NULL ;
   QGC_POINTER_QFileDialog * p = ( QGC_POINTER_QFileDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFileDialog   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFileDialog   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFileDialog          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFileDialog    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFileDialog    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFileDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QFileDialog * p = ( QGC_POINTER_QFileDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QFileDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFileDialog >( ( QFileDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFileDialog;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFileDialog  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFileDialog", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFILEDIALOG )
{
   QFileDialog * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFileDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) 0 ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QFileDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QFileDialog() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFileDialog( ( void * ) pObj, true ) );
}

/*
 * AcceptMode acceptMode () const
 */
HB_FUNC( QT_QFILEDIALOG_ACCEPTMODE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retni( ( QFileDialog::AcceptMode ) ( p )->acceptMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_ACCEPTMODE FP=hb_retni( ( QFileDialog::AcceptMode ) ( p )->acceptMode() ); p is NULL" ) );
   }
}

/*
 * bool confirmOverwrite () const
 */
HB_FUNC( QT_QFILEDIALOG_CONFIRMOVERWRITE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retl( ( p )->confirmOverwrite() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_CONFIRMOVERWRITE FP=hb_retl( ( p )->confirmOverwrite() ); p is NULL" ) );
   }
}

/*
 * QString defaultSuffix () const
 */
HB_FUNC( QT_QFILEDIALOG_DEFAULTSUFFIX )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retc( ( p )->defaultSuffix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_DEFAULTSUFFIX FP=hb_retc( ( p )->defaultSuffix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QDir directory () const
 */
HB_FUNC( QT_QFILEDIALOG_DIRECTORY )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->directory() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_DIRECTORY FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->directory() ), true ) ); p is NULL" ) );
   }
}

/*
 * FileMode fileMode () const
 */
HB_FUNC( QT_QFILEDIALOG_FILEMODE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retni( ( QFileDialog::FileMode ) ( p )->fileMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_FILEMODE FP=hb_retni( ( QFileDialog::FileMode ) ( p )->fileMode() ); p is NULL" ) );
   }
}

/*
 * QDir::Filters filter () const
 */
HB_FUNC( QT_QFILEDIALOG_FILTER )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retni( ( QDir::Filters ) ( p )->filter() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_FILTER FP=hb_retni( ( QDir::Filters ) ( p )->filter() ); p is NULL" ) );
   }
}

/*
 * QStringList history () const
 */
HB_FUNC( QT_QFILEDIALOG_HISTORY )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->history() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_HISTORY FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->history() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFileIconProvider * iconProvider () const
 */
HB_FUNC( QT_QFILEDIALOG_ICONPROVIDER )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFileIconProvider( ( p )->iconProvider(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_ICONPROVIDER FP=hb_retptrGC( hbqt_gcAllocate_QFileIconProvider( ( p )->iconProvider(), false ) ); p is NULL" ) );
   }
}

/*
 * bool isNameFilterDetailsVisible () const
 */
HB_FUNC( QT_QFILEDIALOG_ISNAMEFILTERDETAILSVISIBLE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retl( ( p )->isNameFilterDetailsVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_ISNAMEFILTERDETAILSVISIBLE FP=hb_retl( ( p )->isNameFilterDetailsVisible() ); p is NULL" ) );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QFILEDIALOG_ISREADONLY )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_ISREADONLY FP=hb_retl( ( p )->isReadOnly() ); p is NULL" ) );
   }
}

/*
 * QAbstractItemDelegate * itemDelegate () const
 */
HB_FUNC( QT_QFILEDIALOG_ITEMDELEGATE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_ITEMDELEGATE FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemDelegate( ( p )->itemDelegate(), false ) ); p is NULL" ) );
   }
}

/*
 * QString labelText ( DialogLabel label ) const
 */
HB_FUNC( QT_QFILEDIALOG_LABELTEXT )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retc( ( p )->labelText( ( QFileDialog::DialogLabel ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_LABELTEXT FP=hb_retc( ( p )->labelText( ( QFileDialog::DialogLabel ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QFILEDIALOG_NAMEFILTERS )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_NAMEFILTERS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) ); p is NULL" ) );
   }
}

/*
 * Options options () const
 */
HB_FUNC( QT_QFILEDIALOG_OPTIONS )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retni( ( QFileDialog::Options ) ( p )->options() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_OPTIONS FP=hb_retni( ( QFileDialog::Options ) ( p )->options() ); p is NULL" ) );
   }
}

/*
 * QAbstractProxyModel * proxyModel () const
 */
HB_FUNC( QT_QFILEDIALOG_PROXYMODEL )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractProxyModel( ( p )->proxyModel(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_PROXYMODEL FP=hb_retptrGC( hbqt_gcAllocate_QAbstractProxyModel( ( p )->proxyModel(), false ) ); p is NULL" ) );
   }
}

/*
 * bool resolveSymlinks () const
 */
HB_FUNC( QT_QFILEDIALOG_RESOLVESYMLINKS )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retl( ( p )->resolveSymlinks() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_RESOLVESYMLINKS FP=hb_retl( ( p )->resolveSymlinks() ); p is NULL" ) );
   }
}

/*
 * bool restoreState ( const QByteArray & state )
 */
HB_FUNC( QT_QFILEDIALOG_RESTORESTATE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_RESTORESTATE FP=hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray saveState () const
 */
HB_FUNC( QT_QFILEDIALOG_SAVESTATE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SAVESTATE FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) ); p is NULL" ) );
   }
}

/*
 * void selectFile ( const QString & filename )
 */
HB_FUNC( QT_QFILEDIALOG_SELECTFILE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->selectFile( QFileDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SELECTFILE FP=( p )->selectFile( QFileDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void selectNameFilter ( const QString & filter )
 */
HB_FUNC( QT_QFILEDIALOG_SELECTNAMEFILTER )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->selectNameFilter( QFileDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SELECTNAMEFILTER FP=( p )->selectNameFilter( QFileDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QStringList selectedFiles () const
 */
HB_FUNC( QT_QFILEDIALOG_SELECTEDFILES )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->selectedFiles() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SELECTEDFILES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->selectedFiles() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString selectedNameFilter () const
 */
HB_FUNC( QT_QFILEDIALOG_SELECTEDNAMEFILTER )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retc( ( p )->selectedNameFilter().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SELECTEDNAMEFILTER FP=hb_retc( ( p )->selectedNameFilter().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setAcceptMode ( AcceptMode mode )
 */
HB_FUNC( QT_QFILEDIALOG_SETACCEPTMODE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setAcceptMode( ( QFileDialog::AcceptMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETACCEPTMODE FP=( p )->setAcceptMode( ( QFileDialog::AcceptMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setConfirmOverwrite ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETCONFIRMOVERWRITE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setConfirmOverwrite( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETCONFIRMOVERWRITE FP=( p )->setConfirmOverwrite( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QFILEDIALOG_SETDEFAULTSUFFIX )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setDefaultSuffix( QFileDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETDEFAULTSUFFIX FP=( p )->setDefaultSuffix( QFileDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setDirectory ( const QString & directory )
 */
HB_FUNC( QT_QFILEDIALOG_SETDIRECTORY )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setDirectory( QFileDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETDIRECTORY FP=( p )->setDirectory( QFileDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setDirectory ( const QDir & directory )
 */
HB_FUNC( QT_QFILEDIALOG_SETDIRECTORY_1 )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setDirectory( *hbqt_par_QDir( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETDIRECTORY_1 FP=( p )->setDirectory( *hbqt_par_QDir( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFileMode ( FileMode mode )
 */
HB_FUNC( QT_QFILEDIALOG_SETFILEMODE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setFileMode( ( QFileDialog::FileMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETFILEMODE FP=( p )->setFileMode( ( QFileDialog::FileMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFilter ( QDir::Filters filters )
 */
HB_FUNC( QT_QFILEDIALOG_SETFILTER )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETFILTER FP=( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHistory ( const QStringList & paths )
 */
HB_FUNC( QT_QFILEDIALOG_SETHISTORY )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setHistory( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETHISTORY FP=( p )->setHistory( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIconProvider ( QFileIconProvider * provider )
 */
HB_FUNC( QT_QFILEDIALOG_SETICONPROVIDER )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setIconProvider( hbqt_par_QFileIconProvider( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETICONPROVIDER FP=( p )->setIconProvider( hbqt_par_QFileIconProvider( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItemDelegate ( QAbstractItemDelegate * delegate )
 */
HB_FUNC( QT_QFILEDIALOG_SETITEMDELEGATE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETITEMDELEGATE FP=( p )->setItemDelegate( hbqt_par_QAbstractItemDelegate( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLabelText ( DialogLabel label, const QString & text )
 */
HB_FUNC( QT_QFILEDIALOG_SETLABELTEXT )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setLabelText( ( QFileDialog::DialogLabel ) hb_parni( 2 ), QFileDialog::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETLABELTEXT FP=( p )->setLabelText( ( QFileDialog::DialogLabel ) hb_parni( 2 ), QFileDialog::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setNameFilter ( const QString & filter )
 */
HB_FUNC( QT_QFILEDIALOG_SETNAMEFILTER )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setNameFilter( QFileDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETNAMEFILTER FP=( p )->setNameFilter( QFileDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setNameFilterDetailsVisible ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETNAMEFILTERDETAILSVISIBLE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setNameFilterDetailsVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETNAMEFILTERDETAILSVISIBLE FP=( p )->setNameFilterDetailsVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNameFilters ( const QStringList & filters )
 */
HB_FUNC( QT_QFILEDIALOG_SETNAMEFILTERS )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setNameFilters( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETNAMEFILTERS FP=( p )->setNameFilters( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOption ( Option option, bool on = true )
 */
HB_FUNC( QT_QFILEDIALOG_SETOPTION )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setOption( ( QFileDialog::Option ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETOPTION FP=( p )->setOption( ( QFileDialog::Option ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setOptions ( Options options )
 */
HB_FUNC( QT_QFILEDIALOG_SETOPTIONS )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setOptions( ( QFileDialog::Options ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETOPTIONS FP=( p )->setOptions( ( QFileDialog::Options ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setProxyModel ( QAbstractProxyModel * proxyModel )
 */
HB_FUNC( QT_QFILEDIALOG_SETPROXYMODEL )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setProxyModel( hbqt_par_QAbstractProxyModel( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETPROXYMODEL FP=( p )->setProxyModel( hbqt_par_QAbstractProxyModel( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setReadOnly ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETREADONLY )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETREADONLY FP=( p )->setReadOnly( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setResolveSymlinks ( bool enabled )
 */
HB_FUNC( QT_QFILEDIALOG_SETRESOLVESYMLINKS )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setResolveSymlinks( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETRESOLVESYMLINKS FP=( p )->setResolveSymlinks( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QFILEDIALOG_SETVIEWMODE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      ( p )->setViewMode( ( QFileDialog::ViewMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SETVIEWMODE FP=( p )->setViewMode( ( QFileDialog::ViewMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QList<QUrl> sidebarUrls () const
 */
HB_FUNC( QT_QFILEDIALOG_SIDEBARURLS )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QUrl>( ( p )->sidebarUrls() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_SIDEBARURLS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QUrl>( ( p )->sidebarUrls() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool testOption ( Option option ) const
 */
HB_FUNC( QT_QFILEDIALOG_TESTOPTION )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QFileDialog::Option ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_TESTOPTION FP=hb_retl( ( p )->testOption( ( QFileDialog::Option ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QFILEDIALOG_VIEWMODE )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retni( ( QFileDialog::ViewMode ) ( p )->viewMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_VIEWMODE FP=hb_retni( ( QFileDialog::ViewMode ) ( p )->viewMode() ); p is NULL" ) );
   }
}

/*
 * QString getExistingDirectory ( QWidget * parent = 0, const QString & caption = QString(), const QString & dir = QString(), Options options = ShowDirsOnly )
 */
HB_FUNC( QT_QFILEDIALOG_GETEXISTINGDIRECTORY )
{
   QFileDialog * p = hbqt_par_QFileDialog( 1 );
   if( p )
      hb_retc( ( p )->getExistingDirectory( hbqt_par_QWidget( 2 ), QFileDialog::tr( hb_parc( 3 ) ), QFileDialog::tr( hb_parc( 4 ) ), ( HB_ISNUM( 5 ) ? ( QFileDialog::Options ) hb_parni( 5 ) : ( QFileDialog::Options ) QFileDialog::ShowDirsOnly ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEDIALOG_GETEXISTINGDIRECTORY FP=hb_retc( ( p )->getExistingDirectory( hbqt_par_QWidget( 2 ), QFileDialog::tr( hb_parc( 3 ) ), QFileDialog::tr( hb_parc( 4 ) ), ( HB_ISNUM( 5 ) ? ( QFileDialog::Options ) hb_parni( 5 ) : ( QFileDialog::Options ) QFileDialog::ShowDirsOnly ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
