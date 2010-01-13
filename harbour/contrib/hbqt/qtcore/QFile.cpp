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

/*
 *  enum FileError { NoError, ReadError, WriteError, FatalError, ..., CopyError }
 *  enum MemoryMapFlags { NoOptions }
 *  enum Permission { ReadOwner, WriteOwner, ExeOwner, ReadUser, ..., ExeOther }
 *  flags Permissions
 *  flags OpenMode
 *  enum OpenModeFlag { NotOpen, ReadOnly, WriteOnly, ReadWrite, ..., Unbuffered }
 */

#include <QtCore/QPointer>

#include <QtCore/QFile>


/* QFile ( const QString & name )
 * QFile ( QObject * parent )
 * QFile ( const QString & name, QObject * parent )
 * ~QFile ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QFile > pq;
} QGC_POINTER_QFile;

QT_G_FUNC( hbqt_gcRelease_QFile )
{
   QGC_POINTER_QFile * p = ( QGC_POINTER_QFile * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QFile                        p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QFile                       ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QFile * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QFile * ) p->ph )->~QFile();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QFile * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QFile                       Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QFile                       Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QFile                       Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QFile( void * pObj )
{
   QGC_POINTER_QFile * p = ( QGC_POINTER_QFile * ) hb_gcAllocate( sizeof( QGC_POINTER_QFile ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QFile;
   new( & p->pq ) QPointer< QFile >( ( QFile * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QFile                       %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QFILE )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QFile( hbqt_par_QString( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFile( pObj ) );
}
/*
 * virtual bool atEnd () const
 */
HB_FUNC( QT_QFILE_ATEND )
{
   hb_retl( hbqt_par_QFile( 1 )->atEnd() );
}

/*
 * virtual void close ()
 */
HB_FUNC( QT_QFILE_CLOSE )
{
   hbqt_par_QFile( 1 )->close();
}

/*
 * bool copy ( const QString & newName )
 */
HB_FUNC( QT_QFILE_COPY )
{
   hb_retl( hbqt_par_QFile( 1 )->copy( QFile::tr( hb_parc( 2 ) ) ) );
}

/*
 * FileError error () const
 */
HB_FUNC( QT_QFILE_ERROR )
{
   hb_retni( ( QFile::FileError ) hbqt_par_QFile( 1 )->error() );
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QFILE_EXISTS )
{
   hb_retl( hbqt_par_QFile( 1 )->exists() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QFILE_FILENAME )
{
   hb_retc( hbqt_par_QFile( 1 )->fileName().toAscii().data() );
}

/*
 * bool flush ()
 */
HB_FUNC( QT_QFILE_FLUSH )
{
   hb_retl( hbqt_par_QFile( 1 )->flush() );
}

/*
 * int handle () const
 */
HB_FUNC( QT_QFILE_HANDLE )
{
   hb_retni( hbqt_par_QFile( 1 )->handle() );
}

/*
 * virtual bool isSequential () const
 */
HB_FUNC( QT_QFILE_ISSEQUENTIAL )
{
   hb_retl( hbqt_par_QFile( 1 )->isSequential() );
}

/*
 * bool link ( const QString & linkName )
 */
HB_FUNC( QT_QFILE_LINK )
{
   hb_retl( hbqt_par_QFile( 1 )->link( QFile::tr( hb_parc( 2 ) ) ) );
}

/*
 * uchar * map ( qint64 offset, qint64 size, MemoryMapFlags flags = NoOptions )
 */
HB_FUNC( QT_QFILE_MAP )
{
   hb_retptr( ( uchar* ) hbqt_par_QFile( 1 )->map( hb_parnint( 2 ), hb_parnint( 3 ), ( HB_ISNUM( 4 ) ? ( QFile::MemoryMapFlags ) hb_parni( 4 ) : ( QFile::MemoryMapFlags ) QFile::NoOptions ) ) );
}

/*
 * virtual bool open ( OpenMode mode )
 */
HB_FUNC( QT_QFILE_OPEN )
{
   hb_retl( hbqt_par_QFile( 1 )->open( ( QFile::OpenMode ) hb_parni( 2 ) ) );
}

/*
 * bool open ( int fd, OpenMode mode )
 */
HB_FUNC( QT_QFILE_OPEN_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->open( hb_parni( 2 ), ( QFile::OpenMode ) hb_parni( 3 ) ) );
}

/*
 * Permissions permissions () const
 */
HB_FUNC( QT_QFILE_PERMISSIONS )
{
   hb_retni( ( QFile::Permissions ) hbqt_par_QFile( 1 )->permissions() );
}

/*
 * bool remove ()
 */
HB_FUNC( QT_QFILE_REMOVE )
{
   hb_retl( hbqt_par_QFile( 1 )->remove() );
}

/*
 * bool rename ( const QString & newName )
 */
HB_FUNC( QT_QFILE_RENAME )
{
   hb_retl( hbqt_par_QFile( 1 )->rename( QFile::tr( hb_parc( 2 ) ) ) );
}

/*
 * bool resize ( qint64 sz )
 */
HB_FUNC( QT_QFILE_RESIZE )
{
   hb_retl( hbqt_par_QFile( 1 )->resize( hb_parnint( 2 ) ) );
}

/*
 * void setFileName ( const QString & name )
 */
HB_FUNC( QT_QFILE_SETFILENAME )
{
   hbqt_par_QFile( 1 )->setFileName( QFile::tr( hb_parc( 2 ) ) );
}

/*
 * bool setPermissions ( Permissions permissions )
 */
HB_FUNC( QT_QFILE_SETPERMISSIONS )
{
   hb_retl( hbqt_par_QFile( 1 )->setPermissions( ( QFile::Permissions ) hb_parni( 2 ) ) );
}

/*
 * virtual qint64 size () const
 */
HB_FUNC( QT_QFILE_SIZE )
{
   hb_retnint( hbqt_par_QFile( 1 )->size() );
}

/*
 * QString symLinkTarget () const
 */
HB_FUNC( QT_QFILE_SYMLINKTARGET )
{
   hb_retc( hbqt_par_QFile( 1 )->symLinkTarget().toAscii().data() );
}

/*
 * void unsetError ()
 */
HB_FUNC( QT_QFILE_UNSETERROR )
{
   hbqt_par_QFile( 1 )->unsetError();
}

/*
 * bool copy ( const QString & fileName, const QString & newName )
 */
HB_FUNC( QT_QFILE_COPY_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->copy( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) );
}

/*
 * QString decodeName ( const QByteArray & localFileName )
 */
HB_FUNC( QT_QFILE_DECODENAME )
{
   hb_retc( hbqt_par_QFile( 1 )->decodeName( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
}

/*
 * QString decodeName ( const char * localFileName )
 */
HB_FUNC( QT_QFILE_DECODENAME_1 )
{
   hb_retc( hbqt_par_QFile( 1 )->decodeName( hbqt_par_char( 2 ) ).toAscii().data() );
}

/*
 * QByteArray encodeName ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_ENCODENAME )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QFile( 1 )->encodeName( QFile::tr( hb_parc( 2 ) ) ) ) ) );
}

/*
 * bool exists ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_EXISTS_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->exists( QFile::tr( hb_parc( 2 ) ) ) );
}

/*
 * bool link ( const QString & fileName, const QString & linkName )
 */
HB_FUNC( QT_QFILE_LINK_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->link( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) );
}

/*
 * Permissions permissions ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_PERMISSIONS_1 )
{
   hb_retni( ( QFile::Permissions ) hbqt_par_QFile( 1 )->permissions( QFile::tr( hb_parc( 2 ) ) ) );
}

/*
 * bool remove ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_REMOVE_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->remove( QFile::tr( hb_parc( 2 ) ) ) );
}

/*
 * bool rename ( const QString & oldName, const QString & newName )
 */
HB_FUNC( QT_QFILE_RENAME_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->rename( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) );
}

/*
 * bool resize ( const QString & fileName, qint64 sz )
 */
HB_FUNC( QT_QFILE_RESIZE_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->resize( QFile::tr( hb_parc( 2 ) ), hb_parnint( 3 ) ) );
}

/*
 * bool setPermissions ( const QString & fileName, Permissions permissions )
 */
HB_FUNC( QT_QFILE_SETPERMISSIONS_1 )
{
   hb_retl( hbqt_par_QFile( 1 )->setPermissions( QFile::tr( hb_parc( 2 ) ), ( QFile::Permissions ) hb_parni( 3 ) ) );
}

/*
 * QString symLinkTarget ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_SYMLINKTARGET_1 )
{
   hb_retc( hbqt_par_QFile( 1 )->symLinkTarget( QFile::tr( hb_parc( 2 ) ) ).toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
