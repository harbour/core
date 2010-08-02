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

#include "hbqt.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

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
   QPointer< QFile > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFile;

QT_G_FUNC( hbqt_gcRelease_QFile )
{
   QFile  * ph = NULL ;
   QGC_POINTER_QFile * p = ( QGC_POINTER_QFile * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFile   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFile   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFile          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFile    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFile    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFile( void * pObj, bool bNew )
{
   QGC_POINTER_QFile * p = ( QGC_POINTER_QFile * ) hb_gcAllocate( sizeof( QGC_POINTER_QFile ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFile >( ( QFile * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFile;
   p->type = HBQT_TYPE_QFile;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFile  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFile", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFILE )
{
   QFile * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QFile( hbqt_par_QString( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFile( ( void * ) pObj, true ) );
}

/*
 * virtual bool atEnd () const
 */
HB_FUNC( QT_QFILE_ATEND )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_ATEND FP=hb_retl( ( p )->atEnd() ); p is NULL" ) );
   }
}

/*
 * virtual void close ()
 */
HB_FUNC( QT_QFILE_CLOSE )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      ( p )->close();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_CLOSE FP=( p )->close(); p is NULL" ) );
   }
}

/*
 * bool copy ( const QString & newName )
 */
HB_FUNC( QT_QFILE_COPY )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->copy( QFile::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_COPY FP=hb_retl( ( p )->copy( QFile::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * FileError error () const
 */
HB_FUNC( QT_QFILE_ERROR )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retni( ( QFile::FileError ) ( p )->error() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_ERROR FP=hb_retni( ( QFile::FileError ) ( p )->error() ); p is NULL" ) );
   }
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QFILE_EXISTS )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->exists() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_EXISTS FP=hb_retl( ( p )->exists() ); p is NULL" ) );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QFILE_FILENAME )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retc( ( p )->fileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_FILENAME FP=hb_retc( ( p )->fileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool flush ()
 */
HB_FUNC( QT_QFILE_FLUSH )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->flush() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_FLUSH FP=hb_retl( ( p )->flush() ); p is NULL" ) );
   }
}

/*
 * int handle () const
 */
HB_FUNC( QT_QFILE_HANDLE )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retni( ( p )->handle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_HANDLE FP=hb_retni( ( p )->handle() ); p is NULL" ) );
   }
}

/*
 * virtual bool isSequential () const
 */
HB_FUNC( QT_QFILE_ISSEQUENTIAL )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->isSequential() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_ISSEQUENTIAL FP=hb_retl( ( p )->isSequential() ); p is NULL" ) );
   }
}

/*
 * bool link ( const QString & linkName )
 */
HB_FUNC( QT_QFILE_LINK )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->link( QFile::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_LINK FP=hb_retl( ( p )->link( QFile::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * uchar * map ( qint64 offset, qint64 size, MemoryMapFlags flags = NoOptions )
 */
HB_FUNC( QT_QFILE_MAP )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retc( ( const char * ) ( p )->map( hb_parnint( 2 ), hb_parnint( 3 ), ( HB_ISNUM( 4 ) ? ( QFile::MemoryMapFlags ) hb_parni( 4 ) : ( QFile::MemoryMapFlags ) QFile::NoOptions ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_MAP FP=hb_retc( ( const char * ) ( p )->map( hb_parnint( 2 ), hb_parnint( 3 ), ( HB_ISNUM( 4 ) ? ( QFile::MemoryMapFlags ) hb_parni( 4 ) : ( QFile::MemoryMapFlags ) QFile::NoOptions ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool open ( OpenMode mode )
 */
HB_FUNC( QT_QFILE_OPEN )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->open( ( QFile::OpenMode ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_OPEN FP=hb_retl( ( p )->open( ( QFile::OpenMode ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool open ( int fd, OpenMode mode )
 */
HB_FUNC( QT_QFILE_OPEN_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->open( hb_parni( 2 ), ( QFile::OpenMode ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_OPEN_1 FP=hb_retl( ( p )->open( hb_parni( 2 ), ( QFile::OpenMode ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * Permissions permissions () const
 */
HB_FUNC( QT_QFILE_PERMISSIONS )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retni( ( QFile::Permissions ) ( p )->permissions() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_PERMISSIONS FP=hb_retni( ( QFile::Permissions ) ( p )->permissions() ); p is NULL" ) );
   }
}

/*
 * bool remove ()
 */
HB_FUNC( QT_QFILE_REMOVE )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->remove() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_REMOVE FP=hb_retl( ( p )->remove() ); p is NULL" ) );
   }
}

/*
 * bool rename ( const QString & newName )
 */
HB_FUNC( QT_QFILE_RENAME )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->rename( QFile::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_RENAME FP=hb_retl( ( p )->rename( QFile::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool resize ( qint64 sz )
 */
HB_FUNC( QT_QFILE_RESIZE )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->resize( hb_parnint( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_RESIZE FP=hb_retl( ( p )->resize( hb_parnint( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setFileName ( const QString & name )
 */
HB_FUNC( QT_QFILE_SETFILENAME )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      ( p )->setFileName( QFile::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_SETFILENAME FP=( p )->setFileName( QFile::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool setPermissions ( Permissions permissions )
 */
HB_FUNC( QT_QFILE_SETPERMISSIONS )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->setPermissions( ( QFile::Permissions ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_SETPERMISSIONS FP=hb_retl( ( p )->setPermissions( ( QFile::Permissions ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual qint64 size () const
 */
HB_FUNC( QT_QFILE_SIZE )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retnint( ( p )->size() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_SIZE FP=hb_retnint( ( p )->size() ); p is NULL" ) );
   }
}

/*
 * QString symLinkTarget () const
 */
HB_FUNC( QT_QFILE_SYMLINKTARGET )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retc( ( p )->symLinkTarget().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_SYMLINKTARGET FP=hb_retc( ( p )->symLinkTarget().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void unsetError ()
 */
HB_FUNC( QT_QFILE_UNSETERROR )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      ( p )->unsetError();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_UNSETERROR FP=( p )->unsetError(); p is NULL" ) );
   }
}

/*
 * bool copy ( const QString & fileName, const QString & newName )
 */
HB_FUNC( QT_QFILE_COPY_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->copy( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_COPY_1 FP=hb_retl( ( p )->copy( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * QString decodeName ( const QByteArray & localFileName )
 */
HB_FUNC( QT_QFILE_DECODENAME )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retc( ( p )->decodeName( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_DECODENAME FP=hb_retc( ( p )->decodeName( *hbqt_par_QByteArray( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString decodeName ( const char * localFileName )
 */
HB_FUNC( QT_QFILE_DECODENAME_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retc( ( p )->decodeName( hbqt_par_char( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_DECODENAME_1 FP=hb_retc( ( p )->decodeName( hbqt_par_char( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QByteArray encodeName ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_ENCODENAME )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodeName( QFile::tr( hb_parc( 2 ) ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_ENCODENAME FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodeName( QFile::tr( hb_parc( 2 ) ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool exists ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_EXISTS_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->exists( QFile::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_EXISTS_1 FP=hb_retl( ( p )->exists( QFile::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool link ( const QString & fileName, const QString & linkName )
 */
HB_FUNC( QT_QFILE_LINK_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->link( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_LINK_1 FP=hb_retl( ( p )->link( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * Permissions permissions ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_PERMISSIONS_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retni( ( QFile::Permissions ) ( p )->permissions( QFile::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_PERMISSIONS_1 FP=hb_retni( ( QFile::Permissions ) ( p )->permissions( QFile::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool remove ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_REMOVE_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->remove( QFile::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_REMOVE_1 FP=hb_retl( ( p )->remove( QFile::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool rename ( const QString & oldName, const QString & newName )
 */
HB_FUNC( QT_QFILE_RENAME_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->rename( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_RENAME_1 FP=hb_retl( ( p )->rename( QFile::tr( hb_parc( 2 ) ), QFile::tr( hb_parc( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool resize ( const QString & fileName, qint64 sz )
 */
HB_FUNC( QT_QFILE_RESIZE_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->resize( QFile::tr( hb_parc( 2 ) ), hb_parnint( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_RESIZE_1 FP=hb_retl( ( p )->resize( QFile::tr( hb_parc( 2 ) ), hb_parnint( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool setPermissions ( const QString & fileName, Permissions permissions )
 */
HB_FUNC( QT_QFILE_SETPERMISSIONS_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retl( ( p )->setPermissions( QFile::tr( hb_parc( 2 ) ), ( QFile::Permissions ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_SETPERMISSIONS_1 FP=hb_retl( ( p )->setPermissions( QFile::tr( hb_parc( 2 ) ), ( QFile::Permissions ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QString symLinkTarget ( const QString & fileName )
 */
HB_FUNC( QT_QFILE_SYMLINKTARGET_1 )
{
   QFile * p = hbqt_par_QFile( 1 );
   if( p )
      hb_retc( ( p )->symLinkTarget( QFile::tr( hb_parc( 2 ) ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILE_SYMLINKTARGET_1 FP=hb_retc( ( p )->symLinkTarget( QFile::tr( hb_parc( 2 ) ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
