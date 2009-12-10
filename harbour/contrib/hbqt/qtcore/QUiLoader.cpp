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

#include <QtCore/QPointer>

#include <QtUiTools/QUiLoader>
#include <QtCore/QDir>
#include <QtCore/QStringList>

/*
 * QUiLoader ( QObject * parent = 0 )
 * virtual ~QUiLoader ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QUiLoader > pq;
} QGC_POINTER_QUiLoader;

QT_G_FUNC( release_QUiLoader )
{
   QGC_POINTER_QUiLoader * p = ( QGC_POINTER_QUiLoader * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QUiLoader                    p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QUiLoader                   ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QUiLoader * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QUiLoader * ) p->ph )->~QUiLoader();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QUiLoader * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QUiLoader                   Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QUiLoader                   Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QUiLoader                   Object Allready deleted!" ) );
   }
}

void * gcAllocate_QUiLoader( void * pObj )
{
   QGC_POINTER_QUiLoader * p = ( QGC_POINTER_QUiLoader * ) hb_gcAllocate( sizeof( QGC_POINTER_QUiLoader ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QUiLoader;
   new( & p->pq ) QPointer< QUiLoader >( ( QUiLoader * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QUiLoader                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QUILOADER )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QUiLoader( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QUiLoader() ;
   }

   hb_retptrGC( gcAllocate_QUiLoader( pObj ) );
}
/*
 * void addPluginPath ( const QString & path )
 */
HB_FUNC( QT_QUILOADER_ADDPLUGINPATH )
{
   hbqt_par_QUiLoader( 1 )->addPluginPath( hbqt_par_QString( 2 ) );
}

/*
 * QStringList availableLayouts () const
 */
HB_FUNC( QT_QUILOADER_AVAILABLELAYOUTS )
{
   hb_retptrGC( gcAllocate_QStringList( new QStringList( hbqt_par_QUiLoader( 1 )->availableLayouts() ) ) );
}

/*
 * QStringList availableWidgets () const
 */
HB_FUNC( QT_QUILOADER_AVAILABLEWIDGETS )
{
   hb_retptrGC( gcAllocate_QStringList( new QStringList( hbqt_par_QUiLoader( 1 )->availableWidgets() ) ) );
}

/*
 * void clearPluginPaths ()
 */
HB_FUNC( QT_QUILOADER_CLEARPLUGINPATHS )
{
   hbqt_par_QUiLoader( 1 )->clearPluginPaths();
}

/*
 * virtual QAction * createAction ( QObject * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATEACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QUiLoader( 1 )->createAction( hbqt_par_QObject( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * virtual QActionGroup * createActionGroup ( QObject * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATEACTIONGROUP )
{
   hb_retptr( ( QActionGroup* ) hbqt_par_QUiLoader( 1 )->createActionGroup( hbqt_par_QObject( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * virtual QLayout * createLayout ( const QString & className, QObject * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATELAYOUT )
{
   hb_retptr( ( QLayout* ) hbqt_par_QUiLoader( 1 )->createLayout( hbqt_par_QString( 2 ), hbqt_par_QObject( 3 ), hbqt_par_QString( 4 ) ) );
}

/*
 * virtual QWidget * createWidget ( const QString & className, QWidget * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATEWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QUiLoader( 1 )->createWidget( hbqt_par_QString( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QString( 4 ) ) );
}

/*
 * bool isLanguageChangeEnabled () const
 */
HB_FUNC( QT_QUILOADER_ISLANGUAGECHANGEENABLED )
{
   hb_retl( hbqt_par_QUiLoader( 1 )->isLanguageChangeEnabled() );
}

/*
 * QWidget * load ( QIODevice * device, QWidget * parentWidget = 0 )
 */
HB_FUNC( QT_QUILOADER_LOAD )
{
   hb_retptr( ( QWidget* ) hbqt_par_QUiLoader( 1 )->load( hbqt_par_QIODevice( 2 ), hbqt_par_QWidget( 3 ) ) );
}

/*
 * QStringList pluginPaths () const
 */
HB_FUNC( QT_QUILOADER_PLUGINPATHS )
{
   hb_retptrGC( gcAllocate_QStringList( new QStringList( hbqt_par_QUiLoader( 1 )->pluginPaths() ) ) );
}

/*
 * void setLanguageChangeEnabled ( bool enabled )
 */
HB_FUNC( QT_QUILOADER_SETLANGUAGECHANGEENABLED )
{
   hbqt_par_QUiLoader( 1 )->setLanguageChangeEnabled( hb_parl( 2 ) );
}

/*
 * void setWorkingDirectory ( const QDir & dir )
 */
HB_FUNC( QT_QUILOADER_SETWORKINGDIRECTORY )
{
   hbqt_par_QUiLoader( 1 )->setWorkingDirectory( *hbqt_par_QDir( 2 ) );
}

/*
 * QDir workingDirectory () const
 */
HB_FUNC( QT_QUILOADER_WORKINGDIRECTORY )
{
   hb_retptrGC( gcAllocate_QDir( new QDir( hbqt_par_QUiLoader( 1 )->workingDirectory() ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
