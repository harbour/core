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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

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
   QPointer< QUiLoader > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QUiLoader;

HBQT_GC_FUNC( hbqt_gcRelease_QUiLoader )
{
   QUiLoader  * ph = NULL ;
   HBQT_GC_T_QUiLoader * p = ( HBQT_GC_T_QUiLoader * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QUiLoader   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QUiLoader   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QUiLoader          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QUiLoader    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QUiLoader    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QUiLoader( void * pObj, bool bNew )
{
   HBQT_GC_T_QUiLoader * p = ( HBQT_GC_T_QUiLoader * ) hb_gcAllocate( sizeof( HBQT_GC_T_QUiLoader ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QUiLoader >( ( QUiLoader * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QUiLoader;
   p->type = HBQT_TYPE_QUiLoader;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QUiLoader  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QUiLoader", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QUILOADER )
{
   QUiLoader * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QUiLoader( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QUiLoader() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QUiLoader( ( void * ) pObj, true ) );
}

/*
 * void addPluginPath ( const QString & path )
 */
HB_FUNC( QT_QUILOADER_ADDPLUGINPATH )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      ( p )->addPluginPath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList availableLayouts () const
 */
HB_FUNC( QT_QUILOADER_AVAILABLELAYOUTS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->availableLayouts() ), true ) );
   }
}

/*
 * QStringList availableWidgets () const
 */
HB_FUNC( QT_QUILOADER_AVAILABLEWIDGETS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->availableWidgets() ), true ) );
   }
}

/*
 * void clearPluginPaths ()
 */
HB_FUNC( QT_QUILOADER_CLEARPLUGINPATHS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      ( p )->clearPluginPaths();
   }
}

/*
 * virtual QAction * createAction ( QObject * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATEACTION )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->createAction( hbqt_par_QObject( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * virtual QActionGroup * createActionGroup ( QObject * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATEACTIONGROUP )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QActionGroup( ( p )->createActionGroup( hbqt_par_QObject( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * virtual QLayout * createLayout ( const QString & className, QObject * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATELAYOUT )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->createLayout( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * virtual QWidget * createWidget ( const QString & className, QWidget * parent = 0, const QString & name = QString() )
 */
HB_FUNC( QT_QUILOADER_CREATEWIDGET )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createWidget( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QWidget( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * bool isLanguageChangeEnabled () const
 */
HB_FUNC( QT_QUILOADER_ISLANGUAGECHANGEENABLED )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      hb_retl( ( p )->isLanguageChangeEnabled() );
   }
}

/*
 * QWidget * load ( QIODevice * device, QWidget * parentWidget = 0 )
 */
HB_FUNC( QT_QUILOADER_LOAD )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->load( hbqt_par_QIODevice( 2 ), hbqt_par_QWidget( 3 ) ), false ) );
   }
}

/*
 * QStringList pluginPaths () const
 */
HB_FUNC( QT_QUILOADER_PLUGINPATHS )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->pluginPaths() ), true ) );
   }
}

/*
 * void setLanguageChangeEnabled ( bool enabled )
 */
HB_FUNC( QT_QUILOADER_SETLANGUAGECHANGEENABLED )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      ( p )->setLanguageChangeEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setWorkingDirectory ( const QDir & dir )
 */
HB_FUNC( QT_QUILOADER_SETWORKINGDIRECTORY )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      ( p )->setWorkingDirectory( *hbqt_par_QDir( 2 ) );
   }
}

/*
 * QDir workingDirectory () const
 */
HB_FUNC( QT_QUILOADER_WORKINGDIRECTORY )
{
   QUiLoader * p = hbqt_par_QUiLoader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->workingDirectory() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
