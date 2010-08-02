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
#include "hbqscintilla.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 14/15 [ 93.33% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  virtual QStringList callTips (const QStringList &context, int commas, QsciScintilla::CallTipsStyle style, QList< int > &shifts)
 */

#include <QtCore/QPointer>

#include <qsciapis.h>


/*
 * QsciAPIs (QsciLexer *lexer)
 * virtual ~QsciAPIs ()
 *
 */

typedef struct
{
   QPointer< QsciAPIs > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QsciAPIs;

QT_G_FUNC( hbqt_gcRelease_QsciAPIs )
{
   QsciAPIs  * ph = NULL ;
   QGC_POINTER_QsciAPIs * p = ( QGC_POINTER_QsciAPIs * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciAPIs   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciAPIs   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QsciAPIs          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QsciAPIs    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QsciAPIs    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciAPIs( void * pObj, bool bNew )
{
   QGC_POINTER_QsciAPIs * p = ( QGC_POINTER_QsciAPIs * ) hb_gcAllocate( sizeof( QGC_POINTER_QsciAPIs ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciAPIs >( ( QsciAPIs * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciAPIs;
   p->type = HBQT_TYPE_QsciAPIs;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QsciAPIs  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QsciAPIs", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSCIAPIS )
{
   QsciAPIs * pObj = NULL;

   pObj = new QsciAPIs( hbqt_par_QsciLexer( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QsciAPIs( ( void * ) pObj, true ) );
}

/*
 * void     add (const QString &entry)
 */
HB_FUNC( QT_QSCIAPIS_ADD )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->add( QsciAPIs::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_ADD FP=( p )->add( QsciAPIs::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void     clear ()
 */
HB_FUNC( QT_QSCIAPIS_CLEAR )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * bool     load (const QString &fname)
 */
HB_FUNC( QT_QSCIAPIS_LOAD )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retl( ( p )->load( QsciAPIs::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_LOAD FP=hb_retl( ( p )->load( QsciAPIs::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void     remove (const QString &entry)
 */
HB_FUNC( QT_QSCIAPIS_REMOVE )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->remove( QsciAPIs::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_REMOVE FP=( p )->remove( QsciAPIs::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void     prepare ()
 */
HB_FUNC( QT_QSCIAPIS_PREPARE )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->prepare();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_PREPARE FP=( p )->prepare(); p is NULL" ) );
   }
}

/*
 * void     cancelPreparation ()
 */
HB_FUNC( QT_QSCIAPIS_CANCELPREPARATION )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->cancelPreparation();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_CANCELPREPARATION FP=( p )->cancelPreparation(); p is NULL" ) );
   }
}

/*
 * QString  defaultPreparedName () const
 */
HB_FUNC( QT_QSCIAPIS_DEFAULTPREPAREDNAME )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retc( ( p )->defaultPreparedName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_DEFAULTPREPAREDNAME FP=hb_retc( ( p )->defaultPreparedName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool     isPrepared (const QString &fname=QString()) const
 */
HB_FUNC( QT_QSCIAPIS_ISPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retl( ( p )->isPrepared( QsciAPIs::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_ISPREPARED FP=hb_retl( ( p )->isPrepared( QsciAPIs::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool     loadPrepared (const QString &fname=QString())
 */
HB_FUNC( QT_QSCIAPIS_LOADPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retl( ( p )->loadPrepared( QsciAPIs::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_LOADPREPARED FP=hb_retl( ( p )->loadPrepared( QsciAPIs::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool     savePrepared (const QString &fname=QString()) const
 */
HB_FUNC( QT_QSCIAPIS_SAVEPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retl( ( p )->savePrepared( QsciAPIs::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_SAVEPREPARED FP=hb_retl( ( p )->savePrepared( QsciAPIs::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void updateAutoCompletionList (const QStringList &context, QStringList &list)
 */
HB_FUNC( QT_QSCIAPIS_UPDATEAUTOCOMPLETIONLIST )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->updateAutoCompletionList( *hbqt_par_QStringList( 2 ), *hbqt_par_QStringList( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_UPDATEAUTOCOMPLETIONLIST FP=( p )->updateAutoCompletionList( *hbqt_par_QStringList( 2 ), *hbqt_par_QStringList( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void autoCompletionSelected (const QString &sel)
 */
HB_FUNC( QT_QSCIAPIS_AUTOCOMPLETIONSELECTED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->autoCompletionSelected( QsciAPIs::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_AUTOCOMPLETIONSELECTED FP=( p )->autoCompletionSelected( QsciAPIs::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool event (QEvent *e)
 */
HB_FUNC( QT_QSCIAPIS_EVENT )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retl( ( p )->event( hbqt_par_QEvent( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_EVENT FP=hb_retl( ( p )->event( hbqt_par_QEvent( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QStringList installedAPIFiles () const
 */
HB_FUNC( QT_QSCIAPIS_INSTALLEDAPIFILES )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->installedAPIFiles() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCIAPIS_INSTALLEDAPIFILES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->installedAPIFiles() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
