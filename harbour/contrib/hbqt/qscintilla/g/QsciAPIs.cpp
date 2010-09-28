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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciAPIs;

HBQT_GC_FUNC( hbqt_gcRelease_QsciAPIs )
{
   QsciAPIs  * ph = NULL ;
   HBQT_GC_T_QsciAPIs * p = ( HBQT_GC_T_QsciAPIs * ) Cargo;

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
   HBQT_GC_T_QsciAPIs * p = ( HBQT_GC_T_QsciAPIs * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciAPIs ), hbqt_gcFuncs() );

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
   {
      void * pText;
      ( p )->add( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void     clear ()
 */
HB_FUNC( QT_QSCIAPIS_CLEAR )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * bool     load (const QString &fname)
 */
HB_FUNC( QT_QSCIAPIS_LOAD )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->load( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * void     remove (const QString &entry)
 */
HB_FUNC( QT_QSCIAPIS_REMOVE )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      ( p )->remove( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void     prepare ()
 */
HB_FUNC( QT_QSCIAPIS_PREPARE )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      ( p )->prepare();
   }
}

/*
 * void     cancelPreparation ()
 */
HB_FUNC( QT_QSCIAPIS_CANCELPREPARATION )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      ( p )->cancelPreparation();
   }
}

/*
 * QString  defaultPreparedName () const
 */
HB_FUNC( QT_QSCIAPIS_DEFAULTPREPAREDNAME )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->defaultPreparedName().toUtf8().data() );
   }
}

/*
 * bool     isPrepared (const QString &fname=QString()) const
 */
HB_FUNC( QT_QSCIAPIS_ISPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isPrepared( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool     loadPrepared (const QString &fname=QString())
 */
HB_FUNC( QT_QSCIAPIS_LOADPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->loadPrepared( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool     savePrepared (const QString &fname=QString()) const
 */
HB_FUNC( QT_QSCIAPIS_SAVEPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->savePrepared( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void updateAutoCompletionList (const QStringList &context, QStringList &list)
 */
HB_FUNC( QT_QSCIAPIS_UPDATEAUTOCOMPLETIONLIST )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      ( p )->updateAutoCompletionList( *hbqt_par_QStringList( 2 ), *hbqt_par_QStringList( 3 ) );
   }
}

/*
 * virtual void autoCompletionSelected (const QString &sel)
 */
HB_FUNC( QT_QSCIAPIS_AUTOCOMPLETIONSELECTED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      ( p )->autoCompletionSelected( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual bool event (QEvent *e)
 */
HB_FUNC( QT_QSCIAPIS_EVENT )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      hb_retl( ( p )->event( hbqt_par_QEvent( 2 ) ) );
   }
}

/*
 * QStringList installedAPIFiles () const
 */
HB_FUNC( QT_QSCIAPIS_INSTALLEDAPIFILES )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->installedAPIFiles() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
