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

#include <QtCore/QTranslator>


/* QTranslator ( QObject * parent = 0 )
 * ~QTranslator ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QTranslator > pq;
} QGC_POINTER_QTranslator;

QT_G_FUNC( hbqt_gcRelease_QTranslator )
{
   QGC_POINTER_QTranslator * p = ( QGC_POINTER_QTranslator * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTranslator   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QTranslator * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTranslator   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QTranslatorph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTranslator    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTranslator    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTranslator( void * pObj, bool bNew )
{
   QGC_POINTER_QTranslator * p = ( QGC_POINTER_QTranslator * ) hb_gcAllocate( sizeof( QGC_POINTER_QTranslator ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTranslator;

   if( bNew )
   {
      new( & p->pq ) QPointer< QTranslator >( ( QTranslator * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTranslator                ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTRANSLATOR )
{
   void * pObj = NULL;

   pObj = new QTranslator() ;

   hb_retptrGC( hbqt_gcAllocate_QTranslator( pObj, true ) );
}

/*
 * virtual bool isEmpty () const
 */
HB_FUNC( QT_QTRANSLATOR_ISEMPTY )
{
   hb_retl( hbqt_par_QTranslator( 1 )->isEmpty() );
}

/*
 * bool load ( const QString & filename, const QString & directory = QString(), const QString & search_delimiters = QString(), const QString & suffix = QString() )
 */
HB_FUNC( QT_QTRANSLATOR_LOAD )
{
   hb_retl( hbqt_par_QTranslator( 1 )->load( QTranslator::tr( hb_parc( 2 ) ), QTranslator::tr( hb_parc( 3 ) ), QTranslator::tr( hb_parc( 4 ) ), QTranslator::tr( hb_parc( 5 ) ) ) );
}

/*
 * virtual QString translate ( const char * context, const char * sourceText, const char * disambiguation = 0 ) const
 */
HB_FUNC( QT_QTRANSLATOR_TRANSLATE )
{
   hb_retc( hbqt_par_QTranslator( 1 )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ) ).toAscii().data() );
}

/*
 * QString translate ( const char * context, const char * sourceText, const char * disambiguation, int n ) const
 */
HB_FUNC( QT_QTRANSLATOR_TRANSLATE_1 )
{
   hb_retc( hbqt_par_QTranslator( 1 )->translate( hbqt_par_char( 2 ), hbqt_par_char( 3 ), hbqt_par_char( 4 ), hb_parni( 5 ) ).toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
