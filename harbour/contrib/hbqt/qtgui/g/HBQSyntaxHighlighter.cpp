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
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSyntaxHighlighter>
#include "hbqt_hbqsyntaxhighlighter.h"

/*
 *
 *
 */

typedef struct
{
   QPointer< HBQSyntaxHighlighter > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQSyntaxHighlighter;

HBQT_GC_FUNC( hbqt_gcRelease_HBQSyntaxHighlighter )
{
   HBQSyntaxHighlighter  * ph = NULL ;
   HBQT_GC_T_HBQSyntaxHighlighter * p = ( HBQT_GC_T_HBQSyntaxHighlighter * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQSyntaxHighlighter   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQSyntaxHighlighter   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_HBQSyntaxHighlighter          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_HBQSyntaxHighlighter    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_HBQSyntaxHighlighter    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQSyntaxHighlighter( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQSyntaxHighlighter * p = ( HBQT_GC_T_HBQSyntaxHighlighter * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQSyntaxHighlighter ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQSyntaxHighlighter >( ( HBQSyntaxHighlighter * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQSyntaxHighlighter;
   p->type = HBQT_TYPE_HBQSyntaxHighlighter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_HBQSyntaxHighlighter  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_HBQSyntaxHighlighter", pObj ) );
   }
   return p;
}

HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER )
{
   HBQSyntaxHighlighter * pObj = NULL;

   pObj = new HBQSyntaxHighlighter( hbqt_par_QTextDocument( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_HBQSyntaxHighlighter( ( void * ) pObj, true ) );
}

/*
 * void hbSetMultiLineCommentFormat( const QTextCharFormat & format )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETMULTILINECOMMENTFORMAT )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      ( p )->hbSetMultiLineCommentFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * void hbSetSingleLineCommentFormat( const QTextCharFormat & format )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETSINGLELINECOMMENTFORMAT )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      ( p )->hbSetSingleLineCommentFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * void hbSetRule( QString name, QString pattern, QTextCharFormat & format )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETRULE )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbSetRule( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), *hbqt_par_QTextCharFormat( 4 ) );
      hb_strfree( pText );
   }
}

/*
 * void hbSetFormat( QString name, const QTextCharFormat & format )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETFORMAT )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbSetFormat( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QTextCharFormat( 3 ) );
      hb_strfree( pText );
   }
}

/*
 * void hbSetFormatColumnSelection( int start, int count, const QColor & color )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETFORMATCOLUMNSELECTION )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      ( p )->hbSetFormatColumnSelection( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QColor( 4 ) );
   }
}

/*
 * void hbSetRuleWithRegExp( QString name, const QRegExp & reg, const QTextCharFormat & format )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETRULEWITHREGEXP )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbSetRuleWithRegExp( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QRegExp( 3 ), *hbqt_par_QTextCharFormat( 4 ) );
      hb_strfree( pText );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
