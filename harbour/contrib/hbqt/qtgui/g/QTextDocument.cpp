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
 *  enum FindFlag { FindBackward, FindCaseSensitively, FindWholeWords }
 *  flags FindFlags
 *  enum MetaInformation { DocumentTitle, DocumentUrl }
 *  enum ResourceType { HtmlResource, ImageResource, StyleSheetResource, UserResource }
 */

/*
 *  Constructed[ 67/68 [ 98.53% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QVector<QTextFormat> allFormats () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextDocument>
#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>


/*
 * QTextDocument ( QObject * parent = 0 )
 * QTextDocument ( const QString & text, QObject * parent = 0 )
 * ~QTextDocument ()
 */

typedef struct
{
   QPointer< QTextDocument > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextDocument;

QT_G_FUNC( hbqt_gcRelease_QTextDocument )
{
   QTextDocument  * ph = NULL ;
   QGC_POINTER_QTextDocument * p = ( QGC_POINTER_QTextDocument * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextDocument   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextDocument   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTextDocument          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextDocument    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextDocument    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextDocument( void * pObj, bool bNew )
{
   QGC_POINTER_QTextDocument * p = ( QGC_POINTER_QTextDocument * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextDocument ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextDocument >( ( QTextDocument * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextDocument;
   p->type = HBQT_TYPE_QTextDocument;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextDocument  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextDocument", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTDOCUMENT )
{
   QTextDocument * pObj = NULL;

   pObj =  new QTextDocument( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( void * ) pObj, true ) );
}

/*
 * void addResource ( int type, const QUrl & name, const QVariant & resource )
 */
HB_FUNC( QT_QTEXTDOCUMENT_ADDRESOURCE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->addResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ), *hbqt_par_QVariant( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ADDRESOURCE FP=( p )->addResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ), *hbqt_par_QVariant( 4 ) ); p is NULL" ) );
   }
}

/*
 * void adjustSize ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_ADJUSTSIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->adjustSize();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ADJUSTSIZE FP=( p )->adjustSize(); p is NULL" ) );
   }
}

/*
 * QTextBlock begin () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_BEGIN )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->begin() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_BEGIN FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->begin() ), true ) ); p is NULL" ) );
   }
}

/*
 * int blockCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_BLOCKCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retni( ( p )->blockCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_BLOCKCOUNT FP=hb_retni( ( p )->blockCount() ); p is NULL" ) );
   }
}

/*
 * QChar characterAt ( int pos ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CHARACTERAT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->characterAt( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_CHARACTERAT FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->characterAt( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int characterCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CHARACTERCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retni( ( p )->characterCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_CHARACTERCOUNT FP=hb_retni( ( p )->characterCount() ); p is NULL" ) );
   }
}

/*
 * virtual void clear ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_CLEAR )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * QTextDocument * clone ( QObject * parent = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CLONE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->clone( hbqt_par_QObject( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_CLONE FP=hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->clone( hbqt_par_QObject( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QFont defaultFont () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTFONT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_DEFAULTFONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString defaultStyleSheet () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTSTYLESHEET )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retc( ( p )->defaultStyleSheet().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_DEFAULTSTYLESHEET FP=hb_retc( ( p )->defaultStyleSheet().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextOption defaultTextOption () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTTEXTOPTION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextOption( new QTextOption( ( p )->defaultTextOption() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_DEFAULTTEXTOPTION FP=hb_retptrGC( hbqt_gcAllocate_QTextOption( new QTextOption( ( p )->defaultTextOption() ), true ) ); p is NULL" ) );
   }
}

/*
 * QAbstractTextDocumentLayout * documentLayout () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DOCUMENTLAYOUT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractTextDocumentLayout( ( p )->documentLayout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_DOCUMENTLAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QAbstractTextDocumentLayout( ( p )->documentLayout(), false ) ); p is NULL" ) );
   }
}

/*
 * qreal documentMargin () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DOCUMENTMARGIN )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retnd( ( p )->documentMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_DOCUMENTMARGIN FP=hb_retnd( ( p )->documentMargin() ); p is NULL" ) );
   }
}

/*
 * void drawContents ( QPainter * p, const QRectF & rect = QRectF() )
 */
HB_FUNC( QT_QTEXTDOCUMENT_DRAWCONTENTS )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->drawContents( hbqt_par_QPainter( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QRectF( 3 ) : QRectF() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_DRAWCONTENTS FP=( p )->drawContents( hbqt_par_QPainter( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QRectF( 3 ) : QRectF() ) ); p is NULL" ) );
   }
}

/*
 * QTextBlock end () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_END )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->end() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_END FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->end() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor find ( const QString & subString, const QTextCursor & cursor, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( QTextDocument::tr( hb_parc( 2 ) ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FIND FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( QTextDocument::tr( hb_parc( 2 ) ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor find ( const QRegExp & expr, const QTextCursor & cursor, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_1 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( *hbqt_par_QRegExp( 2 ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FIND_1 FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( *hbqt_par_QRegExp( 2 ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor find ( const QString & subString, int position = 0, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_2 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( QTextDocument::tr( hb_parc( 2 ) ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FIND_2 FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( QTextDocument::tr( hb_parc( 2 ) ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor find ( const QRegExp & expr, int position = 0, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_3 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FIND_3 FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextBlock findBlock ( int pos ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCK )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlock( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FINDBLOCK FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlock( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextBlock findBlockByLineNumber ( int lineNumber ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCKBYLINENUMBER )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlockByLineNumber( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FINDBLOCKBYLINENUMBER FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlockByLineNumber( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextBlock findBlockByNumber ( int blockNumber ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCKBYNUMBER )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlockByNumber( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FINDBLOCKBYNUMBER FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlockByNumber( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextBlock firstBlock () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIRSTBLOCK )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->firstBlock() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_FIRSTBLOCK FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->firstBlock() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal idealWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_IDEALWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retnd( ( p )->idealWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_IDEALWIDTH FP=hb_retnd( ( p )->idealWidth() ); p is NULL" ) );
   }
}

/*
 * qreal indentWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_INDENTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retnd( ( p )->indentWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_INDENTWIDTH FP=hb_retnd( ( p )->indentWidth() ); p is NULL" ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISEMPTY )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ISEMPTY FP=hb_retl( ( p )->isEmpty() ); p is NULL" ) );
   }
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISMODIFIED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ISMODIFIED FP=hb_retl( ( p )->isModified() ); p is NULL" ) );
   }
}

/*
 * bool isRedoAvailable () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISREDOAVAILABLE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retl( ( p )->isRedoAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ISREDOAVAILABLE FP=hb_retl( ( p )->isRedoAvailable() ); p is NULL" ) );
   }
}

/*
 * bool isUndoAvailable () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISUNDOAVAILABLE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retl( ( p )->isUndoAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ISUNDOAVAILABLE FP=hb_retl( ( p )->isUndoAvailable() ); p is NULL" ) );
   }
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISUNDOREDOENABLED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retl( ( p )->isUndoRedoEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ISUNDOREDOENABLED FP=hb_retl( ( p )->isUndoRedoEnabled() ); p is NULL" ) );
   }
}

/*
 * QTextBlock lastBlock () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_LASTBLOCK )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->lastBlock() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_LASTBLOCK FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->lastBlock() ), true ) ); p is NULL" ) );
   }
}

/*
 * int lineCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_LINECOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retni( ( p )->lineCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_LINECOUNT FP=hb_retni( ( p )->lineCount() ); p is NULL" ) );
   }
}

/*
 * void markContentsDirty ( int position, int length )
 */
HB_FUNC( QT_QTEXTDOCUMENT_MARKCONTENTSDIRTY )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->markContentsDirty( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_MARKCONTENTSDIRTY FP=( p )->markContentsDirty( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * int maximumBlockCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_MAXIMUMBLOCKCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retni( ( p )->maximumBlockCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_MAXIMUMBLOCKCOUNT FP=hb_retni( ( p )->maximumBlockCount() ); p is NULL" ) );
   }
}

/*
 * QString metaInformation ( MetaInformation info ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_METAINFORMATION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retc( ( p )->metaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_METAINFORMATION FP=hb_retc( ( p )->metaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextObject * object ( int objectIndex ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_OBJECT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextObject( ( p )->object( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_OBJECT FP=hb_retptrGC( hbqt_gcAllocate_QTextObject( ( p )->object( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextObject * objectForFormat ( const QTextFormat & f ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_OBJECTFORFORMAT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextObject( ( p )->objectForFormat( *hbqt_par_QTextFormat( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_OBJECTFORFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextObject( ( p )->objectForFormat( *hbqt_par_QTextFormat( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * int pageCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PAGECOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retni( ( p )->pageCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_PAGECOUNT FP=hb_retni( ( p )->pageCount() ); p is NULL" ) );
   }
}

/*
 * QSizeF pageSize () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PAGESIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->pageSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_PAGESIZE FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->pageSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PRINT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->print( hbqt_par_QPrinter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_PRINT FP=( p )->print( hbqt_par_QPrinter( 2 ) ); p is NULL" ) );
   }
}

/*
 * void redo ( QTextCursor * cursor )
 */
HB_FUNC( QT_QTEXTDOCUMENT_REDO )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->redo( hbqt_par_QTextCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_REDO FP=( p )->redo( hbqt_par_QTextCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * QVariant resource ( int type, const QUrl & name ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_RESOURCE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->resource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_RESOURCE FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->resource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int revision () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_REVISION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retni( ( p )->revision() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_REVISION FP=hb_retni( ( p )->revision() ); p is NULL" ) );
   }
}

/*
 * QTextFrame * rootFrame () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ROOTFRAME )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->rootFrame(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_ROOTFRAME FP=hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->rootFrame(), false ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultFont ( const QFont & font )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTFONT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setDefaultFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETDEFAULTFONT FP=( p )->setDefaultFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultStyleSheet ( const QString & sheet )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTSTYLESHEET )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setDefaultStyleSheet( QTextDocument::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETDEFAULTSTYLESHEET FP=( p )->setDefaultStyleSheet( QTextDocument::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultTextOption ( const QTextOption & option )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTTEXTOPTION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setDefaultTextOption( *hbqt_par_QTextOption( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETDEFAULTTEXTOPTION FP=( p )->setDefaultTextOption( *hbqt_par_QTextOption( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocumentLayout ( QAbstractTextDocumentLayout * layout )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDOCUMENTLAYOUT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setDocumentLayout( hbqt_par_QAbstractTextDocumentLayout( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETDOCUMENTLAYOUT FP=( p )->setDocumentLayout( hbqt_par_QAbstractTextDocumentLayout( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocumentMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDOCUMENTMARGIN )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setDocumentMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETDOCUMENTMARGIN FP=( p )->setDocumentMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHtml ( const QString & html )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETHTML )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setHtml( QTextDocument::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETHTML FP=( p )->setHtml( QTextDocument::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setIndentWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETINDENTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setIndentWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETINDENTWIDTH FP=( p )->setIndentWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumBlockCount ( int maximum )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMAXIMUMBLOCKCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setMaximumBlockCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETMAXIMUMBLOCKCOUNT FP=( p )->setMaximumBlockCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMetaInformation ( MetaInformation info, const QString & string )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMETAINFORMATION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setMetaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ), QTextDocument::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETMETAINFORMATION FP=( p )->setMetaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ), QTextDocument::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setPageSize ( const QSizeF & size )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETPAGESIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setPageSize( *hbqt_par_QSizeF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETPAGESIZE FP=( p )->setPageSize( *hbqt_par_QSizeF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETPLAINTEXT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setPlainText( QTextDocument::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETPLAINTEXT FP=( p )->setPlainText( QTextDocument::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setTextWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETTEXTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setTextWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETTEXTWIDTH FP=( p )->setTextWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETUNDOREDOENABLED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setUndoRedoEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETUNDOREDOENABLED FP=( p )->setUndoRedoEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUseDesignMetrics ( bool b )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETUSEDESIGNMETRICS )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setUseDesignMetrics( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETUSEDESIGNMETRICS FP=( p )->setUseDesignMetrics( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSizeF size () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_SIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->size() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SIZE FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->size() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal textWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TEXTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retnd( ( p )->textWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_TEXTWIDTH FP=hb_retnd( ( p )->textWidth() ); p is NULL" ) );
   }
}

/*
 * QString toHtml ( const QByteArray & encoding = QByteArray() ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TOHTML )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retc( ( p )->toHtml( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QByteArray( 2 ) : QByteArray() ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_TOHTML FP=hb_retc( ( p )->toHtml( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QByteArray( 2 ) : QByteArray() ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TOPLAINTEXT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retc( ( p )->toPlainText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_TOPLAINTEXT FP=hb_retc( ( p )->toPlainText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void undo ( QTextCursor * cursor )
 */
HB_FUNC( QT_QTEXTDOCUMENT_UNDO )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->undo( hbqt_par_QTextCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_UNDO FP=( p )->undo( hbqt_par_QTextCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool useDesignMetrics () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_USEDESIGNMETRICS )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      hb_retl( ( p )->useDesignMetrics() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_USEDESIGNMETRICS FP=hb_retl( ( p )->useDesignMetrics() ); p is NULL" ) );
   }
}

/*
 * void redo ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_REDO_1 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->redo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_REDO_1 FP=( p )->redo(); p is NULL" ) );
   }
}

/*
 * void setModified ( bool m = true )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMODIFIED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->setModified( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_SETMODIFIED FP=( p )->setModified( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void undo ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_UNDO_1 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
      ( p )->undo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTDOCUMENT_UNDO_1 FP=( p )->undo(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
