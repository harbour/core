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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextDocument;

HBQT_GC_FUNC( hbqt_gcRelease_QTextDocument )
{
   QTextDocument  * ph = NULL ;
   HBQT_GC_T_QTextDocument * p = ( HBQT_GC_T_QTextDocument * ) Cargo;

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
   HBQT_GC_T_QTextDocument * p = ( HBQT_GC_T_QTextDocument * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTextDocument ), hbqt_gcFuncs() );

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

   pObj = new QTextDocument( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( void * ) pObj, true ) );
}

/*
 * void addResource ( int type, const QUrl & name, const QVariant & resource )
 */
HB_FUNC( QT_QTEXTDOCUMENT_ADDRESOURCE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->addResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ), *hbqt_par_QVariant( 4 ) );
   }
}

/*
 * void adjustSize ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_ADJUSTSIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->adjustSize();
   }
}

/*
 * QTextBlock begin () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_BEGIN )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->begin() ), true ) );
   }
}

/*
 * int blockCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_BLOCKCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retni( ( p )->blockCount() );
   }
}

/*
 * QChar characterAt ( int pos ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CHARACTERAT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->characterAt( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * int characterCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CHARACTERCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retni( ( p )->characterCount() );
   }
}

/*
 * virtual void clear ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_CLEAR )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * QTextDocument * clone ( QObject * parent = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CLONE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->clone( hbqt_par_QObject( 2 ) ), false ) );
   }
}

/*
 * QFont defaultFont () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTFONT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont() ), true ) );
   }
}

/*
 * QString defaultStyleSheet () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTSTYLESHEET )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->defaultStyleSheet().toUtf8().data() );
   }
}

/*
 * QTextOption defaultTextOption () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTTEXTOPTION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextOption( new QTextOption( ( p )->defaultTextOption() ), true ) );
   }
}

/*
 * QAbstractTextDocumentLayout * documentLayout () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DOCUMENTLAYOUT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractTextDocumentLayout( ( p )->documentLayout(), false ) );
   }
}

/*
 * qreal documentMargin () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DOCUMENTMARGIN )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retnd( ( p )->documentMargin() );
   }
}

/*
 * void drawContents ( QPainter * p, const QRectF & rect = QRectF() )
 */
HB_FUNC( QT_QTEXTDOCUMENT_DRAWCONTENTS )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->drawContents( hbqt_par_QPainter( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QRectF( 3 ) : QRectF() ) );
   }
}

/*
 * QTextBlock end () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_END )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->end() ), true ) );
   }
}

/*
 * QTextCursor find ( const QString & subString, const QTextCursor & cursor, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QTextCursor find ( const QRegExp & expr, const QTextCursor & cursor, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_1 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( *hbqt_par_QRegExp( 2 ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
   }
}

/*
 * QTextCursor find ( const QString & subString, int position = 0, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_2 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QTextCursor find ( const QRegExp & expr, int position = 0, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_3 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->find( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
   }
}

/*
 * QTextBlock findBlock ( int pos ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCK )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlock( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QTextBlock findBlockByLineNumber ( int lineNumber ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCKBYLINENUMBER )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlockByLineNumber( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QTextBlock findBlockByNumber ( int blockNumber ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCKBYNUMBER )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->findBlockByNumber( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QTextBlock firstBlock () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIRSTBLOCK )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->firstBlock() ), true ) );
   }
}

/*
 * qreal idealWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_IDEALWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retnd( ( p )->idealWidth() );
   }
}

/*
 * qreal indentWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_INDENTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retnd( ( p )->indentWidth() );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISEMPTY )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISMODIFIED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retl( ( p )->isModified() );
   }
}

/*
 * bool isRedoAvailable () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISREDOAVAILABLE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retl( ( p )->isRedoAvailable() );
   }
}

/*
 * bool isUndoAvailable () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISUNDOAVAILABLE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retl( ( p )->isUndoAvailable() );
   }
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISUNDOREDOENABLED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retl( ( p )->isUndoRedoEnabled() );
   }
}

/*
 * QTextBlock lastBlock () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_LASTBLOCK )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->lastBlock() ), true ) );
   }
}

/*
 * int lineCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_LINECOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retni( ( p )->lineCount() );
   }
}

/*
 * void markContentsDirty ( int position, int length )
 */
HB_FUNC( QT_QTEXTDOCUMENT_MARKCONTENTSDIRTY )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->markContentsDirty( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * int maximumBlockCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_MAXIMUMBLOCKCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retni( ( p )->maximumBlockCount() );
   }
}

/*
 * QString metaInformation ( MetaInformation info ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_METAINFORMATION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->metaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QTextObject * object ( int objectIndex ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_OBJECT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextObject( ( p )->object( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QTextObject * objectForFormat ( const QTextFormat & f ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_OBJECTFORFORMAT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextObject( ( p )->objectForFormat( *hbqt_par_QTextFormat( 2 ) ), false ) );
   }
}

/*
 * int pageCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PAGECOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retni( ( p )->pageCount() );
   }
}

/*
 * QSizeF pageSize () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PAGESIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->pageSize() ), true ) );
   }
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PRINT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->print( hbqt_par_QPrinter( 2 ) );
   }
}

/*
 * void redo ( QTextCursor * cursor )
 */
HB_FUNC( QT_QTEXTDOCUMENT_REDO )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->redo( hbqt_par_QTextCursor( 2 ) );
   }
}

/*
 * QVariant resource ( int type, const QUrl & name ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_RESOURCE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->resource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
   }
}

/*
 * int revision () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_REVISION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retni( ( p )->revision() );
   }
}

/*
 * QTextFrame * rootFrame () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ROOTFRAME )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->rootFrame(), false ) );
   }
}

/*
 * void setDefaultFont ( const QFont & font )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTFONT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setDefaultFont( *hbqt_par_QFont( 2 ) );
   }
}

/*
 * void setDefaultStyleSheet ( const QString & sheet )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTSTYLESHEET )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDefaultStyleSheet( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setDefaultTextOption ( const QTextOption & option )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTTEXTOPTION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setDefaultTextOption( *hbqt_par_QTextOption( 2 ) );
   }
}

/*
 * void setDocumentLayout ( QAbstractTextDocumentLayout * layout )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDOCUMENTLAYOUT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setDocumentLayout( hbqt_par_QAbstractTextDocumentLayout( 2 ) );
   }
}

/*
 * void setDocumentMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDOCUMENTMARGIN )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setDocumentMargin( hb_parnd( 2 ) );
   }
}

/*
 * void setHtml ( const QString & html )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETHTML )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHtml( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setIndentWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETINDENTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setIndentWidth( hb_parnd( 2 ) );
   }
}

/*
 * void setMaximumBlockCount ( int maximum )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMAXIMUMBLOCKCOUNT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setMaximumBlockCount( hb_parni( 2 ) );
   }
}

/*
 * void setMetaInformation ( MetaInformation info, const QString & string )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMETAINFORMATION )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      void * pText;
      ( p )->setMetaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setPageSize ( const QSizeF & size )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETPAGESIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setPageSize( *hbqt_par_QSizeF( 2 ) );
   }
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETPLAINTEXT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPlainText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setTextWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETTEXTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setTextWidth( hb_parnd( 2 ) );
   }
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETUNDOREDOENABLED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setUndoRedoEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setUseDesignMetrics ( bool b )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETUSEDESIGNMETRICS )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setUseDesignMetrics( hb_parl( 2 ) );
   }
}

/*
 * QSizeF size () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_SIZE )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->size() ), true ) );
   }
}

/*
 * qreal textWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TEXTWIDTH )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retnd( ( p )->textWidth() );
   }
}

/*
 * QString toHtml ( const QByteArray & encoding = QByteArray() ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TOHTML )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toHtml( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QByteArray( 2 ) : QByteArray() ) ).toUtf8().data() );
   }
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TOPLAINTEXT )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toPlainText().toUtf8().data() );
   }
}

/*
 * void undo ( QTextCursor * cursor )
 */
HB_FUNC( QT_QTEXTDOCUMENT_UNDO )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->undo( hbqt_par_QTextCursor( 2 ) );
   }
}

/*
 * bool useDesignMetrics () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_USEDESIGNMETRICS )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      hb_retl( ( p )->useDesignMetrics() );
   }
}

/*
 * void redo ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_REDO_1 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->redo();
   }
}

/*
 * void setModified ( bool m = true )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMODIFIED )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->setModified( hb_parl( 2 ) );
   }
}

/*
 * void undo ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_UNDO_1 )
{
   QTextDocument * p = hbqt_par_QTextDocument( 1 );
   if( p )
   {
      ( p )->undo();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
