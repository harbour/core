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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QTextDocument > pq;
} QGC_POINTER_QTextDocument;

QT_G_FUNC( hbqt_gcRelease_QTextDocument )
{
   QGC_POINTER_QTextDocument * p = ( QGC_POINTER_QTextDocument * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QTextDocument * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextDocument              ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QTextDocument              ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextDocument               Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextDocument               Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextDocument( void * pObj, bool bNew )
{
   QGC_POINTER_QTextDocument * p = ( QGC_POINTER_QTextDocument * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextDocument ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextDocument;

   if( bNew )
   {
      new( & p->pq ) QPointer< QTextDocument >( ( QTextDocument * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextDocument              ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTDOCUMENT )
{
   void * pObj = NULL;

   pObj = ( QTextDocument* ) new QTextDocument( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextDocument( pObj, true ) );
}

/*
 * void addResource ( int type, const QUrl & name, const QVariant & resource )
 */
HB_FUNC( QT_QTEXTDOCUMENT_ADDRESOURCE )
{
   hbqt_par_QTextDocument( 1 )->addResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ), *hbqt_par_QVariant( 4 ) );
}

/*
 * void adjustSize ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_ADJUSTSIZE )
{
   hbqt_par_QTextDocument( 1 )->adjustSize();
}

/*
 * QTextBlock begin () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_BEGIN )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextDocument( 1 )->begin() ), true ) );
}

/*
 * int blockCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_BLOCKCOUNT )
{
   hb_retni( hbqt_par_QTextDocument( 1 )->blockCount() );
}

/*
 * QChar characterAt ( int pos ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CHARACTERAT )
{
   hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( hbqt_par_QTextDocument( 1 )->characterAt( hb_parni( 2 ) ) ), true ) );
}

/*
 * int characterCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CHARACTERCOUNT )
{
   hb_retni( hbqt_par_QTextDocument( 1 )->characterCount() );
}

/*
 * virtual void clear ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_CLEAR )
{
   hbqt_par_QTextDocument( 1 )->clear();
}

/*
 * QTextDocument * clone ( QObject * parent = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_CLONE )
{
   hb_retptrGC( hbqt_gcAllocate_QTextDocument( hbqt_par_QTextDocument( 1 )->clone( hbqt_par_QObject( 2 ) ), false ) );
}

/*
 * QFont defaultFont () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTFONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QTextDocument( 1 )->defaultFont() ), true ) );
}

/*
 * QString defaultStyleSheet () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTSTYLESHEET )
{
   hb_retc( hbqt_par_QTextDocument( 1 )->defaultStyleSheet().toAscii().data() );
}

/*
 * QTextOption defaultTextOption () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DEFAULTTEXTOPTION )
{
   hb_retptrGC( hbqt_gcAllocate_QTextOption( new QTextOption( hbqt_par_QTextDocument( 1 )->defaultTextOption() ), true ) );
}

/*
 * QAbstractTextDocumentLayout * documentLayout () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DOCUMENTLAYOUT )
{
   hb_retptrGC( hbqt_gcAllocate_QAbstractTextDocumentLayout( hbqt_par_QTextDocument( 1 )->documentLayout(), false ) );
}

/*
 * qreal documentMargin () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_DOCUMENTMARGIN )
{
   hb_retnd( hbqt_par_QTextDocument( 1 )->documentMargin() );
}

/*
 * void drawContents ( QPainter * p, const QRectF & rect = QRectF() )
 */
HB_FUNC( QT_QTEXTDOCUMENT_DRAWCONTENTS )
{
   hbqt_par_QTextDocument( 1 )->drawContents( hbqt_par_QPainter( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QRectF( 3 ) : QRectF() ) );
}

/*
 * QTextBlock end () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_END )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextDocument( 1 )->end() ), true ) );
}

/*
 * QTextCursor find ( const QString & subString, const QTextCursor & cursor, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextDocument( 1 )->find( QTextDocument::tr( hb_parc( 2 ) ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
}

/*
 * QTextCursor find ( const QRegExp & expr, const QTextCursor & cursor, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextDocument( 1 )->find( *hbqt_par_QRegExp( 2 ), *hbqt_par_QTextCursor( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
}

/*
 * QTextCursor find ( const QString & subString, int position = 0, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextDocument( 1 )->find( QTextDocument::tr( hb_parc( 2 ) ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
}

/*
 * QTextCursor find ( const QRegExp & expr, int position = 0, FindFlags options = 0 ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIND_3 )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextDocument( 1 )->find( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ), ( QTextDocument::FindFlags ) hb_parni( 4 ) ) ), true ) );
}

/*
 * QTextBlock findBlock ( int pos ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCK )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextDocument( 1 )->findBlock( hb_parni( 2 ) ) ), true ) );
}

/*
 * QTextBlock findBlockByLineNumber ( int lineNumber ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCKBYLINENUMBER )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextDocument( 1 )->findBlockByLineNumber( hb_parni( 2 ) ) ), true ) );
}

/*
 * QTextBlock findBlockByNumber ( int blockNumber ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FINDBLOCKBYNUMBER )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextDocument( 1 )->findBlockByNumber( hb_parni( 2 ) ) ), true ) );
}

/*
 * QTextBlock firstBlock () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_FIRSTBLOCK )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextDocument( 1 )->firstBlock() ), true ) );
}

/*
 * qreal idealWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_IDEALWIDTH )
{
   hb_retnd( hbqt_par_QTextDocument( 1 )->idealWidth() );
}

/*
 * qreal indentWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_INDENTWIDTH )
{
   hb_retnd( hbqt_par_QTextDocument( 1 )->indentWidth() );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISEMPTY )
{
   hb_retl( hbqt_par_QTextDocument( 1 )->isEmpty() );
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISMODIFIED )
{
   hb_retl( hbqt_par_QTextDocument( 1 )->isModified() );
}

/*
 * bool isRedoAvailable () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISREDOAVAILABLE )
{
   hb_retl( hbqt_par_QTextDocument( 1 )->isRedoAvailable() );
}

/*
 * bool isUndoAvailable () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISUNDOAVAILABLE )
{
   hb_retl( hbqt_par_QTextDocument( 1 )->isUndoAvailable() );
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ISUNDOREDOENABLED )
{
   hb_retl( hbqt_par_QTextDocument( 1 )->isUndoRedoEnabled() );
}

/*
 * QTextBlock lastBlock () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_LASTBLOCK )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextDocument( 1 )->lastBlock() ), true ) );
}

/*
 * int lineCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_LINECOUNT )
{
   hb_retni( hbqt_par_QTextDocument( 1 )->lineCount() );
}

/*
 * void markContentsDirty ( int position, int length )
 */
HB_FUNC( QT_QTEXTDOCUMENT_MARKCONTENTSDIRTY )
{
   hbqt_par_QTextDocument( 1 )->markContentsDirty( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * int maximumBlockCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_MAXIMUMBLOCKCOUNT )
{
   hb_retni( hbqt_par_QTextDocument( 1 )->maximumBlockCount() );
}

/*
 * QString metaInformation ( MetaInformation info ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_METAINFORMATION )
{
   hb_retc( hbqt_par_QTextDocument( 1 )->metaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QTextObject * object ( int objectIndex ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_OBJECT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextObject( hbqt_par_QTextDocument( 1 )->object( hb_parni( 2 ) ), false ) );
}

/*
 * QTextObject * objectForFormat ( const QTextFormat & f ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_OBJECTFORFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextObject( hbqt_par_QTextDocument( 1 )->objectForFormat( *hbqt_par_QTextFormat( 2 ) ), false ) );
}

/*
 * int pageCount () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PAGECOUNT )
{
   hb_retni( hbqt_par_QTextDocument( 1 )->pageCount() );
}

/*
 * QSizeF pageSize () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PAGESIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( hbqt_par_QTextDocument( 1 )->pageSize() ), true ) );
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_PRINT )
{
   hbqt_par_QTextDocument( 1 )->print( hbqt_par_QPrinter( 2 ) );
}

/*
 * void redo ( QTextCursor * cursor )
 */
HB_FUNC( QT_QTEXTDOCUMENT_REDO )
{
   hbqt_par_QTextDocument( 1 )->redo( hbqt_par_QTextCursor( 2 ) );
}

/*
 * QVariant resource ( int type, const QUrl & name ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_RESOURCE )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QTextDocument( 1 )->resource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
}

/*
 * int revision () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_REVISION )
{
   hb_retni( hbqt_par_QTextDocument( 1 )->revision() );
}

/*
 * QTextFrame * rootFrame () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_ROOTFRAME )
{
   hb_retptrGC( hbqt_gcAllocate_QTextFrame( hbqt_par_QTextDocument( 1 )->rootFrame(), false ) );
}

/*
 * void setDefaultFont ( const QFont & font )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTFONT )
{
   hbqt_par_QTextDocument( 1 )->setDefaultFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setDefaultStyleSheet ( const QString & sheet )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTSTYLESHEET )
{
   hbqt_par_QTextDocument( 1 )->setDefaultStyleSheet( QTextDocument::tr( hb_parc( 2 ) ) );
}

/*
 * void setDefaultTextOption ( const QTextOption & option )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDEFAULTTEXTOPTION )
{
   hbqt_par_QTextDocument( 1 )->setDefaultTextOption( *hbqt_par_QTextOption( 2 ) );
}

/*
 * void setDocumentLayout ( QAbstractTextDocumentLayout * layout )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDOCUMENTLAYOUT )
{
   hbqt_par_QTextDocument( 1 )->setDocumentLayout( hbqt_par_QAbstractTextDocumentLayout( 2 ) );
}

/*
 * void setDocumentMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETDOCUMENTMARGIN )
{
   hbqt_par_QTextDocument( 1 )->setDocumentMargin( hb_parnd( 2 ) );
}

/*
 * void setHtml ( const QString & html )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETHTML )
{
   hbqt_par_QTextDocument( 1 )->setHtml( QTextDocument::tr( hb_parc( 2 ) ) );
}

/*
 * void setIndentWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETINDENTWIDTH )
{
   hbqt_par_QTextDocument( 1 )->setIndentWidth( hb_parnd( 2 ) );
}

/*
 * void setMaximumBlockCount ( int maximum )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMAXIMUMBLOCKCOUNT )
{
   hbqt_par_QTextDocument( 1 )->setMaximumBlockCount( hb_parni( 2 ) );
}

/*
 * void setMetaInformation ( MetaInformation info, const QString & string )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMETAINFORMATION )
{
   hbqt_par_QTextDocument( 1 )->setMetaInformation( ( QTextDocument::MetaInformation ) hb_parni( 2 ), QTextDocument::tr( hb_parc( 3 ) ) );
}

/*
 * void setPageSize ( const QSizeF & size )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETPAGESIZE )
{
   hbqt_par_QTextDocument( 1 )->setPageSize( *hbqt_par_QSizeF( 2 ) );
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETPLAINTEXT )
{
   hbqt_par_QTextDocument( 1 )->setPlainText( QTextDocument::tr( hb_parc( 2 ) ) );
}

/*
 * void setTextWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETTEXTWIDTH )
{
   hbqt_par_QTextDocument( 1 )->setTextWidth( hb_parnd( 2 ) );
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETUNDOREDOENABLED )
{
   hbqt_par_QTextDocument( 1 )->setUndoRedoEnabled( hb_parl( 2 ) );
}

/*
 * void setUseDesignMetrics ( bool b )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETUSEDESIGNMETRICS )
{
   hbqt_par_QTextDocument( 1 )->setUseDesignMetrics( hb_parl( 2 ) );
}

/*
 * QSizeF size () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_SIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( hbqt_par_QTextDocument( 1 )->size() ), true ) );
}

/*
 * qreal textWidth () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TEXTWIDTH )
{
   hb_retnd( hbqt_par_QTextDocument( 1 )->textWidth() );
}

/*
 * QString toHtml ( const QByteArray & encoding = QByteArray() ) const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TOHTML )
{
   hb_retc( hbqt_par_QTextDocument( 1 )->toHtml( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QByteArray( 2 ) : QByteArray() ) ).toAscii().data() );
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_TOPLAINTEXT )
{
   hb_retc( hbqt_par_QTextDocument( 1 )->toPlainText().toAscii().data() );
}

/*
 * void undo ( QTextCursor * cursor )
 */
HB_FUNC( QT_QTEXTDOCUMENT_UNDO )
{
   hbqt_par_QTextDocument( 1 )->undo( hbqt_par_QTextCursor( 2 ) );
}

/*
 * bool useDesignMetrics () const
 */
HB_FUNC( QT_QTEXTDOCUMENT_USEDESIGNMETRICS )
{
   hb_retl( hbqt_par_QTextDocument( 1 )->useDesignMetrics() );
}

/*
 * void redo ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_REDO_1 )
{
   hbqt_par_QTextDocument( 1 )->redo();
}

/*
 * void setModified ( bool m = true )
 */
HB_FUNC( QT_QTEXTDOCUMENT_SETMODIFIED )
{
   hbqt_par_QTextDocument( 1 )->setModified( hb_parl( 2 ) );
}

/*
 * void undo ()
 */
HB_FUNC( QT_QTEXTDOCUMENT_UNDO_1 )
{
   hbqt_par_QTextDocument( 1 )->undo();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
