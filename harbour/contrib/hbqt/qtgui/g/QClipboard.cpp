/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum Mode { Clipboard, Selection, FindBuffer }
 */

/*
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //const QMimeData * mimeData ( Mode mode = Clipboard ) const
 *  // QString text ( QString & subtype, Mode mode = Clipboard ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QPalette>
#include <QtGui/QClipboard>
#include <QtGui/QApplication>
#include <QtCore/QMimeData>

/*
 * QApplication::clipboard()
 *
 */

typedef struct
{
   QPointer< QClipboard > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QClipboard;

HBQT_GC_FUNC( hbqt_gcRelease_QClipboard )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QClipboard( void * pObj, bool bNew )
{
   HBQT_GC_T_QClipboard * p = ( HBQT_GC_T_QClipboard * ) hb_gcAllocate( sizeof( HBQT_GC_T_QClipboard ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QClipboard >( ( QClipboard * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QClipboard;
   p->type = HBQT_TYPE_QClipboard;

   return p;
}

HB_FUNC( QT_QCLIPBOARD )
{
   QClipboard * pObj = NULL;

   pObj = QApplication::clipboard() ;

   hb_retptrGC( hbqt_gcAllocate_QClipboard( ( void * ) pObj, false ) );
}

/* void clear ( Mode mode = Clipboard ) */
HB_FUNC( QT_QCLIPBOARD_CLEAR )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->clear( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/* QImage image ( Mode mode = Clipboard ) const */
HB_FUNC( QT_QCLIPBOARD_IMAGE )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->image( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
}

/* bool ownsClipboard () const */
HB_FUNC( QT_QCLIPBOARD_OWNSCLIPBOARD )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->ownsClipboard() );
}

/* bool ownsFindBuffer () const */
HB_FUNC( QT_QCLIPBOARD_OWNSFINDBUFFER )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->ownsFindBuffer() );
}

/* bool ownsSelection () const */
HB_FUNC( QT_QCLIPBOARD_OWNSSELECTION )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->ownsSelection() );
}

/* QPixmap pixmap ( Mode mode = Clipboard ) const */
HB_FUNC( QT_QCLIPBOARD_PIXMAP )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ) ), true ) );
}

/* void setImage ( const QImage & image, Mode mode = Clipboard ) */
HB_FUNC( QT_QCLIPBOARD_SETIMAGE )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->setImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/* void setMimeData ( QMimeData * src, Mode mode = Clipboard ) */
HB_FUNC( QT_QCLIPBOARD_SETMIMEDATA )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->setMimeData( hbqt_par_QMimeData( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/* void setPixmap ( const QPixmap & pixmap, Mode mode = Clipboard ) */
HB_FUNC( QT_QCLIPBOARD_SETPIXMAP )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
}

/* void setText ( const QString & text, Mode mode = Clipboard ) */
HB_FUNC( QT_QCLIPBOARD_SETTEXT )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( QClipboard::Mode ) hb_parni( 3 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) );
      hb_strfree( pText );
   }
}

/* bool supportsFindBuffer () const */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSFINDBUFFER )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->supportsFindBuffer() );
}

/* bool supportsSelection () const */
HB_FUNC( QT_QCLIPBOARD_SUPPORTSSELECTION )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retl( ( p )->supportsSelection() );
}

/* QString text ( Mode mode = Clipboard ) const */
HB_FUNC( QT_QCLIPBOARD_TEXT )
{
   QClipboard * p = hbqt_par_QClipboard( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text( ( HB_ISNUM( 2 ) ? ( QClipboard::Mode ) hb_parni( 2 ) : ( QClipboard::Mode ) QClipboard::Clipboard ) ).toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
