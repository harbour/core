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
 *  Constructed[ 11/11 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QDrag>
#include <QtGui/QPixmap>
#include <QtCore/QPoint>


/*
 * QDrag ( QWidget * dragSource )
 * ~QDrag ()
 */

typedef struct
{
   QPointer< QDrag > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDrag;

HBQT_GC_FUNC( hbqt_gcRelease_QDrag )
{
   HBQT_GC_T_QDrag * p = ( HBQT_GC_T_QDrag * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QDrag * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDrag( void * pObj, bool bNew )
{
   HBQT_GC_T_QDrag * p = ( HBQT_GC_T_QDrag * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDrag ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDrag >( ( QDrag * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDrag;
   p->type = HBQT_TYPE_QDrag;

   return p;
}

HB_FUNC( QT_QDRAG )
{
   QDrag * pObj = NULL;

   pObj = new QDrag( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDrag( ( void * ) pObj, true ) );
}

/* Qt::DropAction exec ( Qt::DropActions supportedActions = Qt::MoveAction ) */
HB_FUNC( QT_QDRAG_EXEC )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->exec( ( HB_ISNUM( 2 ) ? ( Qt::DropActions ) hb_parni( 2 ) : ( Qt::DropActions ) Qt::MoveAction ) ) );
}

/* Qt::DropAction exec ( Qt::DropActions supportedActions, Qt::DropAction defaultDropAction ) */
HB_FUNC( QT_QDRAG_EXEC_1 )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->exec( ( Qt::DropActions ) hb_parni( 2 ), ( Qt::DropAction ) hb_parni( 3 ) ) );
}

/* QPoint hotSpot () const */
HB_FUNC( QT_QDRAG_HOTSPOT )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->hotSpot() ), true ) );
}

/* QMimeData * mimeData () const */
HB_FUNC( QT_QDRAG_MIMEDATA )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMimeData( ( p )->mimeData(), false ) );
}

/* QPixmap pixmap () const */
HB_FUNC( QT_QDRAG_PIXMAP )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
}

/* void setDragCursor ( const QPixmap & cursor, Qt::DropAction action ) */
HB_FUNC( QT_QDRAG_SETDRAGCURSOR )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      ( p )->setDragCursor( *hbqt_par_QPixmap( 2 ), ( Qt::DropAction ) hb_parni( 3 ) );
}

/* void setHotSpot ( const QPoint & hotspot ) */
HB_FUNC( QT_QDRAG_SETHOTSPOT )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      ( p )->setHotSpot( *hbqt_par_QPoint( 2 ) );
}

/* void setMimeData ( QMimeData * data )   [*D=1*] */
HB_FUNC( QT_QDRAG_SETMIMEDATA )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->setMimeData( hbqt_par_QMimeData( 2 ) );
   }
}

/* void setPixmap ( const QPixmap & pixmap ) */
HB_FUNC( QT_QDRAG_SETPIXMAP )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ) );
}

/* QWidget * source () const */
HB_FUNC( QT_QDRAG_SOURCE )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->source(), false ) );
}

/* QWidget * target () const */
HB_FUNC( QT_QDRAG_TARGET )
{
   QDrag * p = hbqt_par_QDrag( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->target(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
