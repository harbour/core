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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // HCURSOR_or_HANDLE handle () const
 */

#include <QtCore/QPointer>

#include <QtGui/QPixmap>
#include <QtGui/QCursor>
#include <QtGui/QBitmap>

/*
 * QCursor ()
 * QCursor ( Qt::CursorShape shape )
 * QCursor ( const QBitmap & bitmap, const QBitmap & mask, int hotX = -1, int hotY = -1 )
 * QCursor ( const QPixmap & pixmap, int hotX = -1, int hotY = -1 )
 * QCursor ( const QCursor & c )
 * QCursor ( HCURSOR cursor )
 * QCursor ( Qt::HANDLE handle )
 * ~QCursor ()
 */

typedef struct
{
   QCursor * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCursor;

HBQT_GC_FUNC( hbqt_gcRelease_QCursor )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QCursor * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QCursor( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QCursor * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCursor;
   p->type = HBQT_TYPE_QCursor;

   return p;
}

HB_FUNC( QT_QCURSOR )
{
   QCursor * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new QCursor( ( Qt::CursorShape ) hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QCursor( *hbqt_par_QCursor( 1 ) ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = hbqt_par_QString( 1 );

      if( objName == ( QString ) "QPixmap" )
      {
         pObj = new QCursor( *hbqt_par_QPixmap( 2 ), HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1, HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ;
      }
      else
      {
         pObj = new QCursor() ;
      }
   }
   else if( hb_pcount() >= 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QCursor( *hbqt_par_QBitmap( 1 ), *hbqt_par_QBitmap( 2 ), HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1, HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ;
   }
   else
   {
      pObj = new QCursor() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QCursor( ( void * ) pObj, true ) );
}

/* const QBitmap * bitmap () const */
HB_FUNC( QT_QCURSOR_BITMAP )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( *( ( p )->bitmap() ) ), true ) );
}

/* QPoint hotSpot () const */
HB_FUNC( QT_QCURSOR_HOTSPOT )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->hotSpot() ), true ) );
}

/* const QBitmap * mask () const */
HB_FUNC( QT_QCURSOR_MASK )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( *( ( p )->mask() ) ), true ) );
}

/* QPixmap pixmap () const */
HB_FUNC( QT_QCURSOR_PIXMAP )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
}

/* void setShape ( Qt::CursorShape shape ) */
HB_FUNC( QT_QCURSOR_SETSHAPE )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      ( p )->setShape( ( Qt::CursorShape ) hb_parni( 2 ) );
}

/* Qt::CursorShape shape () const */
HB_FUNC( QT_QCURSOR_SHAPE )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      hb_retni( ( Qt::CursorShape ) ( p )->shape() );
}

/* QPoint pos () */
HB_FUNC( QT_QCURSOR_POS )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* void setPos ( int x, int y ) */
HB_FUNC( QT_QCURSOR_SETPOS )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      ( p )->setPos( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setPos ( const QPoint & p ) */
HB_FUNC( QT_QCURSOR_SETPOS_1 )
{
   QCursor * p = hbqt_par_QCursor( 1 );
   if( p )
      ( p )->setPos( *hbqt_par_QPoint( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
