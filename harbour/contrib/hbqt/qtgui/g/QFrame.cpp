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
 *  enum Shadow { Plain, Raised, Sunken }
 *  enum Shape { NoFrame, Box, Panel, StyledPanel, ..., WinPanel }
 *  enum StyleMask { Shadow_Mask, Shape_Mask }
 */

/*
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QFrame>


/*
 * QFrame ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QFrame ()
 */

typedef struct
{
   QPointer< QFrame > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFrame;

HBQT_GC_FUNC( hbqt_gcRelease_QFrame )
{
   HBQT_GC_T_QFrame * p = ( HBQT_GC_T_QFrame * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QFrame * ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QFrame( void * pObj, bool bNew )
{
   HBQT_GC_T_QFrame * p = ( HBQT_GC_T_QFrame * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFrame ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFrame >( ( QFrame * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFrame;
   p->type = HBQT_TYPE_QFrame;

   return p;
}

HB_FUNC( QT_QFRAME )
{
   QFrame * pObj = NULL;

   pObj = new QFrame( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFrame( ( void * ) pObj, true ) );
}

/* QRect frameRect () const */
HB_FUNC( QT_QFRAME_FRAMERECT )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->frameRect() ), true ) );
}

/* Shadow frameShadow () const */
HB_FUNC( QT_QFRAME_FRAMESHADOW )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      hb_retni( ( QFrame::Shadow ) ( p )->frameShadow() );
}

/* Shape frameShape () const */
HB_FUNC( QT_QFRAME_FRAMESHAPE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      hb_retni( ( QFrame::Shape ) ( p )->frameShape() );
}

/* int frameStyle () const */
HB_FUNC( QT_QFRAME_FRAMESTYLE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      hb_retni( ( p )->frameStyle() );
}

/* int frameWidth () const */
HB_FUNC( QT_QFRAME_FRAMEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      hb_retni( ( p )->frameWidth() );
}

/* int lineWidth () const */
HB_FUNC( QT_QFRAME_LINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      hb_retni( ( p )->lineWidth() );
}

/* int midLineWidth () const */
HB_FUNC( QT_QFRAME_MIDLINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      hb_retni( ( p )->midLineWidth() );
}

/* void setFrameRect ( const QRect & ) */
HB_FUNC( QT_QFRAME_SETFRAMERECT )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      ( p )->setFrameRect( *hbqt_par_QRect( 2 ) );
}

/* void setFrameShadow ( Shadow ) */
HB_FUNC( QT_QFRAME_SETFRAMESHADOW )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      ( p )->setFrameShadow( ( QFrame::Shadow ) hb_parni( 2 ) );
}

/* void setFrameShape ( Shape ) */
HB_FUNC( QT_QFRAME_SETFRAMESHAPE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      ( p )->setFrameShape( ( QFrame::Shape ) hb_parni( 2 ) );
}

/* void setFrameStyle ( int style ) */
HB_FUNC( QT_QFRAME_SETFRAMESTYLE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      ( p )->setFrameStyle( hb_parni( 2 ) );
}

/* void setLineWidth ( int ) */
HB_FUNC( QT_QFRAME_SETLINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      ( p )->setLineWidth( hb_parni( 2 ) );
}

/* void setMidLineWidth ( int ) */
HB_FUNC( QT_QFRAME_SETMIDLINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
      ( p )->setMidLineWidth( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
