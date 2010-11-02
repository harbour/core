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
 *  Constructed[ 20/25 [ 80.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QList<QPair<qreal, QPointF> > posList () const
 *  QList<QPair<qreal, qreal> > rotationList () const
 *  QList<QPair<qreal, QPointF> > scaleList () const
 *  QList<QPair<qreal, QPointF> > shearList () const
 *  QList<QPair<qreal, QPointF> > translationList () const
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsItemAnimation>
#include <QtGui/QMatrix>
#include <QtCore/QPointF>


/*
 * QGraphicsItemAnimation ( QObject * parent = 0 )
 * virtual ~QGraphicsItemAnimation ()
 */

typedef struct
{
   QPointer< QGraphicsItemAnimation > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsItemAnimation;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsItemAnimation )
{
   HBQT_GC_T_QGraphicsItemAnimation * p = ( HBQT_GC_T_QGraphicsItemAnimation * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QGraphicsItemAnimation * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsItemAnimation( void * pObj, bool bNew )
{
   HBQT_GC_T_QGraphicsItemAnimation * p = ( HBQT_GC_T_QGraphicsItemAnimation * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGraphicsItemAnimation ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGraphicsItemAnimation >( ( QGraphicsItemAnimation * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsItemAnimation;
   p->type = HBQT_TYPE_QGraphicsItemAnimation;

   return p;
}

HB_FUNC( QT_QGRAPHICSITEMANIMATION )
{
   QGraphicsItemAnimation * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QGraphicsItemAnimation( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QGraphicsItemAnimation() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsItemAnimation( ( void * ) pObj, true ) );
}

/* void clear () */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_CLEAR )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->clear();
}

/* qreal horizontalScaleAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_HORIZONTALSCALEAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retnd( ( p )->horizontalScaleAt( hb_parnd( 2 ) ) );
}

/* qreal horizontalShearAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_HORIZONTALSHEARAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retnd( ( p )->horizontalShearAt( hb_parnd( 2 ) ) );
}

/* QGraphicsItem * item () const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_ITEM )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->item(), false ) );
}

/* QMatrix matrixAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_MATRIXAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->matrixAt( hb_parnd( 2 ) ) ), true ) );
}

/* QPointF posAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_POSAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->posAt( hb_parnd( 2 ) ) ), true ) );
}

/* qreal rotationAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_ROTATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retnd( ( p )->rotationAt( hb_parnd( 2 ) ) );
}

/* void setItem ( QGraphicsItem * item ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETITEM )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setItem( hbqt_par_QGraphicsItem( 2 ) );
}

/* void setPosAt ( qreal step, const QPointF & point ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETPOSAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setPosAt( hb_parnd( 2 ), *hbqt_par_QPointF( 3 ) );
}

/* void setRotationAt ( qreal step, qreal angle ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETROTATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setRotationAt( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setScaleAt ( qreal step, qreal sx, qreal sy ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETSCALEAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setScaleAt( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/* void setShearAt ( qreal step, qreal sh, qreal sv ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETSHEARAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setShearAt( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/* void setTimeLine ( QTimeLine * timeLine ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETTIMELINE )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setTimeLine( hbqt_par_QTimeLine( 2 ) );
}

/* void setTranslationAt ( qreal step, qreal dx, qreal dy ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETTRANSLATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setTranslationAt( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/* QTimeLine * timeLine () const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_TIMELINE )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTimeLine( ( p )->timeLine(), false ) );
}

/* qreal verticalScaleAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_VERTICALSCALEAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retnd( ( p )->verticalScaleAt( hb_parnd( 2 ) ) );
}

/* qreal verticalShearAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_VERTICALSHEARAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retnd( ( p )->verticalShearAt( hb_parnd( 2 ) ) );
}

/* qreal xTranslationAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_XTRANSLATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retnd( ( p )->xTranslationAt( hb_parnd( 2 ) ) );
}

/* qreal yTranslationAt ( qreal step ) const */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_YTRANSLATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      hb_retnd( ( p )->yTranslationAt( hb_parnd( 2 ) ) );
}

/* void setStep ( qreal step ) */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETSTEP )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
      ( p )->setStep( hb_parnd( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
