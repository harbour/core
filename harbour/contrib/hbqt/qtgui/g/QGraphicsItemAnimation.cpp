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
 *  Constructed[ 20/25 [ 80.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
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
   QGraphicsItemAnimation  * ph = NULL ;
   HBQT_GC_T_QGraphicsItemAnimation * p = ( HBQT_GC_T_QGraphicsItemAnimation * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGraphicsItemAnimation   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGraphicsItemAnimation   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QGraphicsItemAnimation          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGraphicsItemAnimation    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGraphicsItemAnimation    :    Object not created with new=true", ph ) );
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

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsItemAnimation  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsItemAnimation", pObj ) );
   }
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

/*
 * void clear ()
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_CLEAR )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * qreal horizontalScaleAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_HORIZONTALSCALEAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retnd( ( p )->horizontalScaleAt( hb_parnd( 2 ) ) );
   }
}

/*
 * qreal horizontalShearAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_HORIZONTALSHEARAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retnd( ( p )->horizontalShearAt( hb_parnd( 2 ) ) );
   }
}

/*
 * QGraphicsItem * item () const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_ITEM )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->item(), false ) );
   }
}

/*
 * QMatrix matrixAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_MATRIXAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->matrixAt( hb_parnd( 2 ) ) ), true ) );
   }
}

/*
 * QPointF posAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_POSAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->posAt( hb_parnd( 2 ) ) ), true ) );
   }
}

/*
 * qreal rotationAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_ROTATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retnd( ( p )->rotationAt( hb_parnd( 2 ) ) );
   }
}

/*
 * void setItem ( QGraphicsItem * item )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETITEM )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setItem( hbqt_par_QGraphicsItem( 2 ) );
   }
}

/*
 * void setPosAt ( qreal step, const QPointF & point )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETPOSAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setPosAt( hb_parnd( 2 ), *hbqt_par_QPointF( 3 ) );
   }
}

/*
 * void setRotationAt ( qreal step, qreal angle )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETROTATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setRotationAt( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setScaleAt ( qreal step, qreal sx, qreal sy )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETSCALEAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setScaleAt( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
   }
}

/*
 * void setShearAt ( qreal step, qreal sh, qreal sv )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETSHEARAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setShearAt( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
   }
}

/*
 * void setTimeLine ( QTimeLine * timeLine )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETTIMELINE )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setTimeLine( hbqt_par_QTimeLine( 2 ) );
   }
}

/*
 * void setTranslationAt ( qreal step, qreal dx, qreal dy )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETTRANSLATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setTranslationAt( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
   }
}

/*
 * QTimeLine * timeLine () const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_TIMELINE )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTimeLine( ( p )->timeLine(), false ) );
   }
}

/*
 * qreal verticalScaleAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_VERTICALSCALEAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retnd( ( p )->verticalScaleAt( hb_parnd( 2 ) ) );
   }
}

/*
 * qreal verticalShearAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_VERTICALSHEARAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retnd( ( p )->verticalShearAt( hb_parnd( 2 ) ) );
   }
}

/*
 * qreal xTranslationAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_XTRANSLATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retnd( ( p )->xTranslationAt( hb_parnd( 2 ) ) );
   }
}

/*
 * qreal yTranslationAt ( qreal step ) const
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_YTRANSLATIONAT )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      hb_retnd( ( p )->yTranslationAt( hb_parnd( 2 ) ) );
   }
}

/*
 * void setStep ( qreal step )
 */
HB_FUNC( QT_QGRAPHICSITEMANIMATION_SETSTEP )
{
   QGraphicsItemAnimation * p = hbqt_par_QGraphicsItemAnimation( 1 );
   if( p )
   {
      ( p )->setStep( hb_parnd( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
