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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QCheckBox>


/*
 * QCheckBox ( QWidget * parent = 0 )
 * QCheckBox ( const QString & text, QWidget * parent = 0 )
 * ~QCheckBox ()
 */

typedef struct
{
   QPointer< QCheckBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCheckBox;

HBQT_GC_FUNC( hbqt_gcRelease_QCheckBox )
{
   HBQT_GC_T_QCheckBox * p = ( HBQT_GC_T_QCheckBox * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QCheckBox * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QCheckBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QCheckBox * p = ( HBQT_GC_T_QCheckBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QCheckBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QCheckBox >( ( QCheckBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCheckBox;
   p->type = HBQT_TYPE_QCheckBox;

   return p;
}

HB_FUNC( QT_QCHECKBOX )
{
   QCheckBox * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QCheckBox( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj = new QCheckBox( hbqt_par_QWidget( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QCheckBox( ( void * ) pObj, true ) );
}

/* Qt::CheckState checkState () const */
HB_FUNC( QT_QCHECKBOX_CHECKSTATE )
{
   QCheckBox * p = hbqt_par_QCheckBox( 1 );
   if( p )
      hb_retni( ( Qt::CheckState ) ( p )->checkState() );
}

/* bool isTristate () const */
HB_FUNC( QT_QCHECKBOX_ISTRISTATE )
{
   QCheckBox * p = hbqt_par_QCheckBox( 1 );
   if( p )
      hb_retl( ( p )->isTristate() );
}

/* void setCheckState ( Qt::CheckState state ) */
HB_FUNC( QT_QCHECKBOX_SETCHECKSTATE )
{
   QCheckBox * p = hbqt_par_QCheckBox( 1 );
   if( p )
      ( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
}

/* void setTristate ( bool y = true ) */
HB_FUNC( QT_QCHECKBOX_SETTRISTATE )
{
   QCheckBox * p = hbqt_par_QCheckBox( 1 );
   if( p )
      ( p )->setTristate( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
