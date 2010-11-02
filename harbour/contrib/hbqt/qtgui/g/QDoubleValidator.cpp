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
 *  enum State { Invalid, Intermediate, Acceptable }
 *  enum Notation { StandardNotation, ScientificNotation }
 */

/*
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //virtual QValidator::State validate ( const QString & input, const int & pos ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QDoubleValidator>


/* QDoubleValidator ( QObject * parent )
 * QDoubleValidator ( double bottom, double top, int decimals, QObject * parent )
 * ~QDoubleValidator ()
 */

typedef struct
{
   QPointer< QDoubleValidator > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDoubleValidator;

HBQT_GC_FUNC( hbqt_gcRelease_QDoubleValidator )
{
   QDoubleValidator  * ph = NULL;
   HBQT_GC_T_QDoubleValidator * p = ( HBQT_GC_T_QDoubleValidator * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QDoubleValidator( void * pObj, bool bNew )
{
   HBQT_GC_T_QDoubleValidator * p = ( HBQT_GC_T_QDoubleValidator * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDoubleValidator ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDoubleValidator >( ( QDoubleValidator * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDoubleValidator;
   p->type = HBQT_TYPE_QDoubleValidator;

   return p;
}

HB_FUNC( QT_QDOUBLEVALIDATOR )
{
   QDoubleValidator * pObj = NULL;

   if( hb_pcount() >= 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      pObj = new QDoubleValidator( hb_parnd( 1 ), hb_parnd( 2 ), hb_parni( 3 ), HB_ISPOINTER( 4 ) ? hbqt_par_QObject( 4 ) : 0 ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QDoubleValidator( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QDoubleValidator( ( void * ) pObj, true ) );
}

/* double bottom () const */
HB_FUNC( QT_QDOUBLEVALIDATOR_BOTTOM )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      hb_retnd( ( p )->bottom() );
}

/* int decimals () const */
HB_FUNC( QT_QDOUBLEVALIDATOR_DECIMALS )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      hb_retni( ( p )->decimals() );
}

/* Notation notation () const */
HB_FUNC( QT_QDOUBLEVALIDATOR_NOTATION )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      hb_retni( ( QDoubleValidator::Notation ) ( p )->notation() );
}

/* void setBottom ( double ) */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETBOTTOM )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      ( p )->setBottom( hb_parnd( 2 ) );
}

/* void setDecimals ( int ) */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETDECIMALS )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      ( p )->setDecimals( hb_parni( 2 ) );
}

/* void setNotation ( Notation ) */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETNOTATION )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      ( p )->setNotation( ( QDoubleValidator::Notation ) hb_parni( 2 ) );
}

/* virtual void setRange ( double minimum, double maximum, int decimals = 0 ) */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETRANGE )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      ( p )->setRange( hb_parnd( 2 ), hb_parnd( 3 ), hb_parni( 4 ) );
}

/* void setTop ( double ) */
HB_FUNC( QT_QDOUBLEVALIDATOR_SETTOP )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      ( p )->setTop( hb_parnd( 2 ) );
}

/* double top () const */
HB_FUNC( QT_QDOUBLEVALIDATOR_TOP )
{
   QDoubleValidator * p = hbqt_par_QDoubleValidator( 1 );
   if( p )
      hb_retnd( ( p )->top() );
}


#endif /* #if QT_VERSION >= 0x040500 */
