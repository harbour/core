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
 */

/*
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //virtual QValidator::State validate ( QString & input, int & pos ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QIntValidator>


/* QIntValidator ( QObject * parent )
 * QIntValidator ( int minimum, int maximum, QObject * parent )
* ~QIntValidator ()
 */

typedef struct
{
   QPointer< QIntValidator > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QIntValidator;

HBQT_GC_FUNC( hbqt_gcRelease_QIntValidator )
{
   HBQT_GC_T_QIntValidator * p = ( HBQT_GC_T_QIntValidator * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QIntValidator * ph = p->ph;
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

void * hbqt_gcAllocate_QIntValidator( void * pObj, bool bNew )
{
   HBQT_GC_T_QIntValidator * p = ( HBQT_GC_T_QIntValidator * ) hb_gcAllocate( sizeof( HBQT_GC_T_QIntValidator ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QIntValidator >( ( QIntValidator * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QIntValidator;
   p->type = HBQT_TYPE_QIntValidator;

   return p;
}

HB_FUNC( QT_QINTVALIDATOR )
{
   QIntValidator * pObj = NULL;

   if( hb_pcount() == 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISPOINTER( 3 ) )
   {
      pObj = new QIntValidator( hb_parni( 1 ), hb_parni( 2 ), hbqt_par_QObject( 3 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QIntValidator( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QIntValidator( ( void * ) pObj, true ) );
}

/* int bottom () const */
HB_FUNC( QT_QINTVALIDATOR_BOTTOM )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      hb_retni( ( p )->bottom() );
}

/* void setBottom ( int ) */
HB_FUNC( QT_QINTVALIDATOR_SETBOTTOM )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      ( p )->setBottom( hb_parni( 2 ) );
}

/* virtual void setRange ( int bottom, int top ) */
HB_FUNC( QT_QINTVALIDATOR_SETRANGE )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setTop ( int ) */
HB_FUNC( QT_QINTVALIDATOR_SETTOP )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      ( p )->setTop( hb_parni( 2 ) );
}

/* int top () const */
HB_FUNC( QT_QINTVALIDATOR_TOP )
{
   QIntValidator * p = hbqt_par_QIntValidator( 1 );
   if( p )
      hb_retni( ( p )->top() );
}


#endif /* #if QT_VERSION >= 0x040500 */
