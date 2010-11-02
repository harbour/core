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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSpinBox>


/*
 * QSpinBox ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QSpinBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSpinBox;

HBQT_GC_FUNC( hbqt_gcRelease_QSpinBox )
{
   HBQT_GC_T_QSpinBox * p = ( HBQT_GC_T_QSpinBox * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QSpinBox * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSpinBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QSpinBox * p = ( HBQT_GC_T_QSpinBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSpinBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSpinBox >( ( QSpinBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSpinBox;
   p->type = HBQT_TYPE_QSpinBox;

   return p;
}

HB_FUNC( QT_QSPINBOX )
{
   QSpinBox * pObj = NULL;

   pObj = new QSpinBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSpinBox( ( void * ) pObj, true ) );
}

/* QString cleanText () const */
HB_FUNC( QT_QSPINBOX_CLEANTEXT )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->cleanText().toUtf8().data() );
}

/* int maximum () const */
HB_FUNC( QT_QSPINBOX_MAXIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->maximum() );
}

/* int minimum () const */
HB_FUNC( QT_QSPINBOX_MINIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->minimum() );
}

/* QString prefix () const */
HB_FUNC( QT_QSPINBOX_PREFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->prefix().toUtf8().data() );
}

/* void setMaximum ( int max ) */
HB_FUNC( QT_QSPINBOX_SETMAXIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setMaximum( hb_parni( 2 ) );
}

/* void setMinimum ( int min ) */
HB_FUNC( QT_QSPINBOX_SETMINIMUM )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setMinimum( hb_parni( 2 ) );
}

/* void setPrefix ( const QString & prefix ) */
HB_FUNC( QT_QSPINBOX_SETPREFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPrefix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setRange ( int minimum, int maximum ) */
HB_FUNC( QT_QSPINBOX_SETRANGE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setSingleStep ( int val ) */
HB_FUNC( QT_QSPINBOX_SETSINGLESTEP )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setSingleStep( hb_parni( 2 ) );
}

/* void setSuffix ( const QString & suffix ) */
HB_FUNC( QT_QSPINBOX_SETSUFFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setSuffix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* int singleStep () const */
HB_FUNC( QT_QSPINBOX_SINGLESTEP )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->singleStep() );
}

/* QString suffix () const */
HB_FUNC( QT_QSPINBOX_SUFFIX )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->suffix().toUtf8().data() );
}

/* int value () const */
HB_FUNC( QT_QSPINBOX_VALUE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      hb_retni( ( p )->value() );
}

/* void setValue ( int val ) */
HB_FUNC( QT_QSPINBOX_SETVALUE )
{
   QSpinBox * p = hbqt_par_QSpinBox( 1 );
   if( p )
      ( p )->setValue( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
