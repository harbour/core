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
 *  Constructed[ 18/18 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QDoubleSpinBox>


/*
 * QDoubleSpinBox ( QWidget * parent = 0 )
 * ~QDoubleSpinBox ()
 */

typedef struct
{
   QPointer< QDoubleSpinBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDoubleSpinBox;

HBQT_GC_FUNC( hbqt_gcRelease_QDoubleSpinBox )
{
   HBQT_GC_T_QDoubleSpinBox * p = ( HBQT_GC_T_QDoubleSpinBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QDoubleSpinBox * ph = p->ph;
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

void * hbqt_gcAllocate_QDoubleSpinBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QDoubleSpinBox * p = ( HBQT_GC_T_QDoubleSpinBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDoubleSpinBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDoubleSpinBox >( ( QDoubleSpinBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDoubleSpinBox;
   p->type = HBQT_TYPE_QDoubleSpinBox;

   return p;
}

HB_FUNC( QT_QDOUBLESPINBOX )
{
   QDoubleSpinBox * pObj = NULL;

   pObj = new QDoubleSpinBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDoubleSpinBox( ( void * ) pObj, true ) );
}

/* QString cleanText () const */
HB_FUNC( QT_QDOUBLESPINBOX_CLEANTEXT )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->cleanText().toUtf8().data() );
}

/* int decimals () const */
HB_FUNC( QT_QDOUBLESPINBOX_DECIMALS )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retni( ( p )->decimals() );
}

/* double maximum () const */
HB_FUNC( QT_QDOUBLESPINBOX_MAXIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->maximum() );
}

/* double minimum () const */
HB_FUNC( QT_QDOUBLESPINBOX_MINIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->minimum() );
}

/* QString prefix () const */
HB_FUNC( QT_QDOUBLESPINBOX_PREFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->prefix().toUtf8().data() );
}

/* void setDecimals ( int prec ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETDECIMALS )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setDecimals( hb_parni( 2 ) );
}

/* void setMaximum ( double max ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETMAXIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setMaximum( hb_parnd( 2 ) );
}

/* void setMinimum ( double min ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETMINIMUM )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setMinimum( hb_parnd( 2 ) );
}

/* void setPrefix ( const QString & prefix ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETPREFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPrefix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setRange ( double minimum, double maximum ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETRANGE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setRange( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setSingleStep ( double val ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETSINGLESTEP )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setSingleStep( hb_parnd( 2 ) );
}

/* void setSuffix ( const QString & suffix ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETSUFFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setSuffix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* double singleStep () const */
HB_FUNC( QT_QDOUBLESPINBOX_SINGLESTEP )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->singleStep() );
}

/* QString suffix () const */
HB_FUNC( QT_QDOUBLESPINBOX_SUFFIX )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->suffix().toUtf8().data() );
}

/* virtual QString textFromValue ( double value ) const */
HB_FUNC( QT_QDOUBLESPINBOX_TEXTFROMVALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->textFromValue( hb_parnd( 2 ) ).toUtf8().data() );
}

/* double value () const */
HB_FUNC( QT_QDOUBLESPINBOX_VALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      hb_retnd( ( p )->value() );
}

/* virtual double valueFromText ( const QString & text ) const */
HB_FUNC( QT_QDOUBLESPINBOX_VALUEFROMTEXT )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
   {
      void * pText;
      hb_retnd( ( p )->valueFromText( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* void setValue ( double val ) */
HB_FUNC( QT_QDOUBLESPINBOX_SETVALUE )
{
   QDoubleSpinBox * p = hbqt_par_QDoubleSpinBox( 1 );
   if( p )
      ( p )->setValue( hb_parnd( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
