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
 *  enum Mode { Hex, Dec, Oct, Bin }
 *  enum SegmentStyle { Outline, Filled, Flat }
 */

/*
 *  Constructed[ 19/19 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QLCDNumber>


/*
 * QLCDNumber ( QWidget * parent = 0 )
 * QLCDNumber ( uint numDigits, QWidget * parent = 0 )
 * ~QLCDNumber ()
 */

typedef struct
{
   QPointer< QLCDNumber > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLCDNumber;

HBQT_GC_FUNC( hbqt_gcRelease_QLCDNumber )
{
   HBQT_GC_T_QLCDNumber * p = ( HBQT_GC_T_QLCDNumber * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QLCDNumber * ph = p->ph;
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

void * hbqt_gcAllocate_QLCDNumber( void * pObj, bool bNew )
{
   HBQT_GC_T_QLCDNumber * p = ( HBQT_GC_T_QLCDNumber * ) hb_gcAllocate( sizeof( HBQT_GC_T_QLCDNumber ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QLCDNumber >( ( QLCDNumber * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLCDNumber;
   p->type = HBQT_TYPE_QLCDNumber;

   return p;
}

HB_FUNC( QT_QLCDNUMBER )
{
   QLCDNumber * pObj = NULL;

   pObj = new QLCDNumber( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QLCDNumber( ( void * ) pObj, true ) );
}

/* bool checkOverflow ( double num ) const */
HB_FUNC( QT_QLCDNUMBER_CHECKOVERFLOW )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retl( ( p )->checkOverflow( hb_parnd( 2 ) ) );
}

/* bool checkOverflow ( int num ) const */
HB_FUNC( QT_QLCDNUMBER_CHECKOVERFLOW_1 )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retl( ( p )->checkOverflow( hb_parni( 2 ) ) );
}

/* int intValue () const */
HB_FUNC( QT_QLCDNUMBER_INTVALUE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retni( ( p )->intValue() );
}

/* Mode mode () const */
HB_FUNC( QT_QLCDNUMBER_MODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retni( ( QLCDNumber::Mode ) ( p )->mode() );
}

/* int numDigits () const */
HB_FUNC( QT_QLCDNUMBER_NUMDIGITS )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retni( ( p )->numDigits() );
}

/* SegmentStyle segmentStyle () const */
HB_FUNC( QT_QLCDNUMBER_SEGMENTSTYLE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retni( ( QLCDNumber::SegmentStyle ) ( p )->segmentStyle() );
}

/* void setMode ( Mode ) */
HB_FUNC( QT_QLCDNUMBER_SETMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setMode( ( QLCDNumber::Mode ) hb_parni( 2 ) );
}

/* void setNumDigits ( int nDigits ) */
HB_FUNC( QT_QLCDNUMBER_SETNUMDIGITS )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setNumDigits( hb_parni( 2 ) );
}

/* void setSegmentStyle ( SegmentStyle ) */
HB_FUNC( QT_QLCDNUMBER_SETSEGMENTSTYLE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setSegmentStyle( ( QLCDNumber::SegmentStyle ) hb_parni( 2 ) );
}

/* bool smallDecimalPoint () const */
HB_FUNC( QT_QLCDNUMBER_SMALLDECIMALPOINT )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retl( ( p )->smallDecimalPoint() );
}

/* double value () const */
HB_FUNC( QT_QLCDNUMBER_VALUE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      hb_retnd( ( p )->value() );
}

/* void display ( const QString & s ) */
HB_FUNC( QT_QLCDNUMBER_DISPLAY )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
   {
      void * pText;
      ( p )->display( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void display ( double num ) */
HB_FUNC( QT_QLCDNUMBER_DISPLAY_1 )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->display( hb_parnd( 2 ) );
}

/* void display ( int num ) */
HB_FUNC( QT_QLCDNUMBER_DISPLAY_2 )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->display( hb_parni( 2 ) );
}

/* void setBinMode () */
HB_FUNC( QT_QLCDNUMBER_SETBINMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setBinMode();
}

/* void setDecMode () */
HB_FUNC( QT_QLCDNUMBER_SETDECMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setDecMode();
}

/* void setHexMode () */
HB_FUNC( QT_QLCDNUMBER_SETHEXMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setHexMode();
}

/* void setOctMode () */
HB_FUNC( QT_QLCDNUMBER_SETOCTMODE )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setOctMode();
}

/* void setSmallDecimalPoint ( bool ) */
HB_FUNC( QT_QLCDNUMBER_SETSMALLDECIMALPOINT )
{
   QLCDNumber * p = hbqt_par_QLCDNumber( 1 );
   if( p )
      ( p )->setSmallDecimalPoint( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
