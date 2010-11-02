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
 *  enum Direction { TopToBottom, BottomToTop }
 */

/*
 *  Constructed[ 21/21 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QProgressBar>


/*
 * QProgressBar ( QWidget * parent = 0 )
 * ~QProgressBar ()
 */

typedef struct
{
   QPointer< QProgressBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QProgressBar;

HBQT_GC_FUNC( hbqt_gcRelease_QProgressBar )
{
   HBQT_GC_T_QProgressBar * p = ( HBQT_GC_T_QProgressBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QProgressBar * ph = p->ph;
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

void * hbqt_gcAllocate_QProgressBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QProgressBar * p = ( HBQT_GC_T_QProgressBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QProgressBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QProgressBar >( ( QProgressBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QProgressBar;
   p->type = HBQT_TYPE_QProgressBar;

   return p;
}

HB_FUNC( QT_QPROGRESSBAR )
{
   QProgressBar * pObj = NULL;

   pObj = new QProgressBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QProgressBar( ( void * ) pObj, true ) );
}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QPROGRESSBAR_ALIGNMENT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* QString format () const */
HB_FUNC( QT_QPROGRESSBAR_FORMAT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->format().toUtf8().data() );
}

/* bool invertedAppearance () */
HB_FUNC( QT_QPROGRESSBAR_INVERTEDAPPEARANCE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retl( ( p )->invertedAppearance() );
}

/* bool isTextVisible () const */
HB_FUNC( QT_QPROGRESSBAR_ISTEXTVISIBLE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retl( ( p )->isTextVisible() );
}

/* int maximum () const */
HB_FUNC( QT_QPROGRESSBAR_MAXIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retni( ( p )->maximum() );
}

/* int minimum () const */
HB_FUNC( QT_QPROGRESSBAR_MINIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retni( ( p )->minimum() );
}

/* Qt::Orientation orientation () const */
HB_FUNC( QT_QPROGRESSBAR_ORIENTATION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
}

/* void setAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QPROGRESSBAR_SETALIGNMENT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setFormat ( const QString & format ) */
HB_FUNC( QT_QPROGRESSBAR_SETFORMAT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFormat( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setInvertedAppearance ( bool invert ) */
HB_FUNC( QT_QPROGRESSBAR_SETINVERTEDAPPEARANCE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setInvertedAppearance( hb_parl( 2 ) );
}

/* void setTextDirection ( QProgressBar::Direction textDirection ) */
HB_FUNC( QT_QPROGRESSBAR_SETTEXTDIRECTION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setTextDirection( ( QProgressBar::Direction ) hb_parni( 2 ) );
}

/* void setTextVisible ( bool visible ) */
HB_FUNC( QT_QPROGRESSBAR_SETTEXTVISIBLE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setTextVisible( hb_parl( 2 ) );
}

/* virtual QString text () const */
HB_FUNC( QT_QPROGRESSBAR_TEXT )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* QProgressBar::Direction textDirection () */
HB_FUNC( QT_QPROGRESSBAR_TEXTDIRECTION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retni( ( QProgressBar::Direction ) ( p )->textDirection() );
}

/* int value () const */
HB_FUNC( QT_QPROGRESSBAR_VALUE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      hb_retni( ( p )->value() );
}

/* void reset () */
HB_FUNC( QT_QPROGRESSBAR_RESET )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->reset();
}

/* void setMaximum ( int maximum ) */
HB_FUNC( QT_QPROGRESSBAR_SETMAXIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setMaximum( hb_parni( 2 ) );
}

/* void setMinimum ( int minimum ) */
HB_FUNC( QT_QPROGRESSBAR_SETMINIMUM )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setMinimum( hb_parni( 2 ) );
}

/* void setOrientation ( Qt::Orientation ) */
HB_FUNC( QT_QPROGRESSBAR_SETORIENTATION )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/* void setRange ( int minimum, int maximum ) */
HB_FUNC( QT_QPROGRESSBAR_SETRANGE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setValue ( int value ) */
HB_FUNC( QT_QPROGRESSBAR_SETVALUE )
{
   QProgressBar * p = hbqt_par_QProgressBar( 1 );
   if( p )
      ( p )->setValue( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
