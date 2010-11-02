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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPushButton>


/*
 * QPushButton ( QWidget * parent = 0 )
 * QPushButton ( const QString & text, QWidget * parent = 0 )
 * QPushButton ( const QIcon & icon, const QString & text, QWidget * parent = 0 )
 * ~QPushButton ()
 */

typedef struct
{
   QPointer< QPushButton > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPushButton;

HBQT_GC_FUNC( hbqt_gcRelease_QPushButton )
{
   HBQT_GC_T_QPushButton * p = ( HBQT_GC_T_QPushButton * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QPushButton * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPushButton( void * pObj, bool bNew )
{
   HBQT_GC_T_QPushButton * p = ( HBQT_GC_T_QPushButton * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPushButton ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPushButton >( ( QPushButton * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPushButton;
   p->type = HBQT_TYPE_QPushButton;

   return p;
}

HB_FUNC( QT_QPUSHBUTTON )
{
   QPushButton * pObj = NULL;

    pObj = new QPushButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPushButton( ( void * ) pObj, true ) );
}

/* bool autoDefault () const */
HB_FUNC( QT_QPUSHBUTTON_AUTODEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      hb_retl( ( p )->autoDefault() );
}

/* bool isDefault () const */
HB_FUNC( QT_QPUSHBUTTON_ISDEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      hb_retl( ( p )->isDefault() );
}

/* bool isFlat () const */
HB_FUNC( QT_QPUSHBUTTON_ISFLAT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      hb_retl( ( p )->isFlat() );
}

/* QMenu * menu () const */
HB_FUNC( QT_QPUSHBUTTON_MENU )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) );
}

/* void setAutoDefault ( bool ) */
HB_FUNC( QT_QPUSHBUTTON_SETAUTODEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      ( p )->setAutoDefault( hb_parl( 2 ) );
}

/* void setDefault ( bool ) */
HB_FUNC( QT_QPUSHBUTTON_SETDEFAULT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      ( p )->setDefault( hb_parl( 2 ) );
}

/* void setFlat ( bool ) */
HB_FUNC( QT_QPUSHBUTTON_SETFLAT )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      ( p )->setFlat( hb_parl( 2 ) );
}

/* void setMenu ( QMenu * menu ) */
HB_FUNC( QT_QPUSHBUTTON_SETMENU )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      ( p )->setMenu( hbqt_par_QMenu( 2 ) );
}

/* void showMenu () */
HB_FUNC( QT_QPUSHBUTTON_SHOWMENU )
{
   QPushButton * p = hbqt_par_QPushButton( 1 );
   if( p )
      ( p )->showMenu();
}


#endif /* #if QT_VERSION >= 0x040500 */
