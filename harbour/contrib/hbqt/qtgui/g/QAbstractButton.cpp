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
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractButton>


/*
 * QAbstractButton ( QWidget * parent = 0 )
 * ~QAbstractButton ()
 */

typedef struct
{
   QPointer< QAbstractButton > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractButton;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractButton )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractButton( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractButton * p = ( HBQT_GC_T_QAbstractButton * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractButton ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractButton >( ( QAbstractButton * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractButton;
   p->type = HBQT_TYPE_QAbstractButton;

   return p;
}

HB_FUNC( QT_QABSTRACTBUTTON )
{

}

/* bool autoExclusive () const */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOEXCLUSIVE )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retl( ( p )->autoExclusive() );
}

/* bool autoRepeat () const */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEAT )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retl( ( p )->autoRepeat() );
}

/* int autoRepeatDelay () const */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEATDELAY )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retni( ( p )->autoRepeatDelay() );
}

/* int autoRepeatInterval () const */
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEATINTERVAL )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retni( ( p )->autoRepeatInterval() );
}

/* QButtonGroup * group () const */
HB_FUNC( QT_QABSTRACTBUTTON_GROUP )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QButtonGroup( ( p )->group(), false ) );
}

/* QIcon icon () const */
HB_FUNC( QT_QABSTRACTBUTTON_ICON )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
}

/* QSize iconSize () const */
HB_FUNC( QT_QABSTRACTBUTTON_ICONSIZE )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
}

/* bool isCheckable () const */
HB_FUNC( QT_QABSTRACTBUTTON_ISCHECKABLE )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retl( ( p )->isCheckable() );
}

/* bool isChecked () const */
HB_FUNC( QT_QABSTRACTBUTTON_ISCHECKED )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retl( ( p )->isChecked() );
}

/* bool isDown () const */
HB_FUNC( QT_QABSTRACTBUTTON_ISDOWN )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retl( ( p )->isDown() );
}

/* void setAutoExclusive ( bool ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOEXCLUSIVE )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setAutoExclusive( hb_parl( 2 ) );
}

/* void setAutoRepeat ( bool ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEAT )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setAutoRepeat( hb_parl( 2 ) );
}

/* void setAutoRepeatDelay ( int ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEATDELAY )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setAutoRepeatDelay( hb_parni( 2 ) );
}

/* void setAutoRepeatInterval ( int ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEATINTERVAL )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setAutoRepeatInterval( hb_parni( 2 ) );
}

/* void setCheckable ( bool ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETCHECKABLE )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setCheckable( hb_parl( 2 ) );
}

/* void setDown ( bool ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETDOWN )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setDown( hb_parl( 2 ) );
}

/* void setIcon ( const QIcon & icon ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETICON )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
}

/* void setShortcut ( const QKeySequence & key ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETSHORTCUT )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setShortcut( *hbqt_par_QKeySequence( 2 ) );
}

/* void setText ( const QString & text ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETTEXT )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QKeySequence shortcut () const */
HB_FUNC( QT_QABSTRACTBUTTON_SHORTCUT )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QKeySequence( new QKeySequence( ( p )->shortcut() ), true ) );
}

/* QString text () const */
HB_FUNC( QT_QABSTRACTBUTTON_TEXT )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* void animateClick ( int msec = 100 ) */
HB_FUNC( QT_QABSTRACTBUTTON_ANIMATECLICK )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->animateClick( hb_parnidef( 2, 100 ) );
}

/* void click () */
HB_FUNC( QT_QABSTRACTBUTTON_CLICK )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->click();
}

/* void setChecked ( bool ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETCHECKED )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setChecked( hb_parl( 2 ) );
}

/* void setIconSize ( const QSize & size ) */
HB_FUNC( QT_QABSTRACTBUTTON_SETICONSIZE )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/* void toggle () */
HB_FUNC( QT_QABSTRACTBUTTON_TOGGLE )
{
   QAbstractButton * p = hbqt_par_QAbstractButton( 1 );
   if( p )
      ( p )->toggle();
}


#endif /* #if QT_VERSION >= 0x040500 */
