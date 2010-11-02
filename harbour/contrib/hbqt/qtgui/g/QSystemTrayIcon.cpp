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
 *  enum ActivationReason { Unknown, Context, DoubleClick, Trigger, MiddleClick }
 *  enum MessageIcon { NoIcon, Information, Warning, Critical }
 */

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSystemTrayIcon>


/*
 * QSystemTrayIcon ( QObject * parent = 0 )
 * QSystemTrayIcon ( const QIcon & icon, QObject * parent = 0 )
 * ~QSystemTrayIcon ()
 */

typedef struct
{
   QPointer< QSystemTrayIcon > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSystemTrayIcon;

HBQT_GC_FUNC( hbqt_gcRelease_QSystemTrayIcon )
{
   HBQT_GC_T_QSystemTrayIcon * p = ( HBQT_GC_T_QSystemTrayIcon * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QSystemTrayIcon * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSystemTrayIcon( void * pObj, bool bNew )
{
   HBQT_GC_T_QSystemTrayIcon * p = ( HBQT_GC_T_QSystemTrayIcon * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSystemTrayIcon ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSystemTrayIcon >( ( QSystemTrayIcon * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSystemTrayIcon;
   p->type = HBQT_TYPE_QSystemTrayIcon;

   return p;
}

HB_FUNC( QT_QSYSTEMTRAYICON )
{
   QSystemTrayIcon * pObj = NULL;

   pObj = new QSystemTrayIcon( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSystemTrayIcon( ( void * ) pObj, true ) );
}

/* QMenu * contextMenu () const */
HB_FUNC( QT_QSYSTEMTRAYICON_CONTEXTMENU )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->contextMenu(), false ) );
}

/* QRect geometry () const */
HB_FUNC( QT_QSYSTEMTRAYICON_GEOMETRY )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
}

/* QIcon icon () const */
HB_FUNC( QT_QSYSTEMTRAYICON_ICON )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
}

/* bool isVisible () const */
HB_FUNC( QT_QSYSTEMTRAYICON_ISVISIBLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
}

/* void setContextMenu ( QMenu * menu ) */
HB_FUNC( QT_QSYSTEMTRAYICON_SETCONTEXTMENU )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->setContextMenu( hbqt_par_QMenu( 2 ) );
}

/* void setIcon ( const QIcon & icon ) */
HB_FUNC( QT_QSYSTEMTRAYICON_SETICON )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
}

/* void setToolTip ( const QString & tip ) */
HB_FUNC( QT_QSYSTEMTRAYICON_SETTOOLTIP )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void showMessage ( const QString & title, const QString & message, MessageIcon icon = Information, int millisecondsTimeoutHint = 10000 ) */
HB_FUNC( QT_QSYSTEMTRAYICON_SHOWMESSAGE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
   {
      void * pText;
      ( p )->showMessage( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( QSystemTrayIcon::MessageIcon ) hb_parni( 4 ) : ( QSystemTrayIcon::MessageIcon ) QSystemTrayIcon::Information ), hb_parnidef( 5, 10000 ) );
      hb_strfree( pText );
   }
}

/* QString toolTip () const */
HB_FUNC( QT_QSYSTEMTRAYICON_TOOLTIP )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
}

/* bool isSystemTrayAvailable () */
HB_FUNC( QT_QSYSTEMTRAYICON_ISSYSTEMTRAYAVAILABLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retl( ( p )->isSystemTrayAvailable() );
}

/* bool supportsMessages () */
HB_FUNC( QT_QSYSTEMTRAYICON_SUPPORTSMESSAGES )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      hb_retl( ( p )->supportsMessages() );
}

/* void hide () */
HB_FUNC( QT_QSYSTEMTRAYICON_HIDE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->hide();
}

/* void setVisible ( bool visible ) */
HB_FUNC( QT_QSYSTEMTRAYICON_SETVISIBLE )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* void show () */
HB_FUNC( QT_QSYSTEMTRAYICON_SHOW )
{
   QSystemTrayIcon * p = hbqt_par_QSystemTrayIcon( 1 );
   if( p )
      ( p )->show();
}


#endif /* #if QT_VERSION >= 0x040500 */
