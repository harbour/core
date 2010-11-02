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

#include <QtGui/QKeyEvent>


/*
 * QKeyEvent ( Type type, int key, Qt::KeyboardModifiers modifiers, const QString & text = QString(), bool autorep = false, ushort count = 1 )
 * ~QKeyEvent ()
 */

typedef struct
{
   QKeyEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QKeyEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QKeyEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QKeyEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QKeyEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QKeyEvent;
   p->type = HBQT_TYPE_QKeyEvent;

   return p;
}

HB_FUNC( QT_QKEYEVENT )
{
   //__HB_RETPTRGC__( new QKeyEvent( *hbqt_par_QKeyEvent( 1 ) ) );
}

/* int count () const */
HB_FUNC( QT_QKEYEVENT_COUNT )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* bool isAutoRepeat () const */
HB_FUNC( QT_QKEYEVENT_ISAUTOREPEAT )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retl( ( p )->isAutoRepeat() );
}

/* int key () const */
HB_FUNC( QT_QKEYEVENT_KEY )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retni( ( p )->key() );
}

/* bool matches ( QKeySequence::StandardKey key ) const */
HB_FUNC( QT_QKEYEVENT_MATCHES )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retl( ( p )->matches( ( QKeySequence::StandardKey ) hb_parni( 2 ) ) );
}

/* Qt::KeyboardModifiers modifiers () const */
HB_FUNC( QT_QKEYEVENT_MODIFIERS )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
}

/* quint32 nativeModifiers () const */
HB_FUNC( QT_QKEYEVENT_NATIVEMODIFIERS )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retnl( ( p )->nativeModifiers() );
}

/* quint32 nativeScanCode () const */
HB_FUNC( QT_QKEYEVENT_NATIVESCANCODE )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retnl( ( p )->nativeScanCode() );
}

/* quint32 nativeVirtualKey () const */
HB_FUNC( QT_QKEYEVENT_NATIVEVIRTUALKEY )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retnl( ( p )->nativeVirtualKey() );
}

/* QString text () const */
HB_FUNC( QT_QKEYEVENT_TEXT )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
