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

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 21/21 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QMetaProperty>


/*
 * QMetaProperty ()
 *
 */

typedef struct
{
   QMetaProperty * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMetaProperty;

HBQT_GC_FUNC( hbqt_gcRelease_QMetaProperty )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMetaProperty( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMetaProperty * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMetaProperty;
   p->type = HBQT_TYPE_QMetaProperty;

   return p;
}

HB_FUNC( QT_QMETAPROPERTY )
{
   // __HB_RETPTRGC__( new QMetaProperty() );
}

/* QMetaEnum enumerator () const */
HB_FUNC( QT_QMETAPROPERTY_ENUMERATOR )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaEnum( new QMetaEnum( ( p )->enumerator() ), true ) );
}

/* bool hasNotifySignal () const */
HB_FUNC( QT_QMETAPROPERTY_HASNOTIFYSIGNAL )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->hasNotifySignal() );
}

/* bool isDesignable ( const QObject * object = 0 ) const */
HB_FUNC( QT_QMETAPROPERTY_ISDESIGNABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isDesignable( hbqt_par_QObject( 2 ) ) );
}

/* bool isEnumType () const */
HB_FUNC( QT_QMETAPROPERTY_ISENUMTYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isEnumType() );
}

/* bool isFlagType () const */
HB_FUNC( QT_QMETAPROPERTY_ISFLAGTYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isFlagType() );
}

/* bool isReadable () const */
HB_FUNC( QT_QMETAPROPERTY_ISREADABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isReadable() );
}

/* bool isResettable () const */
HB_FUNC( QT_QMETAPROPERTY_ISRESETTABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isResettable() );
}

/* bool isScriptable ( const QObject * object = 0 ) const */
HB_FUNC( QT_QMETAPROPERTY_ISSCRIPTABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isScriptable( hbqt_par_QObject( 2 ) ) );
}

/* bool isStored ( const QObject * object = 0 ) const */
HB_FUNC( QT_QMETAPROPERTY_ISSTORED )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isStored( hbqt_par_QObject( 2 ) ) );
}

/* bool isUser ( const QObject * object = 0 ) const */
HB_FUNC( QT_QMETAPROPERTY_ISUSER )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isUser( hbqt_par_QObject( 2 ) ) );
}

/* bool isValid () const */
HB_FUNC( QT_QMETAPROPERTY_ISVALID )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* bool isWritable () const */
HB_FUNC( QT_QMETAPROPERTY_ISWRITABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->isWritable() );
}

/* const char * name () const */
HB_FUNC( QT_QMETAPROPERTY_NAME )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retc( ( p )->name() );
}

/* QMetaMethod notifySignal () const */
HB_FUNC( QT_QMETAPROPERTY_NOTIFYSIGNAL )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaMethod( new QMetaMethod( ( p )->notifySignal() ), true ) );
}

/* int notifySignalIndex () const */
HB_FUNC( QT_QMETAPROPERTY_NOTIFYSIGNALINDEX )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retni( ( p )->notifySignalIndex() );
}

/* QVariant read ( const QObject * object ) const */
HB_FUNC( QT_QMETAPROPERTY_READ )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->read( hbqt_par_QObject( 2 ) ) ), true ) );
}

/* bool reset ( QObject * object ) const */
HB_FUNC( QT_QMETAPROPERTY_RESET )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->reset( hbqt_par_QObject( 2 ) ) );
}

/* QVariant::Type type () const */
HB_FUNC( QT_QMETAPROPERTY_TYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retni( ( QVariant::Type ) ( p )->type() );
}

/* const char * typeName () const */
HB_FUNC( QT_QMETAPROPERTY_TYPENAME )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retc( ( p )->typeName() );
}

/* int userType () const */
HB_FUNC( QT_QMETAPROPERTY_USERTYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retni( ( p )->userType() );
}

/* bool write ( QObject * object, const QVariant & value ) const */
HB_FUNC( QT_QMETAPROPERTY_WRITE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
      hb_retl( ( p )->write( hbqt_par_QObject( 2 ), *hbqt_par_QVariant( 3 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
