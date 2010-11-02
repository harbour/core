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
 *  Constructed[ 28/28 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QObject * newInstance ( QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() ) const
 *  //bool invokeMethod ( QObject * obj, const char * member, Qt::ConnectionType type, QGenericReturnArgument ret, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() )
 *  //bool invokeMethod ( QObject * obj, const char * member, QGenericReturnArgument ret, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() )
 *  //bool invokeMethod ( QObject * obj, const char * member, Qt::ConnectionType type, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() )
 *  //bool invokeMethod ( QObject * obj, const char * member, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() )
 */

#include <QtCore/QPointer>

#include <QtCore/QMetaObject>
#include <QtCore/QMetaClassInfo>

/*
 * QMetaObject ()
 *
 */

typedef struct
{
   QMetaObject * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMetaObject;

HBQT_GC_FUNC( hbqt_gcRelease_QMetaObject )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMetaObject( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMetaObject * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMetaObject;
   p->type = HBQT_TYPE_QMetaObject;

   return p;
}

HB_FUNC( QT_QMETAOBJECT )
{
   // __HB_RETPTRGC__( new QMetaObject() );
}

/* QMetaClassInfo classInfo ( int index ) const */
HB_FUNC( QT_QMETAOBJECT_CLASSINFO )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaClassInfo( new QMetaClassInfo( ( p )->classInfo( hb_parni( 2 ) ) ), true ) );
}

/* int classInfoCount () const */
HB_FUNC( QT_QMETAOBJECT_CLASSINFOCOUNT )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->classInfoCount() );
}

/* int classInfoOffset () const */
HB_FUNC( QT_QMETAOBJECT_CLASSINFOOFFSET )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->classInfoOffset() );
}

/* const char * className () const */
HB_FUNC( QT_QMETAOBJECT_CLASSNAME )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retc( ( p )->className() );
}

/* QMetaMethod constructor ( int index ) const */
HB_FUNC( QT_QMETAOBJECT_CONSTRUCTOR )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaMethod( new QMetaMethod( ( p )->constructor( hb_parni( 2 ) ) ), true ) );
}

/* int constructorCount () const */
HB_FUNC( QT_QMETAOBJECT_CONSTRUCTORCOUNT )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->constructorCount() );
}

/* QMetaEnum enumerator ( int index ) const */
HB_FUNC( QT_QMETAOBJECT_ENUMERATOR )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaEnum( new QMetaEnum( ( p )->enumerator( hb_parni( 2 ) ) ), true ) );
}

/* int enumeratorCount () const */
HB_FUNC( QT_QMETAOBJECT_ENUMERATORCOUNT )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->enumeratorCount() );
}

/* int enumeratorOffset () const */
HB_FUNC( QT_QMETAOBJECT_ENUMERATOROFFSET )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->enumeratorOffset() );
}

/* int indexOfClassInfo ( const char * name ) const */
HB_FUNC( QT_QMETAOBJECT_INDEXOFCLASSINFO )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->indexOfClassInfo( ( const char * ) hb_parc( 2 ) ) );
}

/* int indexOfConstructor ( const char * constructor ) const */
HB_FUNC( QT_QMETAOBJECT_INDEXOFCONSTRUCTOR )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->indexOfConstructor( ( const char * ) hb_parc( 2 ) ) );
}

/* int indexOfEnumerator ( const char * name ) const */
HB_FUNC( QT_QMETAOBJECT_INDEXOFENUMERATOR )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->indexOfEnumerator( ( const char * ) hb_parc( 2 ) ) );
}

/* int indexOfMethod ( const char * method ) const */
HB_FUNC( QT_QMETAOBJECT_INDEXOFMETHOD )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->indexOfMethod( ( const char * ) hb_parc( 2 ) ) );
}

/* int indexOfProperty ( const char * name ) const */
HB_FUNC( QT_QMETAOBJECT_INDEXOFPROPERTY )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->indexOfProperty( ( const char * ) hb_parc( 2 ) ) );
}

/* int indexOfSignal ( const char * signal ) const */
HB_FUNC( QT_QMETAOBJECT_INDEXOFSIGNAL )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->indexOfSignal( ( const char * ) hb_parc( 2 ) ) );
}

/* int indexOfSlot ( const char * slot ) const */
HB_FUNC( QT_QMETAOBJECT_INDEXOFSLOT )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->indexOfSlot( ( const char * ) hb_parc( 2 ) ) );
}

/* QMetaMethod method ( int index ) const */
HB_FUNC( QT_QMETAOBJECT_METHOD )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaMethod( new QMetaMethod( ( p )->method( hb_parni( 2 ) ) ), true ) );
}

/* int methodCount () const */
HB_FUNC( QT_QMETAOBJECT_METHODCOUNT )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->methodCount() );
}

/* int methodOffset () const */
HB_FUNC( QT_QMETAOBJECT_METHODOFFSET )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->methodOffset() );
}

/* QMetaProperty property ( int index ) const */
HB_FUNC( QT_QMETAOBJECT_PROPERTY )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaProperty( new QMetaProperty( ( p )->property( hb_parni( 2 ) ) ), true ) );
}

/* int propertyCount () const */
HB_FUNC( QT_QMETAOBJECT_PROPERTYCOUNT )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->propertyCount() );
}

/* int propertyOffset () const */
HB_FUNC( QT_QMETAOBJECT_PROPERTYOFFSET )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retni( ( p )->propertyOffset() );
}

/* const QMetaObject * superClass () const */
HB_FUNC( QT_QMETAOBJECT_SUPERCLASS )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaObject( new QMetaObject( *( ( p )->superClass() ) ), true ) );
}

/* QMetaProperty userProperty () const */
HB_FUNC( QT_QMETAOBJECT_USERPROPERTY )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMetaProperty( new QMetaProperty( ( p )->userProperty() ), true ) );
}

/* bool checkConnectArgs ( const char * signal, const char * method ) */
HB_FUNC( QT_QMETAOBJECT_CHECKCONNECTARGS )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retl( ( p )->checkConnectArgs( ( const char * ) hb_parc( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* void connectSlotsByName ( QObject * object ) */
HB_FUNC( QT_QMETAOBJECT_CONNECTSLOTSBYNAME )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      ( p )->connectSlotsByName( hbqt_par_QObject( 2 ) );
}

/* QByteArray normalizedSignature ( const char * method ) */
HB_FUNC( QT_QMETAOBJECT_NORMALIZEDSIGNATURE )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->normalizedSignature( ( const char * ) hb_parc( 2 ) ) ), true ) );
}

/* QByteArray normalizedType ( const char * type ) */
HB_FUNC( QT_QMETAOBJECT_NORMALIZEDTYPE )
{
   QMetaObject * p = hbqt_par_QMetaObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->normalizedType( ( const char * ) hb_parc( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
