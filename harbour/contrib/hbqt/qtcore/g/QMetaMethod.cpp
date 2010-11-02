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
 *  enum Access { Private, Protected, Public }
 *  enum MethodType { Method, Signal, Slot, Constructor }
 */

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //bool invoke ( QObject * object, Qt::ConnectionType connectionType, QGenericReturnArgument returnValue, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() ) const
 *  //bool invoke ( QObject * object, QGenericReturnArgument returnValue, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() ) const
 *  //bool invoke ( QObject * object, Qt::ConnectionType connectionType, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() ) const
 *  //bool invoke ( QObject * object, QGenericArgument val0 = QGenericArgument( 0 ), QGenericArgument val1 = QGenericArgument(), QGenericArgument val2 = QGenericArgument(), QGenericArgument val3 = QGenericArgument(), QGenericArgument val4 = QGenericArgument(), QGenericArgument val5 = QGenericArgument(), QGenericArgument val6 = QGenericArgument(), QGenericArgument val7 = QGenericArgument(), QGenericArgument val8 = QGenericArgument(), QGenericArgument val9 = QGenericArgument() ) const
 */

#include <QtCore/QPointer>

#include <QtCore/QMetaMethod>


/*
 * QMetaMethod ()
 *
 */

typedef struct
{
   QMetaMethod * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMetaMethod;

HBQT_GC_FUNC( hbqt_gcRelease_QMetaMethod )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QMetaMethod * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMetaMethod( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMetaMethod * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMetaMethod;
   p->type = HBQT_TYPE_QMetaMethod;

   return p;
}

HB_FUNC( QT_QMETAMETHOD )
{
   QMetaMethod * pObj = NULL;

   pObj = new QMetaMethod() ;

   hb_retptrGC( hbqt_gcAllocate_QMetaMethod( ( void * ) pObj, true ) );
}

/* Access access () const */
HB_FUNC( QT_QMETAMETHOD_ACCESS )
{
   QMetaMethod * p = hbqt_par_QMetaMethod( 1 );
   if( p )
      hb_retni( ( QMetaMethod::Access ) ( p )->access() );
}

/* MethodType methodType () const */
HB_FUNC( QT_QMETAMETHOD_METHODTYPE )
{
   QMetaMethod * p = hbqt_par_QMetaMethod( 1 );
   if( p )
      hb_retni( ( QMetaMethod::MethodType ) ( p )->methodType() );
}

/* QList<QByteArray> parameterNames () const */
HB_FUNC( QT_QMETAMETHOD_PARAMETERNAMES )
{
   QMetaMethod * p = hbqt_par_QMetaMethod( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->parameterNames() ), true ) );
}

/* QList<QByteArray> parameterTypes () const */
HB_FUNC( QT_QMETAMETHOD_PARAMETERTYPES )
{
   QMetaMethod * p = hbqt_par_QMetaMethod( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->parameterTypes() ), true ) );
}

/* const char * signature () const */
HB_FUNC( QT_QMETAMETHOD_SIGNATURE )
{
   QMetaMethod * p = hbqt_par_QMetaMethod( 1 );
   if( p )
      hb_retc( ( p )->signature() );
}

/* const char * tag () const */
HB_FUNC( QT_QMETAMETHOD_TAG )
{
   QMetaMethod * p = hbqt_par_QMetaMethod( 1 );
   if( p )
      hb_retc( ( p )->tag() );
}

/* const char * typeName () const */
HB_FUNC( QT_QMETAMETHOD_TYPENAME )
{
   QMetaMethod * p = hbqt_par_QMetaMethod( 1 );
   if( p )
      hb_retc( ( p )->typeName() );
}


#endif /* #if QT_VERSION >= 0x040500 */
