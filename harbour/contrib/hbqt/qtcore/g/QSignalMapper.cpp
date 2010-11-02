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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QObject * mapping ( QWidget * widget ) const
 *  // void setMapping ( QObject * sender, QWidget * widget )
 */

#include <QtCore/QPointer>

#include <QtCore/QSignalMapper>


/*
 * QSignalMapper ( QObject * parent = 0 )
 * ~QSignalMapper ()
 */

typedef struct
{
   QPointer< QSignalMapper > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSignalMapper;

HBQT_GC_FUNC( hbqt_gcRelease_QSignalMapper )
{
   HBQT_GC_T_QSignalMapper * p = ( HBQT_GC_T_QSignalMapper * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QSignalMapper * ph = p->ph;
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

void * hbqt_gcAllocate_QSignalMapper( void * pObj, bool bNew )
{
   HBQT_GC_T_QSignalMapper * p = ( HBQT_GC_T_QSignalMapper * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSignalMapper ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSignalMapper >( ( QSignalMapper * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSignalMapper;
   p->type = HBQT_TYPE_QSignalMapper;

   return p;
}

HB_FUNC( QT_QSIGNALMAPPER )
{
   QSignalMapper * pObj = NULL;

   pObj = new QSignalMapper( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSignalMapper( ( void * ) pObj, true ) );
}

/* QObject * mapping ( int id ) const */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->mapping( hb_parni( 2 ) ), false ) );
}

/* QObject * mapping ( const QString & id ) const */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING_1 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->mapping( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QObject * mapping ( QObject * object ) const */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING_2 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->mapping( hbqt_par_QObject( 2 ) ), false ) );
}

/* void removeMappings ( QObject * sender ) */
HB_FUNC( QT_QSIGNALMAPPER_REMOVEMAPPINGS )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
      ( p )->removeMappings( hbqt_par_QObject( 2 ) );
}

/* void setMapping ( QObject * sender, int id ) */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
      ( p )->setMapping( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
}

/* void setMapping ( QObject * sender, const QString & text ) */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING_1 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      void * pText;
      ( p )->setMapping( hbqt_par_QObject( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setMapping ( QObject * sender, QObject * object ) */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING_2 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
      ( p )->setMapping( hbqt_par_QObject( 2 ), hbqt_par_QObject( 3 ) );
}

/* void map () */
HB_FUNC( QT_QSIGNALMAPPER_MAP )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
      ( p )->map();
}

/* void map ( QObject * sender ) */
HB_FUNC( QT_QSIGNALMAPPER_MAP_1 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
      ( p )->map( hbqt_par_QObject( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
