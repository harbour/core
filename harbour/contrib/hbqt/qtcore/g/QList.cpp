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
 *  Constructed[ 36/40 [ 90.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void append ( const QList<T> & value )
 *  QList<T> mid ( int pos, int length = -1 ) const
 *  QSet<T> toSet () const
 *  QVector<T> toVector () const
 *
 *  *** Commented out protostypes ***
 *
 *  //T & back ()
 *  //iterator begin ()
 *  //const_iterator begin () const
 *  //const_iterator constBegin () const
 *  //const_iterator constEnd () const
 *  //bool contains ( const T & value ) const
 *  //iterator end ()
 *  //const_iterator end () const
 *  //iterator erase ( iterator pos )
 *  //iterator erase ( iterator begin, iterator end )
 *  //T & first ()
 *  //T & front ()
 *  //iterator insert ( iterator before, const T & value )
 *  //T & last ()
 *  //std::list<T> toStdList () const
 */

#include <QtCore/QPointer>

#include <QtCore/QList>


/*
 * QList ()
 * QList ( const QList<T> & other )
 * ~QList ()
 */

typedef struct
{
   QList< void * > * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QList;

HBQT_GC_FUNC( hbqt_gcRelease_QList )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QList< void * > * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QList( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QList< void * > * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QList;
   p->type = HBQT_TYPE_QList;

   return p;
}

HB_FUNC( QT_QLIST )
{
   QList< void * > * pObj = NULL;

   QList<void*>* list = NULL;
   pObj = ( QList<void*>* ) list ;

   hb_retptrGC( hbqt_gcAllocate_QList( ( void * ) pObj, true ) );
}

/* void append ( const T & value ) */
HB_FUNC( QT_QLIST_APPEND )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->append( hb_param( 2, HB_IT_ANY ) );
}

/* const T & at ( int i ) const */
HB_FUNC( QT_QLIST_AT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->at( hb_parni( 2 ) ) );
}

/* const T & back () const */
HB_FUNC( QT_QLIST_BACK )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->back() );
}

/* void clear () */
HB_FUNC( QT_QLIST_CLEAR )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->clear();
}

/* int count ( const T & value ) const */
HB_FUNC( QT_QLIST_COUNT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retni( ( p )->count( hb_param( 2, HB_IT_ANY ) ) );
}

/* int count () const */
HB_FUNC( QT_QLIST_COUNT_1 )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* bool empty () const */
HB_FUNC( QT_QLIST_EMPTY )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retl( ( p )->empty() );
}

/* bool endsWith ( const T & value ) const */
HB_FUNC( QT_QLIST_ENDSWITH )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retl( ( p )->endsWith( hb_param( 2, HB_IT_ANY ) ) );
}

/* const T & first () const */
HB_FUNC( QT_QLIST_FIRST )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->first() );
}

/* const T & front () const */
HB_FUNC( QT_QLIST_FRONT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->front() );
}

/* int indexOf ( const T & value, int from = 0 ) const */
HB_FUNC( QT_QLIST_INDEXOF )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hb_param( 2, HB_IT_ANY ), hb_parni( 3 ) ) );
}

/* void insert ( int i, const T & value ) */
HB_FUNC( QT_QLIST_INSERT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->insert( hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) );
}

/* bool isEmpty () const */
HB_FUNC( QT_QLIST_ISEMPTY )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* const T & last () const */
HB_FUNC( QT_QLIST_LAST )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->last() );
}

/* int lastIndexOf ( const T & value, int from = -1 ) const */
HB_FUNC( QT_QLIST_LASTINDEXOF )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retni( ( p )->lastIndexOf( hb_param( 2, HB_IT_ANY ), hb_parnidef( 3, -1 ) ) );
}

/* int length () const */
HB_FUNC( QT_QLIST_LENGTH )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retni( ( p )->length() );
}

/* void move ( int from, int to ) */
HB_FUNC( QT_QLIST_MOVE )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->move( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void pop_back () */
HB_FUNC( QT_QLIST_POP_BACK )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->pop_back();
}

/* void pop_front () */
HB_FUNC( QT_QLIST_POP_FRONT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->pop_front();
}

/* void prepend ( const T & value ) */
HB_FUNC( QT_QLIST_PREPEND )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->prepend( hb_param( 2, HB_IT_ANY ) );
}

/* void push_back ( const T & value ) */
HB_FUNC( QT_QLIST_PUSH_BACK )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->push_back( hb_param( 2, HB_IT_ANY ) );
}

/* void push_front ( const T & value ) */
HB_FUNC( QT_QLIST_PUSH_FRONT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->push_front( hb_param( 2, HB_IT_ANY ) );
}

/* int removeAll ( const T & value ) */
HB_FUNC( QT_QLIST_REMOVEALL )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retni( ( p )->removeAll( hb_param( 2, HB_IT_ANY ) ) );
}

/* void removeAt ( int i ) */
HB_FUNC( QT_QLIST_REMOVEAT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->removeAt( hb_parni( 2 ) );
}

/* void removeFirst () */
HB_FUNC( QT_QLIST_REMOVEFIRST )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->removeFirst();
}

/* void removeLast () */
HB_FUNC( QT_QLIST_REMOVELAST )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->removeLast();
}

/* bool removeOne ( const T & value ) */
HB_FUNC( QT_QLIST_REMOVEONE )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retl( ( p )->removeOne( hb_param( 2, HB_IT_ANY ) ) );
}

/* void replace ( int i, const T & value ) */
HB_FUNC( QT_QLIST_REPLACE )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->replace( hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) );
}

/* int size () const */
HB_FUNC( QT_QLIST_SIZE )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retni( ( p )->size() );
}

/* bool startsWith ( const T & value ) const */
HB_FUNC( QT_QLIST_STARTSWITH )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retl( ( p )->startsWith( hb_param( 2, HB_IT_ANY ) ) );
}

/* void swap ( int i, int j ) */
HB_FUNC( QT_QLIST_SWAP )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      ( p )->swap( hb_parni( 2 ), hb_parni( 3 ) );
}

/* T takeAt ( int i ) */
HB_FUNC( QT_QLIST_TAKEAT )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->takeAt( hb_parni( 2 ) ) );
}

/* T takeFirst () */
HB_FUNC( QT_QLIST_TAKEFIRST )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->takeFirst() );
}

/* T takeLast () */
HB_FUNC( QT_QLIST_TAKELAST )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->takeLast() );
}

/* T value ( int i ) const */
HB_FUNC( QT_QLIST_VALUE )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->value( hb_parni( 2 ) ) );
}

/* T value ( int i, const T & defaultValue ) const */
HB_FUNC( QT_QLIST_VALUE_1 )
{
   QList< void *> * p = hbqt_par_QList( 1 );
   if( p )
      hb_retptr( ( p )->value( hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
