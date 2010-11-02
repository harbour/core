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
 *  Constructed[ 11/11 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QButtonGroup>


/*
 * QButtonGroup ( QObject * parent = 0 )
 * ~QButtonGroup ()
 */

typedef struct
{
   QPointer< QButtonGroup > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QButtonGroup;

HBQT_GC_FUNC( hbqt_gcRelease_QButtonGroup )
{
   HBQT_GC_T_QButtonGroup * p = ( HBQT_GC_T_QButtonGroup * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QButtonGroup * ph = p->ph;
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

void * hbqt_gcAllocate_QButtonGroup( void * pObj, bool bNew )
{
   HBQT_GC_T_QButtonGroup * p = ( HBQT_GC_T_QButtonGroup * ) hb_gcAllocate( sizeof( HBQT_GC_T_QButtonGroup ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QButtonGroup >( ( QButtonGroup * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QButtonGroup;
   p->type = HBQT_TYPE_QButtonGroup;

   return p;
}

HB_FUNC( QT_QBUTTONGROUP )
{
   QButtonGroup * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QButtonGroup( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QButtonGroup() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QButtonGroup( ( void * ) pObj, true ) );
}

/* void addButton ( QAbstractButton * button ) */
HB_FUNC( QT_QBUTTONGROUP_ADDBUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->addButton( hbqt_par_QAbstractButton( 2 ) );
}

/* void addButton ( QAbstractButton * button, int id ) */
HB_FUNC( QT_QBUTTONGROUP_ADDBUTTON_1 )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->addButton( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) );
}

/* QAbstractButton * button ( int id ) const */
HB_FUNC( QT_QBUTTONGROUP_BUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->button( hb_parni( 2 ) ), false ) );
}

/* QList<QAbstractButton *> buttons () const */
HB_FUNC( QT_QBUTTONGROUP_BUTTONS )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAbstractButton *>( ( p )->buttons() ), true ) );
}

/* QAbstractButton * checkedButton () const */
HB_FUNC( QT_QBUTTONGROUP_CHECKEDBUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->checkedButton(), false ) );
}

/* int checkedId () const */
HB_FUNC( QT_QBUTTONGROUP_CHECKEDID )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retni( ( p )->checkedId() );
}

/* bool exclusive () const */
HB_FUNC( QT_QBUTTONGROUP_EXCLUSIVE )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retl( ( p )->exclusive() );
}

/* int id ( QAbstractButton * button ) const */
HB_FUNC( QT_QBUTTONGROUP_ID )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retni( ( p )->id( hbqt_par_QAbstractButton( 2 ) ) );
}

/* void removeButton ( QAbstractButton * button ) */
HB_FUNC( QT_QBUTTONGROUP_REMOVEBUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->removeButton( hbqt_par_QAbstractButton( 2 ) );
}

/* void setExclusive ( bool ) */
HB_FUNC( QT_QBUTTONGROUP_SETEXCLUSIVE )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->setExclusive( hb_parl( 2 ) );
}

/* void setId ( QAbstractButton * button, int id ) */
HB_FUNC( QT_QBUTTONGROUP_SETID )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->setId( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
