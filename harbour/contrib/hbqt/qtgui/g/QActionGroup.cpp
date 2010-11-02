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
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QActionGroup>


/* QActionGroup ( QObject * parent )
 * ~QActionGroup ()
 */

typedef struct
{
   QPointer< QActionGroup > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QActionGroup;

HBQT_GC_FUNC( hbqt_gcRelease_QActionGroup )
{
   QActionGroup  * ph = NULL;
   HBQT_GC_T_QActionGroup * p = ( HBQT_GC_T_QActionGroup * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QActionGroup( void * pObj, bool bNew )
{
   HBQT_GC_T_QActionGroup * p = ( HBQT_GC_T_QActionGroup * ) hb_gcAllocate( sizeof( HBQT_GC_T_QActionGroup ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QActionGroup >( ( QActionGroup * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QActionGroup;
   p->type = HBQT_TYPE_QActionGroup;

   return p;
}

HB_FUNC( QT_QACTIONGROUP )
{
   QActionGroup * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QActionGroup( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QActionGroup( ( void * ) pObj, true ) );
}

/* QList<QAction *> actions () const */
HB_FUNC( QT_QACTIONGROUP_ACTIONS )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAction *>( ( p )->actions() ), true ) );
}

/* QAction * addAction ( QAction * action ) */
HB_FUNC( QT_QACTIONGROUP_ADDACTION )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hbqt_par_QAction( 2 ) ), false ) );
}

/* QAction * addAction ( const QString & text ) */
HB_FUNC( QT_QACTIONGROUP_ADDACTION_1 )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QIcon & icon, const QString & text ) */
HB_FUNC( QT_QACTIONGROUP_ADDACTION_2 )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * checkedAction () const */
HB_FUNC( QT_QACTIONGROUP_CHECKEDACTION )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->checkedAction(), false ) );
}

/* bool isEnabled () const */
HB_FUNC( QT_QACTIONGROUP_ISENABLED )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
}

/* bool isExclusive () const */
HB_FUNC( QT_QACTIONGROUP_ISEXCLUSIVE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retl( ( p )->isExclusive() );
}

/* bool isVisible () const */
HB_FUNC( QT_QACTIONGROUP_ISVISIBLE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
}

/* void removeAction ( QAction * action ) */
HB_FUNC( QT_QACTIONGROUP_REMOVEACTION )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->removeAction( hbqt_par_QAction( 2 ) );
}

/* void setDisabled ( bool b ) */
HB_FUNC( QT_QACTIONGROUP_SETDISABLED )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setDisabled( hb_parl( 2 ) );
}

/* void setEnabled ( bool ) */
HB_FUNC( QT_QACTIONGROUP_SETENABLED )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
}

/* void setExclusive ( bool ) */
HB_FUNC( QT_QACTIONGROUP_SETEXCLUSIVE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setExclusive( hb_parl( 2 ) );
}

/* void setVisible ( bool ) */
HB_FUNC( QT_QACTIONGROUP_SETVISIBLE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
