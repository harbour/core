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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QAction * defaultAction () const
 *  // OSMenuRef macMenu ()
 *  // void setDefaultAction ( QAction * act )
 */

#include <QtCore/QPointer>

#include <QtGui/QMenuBar>


/*
 * QMenuBar ( QWidget * parent = 0 )
 * ~QMenuBar ()
 */

typedef struct
{
   QPointer< QMenuBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMenuBar;

HBQT_GC_FUNC( hbqt_gcRelease_QMenuBar )
{
   QMenuBar  * ph = NULL;
   HBQT_GC_T_QMenuBar * p = ( HBQT_GC_T_QMenuBar * ) Cargo;

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

void * hbqt_gcAllocate_QMenuBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QMenuBar * p = ( HBQT_GC_T_QMenuBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMenuBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMenuBar >( ( QMenuBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMenuBar;
   p->type = HBQT_TYPE_QMenuBar;

   return p;
}

HB_FUNC( QT_QMENUBAR )
{
   QMenuBar * pObj = NULL;

   pObj = new QMenuBar( HB_ISPOINTER( 1 ) ? hbqt_par_QWidget( 1 ) : 0 ) ;

   hb_retptrGC( hbqt_gcAllocate_QMenuBar( ( void * ) pObj, true ) );
}

/* QAction * activeAction () const */
HB_FUNC( QT_QMENUBAR_ACTIVEACTION )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->activeAction(), false ) );
}

/* QAction * addAction ( const QString & text ) */
HB_FUNC( QT_QMENUBAR_ADDACTION )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QString & text, const QObject * receiver, const char * member ) */
HB_FUNC( QT_QMENUBAR_ADDACTION_1 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), ( const char * ) hb_parc( 4 ) ), false ) );
      hb_strfree( pText );
   }
}

/* void addAction ( QAction * action ) */
HB_FUNC( QT_QMENUBAR_ADDACTION_2 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->addAction( hbqt_par_QAction( 2 ) );
}

/* QAction * addMenu ( QMenu * menu ) */
HB_FUNC( QT_QMENUBAR_ADDMENU )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addMenu( hbqt_par_QMenu( 2 ) ), false ) );
}

/* QMenu * addMenu ( const QString & title ) */
HB_FUNC( QT_QMENUBAR_ADDMENU_1 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QMenu * addMenu ( const QIcon & icon, const QString & title ) */
HB_FUNC( QT_QMENUBAR_ADDMENU_2 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addSeparator () */
HB_FUNC( QT_QMENUBAR_ADDSEPARATOR )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) );
}

/* void clear () */
HB_FUNC( QT_QMENUBAR_CLEAR )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->clear();
}

/* QAction * insertMenu ( QAction * before, QMenu * menu ) */
HB_FUNC( QT_QMENUBAR_INSERTMENU )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ), false ) );
}

/* QAction * insertSeparator ( QAction * before ) */
HB_FUNC( QT_QMENUBAR_INSERTSEPARATOR )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
}

/* bool isDefaultUp () const */
HB_FUNC( QT_QMENUBAR_ISDEFAULTUP )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retl( ( p )->isDefaultUp() );
}

/* void setActiveAction ( QAction * act ) */
HB_FUNC( QT_QMENUBAR_SETACTIVEACTION )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->setActiveAction( hbqt_par_QAction( 2 ) );
}

/* void setDefaultUp ( bool ) */
HB_FUNC( QT_QMENUBAR_SETDEFAULTUP )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->setDefaultUp( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
