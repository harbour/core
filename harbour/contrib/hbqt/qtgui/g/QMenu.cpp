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
 *  Constructed[ 33/33 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // OSMenuRef macMenu ( OSMenuRef merge = 0 )
 *  // HMENU wceMenu ( bool create = false )
 */

#include <QtCore/QPointer>

#include <QtGui/QMenu>


/*
 * QMenu ( QWidget * parent = 0 )
 * QMenu ( const QString & title, QWidget * parent = 0 )
 * ~QMenu ()
 */

typedef struct
{
   QPointer< QMenu > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMenu;

HBQT_GC_FUNC( hbqt_gcRelease_QMenu )
{
   HBQT_GC_T_QMenu * p = ( HBQT_GC_T_QMenu * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QMenu * ph = p->ph;
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

void * hbqt_gcAllocate_QMenu( void * pObj, bool bNew )
{
   HBQT_GC_T_QMenu * p = ( HBQT_GC_T_QMenu * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMenu ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMenu >( ( QMenu * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMenu;
   p->type = HBQT_TYPE_QMenu;

   return p;
}

HB_FUNC( QT_QMENU )
{
   QMenu * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QMenu( hbqt_par_QString( 1 ), HB_ISPOINTER( 2 ) ? hbqt_par_QWidget( 2 ) : 0 ) ;
   }
   else
   {
      pObj = new QMenu( HB_ISPOINTER( 1 ) ? hbqt_par_QWidget( 1 ) : 0 ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMenu( ( void * ) pObj, true ) );
}

/* QAction * actionAt ( const QPoint & pt ) const */
HB_FUNC( QT_QMENU_ACTIONAT )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( *hbqt_par_QPoint( 2 ) ), false ) );
}

/* QRect actionGeometry ( QAction * act ) const */
HB_FUNC( QT_QMENU_ACTIONGEOMETRY )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->actionGeometry( hbqt_par_QAction( 2 ) ) ), true ) );
}

/* QAction * activeAction () const */
HB_FUNC( QT_QMENU_ACTIVEACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->activeAction(), false ) );
}

/* QAction * addAction ( const QString & text ) */
HB_FUNC( QT_QMENU_ADDACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QIcon & icon, const QString & text ) */
HB_FUNC( QT_QMENU_ADDACTION_1 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QString & text, const QObject * receiver, const char * member, const QKeySequence & shortcut = 0 ) */
HB_FUNC( QT_QMENU_ADDACTION_2 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), ( const char * ) hb_parc( 4 ), *hbqt_par_QKeySequence( 5 ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member, const QKeySequence & shortcut = 0 ) */
HB_FUNC( QT_QMENU_ADDACTION_3 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ), hbqt_par_QObject( 4 ), ( const char * ) hb_parc( 5 ), *hbqt_par_QKeySequence( 6 ) ), false ) );
      hb_strfree( pText );
   }
}

/* void addAction ( QAction * action ) */
HB_FUNC( QT_QMENU_ADDACTION_4 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->addAction( hbqt_par_QAction( 2 ) );
}

/* QAction * addMenu ( QMenu * menu ) */
HB_FUNC( QT_QMENU_ADDMENU )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addMenu( hbqt_par_QMenu( 2 ) ), false ) );
}

/* QMenu * addMenu ( const QString & title ) */
HB_FUNC( QT_QMENU_ADDMENU_1 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QMenu * addMenu ( const QIcon & icon, const QString & title ) */
HB_FUNC( QT_QMENU_ADDMENU_2 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QAction * addSeparator () */
HB_FUNC( QT_QMENU_ADDSEPARATOR )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) );
}

/* void clear () */
HB_FUNC( QT_QMENU_CLEAR )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->clear();
}

/* QAction * defaultAction () const */
HB_FUNC( QT_QMENU_DEFAULTACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->defaultAction(), false ) );
}

/* QAction * exec () */
HB_FUNC( QT_QMENU_EXEC )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->exec(), false ) );
}

/* QAction * exec ( const QPoint & p, QAction * action = 0 ) */
HB_FUNC( QT_QMENU_EXEC_1 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->exec( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) ), false ) );
}

/* void hideTearOffMenu () */
HB_FUNC( QT_QMENU_HIDETEAROFFMENU )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->hideTearOffMenu();
}

/* QIcon icon () const */
HB_FUNC( QT_QMENU_ICON )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
}

/* QAction * insertMenu ( QAction * before, QMenu * menu ) */
HB_FUNC( QT_QMENU_INSERTMENU )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ), false ) );
}

/* QAction * insertSeparator ( QAction * before ) */
HB_FUNC( QT_QMENU_INSERTSEPARATOR )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
}

/* bool isEmpty () const */
HB_FUNC( QT_QMENU_ISEMPTY )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool isTearOffEnabled () const */
HB_FUNC( QT_QMENU_ISTEAROFFENABLED )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->isTearOffEnabled() );
}

/* bool isTearOffMenuVisible () const */
HB_FUNC( QT_QMENU_ISTEAROFFMENUVISIBLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->isTearOffMenuVisible() );
}

/* QAction * menuAction () const */
HB_FUNC( QT_QMENU_MENUACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->menuAction(), false ) );
}

/* void popup ( const QPoint & p, QAction * atAction = 0 ) */
HB_FUNC( QT_QMENU_POPUP )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->popup( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) );
}

/* bool separatorsCollapsible () const */
HB_FUNC( QT_QMENU_SEPARATORSCOLLAPSIBLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->separatorsCollapsible() );
}

/* void setActiveAction ( QAction * act ) */
HB_FUNC( QT_QMENU_SETACTIVEACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setActiveAction( hbqt_par_QAction( 2 ) );
}

/* void setDefaultAction ( QAction * act ) */
HB_FUNC( QT_QMENU_SETDEFAULTACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setDefaultAction( hbqt_par_QAction( 2 ) );
}

/* void setIcon ( const QIcon & icon ) */
HB_FUNC( QT_QMENU_SETICON )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
}

/* void setSeparatorsCollapsible ( bool collapse ) */
HB_FUNC( QT_QMENU_SETSEPARATORSCOLLAPSIBLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setSeparatorsCollapsible( hb_parl( 2 ) );
}

/* void setTearOffEnabled ( bool ) */
HB_FUNC( QT_QMENU_SETTEAROFFENABLED )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setTearOffEnabled( hb_parl( 2 ) );
}

/* void setTitle ( const QString & title ) */
HB_FUNC( QT_QMENU_SETTITLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString title () const */
HB_FUNC( QT_QMENU_TITLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
