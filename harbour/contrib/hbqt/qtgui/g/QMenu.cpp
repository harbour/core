/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 33/36 [ 91.67% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *
 *  *** Commented out protos which construct fine but do not compile ***
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
   QMenu  * ph = NULL ;
   HBQT_GC_T_QMenu * p = ( HBQT_GC_T_QMenu * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMenu   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMenu   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMenu          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMenu    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMenu    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMenu( void * pObj, bool bNew )
{
   HBQT_GC_T_QMenu * p = ( HBQT_GC_T_QMenu * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMenu ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMenu >( ( QMenu * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMenu;
   p->type = HBQT_TYPE_QMenu;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMenu  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMenu", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMENU )
{
   QMenu * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISCHAR( 1 ) )
   {
      pObj =  new QMenu( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj =  new QMenu( hbqt_par_QWidget( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMenu( ( void * ) pObj, true ) );
}

/*
 * QAction * actionAt ( const QPoint & pt ) const
 */
HB_FUNC( QT_QMENU_ACTIONAT )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ACTIONAT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QRect actionGeometry ( QAction * act ) const
 */
HB_FUNC( QT_QMENU_ACTIONGEOMETRY )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->actionGeometry( hbqt_par_QAction( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ACTIONGEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->actionGeometry( hbqt_par_QAction( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QAction * activeAction () const
 */
HB_FUNC( QT_QMENU_ACTIVEACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->activeAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ACTIVEACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->activeAction(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QMENU_ADDACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenu::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenu::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QMENU_ADDACTION_1 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenu::tr( hb_parc( 3 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDACTION_1 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenu::tr( hb_parc( 3 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QString & text, const QObject * receiver, const char * member, const QKeySequence & shortcut = 0 )
 */
HB_FUNC( QT_QMENU_ADDACTION_2 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenu::tr( hb_parc( 2 ) ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ), *hbqt_par_QKeySequence( 5 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDACTION_2 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenu::tr( hb_parc( 2 ) ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ), *hbqt_par_QKeySequence( 5 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member, const QKeySequence & shortcut = 0 )
 */
HB_FUNC( QT_QMENU_ADDACTION_3 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenu::tr( hb_parc( 3 ) ), hbqt_par_QObject( 4 ), hbqt_par_char( 5 ), *hbqt_par_QKeySequence( 6 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDACTION_3 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenu::tr( hb_parc( 3 ) ), hbqt_par_QObject( 4 ), hbqt_par_char( 5 ), *hbqt_par_QKeySequence( 6 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QMENU_ADDACTION_4 )
{
  HBQT_GC_T_QMenu * q = ( HBQT_GC_T_QMenu * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
  HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );

  HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QMENU_ADDACTION()" ) );
  if( p && p->ph && q && q->ph )
  {
     HB_TRACE( HB_TR_DEBUG, ( "QT_QMENU_ADDACTION() Qt oject: %p is attached to: %p", ( void * ) p->ph, ( void * ) q->ph ) );
     p->bNew = HB_FALSE;
     ( q->ph )->addAction( ( QAction * ) p->ph );
  }
}

/*
 * QAction * addMenu ( QMenu * menu )
 */
HB_FUNC( QT_QMENU_ADDMENU )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addMenu( hbqt_par_QMenu( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDMENU FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addMenu( hbqt_par_QMenu( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QMenu * addMenu ( const QString & title )
 */
HB_FUNC( QT_QMENU_ADDMENU_1 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( QMenu::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDMENU_1 FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( QMenu::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QMenu * addMenu ( const QIcon & icon, const QString & title )
 */
HB_FUNC( QT_QMENU_ADDMENU_2 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenu::tr( hb_parc( 3 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDMENU_2 FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenu::tr( hb_parc( 3 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addSeparator ()
 */
HB_FUNC( QT_QMENU_ADDSEPARATOR )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ADDSEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QMENU_CLEAR )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * QAction * defaultAction () const
 */
HB_FUNC( QT_QMENU_DEFAULTACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->defaultAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_DEFAULTACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->defaultAction(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * exec ()
 */
HB_FUNC( QT_QMENU_EXEC )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->exec(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_EXEC FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->exec(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * exec ( const QPoint & p, QAction * action = 0 )
 */
HB_FUNC( QT_QMENU_EXEC_1 )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->exec( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_EXEC_1 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->exec( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void hideTearOffMenu ()
 */
HB_FUNC( QT_QMENU_HIDETEAROFFMENU )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->hideTearOffMenu();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_HIDETEAROFFMENU FP=( p )->hideTearOffMenu(); p is NULL" ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QMENU_ICON )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) ); p is NULL" ) );
   }
}

/*
 * QAction * insertMenu ( QAction * before, QMenu * menu )
 */
HB_FUNC( QT_QMENU_INSERTMENU )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_INSERTMENU FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * insertSeparator ( QAction * before )
 */
HB_FUNC( QT_QMENU_INSERTSEPARATOR )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_INSERTSEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QMENU_ISEMPTY )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ISEMPTY FP=hb_retl( ( p )->isEmpty() ); p is NULL" ) );
   }
}

/*
 * bool isTearOffEnabled () const
 */
HB_FUNC( QT_QMENU_ISTEAROFFENABLED )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->isTearOffEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ISTEAROFFENABLED FP=hb_retl( ( p )->isTearOffEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isTearOffMenuVisible () const
 */
HB_FUNC( QT_QMENU_ISTEAROFFMENUVISIBLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->isTearOffMenuVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_ISTEAROFFMENUVISIBLE FP=hb_retl( ( p )->isTearOffMenuVisible() ); p is NULL" ) );
   }
}

/*
 * QAction * menuAction () const
 */
HB_FUNC( QT_QMENU_MENUACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->menuAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_MENUACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->menuAction(), false ) ); p is NULL" ) );
   }
}

/*
 * void popup ( const QPoint & p, QAction * atAction = 0 )
 */
HB_FUNC( QT_QMENU_POPUP )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->popup( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_POPUP FP=( p )->popup( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool separatorsCollapsible () const
 */
HB_FUNC( QT_QMENU_SEPARATORSCOLLAPSIBLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retl( ( p )->separatorsCollapsible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_SEPARATORSCOLLAPSIBLE FP=hb_retl( ( p )->separatorsCollapsible() ); p is NULL" ) );
   }
}

/*
 * void setActiveAction ( QAction * act )
 */
HB_FUNC( QT_QMENU_SETACTIVEACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setActiveAction( hbqt_par_QAction( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_SETACTIVEACTION FP=( p )->setActiveAction( hbqt_par_QAction( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultAction ( QAction * act )
 */
HB_FUNC( QT_QMENU_SETDEFAULTACTION )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setDefaultAction( hbqt_par_QAction( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_SETDEFAULTACTION FP=( p )->setDefaultAction( hbqt_par_QAction( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QMENU_SETICON )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_SETICON FP=( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setSeparatorsCollapsible ( bool collapse )
 */
HB_FUNC( QT_QMENU_SETSEPARATORSCOLLAPSIBLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setSeparatorsCollapsible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_SETSEPARATORSCOLLAPSIBLE FP=( p )->setSeparatorsCollapsible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTearOffEnabled ( bool )
 */
HB_FUNC( QT_QMENU_SETTEAROFFENABLED )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setTearOffEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_SETTEAROFFENABLED FP=( p )->setTearOffEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTitle ( const QString & title )
 */
HB_FUNC( QT_QMENU_SETTITLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      ( p )->setTitle( QMenu::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_SETTITLE FP=( p )->setTitle( QMenu::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString title () const
 */
HB_FUNC( QT_QMENU_TITLE )
{
   QMenu * p = hbqt_par_QMenu( 1 );
   if( p )
      hb_retc( ( p )->title().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENU_TITLE FP=hb_retc( ( p )->title().toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
