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
 *  enum ActionEvent { Trigger, Hover }
 *  enum MenuRole { NoRole, TextHeuristicRole, ApplicationSpecificRole, AboutQtRole, ..., QuitRole }
 */

/*
 *  Constructed[ 50/52 [ 96.15% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setShortcuts ( const QList<QKeySequence> & shortcuts )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //QList<QGraphicsWidget *> associatedGraphicsWidgets () const
 */

#include <QtCore/QPointer>

#include <QtGui/QAction>


/*
 * QAction ( QObject * parent )
 * QAction ( const QString & text, QObject * parent )
 * QAction ( const QIcon & icon, const QString & text, QObject * parent )
 * ~QAction ()
 */

typedef struct
{
   QPointer< QAction > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAction;

HBQT_GC_FUNC( hbqt_gcRelease_QAction )
{
   QAction  * ph = NULL ;
   HBQT_GC_T_QAction * p = ( HBQT_GC_T_QAction * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QAction   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QAction   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QAction          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QAction    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QAction    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAction( void * pObj, bool bNew )
{
   HBQT_GC_T_QAction * p = ( HBQT_GC_T_QAction * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAction ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAction >( ( QAction * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAction;
   p->type = HBQT_TYPE_QAction;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAction  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAction", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QACTION )
{
   QAction * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
      pObj = new QAction( hbqt_par_QObject( 1 ) ) ;
   else if( HB_ISPOINTER( 2 ) )
      pObj = new QAction( hbqt_par_QString( 1 ), hbqt_par_QObject( 2 ) ) ;
   else if( HB_ISPOINTER( 3 ) )
      pObj = new QAction( *hbqt_par_QIcon( 1 ), hbqt_par_QString( 2 ), hbqt_par_QObject( 3 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QAction( ( void * ) pObj, true ) );
}

/*
 * QActionGroup * actionGroup () const
 */
HB_FUNC( QT_QACTION_ACTIONGROUP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QActionGroup( ( p )->actionGroup(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ACTIONGROUP FP=hb_retptrGC( hbqt_gcAllocate_QActionGroup( ( p )->actionGroup(), false ) ); p is NULL" ) );
   }
}

/*
 * void activate ( ActionEvent event )
 */
HB_FUNC( QT_QACTION_ACTIVATE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->activate( ( QAction::ActionEvent ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ACTIVATE FP=( p )->activate( ( QAction::ActionEvent ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QList<QWidget *> associatedWidgets () const
 */
HB_FUNC( QT_QACTION_ASSOCIATEDWIDGETS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWidget *>( ( p )->associatedWidgets() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ASSOCIATEDWIDGETS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWidget *>( ( p )->associatedWidgets() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool autoRepeat () const
 */
HB_FUNC( QT_QACTION_AUTOREPEAT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->autoRepeat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_AUTOREPEAT FP=hb_retl( ( p )->autoRepeat() ); p is NULL" ) );
   }
}

/*
 * QVariant data () const
 */
HB_FUNC( QT_QACTION_DATA )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_DATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QACTION_FONT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QACTION_ICON )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString iconText () const
 */
HB_FUNC( QT_QACTION_ICONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retc( ( p )->iconText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ICONTEXT FP=hb_retc( ( p )->iconText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QACTION_ISCHECKABLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isCheckable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ISCHECKABLE FP=hb_retl( ( p )->isCheckable() ); p is NULL" ) );
   }
}

/*
 * bool isChecked () const
 */
HB_FUNC( QT_QACTION_ISCHECKED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isChecked() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ISCHECKED FP=hb_retl( ( p )->isChecked() ); p is NULL" ) );
   }
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QACTION_ISENABLED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ISENABLED FP=hb_retl( ( p )->isEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isIconVisibleInMenu () const
 */
HB_FUNC( QT_QACTION_ISICONVISIBLEINMENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isIconVisibleInMenu() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ISICONVISIBLEINMENU FP=hb_retl( ( p )->isIconVisibleInMenu() ); p is NULL" ) );
   }
}

/*
 * bool isSeparator () const
 */
HB_FUNC( QT_QACTION_ISSEPARATOR )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isSeparator() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ISSEPARATOR FP=hb_retl( ( p )->isSeparator() ); p is NULL" ) );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QACTION_ISVISIBLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_ISVISIBLE FP=hb_retl( ( p )->isVisible() ); p is NULL" ) );
   }
}

/*
 * QMenu * menu () const
 */
HB_FUNC( QT_QACTION_MENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_MENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) ); p is NULL" ) );
   }
}

/*
 * MenuRole menuRole () const
 */
HB_FUNC( QT_QACTION_MENUROLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retni( ( QAction::MenuRole ) ( p )->menuRole() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_MENUROLE FP=hb_retni( ( QAction::MenuRole ) ( p )->menuRole() ); p is NULL" ) );
   }
}

/*
 * QWidget * parentWidget () const
 */
HB_FUNC( QT_QACTION_PARENTWIDGET )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_PARENTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * void setActionGroup ( QActionGroup * group )
 */
HB_FUNC( QT_QACTION_SETACTIONGROUP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setActionGroup( hbqt_par_QActionGroup( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETACTIONGROUP FP=( p )->setActionGroup( hbqt_par_QActionGroup( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoRepeat ( bool )
 */
HB_FUNC( QT_QACTION_SETAUTOREPEAT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setAutoRepeat( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETAUTOREPEAT FP=( p )->setAutoRepeat( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCheckable ( bool )
 */
HB_FUNC( QT_QACTION_SETCHECKABLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setCheckable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETCHECKABLE FP=( p )->setCheckable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setData ( const QVariant & userData )
 */
HB_FUNC( QT_QACTION_SETDATA )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setData( *hbqt_par_QVariant( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETDATA FP=( p )->setData( *hbqt_par_QVariant( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QACTION_SETFONT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QACTION_SETICON )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETICON FP=( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setIconText ( const QString & text )
 */
HB_FUNC( QT_QACTION_SETICONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setIconText( QAction::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETICONTEXT FP=( p )->setIconText( QAction::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setIconVisibleInMenu ( bool visible )
 */
HB_FUNC( QT_QACTION_SETICONVISIBLEINMENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setIconVisibleInMenu( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETICONVISIBLEINMENU FP=( p )->setIconVisibleInMenu( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMenu ( QMenu * menu )
 */
HB_FUNC( QT_QACTION_SETMENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setMenu( hbqt_par_QMenu( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETMENU FP=( p )->setMenu( hbqt_par_QMenu( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMenuRole ( MenuRole menuRole )
 */
HB_FUNC( QT_QACTION_SETMENUROLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setMenuRole( ( QAction::MenuRole ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETMENUROLE FP=( p )->setMenuRole( ( QAction::MenuRole ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSeparator ( bool b )
 */
HB_FUNC( QT_QACTION_SETSEPARATOR )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setSeparator( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETSEPARATOR FP=( p )->setSeparator( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setShortcut ( const QKeySequence & shortcut )
 */
HB_FUNC( QT_QACTION_SETSHORTCUT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setShortcut( *hbqt_par_QKeySequence( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETSHORTCUT FP=( p )->setShortcut( *hbqt_par_QKeySequence( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setShortcutContext ( Qt::ShortcutContext context )
 */
HB_FUNC( QT_QACTION_SETSHORTCUTCONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setShortcutContext( ( Qt::ShortcutContext ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETSHORTCUTCONTEXT FP=( p )->setShortcutContext( ( Qt::ShortcutContext ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setShortcuts ( QKeySequence::StandardKey key )
 */
HB_FUNC( QT_QACTION_SETSHORTCUTS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setShortcuts( ( QKeySequence::StandardKey ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETSHORTCUTS FP=( p )->setShortcuts( ( QKeySequence::StandardKey ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QACTION_SETSTATUSTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setStatusTip( QAction::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETSTATUSTIP FP=( p )->setStatusTip( QAction::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QACTION_SETTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setText( QAction::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETTEXT FP=( p )->setText( QAction::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( const QString & tip )
 */
HB_FUNC( QT_QACTION_SETTOOLTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setToolTip( QAction::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETTOOLTIP FP=( p )->setToolTip( QAction::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setWhatsThis ( const QString & what )
 */
HB_FUNC( QT_QACTION_SETWHATSTHIS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setWhatsThis( QAction::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETWHATSTHIS FP=( p )->setWhatsThis( QAction::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QKeySequence shortcut () const
 */
HB_FUNC( QT_QACTION_SHORTCUT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QKeySequence( new QKeySequence( ( p )->shortcut() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SHORTCUT FP=hb_retptrGC( hbqt_gcAllocate_QKeySequence( new QKeySequence( ( p )->shortcut() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::ShortcutContext shortcutContext () const
 */
HB_FUNC( QT_QACTION_SHORTCUTCONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retni( ( Qt::ShortcutContext ) ( p )->shortcutContext() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SHORTCUTCONTEXT FP=hb_retni( ( Qt::ShortcutContext ) ( p )->shortcutContext() ); p is NULL" ) );
   }
}

/*
 * QList<QKeySequence> shortcuts () const
 */
HB_FUNC( QT_QACTION_SHORTCUTS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QKeySequence>( ( p )->shortcuts() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SHORTCUTS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QKeySequence>( ( p )->shortcuts() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool showStatusText ( QWidget * widget = 0 )
 */
HB_FUNC( QT_QACTION_SHOWSTATUSTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->showStatusText( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SHOWSTATUSTEXT FP=hb_retl( ( p )->showStatusText( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QACTION_STATUSTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retc( ( p )->statusTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_STATUSTIP FP=hb_retc( ( p )->statusTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QACTION_TEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QACTION_TOOLTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retc( ( p )->toolTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_TOOLTIP FP=hb_retc( ( p )->toolTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QACTION_WHATSTHIS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retc( ( p )->whatsThis().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_WHATSTHIS FP=hb_retc( ( p )->whatsThis().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void hover ()
 */
HB_FUNC( QT_QACTION_HOVER )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->hover();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_HOVER FP=( p )->hover(); p is NULL" ) );
   }
}

/*
 * void setChecked ( bool )
 */
HB_FUNC( QT_QACTION_SETCHECKED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setChecked( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETCHECKED FP=( p )->setChecked( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDisabled ( bool b )
 */
HB_FUNC( QT_QACTION_SETDISABLED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setDisabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETDISABLED FP=( p )->setDisabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEnabled ( bool )
 */
HB_FUNC( QT_QACTION_SETENABLED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETENABLED FP=( p )->setEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVisible ( bool )
 */
HB_FUNC( QT_QACTION_SETVISIBLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void toggle ()
 */
HB_FUNC( QT_QACTION_TOGGLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->toggle();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_TOGGLE FP=( p )->toggle(); p is NULL" ) );
   }
}

/*
 * void trigger ()
 */
HB_FUNC( QT_QACTION_TRIGGER )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->trigger();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTION_TRIGGER FP=( p )->trigger(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
