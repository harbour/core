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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ActionEvent { Trigger, Hover }
 *  enum MenuRole { NoRole, TextHeuristicRole, ApplicationSpecificRole, AboutQtRole, ..., QuitRole }
 */

/*
 *  Constructed[ 48/52 [ 92.31% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QGraphicsWidget *> associatedGraphicsWidgets () const
 *  QList<QWidget *> associatedWidgets () const
 *  void setShortcuts ( const QList<QKeySequence> & shortcuts )
 *  QList<QKeySequence> shortcuts () const
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
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QAction > pq;
} QGC_POINTER_QAction;

QT_G_FUNC( release_QAction )
{
   QGC_POINTER_QAction * p = ( QGC_POINTER_QAction * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QAction                      p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QAction                     ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QAction * ) p->ph )->~QAction();
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QAction                     Object deleted!" ) );
         #if defined(__debug__)
            just_debug( "  YES release_QAction                     %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QAction                     Object Name Missing!" ) );
         #if defined(__debug__)
            just_debug( "  NO  release_QAction" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QAction                     Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QAction" );
      #endif
   }
}

void * gcAllocate_QAction( void * pObj )
{
   QGC_POINTER_QAction * p = ( QGC_POINTER_QAction * ) hb_gcAllocate( sizeof( QGC_POINTER_QAction ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QAction;
   new( & p->pq ) QPointer< QAction >( ( QAction * ) pObj );
   #if defined(__debug__)
      just_debug( "          new_QAction                     %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QACTION )
{
   void * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
      pObj = new QAction( hbqt_par_QObject( 1 ) ) ;
   else if( HB_ISPOINTER( 2 ) )
      pObj = new QAction( hbqt_par_QString( 1 ), hbqt_par_QObject( 2 ) ) ;
   else if( HB_ISPOINTER( 3 ) )
      pObj = new QAction( *hbqt_par_QIcon( 1 ), hbqt_par_QString( 2 ), hbqt_par_QObject( 3 ) ) ;

   hb_retptrGC( gcAllocate_QAction( pObj ) );
}
/*
 * QActionGroup * actionGroup () const
 */
HB_FUNC( QT_QACTION_ACTIONGROUP )
{
   hb_retptr( ( QActionGroup* ) hbqt_par_QAction( 1 )->actionGroup() );
}

/*
 * void activate ( ActionEvent event )
 */
HB_FUNC( QT_QACTION_ACTIVATE )
{
   hbqt_par_QAction( 1 )->activate( ( QAction::ActionEvent ) hb_parni( 2 ) );
}

/*
 * bool autoRepeat () const
 */
HB_FUNC( QT_QACTION_AUTOREPEAT )
{
   hb_retl( hbqt_par_QAction( 1 )->autoRepeat() );
}

/*
 * QVariant data () const
 */
HB_FUNC( QT_QACTION_DATA )
{
   hb_retptrGC( gcAllocate_QVariant( new QVariant( hbqt_par_QAction( 1 )->data() ) ) );
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QACTION_FONT )
{
   hb_retptrGC( gcAllocate_QFont( new QFont( hbqt_par_QAction( 1 )->font() ) ) );
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QACTION_ICON )
{
   hb_retptrGC( gcAllocate_QIcon( new QIcon( hbqt_par_QAction( 1 )->icon() ) ) );
}

/*
 * QString iconText () const
 */
HB_FUNC( QT_QACTION_ICONTEXT )
{
   hb_retc( hbqt_par_QAction( 1 )->iconText().toAscii().data() );
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QACTION_ISCHECKABLE )
{
   hb_retl( hbqt_par_QAction( 1 )->isCheckable() );
}

/*
 * bool isChecked () const
 */
HB_FUNC( QT_QACTION_ISCHECKED )
{
   hb_retl( hbqt_par_QAction( 1 )->isChecked() );
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QACTION_ISENABLED )
{
   hb_retl( hbqt_par_QAction( 1 )->isEnabled() );
}

/*
 * bool isIconVisibleInMenu () const
 */
HB_FUNC( QT_QACTION_ISICONVISIBLEINMENU )
{
   hb_retl( hbqt_par_QAction( 1 )->isIconVisibleInMenu() );
}

/*
 * bool isSeparator () const
 */
HB_FUNC( QT_QACTION_ISSEPARATOR )
{
   hb_retl( hbqt_par_QAction( 1 )->isSeparator() );
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QACTION_ISVISIBLE )
{
   hb_retl( hbqt_par_QAction( 1 )->isVisible() );
}

/*
 * QMenu * menu () const
 */
HB_FUNC( QT_QACTION_MENU )
{
   hb_retptr( ( QMenu* ) hbqt_par_QAction( 1 )->menu() );
}

/*
 * MenuRole menuRole () const
 */
HB_FUNC( QT_QACTION_MENUROLE )
{
   hb_retni( ( QAction::MenuRole ) hbqt_par_QAction( 1 )->menuRole() );
}

/*
 * QWidget * parentWidget () const
 */
HB_FUNC( QT_QACTION_PARENTWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QAction( 1 )->parentWidget() );
}

/*
 * void setActionGroup ( QActionGroup * group )
 */
HB_FUNC( QT_QACTION_SETACTIONGROUP )
{
   hbqt_par_QAction( 1 )->setActionGroup( hbqt_par_QActionGroup( 2 ) );
}

/*
 * void setAutoRepeat ( bool )
 */
HB_FUNC( QT_QACTION_SETAUTOREPEAT )
{
   hbqt_par_QAction( 1 )->setAutoRepeat( hb_parl( 2 ) );
}

/*
 * void setCheckable ( bool )
 */
HB_FUNC( QT_QACTION_SETCHECKABLE )
{
   hbqt_par_QAction( 1 )->setCheckable( hb_parl( 2 ) );
}

/*
 * void setData ( const QVariant & userData )
 */
HB_FUNC( QT_QACTION_SETDATA )
{
   hbqt_par_QAction( 1 )->setData( *hbqt_par_QVariant( 2 ) );
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QACTION_SETFONT )
{
   hbqt_par_QAction( 1 )->setFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QACTION_SETICON )
{
   hbqt_par_QAction( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setIconText ( const QString & text )
 */
HB_FUNC( QT_QACTION_SETICONTEXT )
{
   hbqt_par_QAction( 1 )->setIconText( hbqt_par_QString( 2 ) );
}

/*
 * void setIconVisibleInMenu ( bool visible )
 */
HB_FUNC( QT_QACTION_SETICONVISIBLEINMENU )
{
   hbqt_par_QAction( 1 )->setIconVisibleInMenu( hb_parl( 2 ) );
}

/*
 * void setMenu ( QMenu * menu )
 */
HB_FUNC( QT_QACTION_SETMENU )
{
   hbqt_par_QAction( 1 )->setMenu( hbqt_par_QMenu( 2 ) );
}

/*
 * void setMenuRole ( MenuRole menuRole )
 */
HB_FUNC( QT_QACTION_SETMENUROLE )
{
   hbqt_par_QAction( 1 )->setMenuRole( ( QAction::MenuRole ) hb_parni( 2 ) );
}

/*
 * void setSeparator ( bool b )
 */
HB_FUNC( QT_QACTION_SETSEPARATOR )
{
   hbqt_par_QAction( 1 )->setSeparator( hb_parl( 2 ) );
}

/*
 * void setShortcut ( const QKeySequence & shortcut )
 */
HB_FUNC( QT_QACTION_SETSHORTCUT )
{
   hbqt_par_QAction( 1 )->setShortcut( *hbqt_par_QKeySequence( 2 ) );
}

/*
 * void setShortcutContext ( Qt::ShortcutContext context )
 */
HB_FUNC( QT_QACTION_SETSHORTCUTCONTEXT )
{
   hbqt_par_QAction( 1 )->setShortcutContext( ( Qt::ShortcutContext ) hb_parni( 2 ) );
}

/*
 * void setShortcuts ( QKeySequence::StandardKey key )
 */
HB_FUNC( QT_QACTION_SETSHORTCUTS )
{
   hbqt_par_QAction( 1 )->setShortcuts( ( QKeySequence::StandardKey ) hb_parni( 2 ) );
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QACTION_SETSTATUSTIP )
{
   hbqt_par_QAction( 1 )->setStatusTip( hbqt_par_QString( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QACTION_SETTEXT )
{
   hbqt_par_QAction( 1 )->setText( hbqt_par_QString( 2 ) );
}

/*
 * void setToolTip ( const QString & tip )
 */
HB_FUNC( QT_QACTION_SETTOOLTIP )
{
   hbqt_par_QAction( 1 )->setToolTip( hbqt_par_QString( 2 ) );
}

/*
 * void setWhatsThis ( const QString & what )
 */
HB_FUNC( QT_QACTION_SETWHATSTHIS )
{
   hbqt_par_QAction( 1 )->setWhatsThis( hbqt_par_QString( 2 ) );
}

/*
 * QKeySequence shortcut () const
 */
HB_FUNC( QT_QACTION_SHORTCUT )
{
   hb_retptrGC( gcAllocate_QKeySequence( new QKeySequence( hbqt_par_QAction( 1 )->shortcut() ) ) );
}

/*
 * Qt::ShortcutContext shortcutContext () const
 */
HB_FUNC( QT_QACTION_SHORTCUTCONTEXT )
{
   hb_retni( ( Qt::ShortcutContext ) hbqt_par_QAction( 1 )->shortcutContext() );
}

/*
 * bool showStatusText ( QWidget * widget = 0 )
 */
HB_FUNC( QT_QACTION_SHOWSTATUSTEXT )
{
   hb_retl( hbqt_par_QAction( 1 )->showStatusText( hbqt_par_QWidget( 2 ) ) );
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QACTION_STATUSTIP )
{
   hb_retc( hbqt_par_QAction( 1 )->statusTip().toAscii().data() );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QACTION_TEXT )
{
   hb_retc( hbqt_par_QAction( 1 )->text().toAscii().data() );
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QACTION_TOOLTIP )
{
   hb_retc( hbqt_par_QAction( 1 )->toolTip().toAscii().data() );
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QACTION_WHATSTHIS )
{
   hb_retc( hbqt_par_QAction( 1 )->whatsThis().toAscii().data() );
}

/*
 * void hover ()
 */
HB_FUNC( QT_QACTION_HOVER )
{
   hbqt_par_QAction( 1 )->hover();
}

/*
 * void setChecked ( bool )
 */
HB_FUNC( QT_QACTION_SETCHECKED )
{
   hbqt_par_QAction( 1 )->setChecked( hb_parl( 2 ) );
}

/*
 * void setDisabled ( bool b )
 */
HB_FUNC( QT_QACTION_SETDISABLED )
{
   hbqt_par_QAction( 1 )->setDisabled( hb_parl( 2 ) );
}

/*
 * void setEnabled ( bool )
 */
HB_FUNC( QT_QACTION_SETENABLED )
{
   hbqt_par_QAction( 1 )->setEnabled( hb_parl( 2 ) );
}

/*
 * void setVisible ( bool )
 */
HB_FUNC( QT_QACTION_SETVISIBLE )
{
   hbqt_par_QAction( 1 )->setVisible( hb_parl( 2 ) );
}

/*
 * void toggle ()
 */
HB_FUNC( QT_QACTION_TOGGLE )
{
   hbqt_par_QAction( 1 )->toggle();
}

/*
 * void trigger ()
 */
HB_FUNC( QT_QACTION_TRIGGER )
{
   hbqt_par_QAction( 1 )->trigger();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
