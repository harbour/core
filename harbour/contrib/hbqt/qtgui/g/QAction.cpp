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
 *  enum ActionEvent { Trigger, Hover }
 *  enum MenuRole { NoRole, TextHeuristicRole, ApplicationSpecificRole, AboutQtRole, ..., QuitRole }
 */

/*
 *  Constructed[ 50/51 [ 98.04% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setShortcuts ( const QList<QKeySequence> & shortcuts )
 *
 *  *** Commented out protostypes ***
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
   QAction  * ph = NULL;
   HBQT_GC_T_QAction * p = ( HBQT_GC_T_QAction * ) Cargo;

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

void * hbqt_gcAllocate_QAction( void * pObj, bool bNew )
{
   HBQT_GC_T_QAction * p = ( HBQT_GC_T_QAction * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAction ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAction >( ( QAction * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAction;
   p->type = HBQT_TYPE_QAction;

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

/* QActionGroup * actionGroup () const */
HB_FUNC( QT_QACTION_ACTIONGROUP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QActionGroup( ( p )->actionGroup(), false ) );
}

/* void activate ( ActionEvent event ) */
HB_FUNC( QT_QACTION_ACTIVATE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->activate( ( QAction::ActionEvent ) hb_parni( 2 ) );
}

/* QList<QWidget *> associatedWidgets () const */
HB_FUNC( QT_QACTION_ASSOCIATEDWIDGETS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWidget *>( ( p )->associatedWidgets() ), true ) );
}

/* bool autoRepeat () const */
HB_FUNC( QT_QACTION_AUTOREPEAT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->autoRepeat() );
}

/* QVariant data () const */
HB_FUNC( QT_QACTION_DATA )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data() ), true ) );
}

/* QFont font () const */
HB_FUNC( QT_QACTION_FONT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* QIcon icon () const */
HB_FUNC( QT_QACTION_ICON )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
}

/* QString iconText () const */
HB_FUNC( QT_QACTION_ICONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retstr_utf8( ( p )->iconText().toUtf8().data() );
}

/* bool isCheckable () const */
HB_FUNC( QT_QACTION_ISCHECKABLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isCheckable() );
}

/* bool isChecked () const */
HB_FUNC( QT_QACTION_ISCHECKED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isChecked() );
}

/* bool isEnabled () const */
HB_FUNC( QT_QACTION_ISENABLED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
}

/* bool isIconVisibleInMenu () const */
HB_FUNC( QT_QACTION_ISICONVISIBLEINMENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isIconVisibleInMenu() );
}

/* bool isSeparator () const */
HB_FUNC( QT_QACTION_ISSEPARATOR )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isSeparator() );
}

/* bool isVisible () const */
HB_FUNC( QT_QACTION_ISVISIBLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
}

/* QMenu * menu () const */
HB_FUNC( QT_QACTION_MENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) );
}

/* MenuRole menuRole () const */
HB_FUNC( QT_QACTION_MENUROLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retni( ( QAction::MenuRole ) ( p )->menuRole() );
}

/* QWidget * parentWidget () const */
HB_FUNC( QT_QACTION_PARENTWIDGET )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) );
}

/* void setActionGroup ( QActionGroup * group ) */
HB_FUNC( QT_QACTION_SETACTIONGROUP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setActionGroup( hbqt_par_QActionGroup( 2 ) );
}

/* void setAutoRepeat ( bool ) */
HB_FUNC( QT_QACTION_SETAUTOREPEAT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setAutoRepeat( hb_parl( 2 ) );
}

/* void setCheckable ( bool ) */
HB_FUNC( QT_QACTION_SETCHECKABLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setCheckable( hb_parl( 2 ) );
}

/* void setData ( const QVariant & userData ) */
HB_FUNC( QT_QACTION_SETDATA )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setData( *hbqt_par_QVariant( 2 ) );
}

/* void setFont ( const QFont & font ) */
HB_FUNC( QT_QACTION_SETFONT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setIcon ( const QIcon & icon ) */
HB_FUNC( QT_QACTION_SETICON )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
}

/* void setIconText ( const QString & text ) */
HB_FUNC( QT_QACTION_SETICONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
   {
      void * pText;
      ( p )->setIconText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setIconVisibleInMenu ( bool visible ) */
HB_FUNC( QT_QACTION_SETICONVISIBLEINMENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setIconVisibleInMenu( hb_parl( 2 ) );
}

/* void setMenu ( QMenu * menu ) */
HB_FUNC( QT_QACTION_SETMENU )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setMenu( hbqt_par_QMenu( 2 ) );
}

/* void setMenuRole ( MenuRole menuRole ) */
HB_FUNC( QT_QACTION_SETMENUROLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setMenuRole( ( QAction::MenuRole ) hb_parni( 2 ) );
}

/* void setSeparator ( bool b ) */
HB_FUNC( QT_QACTION_SETSEPARATOR )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setSeparator( hb_parl( 2 ) );
}

/* void setShortcut ( const QKeySequence & shortcut ) */
HB_FUNC( QT_QACTION_SETSHORTCUT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setShortcut( *hbqt_par_QKeySequence( 2 ) );
}

/* void setShortcutContext ( Qt::ShortcutContext context ) */
HB_FUNC( QT_QACTION_SETSHORTCUTCONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setShortcutContext( ( Qt::ShortcutContext ) hb_parni( 2 ) );
}

/* void setShortcuts ( QKeySequence::StandardKey key ) */
HB_FUNC( QT_QACTION_SETSHORTCUTS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setShortcuts( ( QKeySequence::StandardKey ) hb_parni( 2 ) );
}

/* void setStatusTip ( const QString & statusTip ) */
HB_FUNC( QT_QACTION_SETSTATUSTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStatusTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setText ( const QString & text ) */
HB_FUNC( QT_QACTION_SETTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setToolTip ( const QString & tip ) */
HB_FUNC( QT_QACTION_SETTOOLTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setWhatsThis ( const QString & what ) */
HB_FUNC( QT_QACTION_SETWHATSTHIS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWhatsThis( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QKeySequence shortcut () const */
HB_FUNC( QT_QACTION_SHORTCUT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QKeySequence( new QKeySequence( ( p )->shortcut() ), true ) );
}

/* Qt::ShortcutContext shortcutContext () const */
HB_FUNC( QT_QACTION_SHORTCUTCONTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retni( ( Qt::ShortcutContext ) ( p )->shortcutContext() );
}

/* QList<QKeySequence> shortcuts () const */
HB_FUNC( QT_QACTION_SHORTCUTS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QKeySequence>( ( p )->shortcuts() ), true ) );
}

/* bool showStatusText ( QWidget * widget = 0 ) */
HB_FUNC( QT_QACTION_SHOWSTATUSTEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retl( ( p )->showStatusText( hbqt_par_QWidget( 2 ) ) );
}

/* QString statusTip () const */
HB_FUNC( QT_QACTION_STATUSTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retstr_utf8( ( p )->statusTip().toUtf8().data() );
}

/* QString text () const */
HB_FUNC( QT_QACTION_TEXT )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* QString toolTip () const */
HB_FUNC( QT_QACTION_TOOLTIP )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
}

/* QString whatsThis () const */
HB_FUNC( QT_QACTION_WHATSTHIS )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      hb_retstr_utf8( ( p )->whatsThis().toUtf8().data() );
}

/* void hover () */
HB_FUNC( QT_QACTION_HOVER )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->hover();
}

/* void setChecked ( bool ) */
HB_FUNC( QT_QACTION_SETCHECKED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setChecked( hb_parl( 2 ) );
}

/* void setDisabled ( bool b ) */
HB_FUNC( QT_QACTION_SETDISABLED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setDisabled( hb_parl( 2 ) );
}

/* void setEnabled ( bool ) */
HB_FUNC( QT_QACTION_SETENABLED )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
}

/* void setVisible ( bool ) */
HB_FUNC( QT_QACTION_SETVISIBLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* void toggle () */
HB_FUNC( QT_QACTION_TOGGLE )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->toggle();
}

/* void trigger () */
HB_FUNC( QT_QACTION_TRIGGER )
{
   QAction * p = hbqt_par_QAction( 1 );
   if( p )
      ( p )->trigger();
}


#endif /* #if QT_VERSION >= 0x040500 */
