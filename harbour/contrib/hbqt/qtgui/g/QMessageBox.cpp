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
 *  enum ButtonRole { InvalidRole, AcceptRole, RejectRole, DestructiveRole, ..., ResetRole }
 *  enum Icon { NoIcon, Question, Information, Warning, Critical }
 *  enum StandardButton { Ok, Open, Save, Cancel, ..., ButtonMask }
 *  flags StandardButtons
 */

/*
 *  Constructed[ 39/39 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QMessageBox>


/*
 * QMessageBox ( QWidget * parent = 0 )
 * QMessageBox ( Icon icon, const QString & title, const QString & text, StandardButtons buttons = NoButton, QWidget * parent = 0, Qt::WindowFlags f = Qt::Dialog | Qt::MSWindowsFixedSizeDialogHint )
 * ~QMessageBox ()
 */

typedef struct
{
   QPointer< QMessageBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMessageBox;

HBQT_GC_FUNC( hbqt_gcRelease_QMessageBox )
{
   HBQT_GC_T_QMessageBox * p = ( HBQT_GC_T_QMessageBox * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QMessageBox * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMessageBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QMessageBox * p = ( HBQT_GC_T_QMessageBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMessageBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMessageBox >( ( QMessageBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMessageBox;
   p->type = HBQT_TYPE_QMessageBox;

   return p;
}

HB_FUNC( QT_QMESSAGEBOX )
{
   QMessageBox * pObj = NULL;

   pObj = new QMessageBox() ;

   hb_retptrGC( hbqt_gcAllocate_QMessageBox( ( void * ) pObj, true ) );
}

/* void addButton ( QAbstractButton * button, ButtonRole role ) */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->addButton( hbqt_par_QAbstractButton( 2 ), ( QMessageBox::ButtonRole ) hb_parni( 3 ) );
}

/* QPushButton * addButton ( const QString & text, ButtonRole role ) */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON_1 )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QPushButton( ( p )->addButton( hb_parstr_utf8( 2, &pText, NULL ), ( QMessageBox::ButtonRole ) hb_parni( 3 ) ), false ) );
      hb_strfree( pText );
   }
}

/* QPushButton * addButton ( StandardButton button ) */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON_2 )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPushButton( ( p )->addButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) ), false ) );
}

/* QAbstractButton * button ( StandardButton which ) const */
HB_FUNC( QT_QMESSAGEBOX_BUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->button( ( QMessageBox::StandardButton ) hb_parni( 2 ) ), false ) );
}

/* ButtonRole buttonRole ( QAbstractButton * button ) const */
HB_FUNC( QT_QMESSAGEBOX_BUTTONROLE )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retni( ( QMessageBox::ButtonRole ) ( p )->buttonRole( hbqt_par_QAbstractButton( 2 ) ) );
}

/* QList<QAbstractButton *> buttons () const */
HB_FUNC( QT_QMESSAGEBOX_BUTTONS )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAbstractButton *>( ( p )->buttons() ), true ) );
}

/* QAbstractButton * clickedButton () const */
HB_FUNC( QT_QMESSAGEBOX_CLICKEDBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->clickedButton(), false ) );
}

/* QPushButton * defaultButton () const */
HB_FUNC( QT_QMESSAGEBOX_DEFAULTBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPushButton( ( p )->defaultButton(), false ) );
}

/* QString detailedText () const */
HB_FUNC( QT_QMESSAGEBOX_DETAILEDTEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->detailedText().toUtf8().data() );
}

/* QAbstractButton * escapeButton () const */
HB_FUNC( QT_QMESSAGEBOX_ESCAPEBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->escapeButton(), false ) );
}

/* Icon icon () const */
HB_FUNC( QT_QMESSAGEBOX_ICON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retni( ( QMessageBox::Icon ) ( p )->icon() );
}

/* QPixmap iconPixmap () const */
HB_FUNC( QT_QMESSAGEBOX_ICONPIXMAP )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->iconPixmap() ), true ) );
}

/* QString informativeText () const */
HB_FUNC( QT_QMESSAGEBOX_INFORMATIVETEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->informativeText().toUtf8().data() );
}

/* void open ( QObject * receiver, const char * member ) */
HB_FUNC( QT_QMESSAGEBOX_OPEN )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), ( const char * ) hb_parc( 3 ) );
}

/* void removeButton ( QAbstractButton * button ) */
HB_FUNC( QT_QMESSAGEBOX_REMOVEBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->removeButton( hbqt_par_QAbstractButton( 2 ) );
}

/* void setDefaultButton ( QPushButton * button ) */
HB_FUNC( QT_QMESSAGEBOX_SETDEFAULTBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setDefaultButton( hbqt_par_QPushButton( 2 ) );
}

/* void setDefaultButton ( StandardButton button ) */
HB_FUNC( QT_QMESSAGEBOX_SETDEFAULTBUTTON_1 )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setDefaultButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) );
}

/* void setDetailedText ( const QString & text ) */
HB_FUNC( QT_QMESSAGEBOX_SETDETAILEDTEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDetailedText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setEscapeButton ( QAbstractButton * button ) */
HB_FUNC( QT_QMESSAGEBOX_SETESCAPEBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setEscapeButton( hbqt_par_QAbstractButton( 2 ) );
}

/* void setEscapeButton ( StandardButton button ) */
HB_FUNC( QT_QMESSAGEBOX_SETESCAPEBUTTON_1 )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setEscapeButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) );
}

/* void setIcon ( Icon ) */
HB_FUNC( QT_QMESSAGEBOX_SETICON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setIcon( ( QMessageBox::Icon ) hb_parni( 2 ) );
}

/* void setIconPixmap ( const QPixmap & pixmap ) */
HB_FUNC( QT_QMESSAGEBOX_SETICONPIXMAP )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setIconPixmap( *hbqt_par_QPixmap( 2 ) );
}

/* void setInformativeText ( const QString & text ) */
HB_FUNC( QT_QMESSAGEBOX_SETINFORMATIVETEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setInformativeText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setStandardButtons ( StandardButtons buttons ) */
HB_FUNC( QT_QMESSAGEBOX_SETSTANDARDBUTTONS )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setStandardButtons( ( QMessageBox::StandardButtons ) hb_parni( 2 ) );
}

/* void setText ( const QString & text ) */
HB_FUNC( QT_QMESSAGEBOX_SETTEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTextFormat ( Qt::TextFormat format ) */
HB_FUNC( QT_QMESSAGEBOX_SETTEXTFORMAT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setTextFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
}

/* void setWindowModality ( Qt::WindowModality windowModality ) */
HB_FUNC( QT_QMESSAGEBOX_SETWINDOWMODALITY )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      ( p )->setWindowModality( ( Qt::WindowModality ) hb_parni( 2 ) );
}

/* void setWindowTitle ( const QString & title ) */
HB_FUNC( QT_QMESSAGEBOX_SETWINDOWTITLE )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWindowTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* StandardButton standardButton ( QAbstractButton * button ) const */
HB_FUNC( QT_QMESSAGEBOX_STANDARDBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retni( ( QMessageBox::StandardButton ) ( p )->standardButton( hbqt_par_QAbstractButton( 2 ) ) );
}

/* StandardButtons standardButtons () const */
HB_FUNC( QT_QMESSAGEBOX_STANDARDBUTTONS )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retni( ( QMessageBox::StandardButtons ) ( p )->standardButtons() );
}

/* QString text () const */
HB_FUNC( QT_QMESSAGEBOX_TEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* Qt::TextFormat textFormat () const */
HB_FUNC( QT_QMESSAGEBOX_TEXTFORMAT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retni( ( Qt::TextFormat ) ( p )->textFormat() );
}

/* void about ( QWidget * parent, const QString & title, const QString & text ) */
HB_FUNC( QT_QMESSAGEBOX_ABOUT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->about( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void aboutQt ( QWidget * parent, const QString & title = QString() ) */
HB_FUNC( QT_QMESSAGEBOX_ABOUTQT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->aboutQt( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* StandardButton critical ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton ) */
HB_FUNC( QT_QMESSAGEBOX_CRITICAL )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( QMessageBox::StandardButton ) ( p )->critical( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), ( HB_ISNUM( 5 ) ? ( QMessageBox::StandardButtons ) hb_parni( 5 ) : ( QMessageBox::StandardButtons ) QMessageBox::Ok ), ( HB_ISNUM( 6 ) ? ( QMessageBox::StandardButton ) hb_parni( 6 ) : ( QMessageBox::StandardButton ) QMessageBox::NoButton ) ) );
      hb_strfree( pText );
   }
}

/* StandardButton information ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton ) */
HB_FUNC( QT_QMESSAGEBOX_INFORMATION )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( QMessageBox::StandardButton ) ( p )->information( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), ( HB_ISNUM( 5 ) ? ( QMessageBox::StandardButtons ) hb_parni( 5 ) : ( QMessageBox::StandardButtons ) QMessageBox::Ok ), ( HB_ISNUM( 6 ) ? ( QMessageBox::StandardButton ) hb_parni( 6 ) : ( QMessageBox::StandardButton ) QMessageBox::NoButton ) ) );
      hb_strfree( pText );
   }
}

/* StandardButton question ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton ) */
HB_FUNC( QT_QMESSAGEBOX_QUESTION )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( QMessageBox::StandardButton ) ( p )->question( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), ( HB_ISNUM( 5 ) ? ( QMessageBox::StandardButtons ) hb_parni( 5 ) : ( QMessageBox::StandardButtons ) QMessageBox::Ok ), ( HB_ISNUM( 6 ) ? ( QMessageBox::StandardButton ) hb_parni( 6 ) : ( QMessageBox::StandardButton ) QMessageBox::NoButton ) ) );
      hb_strfree( pText );
   }
}

/* StandardButton warning ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton ) */
HB_FUNC( QT_QMESSAGEBOX_WARNING )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( QMessageBox::StandardButton ) ( p )->warning( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), ( HB_ISNUM( 5 ) ? ( QMessageBox::StandardButtons ) hb_parni( 5 ) : ( QMessageBox::StandardButtons ) QMessageBox::Ok ), ( HB_ISNUM( 6 ) ? ( QMessageBox::StandardButton ) hb_parni( 6 ) : ( QMessageBox::StandardButton ) QMessageBox::NoButton ) ) );
      hb_strfree( pText );
   }
}

/* int exec () */
HB_FUNC( QT_QMESSAGEBOX_EXEC )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
      hb_retni( ( p )->exec() );
}


#endif /* #if QT_VERSION >= 0x040500 */
