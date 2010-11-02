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
 *  enum InputDialogOption { NoButtons, UseListViewForComboBoxItems }
 *  enum InputMode { TextInput, IntInput, DoubleInput }
 *  flags InputDialogOptions
 */

/*
 *  Constructed[ 44/44 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QInputDialog>


/*
 * QInputDialog ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QInputDialog ()
 */

typedef struct
{
   QPointer< QInputDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QInputDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QInputDialog )
{
   HBQT_GC_T_QInputDialog * p = ( HBQT_GC_T_QInputDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QInputDialog * ph = p->ph;
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

void * hbqt_gcAllocate_QInputDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QInputDialog * p = ( HBQT_GC_T_QInputDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QInputDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QInputDialog >( ( QInputDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QInputDialog;
   p->type = HBQT_TYPE_QInputDialog;

   return p;
}

HB_FUNC( QT_QINPUTDIALOG )
{
   QInputDialog * pObj = NULL;

   pObj = new QInputDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QInputDialog( ( void * ) pObj, true ) );
}

/* QString cancelButtonText () const */
HB_FUNC( QT_QINPUTDIALOG_CANCELBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retstr_utf8( ( p )->cancelButtonText().toUtf8().data() );
}

/* QStringList comboBoxItems () const */
HB_FUNC( QT_QINPUTDIALOG_COMBOBOXITEMS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->comboBoxItems() ), true ) );
}

/* virtual void done ( int result ) */
HB_FUNC( QT_QINPUTDIALOG_DONE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->done( hb_parni( 2 ) );
}

/* int doubleDecimals () const */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEDECIMALS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->doubleDecimals() );
}

/* double doubleMaximum () const */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retnd( ( p )->doubleMaximum() );
}

/* double doubleMinimum () const */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retnd( ( p )->doubleMinimum() );
}

/* double doubleValue () const */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retnd( ( p )->doubleValue() );
}

/* InputMode inputMode () const */
HB_FUNC( QT_QINPUTDIALOG_INPUTMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( QInputDialog::InputMode ) ( p )->inputMode() );
}

/* int intMaximum () const */
HB_FUNC( QT_QINPUTDIALOG_INTMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intMaximum() );
}

/* int intMinimum () const */
HB_FUNC( QT_QINPUTDIALOG_INTMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intMinimum() );
}

/* int intStep () const */
HB_FUNC( QT_QINPUTDIALOG_INTSTEP )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intStep() );
}

/* int intValue () const */
HB_FUNC( QT_QINPUTDIALOG_INTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intValue() );
}

/* bool isComboBoxEditable () const */
HB_FUNC( QT_QINPUTDIALOG_ISCOMBOBOXEDITABLE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retl( ( p )->isComboBoxEditable() );
}

/* QString labelText () const */
HB_FUNC( QT_QINPUTDIALOG_LABELTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retstr_utf8( ( p )->labelText().toUtf8().data() );
}

/* QString okButtonText () const */
HB_FUNC( QT_QINPUTDIALOG_OKBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retstr_utf8( ( p )->okButtonText().toUtf8().data() );
}

/* void open ( QObject * receiver, const char * member ) */
HB_FUNC( QT_QINPUTDIALOG_OPEN )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), ( const char * ) hb_parc( 3 ) );
}

/* InputDialogOptions options () const */
HB_FUNC( QT_QINPUTDIALOG_OPTIONS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( QInputDialog::InputDialogOptions ) ( p )->options() );
}

/* void setCancelButtonText ( const QString & text ) */
HB_FUNC( QT_QINPUTDIALOG_SETCANCELBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setCancelButtonText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setComboBoxEditable ( bool editable ) */
HB_FUNC( QT_QINPUTDIALOG_SETCOMBOBOXEDITABLE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setComboBoxEditable( hb_parl( 2 ) );
}

/* void setComboBoxItems ( const QStringList & items ) */
HB_FUNC( QT_QINPUTDIALOG_SETCOMBOBOXITEMS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setComboBoxItems( *hbqt_par_QStringList( 2 ) );
}

/* void setDoubleDecimals ( int decimals ) */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEDECIMALS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleDecimals( hb_parni( 2 ) );
}

/* void setDoubleMaximum ( double max ) */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleMaximum( hb_parnd( 2 ) );
}

/* void setDoubleMinimum ( double min ) */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleMinimum( hb_parnd( 2 ) );
}

/* void setDoubleRange ( double min, double max ) */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLERANGE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleRange( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setDoubleValue ( double value ) */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleValue( hb_parnd( 2 ) );
}

/* void setInputMode ( InputMode mode ) */
HB_FUNC( QT_QINPUTDIALOG_SETINPUTMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setInputMode( ( QInputDialog::InputMode ) hb_parni( 2 ) );
}

/* void setIntMaximum ( int max ) */
HB_FUNC( QT_QINPUTDIALOG_SETINTMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntMaximum( hb_parni( 2 ) );
}

/* void setIntMinimum ( int min ) */
HB_FUNC( QT_QINPUTDIALOG_SETINTMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntMinimum( hb_parni( 2 ) );
}

/* void setIntRange ( int min, int max ) */
HB_FUNC( QT_QINPUTDIALOG_SETINTRANGE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setIntStep ( int step ) */
HB_FUNC( QT_QINPUTDIALOG_SETINTSTEP )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntStep( hb_parni( 2 ) );
}

/* void setIntValue ( int value ) */
HB_FUNC( QT_QINPUTDIALOG_SETINTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntValue( hb_parni( 2 ) );
}

/* void setLabelText ( const QString & text ) */
HB_FUNC( QT_QINPUTDIALOG_SETLABELTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setLabelText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setOkButtonText ( const QString & text ) */
HB_FUNC( QT_QINPUTDIALOG_SETOKBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setOkButtonText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setOption ( InputDialogOption option, bool on = true ) */
HB_FUNC( QT_QINPUTDIALOG_SETOPTION )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setOptions ( InputDialogOptions options ) */
HB_FUNC( QT_QINPUTDIALOG_SETOPTIONS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setOptions( ( QInputDialog::InputDialogOptions ) hb_parni( 2 ) );
}

/* void setTextEchoMode ( QLineEdit::EchoMode mode ) */
HB_FUNC( QT_QINPUTDIALOG_SETTEXTECHOMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setTextEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) );
}

/* void setTextValue ( const QString & text ) */
HB_FUNC( QT_QINPUTDIALOG_SETTEXTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTextValue( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* bool testOption ( InputDialogOption option ) const */
HB_FUNC( QT_QINPUTDIALOG_TESTOPTION )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ) ) );
}

/* QLineEdit::EchoMode textEchoMode () const */
HB_FUNC( QT_QINPUTDIALOG_TEXTECHOMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( QLineEdit::EchoMode ) ( p )->textEchoMode() );
}

/* QString textValue () const */
HB_FUNC( QT_QINPUTDIALOG_TEXTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retstr_utf8( ( p )->textValue().toUtf8().data() );
}

/* double getDouble ( QWidget * parent, const QString & title, const QString & label, double value = 0, double min = -2147483647, double max = 2147483647, int decimals = 1, bool * ok = 0, Qt::WindowFlags flags = 0 ) */
HB_FUNC( QT_QINPUTDIALOG_GETDOUBLE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
   {
      void * pText;
      hb_retnd( ( p )->getDouble( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), hb_parnidef( 8, 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) );
      hb_strfree( pText );
   }

   hb_stornl( iOk, 9 );
}

/* int getInt ( QWidget * parent, const QString & title, const QString & label, int value = 0, int min = -2147483647, int max = 2147483647, int step = 1, bool * ok = 0, Qt::WindowFlags flags = 0 ) */
HB_FUNC( QT_QINPUTDIALOG_GETINT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
   {
      void * pText;
      hb_retni( ( p )->getInt( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parni( 5 ), hb_parnidef( 6, -2147483647 ), hb_parnidef( 7, 2147483647 ), hb_parnidef( 8, 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) );
      hb_strfree( pText );
   }

   hb_stornl( iOk, 9 );
}

/* QString getItem ( QWidget * parent, const QString & title, const QString & label, const QStringList & items, int current = 0, bool editable = true, bool * ok = 0, Qt::WindowFlags flags = 0 ) */
HB_FUNC( QT_QINPUTDIALOG_GETITEM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->getItem( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), *hbqt_par_QStringList( 5 ), hb_parni( 6 ), hb_parl( 7 ), &iOk, ( Qt::WindowFlags ) hb_parni( 9 ) ).toUtf8().data() );
      hb_strfree( pText );
   }

   hb_stornl( iOk, 8 );
}

/* QString getText ( QWidget * parent, const QString & title, const QString & label, QLineEdit::EchoMode mode = QLineEdit::Normal, const QString & text = QString(), bool * ok = 0, Qt::WindowFlags flags = 0 ) */
HB_FUNC( QT_QINPUTDIALOG_GETTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->getText( hbqt_par_QWidget( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), ( HB_ISNUM( 5 ) ? ( QLineEdit::EchoMode ) hb_parni( 5 ) : ( QLineEdit::EchoMode ) QLineEdit::Normal ), hb_parstr_utf8( 6, &pText, NULL ), &iOk, ( Qt::WindowFlags ) hb_parni( 8 ) ).toUtf8().data() );
      hb_strfree( pText );
   }

   hb_stornl( iOk, 7 );
}


#endif /* #if QT_VERSION >= 0x040500 */
