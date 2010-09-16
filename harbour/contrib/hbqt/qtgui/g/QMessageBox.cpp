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
 *  enum ButtonRole { InvalidRole, AcceptRole, RejectRole, DestructiveRole, ..., ResetRole }
 *  enum Icon { NoIcon, Question, Information, Warning, Critical }
 *  enum StandardButton { Ok, Open, Save, Cancel, ..., ButtonMask }
 *  flags StandardButtons
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
   QMessageBox  * ph = NULL ;
   HBQT_GC_T_QMessageBox * p = ( HBQT_GC_T_QMessageBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMessageBox   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMessageBox   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMessageBox          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMessageBox    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMessageBox    :    Object not created with new=true", ph ) );
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

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMessageBox  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMessageBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMESSAGEBOX )
{
   QMessageBox * pObj = NULL;

   pObj =  new QMessageBox() ;

   hb_retptrGC( hbqt_gcAllocate_QMessageBox( ( void * ) pObj, true ) );
}

/*
 * void addButton ( QAbstractButton * button, ButtonRole role )
 */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->addButton( hbqt_par_QAbstractButton( 2 ), ( QMessageBox::ButtonRole ) hb_parni( 3 ) );
   }
}

/*
 * QPushButton * addButton ( const QString & text, ButtonRole role )
 */
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

/*
 * QPushButton * addButton ( StandardButton button )
 */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON_2 )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPushButton( ( p )->addButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) ), false ) );
   }
}

/*
 * QAbstractButton * button ( StandardButton which ) const
 */
HB_FUNC( QT_QMESSAGEBOX_BUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->button( ( QMessageBox::StandardButton ) hb_parni( 2 ) ), false ) );
   }
}

/*
 * ButtonRole buttonRole ( QAbstractButton * button ) const
 */
HB_FUNC( QT_QMESSAGEBOX_BUTTONROLE )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retni( ( QMessageBox::ButtonRole ) ( p )->buttonRole( hbqt_par_QAbstractButton( 2 ) ) );
   }
}

/*
 * QList<QAbstractButton *> buttons () const
 */
HB_FUNC( QT_QMESSAGEBOX_BUTTONS )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAbstractButton *>( ( p )->buttons() ), true ) );
   }
}

/*
 * QAbstractButton * clickedButton () const
 */
HB_FUNC( QT_QMESSAGEBOX_CLICKEDBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->clickedButton(), false ) );
   }
}

/*
 * QPushButton * defaultButton () const
 */
HB_FUNC( QT_QMESSAGEBOX_DEFAULTBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPushButton( ( p )->defaultButton(), false ) );
   }
}

/*
 * QString detailedText () const
 */
HB_FUNC( QT_QMESSAGEBOX_DETAILEDTEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->detailedText().toUtf8().data() );
   }
}

/*
 * QAbstractButton * escapeButton () const
 */
HB_FUNC( QT_QMESSAGEBOX_ESCAPEBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->escapeButton(), false ) );
   }
}

/*
 * Icon icon () const
 */
HB_FUNC( QT_QMESSAGEBOX_ICON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retni( ( QMessageBox::Icon ) ( p )->icon() );
   }
}

/*
 * QPixmap iconPixmap () const
 */
HB_FUNC( QT_QMESSAGEBOX_ICONPIXMAP )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->iconPixmap() ), true ) );
   }
}

/*
 * QString informativeText () const
 */
HB_FUNC( QT_QMESSAGEBOX_INFORMATIVETEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->informativeText().toUtf8().data() );
   }
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QMESSAGEBOX_OPEN )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
   }
}

/*
 * void removeButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QMESSAGEBOX_REMOVEBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->removeButton( hbqt_par_QAbstractButton( 2 ) );
   }
}

/*
 * void setDefaultButton ( QPushButton * button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETDEFAULTBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setDefaultButton( hbqt_par_QPushButton( 2 ) );
   }
}

/*
 * void setDefaultButton ( StandardButton button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETDEFAULTBUTTON_1 )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setDefaultButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) );
   }
}

/*
 * void setDetailedText ( const QString & text )
 */
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

/*
 * void setEscapeButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETESCAPEBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setEscapeButton( hbqt_par_QAbstractButton( 2 ) );
   }
}

/*
 * void setEscapeButton ( StandardButton button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETESCAPEBUTTON_1 )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setEscapeButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) );
   }
}

/*
 * void setIcon ( Icon )
 */
HB_FUNC( QT_QMESSAGEBOX_SETICON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setIcon( ( QMessageBox::Icon ) hb_parni( 2 ) );
   }
}

/*
 * void setIconPixmap ( const QPixmap & pixmap )
 */
HB_FUNC( QT_QMESSAGEBOX_SETICONPIXMAP )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setIconPixmap( *hbqt_par_QPixmap( 2 ) );
   }
}

/*
 * void setInformativeText ( const QString & text )
 */
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

/*
 * void setStandardButtons ( StandardButtons buttons )
 */
HB_FUNC( QT_QMESSAGEBOX_SETSTANDARDBUTTONS )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setStandardButtons( ( QMessageBox::StandardButtons ) hb_parni( 2 ) );
   }
}

/*
 * void setText ( const QString & text )
 */
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

/*
 * void setTextFormat ( Qt::TextFormat format )
 */
HB_FUNC( QT_QMESSAGEBOX_SETTEXTFORMAT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setTextFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
   }
}

/*
 * void setWindowModality ( Qt::WindowModality windowModality )
 */
HB_FUNC( QT_QMESSAGEBOX_SETWINDOWMODALITY )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      ( p )->setWindowModality( ( Qt::WindowModality ) hb_parni( 2 ) );
   }
}

/*
 * void setWindowTitle ( const QString & title )
 */
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

/*
 * StandardButton standardButton ( QAbstractButton * button ) const
 */
HB_FUNC( QT_QMESSAGEBOX_STANDARDBUTTON )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retni( ( QMessageBox::StandardButton ) ( p )->standardButton( hbqt_par_QAbstractButton( 2 ) ) );
   }
}

/*
 * StandardButtons standardButtons () const
 */
HB_FUNC( QT_QMESSAGEBOX_STANDARDBUTTONS )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retni( ( QMessageBox::StandardButtons ) ( p )->standardButtons() );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QMESSAGEBOX_TEXT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
   }
}

/*
 * Qt::TextFormat textFormat () const
 */
HB_FUNC( QT_QMESSAGEBOX_TEXTFORMAT )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retni( ( Qt::TextFormat ) ( p )->textFormat() );
   }
}

/*
 * void about ( QWidget * parent, const QString & title, const QString & text )
 */
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

/*
 * void aboutQt ( QWidget * parent, const QString & title = QString() )
 */
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

/*
 * StandardButton critical ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton )
 */
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

/*
 * StandardButton information ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton )
 */
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

/*
 * StandardButton question ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton )
 */
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

/*
 * StandardButton warning ( QWidget * parent, const QString & title, const QString & text, StandardButtons buttons = Ok, StandardButton defaultButton = NoButton )
 */
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

/*
 * int exec ()
 */
HB_FUNC( QT_QMESSAGEBOX_EXEC )
{
   QMessageBox * p = hbqt_par_QMessageBox( 1 );
   if( p )
   {
      hb_retni( ( p )->exec() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
