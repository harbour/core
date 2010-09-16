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
 *  enum ButtonSymbols { UpDownArrows, PlusMinus, NoButtons }
 *  enum CorrectionMode { CorrectToPreviousValue, CorrectToNearestValue }
 *  flags StepEnabled
 *  enum StepEnabledFlag { StepNone, StepUpEnabled, StepDownEnabled }
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractSpinBox>


/*
 * QAbstractSpinBox ( QWidget * parent = 0 )
 * ~QAbstractSpinBox ()
 */

typedef struct
{
   QPointer< QAbstractSpinBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractSpinBox;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractSpinBox )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractSpinBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractSpinBox * p = ( HBQT_GC_T_QAbstractSpinBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractSpinBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractSpinBox >( ( QAbstractSpinBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractSpinBox;
   p->type = HBQT_TYPE_QAbstractSpinBox;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractSpinBox  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractSpinBox", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTSPINBOX )
{

}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_ALIGNMENT )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   }
}

/*
 * ButtonSymbols buttonSymbols () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_BUTTONSYMBOLS )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retni( ( QAbstractSpinBox::ButtonSymbols ) ( p )->buttonSymbols() );
   }
}

/*
 * CorrectionMode correctionMode () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_CORRECTIONMODE )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retni( ( QAbstractSpinBox::CorrectionMode ) ( p )->correctionMode() );
   }
}

/*
 * bool hasAcceptableInput () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_HASACCEPTABLEINPUT )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retl( ( p )->hasAcceptableInput() );
   }
}

/*
 * bool hasFrame () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_HASFRAME )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retl( ( p )->hasFrame() );
   }
}

/*
 * void interpretText ()
 */
HB_FUNC( QT_QABSTRACTSPINBOX_INTERPRETTEXT )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->interpretText();
   }
}

/*
 * bool isAccelerated () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_ISACCELERATED )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retl( ( p )->isAccelerated() );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_ISREADONLY )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retl( ( p )->isReadOnly() );
   }
}

/*
 * bool keyboardTracking () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_KEYBOARDTRACKING )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retl( ( p )->keyboardTracking() );
   }
}

/*
 * void setAccelerated ( bool on )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETACCELERATED )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setAccelerated( hb_parl( 2 ) );
   }
}

/*
 * void setAlignment ( Qt::Alignment flag )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETALIGNMENT )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   }
}

/*
 * void setButtonSymbols ( ButtonSymbols bs )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETBUTTONSYMBOLS )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setButtonSymbols( ( QAbstractSpinBox::ButtonSymbols ) hb_parni( 2 ) );
   }
}

/*
 * void setCorrectionMode ( CorrectionMode cm )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETCORRECTIONMODE )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setCorrectionMode( ( QAbstractSpinBox::CorrectionMode ) hb_parni( 2 ) );
   }
}

/*
 * void setFrame ( bool )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETFRAME )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setFrame( hb_parl( 2 ) );
   }
}

/*
 * void setKeyboardTracking ( bool kt )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETKEYBOARDTRACKING )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setKeyboardTracking( hb_parl( 2 ) );
   }
}

/*
 * void setReadOnly ( bool r )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETREADONLY )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setReadOnly( hb_parl( 2 ) );
   }
}

/*
 * void setSpecialValueText ( const QString & txt )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETSPECIALVALUETEXT )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setSpecialValueText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setWrapping ( bool w )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SETWRAPPING )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->setWrapping( hb_parl( 2 ) );
   }
}

/*
 * QString specialValueText () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SPECIALVALUETEXT )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->specialValueText().toUtf8().data() );
   }
}

/*
 * virtual void stepBy ( int steps )
 */
HB_FUNC( QT_QABSTRACTSPINBOX_STEPBY )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->stepBy( hb_parni( 2 ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_TEXT )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
   }
}

/*
 * bool wrapping () const
 */
HB_FUNC( QT_QABSTRACTSPINBOX_WRAPPING )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      hb_retl( ( p )->wrapping() );
   }
}

/*
 * virtual void clear ()
 */
HB_FUNC( QT_QABSTRACTSPINBOX_CLEAR )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QABSTRACTSPINBOX_SELECTALL )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->selectAll();
   }
}

/*
 * void stepDown ()
 */
HB_FUNC( QT_QABSTRACTSPINBOX_STEPDOWN )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->stepDown();
   }
}

/*
 * void stepUp ()
 */
HB_FUNC( QT_QABSTRACTSPINBOX_STEPUP )
{
   QAbstractSpinBox * p = hbqt_par_QAbstractSpinBox( 1 );
   if( p )
   {
      ( p )->stepUp();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
