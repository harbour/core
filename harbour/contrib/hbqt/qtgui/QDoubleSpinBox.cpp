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

#include <QtCore/QPointer>

#include <QtGui/QDoubleSpinBox>


/*
 * QDoubleSpinBox ( QWidget * parent = 0 )
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QDoubleSpinBox > pq;
} QGC_POINTER_QDoubleSpinBox;

QT_G_FUNC( release_QDoubleSpinBox )
{
   QGC_POINTER_QDoubleSpinBox * p = ( QGC_POINTER_QDoubleSpinBox * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QDoubleSpinBox               p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QDoubleSpinBox              ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QDoubleSpinBox * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QDoubleSpinBox * ) p->ph )->~QDoubleSpinBox();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QDoubleSpinBox * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QDoubleSpinBox              Object deleted!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  YES release_QDoubleSpinBox              %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QDoubleSpinBox              Object Name Missing!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  NO  release_QDoubleSpinBox" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QDoubleSpinBox              Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QDoubleSpinBox" );
      #endif
   }
}

void * gcAllocate_QDoubleSpinBox( void * pObj )
{
   QGC_POINTER_QDoubleSpinBox * p = ( QGC_POINTER_QDoubleSpinBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QDoubleSpinBox ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QDoubleSpinBox;
   new( & p->pq ) QPointer< QDoubleSpinBox >( ( QDoubleSpinBox * ) pObj );
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QDoubleSpinBox              %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QDOUBLESPINBOX )
{
   void * pObj = NULL;

   pObj = ( QDoubleSpinBox* ) new QDoubleSpinBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QDoubleSpinBox( pObj ) );
}
/*
 * QString cleanText () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_CLEANTEXT )
{
   hb_retc( hbqt_par_QDoubleSpinBox( 1 )->cleanText().toAscii().data() );
}

/*
 * int decimals () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_DECIMALS )
{
   hb_retni( hbqt_par_QDoubleSpinBox( 1 )->decimals() );
}

/*
 * double maximum () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_MAXIMUM )
{
   hb_retnd( hbqt_par_QDoubleSpinBox( 1 )->maximum() );
}

/*
 * double minimum () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_MINIMUM )
{
   hb_retnd( hbqt_par_QDoubleSpinBox( 1 )->minimum() );
}

/*
 * QString prefix () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_PREFIX )
{
   hb_retc( hbqt_par_QDoubleSpinBox( 1 )->prefix().toAscii().data() );
}

/*
 * void setDecimals ( int prec )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETDECIMALS )
{
   hbqt_par_QDoubleSpinBox( 1 )->setDecimals( hb_parni( 2 ) );
}

/*
 * void setMaximum ( double max )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETMAXIMUM )
{
   hbqt_par_QDoubleSpinBox( 1 )->setMaximum( hb_parnd( 2 ) );
}

/*
 * void setMinimum ( double min )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETMINIMUM )
{
   hbqt_par_QDoubleSpinBox( 1 )->setMinimum( hb_parnd( 2 ) );
}

/*
 * void setPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETPREFIX )
{
   hbqt_par_QDoubleSpinBox( 1 )->setPrefix( hbqt_par_QString( 2 ) );
}

/*
 * void setRange ( double minimum, double maximum )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETRANGE )
{
   hbqt_par_QDoubleSpinBox( 1 )->setRange( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void setSingleStep ( double val )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETSINGLESTEP )
{
   hbqt_par_QDoubleSpinBox( 1 )->setSingleStep( hb_parnd( 2 ) );
}

/*
 * void setSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETSUFFIX )
{
   hbqt_par_QDoubleSpinBox( 1 )->setSuffix( hbqt_par_QString( 2 ) );
}

/*
 * double singleStep () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_SINGLESTEP )
{
   hb_retnd( hbqt_par_QDoubleSpinBox( 1 )->singleStep() );
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_SUFFIX )
{
   hb_retc( hbqt_par_QDoubleSpinBox( 1 )->suffix().toAscii().data() );
}

/*
 * virtual QString textFromValue ( double value ) const
 */
HB_FUNC( QT_QDOUBLESPINBOX_TEXTFROMVALUE )
{
   hb_retc( hbqt_par_QDoubleSpinBox( 1 )->textFromValue( hb_parnd( 2 ) ).toAscii().data() );
}

/*
 * double value () const
 */
HB_FUNC( QT_QDOUBLESPINBOX_VALUE )
{
   hb_retnd( hbqt_par_QDoubleSpinBox( 1 )->value() );
}

/*
 * virtual double valueFromText ( const QString & text ) const
 */
HB_FUNC( QT_QDOUBLESPINBOX_VALUEFROMTEXT )
{
   hb_retnd( hbqt_par_QDoubleSpinBox( 1 )->valueFromText( hbqt_par_QString( 2 ) ) );
}

/*
 * void setValue ( double val )
 */
HB_FUNC( QT_QDOUBLESPINBOX_SETVALUE )
{
   hbqt_par_QDoubleSpinBox( 1 )->setValue( hb_parnd( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
