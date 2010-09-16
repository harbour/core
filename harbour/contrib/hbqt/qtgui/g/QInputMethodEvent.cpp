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
 *  enum AttributeType { TextFormat, Cursor, Language, Ruby }
 */

#include <QtCore/QPointer>

#include <QtGui/QInputMethodEvent>


/* QInputMethodEvent ()
 * QInputMethodEvent ( const QString & preeditText, const QList<Attribute> & attributes )
 * QInputMethodEvent ( const QInputMethodEvent & other )
 */

typedef struct
{
   QInputMethodEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QInputMethodEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QInputMethodEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QInputMethodEvent   /.\\", p->ph ) );
         delete ( ( QInputMethodEvent * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QInputMethodEvent   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QInputMethodEvent    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QInputMethodEvent    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QInputMethodEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QInputMethodEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QInputMethodEvent;
   p->type = HBQT_TYPE_QInputMethodEvent;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QInputMethodEvent", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QInputMethodEvent", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QINPUTMETHODEVENT )
{
   QInputMethodEvent * pObj = NULL;

   pObj = new QInputMethodEvent() ;

   hb_retptrGC( hbqt_gcAllocate_QInputMethodEvent( ( void * ) pObj, true ) );
}

/*
 * const QString & commitString () const
 */
HB_FUNC( QT_QINPUTMETHODEVENT_COMMITSTRING )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->commitString().toUtf8().data() );
   }
}

/*
 * const QString & preeditString () const
 */
HB_FUNC( QT_QINPUTMETHODEVENT_PREEDITSTRING )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->preeditString().toUtf8().data() );
   }
}

/*
 * int replacementLength () const
 */
HB_FUNC( QT_QINPUTMETHODEVENT_REPLACEMENTLENGTH )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
   {
      hb_retni( ( p )->replacementLength() );
   }
}

/*
 * int replacementStart () const
 */
HB_FUNC( QT_QINPUTMETHODEVENT_REPLACEMENTSTART )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
   {
      hb_retni( ( p )->replacementStart() );
   }
}

/*
 * void setCommitString ( const QString & commitString, int replaceFrom = 0, int replaceLength = 0 )
 */
HB_FUNC( QT_QINPUTMETHODEVENT_SETCOMMITSTRING )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
   {
      void * pText;
      ( p )->setCommitString( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), hb_parni( 4 ) );
      hb_strfree( pText );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
