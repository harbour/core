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
#include "hbqscintilla.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum TextCase { OriginalCase = 0, UpperCase = 1, LowerCase = 2 }
 */

/*
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscistyle.h>

/*
 * QsciStyle (int style=-1)
 * QsciStyle (int style, const QString &description, const QColor &color, const QColor &paper, const QFont &font, bool eol_fill=false)
 *
 */

typedef struct
{
   QPointer< QsciStyle > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciStyle;

HBQT_GC_FUNC( hbqt_gcRelease_QsciStyle )
{
   QsciStyle  * ph = NULL ;
   HBQT_GC_T_QsciStyle * p = ( HBQT_GC_T_QsciStyle * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciStyle   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciStyle   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QsciStyle          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QsciStyle    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QsciStyle    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciStyle( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciStyle * p = ( HBQT_GC_T_QsciStyle * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciStyle ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciStyle >( ( QsciStyle * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciStyle;
   p->type = HBQT_TYPE_QsciStyle;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QsciStyle  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QsciStyle", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSCISTYLE )
{
   QsciStyle * pObj = NULL;

   if( HB_ISNUMERIC( 1 ) )
   {
      pObj = new QsciStyle( hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() >= 5 )
   {
      pObj = new QsciStyle( hb_parni( 1 ), hbqt_par_QString( 2 ), *hbqt_par_QColor( 3 ), *hbqt_par_QColor( 4 ), * hbqt_par_QFont( 5 ), HB_ISLOG( 6 ) ? hb_parl( 6 ) : false ) ;
   }
   else
   {
      pObj = new QsciStyle() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QsciStyle( ( void * ) pObj, true ) );
}

/*
 * int style () const
 */
HB_FUNC( QT_QSCISTYLE_STYLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retni( ( p )->style() );
   }
}

/*
 * void setDescription (const QString &description)
 */
HB_FUNC( QT_QSCISTYLE_SETDESCRIPTION )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDescription( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QString description () const
 */
HB_FUNC( QT_QSCISTYLE_DESCRIPTION )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->description().toUtf8().data() );
   }
}

/*
 * void setColor (const QColor &color)
 */
HB_FUNC( QT_QSCISTYLE_SETCOLOR )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * QColor color () const
 */
HB_FUNC( QT_QSCISTYLE_COLOR )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) );
   }
}

/*
 * void setPaper (const QColor &paper)
 */
HB_FUNC( QT_QSCISTYLE_SETPAPER )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setPaper( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * QColor paper () const
 */
HB_FUNC( QT_QSCISTYLE_PAPER )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->paper() ), true ) );
   }
}

/*
 * void setFont (const QFont &font)
 */
HB_FUNC( QT_QSCISTYLE_SETFONT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QSCISTYLE_FONT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   }
}

/*
 * void setEolFill (bool fill)
 */
HB_FUNC( QT_QSCISTYLE_SETEOLFILL )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setEolFill( hb_parl( 2 ) );
   }
}

/*
 * bool eolFill () const
 */
HB_FUNC( QT_QSCISTYLE_EOLFILL )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retl( ( p )->eolFill() );
   }
}

/*
 * void setTextCase (TextCase text_case)
 */
HB_FUNC( QT_QSCISTYLE_SETTEXTCASE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setTextCase( ( QsciStyle::TextCase ) hb_parni( 2 ) );
   }
}

/*
 * TextCase textCase () const
 */
HB_FUNC( QT_QSCISTYLE_TEXTCASE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retni( ( QsciStyle::TextCase ) ( p )->textCase() );
   }
}

/*
 * void setVisible (bool visible)
 */
HB_FUNC( QT_QSCISTYLE_SETVISIBLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setVisible( hb_parl( 2 ) );
   }
}

/*
 * bool visible () const
 */
HB_FUNC( QT_QSCISTYLE_VISIBLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retl( ( p )->visible() );
   }
}

/*
 * void setChangeable (bool changeable)
 */
HB_FUNC( QT_QSCISTYLE_SETCHANGEABLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setChangeable( hb_parl( 2 ) );
   }
}

/*
 * bool changeable () const
 */
HB_FUNC( QT_QSCISTYLE_CHANGEABLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retl( ( p )->changeable() );
   }
}

/*
 * void setHotspot (bool hotspot)
 */
HB_FUNC( QT_QSCISTYLE_SETHOTSPOT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->setHotspot( hb_parl( 2 ) );
   }
}

/*
 * bool hotspot () const
 */
HB_FUNC( QT_QSCISTYLE_HOTSPOT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      hb_retl( ( p )->hotspot() );
   }
}

/*
 * void refresh ()
 */
HB_FUNC( QT_QSCISTYLE_REFRESH )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      ( p )->refresh();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
