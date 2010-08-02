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
#include "hbqtdesigner.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  flags Feature
 *  enum FeatureFlag { EditFeature, GridFeature, TabOrderFeature, DefaultFeature }
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerFormWindowInterface>


/*
 * QDesignerFormWindowInterface ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QDesignerFormWindowInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerFormWindowInterface > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDesignerFormWindowInterface;

QT_G_FUNC( hbqt_gcRelease_QDesignerFormWindowInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesignerFormWindowInterface( void * pObj, bool bNew )
{
   QGC_POINTER_QDesignerFormWindowInterface * p = ( QGC_POINTER_QDesignerFormWindowInterface * ) hb_gcAllocate( sizeof( QGC_POINTER_QDesignerFormWindowInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerFormWindowInterface >( ( QDesignerFormWindowInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormWindowInterface;
   p->type = HBQT_TYPE_QDesignerFormWindowInterface;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDesignerFormWindowInterface  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDesignerFormWindowInterface", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE )
{
   //hb_retptr( new QDesignerFormWindowInterface() );
}

/*
 * virtual void addResourceFile ( const QString & path ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_ADDRESOURCEFILE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->addResourceFile( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_ADDRESOURCEFILE FP=( p )->addResourceFile( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual QString author () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_AUTHOR )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retc( ( p )->author().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_AUTHOR FP=hb_retc( ( p )->author().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual QString comment () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_COMMENT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retc( ( p )->comment().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_COMMENT FP=hb_retc( ( p )->comment().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual QString contents () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CONTENTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retc( ( p )->contents().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_CONTENTS FP=hb_retc( ( p )->contents().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual QDesignerFormEditorInterface * core () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CORE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_CORE FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QDesignerFormWindowCursorInterface * cursor () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CURSOR )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowCursorInterface( ( p )->cursor(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_CURSOR FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowCursorInterface( ( p )->cursor(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual void emitSelectionChanged () = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_EMITSELECTIONCHANGED )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->emitSelectionChanged();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_EMITSELECTIONCHANGED FP=( p )->emitSelectionChanged(); p is NULL" ) );
   }
}

/*
 * virtual QString exportMacro () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_EXPORTMACRO )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retc( ( p )->exportMacro().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_EXPORTMACRO FP=hb_retc( ( p )->exportMacro().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual Feature features () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FEATURES )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retni( ( QDesignerFormWindowInterface::Feature ) ( p )->features() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_FEATURES FP=hb_retni( ( QDesignerFormWindowInterface::Feature ) ( p )->features() ); p is NULL" ) );
   }
}

/*
 * virtual QString fileName () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FILENAME )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retc( ( p )->fileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_FILENAME FP=hb_retc( ( p )->fileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual QPoint grid () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_GRID )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->grid() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_GRID FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->grid() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual bool hasFeature ( Feature feature ) const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_HASFEATURE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retl( ( p )->hasFeature( ( QDesignerFormWindowInterface::Feature ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_HASFEATURE FP=hb_retl( ( p )->hasFeature( ( QDesignerFormWindowInterface::Feature ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual QStringList includeHints () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_INCLUDEHINTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->includeHints() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_INCLUDEHINTS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->includeHints() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual bool isDirty () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_ISDIRTY )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retl( ( p )->isDirty() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_ISDIRTY FP=hb_retl( ( p )->isDirty() ); p is NULL" ) );
   }
}

/*
 * virtual bool isManaged ( QWidget * widget ) const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_ISMANAGED )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retl( ( p )->isManaged( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_ISMANAGED FP=hb_retl( ( p )->isManaged( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void layoutDefault ( int * margin, int * spacing ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_LAYOUTDEFAULT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   int iMargin = 0;
   int iSpacing = 0;

   if( p )
      ( p )->layoutDefault( &iMargin, &iSpacing );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_LAYOUTDEFAULT FP=( p )->layoutDefault( &iMargin, &iSpacing ); p is NULL" ) );
   }

   hb_storni( iMargin, 2 );
   hb_storni( iSpacing, 3 );
}

/*
 * virtual QWidget * mainContainer () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_MAINCONTAINER )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->mainContainer(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_MAINCONTAINER FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->mainContainer(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QString pixmapFunction () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_PIXMAPFUNCTION )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retc( ( p )->pixmapFunction().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_PIXMAPFUNCTION FP=hb_retc( ( p )->pixmapFunction().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual void removeResourceFile ( const QString & path ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_REMOVERESOURCEFILE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->removeResourceFile( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_REMOVERESOURCEFILE FP=( p )->removeResourceFile( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual QStringList resourceFiles () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_RESOURCEFILES )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->resourceFiles() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_RESOURCEFILES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->resourceFiles() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void setAuthor ( const QString & author ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETAUTHOR )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setAuthor( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETAUTHOR FP=( p )->setAuthor( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setComment ( const QString & comment ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETCOMMENT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setComment( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETCOMMENT FP=( p )->setComment( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setContents ( QIODevice * device ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETCONTENTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setContents( hbqt_par_QIODevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETCONTENTS FP=( p )->setContents( hbqt_par_QIODevice( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setExportMacro ( const QString & exportMacro ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETEXPORTMACRO )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setExportMacro( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETEXPORTMACRO FP=( p )->setExportMacro( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setIncludeHints ( const QStringList & includeHints ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETINCLUDEHINTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setIncludeHints( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETINCLUDEHINTS FP=( p )->setIncludeHints( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setLayoutDefault ( int margin, int spacing ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETLAYOUTDEFAULT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setLayoutDefault( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETLAYOUTDEFAULT FP=( p )->setLayoutDefault( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMainContainer ( QWidget * mainContainer ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETMAINCONTAINER )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setMainContainer( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETMAINCONTAINER FP=( p )->setMainContainer( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setPixmapFunction ( const QString & pixmapFunction ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETPIXMAPFUNCTION )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setPixmapFunction( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETPIXMAPFUNCTION FP=( p )->setPixmapFunction( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QDesignerFormWindowInterface * findFormWindow ( QWidget * widget )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FINDFORMWINDOW )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->findFormWindow( hbqt_par_QWidget( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_FINDFORMWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->findFormWindow( hbqt_par_QWidget( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QDesignerFormWindowInterface * findFormWindow ( QObject * object )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FINDFORMWINDOW_1 )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->findFormWindow( hbqt_par_QObject( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_FINDFORMWINDOW_1 FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->findFormWindow( hbqt_par_QObject( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual void clearSelection ( bool update = true ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CLEARSELECTION )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->clearSelection( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_CLEARSELECTION FP=( p )->clearSelection( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void manageWidget ( QWidget * widget ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_MANAGEWIDGET )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->manageWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_MANAGEWIDGET FP=( p )->manageWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void selectWidget ( QWidget * widget, bool select = true ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SELECTWIDGET )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->selectWidget( hbqt_par_QWidget( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SELECTWIDGET FP=( p )->selectWidget( hbqt_par_QWidget( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setContents ( const QString & contents ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETCONTENTS_1 )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setContents( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETCONTENTS_1 FP=( p )->setContents( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setDirty ( bool dirty ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETDIRTY )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setDirty( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETDIRTY FP=( p )->setDirty( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setFeatures ( Feature features ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETFEATURES )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setFeatures( ( QDesignerFormWindowInterface::Feature ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETFEATURES FP=( p )->setFeatures( ( QDesignerFormWindowInterface::Feature ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setFileName ( const QString & fileName ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETFILENAME )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setFileName( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETFILENAME FP=( p )->setFileName( QDesignerFormWindowInterface::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setGrid ( const QPoint & grid ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETGRID )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->setGrid( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_SETGRID FP=( p )->setGrid( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void unmanageWidget ( QWidget * widget ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_UNMANAGEWIDGET )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
      ( p )->unmanageWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMWINDOWINTERFACE_UNMANAGEWIDGET FP=( p )->unmanageWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
