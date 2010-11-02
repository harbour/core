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
#include "hbqscintilla.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 3/4 [ 75.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  virtual QStringList callTips (const QStringList &context, int commas, QsciScintilla::CallTipsStyle style, QList< int > &shifts)=0
 */

#include <QtCore/QPointer>

#include <qsciabstractapis.h>


/*
 * QsciAbstractAPIs (QsciLexer *lexer)
 * virtual ~QsciAbstractAPIs ()
 *
 */

typedef struct
{
   QPointer< QsciAbstractAPIs > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciAbstractAPIs;

HBQT_GC_FUNC( hbqt_gcRelease_QsciAbstractAPIs )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QsciAbstractAPIs( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciAbstractAPIs * p = ( HBQT_GC_T_QsciAbstractAPIs * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciAbstractAPIs ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciAbstractAPIs >( ( QsciAbstractAPIs * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciAbstractAPIs;
   p->type = HBQT_TYPE_QsciAbstractAPIs;

   return p;
}

HB_FUNC( QT_QSCIABSTRACTAPIS )
{

}

/* QsciLexer * lexer () const */
HB_FUNC( QT_QSCIABSTRACTAPIS_LEXER )
{
   QsciAbstractAPIs * p = hbqt_par_QsciAbstractAPIs( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciLexer( ( p )->lexer(), false ) );
}

/* virtual void updateAutoCompletionList (const QStringList &context, QStringList &list)=0 */
HB_FUNC( QT_QSCIABSTRACTAPIS_UPDATEAUTOCOMPLETIONLIST )
{
   QsciAbstractAPIs * p = hbqt_par_QsciAbstractAPIs( 1 );
   if( p )
      ( p )->updateAutoCompletionList( *hbqt_par_QStringList( 2 ), *hbqt_par_QStringList( 3 ) );
}

/* virtual void autoCompletionSelected (const QString &selection) */
HB_FUNC( QT_QSCIABSTRACTAPIS_AUTOCOMPLETIONSELECTED )
{
   QsciAbstractAPIs * p = hbqt_par_QsciAbstractAPIs( 1 );
   if( p )
   {
      void * pText;
      ( p )->autoCompletionSelected( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
