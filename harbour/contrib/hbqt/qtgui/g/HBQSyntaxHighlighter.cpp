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
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSyntaxHighlighter>
#include "hbqt_hbqsyntaxhighlighter.h"

/*
 * HBQSyntaxHighlighter ( QTextDocument * textDocument )
 *
 */

typedef struct
{
   QPointer< HBQSyntaxHighlighter > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQSyntaxHighlighter;

HBQT_GC_FUNC( hbqt_gcRelease_HBQSyntaxHighlighter )
{
   HBQT_GC_T_HBQSyntaxHighlighter * p = ( HBQT_GC_T_HBQSyntaxHighlighter * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      HBQSyntaxHighlighter * ph = p->ph;
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

void * hbqt_gcAllocate_HBQSyntaxHighlighter( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQSyntaxHighlighter * p = ( HBQT_GC_T_HBQSyntaxHighlighter * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQSyntaxHighlighter ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQSyntaxHighlighter >( ( HBQSyntaxHighlighter * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQSyntaxHighlighter;
   p->type = HBQT_TYPE_HBQSyntaxHighlighter;

   return p;
}

HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER )
{
   HBQSyntaxHighlighter * pObj = NULL;

   pObj = new HBQSyntaxHighlighter( hbqt_par_QTextDocument( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_HBQSyntaxHighlighter( ( void * ) pObj, true ) );
}

/* void hbSetMultiLineCommentFormat( const QTextCharFormat & format ) */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETMULTILINECOMMENTFORMAT )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
      ( p )->hbSetMultiLineCommentFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/* void hbSetSingleLineCommentFormat( const QTextCharFormat & format ) */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETSINGLELINECOMMENTFORMAT )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
      ( p )->hbSetSingleLineCommentFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/* void hbSetRule( QString name, QString pattern, QTextCharFormat & format ) */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETRULE )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbSetRule( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), *hbqt_par_QTextCharFormat( 4 ) );
      hb_strfree( pText );
   }
}

/* void hbSetFormat( QString name, const QTextCharFormat & format ) */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETFORMAT )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbSetFormat( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QTextCharFormat( 3 ) );
      hb_strfree( pText );
   }
}

/* void hbSetFormatColumnSelection( int start, int count, const QColor & color ) */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETFORMATCOLUMNSELECTION )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
      ( p )->hbSetFormatColumnSelection( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QColor( 4 ) );
}

/* void hbSetRuleWithRegExp( QString name, const QRegExp & reg, const QTextCharFormat & format ) */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_HBSETRULEWITHREGEXP )
{
   HBQSyntaxHighlighter * p = hbqt_par_HBQSyntaxHighlighter( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbSetRuleWithRegExp( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QRegExp( 3 ), *hbqt_par_QTextCharFormat( 4 ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
