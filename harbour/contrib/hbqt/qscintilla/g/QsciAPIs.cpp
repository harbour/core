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
 *  Constructed[ 14/15 [ 93.33% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  virtual QStringList callTips (const QStringList &context, int commas, QsciScintilla::CallTipsStyle style, QList< int > &shifts)
 */

#include <QtCore/QPointer>

#include <qsciapis.h>


/*
 * QsciAPIs (QsciLexer *lexer)
 * virtual ~QsciAPIs ()
 *
 */

typedef struct
{
   QPointer< QsciAPIs > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciAPIs;

HBQT_GC_FUNC( hbqt_gcRelease_QsciAPIs )
{
   HBQT_GC_T_QsciAPIs * p = ( HBQT_GC_T_QsciAPIs * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QsciAPIs * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciAPIs( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciAPIs * p = ( HBQT_GC_T_QsciAPIs * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciAPIs ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciAPIs >( ( QsciAPIs * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciAPIs;
   p->type = HBQT_TYPE_QsciAPIs;

   return p;
}

HB_FUNC( QT_QSCIAPIS )
{
   QsciAPIs * pObj = NULL;

   pObj = new QsciAPIs( hbqt_par_QsciLexer( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QsciAPIs( ( void * ) pObj, true ) );
}

/* void     add (const QString &entry) */
HB_FUNC( QT_QSCIAPIS_ADD )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      ( p )->add( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void     clear () */
HB_FUNC( QT_QSCIAPIS_CLEAR )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->clear();
}

/* bool     load (const QString &fname) */
HB_FUNC( QT_QSCIAPIS_LOAD )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->load( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* void     remove (const QString &entry) */
HB_FUNC( QT_QSCIAPIS_REMOVE )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      ( p )->remove( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void     prepare () */
HB_FUNC( QT_QSCIAPIS_PREPARE )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->prepare();
}

/* void     cancelPreparation () */
HB_FUNC( QT_QSCIAPIS_CANCELPREPARATION )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->cancelPreparation();
}

/* QString  defaultPreparedName () const */
HB_FUNC( QT_QSCIAPIS_DEFAULTPREPAREDNAME )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retstr_utf8( ( p )->defaultPreparedName().toUtf8().data() );
}

/* bool     isPrepared (const QString &fname=QString()) const */
HB_FUNC( QT_QSCIAPIS_ISPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isPrepared( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool     loadPrepared (const QString &fname=QString()) */
HB_FUNC( QT_QSCIAPIS_LOADPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->loadPrepared( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool     savePrepared (const QString &fname=QString()) const */
HB_FUNC( QT_QSCIAPIS_SAVEPREPARED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->savePrepared( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* virtual void updateAutoCompletionList (const QStringList &context, QStringList &list) */
HB_FUNC( QT_QSCIAPIS_UPDATEAUTOCOMPLETIONLIST )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      ( p )->updateAutoCompletionList( *hbqt_par_QStringList( 2 ), *hbqt_par_QStringList( 3 ) );
}

/* virtual void autoCompletionSelected (const QString &sel) */
HB_FUNC( QT_QSCIAPIS_AUTOCOMPLETIONSELECTED )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
   {
      void * pText;
      ( p )->autoCompletionSelected( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual bool event (QEvent *e) */
HB_FUNC( QT_QSCIAPIS_EVENT )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retl( ( p )->event( hbqt_par_QEvent( 2 ) ) );
}

/* QStringList installedAPIFiles () const */
HB_FUNC( QT_QSCIAPIS_INSTALLEDAPIFILES )
{
   QsciAPIs * p = hbqt_par_QsciAPIs( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->installedAPIFiles() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
