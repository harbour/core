/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

#ifndef __HBQTWEBKIT_H
#define __HBQTWEBKIT_H

#include "hbqt.h"

extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebFrame );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebHistory );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebHistoryInterface );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebHistoryItem );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebHitTestResult );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebPage );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebPluginFactory );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebSecurityOrigin );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebSettings );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QWebView );

extern HB_EXPORT void * hbqt_gcAllocate_QWebFrame( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebHistory( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebHistoryInterface( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebHistoryItem( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebHitTestResult( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebPage( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebPluginFactory( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebSecurityOrigin( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebSettings( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWebView( void * pObj, bool bNew );

#define hbqt_par_QWebFrame( n )                                 ( ( QWebFrame                                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHistory( n )                               ( ( QWebHistory                                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHistoryInterface( n )                      ( ( QWebHistoryInterface                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHistoryItem( n )                           ( ( QWebHistoryItem                             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHitTestResult( n )                         ( ( QWebHitTestResult                           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebPage( n )                                  ( ( QWebPage                                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebPluginFactory( n )                         ( ( QWebPluginFactory                           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebSecurityOrigin( n )                        ( ( QWebSecurityOrigin                          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebSettings( n )                              ( ( QWebSettings                                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebView( n )                                  ( ( QWebView                                    * ) hbqt_gcpointer( n ) )

#define HBQT_TYPE_QTWEBKIT_BASE                                 60000

#define HBQT_TYPE_QWebFrame                                     ( HBQT_TYPE_QTWEBKIT_BASE + 1 )
#define HBQT_TYPE_QWebHistory                                   ( HBQT_TYPE_QTWEBKIT_BASE + 2 )
#define HBQT_TYPE_QWebHistoryInterface                          ( HBQT_TYPE_QTWEBKIT_BASE + 3 )
#define HBQT_TYPE_QWebHistoryItem                               ( HBQT_TYPE_QTWEBKIT_BASE + 4 )
#define HBQT_TYPE_QWebHitTestResult                             ( HBQT_TYPE_QTWEBKIT_BASE + 5 )
#define HBQT_TYPE_QWebPage                                      ( HBQT_TYPE_QTWEBKIT_BASE + 6 )
#define HBQT_TYPE_QWebPluginFactory                             ( HBQT_TYPE_QTWEBKIT_BASE + 7 )
#define HBQT_TYPE_QWebSecurityOrigin                            ( HBQT_TYPE_QTWEBKIT_BASE + 8 )
#define HBQT_TYPE_QWebSettings                                  ( HBQT_TYPE_QTWEBKIT_BASE + 9 )
#define HBQT_TYPE_QWebView                                      ( HBQT_TYPE_QTWEBKIT_BASE + 10 )

#endif /* __HBQTWEBKIT_H */
