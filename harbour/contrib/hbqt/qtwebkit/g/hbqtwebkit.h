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

HB_EXTERN_BEGIN

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

HB_EXTERN_END

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

#define HBQT_TYPE_QWebFrame                                     0xC549CCEE
#define HBQT_TYPE_QWebHistory                                   0x24AE8587
#define HBQT_TYPE_QWebHistoryInterface                          0x24436755
#define HBQT_TYPE_QWebHistoryItem                               0x24DEA87F
#define HBQT_TYPE_QWebHitTestResult                             0xA2B0A23E
#define HBQT_TYPE_QWebPage                                      0xCF0E1A25
#define HBQT_TYPE_QWebPluginFactory                             0x8313F829
#define HBQT_TYPE_QWebSecurityOrigin                            0x76EEBFE7
#define HBQT_TYPE_QWebSettings                                  0x77943AAB
#define HBQT_TYPE_QWebView                                      0x25F9078B

#endif /* __HBQTWEBKIT_H */
