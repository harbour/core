/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The eval stack
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* TOFIX: There are several things in this file which are not part of the
          standard Harbour API, in other words these things are not
          guaranteed to remain unchanged. To avoid confusion these should be
          moved to somewhere else (like hbrtl.h). [vszakats] */

#ifndef HB_STACK_H_
#define HB_STACK_H_

#include "hbvmpub.h"

HB_EXTERN_BEGIN

#if defined( HB_MT_VM ) && defined( _HB_API_INTERNAL_ )
#  include "hbthread.h"
#endif

/* thread specific data */
typedef void (*PHB_TSD_FUNC)(void *);
typedef struct
{
   int          iHandle;
   int          iSize;
   PHB_TSD_FUNC pInitFunc;
   PHB_TSD_FUNC pCleanFunc;
}
HB_TSD, * PHB_TSD;
#define HB_TSD_NEW(name,size,init,clean)  \
        HB_TSD name = { 0, size, init, clean }
#define HB_TSD_INIT(name,size,init,clean) do { \
            (name)->iHandle = 0; \
            (name)->iSize = (size); \
            (name)->pInitFunc = (init); \
            (name)->pCleanFunc = (clean); \
         } while( 0 )

typedef struct
{
   HB_ERRCODE uiFError;
   HB_ERRCODE uiErrorLast;
   HB_ERRCODE uiOsErrorLast;
   HB_ERRCODE uiSocketError;
   int        iSocketOsError;
}
HB_IOERRORS, * PHB_IOERRORS;

typedef struct
{
   const char *   szDefaultRDD;     /* default RDD */
   HB_BOOL        fNetError;        /* current NETERR() flag */

   void **        waList;           /* Allocated WorkAreas */
   HB_USHORT      uiWaMax;          /* Number of allocated WA */
   HB_USHORT      uiWaSpace;        /* Number of allocated WA */

   HB_USHORT *    waNums;           /* Allocated WorkAreas */
   HB_USHORT      uiWaNumMax;       /* Number of allocated WA */

   HB_USHORT      uiCurrArea;       /* Current WokrArea number */
   void *         pCurrArea;        /* Current WorkArea pointer */
}
HB_STACKRDD, * PHB_STACKRDD;

#ifdef _HB_API_INTERNAL_

#include "hbset.h"

typedef struct
{
   PHB_TSD  pTSD;
   void *   value;
}
HB_TSD_HOLDER, * PHB_TSD_HOLDER;

typedef struct
{
   PHB_DYNS    pDynSym;
   PHB_ITEM    pPrevMemvar;
}
HB_PRIVATE_ITEM, * PHB_PRIVATE_ITEM;

typedef struct
{
   PHB_PRIVATE_ITEM stack;
   HB_SIZE          size;
   HB_SIZE          count;
   HB_SIZE          base;
}
HB_PRIVATE_STACK, * PHB_PRIVATE_STACK;

#if defined( HB_MT_VM )
typedef struct
{
   void *     pMemvar;        /* memvar pointer ( publics & privates ) */
   HB_USHORT  uiArea;         /* Workarea number */
}
HB_DYN_HANDLES, * PHB_DYN_HANDLES;
#endif

/* stack managed by the virtual machine */
typedef struct
{
   PHB_ITEM * pPos;           /* pointer to the latest used item */
   PHB_ITEM * pEnd;           /* pointer to the end of stack items */
   PHB_ITEM * pItems;         /* pointer to the stack items */
   PHB_ITEM * pBase;          /* stack frame position for the current function call */
   HB_ITEM    Return;         /* latest returned value */
   HB_ISIZ    nItems;         /* total items that may be held on the stack */
   HB_ISIZ    nWithObject;    /* stack offset to base current WITH OBJECT item */
   HB_ISIZ    nRecoverBase;   /* current SEQUENCE envelope offset or 0 if no SEQUENCE is active */
   HB_USHORT  uiActionRequest;/* request for some action - stop processing of opcodes */
   HB_USHORT  uiQuitState;    /* HVM is quiting */
   HB_STACK_STATE state;      /* first (default) stack state frame */
   HB_STACKRDD rdd;           /* RDD related data */
   char       szDate[ 9 ];    /* last returned date from hb_pards() YYYYMMDD format */
   void *     pCDP;           /* current codepage module */
   void *     pLang;          /* current language module */
   void *     pI18N;          /* current I18N module */
   void *     hGT;            /* current GT module */
   int        iTSD;           /* number of allocated TSD holders */
   PHB_TSD_HOLDER pTSD;       /* thread specific data holder */
   void *     pStatics;       /* statics base for the current function call */
   HB_PRIVATE_STACK privates; /* private variables stack */
   HB_SET_STRUCT set;
   int        iKeyPoll;       /* counter for GT/keyboard polling */
   HB_BOOL    fDebugRequest;  /* request debugger activation */
   void *     pDebugInfo;     /* internal debugger structure */
#if defined( HB_MT_VM )
   int        iUnlocked;      /* counter for nested hb_vmUnlock() calls */
   PHB_DYN_HANDLES pDynH;     /* dynamic symbol handles */
   int        iDynH;          /* number of dynamic symbol handles */
   void *     pStackLst;      /* this stack entry in stack linked list */
   HB_IOERRORS IOErrors;      /* MT safe buffer for IO errors */
   HB_TRACEINFO traceInfo;    /* MT safe buffer for HB_TRACE data */
   char *     pDirBuffer;     /* MT safe buffer for hb_fsCurDir() results */
   void *     allocator;      /* memory manager global struct pointer */
#endif
} HB_STACK, * PHB_STACK;

#if defined( _HB_STACK_MACROS_ )
#  if defined( HB_MT_VM )
#     if defined( HB_USE_TLS )
#        if !defined( _HB_STACK_LOCAL_MACROS_ )
#           if defined( __BORLANDC__ )
               extern PHB_STACK HB_TLS_ATTR hb_stack_ptr;
#           else
               extern HB_TLS_ATTR PHB_STACK hb_stack_ptr;
#           endif
#        endif
#     else
#        if !defined( _HB_STACK_LOCAL_MACROS_ )
            extern HB_TLS_KEY hb_stack_key;
#        endif
#        if defined( __BORLANDC__ ) && defined( HB_STACK_PRELOAD ) && \
            !defined( HB_OS_WIN_64 ) && !defined( HB_OS_WIN_CE ) && \
            defined( HB_ASM_TLS )
#           if defined( _HB_STACK_LOCAL_MACROS_ )
               static HB_TLS_KEY hb_stack_key;
#           endif
            static __inline void * hb_stack_ptr_from_tls( void )
            {
               /* mov ecx,hb_stack_key */
               _ECX = hb_stack_key;
               /* mov eax,dword ptr fs:[00000018h] */
               __emit__( 0x64, 0xA1, 0x18, 0x00, 0x00, 0x00 );
               /* mov eax,[eax+ecx*4+00000E10h] */
               __emit__( 0x8B, 0x84, 0x88, 0x10, 0x0E, 0x00, 0x00 );
               /* ret (if function is not inlined) */
               return (void*) _EAX;
            }
#           define hb_stack_ptr_get()    hb_stack_ptr_from_tls()
#        elif defined( __MINGW32__ ) && defined( HB_ASM_TLS ) && \
              !defined( HB_OS_WIN_64 ) && !defined( HB_OS_WIN_CE )
#           if defined( _HB_STACK_LOCAL_MACROS_ )
               static HB_TLS_KEY hb_stack_key;
#           endif
            static __inline__  __attribute__ ((pure, malloc)) void * hb_stack_ptr_from_tls( void )
            {
               void * p;
               __asm__ (
                  "movl  %%fs:(0x18), %0\n\t"
                  "movl  0x0e10(%0,%1,4), %0\n\t"
                  :"=a" (p)
                  :"c" (hb_stack_key)
               );
               return p;
            }
#           define hb_stack_ptr_get()    hb_stack_ptr_from_tls()
#           define hb_stack_ptr  ( ( PHB_STACK ) hb_stack_ptr_from_tls() )
#        endif
#        if !defined( hb_stack_ptr )
#           define hb_stack_ptr  ( ( PHB_STACK ) hb_tls_get( hb_stack_key ) )
#        endif
#     endif
#     if defined( HB_STACK_PRELOAD ) && !defined( HB_USE_TLS )
#        if defined( hb_stack_ptr_get )
#           define HB_STACK_TLS_RELOAD    _hb_stack_ptr_ = ( PHB_STACK ) hb_stack_ptr_get();
#           undef hb_stack_ptr
#        else
#           define HB_STACK_TLS_RELOAD    _hb_stack_ptr_ = hb_stack_ptr;
#        endif
#        define HB_STACK_TLS_PRELOAD   PHB_STACK HB_STACK_TLS_RELOAD
#        define hb_stack            ( * _hb_stack_ptr_ )
#        define hb_stack_ref()      ( _hb_stack_ptr_ )
#     else
#        define hb_stack            ( * hb_stack_ptr )
#        define hb_stack_ref()      ( hb_stack_ptr )
#     endif
#  else
#     if !defined( _HB_STACK_LOCAL_MACROS_ )
         extern HB_STACK hb_stack;
#     endif
#     define hb_stack_ref()         ( &hb_stack )
#  endif
#endif
#if !defined( HB_STACK_TLS_PRELOAD )
#  if defined( HB_STACK_PRELOAD )
#     define HB_STACK_TLS_PRELOAD
#     define HB_STACK_TLS_RELOAD
#     undef  HB_STACK_PRELOAD
#  elif defined( _HB_STACK_MACROS_ )
#     define HB_STACK_TLS_PRELOAD
#  endif
#endif

#endif /* _HB_API_INTERNAL_ */

extern HB_EXPORT void *      hb_stackId( void );
extern HB_EXPORT HB_ITEM_PTR hb_stackItemFromTop( int nFromTop );
extern HB_EXPORT HB_ITEM_PTR hb_stackItemFromBase( int nFromBase );
extern HB_EXPORT HB_ITEM_PTR hb_stackBaseItem( void );
extern HB_EXPORT HB_ITEM_PTR hb_stackSelfItem( void );   /* returns Self object at C function level */
extern HB_EXPORT HB_ITEM_PTR hb_stackReturnItem( void ); /* returns RETURN Item from stack */

extern HB_EXPORT HB_ITEM_PTR hb_stackAllocItem( void );  /* allocates new item on the top of stack, returns pointer to it */
extern HB_EXPORT void        hb_stackPop( void );        /* pops an item from the stack */

extern void        hb_stackPush( void );                 /* pushes an item on to the stack */
extern void        hb_stackPushReturn( void );
extern void        hb_stackPopReturn( void );
extern void        hb_stackRemove( HB_ISIZ nUntilPos );

extern           HB_ISIZ     hb_stackTopOffset( void );
extern HB_EXPORT HB_ISIZ     hb_stackBaseOffset( void );
extern           HB_ISIZ     hb_stackTotalItems( void );
extern HB_EXPORT HB_ITEM_PTR hb_stackItem( HB_ISIZ nItemPos );
extern           char *      hb_stackDateBuffer( void );

/* stack management functions */
extern HB_EXPORT int         hb_stackCallDepth( void );
extern HB_EXPORT void        hb_stackBaseProcInfo( char * szProcName, HB_USHORT * puiProcLine ); /* get current .prg function name and line number */

extern HB_EXPORT HB_ISIZ     hb_stackBaseProcOffset( int iLevel );
extern           void        hb_stackDispCall( void );
extern           void        hb_stackFree( void );       /* releases all memory used by the stack */
extern           void        hb_stackInit( void );       /* initializes the stack */
extern           void        hb_stackIncrease( void );   /* increase the stack size */

/* thread specific data */
extern HB_EXPORT void * hb_stackGetTSD( PHB_TSD pTSD );
extern HB_EXPORT void * hb_stackTestTSD( PHB_TSD pTSD );
extern HB_EXPORT void   hb_stackReleaseTSD( PHB_TSD pTSD );

extern char *       hb_stackDirBuffer( void );
extern PHB_IOERRORS hb_stackIOErrors( void );
extern void *       hb_stackGetGT( void );
extern void         hb_stackSetGT( void * );
extern PHB_STACKRDD hb_stackRDD( void );

extern HB_EXPORT void ** hb_stackDebugInfo( void );

#ifdef _HB_API_INTERNAL_
extern void        hb_stackDec( void );
extern void        hb_stackDecrease( HB_SIZE nItems );
extern HB_ITEM_PTR hb_stackNewFrame( PHB_STACK_STATE pFrame, HB_USHORT uiParams );
extern void        hb_stackOldFrame( PHB_STACK_STATE pFrame );
extern void        hb_stackClearMemvarsBase( void );

extern HB_ITEM_PTR hb_stackLocalVariable( int * piFromBase );
extern PHB_ITEM ** hb_stackItemBasePtr( void );

extern HB_EXPORT HB_ISIZ     hb_stackGetRecoverBase( void );
extern           void        hb_stackSetRecoverBase( HB_ISIZ nBase );
extern           HB_USHORT   hb_stackGetActionRequest( void );
extern           void        hb_stackSetActionRequest( HB_USHORT uiAction );

extern void        hb_stackSetStaticsBase( void * pBase );
extern void *      hb_stackGetStaticsBase( void );

extern           PHB_ITEM    hb_stackWithObjectItem( void );
extern HB_EXPORT HB_ISIZ     hb_stackWithObjectOffset( void );
extern           void        hb_stackWithObjectSetOffset( HB_ISIZ nOffset );

extern int *       hb_stackKeyPolls( void );
extern HB_BOOL *   hb_stackDebugRequest( void );

extern void        hb_stackDestroyTSD( void );

extern PHB_PRIVATE_STACK hb_stackGetPrivateStack( void );
extern void *      hb_stackGetCDP( void );
extern void        hb_stackSetCDP( void * );
extern void *      hb_stackGetLang( void );
extern void        hb_stackSetLang( void * );
extern void *      hb_stackGetI18N( void );
extern void        hb_stackSetI18N( void * );

extern void        hb_stackIsStackRef( void *, PHB_TSD_FUNC );
extern void        hb_stackUpdateAllocator( void *, PHB_ALLOCUPDT_FUNC, int );

#if defined( HB_MT_VM )
   extern void *           hb_stackList( void );
   extern void             hb_stackListSet( void * pStackLst );
   extern void             hb_stackIdSetActionRequest( void * pStackID, HB_USHORT uiAction );
   extern PHB_DYN_HANDLES  hb_stackGetDynHandle( PHB_DYNS pDynSym );
   extern int              hb_stackDynHandlesCount( void );
   extern void             hb_stackClearMemvars( int );
   extern HB_BOOL          hb_stackQuitState( void );
   extern void             hb_stackSetQuitState( HB_USHORT uiState );
   extern int              hb_stackUnlock( void );
   extern int              hb_stackLock( void );
   extern int              hb_stackLockCount( void );
   extern void *           hb_stackAllocator( void );
#endif

#endif /* _HB_API_INTERNAL_ */

#if defined( _HB_API_INTERNAL_ ) || defined( _HB_SET_INTERNAL_ )
   extern PHB_SET_STRUCT hb_stackSetStruct( void );
#endif


#if defined( _HB_STACK_MACROS_ )

#define hb_stackItemFromTop( n )    ( * ( hb_stack.pPos + ( int ) ( n ) ) )
#define hb_stackItemFromBase( n )   ( * ( hb_stack.pBase + ( int ) ( n ) + 1 ) )
#define hb_stackTopOffset( )        ( hb_stack.pPos - hb_stack.pItems )
#define hb_stackBaseOffset( )       ( hb_stack.pBase - hb_stack.pItems + 1 )
/* #define hb_stackTotalItems( )       ( hb_stack.nItems ) */
#define hb_stackBaseItem( )         ( * hb_stack.pBase )
#define hb_stackSelfItem( )         ( * ( hb_stack.pBase + 1 ) )
#define hb_stackItem( iItemPos )    ( * ( hb_stack.pItems + ( HB_ISIZ ) ( iItemPos ) ) )
#define hb_stackReturnItem( )       ( &hb_stack.Return )
#define hb_stackDateBuffer( )       ( hb_stack.szDate )
#define hb_stackItemBasePtr( )      ( &hb_stack.pItems )
#define hb_stackGetStaticsBase( )   ( hb_stack.pStatics )
#define hb_stackSetStaticsBase( p ) do { hb_stack.pStatics = ( p ); } while ( 0 )
#define hb_stackGetRecoverBase( )   ( hb_stack.nRecoverBase )
#define hb_stackSetRecoverBase( n ) do { hb_stack.nRecoverBase = ( n ); } while( 0 )
#define hb_stackGetActionRequest( ) ( hb_stack.uiActionRequest )
#define hb_stackSetActionRequest( n )     do { hb_stack.uiActionRequest = ( n ); } while( 0 )
#define hb_stackWithObjectItem( )   ( hb_stack.nWithObject ? * ( hb_stack.pItems + hb_stack.nWithObject ) : NULL )
#define hb_stackWithObjectOffset( ) ( hb_stack.nWithObject )
#define hb_stackWithObjectSetOffset( n )  do { hb_stack.nWithObject = ( n ); } while( 0 )
#define hb_stackGetCDP( )           ( hb_stack.pCDP )
#define hb_stackSetCDP( p )         do { hb_stack.pCDP = ( p ); } while ( 0 )
#define hb_stackGetLang( )          ( hb_stack.pLang )
#define hb_stackSetLang( p )        do { hb_stack.pLang = ( p ); } while ( 0 )
#define hb_stackGetI18N( )          ( hb_stack.pI18N )
#define hb_stackSetI18N( p )        do { hb_stack.pI18N = ( p ); } while ( 0 )

#define hb_stackId( )               ( ( void * ) hb_stack_ref() )
#if defined( HB_MT_VM )
#  define hb_stackList()            ( hb_stack.pStackLst )
#  define hb_stackListSet( p )      do { hb_stack.pStackLst = ( p ); } while ( 0 )
#  define hb_stackDynHandlesCount() ( hb_stack.iDynH )
#  define hb_stackQuitState( )      ( hb_stack.uiQuitState != 0 )
#  define hb_stackSetQuitState( n ) do { hb_stack.uiQuitState = ( n ); } while( 0 )
#  define hb_stackUnlock()          ( ++hb_stack.iUnlocked )
#  define hb_stackLock()            ( --hb_stack.iUnlocked )
#  define hb_stackLockCount()       ( hb_stack.iUnlocked )
#endif

#define hb_stackAllocItem( )        ( ( ++hb_stack.pPos == hb_stack.pEnd ? \
                                        hb_stackIncrease() : ( void ) 0 ), \
                                      * ( hb_stack.pPos - 1 ) )

#ifdef HB_STACK_SAFEMACROS

#define hb_stackDecrease( n )       do { \
                                       if( ( hb_stack.pPos -= (n) ) <= hb_stack.pBase ) \
                                          hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                    } while ( 0 )

#define hb_stackDec( )              do { \
                                       if( --hb_stack.pPos <= hb_stack.pBase ) \
                                          hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                    } while ( 0 )

#define hb_stackPop( )              do { \
                                       if( --hb_stack.pPos <= hb_stack.pBase ) \
                                          hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                       if( HB_IS_COMPLEX( * hb_stack.pPos ) ) \
                                          hb_itemClear( * hb_stack.pPos ); \
                                    } while ( 0 )

#define hb_stackPopReturn( )        do { \
                                       if( HB_IS_COMPLEX( &hb_stack.Return ) ) \
                                          hb_itemClear( &hb_stack.Return ); \
                                       if( --hb_stack.pPos <= hb_stack.pBase ) \
                                          hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                       hb_itemRawMove( &hb_stack.Return, * hb_stack.pPos ); \
                                    } while ( 0 )

#else

#define hb_stackDecrease( n )       do { hb_stack.pPos -= (n); } while ( 0 )
#define hb_stackDec( )              do { --hb_stack.pPos; } while ( 0 )
#define hb_stackPop( )              do { --hb_stack.pPos; \
                                       if( HB_IS_COMPLEX( * hb_stack.pPos ) ) \
                                          hb_itemClear( * hb_stack.pPos ); \
                                    } while ( 0 )
#define hb_stackPopReturn( )        do { \
                                       if( HB_IS_COMPLEX( &hb_stack.Return ) ) \
                                          hb_itemClear( &hb_stack.Return ); \
                                       --hb_stack.pPos; \
                                       hb_itemRawMove( &hb_stack.Return, * hb_stack.pPos ); \
                                    } while ( 0 )

#endif /* HB_STACK_SAFEMACROS */

#define hb_stackPush( )             do { \
                                       if( ++hb_stack.pPos == hb_stack.pEnd ) \
                                          hb_stackIncrease(); \
                                    } while ( 0 )

#define hb_stackPushReturn( )       do { \
                                       hb_itemRawMove( * hb_stack.pPos, &hb_stack.Return ); \
                                       if( ++hb_stack.pPos == hb_stack.pEnd ) \
                                          hb_stackIncrease(); \
                                    } while ( 0 )

#define hb_stackLocalVariable( p )  ( ( ( ( *hb_stack.pBase )->item.asSymbol.paramcnt > \
                                          ( * hb_stack.pBase )->item.asSymbol.paramdeclcnt ) && \
                                        ( * (p) ) > ( * hb_stack.pBase )->item.asSymbol.paramdeclcnt ) ? \
                                      ( * ( hb_stack.pBase + ( int ) ( * (p) += \
                                          ( * hb_stack.pBase )->item.asSymbol.paramcnt - \
                                          ( * hb_stack.pBase )->item.asSymbol.paramdeclcnt ) + 1 ) ) : \
                                      ( * ( hb_stack.pBase + ( int ) ( * (p) ) + 1 ) ) )

#define hb_stackGetPrivateStack( )  ( &hb_stack.privates )
#define hb_stackSetStruct( )        ( &hb_stack.set )
#define hb_stackKeyPolls( )         ( &hb_stack.iKeyPoll )
#define hb_stackDebugRequest( )     ( &hb_stack.fDebugRequest )
#define hb_stackDebugInfo( )        ( &hb_stack.pDebugInfo )

#endif


HB_EXTERN_END

#endif /* HB_STACK_H_ */
