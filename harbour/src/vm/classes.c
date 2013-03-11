/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base-routines for OOPS system
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    :CLASSSEL()
 *    __clsDelMsg()
 *    __clsModMsg()
 *    __clsInstSuper()
 *    __cls_CntClsData()
 *    __cls_CntData()
 *    __cls_DecData()
 *    __cls_IncData()
 *    __objClone()
 *    __objHasMsg()
 *    __objSendMsg()
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    __classNew()
 *    __classInstance()
 *    __classAdd()
 *    __className()
 *    __classSel() (based on hb___msgClsSel())
 *
 * Copyright 1999 Janica Lubos <janica@fornax.elf.stuba.sk>
 *    hb_clsDictRealloc()
 *
 * Copyright 2000-2007 JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <cakiral@altern.org
 *    Multiple inheritence fully implemented
 *    Forwarding, delegating
 *    Data initialisation & Autoinit for Bool and Numeric
 *    Scoping : Protected / exported
 *
 * Copyright 2008- JF. Lefebvre <jfl@mafact.com>
 *    hb_clsDictRealloc()   New version
 *    Now support of shared and not shared class data
 *    Multiple datas declaration fully supported
 *
 *    2000 RGlab
 *    Garbage collector fixe
 *
 * Copyright 2001 JF. Lefebvre <jfl@mafact.com>
 *    Super msg corrected
 *    Scoping : working for protected, hidden and readonly
 *    To Many enhancement and correction to give a full list :-)
 *    Improved class(y) compatibility
 *    Improved TopClass compatibility
 *    __CLS_PAR00() (Allow the creation of class wich not autoinherit of the default HBObject())
 *    Adding HB_CLS_ENFORCERO FLAG to disable Write access to RO VAR
 *    outside of Constructors /!\ Could be related to some incompatibility
 *    Added hb_objGetRealClsName to keep a full class tree (for 99% cases)
 *    Fixed hb_clsIsParent
 *
 *
 *    hb_objGetMthd() & __clsAddMsg() modified to translate the followings operators
 *
 "+"     = __OpPlus
 "-"     = __OpMinus
 "*"     = __OpMult
 "/"     = __OpDivide
 "%"     = __OpMod
 "^"     = __OpPower
 "**"    = __OpPower
 "++"    = __OpInc
 "--"    = __OpDec
 "=="    = __OpEqual
 "="     = __OpEqual (same as "==")
 "!="    = __OpNotEqual
 "<>"    = __OpNotEqual (same as "!=")
 "#"     = __OpNotEqual (same as "!=")
 "<"     = __OpLess
 "<="    = __OpLessEqual
 ">"     = __OpGreater
 ">="    = __OpGreaterEqual
 "$"     = __OpInstring
 "$$"    = __OpInclude
 "!"     = __OpNot
 ".NOT." = __OpNot (same as "!")
 ".AND." = __OpAnd
 ".OR."  = __OpOr
 ":="    = __OpAssign   ... not tested ...
 "[]"    = __OpArrayIndex
 *
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapicls.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbthread.h"
#include "hboo.ch"

typedef struct
{
   HB_USHORT   uiClass;
   HB_USHORT   uiOffset;
} HB_CLSCAST, * PHB_CLSCAST;

typedef struct
{
   PHB_ITEM    pInitValue;       /* Init Value for data */
   HB_USHORT   uiType;           /* HB_OO_MSG_DATA, HB_OO_MSG_CLASSDATA or HB_OO_MSG_INITIALIZED */
   HB_USHORT   uiData;           /* Item position in instance area or class data */
   HB_USHORT   uiOffset;         /* Super cast instance area offset for HB_OO_MSG_DATA or real class item position */
   HB_USHORT   uiSprClass;       /* The real class where method were defined */
} INITDATA, * PINITDATA;

typedef struct
{
   PHB_DYNS    pMessage;         /* Method symbolic name */
   PHB_DYNS    pAccMsg;          /* Corresponding access method symbolic name */
   PHB_SYMB    pFuncSym;         /* Function symbol */
   PHB_SYMB    pRealSym;         /* Real function symbol when wrapper is used */
   HB_TYPE     itemType;         /* Type of item in restricted assignment */
   HB_USHORT   uiSprClass;       /* Originalclass'handel (super or current class'handel if not herited). */ /*Added by RAC&JF*/
   HB_USHORT   uiScope;          /* Scoping value */
   HB_USHORT   uiData;           /* Item position for instance data, class data and shared data (Harbour like, begin from 1), supercast class or delegated message index object */
   HB_USHORT   uiOffset;         /* position in pInitData for class datas (from 1) or offset to instance area in inherited instance data and supercast messages (from 0) */
   HB_USHORT   uiPrevCls;
   HB_USHORT   uiPrevMth;
#ifndef HB_NO_PROFILER
   HB_ULONG    ulCalls;          /* profiler support */
   HB_ULONG    ulTime;           /* profiler support */
   HB_ULONG    ulRecurse;        /* profiler support */
#endif
} METHOD, * PMETHOD;


#define HB_MSG_POOL
typedef struct
{
   char *      szName;           /* Class name */
   PHB_DYNS    pClassSym;        /* Class symbolic name */
   PMETHOD     pMethods;         /* Class methods */
   PHB_SYMB    pClassFuncSym;    /* Class function symbol */
   PHB_SYMB    pFriendModule;    /* Class friend symbols */
   PINITDATA   pInitData;        /* Class/instance Initialization data */
   PHB_ITEM    pClassDatas;      /* Harbour Array for Class Datas */
   PHB_ITEM    pSharedDatas;     /* Harbour Array for Class Shared Datas */
   PHB_ITEM    pInlines;         /* Array for inline codeblocks */
   PHB_ITEM    pMutex;           /* Class sync method mutex */
   PHB_SYMB *  pFriendSyms;      /* Friend functions' symbols */
   PHB_CLSCAST pSuperClasses;    /* Super classes */
   HB_U32      nOpFlags;         /* Flags for overloaded operators */
   HB_USHORT   uiClass;          /* This class handle */
   HB_USHORT   fHasDestructor;   /* has the class destructor message? */
   HB_USHORT   fHasOnError;      /* has the class OnError message? */
   HB_USHORT   fLocked;          /* Class is locked against modifications */
   HB_USHORT   uiMethods;        /* Total Method initialised Counter */
   HB_USHORT   uiInitDatas;      /* Total Method initialised Counter */
   HB_USHORT   uiDatas;          /* Total Data Counter */
   HB_USHORT   uiDataFirst;      /* First instance item from this class */
   HB_USHORT   uiSuperClasses;   /* Number of super classes */
   HB_USHORT   uiFriendSyms;     /* Number of friend function's symbols */
   HB_USHORT   uiFriendModule;   /* Number of friend symbols in pFriendModule */
   HB_USHORT   uiMutexOffset;    /* Offset in instance area to SYNC method mutex */
   HB_USHORT   uiHashKey;
#ifdef HB_MSG_POOL
   HB_USHORT * puiMsgIdx;
   HB_USHORT   uiMethodCount;
#endif
} CLASS, * PCLASS;

#define BUCKETBITS      2
#define BUCKETSIZE      ( 1 << BUCKETBITS )
#define BUCKETMASK      ( BUCKETSIZE - 1 )
#define HASHBITS        3
#define HASH_KEY        ( ( 1 << HASHBITS ) - 1 )
#define HASH_KEYMAX     ( 1 << ( 16 - BUCKETBITS ) )
#define hb_clsInited(p) ( (p)->pMethods != NULL )
#define hb_clsBucketPos( p, m )     ( ( (p)->uiSymNum & (m) ) << BUCKETBITS )

#ifdef HB_MSG_POOL
#  define hb_clsMthNum( p )      ( ( HB_SIZE ) ( p )->uiMethodCount )
#else
#  define hb_clsMthNum( p )      ( ( ( HB_SIZE ) ( p )->uiHashKey + 1 ) << BUCKETBITS )
#endif

#if defined( HB_REAL_BLOCK_SCOPE )
#  undef HB_CLASSY_BLOCK_SCOPE
#elif ! defined( HB_CLASSY_BLOCK_SCOPE )
#  define HB_REAL_BLOCK_SCOPE
#endif

#if ! defined( HB_CLASSY_BLOCK_SCOPE )
#  define hb_clsSenderOffset()  hb_stackBaseProcOffset( 1 )
#endif


HB_FUNC_STATIC( msgGetData );
HB_FUNC_STATIC( msgSetData );
HB_FUNC_STATIC( msgGetClsData );
HB_FUNC_STATIC( msgSetClsData );
HB_FUNC_STATIC( msgGetShrData );
HB_FUNC_STATIC( msgSetShrData );
HB_FUNC_STATIC( msgEvalInline );
HB_FUNC_STATIC( msgVirtual );
HB_FUNC_STATIC( msgSuper );
HB_FUNC_STATIC( msgRealClass );
HB_FUNC_STATIC( msgPerform );
HB_FUNC_STATIC( msgDelegate );
HB_FUNC_STATIC( msgSync );
HB_FUNC_STATIC( msgSyncClass );
HB_FUNC_STATIC( msgNoMethod );
HB_FUNC_STATIC( msgScopeErr );
HB_FUNC_STATIC( msgTypeErr );
HB_FUNC_STATIC( msgNull );

HB_FUNC_STATIC( msgClassH );
HB_FUNC_STATIC( msgClassName );
HB_FUNC_STATIC( msgClassSel );
/* HB_FUNC_STATIC( msgClass ); */
/* HB_FUNC_STATIC( msgClassParent ); */

/* ================================================ */

/* static variables and structures initialized at HVM startup which
 * do not need any synchronization mechanism in MT mode, [druzus]
 */

/*
 * The positions of items in symbol table below have to correspond
 * to HB_OO_OP_* constants in hbapicls.h, [druzus]
 */
static HB_SYMB s_opSymbols[ HB_OO_MAX_OPERATOR + 1 ] = {
   { "__OPPLUS",              {HB_FS_MESSAGE}, {NULL}, NULL },  /* 00 */
   { "__OPMINUS",             {HB_FS_MESSAGE}, {NULL}, NULL },  /* 01 */
   { "__OPMULT",              {HB_FS_MESSAGE}, {NULL}, NULL },  /* 02 */
   { "__OPDIVIDE",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 03 */
   { "__OPMOD",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 04 */
   { "__OPPOWER",             {HB_FS_MESSAGE}, {NULL}, NULL },  /* 05 */
   { "__OPINC",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 06 */
   { "__OPDEC",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 07 */
   { "__OPEQUAL",             {HB_FS_MESSAGE}, {NULL}, NULL },  /* 08 */
   { "__OPEXACTEQUAL",        {HB_FS_MESSAGE}, {NULL}, NULL },  /* 09 */
   { "__OPNOTEQUAL",          {HB_FS_MESSAGE}, {NULL}, NULL },  /* 10 */
   { "__OPLESS",              {HB_FS_MESSAGE}, {NULL}, NULL },  /* 11 */
   { "__OPLESSEQUAL",         {HB_FS_MESSAGE}, {NULL}, NULL },  /* 12 */
   { "__OPGREATER",           {HB_FS_MESSAGE}, {NULL}, NULL },  /* 13 */
   { "__OPGREATEREQUAL",      {HB_FS_MESSAGE}, {NULL}, NULL },  /* 14 */
   { "__OPASSIGN",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 15 */
   { "__OPINSTRING",          {HB_FS_MESSAGE}, {NULL}, NULL },  /* 16 */
   { "__OPINCLUDE",           {HB_FS_MESSAGE}, {NULL}, NULL },  /* 17 */
   { "__OPNOT",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 18 */
   { "__OPAND",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 19 */
   { "__OPOR",                {HB_FS_MESSAGE}, {NULL}, NULL },  /* 20 */
   { "__OPARRAYINDEX",        {HB_FS_MESSAGE}, {NULL}, NULL },  /* 21 */
   { "__ENUMINDEX",           {HB_FS_MESSAGE}, {NULL}, NULL },  /* 22 */
   { "__ENUMBASE",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 23 */
   { "__ENUMVALUE",           {HB_FS_MESSAGE}, {NULL}, NULL },  /* 24 */
   { "__ENUMSTART",           {HB_FS_MESSAGE}, {NULL}, NULL },  /* 25 */
   { "__ENUMSKIP",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 26 */
   { "__ENUMSTOP",            {HB_FS_MESSAGE}, {NULL}, NULL }   /* 27 */
};

static HB_SYMB s___msgDestructor = { "__msgDestructor", {HB_FS_MESSAGE}, {NULL},               NULL };
static HB_SYMB s___msgOnError    = { "__msgOnError",    {HB_FS_MESSAGE}, {NULL},               NULL };

static HB_SYMB s___msgSetData    = { "__msgSetData",    {HB_FS_MESSAGE}, {HB_FUNCNAME( msgSetData )},    NULL };
static HB_SYMB s___msgGetData    = { "__msgGetData",    {HB_FS_MESSAGE}, {HB_FUNCNAME( msgGetData )},    NULL };
static HB_SYMB s___msgSetClsData = { "__msgSetClsData", {HB_FS_MESSAGE}, {HB_FUNCNAME( msgSetClsData )}, NULL };
static HB_SYMB s___msgGetClsData = { "__msgGetClsData", {HB_FS_MESSAGE}, {HB_FUNCNAME( msgGetClsData )}, NULL };
static HB_SYMB s___msgSetShrData = { "__msgSetShrData", {HB_FS_MESSAGE}, {HB_FUNCNAME( msgSetShrData )}, NULL };
static HB_SYMB s___msgGetShrData = { "__msgGetShrData", {HB_FS_MESSAGE}, {HB_FUNCNAME( msgGetShrData )}, NULL };
static HB_SYMB s___msgEvalInline = { "__msgEvalInline", {HB_FS_MESSAGE}, {HB_FUNCNAME( msgEvalInline )}, NULL };
static HB_SYMB s___msgVirtual    = { "__msgVirtual",    {HB_FS_MESSAGE}, {HB_FUNCNAME( msgVirtual )},    NULL };
static HB_SYMB s___msgSuper      = { "__msgSuper",      {HB_FS_MESSAGE}, {HB_FUNCNAME( msgSuper )},      NULL };
static HB_SYMB s___msgRealClass  = { "__msgRealClass",  {HB_FS_MESSAGE}, {HB_FUNCNAME( msgRealClass )},  NULL };
static HB_SYMB s___msgPerform    = { "__msgPerform",    {HB_FS_MESSAGE}, {HB_FUNCNAME( msgPerform )},    NULL };
static HB_SYMB s___msgDelegate   = { "__msgDelegate",   {HB_FS_MESSAGE}, {HB_FUNCNAME( msgDelegate )},   NULL };
static HB_SYMB s___msgSync       = { "__msgSync",       {HB_FS_MESSAGE}, {HB_FUNCNAME( msgSync )},       NULL };
static HB_SYMB s___msgSyncClass  = { "__msgSyncClass",  {HB_FS_MESSAGE}, {HB_FUNCNAME( msgSyncClass )},  NULL };
static HB_SYMB s___msgNoMethod   = { "__msgNoMethod",   {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNoMethod )},   NULL };
static HB_SYMB s___msgScopeErr   = { "__msgScopeErr",   {HB_FS_MESSAGE}, {HB_FUNCNAME( msgScopeErr )},   NULL };
static HB_SYMB s___msgTypeErr    = { "__msgTypeErr",    {HB_FS_MESSAGE}, {HB_FUNCNAME( msgTypeErr )},    NULL };

static HB_SYMB s___msgNew        = { "NEW",             {HB_FS_MESSAGE}, {NULL},               NULL };
static HB_SYMB s___msgSymbol     = { "SYMBOL",          {HB_FS_MESSAGE}, {NULL},               NULL };

static HB_SYMB s___msgClassName  = { "CLASSNAME",       {HB_FS_MESSAGE}, {HB_FUNCNAME( msgClassName )},  NULL };
static HB_SYMB s___msgClassH     = { "CLASSH",          {HB_FS_MESSAGE}, {HB_FUNCNAME( msgClassH )},     NULL };
static HB_SYMB s___msgClassSel   = { "CLASSSEL",        {HB_FS_MESSAGE}, {HB_FUNCNAME( msgClassSel )},   NULL };
static HB_SYMB s___msgExec       = { "EXEC",            {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };
static HB_SYMB s___msgName       = { "NAME",            {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };
/*
static HB_SYMB s___msgClsParent  = { "ISDERIVEDFROM",   {HB_FS_MESSAGE}, {HB_FUNCNAME( msgClassParent )},NULL };
static HB_SYMB s___msgClass      = { "CLASS",           {HB_FS_MESSAGE}, {HB_FUNCNAME( msgClass )},      NULL };
*/
static HB_SYMB s___msgKeys       = { "KEYS",            {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };
static HB_SYMB s___msgValues     = { "VALUES",          {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };

/* Default enumerator methods (FOR EACH) */
static HB_SYMB s___msgEnumIndex  = { "__ENUMINDEX",     {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };
static HB_SYMB s___msgEnumBase   = { "__ENUMBASE",      {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };
static HB_SYMB s___msgEnumKey    = { "__ENUMKEY",       {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };
static HB_SYMB s___msgEnumValue  = { "__ENUMVALUE",     {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };

/* WITH OBJECT base value access/asign methods (:__withobject) */
static HB_SYMB s___msgWithObjectPush = { "__WITHOBJECT",  {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };
static HB_SYMB s___msgWithObjectPop  = { "___WITHOBJECT", {HB_FS_MESSAGE}, {HB_FUNCNAME( msgNull )},       NULL };

/* ================================================ */

/*
 * Scalar classes' handles
 */
/* If user wants to change scalar classes at runtime in MT mode then
 * he must resolve thread synchronization problem himself, [druzus]
 */
static HB_USHORT s_uiArrayClass     = 0;
static HB_USHORT s_uiBlockClass     = 0;
static HB_USHORT s_uiCharacterClass = 0;
static HB_USHORT s_uiDateClass      = 0;
static HB_USHORT s_uiTimeStampClass = 0;
static HB_USHORT s_uiHashClass      = 0;
static HB_USHORT s_uiLogicalClass   = 0;
static HB_USHORT s_uiNilClass       = 0;
static HB_USHORT s_uiNumericClass   = 0;
static HB_USHORT s_uiSymbolClass    = 0;
static HB_USHORT s_uiPointerClass   = 0;

static HB_USHORT s_uiObjectClass    = 0;

/* ================================================ */

/*
 * Class definition holder
 */
/* In MT mode we are allocating array big enough to hold all
 * class definitions so we do not have to worry about runtime
 * s_pClasses reallocation, [druzus]
 */
#if defined( HB_MT_VM )

#  include "hbthread.h"

#  define HB_CLASS_POOL_SIZE  16382
#  define HB_CLASS_LOCK()     hb_threadEnterCriticalSection( &s_clsMtx )
#  define HB_CLASS_UNLOCK()   hb_threadLeaveCriticalSection( &s_clsMtx )
   static HB_CRITICAL_NEW( s_clsMtx );

#else

#  define HB_CLASS_POOL_SIZE  0
#  define HB_CLASS_LOCK()     do {} while( 0 )
#  define HB_CLASS_UNLOCK()   do {} while( 0 )

#endif

#define HB_CLASS_POOL_RESIZE  64

static PCLASS *  s_pClasses  = NULL;
static HB_USHORT s_uiClsSize = 0;
static HB_USHORT s_uiClasses = 0;

static PHB_ITEM s_pClassMtx = NULL;

/* ================================================ */

#if 0
static HB_USHORT hb_clsBucketPos( PHB_DYNS pMsg, HB_USHORT uiMask )
{
   /*
    * we can use PHB_DYNS address as base for hash key.
    * This value is perfectly unique and we do not need anything more
    * but it's not continuous so we will have to add dynamic BUCKETSIZE
    * modification to be 100% sure that we can resolve all symbol name
    * conflicts (though even without it it's rather theoretical problem).
    * [druzus]
    */

   /* Safely divide it by 16 - it's minimum memory allocated for single
    * HB_DYNS structure
    */
   /*
      return ( ( HB_USHORT ) ( ( HB_PTRDIFF ) pMsg >> 4 ) & uiMask ) << BUCKETBITS;
    */

   /* Using continuous symbol numbers we are 100% sure that we will cover
    * the whole 16bit area and we will never have any problems until number
    * of symbols is limited to 2^16. [druzus]
    */
   return ( pMsg->uiSymNum & uiMask ) << BUCKETBITS;
}
#endif

/*
 * hb_clsDictRealloc( PCLASS )
 *
 * Realloc (widen) class
 */
static HB_BOOL hb_clsDictRealloc( PCLASS pClass )
{
   HB_SIZE nNewHashKey, nLimit, n;

#ifdef HB_MSG_POOL
   HB_USHORT * puiMsgIdx;
#else
   PMETHOD pNewMethods;
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsDictRealloc(%p)", pClass ) );

   nNewHashKey = ( HB_SIZE ) pClass->uiHashKey + 1;
   nLimit = nNewHashKey << BUCKETBITS;

   do
   {
      nNewHashKey <<= 1;
      if( nNewHashKey > HASH_KEYMAX )
         hb_errInternal( 6002, "Unable to realloc class message in __clsDictRealloc()", NULL, NULL );

#ifdef HB_MSG_POOL
      puiMsgIdx = ( HB_USHORT * ) hb_xgrab( ( nNewHashKey << BUCKETBITS ) * sizeof( HB_USHORT ) );
      memset( puiMsgIdx, 0, ( nNewHashKey << BUCKETBITS ) * sizeof( HB_USHORT ) );

      for( n = 0; n < nLimit; n++ )
      {
         HB_USHORT uiMsg = pClass->puiMsgIdx[ n ];
         if( pClass->puiMsgIdx[ n ] )
         {
            HB_USHORT uiBucket = BUCKETSIZE;
            HB_USHORT * puiIdx = puiMsgIdx + hb_clsBucketPos(
                        pClass->pMethods[ uiMsg ].pMessage, nNewHashKey - 1 );
            do
            {
               if( *puiIdx == 0 )  /* this message position is empty */
               {
                  *puiIdx = uiMsg;
                  break;
               }
               ++puiIdx;
            }
            while( --uiBucket );

            /* Not enough go back to the beginning */
            if( ! uiBucket )
            {
               hb_xfree( puiMsgIdx );
               break;
            }
         }
      }
   }
   while( n < nLimit );

   pClass->uiHashKey = ( HB_USHORT ) ( nNewHashKey - 1 );
   hb_xfree( pClass->puiMsgIdx );
   pClass->puiMsgIdx = puiMsgIdx;

#else

      pNewMethods = ( PMETHOD ) hb_xgrab( ( nNewHashKey << BUCKETBITS ) * sizeof( METHOD ) );
      memset( pNewMethods, 0, ( nNewHashKey << BUCKETBITS ) * sizeof( METHOD ) );

      for( n = 0; n < nLimit; n++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ n ].pMessage;

         if( pMessage )
         {
            PMETHOD pMethod = pNewMethods + hb_clsBucketPos( pMessage, nNewHashKey - 1 );
            HB_USHORT uiBucket = BUCKETSIZE;

            do
            {
               if( ! pMethod->pMessage ) /* this message position is empty */
               {
                  memcpy( pMethod, pClass->pMethods + n, sizeof( METHOD ) );
                  break;
               }
               ++pMethod;
            }
            while( --uiBucket );

            /* Not enough go back to the beginning */
            if( ! uiBucket )
            {
               hb_xfree( pNewMethods );
               break;
            }
         }
      }
   }
   while( n < nLimit );

   pClass->uiHashKey = ( HB_USHORT ) ( nNewHashKey - 1 );
   hb_xfree( pClass->pMethods );
   pClass->pMethods = pNewMethods;
#endif

   return HB_TRUE;
}

static void hb_clsDictInit( PCLASS pClass, HB_USHORT uiHashKey )
{
   HB_SIZE nSize;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsDictInit(%p,%hu)", pClass, uiHashKey ) );

   pClass->uiHashKey = uiHashKey;
#ifdef HB_MSG_POOL
   nSize = ( ( ( HB_SIZE ) uiHashKey + 1 ) << BUCKETBITS ) * sizeof( HB_USHORT );
   pClass->puiMsgIdx = ( HB_USHORT * ) hb_xgrab( nSize );
   memset( pClass->puiMsgIdx, 0, nSize );

   pClass->uiMethodCount = 1;
   pClass->pMethods = ( PMETHOD ) hb_xgrab( sizeof( METHOD ) );
   memset( pClass->pMethods, 0, sizeof( METHOD ) );
#else
   nSize = ( ( ( HB_SIZE ) uiHashKey + 1 ) << BUCKETBITS ) * sizeof( METHOD );
   pClass->pMethods = ( PMETHOD ) hb_xgrab( nSize );
   memset( pClass->pMethods, 0, nSize );
#endif
}

static PMETHOD hb_clsFindMsg( PCLASS pClass, PHB_DYNS pMsg )
{
#ifdef HB_MSG_POOL

   HB_USHORT uiBucket, * puiMsgIdx;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsFindMsg(%p,%p)", pClass, pMsg ) );

   puiMsgIdx = pClass->puiMsgIdx + hb_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      PMETHOD pMethod = &pClass->pMethods[ *puiMsgIdx ];

      if( pMethod->pMessage == pMsg )
         return pMethod;
      ++puiMsgIdx;
   }
   while( --uiBucket );

#else

   PMETHOD pMethod;
   HB_USHORT uiBucket;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsFindMsg(%p,%p)", pClass, pMsg ) );

   pMethod = pClass->pMethods + hb_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      if( pMethod->pMessage == pMsg )
         return pMethod;
      ++pMethod;
   }
   while( --uiBucket );

#endif

   return NULL;
}

static PMETHOD hb_clsAllocMsg( PCLASS pClass, PHB_DYNS pMsg )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsAllocMsg(%p,%p)", pClass, pMsg ) );

   do
   {

#ifdef HB_MSG_POOL

      HB_USHORT uiBucket = BUCKETSIZE, * puiMsgIdx = pClass->puiMsgIdx +
                                    hb_clsBucketPos( pMsg, pClass->uiHashKey );

      do
      {
         if( *puiMsgIdx == 0 )
         {
            pClass->pMethods = ( PMETHOD ) hb_xrealloc( pClass->pMethods,
                           sizeof( METHOD ) * ( pClass->uiMethodCount + 1 ) );
            memset( &pClass->pMethods[ pClass->uiMethodCount ], 0, sizeof( METHOD ) );
            *puiMsgIdx = pClass->uiMethodCount++;
            return &pClass->pMethods[ *puiMsgIdx ];
         }
         else if( pClass->pMethods[ *puiMsgIdx ].pMessage == pMsg )
            return &pClass->pMethods[ *puiMsgIdx ];
         ++puiMsgIdx;
      }
      while( --uiBucket );

#else

      PMETHOD pMethod = pClass->pMethods + hb_clsBucketPos( pMsg, pClass->uiHashKey );
      HB_USHORT uiBucket = BUCKETSIZE;

      do
      {
         if( ! pMethod->pMessage || pMethod->pMessage == pMsg )
            return pMethod;
         ++pMethod;
      }
      while( --uiBucket );

#endif

   }
   while( hb_clsDictRealloc( pClass ) );

   hb_errInternal( 6001, "Unable to allocate new message", NULL, NULL );

   return NULL;
}

static HB_BOOL hb_clsCanClearMethod( PMETHOD pMethod, HB_BOOL fError )
{
   HB_SYMBOL_UNUSED( pMethod );
   HB_SYMBOL_UNUSED( fError );
#if 0
   if( pMethod->pFuncSym == &s___msgSuper )
   {
      if( fError )
         hb_errRT_BASE( EG_ARG, 3000, "Cannot delete supercast messages", HB_ERR_FUNCNAME, 0 );
      return HB_FALSE;
   }
#endif
   return HB_TRUE;
}

static void hb_clsFreeMsg( PCLASS pClass, PHB_DYNS pMsg )
{
#ifdef HB_MSG_POOL

   HB_USHORT uiBucket, * puiMsgIdx;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsFreeMsg(%p,%p)", pClass, pMsg ) );


   puiMsgIdx = pClass->puiMsgIdx + hb_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      if( *puiMsgIdx && pClass->pMethods[ *puiMsgIdx ].pMessage == pMsg )
      {
         if( hb_clsCanClearMethod( &pClass->pMethods[ *puiMsgIdx ], HB_TRUE ) )
         {
            memset( &pClass->pMethods[ *puiMsgIdx ], 0, sizeof( METHOD ) );
            *puiMsgIdx = 0;
            pClass->uiMethods--;       /* Decrease number of messages */
         }
         return;
      }
      ++puiMsgIdx;
   }
   while( --uiBucket );

#else

   PMETHOD pMethod;
   HB_USHORT uiBucket;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsFreeMsg(%p,%p)", pClass, pMsg ) );

   pMethod = pClass->pMethods + hb_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      if( pMethod->pMessage == pMsg )
      {
         if( hb_clsCanClearMethod( pMethod, HB_TRUE ) )
         {
            /* Move messages */
            while( --uiBucket )
            {
               memcpy( pMethod, pMethod + 1, sizeof( METHOD ) );
               pMethod++;
            }
            memset( pMethod, 0, sizeof( METHOD ) );
            pClass->uiMethods--;       /* Decrease number of messages */
         }
         return;
      }
      ++pMethod;
   }
   while( --uiBucket );

#endif
}

static HB_BOOL hb_clsHasParentClass( PCLASS pClass, HB_USHORT uiParentCls )
{
   HB_USHORT uiCount = pClass->uiSuperClasses;

   while( uiCount )
   {
      if( pClass->pSuperClasses[ --uiCount ].uiClass == uiParentCls )
         return HB_TRUE;
   }
   return HB_FALSE;

   /* alternative method but can give wrong results
    * if user overloads super casting method, [druzus].
    */
   /*
   PMETHOD pMethod = hb_clsFindMsg( pClass, s_pClasses[ uiParentCls ]->pClassSym );
   return pMethod && pMethod->pFuncSym == &s___msgSuper;
   */
}

static HB_USHORT hb_clsGetParent( PCLASS pClass, PHB_DYNS pParentSym )
{
   HB_USHORT uiCount = pClass->uiSuperClasses;

   while( uiCount )
   {
      HB_USHORT uiClass = pClass->pSuperClasses[ --uiCount ].uiClass;
      if( s_pClasses[ uiClass ]->pClassSym == pParentSym )
         return uiClass;
   }
   return 0;

   /* alternative method but can give wrong results
    * if user overloads super casting method, [druzus].
    */
   /*
   PMETHOD pMethod = hb_clsFindMsg( pClass, pParentSym );
   return pMethod && pMethod->pFuncSym == &s___msgSuper;
   */
}

static HB_USHORT hb_clsParentInstanceOffset( PCLASS pClass, HB_USHORT uiParentCls )
{
   HB_USHORT uiCount = pClass->uiSuperClasses;

   while( uiCount )
   {
      if( pClass->pSuperClasses[ --uiCount ].uiClass == uiParentCls )
         return pClass->pSuperClasses[ uiCount ].uiOffset;
   }
   return 0;
}

/*
static HB_USHORT hb_clsParentInstanceOffset( PCLASS pClass, HB_USHORT uiParentCls )
{
   PMETHOD pMethod = hb_clsFindMsg( pClass, s_pClasses[ uiParentCls ]->pClassSym );

   return ( pMethod && pMethod->pFuncSym == &s___msgSuper ) ? pMethod->uiOffset : 0;
}
*/

static HB_USHORT hb_clsAddInitValue( PCLASS pClass, PHB_ITEM pItem,
                                     HB_USHORT uiType, HB_USHORT uiData,
                                     HB_USHORT uiOffset, HB_USHORT uiSprClass )
{
   PINITDATA pInitData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsAddInitValue(%p,%p,%hu,%hu,%hu,%hu)", pClass, pItem, uiType, uiData, uiOffset, uiSprClass ) );

   if( ! pItem || HB_IS_NIL( pItem ) )
      return 0;

   if( ! pClass->uiInitDatas )
   {
      pClass->pInitData = ( PINITDATA ) hb_xgrab( sizeof( INITDATA ) );
      pInitData = pClass->pInitData + pClass->uiInitDatas++;
   }
   else
   {
      HB_USHORT ui = pClass->uiInitDatas;
      pInitData = pClass->pInitData;
      do
      {
         if( pInitData->uiType == uiType &&
             pInitData->uiData + pInitData->uiOffset == uiData + uiOffset )
         {
            hb_itemRelease( pInitData->pInitValue );
            break;
         }
         ++pInitData;
      }
      while( --ui );

      if( ui == 0 )
      {
         pClass->pInitData = ( PINITDATA ) hb_xrealloc( pClass->pInitData,
                  ( HB_SIZE ) ( pClass->uiInitDatas + 1 ) * sizeof( INITDATA ) );
         pInitData = pClass->pInitData + pClass->uiInitDatas++;
      }
   }

   pInitData->pInitValue = hb_itemClone( pItem );
   pInitData->uiType = uiType;
   pInitData->uiData = uiData;
   pInitData->uiOffset = uiOffset;
   pInitData->uiSprClass = uiSprClass;

   return pClass->uiInitDatas;
}

static HB_USHORT hb_clsFindRealClassDataOffset( PMETHOD pMethod )
{
   PMETHOD pRealMth;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsFindRealClassDataOffset(%p)", pMethod ) );

   pRealMth = hb_clsFindMsg( s_pClasses[ pMethod->uiSprClass ],
                             pMethod->pMessage );
   if( pRealMth && pRealMth->uiSprClass == pMethod->uiSprClass &&
       ( pRealMth->pFuncSym == &s___msgSetClsData ||
         pRealMth->pFuncSym == &s___msgGetClsData ) )
   {
      return pRealMth->uiData;
   }
   return 0;
}

static HB_USHORT hb_clsFindClassDataOffset( PCLASS pClass, PMETHOD pNewMethod )
{
   HB_USHORT uiData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsFindClassDataOffset(%p,%p)", pClass, pNewMethod ) );

   uiData = hb_clsFindRealClassDataOffset( pNewMethod );
   if( uiData )
   {
      HB_SIZE nLimit = hb_clsMthNum( pClass );
      PMETHOD pMethod = pClass->pMethods;
      do
      {
         if( pMethod->pMessage && pMethod != pNewMethod &&
             pMethod->uiSprClass == pNewMethod->uiSprClass &&
             ( pMethod->pFuncSym == &s___msgSetClsData ||
               pMethod->pFuncSym == &s___msgGetClsData ) &&
             uiData == hb_clsFindRealClassDataOffset( pMethod ) )
         {
            return pMethod->uiData;
         }
         ++pMethod;
      }
      while( --nLimit );
   }

   return 0;
}

static HB_BOOL hb_clsUpdateHiddenMessages( PMETHOD pSrcMethod, PMETHOD pDstMethod,
                                           PCLASS pDstClass )
{
   PMETHOD pNewMethod = pSrcMethod;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsUpdateHiddenMessages(%p,%p,%p)", pSrcMethod, pDstMethod, pDstClass ) );

   if( ! pDstMethod->pMessage ||
       ( hb_clsCanClearMethod( pDstMethod, HB_FALSE ) &&
         pDstMethod->uiPrevCls != pDstMethod->uiSprClass &&
         ( pDstMethod->uiScope & HB_OO_CLSTP_HIDDEN ) &&
         ( pDstMethod->uiScope & HB_OO_CLSTP_NONVIRTUAL ) ) )
   {
      while( pNewMethod &&
             pNewMethod->uiPrevCls != pNewMethod->uiSprClass &&
             ( pNewMethod->uiScope & HB_OO_CLSTP_HIDDEN ) &&
             ( pNewMethod->uiScope & HB_OO_CLSTP_NONVIRTUAL ) )
      {
         pNewMethod = hb_clsFindMsg( s_pClasses[ pNewMethod->uiPrevCls ],
                                     pNewMethod->pMessage );
      }
      if( pNewMethod && pNewMethod != pSrcMethod &&
          ! ( pNewMethod->uiScope & HB_OO_CLSTP_HIDDEN ) &&
          hb_clsCanClearMethod( pDstMethod, HB_FALSE ) )
      {
         HB_USHORT uiPrevCls = pDstMethod->uiPrevCls,
                   uiPrevMth = pDstMethod->uiPrevMth;
         PHB_SYMB pFuncSym;

         memcpy( pDstMethod, pNewMethod, sizeof( METHOD ) );
         pDstMethod->uiPrevCls = uiPrevCls;
         pDstMethod->uiPrevMth = uiPrevMth;
         pDstMethod->uiScope |= HB_OO_CLSTP_OVERLOADED | HB_OO_CLSTP_SUPER;
         pFuncSym = pDstMethod->pFuncSym;
         if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
            pFuncSym = pDstMethod->pRealSym;
         if( pFuncSym == &s___msgSetData ||
             pFuncSym == &s___msgGetData )
         {
            pDstMethod->uiOffset = hb_clsParentInstanceOffset( pDstClass,
                                                   pDstMethod->uiSprClass );
         }
         else if( pFuncSym == &s___msgSetClsData ||
                  pFuncSym == &s___msgGetClsData )
         {
            PCLASS pSrcClass = s_pClasses[ pDstMethod->uiSprClass ];
            HB_USHORT uiData;

            /* check if we already have corresponding access or assign
             * message for this class var to reuse its index
             */
            uiData = hb_clsFindClassDataOffset( pDstClass, pDstMethod );

            if( uiData == 0 )
            {
               uiData = ( HB_USHORT ) hb_arrayLen( pDstClass->pClassDatas ) + 1;
               hb_arraySize( pDstClass->pClassDatas, uiData );
            }
            if( pDstMethod->uiOffset )
            {
               pDstMethod->uiOffset = hb_clsAddInitValue( pDstClass,
                  pSrcClass->pInitData[ pDstMethod->uiOffset - 1 ].pInitValue,
                  HB_OO_MSG_CLASSDATA, uiData, 0, pDstMethod->uiSprClass );
            }
            pDstMethod->uiData = uiData;
         }
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static void hb_clsAddSuperClass( PCLASS pClass, HB_USHORT uiSuperCls, HB_USHORT uiOffset )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsAddSuperClass(%p,%hu,%hu)", pClass, uiSuperCls, uiOffset ) );

   pClass->pSuperClasses = ( PHB_CLSCAST ) hb_xrealloc( pClass->pSuperClasses,
                     ( pClass->uiSuperClasses + 1 ) * sizeof( HB_CLSCAST ) );
   pClass->pSuperClasses[ pClass->uiSuperClasses ].uiClass = uiSuperCls;
   pClass->pSuperClasses[ pClass->uiSuperClasses++ ].uiOffset = uiOffset;
}

static void hb_clsDefineSuperClass( PCLASS pClass, HB_USHORT uiSuperCls, HB_BOOL fNew )
{
   PMETHOD pMethod;
   PCLASS pSprCls;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsDefineSuperClass(%p,%hu,%d)", pClass, uiSuperCls, fNew ) );

   pSprCls = s_pClasses[ uiSuperCls ];

   if( ! hb_clsHasParentClass( pClass, uiSuperCls ) )
   {
      if( fNew )
      {
         hb_clsAddSuperClass( pClass, uiSuperCls, pClass->uiDatas );
         pClass->uiDatas += pSprCls->uiDatas - pSprCls->uiDataFirst;
      }
      else
         hb_clsAddSuperClass( pClass, uiSuperCls, pSprCls->uiDataFirst );
   }

   pMethod = hb_clsAllocMsg( pClass, pSprCls->pClassSym );
   if( pMethod->pMessage == NULL )
   {
      pClass->uiMethods++;
      pMethod->pMessage = pSprCls->pClassSym;
      pMethod->uiSprClass = pClass->uiClass;
      pMethod->uiData = uiSuperCls;
      pMethod->uiScope = HB_OO_CLSTP_EXPORTED;
      pMethod->pFuncSym = &s___msgSuper;
      pMethod->uiOffset = hb_clsParentInstanceOffset( pClass, uiSuperCls );
   }
   else
   {
      PHB_SYMB pFuncSym = pMethod->pFuncSym;

      if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
         pFuncSym = pMethod->pRealSym;
      if( pFuncSym == &s___msgSuper && pMethod->uiData == uiSuperCls )
         pMethod->uiOffset = hb_clsParentInstanceOffset( pClass, uiSuperCls );
   }
}

static void hb_clsCopyClass( PCLASS pClsDst, PCLASS pClsSrc )
{
   PMETHOD pMethod;
   HB_SIZE nLimit;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsCopyClass(%p,%p)", pClsDst, pClsSrc ) );

   hb_clsDictInit( pClsDst, pClsSrc->uiHashKey );
   pClsDst->fHasOnError = pClsSrc->fHasOnError;
   pClsDst->fHasDestructor = pClsSrc->fHasDestructor;

   /* CLASS DATA Not Shared ( new array, new value ) */
#if 0
   /* enable this code if you want to inherit class variables values
    * from ancestor classes when new class is dynamically created.
    * (compatibility with older [x]Harbour versions)
    */
   pClsDst->pClassDatas = hb_arrayClone( pClsSrc->pClassDatas );
#else
   pClsDst->pClassDatas = hb_itemArrayNew( hb_arrayLen( pClsSrc->pClassDatas ) );
#endif
   /* do not copy shared data array - just simply create new one */
   pClsDst->pSharedDatas = hb_itemArrayNew( 0 );
   pClsDst->pInlines = hb_arrayClone( pClsSrc->pInlines );
   pClsDst->uiDatas = pClsSrc->uiDatas;
   pClsDst->uiMutexOffset = pClsSrc->uiMutexOffset;
   pClsDst->nOpFlags = pClsSrc->nOpFlags;
   if( pClsSrc->pMutex )
      pClsDst->pMutex = hb_threadMutexCreate();

   if( pClsSrc->uiInitDatas )
   {
      HB_SIZE nSize = ( HB_SIZE ) pClsSrc->uiInitDatas * sizeof( INITDATA );
      HB_USHORT uiData;

      pClsDst->uiInitDatas = pClsSrc->uiInitDatas;
      pClsDst->pInitData = ( PINITDATA ) hb_xgrab( nSize );
      memcpy( pClsDst->pInitData, pClsSrc->pInitData, nSize );
      for( uiData = 0; uiData < pClsDst->uiInitDatas; ++uiData )
      {
         if( pClsDst->pInitData[ uiData ].uiType == HB_OO_MSG_INITIALIZED )
            pClsDst->pInitData[ uiData ].uiType = HB_OO_MSG_CLASSDATA;
         pClsDst->pInitData[ uiData ].pInitValue =
                        hb_itemNew( pClsDst->pInitData[ uiData ].pInitValue );
      }
   }

   nLimit = hb_clsMthNum( pClsSrc );
#ifdef HB_MSG_POOL
   memcpy( pClsDst->puiMsgIdx, pClsSrc->puiMsgIdx,
      ( ( ( HB_SIZE ) pClsSrc->uiHashKey + 1 ) << BUCKETBITS ) * sizeof( HB_USHORT ) );
   pClsDst->uiMethodCount = pClsSrc->uiMethodCount;
   pClsDst->pMethods = ( PMETHOD ) hb_xrealloc( pClsDst->pMethods,
                                                nLimit * sizeof( METHOD ) );
#endif
   memcpy( pClsDst->pMethods, pClsSrc->pMethods, nLimit * sizeof( METHOD ) );
   pClsDst->uiMethods = pClsSrc->uiMethods;

   if( pClsSrc->uiSuperClasses )
   {
      pClsDst->uiSuperClasses = pClsSrc->uiSuperClasses;
      pClsDst->pSuperClasses = ( PHB_CLSCAST )
                     hb_xgrab( pClsSrc->uiSuperClasses * sizeof( HB_CLSCAST ) );
      memcpy( pClsDst->pSuperClasses, pClsSrc->pSuperClasses,
              pClsSrc->uiSuperClasses * sizeof( HB_CLSCAST ) );
   }
   hb_clsDefineSuperClass( pClsDst, pClsSrc->uiClass, HB_FALSE );

   pMethod = pClsDst->pMethods;
   do
   {
      if( pMethod->pMessage )
      {
         hb_clsUpdateHiddenMessages( pMethod, pMethod, pClsDst );
         pMethod->uiScope |= HB_OO_CLSTP_SUPER;
      }
      ++pMethod;
   }
   while( --nLimit );
}

static HB_BOOL hb_clsIsFriendSymbol( PCLASS pClass, PHB_SYMB pSym )
{
   HB_USHORT uiCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsIsFriendSymbol(%p,%p)", pClass, pSym ) );

   if( pSym >= pClass->pFriendModule &&
       pSym < pClass->pFriendModule + pClass->uiFriendModule )
      return HB_TRUE;

   for( uiCount = 0; uiCount < pClass->uiFriendSyms; ++uiCount )
   {
      if( pClass->pFriendSyms[ uiCount ] == pSym )
         return HB_TRUE;
   }

   return HB_FALSE;
}

static void hb_clsAddFriendSymbol( PCLASS pClass, PHB_SYMB pSym )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsAddFriendSymbol(%p,%p)", pClass, pSym ) );

   if( ! hb_clsIsFriendSymbol( pClass, pSym ) )
   {
      if( pClass->uiFriendSyms == 0 )
      {
         pClass->pFriendSyms = ( PHB_SYMB * ) hb_xgrab( sizeof( PHB_SYMB ) );
         pClass->pFriendSyms[ 0 ] = pSym;
         pClass->uiFriendSyms++;
      }
      else
      {
         pClass->pFriendSyms = ( PHB_SYMB * ) hb_xrealloc( pClass->pFriendSyms,
                           ( pClass->uiFriendSyms + 1 ) * sizeof( PHB_SYMB ) );
         pClass->pFriendSyms[ pClass->uiFriendSyms++ ] = pSym;
      }
   }
}

/*
 * initialize Classy/OO system at HVM startup
 */
void hb_clsInit( void )
{
   PHB_SYMB pOpSym;
   HB_USHORT uiOperator;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsInit()" ) );

   for( uiOperator = 0, pOpSym = s_opSymbols; uiOperator <= HB_OO_MAX_OPERATOR;
        ++uiOperator, ++pOpSym )
   {
      pOpSym->pDynSym = hb_dynsymGetCase( pOpSym->szName );
   }

   s___msgDestructor.pDynSym  = hb_dynsymGetCase( s___msgDestructor.szName );
   s___msgOnError.pDynSym     = hb_dynsymGetCase( s___msgOnError.szName );

   s___msgClassName.pDynSym   = hb_dynsymGetCase( s___msgClassName.szName );  /* Standard messages */
   s___msgClassH.pDynSym      = hb_dynsymGetCase( s___msgClassH.szName );     /* Not present in classdef. */
   s___msgClassSel.pDynSym    = hb_dynsymGetCase( s___msgClassSel.szName );
   s___msgExec.pDynSym        = hb_dynsymGetCase( s___msgExec.szName );
   s___msgName.pDynSym        = hb_dynsymGetCase( s___msgName.szName );
   s___msgNew.pDynSym         = hb_dynsymGetCase( s___msgNew.szName );
   s___msgSymbol.pDynSym      = hb_dynsymGetCase( s___msgSymbol.szName );
   s___msgKeys.pDynSym        = hb_dynsymGetCase( s___msgKeys.szName );
   s___msgValues.pDynSym      = hb_dynsymGetCase( s___msgValues.szName );
/*
   s___msgClsParent.pDynSym   = hb_dynsymGetCase( s___msgClsParent.szName );
   s___msgClass.pDynSym       = hb_dynsymGetCase( s___msgClass.szName );
*/
   s___msgEnumIndex.pDynSym   = hb_dynsymGetCase( s___msgEnumIndex.szName );
   s___msgEnumBase.pDynSym    = hb_dynsymGetCase( s___msgEnumBase.szName );
   s___msgEnumKey.pDynSym     = hb_dynsymGetCase( s___msgEnumKey.szName );
   s___msgEnumValue.pDynSym   = hb_dynsymGetCase( s___msgEnumValue.szName );

   s___msgWithObjectPush.pDynSym = hb_dynsymGetCase( s___msgWithObjectPush.szName );
   s___msgWithObjectPop.pDynSym  = hb_dynsymGetCase( s___msgWithObjectPop.szName );


   s_uiClsSize = HB_CLASS_POOL_SIZE;
   s_uiClasses = 0;
   s_pClasses = ( PCLASS * ) hb_xgrab( ( ( HB_SIZE ) s_uiClsSize + 1 ) * sizeof( PCLASS ) );
   s_pClasses[ 0 ] = NULL;

#if defined( HB_MT_VM )
   s_pClassMtx = hb_threadMutexCreate();
#endif
}

/*
 * initialize Classy/OO system .prg functions
 */
void hb_clsDoInit( void )
{
   static const char * s_pszFuncNames[] =
      { "HBARRAY", "HBBLOCK", "HBCHARACTER",
        "HBDATE", "HBTIMESTAMP",
        "HBHASH", "HBLOGICAL", "HBNIL", "HBNUMERIC",
        "HBSYMBOL", "HBPOINTER",
        "HBOBJECT" };
   static HB_USHORT * s_puiHandles[] =
      { &s_uiArrayClass, &s_uiBlockClass, &s_uiCharacterClass,
        &s_uiDateClass, &s_uiTimeStampClass,
        &s_uiHashClass, &s_uiLogicalClass, &s_uiNilClass, &s_uiNumericClass,
        &s_uiSymbolClass, &s_uiPointerClass,
        &s_uiObjectClass };

   HB_STACK_TLS_PRELOAD
   int i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsDoInit()" ) );

   for( i = 0; i < ( int ) ( sizeof( s_puiHandles ) / sizeof( HB_USHORT * ) ); ++i )
   {
      PHB_DYNS pFuncSym = hb_dynsymFindName( s_pszFuncNames[i] );
      if( pFuncSym && hb_dynsymIsFunction( pFuncSym ) )
      {
         PHB_ITEM pReturn = hb_stackReturnItem();
         hb_itemSetNil( pReturn );
         hb_vmPushDynSym( pFuncSym );
         hb_vmPushNil();
         hb_vmProc( 0 );
         if( HB_IS_OBJECT( pReturn ) )
            *( s_puiHandles[ i ] ) = pReturn->item.asArray.value->uiClass;
      }
   }
}

/*
 * hb_clsRelease( <pClass> )
 *
 * Release a class from memory
 */
static void hb_clsRelease( PCLASS pClass )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsRelease(%p)", pClass ) );

   if( pClass->uiInitDatas )
   {
      HB_USHORT ui = pClass->uiInitDatas;
      PINITDATA pInitData = pClass->pInitData;

      do
      {
         hb_itemRelease( pInitData->pInitValue );
         ++pInitData;
      }
      while( --ui );
      hb_xfree( pClass->pInitData );
   }

   if( pClass->szName )
      hb_xfree( pClass->szName );
   if( pClass->pMethods )
      hb_xfree( pClass->pMethods );
   if( pClass->uiFriendSyms )
      hb_xfree( pClass->pFriendSyms );
   if( pClass->pSuperClasses )
      hb_xfree( pClass->pSuperClasses );
#ifdef HB_MSG_POOL
   if( pClass->puiMsgIdx )
      hb_xfree( pClass->puiMsgIdx );
#endif
   if( pClass->pClassDatas )
      hb_itemRelease( pClass->pClassDatas );
   if( pClass->pSharedDatas )
      hb_itemRelease( pClass->pSharedDatas );
   if( pClass->pInlines )
      hb_itemRelease( pClass->pInlines );

   hb_xfree( pClass );
}


/*
 * hb_clsReleaseAll()
 *
 * Release all classes
 */
void hb_clsReleaseAll( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsReleaseAll()" ) );

   if( s_uiClasses )
   {
      HB_USHORT uiClass = s_uiClasses;

      /* It blocks destructor execution - don't move. [druzus] */
      s_uiClasses = 0;

      do
      {
         hb_clsRelease( s_pClasses[ uiClass ] );
      }
      while( --uiClass );

   }

   if( s_pClasses )
   {
      hb_xfree( s_pClasses );
      s_pClasses = NULL;
      s_uiClsSize = 0;
   }

   if( s_pClassMtx )
   {
      hb_itemRelease( s_pClassMtx );
      s_pClassMtx = NULL;
   }
}

/* Mark all internal data as used so it will not be released by the
 * garbage collector
 */

void hb_clsIsClassRef( void )
{
   /*
    * All internal items are allocated with hb_itemNew()
    * GC knows them and scan itself so it's not necessary
    * to repeat scanning here [druzus].
    */
#if 0
   HB_USHORT uiClass = s_uiClasses;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsIsClassRef()" ) );

   while( uiClass )
   {
      PCLASS pClass = s_pClasses[ uiClass-- ];

      if( pClass->pInlines )
         hb_gcItemRef( pClass->pInlines );

      if( pClass->pClassDatas )
         hb_gcItemRef( pClass->pClassDatas );

      if( pClass->pSharedDatas )
         hb_gcItemRef( pClass->pSharedDatas );

      if( pClass->uiInitDatas )
      {
         HB_USHORT ui = pClass->uiInitDatas;
         PINITDATA pInitData = pClass->pInitData;

         do
         {
            if( HB_IS_GCITEM( pInitData->pInitValue ) )
               hb_gcItemRef( pInitData->pInitValue );
            ++pInitData;
         }
         while( --ui );
      }
   }
#endif
}

HB_BOOL hb_clsIsParent( HB_USHORT uiClass, const char * szParentName )
{
   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];

      if( strcmp( pClass->szName, szParentName ) == 0 )
         return HB_TRUE;
      else
      {
         PHB_DYNS pMsg = hb_dynsymFindName( szParentName );

         if( pMsg )
            return hb_clsGetParent( pClass, pMsg ) != 0;
      }
   }

   return HB_FALSE;
}

HB_USHORT hb_objGetClass( PHB_ITEM pItem )
{
   if( pItem && HB_IS_ARRAY( pItem ) )
      return pItem->item.asArray.value->uiClass;
   else
      return 0;
}

/* get object class handle using class name and class function name */
HB_USHORT hb_objSetClass( PHB_ITEM pItem, const char * szClass, const char * szFunc )
{
   HB_USHORT uiClass = 0;

   if( pItem && HB_IS_ARRAY( pItem ) &&
       pItem->item.asArray.value->uiClass == 0 )
   {
      uiClass = pItem->item.asArray.value->uiClass =
                                          hb_clsFindClass( szClass, szFunc );
   }
   return uiClass;
}

/* ================================================ */

/*
 * Get the class handle
 */
static HB_USHORT hb_objGetClassH( PHB_ITEM pObject )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetClassH(%p)", pObject ) );

   if( HB_IS_ARRAY( pObject ) )
   {
      if( pObject->item.asArray.value->uiClass != 0 )
         return pObject->item.asArray.value->uiClass;
      else
         return s_uiArrayClass;
   }
   /* built in types */
   else if( HB_IS_NIL( pObject ) )
      return s_uiNilClass;

   else if( HB_IS_STRING( pObject ) )
      return s_uiCharacterClass;

   else if( HB_IS_NUMERIC( pObject ) )
      return s_uiNumericClass;

   else if( HB_IS_DATE( pObject ) )
      return s_uiDateClass;

   else if( HB_IS_TIMESTAMP( pObject ) )
      return s_uiTimeStampClass;

   else if( HB_IS_LOGICAL( pObject ) )
      return s_uiLogicalClass;

   else if( HB_IS_BLOCK( pObject ) )
      return s_uiBlockClass;

   else if( HB_IS_HASH( pObject ) )
      return s_uiHashClass;

   else if( HB_IS_POINTER( pObject ) )
      return s_uiPointerClass;

   else if( HB_IS_SYMBOL( pObject ) )
      return s_uiSymbolClass;

   return 0;
}

/*
 * Get the class name of an object
 */
const char * hb_objGetClsName( PHB_ITEM pObject )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetClsName(%p)", pObject ) );

   if( HB_IS_ARRAY( pObject ) )
   {
      if( pObject->item.asArray.value->uiClass != 0 )
         return s_pClasses[ pObject->item.asArray.value->uiClass ]->szName;
      else
         return "ARRAY";
   }
   /* built in types */
   else if( HB_IS_NIL( pObject ) )
      return "NIL";

   else if( HB_IS_STRING( pObject ) )
      return "CHARACTER";

   else if( HB_IS_NUMERIC( pObject ) )
      return "NUMERIC";

   else if( HB_IS_DATE( pObject ) )
      return "DATE";

   else if( HB_IS_TIMESTAMP( pObject ) )
      return "TIMESTAMP";

   else if( HB_IS_LOGICAL( pObject ) )
      return "LOGICAL";

   else if( HB_IS_BLOCK( pObject ) )
      return "BLOCK";

   else if( HB_IS_HASH( pObject ) )
      return "HASH";

   else if( HB_IS_POINTER( pObject ) )
      return "POINTER";

   else if( HB_IS_SYMBOL( pObject ) )
      return "SYMBOL";

   else
      return "UNKNOWN";
}

const char * hb_clsName( HB_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->szName;
   else
      return NULL;
}

const char * hb_clsFuncName( HB_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->pClassFuncSym ?
             s_pClasses[ uiClass ]->pClassFuncSym->szName :
             "";
   else
      return NULL;
}

PHB_SYMB hb_clsFuncSym( HB_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->pClassFuncSym;
   else
      return NULL;
}

const char * hb_clsMethodName( HB_USHORT uiClass, HB_USHORT uiMethod )
{
   if( uiClass && uiClass <= s_uiClasses &&
       ( HB_UINT ) uiMethod < ( HB_UINT ) hb_clsMthNum( s_pClasses[ uiClass ] ) )
   {
      PMETHOD pMethod = s_pClasses[ uiClass ]->pMethods + uiMethod;
      if( pMethod->pMessage )
         return pMethod->pMessage->pSymbol->szName;
   }
   return NULL;
}

static HB_SIZE hb_clsGetVarIndexEx( HB_USHORT uiClass, PHB_DYNS pVarSym,
                                    HB_USHORT uiSuper )
{
   PMETHOD pMethod = hb_clsFindMsg( s_pClasses[ uiSuper ], pVarSym );
   if( pMethod )
   {
      PHB_SYMB pFuncSym = pMethod->pFuncSym;

      if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
         pFuncSym = pMethod->pRealSym;

      if( pFuncSym == &s___msgSetData || pFuncSym == &s___msgGetData )
      {
         return ( HB_SIZE ) pMethod->uiData + ( uiClass != uiSuper ?
                hb_clsParentInstanceOffset( s_pClasses[ uiClass ], uiSuper ) :
                pMethod->uiOffset );
      }
   }
   return 0;
}

HB_SIZE hb_clsGetVarIndex( HB_USHORT uiClass, PHB_DYNS pVarSym )
{
   if( uiClass && uiClass <= s_uiClasses )
      return hb_clsGetVarIndexEx( uiClass, pVarSym, uiClass );
   else
      return 0;
}

HB_USHORT hb_clsFindClass( const char * szClass, const char * szFunc )
{
   HB_USHORT uiClass;

   for( uiClass = 1; uiClass <= s_uiClasses; uiClass++ )
   {
      if( strcmp( szClass, s_pClasses[ uiClass ]->szName ) == 0 &&
          ( ! szFunc || ( ! s_pClasses[ uiClass ]->pClassFuncSym ? ! *szFunc :
            strcmp( szFunc, s_pClasses[ uiClass ]->pClassFuncSym->szName ) == 0 ) ) )
      {
         return uiClass;
      }
   }
   return 0;
}

static HB_USHORT hb_clsFindClassByFunc( PHB_SYMB pClassFuncSym )
{
   HB_USHORT uiClass;

   for( uiClass = 1; uiClass <= s_uiClasses; uiClass++ )
   {
      if( s_pClasses[ uiClass ]->pClassFuncSym == pClassFuncSym )
      {
         return uiClass;
      }
   }
   return 0;
}

/*
 * Get the real method symbol for given stack symbol
 */
PHB_SYMB hb_clsMethodSym( PHB_ITEM pBaseSymbol )
{
   PHB_STACK_STATE pStack = pBaseSymbol->item.asSymbol.stackstate;

   if( pStack->uiClass )
   {
      PMETHOD pMethod = s_pClasses[ pStack->uiClass ]->pMethods + pStack->uiMethod;
      PHB_SYMB pFuncSym = pMethod->pFuncSym;

      if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
         pFuncSym = pMethod->pRealSym;

      if( pFuncSym == &s___msgEvalInline )
         return hb_arrayGetItemPtr( s_pClasses[ pMethod->uiSprClass ]->pInlines,
                                    pMethod->uiData )->item.asBlock.value->pDefSymb;
/*
      else if( pFuncSym == &s___msgPerform )
 */
      else if( pFuncSym == &s___msgDelegate )
         return s_pClasses[ pStack->uiClass ]->pMethods[ pMethod->uiData ].pFuncSym;
      else
         return pFuncSym;
   }

   return pBaseSymbol->item.asSymbol.value;
}

/*
 * Get the real class name of an object message
 * Will return the class name from wich the message is inherited in case
 * of inheritance.
 */
const char * hb_objGetRealClsName( PHB_ITEM pObject, const char * szName )
{
   HB_USHORT uiClass;

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetrealClsName(%p,%s)", pObject, szName ) );

   uiClass = hb_objGetClassH( pObject );
   if( uiClass && uiClass <= s_uiClasses )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( szName );

      if( pMsg )
      {
         PMETHOD pMethod = hb_clsFindMsg( s_pClasses[ uiClass ], pMsg );
         if( pMethod )
            uiClass = pMethod->uiSprClass;
      }
      if( uiClass && uiClass <= s_uiClasses )
         return s_pClasses[ uiClass ]->szName;
   }

   return hb_objGetClsName( pObject );
}

#if defined( HB_CLASSY_BLOCK_SCOPE )
static HB_ISIZ hb_clsSenderOffset( void )
{
   HB_ISIZ nOffset = hb_stackBaseProcOffset( 1 );

   if( nOffset > 0 )
   {
      /* Is it inline method? */
      if( nOffset > 0 && HB_IS_BLOCK( hb_stackItem( nOffset + 1 ) ) &&
          ( hb_stackItem( nOffset )->item.asSymbol.value == &hb_symEval ||
            hb_stackItem( nOffset )->item.asSymbol.value->pDynSym ==
            hb_symEval.pDynSym ) )
      {
         nOffset = hb_stackItem( nOffset )->item.asSymbol.stackstate->nBaseItem;

         /* I do not like it but Class(y) makes sth like that. [druzus] */
         while( nOffset > 0 &&
                hb_stackItem( nOffset )->item.asSymbol.stackstate->uiClass == 0 )
            nOffset = hb_stackItem( nOffset )->item.asSymbol.stackstate->nBaseItem;
      }
      return nOffset;
   }
   return -1;
}
#endif

#if 0
static HB_USHORT hb_clsSenderClass( void )
{
   HB_ISIZ nOffset = hb_clsSenderOffset();

   if( nOffset > 0 )
      return hb_stackItem( nOffset )->item.asSymbol.stackstate->uiClass;
   else
      return 0;
}
#endif

static HB_USHORT hb_clsSenderMethodClass( void )
{
   HB_ISIZ nOffset = hb_clsSenderOffset();

   if( nOffset > 0 )
   {
      HB_STACK_TLS_PRELOAD
      PHB_STACK_STATE pStack = hb_stackItem( nOffset )->item.asSymbol.stackstate;

      if( pStack->uiClass )
         return ( s_pClasses[ pStack->uiClass ]->pMethods +
                  pStack->uiMethod )->uiSprClass;
   }
   return 0;
}

static PHB_SYMB hb_clsSenderSymbol( void )
{
   PHB_SYMB pSym = NULL;
   HB_ISIZ nOffset = hb_clsSenderOffset();

   if( nOffset > 0 )
   {
      HB_STACK_TLS_PRELOAD
      pSym = hb_stackItem( nOffset )->item.asSymbol.value;

      if( pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym )
      {
         PHB_ITEM pBlock = hb_stackItem( nOffset + 1 );

         if( HB_IS_BLOCK( pBlock ) )
            pSym = pBlock->item.asBlock.value->pDefSymb;
      }
   }

   return hb_vmGetRealFuncSym( pSym );
}

static HB_USHORT hb_clsSenderObjectClass( void )
{
   HB_ISIZ nOffset = hb_clsSenderOffset();

   if( nOffset > 0 )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pSender = hb_stackItem( nOffset + 1 );

      if( HB_IS_ARRAY( pSender ) )
         return pSender->item.asArray.value->uiClass;
   }
   return 0;
}

static PHB_SYMB hb_clsValidScope( PMETHOD pMethod, PHB_STACK_STATE pStack )
{
   if( pMethod->uiScope & ( HB_OO_CLSTP_HIDDEN | HB_OO_CLSTP_PROTECTED |
                            HB_OO_CLSTP_OVERLOADED ) )
   {
      HB_USHORT uiSenderClass = hb_clsSenderMethodClass();

      if( uiSenderClass == pMethod->uiSprClass )
         return pMethod->pFuncSym;
      else if( uiSenderClass )
      {
         /*
          * Warning!!! Friends cannot access overloaded non virtual methods.
          * This feature is available _ONLY_ for real class members, [druzus]
          */
         if( pMethod->uiScope & HB_OO_CLSTP_OVERLOADED &&
             hb_clsHasParentClass( s_pClasses[ pStack->uiClass ], uiSenderClass ) )
         {
            PCLASS pClass = s_pClasses[ uiSenderClass ];
            PMETHOD pHiddenMthd = hb_clsFindMsg( pClass, pMethod->pMessage );

            if( pHiddenMthd && ( pHiddenMthd->uiScope & HB_OO_CLSTP_NONVIRTUAL ) &&
                pHiddenMthd->uiSprClass == uiSenderClass )
            {
               pStack->uiClass = uiSenderClass;
               pStack->uiMethod = ( HB_USHORT ) ( pHiddenMthd - pClass->pMethods );
               return pHiddenMthd->pFuncSym;
            }
         }

         if( pMethod->uiScope & HB_OO_CLSTP_HIDDEN )
         {
            if( ! hb_clsIsFriendSymbol( s_pClasses[ pMethod->uiSprClass ],
                              s_pClasses[ uiSenderClass ]->pClassFuncSym ) )
               return &s___msgScopeErr;
         }
         else if( pMethod->uiScope & HB_OO_CLSTP_PROTECTED &&
             ! hb_clsHasParentClass( s_pClasses[ pStack->uiClass ],
                                     uiSenderClass ) &&
             ! hb_clsHasParentClass( s_pClasses[ uiSenderClass ],
                                     pStack->uiClass ) &&
             ! hb_clsIsFriendSymbol( s_pClasses[ pMethod->uiSprClass ],
                                     s_pClasses[ uiSenderClass ]->pClassFuncSym ) &&
             ( pStack->uiClass == pMethod->uiSprClass ||
               ! hb_clsIsFriendSymbol( s_pClasses[ pStack->uiClass ],
                              s_pClasses[ uiSenderClass ]->pClassFuncSym ) ) )
            return &s___msgScopeErr;
      }
      else if( pMethod->uiScope & ( HB_OO_CLSTP_HIDDEN | HB_OO_CLSTP_PROTECTED ) )
      {
         PHB_SYMB pSym = hb_clsSenderSymbol();

         if( ! hb_clsIsFriendSymbol( s_pClasses[ pMethod->uiSprClass ], pSym ) )
         {
            if( ( pMethod->uiScope & HB_OO_CLSTP_HIDDEN ) ||
                ! hb_clsIsFriendSymbol( s_pClasses[ pStack->uiClass ], pSym ) )
               return &s___msgScopeErr;
         }
      }
   }

   return pMethod->pFuncSym;
}

static PHB_SYMB hb_clsScalarMethod( PCLASS pClass, PHB_DYNS pMsg,
                                    PHB_STACK_STATE pStack )
{
   PMETHOD pMethod = hb_clsFindMsg( pClass, pMsg );

   if( pStack )
   {
      pStack->uiClass = pClass->uiClass;
      if( pMethod )
      {
         pStack->uiMethod = ( HB_USHORT ) ( pMethod - pClass->pMethods );
         return hb_clsValidScope( pMethod, pStack );
      }
   }
   else if( pMethod )
      return pMethod->pFuncSym;

   return NULL;
}

static void hb_clsMakeSuperObject( PHB_ITEM pDest, PHB_ITEM pObject,
                                   HB_USHORT uiSuperClass )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsMakeSuperObject(%p, %p, %hu)", pDest, pObject, uiSuperClass ) );

   /* create a fake object array */
   hb_arrayNew( pDest, 1 );
   /* Now save the Self object as the 1st elem. */
   hb_arraySet( pDest, 1, pObject );
   /* And transform it into a fake object */
   /* backup of actual handel */
   pDest->item.asArray.value->uiPrevCls = hb_objGetClassH( pObject );
   /* superclass handel casting */
   pDest->item.asArray.value->uiClass = uiSuperClass;
}

/*
 * <pFuncSym> = hb_objGetMethod( <pObject>, <pMessage>, <pStackState> )
 *
 * Internal function to the function pointer of a message of an object
 */
PHB_SYMB hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pMessage,
                          PHB_STACK_STATE pStack )
{
   HB_STACK_TLS_PRELOAD
   PCLASS pClass = NULL;
   PHB_DYNS pMsg;

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetMethod(%p, %p, %p)", pObject, pMessage, pStack ) );

   pMsg = pMessage->pDynSym;

   if( HB_IS_ARRAY( pObject ) )
   {
      if( pObject->item.asArray.value->uiClass )
      {
         pClass = s_pClasses[ pObject->item.asArray.value->uiClass ];
         if( pStack )
         {
            pStack->uiClass = pObject->item.asArray.value->uiClass;
            if( pObject->item.asArray.value->uiPrevCls )
            {
               if( pObject->item.asArray.value->nLen )
               {
                  /*
                   * Copy real object - do not move! the same super casted
                   * object can be used more then once and we mustn't
                   * destroy it. We can safely use hb_stackReturnItem() here.
                   */
                  hb_itemCopy( hb_stackReturnItem(), pObject->item.asArray.value->pItems );
                  /* move real object back to the stack */
                  hb_itemMove( pObject, hb_stackReturnItem() );
               }
               else
                  /* Someone tried to manipulate with supercast array */
                  hb_itemClear( pObject );
            }
#ifdef HB_MSG_POOL
            {
               HB_USHORT uiBucket = BUCKETSIZE, * puiMsgIdx =
                  pClass->puiMsgIdx + hb_clsBucketPos( pMsg, pClass->uiHashKey );

               do
               {
                  PMETHOD pMethod = &pClass->pMethods[ *puiMsgIdx ];
                  if( pMethod->pMessage == pMsg )
                  {
                     pStack->uiMethod = *puiMsgIdx;
                     return hb_clsValidScope( pMethod, pStack );
                  }
                  ++puiMsgIdx;
               }
               while( --uiBucket );
            }
#else
            {
               PMETHOD pMethod = hb_clsFindMsg( pClass, pMsg );
               if( pMethod )
               {
                  pStack->uiMethod = ( HB_USHORT ) ( pMethod - pClass->pMethods );
                  return hb_clsValidScope( pMethod, pStack );
               }
            }
#endif
         }
         else
         {
            PMETHOD pMethod = hb_clsFindMsg( pClass, pMsg );
            if( pMethod )
               return pMethod->pFuncSym;
         }
      }
      else if( s_uiArrayClass )
      {
         pClass = s_pClasses[ s_uiArrayClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_BLOCK( pObject ) )
   {
      if( pMsg == hb_symEval.pDynSym )
         return &hb_symEval;
      else if( s_uiBlockClass )
      {
         pClass = s_pClasses[ s_uiBlockClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_BYREF( pObject ) )
   {
      if( pStack )
      {
         /* method of enumerator variable from FOR EACH statement
          */
         PHB_ITEM pEnum = hb_itemUnRefOnce( pObject );

         if( HB_IS_ENUM( pEnum ) )
         {
            /*
             * Do actions here - we already have unreferenced pEnum so
             * it will be a little bit faster but in the future it's
             * possible that I'll move it to separate function when
             * I'll add enumerators overloading. [druzus]
             */
            if( pMsg == s___msgEnumIndex.pDynSym )
            {
               hb_itemPutNS( hb_stackReturnItem(), pEnum->item.asEnum.offset );
               if( hb_pcount() > 0 && HB_ISNUM( 1 ) )
                  pEnum->item.asEnum.offset = hb_itemGetNS( hb_param( 1, HB_IT_ANY ) );
               return &s___msgEnumIndex;
            }
            else if( pMsg == s___msgEnumKey.pDynSym )
            {
               PHB_ITEM pBase = HB_IS_BYREF( pEnum->item.asEnum.basePtr ) ?
                                hb_itemUnRef( pEnum->item.asEnum.basePtr ) :
                                pEnum->item.asEnum.basePtr;
               if( HB_IS_HASH( pBase ) )
               {
                  pBase = hb_hashGetKeyAt( pBase, pEnum->item.asEnum.offset );
                  if( pBase )
                     hb_itemCopy( hb_stackReturnItem(), pBase );
               }
               return &s___msgEnumKey;
            }
            else if( pMsg == s___msgEnumBase.pDynSym )
            {
               if( HB_IS_BYREF( pEnum->item.asEnum.basePtr ) )
                  hb_itemCopy( hb_stackReturnItem(),
                               hb_itemUnRef( pEnum->item.asEnum.basePtr ) );
               else
                  hb_itemCopy( hb_stackReturnItem(),
                               pEnum->item.asEnum.basePtr );
               if( hb_pcount() > 0 )
                  hb_itemCopy( pEnum->item.asEnum.basePtr,
                               hb_itemUnRef( hb_stackItemFromBase( 1 ) ) );
               return &s___msgEnumBase;
            }
            else if( pMsg == s___msgEnumValue.pDynSym )
            {
               pEnum = hb_itemUnRef( pEnum );
               hb_itemCopy( hb_stackReturnItem(), pEnum );
               if( hb_pcount() > 0 )
                  hb_itemCopy( pEnum, hb_itemUnRef( hb_stackItemFromBase( 1 ) ) );
               return &s___msgEnumValue;
            }
         }
      }
   }
   else if( HB_IS_SYMBOL( pObject ) )
   {
      if( s_uiSymbolClass )
      {
         pClass = s_pClasses[ s_uiSymbolClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
      if( pMsg == s___msgExec.pDynSym || pMsg == hb_symEval.pDynSym )
      {
         if( ! pObject->item.asSymbol.value->value.pFunPtr &&
               pObject->item.asSymbol.value->pDynSym )
            return pObject->item.asSymbol.value->pDynSym->pSymbol;
         else
            return pObject->item.asSymbol.value;
      }
      else if( pMsg == s___msgName.pDynSym )
      {
         hb_itemPutC( hb_stackReturnItem(),
                      pObject->item.asSymbol.value->szName );
         return &s___msgName;
      }
   }
   else if( HB_IS_HASH( pObject ) )
   {
      if( s_uiHashClass )
      {
         pClass = s_pClasses[ s_uiHashClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }

      if( pMsg == s___msgKeys.pDynSym )
      {
         hb_itemReturnRelease( hb_hashGetKeys( pObject ) );
         return &s___msgKeys;
      }
      else if( pMsg == s___msgValues.pDynSym )
      {
         hb_itemReturnRelease( hb_hashGetValues( pObject ) );
         return &s___msgValues;
      }
#if defined( HB_HASH_MSG_ITEMS )
      else
      {
         if( hb_pcount() == 1 && pMessage->szName[ 0 ] == '_' ) /* ASSIGN */
         {
            PHB_ITEM pIndex = hb_itemPutCConst( hb_stackAllocItem(), pMessage->szName + 1 );
            PHB_ITEM pDest = hb_hashGetItemPtr( pObject, pIndex, HB_HASH_AUTOADD_ASSIGN );
            hb_stackPop();
            if( pDest )
            {
               PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );
               hb_itemCopyFromRef( pDest, pValue );
               hb_itemReturn( pValue );
               return &s___msgVirtual;
            }
         }
         else if( hb_pcount() == 0 ) /* ACCESS */
         {
            PHB_ITEM pIndex = hb_itemPutCConst( hb_stackAllocItem(), pMessage->szName );
            PHB_ITEM pValue = hb_hashGetItemPtr( pObject, pIndex, HB_HASH_AUTOADD_ACCESS );
            hb_stackPop();
            if( pValue )
            {
               hb_itemReturn( pValue );
               return &s___msgVirtual;
            }
         }
      }
#endif
   }
   else if( HB_IS_STRING( pObject ) )
   {
      if( s_uiCharacterClass )
      {
         pClass = s_pClasses[ s_uiCharacterClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_DATE( pObject ) )
   {
      if( s_uiDateClass )
      {
         pClass = s_pClasses[ s_uiDateClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_TIMESTAMP( pObject ) )
   {
      if( s_uiTimeStampClass )
      {
         pClass = s_pClasses[ s_uiTimeStampClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_NUMERIC( pObject ) )
   {
      if( s_uiNumericClass )
      {
         pClass = s_pClasses[ s_uiNumericClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_LOGICAL( pObject ) )
   {
      if( s_uiLogicalClass )
      {
         pClass = s_pClasses[ s_uiLogicalClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_POINTER( pObject ) )
   {
      if( s_uiPointerClass )
      {
         pClass = s_pClasses[ s_uiPointerClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( HB_IS_NIL( pObject ) )
   {
      if( s_uiNilClass )
      {
         pClass = s_pClasses[ s_uiNilClass ];
         {
            PHB_SYMB pExecSym = hb_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }

   /* Default messages here */
   if( pMsg == s___msgWithObjectPush.pDynSym )
   {
      if( pStack )
      {
         PHB_ITEM pItem = hb_stackWithObjectItem();
         if( pItem )
         {
            /* push current WITH OBJECT object */
            hb_itemCopy( hb_stackReturnItem(), pItem );
            return &s___msgWithObjectPush;
         }
      }
   }
   else if( pMsg == s___msgWithObjectPop.pDynSym )
   {
      if( pStack )
      {
         PHB_ITEM pItem = hb_stackWithObjectItem();
         if( pItem )
         {
            /* replace current WITH OBJECT object */
            hb_itemCopy( pItem, hb_stackItemFromBase( 1 ) );
            hb_itemCopy( hb_stackReturnItem(), pItem );
            return &s___msgWithObjectPop;
         }
      }
   }
   else if( pMsg == s___msgClassName.pDynSym )
      return &s___msgClassName;

   else if( pMsg == s___msgClassH.pDynSym )
      return &s___msgClassH;

   else if( pMsg == s___msgClassSel.pDynSym )
      return &s___msgClassSel;

/*
   else if( pMsg == s___msgClsParent.pDynSym )
      return &s___msgClsParent;

   else if( pMsg == s___msgClass.pDynSym )
      return &s___msgClass;
 */
   if( pStack )
   {
      if( pClass && pClass->fHasOnError )
      {
         PMETHOD pMethod = hb_clsFindMsg( pClass, s___msgOnError.pDynSym );
         if( pMethod )
         {
            pStack->uiMethod = ( HB_USHORT ) ( pMethod - pClass->pMethods );
            return pMethod->pFuncSym;
         }
      }

      /* remove this line if you want default HVM error message */
      return &s___msgNoMethod;
   }
   return NULL;
}

HB_BOOL hb_objGetVarRef( PHB_ITEM pObject, PHB_SYMB pMessage,
                         PHB_STACK_STATE pStack )
{
   PHB_SYMB pExecSym;

#if defined( HB_HASH_MSG_ITEMS )
   if( HB_IS_HASH( pObject ) )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pIndex = hb_itemPutCConst( hb_stackAllocItem(), pMessage->szName + 1 );
      PHB_ITEM pValue = hb_hashGetItemRefPtr( pObject, pIndex );
      hb_stackPop();
      if( pValue )
         hb_itemReturn( pValue );
      return pValue != NULL;
   }
#endif

   pExecSym = hb_objGetMethod( pObject, pMessage, pStack );
   if( pExecSym )
   {
      HB_STACK_TLS_PRELOAD
      if( pExecSym == &s___msgSetData )
      {
         HB_USHORT uiObjClass = pObject->item.asArray.value->uiClass;
         PCLASS pClass        = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod      = pClass->pMethods + pStack->uiMethod;
         HB_SIZE nIndex       = pMethod->uiData;

         if( pStack->uiClass != uiObjClass )
            nIndex += hb_clsParentInstanceOffset( s_pClasses[ uiObjClass ],
                                                  pMethod->uiSprClass );
         else
            nIndex += pMethod->uiOffset;

         /* will arise only if the class has been modified after first instance */
         if( nIndex > hb_arrayLen( pObject ) ) /* Resize needed */
            hb_arraySize( pObject, nIndex );   /* Make large enough */

         return hb_arrayGetItemRef( pObject, nIndex, hb_stackReturnItem() );
      }
      else if( pExecSym == &s___msgSetClsData )
      {
         PCLASS pClass   = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         return hb_arrayGetItemRef( pClass->pClassDatas, pMethod->uiData,
                                    hb_stackReturnItem() );
      }
      else if( pExecSym == &s___msgSetShrData )
      {
         PCLASS pClass   = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         return hb_arrayGetItemRef( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                                    pMethod->uiData, hb_stackReturnItem() );
      }
      else if( pExecSym == &s___msgScopeErr )
      {
         pExecSym->value.pFunPtr();
      }
      else
      {
         PCLASS pClass   = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         if( pMethod->pMessage == s___msgOnError.pDynSym )
            return hb_vmMsgReference( pObject, pMessage->pDynSym, NULL );

         if( ! pMethod->pAccMsg )
            pMethod->pAccMsg = hb_dynsymGetCase( pMessage->szName + 1 );

         return hb_vmMsgReference( pObject, pMessage->pDynSym, pMethod->pAccMsg );
      }
   }

   return HB_FALSE;
}

/*
 * Check if class has object destructors
 */
HB_BOOL hb_clsHasDestructor( HB_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->fHasDestructor;
   else
      return HB_FALSE;
}

/*
 * Call all known super destructors
 */
static void hb_objSuperDestructorCall( PHB_ITEM pObject, PCLASS pClass )
{
#if 0
   HB_STACK_TLS_PRELOAD
   PMETHOD pMethod = pClass->pMethods;
   HB_SIZE nLimit = hb_clsMthNum( pClass );
   char * pcClasses;
   HB_USHORT uiClass;

   pcClasses = ( char * ) hb_xgrab( ( HB_SIZE ) s_uiClasses + 1 );
   memset( pcClasses, 0, s_uiClasses + 1 );

   do
   {
      if( pMethod->pMessage )
      {
         if( pMethod->pFuncSym == &s___msgSuper )
         {
            PCLASS pSuperClass = s_pClasses[ pMethod->uiData ];
            if( pSuperClass->fHasDestructor && pSuperClass != pClass )
               pcClasses[ pMethod->uiData ] |= 1;
         }
         else if( pMethod->pMessage == s___msgDestructor.pDynSym )
            pcClasses[ pMethod->uiSprClass ] |= 2;
      }
      ++pMethod;
   }
   while( --nLimit );

   for( uiClass = s_uiClasses; uiClass; --uiClass )
   {
      if( pcClasses[ uiClass ] == 1 )
      {
         PMETHOD pDestructor = hb_clsFindMsg( s_pClasses[ uiClass ],
                                              s___msgDestructor.pDynSym );
         if( pDestructor )
         {
            if( pcClasses[ pDestructor->uiSprClass ] == 1 )
            {
               hb_vmPushSymbol( &s___msgDestructor );
               hb_clsMakeSuperObject( hb_stackAllocItem(), pObject, uiClass );
               hb_vmSend( 0 );
               if( hb_vmRequestQuery() != 0 )
                  break;
               pcClasses[ pDestructor->uiSprClass ] |= 2;
            }
         }
      }
   }

   hb_xfree( pcClasses );
#else
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiCount, uiDtorClass;

   uiDtorClass = hb_clsFindMsg( pClass, s___msgDestructor.pDynSym )->uiSprClass;
   uiCount = pClass->uiSuperClasses;
   while( uiCount-- )
   {
      HB_USHORT uiParentCls = pClass->pSuperClasses[ uiCount ].uiClass;

      if( uiParentCls != uiDtorClass && uiParentCls != pClass->uiClass )
      {
         PCLASS pSuperClass = s_pClasses[ uiParentCls ];

         if( pSuperClass->fHasDestructor )
         {
            PMETHOD pDestructor = hb_clsFindMsg( s_pClasses[ uiParentCls ],
                                                 s___msgDestructor.pDynSym );
            if( pDestructor && pDestructor->uiSprClass == uiParentCls )
            {
               hb_vmPushSymbol( &s___msgDestructor );
               hb_clsMakeSuperObject( hb_stackAllocItem(), pObject, uiParentCls );
               hb_vmSend( 0 );
               if( hb_vmRequestQuery() != 0 )
                  break;
            }
         }
      }
   }
#endif
}

/*
 * Call object destructor
 */
void hb_objDestructorCall( PHB_ITEM pObject )
{
   if( HB_IS_OBJECT( pObject ) &&
       pObject->item.asArray.value->uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ pObject->item.asArray.value->uiClass ];

      if( pClass->fHasDestructor )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushSymbol( &s___msgDestructor );
            hb_vmPush( pObject );
            hb_vmSend( 0 );
            if( hb_vmRequestQuery() == 0 )
               hb_objSuperDestructorCall( pObject, pClass );
            hb_vmRequestRestore();
         }
      }
   }
}

/*
 * Check if object has a given operator
 */
HB_BOOL hb_objHasOperator( PHB_ITEM pObject, HB_USHORT uiOperator )
{
   HB_USHORT uiClass;

   HB_TRACE( HB_TR_DEBUG, ( "hb_objHasOperator(%p,%hu)", pObject, uiOperator ) );

   uiClass = hb_objGetClassH( pObject );
   if( uiClass && uiClass <= s_uiClasses )
   {
      return ( s_pClasses[ uiClass ]->nOpFlags & ( 1 << uiOperator ) ) != 0;
   }

   return HB_FALSE;
}

/*
 * Call object operator. If pMsgArg is NULL then operator is unary.
 * Function return HB_TRUE when object class overloads given operator
 * and HB_FALSE otherwise. [druzus]
 */
HB_BOOL hb_objOperatorCall( HB_USHORT uiOperator, PHB_ITEM pResult, PHB_ITEM pObject,
                            PHB_ITEM pMsgArg1, PHB_ITEM pMsgArg2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_objOperatorCall(%hu,%p,%p,%p,%p)", uiOperator, pResult, pObject, pMsgArg1, pMsgArg2 ) );

   if( hb_objHasOperator( pObject, uiOperator ) )
   {
      HB_STACK_TLS_PRELOAD
      hb_vmPushSymbol( s_opSymbols + uiOperator );
      hb_vmPush( pObject );
      hb_itemSetNil( hb_stackReturnItem() );
      if( pMsgArg1 )
      {
         hb_vmPush( pMsgArg1 );
         if( pMsgArg2 )
         {
            hb_vmPush( pMsgArg2 );
            hb_vmSend( 2 );
         }
         else
            hb_vmSend( 1 );
      }
      else
         hb_vmSend( 0 );

      /* store the return value */
      hb_itemMove( pResult, hb_stackReturnItem() );
      return HB_TRUE;
   }
   return HB_FALSE;
}

/*
 * return HB_TRUE if object has a given message
 */
HB_BOOL hb_objHasMessage( PHB_ITEM pObject, PHB_DYNS pMessage )
{
   return hb_objGetMethod( pObject, pMessage->pSymbol, NULL ) != NULL;
}

/*
 * <bool> = hb_objHasMsg( <pObject>, <szString> )
 *
 * Check whether <szString> is an existing message for object.
 *
 * <uPtr> should be read as a boolean
 */
HB_BOOL hb_objHasMsg( PHB_ITEM pObject, const char * szString )
{
   PHB_DYNS pDynSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_objHasMsg(%p, %s)", pObject, szString ) );

   pDynSym = hb_dynsymFindName( szString );
   if( pDynSym )
      return hb_objGetMethod( pObject, pDynSym->pSymbol, NULL ) != NULL;
   else
      return HB_FALSE;
}

PHB_ITEM hb_objSendMessage( PHB_ITEM pObject, PHB_DYNS pMsgSym, HB_ULONG ulArg, ... )
{
   if( pObject && pMsgSym )
   {
      hb_vmPushSymbol( pMsgSym->pSymbol );
      hb_vmPush( pObject );

      if( ulArg )
      {
         HB_ULONG i;
         va_list ap;

         va_start( ap, ulArg );
         for( i = 0; i < ulArg; i++ )
         {
            hb_vmPush( va_arg( ap, PHB_ITEM ) );
         }
         va_end( ap );
      }
      hb_vmSend( ( HB_USHORT ) ulArg );
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__ObjSendMessage()", 0 );

   {
      HB_STACK_TLS_PRELOAD
      return hb_stackReturnItem();
   }
}

PHB_ITEM hb_objSendMsg( PHB_ITEM pObject, const char * szMsg, HB_ULONG ulArg, ... )
{
   hb_vmPushSymbol( hb_dynsymGet( szMsg )->pSymbol );
   hb_vmPush( pObject );
   if( ulArg )
   {
      HB_ULONG i;
      va_list ap;

      va_start( ap, ulArg );
      for( i = 0; i < ulArg; i++ )
      {
         hb_vmPush( va_arg( ap, PHB_ITEM ) );
      }
      va_end( ap );
   }
   hb_vmSend( ( HB_USHORT ) ulArg );

   {
      HB_STACK_TLS_PRELOAD
      return hb_stackReturnItem();
   }
}

PHB_ITEM hb_objGetVarPtr( PHB_ITEM pObject, PHB_DYNS pVarMsg )
{
   if( pObject && HB_IS_OBJECT( pObject ) && pVarMsg )
   {
      HB_USHORT uiClass = pObject->item.asArray.value->uiClass;
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod = hb_clsFindMsg( pClass, pVarMsg );

      if( pMethod )
      {
         PHB_SYMB pFuncSym = pMethod->pFuncSym;

         if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
            pFuncSym = pMethod->pRealSym;

         if( pFuncSym == &s___msgSetData || pFuncSym == &s___msgGetData )
         {
            HB_SIZE nIndex = pMethod->uiData + pMethod->uiOffset;
            if( pObject->item.asArray.value->uiPrevCls )
            {
               pObject = hb_arrayGetItemPtr( pObject, 1 );
               if( ! pObject )
                  return NULL;
               if( uiClass != pObject->item.asArray.value->uiClass )
                  nIndex = pMethod->uiData +
                           hb_clsParentInstanceOffset( s_pClasses[ pObject->item.asArray.value->uiClass ],
                                                       pMethod->uiSprClass );
            }
            return hb_arrayGetItemPtr( pObject, nIndex );
         }
      }
   }
   return NULL;
}

static PHB_DYNS hb_objGetMsgSym( PHB_ITEM pMessage )
{
   PHB_DYNS pDynSym = NULL;

   if( pMessage )
   {
      const char * szMsg = NULL;

      if( HB_IS_STRING( pMessage ) )
         szMsg = pMessage->item.asString.value;
      else if( HB_IS_SYMBOL( pMessage ) )
      {
         pDynSym = pMessage->item.asSymbol.value->pDynSym;
         if( ! pDynSym )
            szMsg = pMessage->item.asSymbol.value->szName;
      }

      if( szMsg && *szMsg )
         pDynSym = hb_dynsymGet( szMsg );
   }

   return pDynSym;
}

static PHB_SYMB hb_objGetFuncSym( PHB_ITEM pItem )
{
   if( pItem )
   {
      if( HB_IS_SYMBOL( pItem ) )
         return pItem->item.asSymbol.value;
      else if( HB_IS_STRING( pItem ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFindName( hb_itemGetCPtr( pItem ) );

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
            return pDynSym->pSymbol;
      }
   }

   return NULL;
}

/* clone object if user defined clone method or copy it */
void hb_objCloneTo( PHB_ITEM pDest, PHB_ITEM pSource, PHB_NESTED_CLONED pClonedList )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_objCloneTo(%p,%p,%p)", pDest, pSource, pClonedList ) );

   HB_SYMBOL_UNUSED( pClonedList );

   /* TODO: add support for user defined clone operation */

   hb_itemCopy( pDest, pSource );
}

/* send message which allows to set execution context for debugger */
void hb_dbg_objSendMessage( int iProcLevel, PHB_ITEM pObject, PHB_ITEM pMessage, int iParamOffset )
{
   HB_STACK_TLS_PRELOAD
   PHB_DYNS pMsgSym;

   pMsgSym = hb_objGetMsgSym( pMessage );
   if( pObject && pMsgSym )
   {
      HB_USHORT uiParams = 0;

      /* set requested sender class and method id for scope verification */
      if( iProcLevel > 0 )
      {
         int iLevel = hb_stackCallDepth();
         if( iProcLevel < iLevel )
         {
            HB_ISIZ nOffset = hb_stackBaseProcOffset( iLevel - iProcLevel );
            if( nOffset > 0 )
            {
               PHB_ITEM pItem = hb_stackItem( nOffset );
               PHB_ITEM pBase = hb_stackBaseItem();
               pBase->item.asSymbol.stackstate->uiClass =
                                    pItem->item.asSymbol.stackstate->uiClass;
               pBase->item.asSymbol.stackstate->uiMethod =
                                    pItem->item.asSymbol.stackstate->uiMethod;
            }
         }
      }
      else if( iProcLevel == 0 )
      {
         /* set scope like for internal object messages to any visible
            method without respecting overloaded methods */
         HB_USHORT uiClass = hb_objGetClassH( pObject );

         if( uiClass && uiClass <= s_uiClasses )
         {
            PMETHOD pMethod = hb_clsFindMsg( s_pClasses[ uiClass ], pMsgSym );
            if( pMethod )
            {
               PHB_ITEM pBase = hb_stackBaseItem();

               pBase->item.asSymbol.stackstate->uiClass = uiClass;
               pBase->item.asSymbol.stackstate->uiMethod =
                     ( HB_USHORT ) ( pMethod - s_pClasses[ uiClass ]->pMethods );
            }
         }
      }

      hb_vmPushSymbol( pMsgSym->pSymbol );
      hb_vmPush( pObject );

      if( iParamOffset > 0 )
      {
         int iPCount = hb_pcount();

         while( iParamOffset <= iPCount )
         {
            hb_vmPush( hb_stackItemFromBase( iParamOffset ) );
            ++uiParams;
            ++iParamOffset;
         }
      }
      hb_vmSend( uiParams );
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, "hb_dbg_objSendMessage()", 2, pObject, pMsgSym );
}

static HB_USHORT hb_clsUpdateScope( HB_USHORT uiScope, HB_BOOL fAssign )
{
   if( ! fAssign )
      uiScope &= ~HB_OO_CLSTP_READONLY;
   else
   {
      uiScope &= ~HB_OO_CLSTP_PERSIST;

      if( ( uiScope & HB_OO_CLSTP_READONLY ) &&
         !( uiScope & HB_OO_CLSTP_HIDDEN ) )
      {
         /* Class(y) does not allow to write to HIDDEN+READONLY
            instance variables, [druzus] */

         uiScope &= ~HB_OO_CLSTP_READONLY;
         uiScope |= uiScope & HB_OO_CLSTP_PROTECTED ?
                       HB_OO_CLSTP_HIDDEN : HB_OO_CLSTP_PROTECTED;
      }
   }
   return uiScope;
}

static HB_TYPE hb_clsGetItemType( PHB_ITEM pItem, HB_TYPE nDefault )
{
   if( pItem )
   {
      if( HB_IS_STRING( pItem ) )
      {
         switch( hb_itemGetCPtr( pItem )[ 0 ] )
         {
            case 'C':
            case 'c':
            case '\0':
               if( hb_strnicmp( hb_itemGetCPtr( pItem ), "code", 4 ) == 0 )
                  return HB_IT_BLOCK;
               else
                  return HB_IT_STRING;

            case 'S':
            case 's':
               if( hb_strnicmp( hb_itemGetCPtr( pItem ), "str", 3 ) == 0 )
                  return HB_IT_STRING;
               else
                  return HB_IT_SYMBOL;

            case 'B':
            case 'b':
               return HB_IT_BLOCK;

            case 'D':
            case 'd':
               return HB_IT_DATE;

            case 'T':
            case 't':
               return HB_IT_TIMESTAMP;

            case 'L':
            case 'l':
               return HB_IT_LOGICAL;

            case 'I':
            case 'i':
               return HB_IT_NUMINT;

            case 'N':
            case 'n':
               if( hb_stricmp( hb_itemGetCPtr( pItem ), "nil" ) == 0 )
                  return HB_IT_NIL;
               else
                  return HB_IT_NUMERIC;

            case 'A':
            case 'a':
               return HB_IT_ARRAY;

            case 'P':
            case 'p':
               return HB_IT_POINTER;

            case 'H':
            case 'h':
               return HB_IT_HASH;
         }
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         if( pItem->item.asArray.value->uiClass == 0 )
            return HB_IT_ARRAY;
      }
      else if( HB_IS_NUMINT( pItem ) )
         return HB_IT_NUMINT;

      else if( HB_IS_NUMERIC( pItem ) )
         return HB_IT_NUMERIC;

      else if( HB_IS_DATE( pItem ) )
         return HB_IT_DATE;

      else if( HB_IS_TIMESTAMP( pItem ) )
         return HB_IT_TIMESTAMP;

      else if( HB_IS_LOGICAL( pItem ) )
         return HB_IT_LOGICAL;

      else if( HB_IS_BLOCK( pItem ) )
         return HB_IT_BLOCK;

      else if( HB_IS_POINTER( pItem ) )
         return HB_IT_POINTER;

      else if( HB_IS_SYMBOL( pItem ) )
         return HB_IT_SYMBOL;

      else if( HB_IS_NIL( pItem ) )
         return HB_IT_NIL;
   }

   return nDefault;
}

/* ================================================ */

/*
 * <uiType>    HB_OO_MSG_METHOD     : standard method
 *             HB_OO_MSG_ONERROR    : error handler method
 *             HB_OO_MSG_DESTRUCTOR : destructor method
 *             HB_OO_MSG_INLINE     : inline (codeblock) method
 *             HB_OO_MSG_ASSIGN     : assign instance data
 *             HB_OO_MSG_ACCESS     : access instance data
 *             HB_OO_MSG_CLSASSIGN  : assign class data
 *             HB_OO_MSG_CLSACCESS  : access class data
 *             HB_OO_MSG_SUPER      : supercasting
 *             HB_OO_MSG_REALCLASS  : caller method real class casting
 *             HB_OO_MSG_PERFORM    : perform method
 *             HB_OO_MSG_VIRTUAL    : virtual method
 *             HB_OO_MSG_DELEGATE   : delegate method
 *
 * <uiScope>   HB_OO_CLSTP_EXPORTED        1 : default for data and method
 *             HB_OO_CLSTP_PROTECTED       2 : method or data protected
 *             HB_OO_CLSTP_HIDDEN          4 : method or data hidden
 *           * HB_OO_CLSTP_CTOR            8 : method constructor
 *             HB_OO_CLSTP_READONLY       16 : data read only
 *             HB_OO_CLSTP_SHARED         32 : (method or) data shared
 *             HB_OO_CLSTP_CLASS          64 : message is class message not object
 *           * HB_OO_CLSTP_SUPER         128 : message is herited
 *             HB_OO_CLSTP_PERSIST       256 : message is persistent (PROPERTY)
 *             HB_OO_CLSTP_NONVIRTUAL    512 : Non Virtual message - should not be covered by subclass(es) messages with the same name when executed from a given class message
 *             HB_OO_CLSTP_OVERLOADED   1024 : message overload NONVIRTUAL one
 *             HB_OO_CLSTP_SYNC         2048 : message synchronized by object or class mutex
 *
 * <pFunction> HB_OO_MSG_METHOD     : \
 *             HB_OO_MSG_ONERROR    :  > Pointer to function
 *             HB_OO_MSG_DESTRUCTOR : /
 *             HB_OO_MSG_INLINE     : Code block
 *             HB_OO_MSG_ASSIGN     :  Index to instance area array 1 based (without offset)
 *             HB_OO_MSG_ACCESS     : /
 *             HB_OO_MSG_CLSASSIGN  :  Index class data array
 *             HB_OO_MSG_CLSACCESS  : /
 *             HB_OO_MSG_SUPER      : Instance area offset for class casting
 *             HB_OO_MSG_DELEGATE   : Delegated message symbol
 *
 * <pInit>     HB_OO_MSG_ACCESS     :  Optional initializer for (Class)DATA
 *             HB_OO_MSG_CLSACCESS  : /
 *             HB_OO_MSG_ASSIGN     :  Item type restriction in assignment
 *             HB_OO_MSG_CLSASSIGN  : /
 *             HB_OO_MSG_SUPER      : Superclass handle
 *             HB_OO_MSG_DELEGATE   : Object symbol for delegated message
 */
static HB_BOOL hb_clsAddMsg( HB_USHORT uiClass, const char * szMessage,
                             HB_USHORT uiType, HB_USHORT uiScope,
                             PHB_ITEM pFunction, PHB_ITEM pInit )
{
   if( szMessage && uiClass && uiClass <= s_uiClasses )
   {
      PCLASS    pClass   = s_pClasses[ uiClass ];

      PHB_DYNS  pMessage;
      PMETHOD   pNewMeth;
      HB_USHORT uiOperator, uiSprClass = 0, uiIndex = 0, uiPrevCls, uiPrevMth;
      PHB_SYMB  pOpSym, pFuncSym = NULL;
      HB_BOOL   fOK;
      HB_U32    nOpFlags = 0;

      if( pClass->fLocked )
         return HB_FALSE;

      if( ! ( uiScope & ( HB_OO_CLSTP_EXPORTED | HB_OO_CLSTP_PROTECTED | HB_OO_CLSTP_HIDDEN ) ) )
         uiScope |= HB_OO_CLSTP_EXPORTED;

      /* translate names of operator overloading messages */
      if( uiType == HB_OO_MSG_DESTRUCTOR )
         pMessage = s___msgDestructor.pDynSym;
      else if( uiType == HB_OO_MSG_ONERROR )
         pMessage = s___msgOnError.pDynSym;
      else if( strcmp( "+", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_PLUS )->pDynSym;
      else if( strcmp( "-", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_MINUS )->pDynSym;
      else if( strcmp( "*", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_MULT )->pDynSym;
      else if( strcmp( "/", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_DIVIDE )->pDynSym;
      else if( strcmp( "%", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_MOD )->pDynSym;
      else if( strcmp( "^", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_POWER )->pDynSym;
      else if( strcmp( "**", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_POWER )->pDynSym;
      else if( strcmp( "++", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_INC )->pDynSym;
      else if( strcmp( "--", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_DEC )->pDynSym;
      else if( strcmp( "==", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_EXACTEQUAL )->pDynSym;
      else if( strcmp( "=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_EQUAL )->pDynSym;
      else if( strcmp( "!=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_NOTEQUAL )->pDynSym;
      else if( strcmp( "<>", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_NOTEQUAL )->pDynSym;
      else if( strcmp( "#", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_NOTEQUAL )->pDynSym;
      else if( strcmp( "<", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_LESS )->pDynSym;
      else if( strcmp( "<=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_LESSEQUAL )->pDynSym;
      else if( strcmp( ">", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_GREATER )->pDynSym;
      else if( strcmp( ">=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_GREATEREQUAL )->pDynSym;
      else if( strcmp( ":=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_ASSIGN )->pDynSym;
      else if( strcmp( "$", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_INSTRING )->pDynSym;
      else if( strcmp( "$$", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_INCLUDE )->pDynSym;
      else if( strcmp( "!", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_NOT )->pDynSym;
      else if( hb_stricmp( ".NOT.", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_NOT )->pDynSym;
      else if( hb_stricmp( ".AND.", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_AND )->pDynSym;
      else if( hb_stricmp( ".OR.", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_OR )->pDynSym;
      else if( strcmp( "[]", szMessage ) == 0 )
         pMessage = ( s_opSymbols + HB_OO_OP_ARRAYINDEX )->pDynSym;
      else
         pMessage = hb_dynsymGet( szMessage );

      for( uiOperator = 0, pOpSym = s_opSymbols;
           uiOperator <= HB_OO_MAX_OPERATOR; ++uiOperator, ++pOpSym )
      {
         if( pOpSym->pDynSym == pMessage )
         {
            nOpFlags |= 1 << uiOperator;
            break;
         }
      }

      /* basic parameter validation */
      switch( uiType )
      {
         case HB_OO_MSG_METHOD:
         case HB_OO_MSG_ONERROR:
         case HB_OO_MSG_DESTRUCTOR:
            pFuncSym = hb_objGetFuncSym( pFunction );
            fOK = pFuncSym != NULL;
            break;

         case HB_OO_MSG_INLINE:
            fOK = pFunction && HB_IS_BLOCK( pFunction );
            break;

         case HB_OO_MSG_SUPER:
            uiIndex = ( HB_USHORT ) hb_itemGetNI( pFunction );
            uiSprClass = ( HB_USHORT ) hb_itemGetNI( pInit );
            fOK = uiSprClass && uiSprClass <= s_uiClasses &&
                  uiIndex <= pClass->uiDatas;
            break;

         case HB_OO_MSG_ASSIGN:
         case HB_OO_MSG_ACCESS:
            uiIndex = ( HB_USHORT ) hb_itemGetNI( pFunction );
            /* This validation can break buggy .prg code which wrongly
             * sets data offsets but IMHO it will help to clean the code.
             * [druzus]
             */
            fOK = uiIndex && uiIndex <= pClass->uiDatas - pClass->uiDataFirst;
            break;

         case HB_OO_MSG_CLSASSIGN:
         case HB_OO_MSG_CLSACCESS:
            uiIndex = ( HB_USHORT ) hb_itemGetNI( pFunction );
            fOK = uiIndex != 0;
            break;

         case HB_OO_MSG_DELEGATE:
         {
            PHB_DYNS pDelegMsg = hb_objGetMsgSym( pFunction );
            if( pDelegMsg )
            {
               pNewMeth = hb_clsFindMsg( pClass, pDelegMsg );
               if( pNewMeth )
                  uiIndex = ( HB_USHORT ) ( pNewMeth - pClass->pMethods );
            }
            fOK = pFunction == NULL || HB_IS_NIL( pFunction ) || uiIndex != 0;
            if( fOK )
            {
               pDelegMsg = hb_objGetMsgSym( pInit );
               if( pDelegMsg )
               {
                  pNewMeth = hb_clsFindMsg( pClass, pDelegMsg );
                  if( pNewMeth )
                     uiSprClass = ( HB_USHORT ) ( pNewMeth - pClass->pMethods );
               }
               fOK = ( pInit == NULL || HB_IS_NIL( pInit ) || uiSprClass != 0 ) &&
                     ( uiIndex != 0 || uiSprClass != 0 );
            }
            break;
         }
         case HB_OO_MSG_REALCLASS:
         case HB_OO_MSG_VIRTUAL:
         case HB_OO_MSG_PERFORM:
            fOK = HB_TRUE;
            break;

         default:
            fOK = HB_FALSE;
      }

      if( ! fOK )
      {
         hb_errRT_BASE( EG_ARG, 3000, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
         return HB_FALSE;
      }

      pNewMeth = hb_clsAllocMsg( pClass, pMessage );

      uiPrevCls = uiClass;
      uiPrevMth = ( HB_USHORT ) ( pClass->pMethods - pNewMeth );

#ifndef HB_VIRTUAL_HIDDEN
      if( uiScope & HB_OO_CLSTP_HIDDEN )
         uiScope |= HB_OO_CLSTP_NONVIRTUAL;
#endif

      if( ! pNewMeth->pMessage )
         pClass->uiMethods++;           /* One more message */
      else
      {
         HB_BOOL fOverLoad = ( pNewMeth->uiScope & HB_OO_CLSTP_OVERLOADED ) ||
                             ( ( pNewMeth->uiScope & HB_OO_CLSTP_NONVIRTUAL ) &&
                               pNewMeth->uiSprClass != uiClass );

         uiPrevCls = pNewMeth->uiPrevCls;
         uiPrevMth = pNewMeth->uiPrevMth;
         if( ! hb_clsCanClearMethod( pNewMeth, HB_TRUE ) )
            return HB_FALSE;

         memset( pNewMeth, 0, sizeof( METHOD ) );
         if( fOverLoad )
            uiScope |= HB_OO_CLSTP_OVERLOADED;
      }
      pNewMeth->pMessage = pMessage;
      pNewMeth->uiSprClass = uiClass;
      pNewMeth->uiPrevCls = uiPrevCls;
      pNewMeth->uiPrevMth = uiPrevMth;

      switch( uiType )
      {
         case HB_OO_MSG_METHOD:

            pNewMeth->pFuncSym = pFuncSym;
            pNewMeth->uiScope = uiScope;
            break;

         case HB_OO_MSG_ASSIGN:

            pNewMeth->uiScope = hb_clsUpdateScope( uiScope, HB_TRUE );
            /* Class(y) does not allow to write to HIDDEN+READONLY
               instance variables, [druzus] */
            if( pNewMeth->uiScope & HB_OO_CLSTP_READONLY &&
                pNewMeth->uiScope & HB_OO_CLSTP_HIDDEN )
               pNewMeth->pFuncSym = &s___msgScopeErr;
            else
            {
               pNewMeth->pFuncSym = &s___msgSetData;
               pNewMeth->uiData = uiIndex;
               pNewMeth->uiOffset = pClass->uiDataFirst;
               pNewMeth->itemType = hb_clsGetItemType( pInit, 0 );
            }
            break;

         case HB_OO_MSG_ACCESS:

            pNewMeth->uiScope = hb_clsUpdateScope( uiScope, HB_FALSE );
            pNewMeth->uiData = uiIndex;
            pNewMeth->uiOffset = pClass->uiDataFirst;
            hb_clsAddInitValue( pClass, pInit, HB_OO_MSG_DATA,
                                pNewMeth->uiData, pNewMeth->uiOffset, uiClass );
            pNewMeth->pFuncSym = &s___msgGetData;
            break;

         case HB_OO_MSG_CLSASSIGN:

            pNewMeth->uiData = uiIndex;
            pNewMeth->itemType = hb_clsGetItemType( pInit, 0 );
            pNewMeth->uiScope = hb_clsUpdateScope( uiScope, HB_TRUE );
            /* Class(y) does not allow to write to HIDDEN+READONLY
               instance variables, [druzus] */
            if( pNewMeth->uiScope & HB_OO_CLSTP_READONLY &&
                pNewMeth->uiScope & HB_OO_CLSTP_HIDDEN )
               pNewMeth->pFuncSym = &s___msgScopeErr;
            else if( pNewMeth->uiScope & HB_OO_CLSTP_SHARED )
            {
               if( hb_arrayLen( pClass->pSharedDatas ) < ( HB_SIZE ) pNewMeth->uiData )
                  hb_arraySize( pClass->pSharedDatas, pNewMeth->uiData );
               pNewMeth->pFuncSym = &s___msgSetShrData;
            }
            else
            {
               if( hb_arrayLen( pClass->pClassDatas ) < ( HB_SIZE ) pNewMeth->uiData )
                  hb_arraySize( pClass->pClassDatas, pNewMeth->uiData );
               pNewMeth->pFuncSym = &s___msgSetClsData;
            }
            break;

         case HB_OO_MSG_CLSACCESS:

            pNewMeth->uiScope = hb_clsUpdateScope( uiScope, HB_FALSE );
            pNewMeth->uiData = uiIndex;
            if( pNewMeth->uiScope & HB_OO_CLSTP_SHARED )
            {
               if( hb_arrayLen( pClass->pSharedDatas ) < ( HB_SIZE ) pNewMeth->uiData )
                  hb_arraySize( pClass->pSharedDatas, pNewMeth->uiData );

               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  /* Shared Classdata need to be initialized only once
                   * ACCESS/ASSIGN methods will be inherited by subclasses
                   * and will operate on this value so it's not necessary
                   * to keep the init value. [druzus]
                   */
                  hb_itemCloneTo( hb_arrayGetItemPtr( pClass->pSharedDatas,
                                                      pNewMeth->uiData ), pInit );
               }
               pNewMeth->pFuncSym = &s___msgGetShrData;
            }
            else
            {
               if( hb_arrayLen( pClass->pClassDatas ) < ( HB_SIZE ) pNewMeth->uiData )
                  hb_arraySize( pClass->pClassDatas, pNewMeth->uiData );
               /* uiOffset is used to copy ancestor class data initializaers when
                * new class is created
                */
               pNewMeth->uiOffset = hb_clsAddInitValue( pClass, pInit,
                           HB_OO_MSG_CLASSDATA, pNewMeth->uiData, 0, uiClass );
               pNewMeth->pFuncSym = &s___msgGetClsData;
            }
            break;

         case HB_OO_MSG_INLINE:

            pNewMeth->pFuncSym = &s___msgEvalInline;
            pNewMeth->uiScope = uiScope;
            hb_arrayAdd( pClass->pInlines, pFunction );
            pNewMeth->uiData = ( HB_USHORT ) hb_arrayLen( pClass->pInlines );
            break;

         case HB_OO_MSG_VIRTUAL:

            pNewMeth->pFuncSym = &s___msgVirtual;
            pNewMeth->uiScope = uiScope;
            break;

         case HB_OO_MSG_SUPER:

            pNewMeth->uiData = uiSprClass; /* store the super handel */
            pNewMeth->uiOffset = uiIndex; /* offset to instance area */
            pNewMeth->uiScope = uiScope;
            pNewMeth->pFuncSym = &s___msgSuper;
            break;

         case HB_OO_MSG_REALCLASS:

            pNewMeth->pFuncSym = &s___msgRealClass;
            pNewMeth->uiScope = uiScope;
            break;

         case HB_OO_MSG_PERFORM:
            pNewMeth->pFuncSym = &s___msgPerform;
            pNewMeth->uiScope = uiScope;
            break;

         case HB_OO_MSG_DELEGATE:

            pNewMeth->pFuncSym = &s___msgDelegate;
            pNewMeth->uiScope = uiScope;
            pNewMeth->uiData = uiIndex;
            break;

         case HB_OO_MSG_ONERROR:

            pNewMeth->pFuncSym = pFuncSym;
            pClass->fHasOnError = HB_TRUE;
            break;

         case HB_OO_MSG_DESTRUCTOR:

            pNewMeth->pFuncSym = pFuncSym;
            pClass->fHasDestructor = HB_TRUE;
            break;

         default:

            hb_errInternal( HB_EI_CLSINVMETHOD, NULL, "__clsAddMsg()", NULL );
      }

      pClass->nOpFlags |= nOpFlags;

      if( uiScope & HB_OO_CLSTP_SYNC )
      {
         pNewMeth->pRealSym = pNewMeth->pFuncSym;
         if( uiScope & HB_OO_CLSTP_CLASS )
         {
            if( ! pClass->pMutex )
               pClass->pMutex = hb_threadMutexCreate();
            pNewMeth->pFuncSym = &s___msgSyncClass;
         }
         else
         {
            if( ! pClass->uiMutexOffset )
               pClass->uiMutexOffset = pClass->uiDatas + 1;
            pNewMeth->pFuncSym = &s___msgSync;
         }
      }
   }

   return HB_TRUE;
}

/*
 * __clsAddMsg( <hClass>, <cMessage>, <pFunction>, <nType>, [xInit], <uiScope>, [xType] )
 *
 * Add a message to the class.
 *
 * <hClass>    Class handle
 * <cMessage>  Message
 * <pFunction> HB_OO_MSG_METHOD     : \
 *             HB_OO_MSG_ONERROR    :  > Pointer to function
 *             HB_OO_MSG_DESTRUCTOR : /
 *             HB_OO_MSG_INLINE     : Code block
 *             HB_OO_MSG_DATA       : \
 *             HB_OO_MSG_ASSIGN     :  > Index to instance area array
 *             HB_OO_MSG_ACCESS     : /
 *             HB_OO_MSG_CLASSDATA  : \
 *             HB_OO_MSG_CLSASSIGN  :  > Index class data array
 *             HB_OO_MSG_CLSACCESS  : /
 *             HB_OO_MSG_SUPER      : Handle of super class
 *             HB_OO_MSG_DELEGATE   : delegated message symbol
 *
 * <nType>     see HB_OO_MSG_* above and:
 *             HB_OO_MSG_REALCLASS  : caller method real class casting
 *             HB_OO_MSG_PERFORM    : perform message
 *             HB_OO_MSG_VIRTUAL    : virtual message
 *             HB_OO_MSG_DELEGATE   : delegate method
 *
 * <xInit>     HB_OO_MSG_ACCESS     : \
 *             HB_OO_MSG_CLSACCESS  :   > Optional initializer for DATA
 *             HB_OO_MSG_DATA       :  /
 *             HB_OO_MSG_CLASSDATA  : /
 *             HB_OO_MSG_SUPER      : Superclass handle
 *             HB_OO_MSG_ASSIGN     : \ item type restriction in assignment not
 *             HB_OO_MSG_CLSASSIGN: :   empty character value where first letter
 *                                      is item type or item of a given value
 *
 * <uiScope>   HB_OO_CLSTP_EXPORTED        1 : default for data and method
 *             HB_OO_CLSTP_PROTECTED       2 : method or data protected
 *             HB_OO_CLSTP_HIDDEN          4 : method or data hidden
 *           * HB_OO_CLSTP_CTOR            8 : method constructor
 *             HB_OO_CLSTP_READONLY       16 : data read only
 *             HB_OO_CLSTP_SHARED         32 : (method or) data shared
 *           * HB_OO_CLSTP_CLASS          64 : message is class message not object
 *           * HB_OO_CLSTP_SUPER         128 : message is herited
 *             HB_OO_CLSTP_PERSIST       256 : message is persistent (PROPERTY)
 *             HB_OO_CLSTP_NONVIRTUAL    512 : Class method constructor
 *             HB_OO_CLSTP_OVERLOADED   1024 : Class method constructor
 *             HB_OO_CLSTP_SYNC         2048 : message synchronized by object or class mutex
 *
 * <xType>     HB_OO_MSG_PROPERTY      :  > optional item type restriction in assignment
 *             HB_OO_MSG_CLASSPROPERTY : /
 */

HB_FUNC( __CLSADDMSG )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
   const char * szMessage = hb_parc( 2 );

   if( szMessage && uiClass && uiClass <= s_uiClasses )
   {
      HB_USHORT nType     = ( HB_USHORT ) hb_parni( 4 );
      HB_USHORT uiScope   = ( HB_USHORT ) hb_parni( 6 );
      PHB_ITEM  pFunction = hb_param( 3, HB_IT_ANY );
      PHB_ITEM  pInit     = hb_param( 5, HB_IT_ANY );

      if( nType == HB_OO_MSG_DATA )
      {
         nType = szMessage[ 0 ] == '_' ? HB_OO_MSG_ASSIGN : HB_OO_MSG_ACCESS;
      }
      else if( nType == HB_OO_MSG_CLASSDATA )
      {
            nType = szMessage[ 0 ] == '_' ? HB_OO_MSG_CLSASSIGN :
                                            HB_OO_MSG_CLSACCESS;
      }
      /* to make xHarbour users happy ;-) */
      else if( nType == HB_OO_MSG_PROPERTY ||
               nType == HB_OO_MSG_CLASSPROPERTY )
      {
         PHB_ITEM pType = hb_param( 7, HB_IT_ANY );
         char szAssign[ HB_SYMBOL_NAME_LEN + 1 ];
         int iLen = ( int ) hb_parclen( 2 );
         if( iLen >= HB_SYMBOL_NAME_LEN )
            iLen = HB_SYMBOL_NAME_LEN - 1;
         szAssign[ 0 ] = '_';
         memcpy( szAssign + 1, szMessage, iLen );
         szAssign[ iLen + 1 ] = '\0';

         uiScope = ( uiScope | HB_OO_CLSTP_EXPORTED ) &
                  ~( HB_OO_CLSTP_PROTECTED | HB_OO_CLSTP_HIDDEN );
         if( nType == HB_OO_MSG_PROPERTY )
         {
            hb_clsAddMsg( uiClass, szAssign, HB_OO_MSG_ASSIGN,
                          ( HB_USHORT ) ( uiScope & ~HB_OO_CLSTP_PERSIST ),
                          pFunction, pType );
            nType = HB_OO_MSG_ACCESS;
         }
         else
         {
            hb_clsAddMsg( uiClass, szAssign, HB_OO_MSG_CLSASSIGN,
                          ( HB_USHORT ) ( uiScope & ~HB_OO_CLSTP_PERSIST ),
                          pFunction, pType );
            nType = HB_OO_MSG_CLSACCESS;
         }
      }

      hb_clsAddMsg( uiClass, szMessage, nType, uiScope, pFunction, pInit );
   }
}

/*
 * __clsNew( <szClassName>, <uiDatas>,
 *           [<pSuperArray>], [<pClassFunc>],
 *           [<fModuleFriendly>] ) -> <hClass>
 *
 * Create a new class
 *
 * <szClassName>     Name of the class
 * <uiDatas>         Number of DATAs in the class
 * <pSuperArray>     Optional array with handle(s) of superclass(es)
 * <pClassFunc>      Class function symbol, when NULL public function
 *                   with the same name as szClassName is used
 * <fModuleFriendly> when true all functions and classes from the same
 *                   module as pClassFunc are defined as friends
 */
static HB_USHORT hb_clsNew( const char * szClassName, HB_USHORT uiDatas,
                            PHB_ITEM pSuperArray, PHB_SYMB pClassFunc,
                            HB_BOOL fModuleFriendly )
{
   PCLASS pNewCls;
   PMETHOD pMethod;
   HB_USHORT ui, uiSuper, uiSuperCls;
   HB_USHORT * puiClassData = NULL, uiClassDataSize = 0;
   HB_BOOL fClsMutex = HB_FALSE;

   uiSuper  = ( HB_USHORT ) ( pSuperArray ? hb_arrayLen( pSuperArray ) : 0 );
   pClassFunc = hb_vmGetRealFuncSym( pClassFunc );

   pNewCls = ( PCLASS ) hb_xgrab( sizeof( CLASS ) );
   memset( pNewCls, 0, sizeof( CLASS ) );

   HB_CLASS_LOCK();

   if( s_uiClasses == s_uiClsSize )
   {
      s_uiClsSize += HB_CLASS_POOL_RESIZE;
      s_pClasses = ( PCLASS * ) hb_xrealloc( s_pClasses, sizeof( PCLASS ) *
                                             ( ( HB_SIZE ) s_uiClsSize + 1 ) );
   }
   pNewCls->uiClass = s_uiClasses + 1;
   s_pClasses[ pNewCls->uiClass ] = pNewCls;
   ++s_uiClasses;

   HB_CLASS_UNLOCK();

   pNewCls->szName = hb_strdup( szClassName );
   pNewCls->pClassSym = hb_dynsymGet( pNewCls->szName );
   if( ! pClassFunc )
      pClassFunc = hb_vmGetRealFuncSym( pNewCls->pClassSym->pSymbol );
   pNewCls->pClassFuncSym = pClassFunc;
   if( fModuleFriendly )
      hb_vmFindModuleSymbols( pClassFunc, &pNewCls->pFriendModule,
                                          &pNewCls->uiFriendModule );

   for( ui = 1; ui <= uiSuper; ++ui )
   {
      uiSuperCls = ( HB_USHORT ) hb_arrayGetNI( pSuperArray, ui );
      if( uiSuperCls && uiSuperCls < pNewCls->uiClass )
      {
         PCLASS pSprCls;

         pSprCls = s_pClasses[ uiSuperCls ];
         if( ! hb_clsInited( pNewCls ) ) /* This is the first superclass */
         {
            hb_clsCopyClass( pNewCls, pSprCls );
         }
         else if( ! hb_clsHasParentClass( pNewCls, uiSuperCls ) )
         {
            HB_SIZE n, nLimit;
            HB_USHORT nLenClsDatas;
            HB_USHORT uiCount;

            /* create class data translation tables */
            nLenClsDatas = ( HB_USHORT ) hb_itemSize( pSprCls->pClassDatas );
            if( nLenClsDatas )
            {
               if( nLenClsDatas > uiClassDataSize )
               {
                  puiClassData = ( HB_USHORT * ) hb_xrealloc( puiClassData,
                                             sizeof( HB_USHORT ) * nLenClsDatas );
                  uiClassDataSize = nLenClsDatas;
               }
               memset( puiClassData, 0, sizeof( HB_USHORT ) * nLenClsDatas );
            }

            /* Copy super class handles */
            for( uiCount = 0; uiCount < pSprCls->uiSuperClasses; ++uiCount )
               hb_clsDefineSuperClass( pNewCls, pSprCls->pSuperClasses[ uiCount ].uiClass, HB_TRUE );
            hb_clsDefineSuperClass( pNewCls, uiSuperCls, HB_TRUE );

            /* Copy instance area init data */
            if( pSprCls->uiInitDatas )
            {
               HB_USHORT u;
               for( u = 0; u < pSprCls->uiInitDatas; ++u )
               {
                  if( pSprCls->pInitData[ u ].uiType == HB_OO_MSG_DATA )
                  {
                     HB_USHORT uiCls = pSprCls->pInitData[ u ].uiSprClass;
                     hb_clsAddInitValue( pNewCls,
                                 pSprCls->pInitData[ u ].pInitValue,
                                 HB_OO_MSG_DATA,
                                 pSprCls->pInitData[ u ].uiData,
                                 hb_clsParentInstanceOffset( pNewCls, uiCls ),
                                 uiCls );
                  }
               }
            }

            /* Now working on other methods */
            nLimit = hb_clsMthNum( pSprCls );
            for( n = 0; n < nLimit; ++n )
            {
               if( pSprCls->pMethods[ n ].pMessage )
               {
                  pMethod = hb_clsAllocMsg( pNewCls, pSprCls->pMethods[ n ].pMessage );

                  /* update instance area offset */
                  if( pMethod->pMessage && pMethod->pFuncSym == &s___msgSuper )
                     pMethod->uiOffset = hb_clsParentInstanceOffset( pNewCls, pMethod->uiData );

                  /* Ok, this bucket is empty */
                  if( pMethod->pMessage == NULL ||
                      ( hb_clsCanClearMethod( pMethod, HB_FALSE ) &&
                        ( pMethod->pFuncSym == &s___msgVirtual ||
                          ( s_uiObjectClass != 0 &&
                            pMethod->uiSprClass == s_uiObjectClass ) ) ) )
                  {
                     if( pMethod->pMessage == NULL )
                        /* Now, we can increment the msg count */
                        pNewCls->uiMethods++;

                     memcpy( pMethod, pSprCls->pMethods + n, sizeof( METHOD ) );
                     if( ! hb_clsUpdateHiddenMessages( pMethod, pMethod, pNewCls ) )
                     {
                        PHB_SYMB pFuncSym = pMethod->pFuncSym;

                        if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
                           pFuncSym = pMethod->pRealSym;

                        if( pFuncSym == &s___msgSetClsData ||
                            pFuncSym == &s___msgGetClsData )
                        {
                           if( pMethod->uiData > nLenClsDatas )
                              hb_errInternal( HB_EI_CLSINVMETHOD, NULL, "__clsNew()", NULL );

                           if( puiClassData[ pMethod->uiData - 1 ] == 0 )
                           {
                              puiClassData[ pMethod->uiData - 1 ] = ( HB_USHORT )
                                          hb_arrayLen( pNewCls->pClassDatas ) + 1;
                              hb_arraySize( pNewCls->pClassDatas,
                                            puiClassData[ pMethod->uiData - 1 ] );
                           }
                           if( pMethod->uiOffset )
                           {
                              pMethod->uiOffset = hb_clsAddInitValue( pNewCls,
                                 pSprCls->pInitData[ pMethod->uiOffset - 1 ].pInitValue,
                                 HB_OO_MSG_CLASSDATA, puiClassData[ pMethod->uiData - 1 ],
                                 0, uiSuperCls );
                           }
                           pMethod->uiData = puiClassData[ pMethod->uiData - 1 ];
                        }
                        else if( pFuncSym == &s___msgSetData ||
                                 pFuncSym == &s___msgGetData )
                        {
                           pMethod->uiOffset = hb_clsParentInstanceOffset( pNewCls,
                                                            pMethod->uiSprClass );
                        }
                        pMethod->uiScope |= HB_OO_CLSTP_SUPER;
                     }
                  }
                  else
                  {
                     if( pSprCls->pMethods[ n ].uiScope &
                         ( HB_OO_CLSTP_OVERLOADED | HB_OO_CLSTP_NONVIRTUAL ) )
                        pMethod->uiScope |= HB_OO_CLSTP_OVERLOADED;

                     hb_clsUpdateHiddenMessages( pSprCls->pMethods + n, pMethod, pNewCls );
                  }
               }
            }
            if( pSprCls->fHasOnError )
               pNewCls->fHasOnError = HB_TRUE;
            if( pSprCls->fHasDestructor )
               pNewCls->fHasDestructor = HB_TRUE;
            pNewCls->nOpFlags |= pSprCls->nOpFlags;
            if( pSprCls->uiMutexOffset )
               pNewCls->uiMutexOffset = 1;
            if( pSprCls->pMutex )
               fClsMutex = HB_TRUE;
         }
      }
   }
   if( puiClassData )
      hb_xfree( puiClassData );

   if( ! hb_clsInited( pNewCls ) )
   {
      hb_clsDictInit( pNewCls, HASH_KEY );
      pNewCls->pClassDatas  = hb_itemArrayNew( 0 );
      pNewCls->pSharedDatas = hb_itemArrayNew( 0 );
      pNewCls->pInlines     = hb_itemArrayNew( 0 );
   }

   /* add self class casting */
   pNewCls->uiDataFirst = pNewCls->uiDatas;
   hb_clsDefineSuperClass( pNewCls, pNewCls->uiClass, HB_FALSE );
   pNewCls->uiDatas += uiDatas;
   if( pNewCls->uiMutexOffset )
      pNewCls->uiMutexOffset = pNewCls->uiDatas + 1;
   if( fClsMutex && ! pNewCls->pMutex )
      pNewCls->pMutex = hb_threadMutexCreate();

   return pNewCls->uiClass;
}

/*
 * __clsNew( <cClassName>, <nDatas>, [<ahSuper>], [<pClassFunc>], [<lModuleFriendly>] ) -> <hClass>
 *
 * Create a new class
 *
 * <cClassName> Name of the class
 * <nDatas>     Number of DATAs in the class
 * <ahSuper>    Optional array with handle(s) of superclass(es)
 * <pClassFunc> Class function symbol
 * <lModuleFriendly> when true all functions and classes from the same
 *                   module as pClassFunc are defined as friends
 */
HB_FUNC( __CLSNEW )
{
   const char * szClassName;
   PHB_ITEM pDatas, pSuperArray, pClassFunc, pModFriend;

   szClassName = hb_parc( 1 );

   pDatas = hb_param( 2, HB_IT_ANY );

   pSuperArray = hb_param( 3, HB_IT_ANY );
   if( pSuperArray && HB_IS_NIL( pSuperArray ) )
      pSuperArray = NULL;

   pClassFunc = hb_param( 4, HB_IT_ANY );
   if( pClassFunc && HB_IS_NIL( pClassFunc ) )
      pClassFunc = NULL;

   pModFriend = hb_param( 5, HB_IT_ANY );
   if( pModFriend && HB_IS_NIL( pModFriend ) )
      pModFriend = NULL;

   if( szClassName &&
       ( ! pDatas || HB_IS_NUMERIC( pDatas ) ) &&
       ( ! pSuperArray || HB_IS_ARRAY( pSuperArray ) ) &&
       ( ! pClassFunc || HB_IS_SYMBOL( pClassFunc ) ) &&
       ( ! pModFriend || HB_IS_LOGICAL( pModFriend ) ) )
   {
      HB_STACK_TLS_PRELOAD
      HB_USHORT uiClass;
      uiClass = hb_clsNew( szClassName, ( HB_USHORT ) hb_itemGetNI( pDatas ),
                           pSuperArray, hb_itemGetSymbol( pClassFunc ),
                           hb_itemGetL( pModFriend ) );
      hb_retni( uiClass );
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * __clsAddFriend( <hClass>, <sFuncSym> )
 *
 * Add friend function
 */
HB_FUNC( __CLSADDFRIEND )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];

      if( ! pClass->fLocked )
      {
         PHB_SYMB pSym = hb_vmGetRealFuncSym( hb_itemGetSymbol( hb_param( 2,
                                                         HB_IT_SYMBOL ) ) );
         if( pSym )
            hb_clsAddFriendSymbol( pClass, pSym );
      }
   }
}

/*
 * __clsDelMsg( <hClass>, <cMessage> )
 *
 * Delete message (only for INLINE and METHOD)
 *
 * <hClass>   class handle
 * <cMessage> message
 */
HB_FUNC( __CLSDELMSG )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString &&
       ! s_pClasses[ uiClass ]->fLocked )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
         hb_clsFreeMsg( s_pClasses[ uiClass ], pMsg );
   }
}


/*
 * hb_clsInst( <hClass> ) -> <pObjectItm>
 *
 * Create a new object from class definition <hClass>
 */
static PHB_ITEM hb_clsInst( HB_USHORT uiClass )
{
   PHB_ITEM pSelf = NULL;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS    pClass = s_pClasses[ uiClass ];
      HB_USHORT uiDatas = pClass->uiDatas;

      if( pClass->uiMutexOffset )
         ++uiDatas;
      pSelf = hb_itemNew( NULL );
      hb_arrayNew( pSelf, uiDatas );
      pSelf->item.asArray.value->uiClass = uiClass;

      if( pClass->uiMutexOffset )
      {
         PHB_ITEM pMutex = hb_threadMutexCreate();
         hb_arraySet( pSelf, pClass->uiMutexOffset, pMutex );
         hb_itemRelease( pMutex );
      }
      /* Initialise value if initialisation was requested */
      if( pClass->uiInitDatas )
      {
         PINITDATA pInitData = pClass->pInitData;
         HB_USHORT ui = pClass->uiInitDatas;
         PHB_ITEM pDestItm;

         do
         {
            if( pInitData->uiType == HB_OO_MSG_DATA )
               pDestItm = hb_arrayGetItemPtr( pSelf,
                                    pInitData->uiData + pInitData->uiOffset );
            else if( pInitData->uiType == HB_OO_MSG_CLASSDATA )
            {
               pDestItm = hb_arrayGetItemPtr( pClass->pClassDatas,
                                              pInitData->uiData );
               /* do not initialize it again */
               pInitData->uiType = HB_OO_MSG_INITIALIZED;
            }
            else
               pDestItm = NULL;

            if( pDestItm )
               hb_itemCloneTo( pDestItm, pInitData->pInitValue );

            ++pInitData;
         }
         while( --ui );
      }
   }

   return pSelf;
}

/*
 * __clsInst( <hClass> ) -> <oNewObject>
 *
 * Create a new object from class definition <hClass>
 */
HB_FUNC( __CLSINST )
{
   PHB_ITEM pSelf = hb_clsInst( ( HB_USHORT ) hb_parni( 1 ) );

   if( pSelf )
      hb_itemReturnRelease( pSelf );
}

/*
 * __clsLock( <hClass> )
 * Block farther class modifications
 */
HB_FUNC( __CLSLOCK )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
      s_pClasses[ uiClass ]->fLocked = HB_TRUE;
}

/*
 * __clsModMsg( <hClass>, <cMessage>, <pFunc> )
 *
 * Modify message (only for INLINE and METHOD)
 */
HB_FUNC( __CLSMODMSG )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString &&
       ! s_pClasses[ uiClass ]->fLocked )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS  pClass  = s_pClasses[ uiClass ];
         PMETHOD pMethod = hb_clsFindMsg( pClass, pMsg );

         if( pMethod )
         {
            PHB_SYMB pFuncSym = pMethod->pFuncSym;

            if( pFuncSym == &s___msgSetData || pFuncSym == &s___msgGetData )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DATA item", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSetClsData || pFuncSym == &s___msgGetClsData )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a CLASSDATA item", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSetShrData || pFuncSym == &s___msgGetShrData )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a SHARED DATA item", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSuper || pFuncSym == &s___msgRealClass )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a SUPER class casting", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgDestructor )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DESTRUCTOR method", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgOnError )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a ONERROR method", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgScopeErr )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a SCOPE ERROR method", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgPerform )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a PERFORM method", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgDelegate )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DELEGATE method", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSync )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a SYNC method", HB_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSyncClass )
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a CLASS SYNC method", HB_ERR_FUNCNAME, 0 );
            }
            else
            {
               PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK );
               if( pBlock )
               {
                  if( pFuncSym == &s___msgEvalInline &&
                      pMethod->uiSprClass == uiClass )
                  {
                     hb_arraySet( s_pClasses[ pMethod->uiSprClass ]->pInlines,
                                  pMethod->uiData, pBlock );
                  }
                  else
                  {
                     hb_arrayAdd( pClass->pInlines, pBlock );
                     pMethod->uiData = ( HB_USHORT ) hb_arrayLen( pClass->pInlines );
                  }
               }
               else
               {
                  pFuncSym = hb_objGetFuncSym( hb_param( 3, HB_IT_ANY ) );
                  if( pFuncSym )
                  {
                     pMethod->pFuncSym = pFuncSym;
                     pMethod->uiData = 0;
                  }
                  else
                     hb_errRT_BASE( EG_ARG, 3000, NULL, HB_ERR_FUNCNAME, 0 );
               }
            }
         }
      }
   }
}


/*
 * __objGetClsName( <hClass> | <oObj> ) -> <cClassName>
 *
 * Returns class name of <oObj> or <hClass>
 */
HB_FUNC( __OBJGETCLSNAME )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   HB_USHORT uiClass;

   if( pObject )
      uiClass = pObject->item.asArray.value->uiClass;
   else
      uiClass = ( HB_USHORT ) hb_parni( 1 );

   hb_retc( hb_clsName( uiClass ) );
}


/*
 * __objHasMsg( <oObj>, <cMsgName> | <sMsgName> ) -> <lRet>
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
HB_FUNC( __OBJHASMSG )
{
   PHB_DYNS pMessage = hb_objGetMsgSym( hb_param( 2, HB_IT_ANY ) );

   if( pMessage )
   {
      HB_STACK_TLS_PRELOAD
      hb_retl( hb_objHasMessage( hb_param( 1, HB_IT_ANY ), pMessage ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * __objHasMsgAssigned( <oObj>, <cMsgName> | <sMsgName> ) -> <lExists>
 *
 * checks if function exists and is not virtual
 */
HB_FUNC( __OBJHASMSGASSIGNED )
{
   PHB_DYNS pMessage = hb_objGetMsgSym( hb_param( 2, HB_IT_ANY ) );

   if( pMessage )
   {
      HB_STACK_TLS_PRELOAD
      PHB_SYMB pExecSym = hb_objGetMethod( hb_param( 1, HB_IT_ANY ),
                                           pMessage->pSymbol, NULL );
      hb_retl( pExecSym && pExecSym != &s___msgVirtual );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * __objSendMsg( <oObj>, <cMsgName> | <sMsgName>, <xArg,..> ) -> <xRet>
 *
 * Send a message to an object
 */
HB_FUNC( __OBJSENDMSG )
{
   PHB_DYNS pMessage = hb_objGetMsgSym( hb_param( 2, HB_IT_ANY ) );

   if( pMessage )
   {
      HB_STACK_TLS_PRELOAD
      HB_USHORT uiPCount = hb_pcount();
      HB_USHORT uiParam;

      hb_vmPushSymbol( pMessage->pSymbol );     /* Push message symbol */
      hb_vmPush( hb_param( 1, HB_IT_ANY ) );    /* Push object */

      for( uiParam = 3; uiParam <= uiPCount; ++uiParam )    /* Push arguments on stack */
         hb_vmPush( hb_stackItemFromBase( uiParam ) );

      hb_vmSend( ( HB_USHORT ) ( uiPCount - 2 ) );             /* Execute message */
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * __objClone( <oSource> ) -> <oNew>
 *
 * Clone an object. Note the similarity with aClone ;-)
 */
HB_FUNC( __OBJCLONE )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );

   if( pObject )
      hb_arrayCloneTo( hb_stackReturnItem(), pObject );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, 0 );
}

/*
 * __clsInstSuper( <cClassName> | <sClassFunc> ) -> <hClass>
 *
 * Instance super class and return class handle
 */
HB_FUNC( __CLSINSTSUPER )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING | HB_IT_SYMBOL );
   HB_USHORT uiClassH = 0, uiClass;
   PHB_SYMB pClassFuncSym = NULL;
   char szDesc[ 128 ];

   if( pItem )
   {
      if( HB_IS_SYMBOL( pItem ) )
         pClassFuncSym = hb_itemGetSymbol( pItem );
      else if( HB_IS_STRING( pItem ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFindName( hb_itemGetCPtr( pItem ) );
         if( pDynSym )
            pClassFuncSym = pDynSym->pSymbol;
      }
      pClassFuncSym = hb_vmGetRealFuncSym( pClassFuncSym );
   }

   if( pClassFuncSym )
   {
      uiClassH = hb_clsFindClassByFunc( pClassFuncSym );
      if( uiClassH == 0 )
      {
         hb_vmPushSymbol( pClassFuncSym );
         hb_vmPushNil();
         hb_vmProc( 0 ); /* Execute super class */

         if( hb_vmRequestQuery() == 0 )
         {
            PHB_ITEM pObject = hb_stackReturnItem();

            if( HB_IS_OBJECT( pObject ) )
            {
               uiClass = pObject->item.asArray.value->uiClass;

               if( s_pClasses[ uiClass ]->pClassFuncSym == pClassFuncSym )
                  uiClassH = uiClass;
               else
               {
                  uiClassH = hb_clsFindClassByFunc( pClassFuncSym );
                  /* still not found, try to send NEW() message */
                  if( uiClassH == 0 )
                  {
                     hb_vmPushSymbol( &s___msgNew );
                     hb_vmPush( pObject );
                     hb_vmSend( 0 );

                     pObject = hb_stackReturnItem();
                     if( HB_IS_OBJECT( pObject ) )
                     {
                        uiClass = pObject->item.asArray.value->uiClass;
                        if( s_pClasses[ uiClass ]->pClassFuncSym == pClassFuncSym )
                           uiClassH = uiClass;
                     }
                  }
               }
            }

            /* This disables destructor execution for this object */
            if( uiClassH && HB_IS_OBJECT( pObject ) )
               pObject->item.asArray.value->uiClass = 0;
            else if( hb_vmRequestQuery() == 0 )
            {
               hb_snprintf( szDesc, sizeof( szDesc ),
                            "Super class '%s' does not return an object",
                            pClassFuncSym->szName );
               hb_errRT_BASE( EG_ARG, 3002, "Super class does not return an object", HB_ERR_FUNCNAME, 0 );
            }
         }
      }
   }
   else
   {
      const char * pszName;

      pClassFuncSym = hb_itemGetSymbol( pItem );
      if( pClassFuncSym )
         pszName = pClassFuncSym->szName;
      else
         pszName = hb_itemGetCPtr( pItem );
      hb_snprintf( szDesc, sizeof( szDesc ),
                   "Cannot find super class '%s'", pszName );
      hb_errRT_BASE( EG_ARG, 3003, szDesc, HB_ERR_FUNCNAME, 0 );
   }

   hb_retni( uiClassH );
}

/*
 * __clsAssocType( <hClass>, <cType> ) -> <lOK>
 *
 * Associate class with given basic type
 */
HB_FUNC( __CLSASSOCTYPE )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
   PHB_ITEM pType = hb_param( 2, HB_IT_ANY );
   HB_BOOL fResult = HB_FALSE;

   if( uiClass && uiClass <= s_uiClasses && pType )
   {
      HB_TYPE nType = hb_clsGetItemType( pType, HB_IT_ANY );

      if( s_pClasses[ uiClass ]->uiDatas )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain instance variables", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      else if( nType != HB_IT_ANY )
      {
         switch( nType )
         {
            case HB_IT_ARRAY:
               s_uiArrayClass = uiClass;
               break;
            case HB_IT_BLOCK:
               s_uiBlockClass = uiClass;
               break;
            case HB_IT_STRING:
               s_uiCharacterClass = uiClass;
               break;
            case HB_IT_DATE:
               s_uiDateClass = uiClass;
               break;
            case HB_IT_TIMESTAMP:
               s_uiTimeStampClass = uiClass;
               break;
            case HB_IT_HASH:
               s_uiHashClass = uiClass;
               break;
            case HB_IT_LOGICAL:
               s_uiLogicalClass = uiClass;
               break;
            case HB_IT_NIL:
               s_uiNilClass = uiClass;
               break;
            case HB_IT_NUMERIC:
               s_uiNumericClass = uiClass;
               break;
            case HB_IT_SYMBOL:
               s_uiSymbolClass = uiClass;
               break;
            case HB_IT_POINTER:
               s_uiPointerClass = uiClass;
               break;
            default:
               uiClass = 0;
         }
         fResult = uiClass != 0;
      }
   }

   hb_retl( fResult );
}

/*
 * __clsCntClasses() -> <nCount>
 *
 * Return number of classes
 */
HB_FUNC( __CLSCNTCLASSES )
{
   HB_STACK_TLS_PRELOAD
   hb_retni( ( int ) s_uiClasses );
}

/*
 * __cls_CntClsData( <hClass> ) -> <nCount>
 *
 * Return number of class datas
 */
HB_FUNC( __CLS_CNTCLSDATA )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );

   hb_retni( uiClass && uiClass <= s_uiClasses ?
                  ( HB_USHORT ) hb_arrayLen( s_pClasses[ uiClass ]->pClassDatas ) : 0 );
}

/*
 * __cls_CntShrData( <hClass> ) -> <nCount>
 *
 * Return number of class datas
 */
HB_FUNC( __CLS_CNTSHRDATA )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );

   hb_retni( uiClass && uiClass <= s_uiClasses ?
                  ( HB_USHORT ) hb_arrayLen( s_pClasses[ uiClass ]->pSharedDatas ) : 0 );
}

/*
 * __cls_CntData( <hClass> ) -> <nCount>
 *
 * Return number of datas
 */
HB_FUNC( __CLS_CNTDATA )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );

   hb_retni( uiClass && uiClass <= s_uiClasses ?
             s_pClasses[ uiClass ]->uiDatas : 0 );
}

/*
 * __cls_DecData( <hClass> ) -> <nCount>
 *
 * Decrease number of datas and return new value
 */
HB_FUNC( __CLS_DECDATA )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses &&
       s_pClasses[ uiClass ]->uiDatas > s_pClasses[ uiClass ]->uiDataFirst )
   {
      if( ! s_pClasses[ uiClass ]->fLocked )
         s_pClasses[ uiClass ]->uiDatas--;
      hb_retni( s_pClasses[ uiClass ]->uiDatas - s_pClasses[ uiClass ]->uiDataFirst );
   }
   else
      hb_retni( 0 );
}

/*
 * __cls_IncData( <hClass> ) -> <nCount>
 * Increase number of datas and return offset to new value
 */
HB_FUNC( __CLS_INCDATA )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      if( ! s_pClasses[ uiClass ]->fLocked )
         s_pClasses[ uiClass ]->uiDatas++;
      hb_retni( s_pClasses[ uiClass ]->uiDatas - s_pClasses[ uiClass ]->uiDataFirst );
   }
   else
      hb_retni( 0 );
}

/* NOTE: Undocumented Clipper function */
/* see for parameter compatibility with Clipper. */

HB_FUNC_TRANSLATE( __CLASSNEW, __CLSNEW )

/* NOTE: Undocumented Clipper function */

HB_FUNC_TRANSLATE( __CLASSINSTANCE, __CLSINST )

/* NOTE: Undocumented Clipper function */

HB_FUNC_TRANSLATE( __CLASSADD, __CLSADDMSG )

/* NOTE: Undocumented Clipper function */

HB_FUNC( __CLASSNAME )
{
   HB_STACK_TLS_PRELOAD
   hb_retc( hb_clsName( ( HB_USHORT ) hb_parni( 1 ) ) );
}

/* NOTE: Undocumented Clipper function */

HB_FUNC( __CLASSSEL )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod = pClass->pMethods;
      HB_SIZE nLimit = hb_clsMthNum( pClass ), nPos = 0;

      hb_arrayNew( pReturn, pClass->uiMethods ); /* Create a transfer array */

      do
      {
         if( pMethod->pMessage )    /* Hash Entry used ? */
            hb_arraySetC( pReturn, ++nPos, pMethod->pMessage->pSymbol->szName );
         ++pMethod;
      }
      while( --nLimit );

      if( nPos < ( HB_SIZE ) pClass->uiMethods )
         hb_arraySize( pReturn, nPos );
   }

   hb_itemReturnRelease( pReturn );
}

/* to be used from Classes ERROR HANDLER method */
HB_FUNC( __GETMESSAGE )
{
   HB_STACK_TLS_PRELOAD
   hb_retc( hb_stackItem ( hb_stackBaseItem()->item.asSymbol.stackstate->nBaseItem )->item.asSymbol.value->szName );
}

/* __clsParent( <hClass>, <cParentClass> ) -> <lIsParent>
 * Checks if <cParentClass> is parent of <hClass>
 */
HB_FUNC( __CLSPARENT )
{
   HB_STACK_TLS_PRELOAD
   const char * szParentName = hb_parc( 2 );

   hb_retl( szParentName &&
            hb_clsIsParent( ( HB_USHORT ) hb_parni( 1 ), szParentName ) );
}

/* __sender() -> <obj> | NIL
 * returns sender object
 */
HB_FUNC( __SENDER )
{
   HB_STACK_TLS_PRELOAD
   HB_ISIZ nOffset = hb_stackBaseProcOffset( 2 );

   if( nOffset > 0 )
   {
      PHB_ITEM pSelf = hb_stackItem( nOffset + 1 );

      /* Is it inline method? */
      if( nOffset > 0 && HB_IS_BLOCK( pSelf ) &&
          hb_stackItem( nOffset )->item.asSymbol.value == &hb_symEval )
      {
         pSelf = hb_stackItem( hb_stackItem( nOffset )->
                               item.asSymbol.stackstate->nBaseItem + 1 );
      }

      if( HB_IS_OBJECT( pSelf ) )
      {
         hb_itemReturn( pSelf );
      }
   }
}

HB_FUNC( __CLSSYNCSIGNAL )
{
#if defined( HB_MT_VM )
   hb_threadMutexSyncSignal( hb_param( 1, HB_IT_ANY ) );
#endif /* HB_MT_VM */
}

HB_FUNC( __CLSSYNCWAIT )
{
#if defined( HB_MT_VM )
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pMutex = NULL;
   HB_ULONG ulMilliSec = HB_THREAD_INFINITE_WAIT;
   HB_ISIZ nOffset = hb_stackBaseProcOffset( 2 );

   if( nOffset > 0 )
   {
      PHB_ITEM pBase = hb_stackItem( nOffset );
      PHB_STACK_STATE pStack = pBase->item.asSymbol.stackstate;
      HB_USHORT uiClass = pStack->uiClass;

      if( uiClass && uiClass <= s_uiClasses )
      {
         PCLASS pClass = s_pClasses[ uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         if( pMethod->pFuncSym == &s___msgSync )
         {
            PHB_ITEM pSelf = hb_stackItem( nOffset + 1 );

            /* Is it inline method? */
            if( HB_IS_BLOCK( pSelf ) && pBase->item.asSymbol.value == &hb_symEval )
               pSelf = hb_stackItem( pBase->item.asSymbol.stackstate->nBaseItem + 1 );

            uiClass = hb_objGetClass( pSelf );
            if( uiClass && uiClass <= s_uiClasses )
               pMutex = hb_arrayGetItemPtr( pSelf, s_pClasses[ uiClass ]->uiMutexOffset );
         }
         else if( pMethod->pFuncSym == &s___msgSyncClass )
            pMutex = pClass->pMutex;
      }
   }

   if( HB_ISNUM( 2 ) )
   {
      double dTimeOut = hb_parnd( 2 );
      if( dTimeOut > 0 )
         ulMilliSec = ( HB_ULONG ) ( dTimeOut * 10 );
   }

   hb_retl( hb_threadMutexSyncWait( hb_param( 1, HB_IT_ANY ), ulMilliSec, pMutex ) );
#endif /* HB_MT_VM */
}

/*
 * __classH( <obj> ) -> <hClass>
 *
 * Returns class handle of <obj>
 */
HB_FUNC( __CLASSH )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject = hb_param( 1, HB_IT_ANY );

   hb_retni( pObject ? hb_objGetClassH( pObject ) : 0 );
}

/* ================================================ */

/*
 * <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
HB_FUNC_STATIC( msgClassH )
{
   HB_STACK_TLS_PRELOAD
   hb_retni( hb_stackBaseItem()->item.asSymbol.stackstate->uiClass );
}


/*
 * <cClassName> := <obj>:ClassName()
 *
 * Return class name of <obj>. Can also be used for all types.
 */
HB_FUNC_STATIC( msgClassName )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;

   if( uiClass )
      hb_retc( s_pClasses[ uiClass ]->szName );
   else
      hb_retc( hb_objGetClsName( hb_stackSelfItem() ) );
}


static int hb_methodType( PMETHOD pMethod )
{
   PHB_SYMB pFuncSym = pMethod->pFuncSym;

   if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
      pFuncSym = pMethod->pRealSym;

   if     ( pFuncSym == &s___msgSetClsData ||
            pFuncSym == &s___msgGetClsData ||
            pFuncSym == &s___msgSetShrData ||
            pFuncSym == &s___msgGetShrData )
      return HB_OO_MSG_CLASSDATA;

   else if( pFuncSym == &s___msgSetData ||
            pFuncSym == &s___msgGetData )
      return HB_OO_MSG_DATA;

   else if( pFuncSym == &s___msgEvalInline )
      return HB_OO_MSG_INLINE;

   else if( pFuncSym == &s___msgVirtual )
      return HB_OO_MSG_VIRTUAL;

   else if( pFuncSym == &s___msgSuper )
      return HB_OO_MSG_SUPER;

   else if( pFuncSym == &s___msgRealClass )
      return HB_OO_MSG_REALCLASS;

   else if( pFuncSym == &s___msgDelegate )
      return HB_OO_MSG_DELEGATE;

   else if( pFuncSym == &s___msgPerform )
      return HB_OO_MSG_PERFORM;

   else if( pMethod->pMessage == s___msgOnError.pDynSym )
      return HB_OO_MSG_ONERROR;

   else if( pMethod->pMessage == s___msgDestructor.pDynSym )
      return HB_OO_MSG_DESTRUCTOR;

   else
      return HB_OO_MSG_METHOD;
}

/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
HB_FUNC_STATIC( msgClassSel )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiClass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PHB_ITEM pReturn, pItem;
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod = pClass->pMethods;
      HB_SIZE nLimit = hb_clsMthNum( pClass ), nPos = 0;
      HB_USHORT nParam, nScope;
      HB_BOOL lFull;

      nParam = ( HB_USHORT ) hb_parnidef( 1, HB_MSGLISTALL );
      nScope = ( HB_USHORT ) hb_parni( 2 );
      lFull = hb_parl( 3 );
      pReturn = hb_itemArrayNew( pClass->uiMethods );

      do
      {
         if( pMethod->pMessage )  /* Hash Entry used ? */
         {
            if( ( nParam == HB_MSGLISTALL ) ||
                ( nParam == HB_MSGLISTCLASS &&
                  hb_methodType( pMethod ) == HB_OO_MSG_CLASSDATA ) ||
                ( nParam == HB_MSGLISTPURE &&
                  hb_methodType( pMethod ) != HB_OO_MSG_CLASSDATA ) )
            {
               if( nScope == 0 || ( pMethod->uiScope & nScope ) != 0 )
               {
                  if( lFull )
                  {
                     pItem = hb_arrayGetItemPtr( pReturn, ++nPos );
                     hb_arrayNew( pItem, 4 );
                     hb_arraySetC( pItem, HB_OO_DATA_SYMBOL,
                                   pMethod->pMessage->pSymbol->szName );
                     hb_arraySetNI( pItem, HB_OO_DATA_TYPE, hb_methodType( pMethod ) );
                     hb_arraySetNI( pItem, HB_OO_DATA_SCOPE, pMethod->uiScope );
                  }
                  else
                     hb_arraySetC( pReturn, ++nPos,
                                   pMethod->pMessage->pSymbol->szName );
               }
            }
         }
         ++pMethod;
      }
      while( --nLimit && nPos < ( HB_SIZE ) pClass->uiMethods );

      if( nPos < ( HB_SIZE ) pClass->uiMethods )
         hb_arraySize( pReturn, nPos );
      hb_itemReturnRelease( pReturn );
   }
}

#if 0

/*
 * __msgClass()
 *
 * Internal function to return Self at Self:Class call (classy compatibility)
 */
HB_FUNC_STATIC( msgClass )
{
   hb_itemReturnForward( hb_stackSelfItem() );
}

/* Added by JfL&RaC
 * <logical> <= <obj>:IsDerivedFrom( xParam )
 *
 * Return true if <obj> is derived from xParam.
 * xParam can be either an obj or a classname
 */
HB_FUNC_STATIC( msgClassParent )
{
   HB_BOOL fHasParent = HB_FALSE;
   PHB_ITEM pItem;
   HB_USHORT uiClass;

   uiClass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItemParam = hb_param( 1, HB_IT_ANY );

   if( pItemParam && uiClass && uiClass <= s_uiClasses )
   {
      if( HB_IS_OBJECT( pItemParam ) )
         fHasParent = hb_clsHasParentClass( s_pClasses[ uiClass ],
                                   pItemParam->item.asArray.value->uiClass );
      else if( HB_IS_STRING( pItemParam ) )
         fHasParent = hb_clsIsParent( uiClass, hb_parc( pItemParam ) )
   }

   hb_retl( fHasParent );
}

#endif


/*
 * __msgEvalInline()
 *
 * Internal function executed for inline methods
 */
HB_FUNC_STATIC( msgEvalInline )
{
   HB_STACK_TLS_PRELOAD
   PHB_STACK_STATE pStack = hb_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass   = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   HB_USHORT uiPCount = hb_pcount(), uiParam;
   PHB_ITEM pBlock;

   hb_vmPushEvalSym();

   hb_vmPush( hb_arrayGetItemPtr( s_pClasses[ pMethod->uiSprClass ]->pInlines,
                                  pMethod->uiData ) );
   pBlock = hb_stackItemFromTop( -1 );    /* Push block */
   pBlock->item.asBlock.hclass = pStack->uiClass;
   pBlock->item.asBlock.method = pStack->uiMethod;

   hb_vmPush( hb_stackSelfItem() );       /* Push self as first argument */

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_vmPush( hb_stackItemFromBase( uiParam ) );
   }

   hb_vmEval( ( HB_USHORT ) ( uiPCount + 1 ) );
}

HB_FUNC_STATIC( msgPerform )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );
   HB_USHORT uiPCount = hb_pcount(), uiParam;
   PHB_SYMB pSym = NULL;

   if( pItem )
   {
      if( HB_IS_SYMBOL( pItem ) )
         pSym = pItem->item.asSymbol.value;

      else if( HB_IS_OBJECT( pItem ) &&
               s_pClasses[ pItem->item.asArray.value->uiClass ]->pClassSym ==
               s___msgSymbol.pDynSym )
      {
         /* Dirty hack */
         pItem = hb_arrayGetItemPtr( pItem, 1 );
         if( pItem && HB_IS_SYMBOL( pItem ) )
            pSym = pItem->item.asSymbol.value;
      }

      if( pSym )
      {
         hb_vmPushSymbol( pSym );
         hb_vmPush( hb_stackSelfItem() );

         for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
         {
            hb_vmPush( hb_stackItemFromBase( uiParam ) );
         }
         hb_vmSend( ( HB_USHORT ) ( uiPCount - 1 ) );
      }
   }
}

HB_FUNC_STATIC( msgDelegate )
{
   HB_STACK_TLS_PRELOAD
   PHB_STACK_STATE pStack = hb_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass   = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   PHB_SYMB pExecSym = pClass->pMethods[ pMethod->uiData ].pFuncSym;

   if( pExecSym )
      HB_VM_FUNCUNREF( pExecSym );
   if( pExecSym && HB_VM_ISFUNC( pExecSym ) )
   {
      HB_VM_EXECUTE( pExecSym );
   }
   else
   {
      HB_FUNC_EXEC( msgNoMethod );
   }
}

HB_FUNC_STATIC( msgSync )
{
   HB_STACK_TLS_PRELOAD
   PHB_STACK_STATE pStack = hb_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   PHB_SYMB pExecSym = pMethod->pRealSym;

   if( pExecSym )
      HB_VM_FUNCUNREF( pExecSym );
   if( pExecSym && HB_VM_ISFUNC( pExecSym ) )
   {
      PHB_ITEM pObject = hb_stackSelfItem();
      HB_USHORT uiClass = hb_objGetClass( pObject );
      PHB_ITEM pMutex = NULL;

      if( uiClass && uiClass <= s_uiClasses )
         pMutex = hb_arrayGetItemPtr( pObject, s_pClasses[ uiClass ]->uiMutexOffset );

      if( ! pMutex || hb_threadMutexLock( pMutex ) )
      {
         HB_VM_EXECUTE( pExecSym );
         if( pMutex )
            hb_threadMutexUnlock( pMutex );
      }
   }
   else
   {
      HB_FUNC_EXEC( msgNoMethod );
   }
}

HB_FUNC_STATIC( msgSyncClass )
{
   HB_STACK_TLS_PRELOAD
   PHB_STACK_STATE pStack = hb_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   PHB_SYMB pExecSym = pMethod->pRealSym;

   if( pExecSym )
      HB_VM_FUNCUNREF( pExecSym );
   if( pExecSym && HB_VM_ISFUNC( pExecSym ) )
   {
      if( ! pClass->pMutex || hb_threadMutexLock( pClass->pMutex ) )
      {
         HB_VM_EXECUTE( pExecSym );
         if( pClass->pMutex )
            hb_threadMutexUnlock( pClass->pMutex );
      }
   }
   else
   {
      HB_FUNC_EXEC( msgNoMethod );
   }
}

/*
 * __msgNoMethod()
 *
 * Internal function for generating error when not existing message is sent
 */
HB_FUNC_STATIC( msgNoMethod )
{
   HB_STACK_TLS_PRELOAD
   PHB_SYMB pSym = hb_itemGetSymbol( hb_stackBaseItem() );

#if 1  /* Clipper compatible error message */
   if( pSym->szName[ 0 ] == '_' )
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1, HB_ERR_ARGS_SELFPARAMS );
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, pSym->szName, HB_ERR_ARGS_SELFPARAMS );
#else
   char szDesc[ 40 + HB_SYMBOL_NAME_LEN ];

   if( pSym->szName[ 0 ] == '_' )
   {
      hb_snprintf( szDesc, sizeof( szDesc ), "Class: '%s' has no property", hb_objGetClsName( hb_stackSelfItem() ) );
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, szDesc, pSym->szName + 1, HB_ERR_ARGS_BASEPARAMS );
   }
   else
   {
      hb_snprintf( szDesc, sizeof( szDesc ), "Class: '%s' has no exported method", hb_objGetClsName( hb_stackSelfItem() ) );
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, szDesc, pSym->szName, HB_ERR_ARGS_BASEPARAMS );
   }
#endif
}

/*
 * __msgScopeErr()
 *
 * Internal function for generating error when not existing message is sent
 */
HB_FUNC_STATIC( msgScopeErr )
{
   HB_STACK_TLS_PRELOAD
   char * pszProcName;
   PHB_ITEM pObject = hb_stackSelfItem();
   PMETHOD pMethod = s_pClasses[
      hb_stackBaseItem()->item.asSymbol.stackstate->uiClass ]->pMethods +
      hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   pszProcName = hb_xstrcpy( NULL, hb_objGetClsName( pObject ), ":",
                             pMethod->pMessage->pSymbol->szName, NULL );
   if( pMethod->uiScope & HB_OO_CLSTP_HIDDEN )
      hb_errRT_BASE( EG_NOMETHOD, 41, "Scope violation (hidden)", pszProcName, 0 );
   else
      hb_errRT_BASE( EG_NOMETHOD, 42, "Scope violation (protected)", pszProcName, 0 );
   hb_xfree( pszProcName );
}

HB_FUNC_STATIC( msgTypeErr )
{
   HB_STACK_TLS_PRELOAD
   char * pszProcName;
   PHB_ITEM pObject = hb_stackSelfItem();
   PMETHOD pMethod = s_pClasses[
      hb_stackBaseItem()->item.asSymbol.stackstate->uiClass ]->pMethods +
      hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   pszProcName = hb_xstrcpy( NULL, hb_objGetClsName( pObject ), ":",
                             pMethod->pMessage->pSymbol->szName + 1, NULL );
   hb_errRT_BASE( EG_NOMETHOD, 44, "Assigned value is wrong class", pszProcName, HB_ERR_ARGS_BASEPARAMS );
   hb_xfree( pszProcName );
}

/*
 * __msgSuper()
 *
 * Internal function to return a superobject
 */
HB_FUNC_STATIC( msgSuper )
{
   HB_STACK_TLS_PRELOAD
   PHB_STACK_STATE pStack = hb_stackBaseItem()->item.asSymbol.stackstate;

   hb_clsMakeSuperObject( hb_stackReturnItem(), hb_stackSelfItem(),
      s_pClasses[ pStack->uiClass ]->pMethods[ pStack->uiMethod ].uiData );
}

/*
 * __msgRealClass()
 *
 * Internal function to return a superobject of class where the method was
 * defined
 */
HB_FUNC_STATIC( msgRealClass )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject = hb_stackSelfItem();
   HB_USHORT uiClass = hb_clsSenderMethodClass();
   HB_USHORT uiCurClass = hb_objGetClassH( pObject );

   if( uiClass && uiClass != uiCurClass &&
       hb_clsSenderObjectClass() == uiCurClass )
   {
      hb_clsMakeSuperObject( hb_stackReturnItem(), pObject, uiClass );
   }
   else
   {
      hb_itemReturnForward( pObject );
   }
}

/*
 * __msgGetClsData()
 *
 * Internal function to return a CLASSDATA
 */
HB_FUNC_STATIC( msgGetClsData )
{
   HB_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   hb_arrayGet( pClass->pClassDatas, pMethod->uiData, hb_stackReturnItem() );
}


/*
 * __msgSetClsData()
 *
 * Internal function to set a CLASSDATA
 */
HB_FUNC_STATIC( msgSetClsData )
{
   HB_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
   PHB_ITEM pReturn = hb_param( 1, HB_IT_ANY );

   if( ! pReturn )
      hb_arrayGet( pClass->pClassDatas, pMethod->uiData, hb_stackReturnItem() );

   else
   {
      if( pMethod->itemType &&
          ! ( pMethod->itemType & HB_ITEM_TYPERAW( pReturn ) ) )
      {
         if( pMethod->itemType == HB_IT_NUMINT && HB_IS_NUMERIC( pReturn ) )
            hb_itemPutNInt( pReturn, hb_itemGetNInt( pReturn ) );
         else
         {
            ( s___msgTypeErr.value.pFunPtr )();
            return;
         }
      }

      hb_arraySet( pClass->pClassDatas, pMethod->uiData, pReturn );
      hb_itemReturn( pReturn );
   }
}

/*
 * __msgGetShrData()
 *
 * Internal function to return a SHAREDDATA
 */
HB_FUNC_STATIC( msgGetShrData )
{
   HB_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   hb_arrayGet( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                pMethod->uiData, hb_stackReturnItem() );
}

/*
 * __msgSetShrData()
 *
 * Internal function to set a SHAREDDATA
 */
HB_FUNC_STATIC( msgSetShrData )
{
   HB_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
   PHB_ITEM pReturn = hb_param( 1, HB_IT_ANY );

   if( ! pReturn )
      hb_arrayGet( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                   pMethod->uiData, hb_stackReturnItem() );
   else
   {
      if( pMethod->itemType &&
          ! ( pMethod->itemType & HB_ITEM_TYPERAW( pReturn ) ) )
      {
         if( pMethod->itemType == HB_IT_NUMINT && HB_IS_NUMERIC( pReturn ) )
            hb_itemPutNInt( pReturn, hb_itemGetNInt( pReturn ) );
         else
         {
            ( s___msgTypeErr.value.pFunPtr )();
            return;
         }
      }

      hb_arraySet( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                   pMethod->uiData, pReturn );
      hb_itemReturn( pReturn );
   }
}

/*
 * __msgGetData()
 *
 * Internal function to return a DATA
 */
HB_FUNC_STATIC( msgGetData )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject  = hb_stackSelfItem();

   if( HB_IS_ARRAY( pObject ) )
   {
      HB_USHORT uiObjClass = pObject->item.asArray.value->uiClass;
      HB_USHORT uiClass    = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
      PCLASS pClass        = s_pClasses[ uiClass ];
      PMETHOD pMethod      = pClass->pMethods +
                             hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
      HB_SIZE nIndex       = pMethod->uiData;

      if( uiClass != uiObjClass )
      {
         nIndex += hb_clsParentInstanceOffset( s_pClasses[ uiObjClass ],
                                               pMethod->uiSprClass );
      }
      else
      {
         nIndex += pMethod->uiOffset;
      }

      hb_arrayGet( pObject, nIndex, hb_stackReturnItem() );
   }
}

/*
 * __msgSetData()
 *
 * Internal function to set a DATA
 */
HB_FUNC_STATIC( msgSetData )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject  = hb_stackSelfItem();

   if( HB_IS_ARRAY( pObject ) )
   {
      PHB_ITEM pReturn     = hb_param( 1, HB_IT_ANY );
      HB_USHORT uiObjClass = pObject->item.asArray.value->uiClass;
      HB_USHORT uiClass    = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
      PCLASS pClass        = s_pClasses[ uiClass ];
      PMETHOD pMethod      = pClass->pMethods +
                             hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
      HB_SIZE nIndex       = pMethod->uiData;

      if( uiClass != uiObjClass )
      {
         nIndex += hb_clsParentInstanceOffset( s_pClasses[ uiObjClass ],
                                               pMethod->uiSprClass );
      }
      else
      {
         nIndex += pMethod->uiOffset;
      }

      if( ! pReturn )
         hb_arrayGet( pObject, nIndex, hb_stackReturnItem() );

      else
      {
         if( pMethod->itemType &&
             ! ( pMethod->itemType & HB_ITEM_TYPERAW( pReturn ) ) )
         {
            if( pMethod->itemType == HB_IT_NUMINT && HB_IS_NUMERIC( pReturn ) )
               hb_itemPutNInt( pReturn, hb_itemGetNInt( pReturn ) );
            else
            {
               ( s___msgTypeErr.value.pFunPtr )();
               return;
            }
         }

         /* will arise only if the class has been modified after first instance */
         if( nIndex > hb_arrayLen( pObject ) ) /* Resize needed ? */
            hb_arraySize( pObject, nIndex );   /* Make large enough */
         hb_arraySet( pObject, nIndex, pReturn );
         hb_itemReturn( pReturn );
      }
   }
}

/* No comment :-) */
HB_FUNC_STATIC( msgVirtual )
{
   /* hb_ret(); */ /* NOTE: It's safe to comment this out */
}

HB_FUNC_STATIC( msgNull )
{
}

#ifndef HB_NO_PROFILER
void hb_mthAddTime( HB_ULONG ulClockTicks )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject = hb_stackSelfItem();
   PCLASS pClass = s_pClasses[ hb_objGetClassH( pObject ) ];
   HB_USHORT uiMethod = hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   if( pClass && uiMethod < hb_clsMthNum( pClass ) )
   {
      PMETHOD pMethod = pClass->pMethods + uiMethod;
      pMethod->ulCalls++;
      pMethod->ulTime += ulClockTicks;
      return;
   }

   if( HB_IS_BLOCK( pObject ) )
   {
      PHB_SYMB pSym = hb_stackBaseItem()->item.asSymbol.value;

      if( pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym )
      {
         pSym->pDynSym->ulCalls++;
         if( --pSym->pDynSym->ulRecurse == 0 )
            pSym->pDynSym->ulTime += ulClockTicks;
      }
   }
}
#endif

/* __getMsgPrf( <hClass>, <cMsg> ) -> <aMethodInfo> { { <nTimes>, <nTime> }, ... }
 */
HB_FUNC( __GETMSGPRF ) /* profiler: returns a method called and consumed times */
{
   HB_STACK_TLS_PRELOAD
#ifndef HB_NO_PROFILER
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
   char * cMsg = hb_parc( 2 );

   hb_reta( 2 );
   if( uiClass && uiClass <= s_uiClasses && cMsg && *cMsg )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( cMsg );

      if( pMsg )
      {
         PMETHOD pMethod = hb_clsFindMsg( s_pClasses[ uiClass ], pMsg );

         if( pMethod )
         {
            hb_storvnl( pMethod->ulCalls, -1, 1 );
            hb_storvnl( pMethod->ulTime, -1, 2 );
            return;
         }
      }
   }
#else
   hb_reta( 2 );
#endif
   hb_storvnl( 0, -1, 1 );
   hb_storvnl( 0, -1, 2 );
}

typedef struct
{
   PMETHOD   pMethod;
   HB_USHORT uiClass;
   HB_USHORT uiStatus;
}
HB_IVARINFO, * PHB_IVARINFO;

static PHB_ITEM hb_objGetIVars( PHB_ITEM pObject,
                                HB_USHORT uiScope, HB_BOOL fChanged )
{
   PHB_IVARINFO pIndex, pInfo;
   PCLASS pClass;
   PMETHOD pMethod;
   PHB_ITEM pReturn, pItem;
   HB_SIZE nLimit, nLen, nCount, nSize, nIndex, nOffset;
   HB_USHORT uiClass, uiSuperClasses;

   if( ! pObject || ! HB_IS_OBJECT( pObject ) )
      return NULL;

   uiClass = pObject->item.asArray.value->uiClass;
   pClass = s_pClasses[ uiClass ];
   nLen = nCount = hb_arrayLen( pObject );
   nSize = 0;
   pIndex = ( PHB_IVARINFO ) hb_xgrab( nLen * sizeof( HB_IVARINFO ) );
   memset( pIndex, 0, nLen * sizeof( HB_IVARINFO ) );

   if( fChanged && pClass->uiInitDatas )
   {
      PINITDATA pInitData = pClass->pInitData;

      nLimit = pClass->uiInitDatas;
      do
      {
         if( pInitData->uiType == HB_OO_MSG_DATA )
         {
            nIndex = pInitData->uiData + pInitData->uiOffset;
            pItem = hb_arrayGetItemPtr( pObject, nIndex );
            if( pItem )
            {
               if( hb_itemEqual( pItem, pInitData->pInitValue ) )
               {
                  pIndex[ nIndex - 1 ].uiStatus = 3;
                  --nCount;
               }
               else
                  pIndex[ nIndex - 1 ].uiStatus = 1;
            }
         }
         ++pInitData;
      }
      while( --nLimit );
   }

   uiSuperClasses = pClass->uiSuperClasses;
   nOffset = 0;
   nLimit = hb_clsMthNum( pClass );
   pMethod = pClass->pMethods;
   while( nCount && nLimit )
   {
      if( pMethod->pMessage &&
          ( uiScope == 0 || ( pMethod->uiScope & uiScope ) != 0 ) &&
          ( uiClass == pClass->uiClass || uiClass == pMethod->uiSprClass ) )
      {
         PHB_SYMB pFuncSym = pMethod->pFuncSym;

         if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
            pFuncSym = pMethod->pRealSym;

         if( pFuncSym == &s___msgGetData || pFuncSym == &s___msgSetData )
         {
            HB_USHORT uiStatus = pFuncSym == &s___msgGetData ? 3 : 2;

            nIndex = ( uiClass == pClass->uiClass ?
                     ( HB_SIZE ) pMethod->uiOffset : nOffset ) + pMethod->uiData;
            if( nIndex == 0 || nIndex > nLen )
               hb_errInternal( HB_EI_CLSINVMETHOD, NULL, "__objGetIVars()", NULL );

            pInfo = &pIndex[ nIndex - 1 ];
            if( pInfo->uiStatus < uiStatus )
            {
               pItem = hb_arrayGetItemPtr( pObject, nIndex );
               if( ! pItem || ( pInfo->uiStatus == 0 && HB_IS_NIL( pItem ) ) )
                  uiStatus = 3;
               else
               {
                  if( ! pInfo->pMethod )
                     ++nSize;
                  pInfo->pMethod = pMethod;
                  pInfo->uiClass = uiClass;
               }
               pInfo->uiStatus = uiStatus;
               if( uiStatus == 3 )
                  --nCount;
            }
         }
      }
      ++pMethod;
      if( --nLimit == 0 )
      {
         if( uiSuperClasses-- )
         {
            if( uiClass == pClass->pSuperClasses[ uiSuperClasses ].uiClass )
               if( uiSuperClasses-- == 0 )
                  break;
            uiClass = pClass->pSuperClasses[ uiSuperClasses ].uiClass;
            nOffset = pClass->pSuperClasses[ uiSuperClasses ].uiOffset;
            nLimit = hb_clsMthNum( s_pClasses[ uiClass ] );
            pMethod = s_pClasses[ uiClass ]->pMethods;
         }
      }
   }

   nCount = 0;
   pReturn = hb_itemArrayNew( nSize );
   for( nIndex = 1; nIndex <= nLen && nCount < nSize; ++nIndex )
   {
      pInfo = &pIndex[ nIndex - 1 ];
      if( pInfo->pMethod )
      {
         const char * pszVar = pInfo->pMethod->pMessage->pSymbol->szName;
         PHB_ITEM pValue = hb_arrayGetItemPtr( pReturn, ++nCount );

         hb_arrayNew( pValue, 2 );
         if( pInfo->uiClass != pClass->uiClass )
         {
            char * pszCast = hb_xstrcpy( NULL, s_pClasses[ pInfo->uiClass ]->szName,
                                         ":", pszVar, NULL );
            hb_arraySetCPtr( pValue, 1, pszCast );
         }
         else
            hb_arraySetCConst( pValue, 1, pszVar );
         hb_arraySet( pValue, 2, hb_arrayGetItemPtr( pObject, nIndex ) );
      }
   }

   hb_xfree( pIndex );

   return pReturn;
}

static void hb_objSetIVars( PHB_ITEM pObject, PHB_ITEM pArray )
{
   if( pObject && HB_IS_OBJECT( pObject ) &&
       pArray && HB_IS_ARRAY( pArray ) &&
       pArray->item.asArray.value->uiClass == 0 )
   {
      HB_USHORT uiClass = pObject->item.asArray.value->uiClass;
      HB_SIZE nPos, nIndex, nLen;
      PHB_ITEM pValue;

      nPos = 0;
      while( ( pValue = hb_arrayGetItemPtr( pArray, ++nPos ) ) != NULL )
      {
         const char * pszMethod = hb_arrayGetCPtr( pValue, 1 );
         PHB_DYNS pVarSym = hb_dynsymFind( pszMethod );
         PHB_ITEM pNewVal = hb_arrayGetItemPtr( pValue, 2 );
         HB_USHORT uiSuper = uiClass;

         if( ! pVarSym )
         {
            const char * pszClass = strchr( pszMethod, ':' );
            if( pszClass )
            {
               nLen = pszClass - pszMethod;
               if( nLen )
               {
                  PHB_DYNS pParentSym;
                  char szClassName[ HB_SYMBOL_NAME_LEN + 1 ];

                  memcpy( szClassName, pszMethod, nLen );
                  szClassName[ nLen ] = '\0';
                  pParentSym = hb_dynsymFindName( szClassName );
                  uiSuper = pParentSym == NULL ? 0 :
                            hb_clsGetParent( s_pClasses[ uiClass ], pParentSym );
               }
               pVarSym = hb_dynsymFindName( pszClass + 1 );
            }
            else
               pVarSym = hb_dynsymFindName( pszMethod );
         }
         if( uiSuper && pNewVal && pVarSym &&
             ( nIndex = hb_clsGetVarIndexEx( uiClass, pVarSym, uiSuper ) ) != 0 )
         {
            hb_arraySet( pObject, nIndex, pNewVal );
         }
      }
   }
}

/* __objGetIVars( <oObject>, [<nScope>], [<lChanged>] )
 *          -> <aIVars> { { <cName>, <xVal> }, ... }
 */
HB_FUNC( __OBJGETIVARS )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   HB_USHORT uiScope = ( HB_USHORT ) hb_parni( 2 );
   HB_BOOL fChanged = hb_parldef( 3, HB_TRUE );

   hb_itemReturnRelease( hb_objGetIVars( pObject, uiScope, fChanged ) );
}

/* __objSetIVars( <oObject> | <hClass> | <cClassName> | <sClassFunc>,
 *                <aIVars> ) -> <oObject>
 */
HB_FUNC( __OBJSETIVARS )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( pObject && pArray )
   {
      PHB_ITEM pNewObj = NULL;

      if( HB_IS_NUMERIC( pObject ) )
         pObject = pNewObj = hb_clsInst( ( HB_USHORT ) hb_itemGetNI( pObject ) );
      else if( HB_IS_STRING( pObject ) )
         pObject = pNewObj = hb_clsInst( hb_clsFindClass( hb_itemGetCPtr( pObject ), NULL ) );
      else if( HB_IS_SYMBOL( pObject ) )
         pObject = pNewObj = hb_clsInst( hb_clsFindClassByFunc( hb_itemGetSymbol( pObject ) ) );
      else if( ! HB_IS_OBJECT( pObject ) )
         pObject = NULL;

      hb_objSetIVars( pObject, pArray );

      if( pObject )
         hb_itemReturn( pObject );
      if( pNewObj )
         hb_itemRelease( pNewObj );
   }
}

/* __objRestoreIVars( <aIVars>, <hClass> | <sClassFunc> |
                                <cClassName>[, <cClassFuncName>] ) -> <oObject>
 */
HB_FUNC( __OBJRESTOREIVARS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pClass = hb_param( 2, HB_IT_NUMERIC | HB_IT_STRING | HB_IT_SYMBOL );

   if( pClass && pArray && pArray->item.asArray.value->uiClass == 0 )
   {
      PHB_ITEM pObject = NULL;


      if( HB_IS_NUMERIC( pClass ) )
         pObject = hb_clsInst( ( HB_USHORT ) hb_itemGetNI( pClass ) );
      else if( HB_IS_STRING( pClass ) )
         pObject = hb_clsInst( hb_clsFindClass( hb_itemGetCPtr( pClass ), hb_parc( 3 ) ) );
      else if( HB_IS_SYMBOL( pClass ) )
         pObject = hb_clsInst( hb_clsFindClassByFunc( hb_itemGetSymbol( pClass ) ) );

      if( pObject )
      {
         hb_objSetIVars( pObject, pArray );
         hb_arraySwap( pObject, pArray );
         hb_itemRelease( pObject );
      }
   }

   hb_itemReturn( pArray );
}

/* __clsGetProperties( <nClassHandle>, [<lAllExported>] ) -> <acProperties>
 * Notice that this function works quite similar to __CLASSSEL()
 * except that just returns the name of the datas and methods
 * that have been declared as PROPERTY (PERSISTENT) or also EXPORTED
 * if second parameter <lAllExported> is true and message has corresponding
 * assign message (with "_" prefix)
 */

HB_FUNC( __CLSGETPROPERTIES )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod, pAccMth;
      HB_SIZE nLimit, nCount;
      HB_USHORT uiScope = HB_OO_CLSTP_PERSIST;

      if( hb_parl( 2 ) )
         uiScope |= HB_OO_CLSTP_EXPORTED;

      nCount = 0;
      nLimit = hb_clsMthNum( pClass );
      pMethod = pClass->pMethods;
      do
      {
         if( pMethod->pMessage && ( pMethod->uiScope & uiScope ) != 0 )
         {
            if( ( pMethod->uiScope & HB_OO_CLSTP_PERSIST ) != 0 )
               ++nCount;
            else if( pMethod->pMessage->pSymbol->szName[ 0 ] == '_' )
            {
               if( ! pMethod->pAccMsg )
                  pMethod->pAccMsg = hb_dynsymGetCase( pMethod->pMessage->pSymbol->szName + 1 );
               pAccMth = hb_clsFindMsg( pClass, pMethod->pAccMsg );
               if( pAccMth && ( pAccMth->uiScope & HB_OO_CLSTP_PERSIST ) == 0 )
                  ++nCount;
            }
         }
         ++pMethod;
      }
      while( --nLimit );

      hb_arrayNew( pReturn, nCount );

      nCount = 0;
      nLimit = hb_clsMthNum( pClass );
      pMethod = pClass->pMethods;
      do
      {
         if( pMethod->pMessage && ( pMethod->uiScope & uiScope ) != 0 )
         {
            if( ( pMethod->uiScope & HB_OO_CLSTP_PERSIST ) != 0 )
               hb_arraySetC( pReturn, ++nCount, pMethod->pMessage->pSymbol->szName );
            else if( pMethod->pMessage->pSymbol->szName[ 0 ] == '_' &&
                     pMethod->pAccMsg )
            {
               pAccMth = hb_clsFindMsg( pClass, pMethod->pAccMsg );
               if( pAccMth && ( pAccMth->uiScope & HB_OO_CLSTP_PERSIST ) == 0 )
                  hb_arraySetC( pReturn, ++nCount, pMethod->pMessage->pSymbol->szName + 1 );
            }
         }
         ++pMethod;
      }
      while( --nLimit );
   }

   hb_itemReturnRelease( pReturn );
}

/* __clsGetAncestors( <nClass> ) -> { <nSuper1>, <nSuper2>, ... }
 */
HB_FUNC( __CLSGETANCESTORS )
{
   HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 ), uiCount;

   if( uiClass && uiClass <= s_uiClasses )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pReturn = hb_stackReturnItem();
      PCLASS pClass = s_pClasses[ uiClass ];
      HB_SIZE nPos = 0;

      uiCount = pClass->uiSuperClasses;
      hb_arrayNew( pReturn, uiCount );
      while( uiCount-- )
      {
         HB_USHORT uiSuperCls = pClass->pSuperClasses[ uiCount ].uiClass;
         if( uiSuperCls != uiClass )
            hb_arraySetNI( pReturn, ++nPos, uiSuperCls );
      }
      hb_arraySize( pReturn, nPos );
   }
}

/*
 * __clsMsgType( <hClass>, <cMsgName> | <sMsgName> ) -> <nType>
 *
 * return type of method attached to given message,
 * <nType> is one of HB_OO_MSG_* values defined in hboo.ch or
 * -1 if message is not supported.
 */
HB_FUNC( __CLSMSGTYPE )
{
   PHB_DYNS pMessage = hb_objGetMsgSym( hb_param( 2, HB_IT_ANY ) );

   if( pMessage )
   {
      HB_STACK_TLS_PRELOAD
      HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 1 );
      PMETHOD pMethod = NULL;

      if( uiClass && uiClass <= s_uiClasses )
         pMethod = hb_clsFindMsg( s_pClasses[ uiClass ], pMessage );

      hb_retni( pMethod ? hb_methodType( pMethod ) : -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* extend the size of classes buffer to given value to avoid later
 * RT reallocations. It may be useful in some very seldom cases
 * for MT programs which will allocate dynamically at runtime
 * more then 16386 classes. In practice rather impossible though
 * who knows ;-)
 * __clsPreAllocate( [<nMaxClasses>] ) -> <nMaxClasses>
 */
HB_FUNC( __CLSPREALLOCATE )
{
   HB_STACK_TLS_PRELOAD
   HB_LONG lNewSize = hb_parnl( 1 );

   if( lNewSize > ( HB_LONG ) USHRT_MAX )
      lNewSize = USHRT_MAX;

   HB_CLASS_LOCK();

   if( lNewSize > ( HB_LONG ) s_uiClsSize )
   {
      s_uiClsSize = ( HB_USHORT ) lNewSize;
      s_pClasses = ( PCLASS * ) hb_xrealloc( s_pClasses, sizeof( PCLASS ) *
                                             ( ( HB_SIZE ) s_uiClsSize + 1 ) );
   }

   HB_CLASS_UNLOCK();

   hb_retnl( s_uiClsSize );
}

/* __clsLockDef( <clsItem> ) -> <lLocked>
 */
HB_FUNC( __CLSLOCKDEF )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pClsItm = hb_param( 1, HB_IT_BYREF );
   HB_BOOL fLocked = HB_FALSE;

   if( pClsItm && HB_IS_NIL( pClsItm ) )
   {
      if( ! s_pClassMtx || hb_threadMutexLock( s_pClassMtx ) )
      {
         if( HB_IS_NIL( pClsItm ) )
            fLocked = HB_TRUE;
         else if( s_pClassMtx )
            hb_threadMutexUnlock( s_pClassMtx );
      }
   }
   hb_retl( fLocked );
}

/* __clsUnlockDef( @<clsItem>, <clsDef> )
 */
HB_FUNC( __CLSUNLOCKDEF )
{
   PHB_ITEM pClsDst = hb_param( 1, HB_IT_BYREF ),
            pClsSrc = hb_param( 2, HB_IT_ANY );

   if( pClsDst && pClsSrc && HB_IS_NIL( pClsDst ) && ! HB_ISBYREF( 2 ) )
   {
      /* special core code only macro used to eliminate race condition
       * in unprotected readonly access to pClsDst variable.
       */
      hb_itemSafeMove( pClsDst, pClsSrc );
   }

   if( s_pClassMtx )
      hb_threadMutexUnlock( s_pClassMtx );
}

/* Dirty functions which converts array to object of given class
 * __objSetClass( <oObject>, <cClassName> [, <cClassFuncName> ] ) -> <oObject>
 */
HB_FUNC( __OBJSETCLASS )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );

   if( pObject && pObject->item.asArray.value->uiClass == 0 )
   {
      const char * szClass = hb_parc( 2 );

      if( szClass )
         hb_objSetClass( pObject, szClass, hb_parc( 3 ) );
   }

   hb_itemReturn( pObject );
}

/* Real dirty function, though very usefull under certain circunstances:
 * It allows to change the class handle of an object into another class handle,
 * so the object behaves like a different Class of object.
 * Based on objects.lib SetClsHandle()
 * __objSetClassHandle( <oObject>, <nClassHandle> ) -> <nPrevClassHandle>
 */
HB_FUNC( __OBJSETCLASSHANDLE )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   HB_USHORT uiPrevClassHandle = 0;

   if( pObject )
   {
      HB_USHORT uiClass = ( HB_USHORT ) hb_parni( 2 );

      uiPrevClassHandle = pObject->item.asArray.value->uiClass;
      if( uiClass <= s_uiClasses )
         pObject->item.asArray.value->uiClass = uiClass;
   }

   hb_retnl( uiPrevClassHandle );
}

#if defined( HB_LEGACY_LEVEL5 )
HB_FUNC_TRANSLATE( HB_SETCLSHANDLE, __OBJSETCLASSHANDLE )
#endif

/* Harbour equivalent for Clipper internal __mdCreate() */
HB_USHORT hb_clsCreate( HB_USHORT usSize, const char * szClassName )
{
   return hb_clsNew( szClassName, usSize, NULL, NULL, HB_FALSE );
}

/* Harbour equivalent for Clipper internal __mdAdd() */
void hb_clsAdd( HB_USHORT usClassH, const char * szMethodName, PHB_FUNC pFuncPtr )
{
   PHB_SYMB pExecSym;
   PHB_ITEM pFuncItem;

   /*
    * We can use empty name "" for this symbol in hb_symbolNew()
    * It's only envelop for function with additional execution
    * information for HVM not registered symbol. [druzus]
    */
   pExecSym = hb_symbolNew( "" );
   pExecSym->value.pFunPtr = pFuncPtr;
   pFuncItem = hb_itemPutSymbol( NULL, pExecSym );

   hb_clsAddMsg( usClassH, szMethodName, HB_OO_MSG_METHOD, 0, pFuncItem, NULL );

   hb_itemRelease( pFuncItem );
}

/* Harbour equivalent for Clipper internal __mdAssociate() */
void hb_clsAssociate( HB_USHORT usClassH )
{
   PHB_ITEM pSelf = hb_clsInst( usClassH );

   if( pSelf )
      hb_itemReturnRelease( pSelf );
   else
   {
      HB_STACK_TLS_PRELOAD
      hb_ret();
   }
}

#if 0
/*
 * return real function name ignoring aliasing
 */
const char * hb_clsRealMethodName( void )
{
   HB_ISIZ nOffset = hb_stackBaseProcOffset( 1 );
   const char * szName = NULL;

   if( nOffset > 0 )
   {
      PHB_STACK_STATE pStack = hb_stackItem( nOffset )->item.asSymbol.stackstate;

      if( pStack->uiClass && pStack->uiClass <= s_uiClasses )
      {
         PCLASS pClass = s_pClasses[ pStack->uiClass ];

         if( ( HB_SIZE ) pStack->uiMethod < hb_clsMthNum( pClass ) )
         {
            PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

            if( pMethod->pMessage )
               szName = pMethod->pMessage->pSymbol->szName;
         }
      }
   }
   return szName;
}
#endif
