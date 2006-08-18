/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base-routines for OOPS system
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb___msgEval()
 *    __CLASSNEW()
 *    __CLASSINSTANCE()
 *    __CLASSADD()
 *    __CLASSNAME()
 *    __CLASSSEL() (based on hb___msgClsSel())
 *
 * Copyright 1999 Janica Lubos <janica@fornax.elf.stuba.sk>
 *    hb_clsDictRealloc()
 *
 * Copyright 2000 ( ->07/2000 ) JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <cakiral@altern.org
 *    Multiple inheritence fully implemented
 *    Forwarding, delegating
 *    Data initialisation & Autoinit for Bool and Numeric
 *    Scoping : Protected / exported
 *
 * Copyright 2000 ( 08/2000-> ) JF. Lefebvre <jfl@mafact.com>
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
 *    __CLS_PAR00() (Allow the creation of class wich not autoinherit of the default HBObject)
 *    Adding HB_CLS_ENFORCERO FLAG to disable Write access to RO VAR
 *    outside of Constructors /!\ Could be related to some incompatibility
 *    Added hb_objGetRealClsName to keep a full class tree ( for 99% cases )
 *    Fixed hb_clsIsParent
 *
 *
 *    hb_objGetMthd() & __CLSADDMSG modified to translate the followings operators
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
 "!"     = __OpNot
 ".NOT." = __OpNot (same as "!")
 ".AND." = __OpAnd
 ".OR."  = __OpOr
 ":="    = __OpAssign   ... not tested ...
 "[]"    = __OpArrayIndex
 *
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapicls.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hboo.ch"

#include <ctype.h>             /* For toupper() */

/* DEBUG only*/
/* #include <windows.h> */

typedef struct
{
   PHB_DYNS pMessage;            /* Method Symbolic name */
   PHB_SYMB pFuncSym;            /* Function symbol */
   USHORT   uiData;              /* Item position for data (Harbour like, begin from 1) */
   USHORT   uiDataShared;        /* Item position for datashared (original pos within Shared Class) */
   USHORT   uiSprClass;          /* Originalclass'handel (super or current class'handel if not herited). */ /*Added by RAC&JF*/
   USHORT   uiScope;             /* Scoping value */
   PHB_ITEM pInitValue;          /* Init Value for data */
   USHORT   bClsDataInitiated;   /* There is one value assigned at init time */
   USHORT   bIsPersistent;       /* persistence support */
   ULONG    ulCalls;             /* profiler support */
   ULONG    ulTime;              /* profiler support */
   ULONG    ulRecurse;           /* profiler support */
} METHOD, * PMETHOD;

typedef struct
{
   char *   szName;         /* Class name */
   USHORT   uiDatas;        /* Total Data Counter */
   USHORT   uiDataFirst;    /* First uiData from this class */
   PMETHOD  pMethods;
   USHORT   uiMethods;      /* Total Method initialised Counter */
   USHORT   uiHashKey;
   USHORT   uiDatasShared;  /* Total shared Class data within Class data */
   PHB_ITEM pClassDatas;    /* Harbour Array for ClassDatas and shared */
   PHB_ITEM pInlines;       /* Array for inline codeblocks */
   PHB_SYMB pFunError;      /* error handler for not defined messages */
   ULONG    ulOpFlags;      /* Flags for overloaded operators */
} CLASS, * PCLASS;

#define BASE_METHODS   100  /* starting maximum number of message */
#define BUCKET          5
#define HASH_KEY        ( BASE_METHODS / BUCKET )


static HARBOUR  hb___msgGetData( void );
static HARBOUR  hb___msgSetData( void );
static HARBOUR  hb___msgGetClsData( void );
static HARBOUR  hb___msgSetClsData( void );
static HARBOUR  hb___msgGetShrData( void );
static HARBOUR  hb___msgSetShrData( void );
static HARBOUR  hb___msgEvalInline( void );
static HARBOUR  hb___msgVirtual( void );
static HARBOUR  hb___msgSuper( void );
static HARBOUR  hb___msgNoMethod( void );
static HARBOUR  hb___msgNull( void );

static HARBOUR  hb___msgClsH( void );
static HARBOUR  hb___msgClsName( void );
static HARBOUR  hb___msgClsSel( void );
static HARBOUR  hb___msgEval( void );
/* static HARBOUR  hb___msgClass( void ); */
/* static HARBOUR  hb___msgClsParent( void ); */

/*
 * The positions of items in symbol table below have to correspond
 * to HB_OO_OP_* constants in hbapicls.h, [druzus]
 */
static HB_SYMB s_opSymbols[ HB_OO_MAX_OPERATOR + 1 ] = {
   { "__OPPLUS",              {HB_FS_MESSAGE}, {NULL}, NULL },  /* 01 */
   { "__OPMINUS",             {HB_FS_MESSAGE}, {NULL}, NULL },  /* 02 */
   { "__OPMULT",              {HB_FS_MESSAGE}, {NULL}, NULL },  /* 03 */
   { "__OPDIVIDE",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 04 */
   { "__OPMOD",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 05 */
   { "__OPPOWER",             {HB_FS_MESSAGE}, {NULL}, NULL },  /* 06 */
   { "__OPINC",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 07 */
   { "__OPDEC",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 08 */
   { "__OPEQUAL",             {HB_FS_MESSAGE}, {NULL}, NULL },  /* 09 */
   { "__OPEXACTEQUAL",        {HB_FS_MESSAGE}, {NULL}, NULL },  /* 10 */
   { "__OPNOTEQUAL",          {HB_FS_MESSAGE}, {NULL}, NULL },  /* 11 */
   { "__OPLESS",              {HB_FS_MESSAGE}, {NULL}, NULL },  /* 12 */
   { "__OPLESSEQUAL",         {HB_FS_MESSAGE}, {NULL}, NULL },  /* 13 */
   { "__OPGREATER",           {HB_FS_MESSAGE}, {NULL}, NULL },  /* 14 */
   { "__OPGREATEREQUAL",      {HB_FS_MESSAGE}, {NULL}, NULL },  /* 15 */
   { "__OPADDIGN",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 16 */
   { "__OPINSTRING",          {HB_FS_MESSAGE}, {NULL}, NULL },  /* 17 */
   { "__OPNOT",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 18 */
   { "__OPAND",               {HB_FS_MESSAGE}, {NULL}, NULL },  /* 19 */
   { "__OPOR",                {HB_FS_MESSAGE}, {NULL}, NULL },  /* 20 */
   { "__OPARRAYINDEX",        {HB_FS_MESSAGE}, {NULL}, NULL },  /* 21 */
   { "__ENUMNEXT",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 22 */
   { "__ENUMPREV",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 23 */
   { "__ENUMINDEX",           {HB_FS_MESSAGE}, {NULL}, NULL },  /* 24 */
   { "__ENUMBASE",            {HB_FS_MESSAGE}, {NULL}, NULL },  /* 25 */
   { "__ENUMVALUE",           {HB_FS_MESSAGE}, {NULL}, NULL }   /* 26 */
};

static HB_SYMB s___msgSetData    = { "__msgSetData",    {HB_FS_MESSAGE}, {hb___msgSetData},    NULL };
static HB_SYMB s___msgGetData    = { "__msgGetData",    {HB_FS_MESSAGE}, {hb___msgGetData},    NULL };
static HB_SYMB s___msgSetClsData = { "__msgSetClsData", {HB_FS_MESSAGE}, {hb___msgSetClsData}, NULL };
static HB_SYMB s___msgGetClsData = { "__msgGetClsData", {HB_FS_MESSAGE}, {hb___msgGetClsData}, NULL };
static HB_SYMB s___msgSetShrData = { "__msgSetShrData", {HB_FS_MESSAGE}, {hb___msgSetShrData}, NULL };
static HB_SYMB s___msgGetShrData = { "__msgGetShrData", {HB_FS_MESSAGE}, {hb___msgGetShrData}, NULL };
static HB_SYMB s___msgEvalInline = { "__msgEvalInline", {HB_FS_MESSAGE}, {hb___msgEvalInline}, NULL };
static HB_SYMB s___msgVirtual    = { "__msgVirtual",    {HB_FS_MESSAGE}, {hb___msgVirtual},    NULL };
static HB_SYMB s___msgSuper      = { "__msgSuper",      {HB_FS_MESSAGE}, {hb___msgSuper},      NULL };
static HB_SYMB s___msgNoMethod   = { "__msgNoMethod",   {HB_FS_MESSAGE}, {hb___msgNoMethod},   NULL };

static HB_SYMB s___msgClassName  = { "CLASSNAME",       {HB_FS_MESSAGE}, {hb___msgClsName},    NULL };
static HB_SYMB s___msgClassH     = { "CLASSH",          {HB_FS_MESSAGE}, {hb___msgClsH},       NULL };
static HB_SYMB s___msgClassSel   = { "CLASSSEL",        {HB_FS_MESSAGE}, {hb___msgClsSel},     NULL };
static HB_SYMB s___msgEval       = { "EVAL",            {HB_FS_MESSAGE}, {hb___msgEval},       NULL };
/*
static HB_SYMB s___msgClsParent  = { "ISDERIVEDFROM",   {HB_FS_MESSAGE}, {hb___msgClsParent},  NULL };
static HB_SYMB s___msgClass      = { "CLASS",           {HB_FS_MESSAGE}, {hb___msgClass},      NULL };
*/
/* Default enumerator methods (FOR EACH) */
static HB_SYMB s___msgEnumIndex  = { "__ENUMINDEX",     {HB_FS_MESSAGE}, {hb___msgNull},       NULL };
static HB_SYMB s___msgEnumBase   = { "__ENUMBASE",      {HB_FS_MESSAGE}, {hb___msgNull},       NULL };
static HB_SYMB s___msgEnumValue  = { "__ENUMVALUE",     {HB_FS_MESSAGE}, {hb___msgNull},       NULL };

/* WITH OBJECT base value access/asign methods (:__withobject) */
static HB_SYMB s___msgWithObjectPush = { "__WITHOBJECT",  {HB_FS_MESSAGE}, {hb___msgNull},       NULL };
static HB_SYMB s___msgWithObjectPop  = { "___WITHOBJECT", {HB_FS_MESSAGE}, {hb___msgNull},       NULL };

static PCLASS   s_pClasses     = NULL;
static USHORT   s_uiClasses    = 0;
static PMETHOD  s_pMethod      = NULL; /* TOFIX: The object engine is not thread safe because of this. [vszakats] */

/* All functions contained in classes.c */

static PHB_ITEM hb_clsInst( USHORT uiClass );
static ULONG    hb_cls_MsgToNum( PHB_DYNS pMsg );
static void     hb_clsDictRealloc( PCLASS pClass );
static void     hb_clsRelease( PCLASS );

#ifdef HB_CLS_ENFORCERO
static PMETHOD  hb_objGetpMethod( PHB_ITEM, PHB_SYMB );
#endif

/* ================================================ */

/*
 * hb_clsDictRealloc( PCLASS )
 *
 * Realloc (widen) class
 */
static void hb_clsDictRealloc( PCLASS pClass )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_clsDictRealloc(%p)", pClass));

   if( pClass )
   {
      PMETHOD pNewMethods;
      USHORT  uiNewHashKey = pClass->uiHashKey * 2 ;
      USHORT  ui;
      USHORT  uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );

      do
      {
         uiNewHashKey += ( USHORT ) HASH_KEY ;

         pNewMethods = ( PMETHOD ) hb_xgrab( uiNewHashKey * BUCKET * sizeof( METHOD ) );
         memset( pNewMethods, 0, uiNewHashKey * BUCKET * sizeof( METHOD ) );


         for( ui = 0; ui < uiLimit; ui++ )
         {
             PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ ui ].pMessage;

             if( pMessage )
             {
                USHORT uiBucket;
                USHORT uiAt = ( USHORT ) ( ( hb_cls_MsgToNum( pMessage ) % uiNewHashKey ) * BUCKET );

                for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
                {
                    if( pNewMethods[ uiAt+uiBucket ].pMessage == 0 ) /* this message position is empty */
                    {
                       hb_xmemcpy( pNewMethods + (uiAt+uiBucket), pClass->pMethods + ui, sizeof( METHOD ) );
                       break;
                    }
                }

                /* Not enough go back to the beginning */
                if( uiBucket >= BUCKET ) /*&& nOccurs++ < 5)*/
                {
                   hb_xfree( pNewMethods );
                   break;
                }
                /*else
                   if( nOccurs < 5 )
                      nOccurs = 0;
                   else
                   {
                      hb_xfree( pNewMethods );
                      hb_errInternal( 9999, "Not able to realloc classmessage! __clsDictRealloc", NULL, NULL );
                      break;
                   }*/
             }
         }

      } while( ui < uiLimit );


      pClass->uiHashKey = uiNewHashKey;
      hb_xfree( pClass->pMethods );
      pClass->pMethods = pNewMethods;

   }
}

/*
 * initialize Classy/OO system at HVM startup
 */
void hb_clsInit( void )
{
   PHB_SYMB pOpSym;
   USHORT uiOperator;

   for( uiOperator = 0, pOpSym = s_opSymbols; uiOperator <= HB_OO_MAX_OPERATOR;
        ++uiOperator, ++pOpSym )
   {
      pOpSym->pDynSym = hb_dynsymGetCase( pOpSym->szName );
   }

   s___msgClassName.pDynSym = hb_dynsymGetCase( s___msgClassName.szName );  /* Standard messages        */
   s___msgClassH.pDynSym    = hb_dynsymGetCase( s___msgClassH.szName );     /* Not present in classdef. */
   s___msgClassSel.pDynSym  = hb_dynsymGetCase( s___msgClassSel.szName );
   s___msgEval.pDynSym      = hb_dynsymGetCase( s___msgEval.szName );
/*
   s___msgClsParent.pDynSym = hb_dynsymGetCase( s___msgClsParent.szName );
   s___msgClass.pDynSym     = hb_dynsymGetCase( s___msgClass.szName );
*/
   s___msgEnumIndex.pDynSym = hb_dynsymGetCase( s___msgEnumIndex.szName );
   s___msgEnumBase.pDynSym  = hb_dynsymGetCase( s___msgEnumBase.szName );
   s___msgEnumValue.pDynSym = hb_dynsymGetCase( s___msgEnumValue.szName );

   s___msgWithObjectPush.pDynSym = hb_dynsymGetCase( s___msgWithObjectPush.szName );
   s___msgWithObjectPop.pDynSym  = hb_dynsymGetCase( s___msgWithObjectPop.szName );
}

/*
 * hb_clsRelease( <pClass> )
 *
 * Release a class from memory
 */
static void hb_clsRelease( PCLASS pClass )
{
   USHORT uiAt;
   USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
   PMETHOD pMeth = pClass->pMethods;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsRelease(%p)", pClass));

   for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
   {
     if( pMeth->pInitValue )
     {
        hb_itemRelease( pMeth->pInitValue );
     }
   }

   hb_xfree( pClass->szName );
   hb_xfree( pClass->pMethods );

   hb_itemRelease( pClass->pClassDatas );
   hb_itemRelease( pClass->pInlines );
}


/*
 * hb_clsReleaseAll()
 *
 * Release all classes
 */
void hb_clsReleaseAll( void )
{
   SHORT uiClass;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsReleaseAll()"));

   for( uiClass = 0  ; uiClass < s_uiClasses  ; uiClass++ )
   {
      hb_clsRelease( s_pClasses + uiClass );
   }

   if( s_pClasses )
   {
      hb_xfree( s_pClasses );
   }

   s_uiClasses = 0;
   s_pClasses  = NULL;
}

/* Mark all internal data as used so it will not be released by the
 * garbage collector
 */

void hb_clsIsClassRef( void )
{
   USHORT uiClass = s_uiClasses;
   PCLASS pClass = s_pClasses;
   USHORT uiAt;
   USHORT uiLimit;
   PMETHOD pMeth;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsIsClassRef()"));

   while( uiClass-- )
   {
      if( pClass->pInlines )
      {
         if( HB_IS_GCITEM( pClass->pInlines ) )
            hb_gcItemRef( pClass->pInlines );
      }

      if( pClass->pClassDatas )
      {
         if( HB_IS_GCITEM( pClass->pClassDatas ) )
            hb_gcItemRef( pClass->pClassDatas );
      }

      uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
      {
         if( pMeth->pInitValue )
         {
            if( HB_IS_GCITEM( pMeth->pInitValue ) )
               hb_gcItemRef( pMeth->pInitValue );
         }
      }

      ++pClass;
   }
}

/* Currently (2004.04.02) this function is not used
 it is commented out to suppress warning message in gcc
*/
#if 0
static void hb_clsScope( PHB_ITEM pObject, PMETHOD pMethod )
{
   long lOffset = hb_stackBaseOffset();
   PHB_ITEM pCaller;
   LONG iLevel = 1;
   BOOL bRetVal = FALSE ;
   USHORT uiScope = pMethod->uiScope;
   PHB_DYNS pMessage = pMethod->pMessage;
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];
   char * szCallerNameMsg;
   char * szCallerNameObject;
   char * szSelfNameMsg;
   char * szSelfNameObject;    /* debug */
   char * szSelfNameRealClass;

   if( ( ( uiScope & HB_OO_CLSTP_PROTECTED ) ) ||
       ( ( uiScope & HB_OO_CLSTP_HIDDEN    ) ) ||
       ( ( uiScope & HB_OO_CLSTP_READONLY  ) ) )
   {
      szSelfNameObject    = hb_objGetClsName( pObject );  /* debug */
      szSelfNameMsg       = pMessage->pSymbol->szName  ;
      szSelfNameRealClass = hb_objGetRealClsName( pObject, pMessage->pSymbol->szName );

      while( iLevel-- > 0 && lOffset > 1 )
         lOffset = hb_stackItem( lOffset - 1 )->item.asSymbol.stackstate->lBaseItem + 1;

      szCallerNameMsg = hb_stackItem( lOffset - 1 )->item.asSymbol.value->szName;

      /* Is it an inline ? if so back one more ... */
      if( strcmp( szCallerNameMsg, "__EVAL" ) == 0 && lOffset > 1 )
      {
         lOffset = hb_stackItem( lOffset - 1 )->item.asSymbol.stackstate->lBaseItem + 1;
         szCallerNameMsg = hb_stackItem( lOffset - 1 )->item.asSymbol.value->szName;
      }

      /* Is it an eval ? if so back another one more ... */
      if( ( strcmp( szCallerNameMsg, "EVAL" ) == 0 ) && lOffset > 1 )
      {
         lOffset = hb_stackItem( lOffset - 1 )->item.asSymbol.stackstate->lBaseItem + 1;
         szCallerNameMsg = hb_stackItem( lOffset - 1 )->item.asSymbol.value->szName;
      }

      /* Is it an Aeval ? if so back another one more ... */
      if ( ( strcmp( szCallerNameMsg, "AEVAL" ) == 0 ) && lOffset > 1 )
      {
         lOffset = hb_stackItem( lOffset - 1 )->item.asSymbol.stackstate->lBaseItem + 1;
         szCallerNameMsg = hb_stackItem( lOffset - 1 )->item.asSymbol.value->szName;
      }

      if( iLevel == -1 )
      {
         /* Now get the callers ...  */
         pCaller = hb_stackItem( lOffset );
         szCallerNameObject = hb_objGetRealClsName( pCaller, szCallerNameMsg );

         strcpy( szName, szCallerNameObject );
         strcat( szName, ":" );
         strcat( szName, szCallerNameMsg );
         strcat( szName, ">" );
         strcat( szName, szSelfNameRealClass );
         strcat( szName, ">" );
         strcat( szName, szSelfNameObject );
         strcat( szName, ":" );
         strcat( szName, szSelfNameMsg );

         /*strcpy( szName, szSelfNameRealClass ); */
         /*strcat( szName, ":" );                 */
         /*strcat( szName, szSelfNameMsg );       */

         if( uiScope & HB_OO_CLSTP_PROTECTED )
         {
            if( pCaller->type == HB_IT_ARRAY )  /* is the sender an object  */
            {
               /* Trying to access a protected Msg from outside the object ... */
               if( strcmp( szCallerNameObject, szSelfNameRealClass ) != 0 )
               {
                  hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (protected 1)", szName, 0 );
                  return;
               }
            }
            else
            {
               /* If called from a function ... protected violation !  */
               hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (protected 0)", szName, 0 );
               return;
            }
         }

         if( uiScope & HB_OO_CLSTP_HIDDEN )
         {
            if( pCaller->type == HB_IT_ARRAY )  /* is the sender an object  */
            {
               /* Trying to access a protected Msg from outside the object ... */
               if( strcmp( szCallerNameObject, szSelfNameRealClass ) != 0 )
               {
                  hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (Hidden 1)", szName, 0 );
                  return;
               }
               else
               {
                  /* Now as it is an hidden Msg, it can only be called from */
                  /* a method of its original class */
                  if( !( hb_objGetRealClsName( pCaller, szCallerNameMsg ) == szSelfNameRealClass ) )
                  {
                     hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (Hidden 2)", szName, 0 );
                     return;
                  }
               }
            }
            else
            {
               /* If called from a function ... Hidden violation ! */
               hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (Hidden 0)", szName, 0 );
            }
         }

         if( uiScope & HB_OO_CLSTP_READONLY )
         {
            if( ( pMethod->pFuncSym == &s___msgSetData    ) ||
                ( pMethod->pFuncSym == &s___msgSetClsData ) ||
                ( pMethod->pFuncSym == &s___msgSetShrData ) )
               bRetVal = TRUE;

            if( bRetVal )
            {
               if( pCaller->type == HB_IT_ARRAY )  /* is the sender an object  */
               {
                  /* Trying to assign a RO Msg from outside the object ... */
                  if( strcmp( szCallerNameObject, szSelfNameRealClass ) != 0 )
                  {
                     hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (ReadOnly)", szName, 0 );
                     return;
                  }
                  else
                  {
#ifdef HB_CLS_ENFORCERO  /* Not enabled by default */
                         /* can only be called from a Constructor */
                         /* ok Now is it a CTOR ? */
                     PMETHOD pCallerMethod ;
                     PHB_DYNS pCallerMsg = hb_dynsymGet( szCallerNameMsg );

                     pCallerMethod = hb_objGetpMethod( pCaller, pCallerMsg->pSymbol );

                     if( pCallerMethod )
                     {
                        if( ! ( pCallerMethod->uiScope & HB_OO_CLSTP_CTOR ) )
                        {
                           hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (ReadOnly)", szName, 0 );
                           return;
                        }
                     }
#endif
                  }
               }
               else
               {
                  /* If called from a function ... ReadOnly violation ! */
                  hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (ReadOnly 0)", szName, 0 );
               }
            }
         }
      }
   }
}
#endif

ULONG hb_cls_MsgToNum( PHB_DYNS pMsg )
{
   USHORT i;
   ULONG nRetVal = 0;

   for( i = 0; pMsg->pSymbol->szName[ i ] != '\0'; i++)
      nRetVal = ( nRetVal << 1 ) + pMsg->pSymbol->szName[ i ];

   return nRetVal;
}

BOOL hb_clsIsParent(  USHORT uiClass, char * szParentName )
{
   USHORT uiAt, uiLimit;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );

      uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );

      if( strcmp( pClass->szName, szParentName ) == 0 )
         return TRUE;

      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         if( ( pClass->pMethods[ uiAt ].uiScope & HB_OO_CLSTP_CLASS ) == HB_OO_CLSTP_CLASS )
         {
            if( strcmp( pClass->pMethods[ uiAt ].pMessage->pSymbol->szName, szParentName ) == 0 )
               return TRUE;
         }
      }
   }

   return FALSE;
}

USHORT hb_objGetClass( PHB_ITEM pItem )
{
   if( pItem && HB_IS_ARRAY( pItem ) )
      return pItem->item.asArray.value->uiClass;
   else
      return 0;
}

/* ================================================ */

/*
 * <szName> = ( pObject )
 *
 * Get the class name of an object
 *
 */
char * hb_objGetClsName( PHB_ITEM pObject )
{
   char * szClassName;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetClsName(%p)", pObject));

   if( HB_IS_ARRAY( pObject ) )
   {
      if( ! pObject->item.asArray.value->uiClass )
         szClassName = "ARRAY";
      else
         szClassName =
            ( s_pClasses + pObject->item.asArray.value->uiClass - 1 )->szName;
   }
   else                                         /* built in types */
   {
      switch( pObject->type )
      {
         case HB_IT_NIL:
            szClassName = "NIL";
            break;

         case HB_IT_STRING:
         case HB_IT_MEMO:
            szClassName = "CHARACTER";
            break;

         case HB_IT_BLOCK:
            szClassName = "BLOCK";
            break;

         case HB_IT_SYMBOL:
            szClassName = "SYMBOL";
            break;

         case HB_IT_DATE:
            szClassName = "DATE";
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DOUBLE:
            szClassName = "NUMERIC";
            break;

         case HB_IT_LOGICAL:
            szClassName = "LOGICAL";
            break;

         default:
            szClassName = "UNKNOWN";
            break;
      }
   }

   return szClassName;
}

/*
 * <szName> = ( pObject )
 *
 * Get the real class name of an object message
 * Will return the class name from wich the message is inherited in case
 * of inheritance.
 *
 */
char * hb_objGetRealClsName( PHB_ITEM pObject, char * szName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_objGetrealClsName(%p)", pObject));

   if( HB_IS_OBJECT( pObject ) )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( szName );
      USHORT uiClass;
      USHORT uiCurCls;
      USHORT uiClsTree;

      uiClass = pObject->item.asArray.value->uiClass;

      /* default value to current class object */
      if (pObject->item.asArray.value->puiClsTree && pObject->item.asArray.value->puiClsTree[0])
      {
         uiClsTree = pObject->item.asArray.value->puiClsTree[0] ;
         uiCurCls  = pObject->item.asArray.value->puiClsTree[uiClsTree] ;
      }
      else
      {
         uiClsTree = 1;          /* Flag value */
         uiCurCls = uiClass;
      }

      while (uiClsTree)
      {
         if( uiCurCls && uiCurCls <= s_uiClasses )
         {
            PCLASS pClass  = s_pClasses + ( uiCurCls - 1 );
            USHORT uiAt    = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pClass->uiHashKey ) * BUCKET );
            USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
            USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

            while( uiAt != uiLimit )
            {
               if( pClass->pMethods[ uiAt ].pMessage == pMsg )
               {
                  uiClass = (pClass->pMethods + uiAt)->uiSprClass;
                  uiClsTree=1; /* Flag Value */
                  break;
               }
               if( ++uiAt == uiMask )
                  uiAt = 0;
            }
         }
         if (-- uiClsTree)
            uiCurCls = pObject->item.asArray.value->puiClsTree[uiClsTree] ;
      }

      if( uiClass && uiClass <= s_uiClasses )
         return ( s_pClasses + uiClass - 1 )->szName;
   }

   return hb_objGetClsName( pObject );
}

static void hb_objPushSuperCast( PHB_ITEM pObject )
{
   PHB_BASEARRAY pObjBase;
   PHB_ITEM pRealObj;
   USHORT uiClass;
   USHORT nPos;

   pObjBase = pObject->item.asArray.value;
   uiClass = pObjBase->uiClass;

   pRealObj = hb_itemNew( pObjBase->pItems );
   /* and take back the good pObjBase */
   pObjBase = pRealObj->item.asArray.value;
   /* Now I should exchnage it with the current stacked value */
   hb_itemSwap( pObject, pRealObj );
   /* and release the fake one */
   hb_itemRelease( pRealObj );

   /* Push current SuperClass handle */
   if ( ! pObjBase->puiClsTree )
   {
      pObjBase->puiClsTree = ( USHORT * ) hb_xgrab( sizeof( USHORT ) );
      pObjBase->puiClsTree[ 0 ] = 0;
   }
   nPos = pObjBase->puiClsTree[ 0 ] + 1;
   pObjBase->puiClsTree = ( USHORT * ) hb_xrealloc( pObjBase->puiClsTree, sizeof( USHORT ) * ( nPos + 1 ) );
   pObjBase->puiClsTree[ 0 ] = nPos ;
   pObjBase->puiClsTree[ nPos ] = uiClass;
}

void hb_objPopSuperCast( PHB_ITEM pObject )
{
   if( HB_IS_OBJECT( pObject ) )
   {
      PHB_BASEARRAY pObjBase = pObject->item.asArray.value;

      if( pObjBase->puiClsTree )
      {
         USHORT nPos = pObjBase->puiClsTree[ 0 ] - 1;

         /* POP SuperClass handle */
         if( nPos )
         {
            pObjBase->puiClsTree = ( USHORT * ) hb_xrealloc( pObjBase->puiClsTree, sizeof( USHORT ) * ( nPos + 1 ) );
            pObjBase->puiClsTree[ 0 ] = nPos;
         }
         else
         {
            hb_xfree(pObjBase->puiClsTree);
            pObjBase->puiClsTree = NULL ;
         }
      }
   }
}

/*
 * <pFuncSym> = hb_objGetMethod( <pObject>, <pMessage>, <fpPopSuper> )
 *
 * Internal function to the function pointer of a message of an object
 */
PHB_SYMB hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pMessage, BOOL * pfPopSuper )
{
   PCLASS pClass = NULL;
   PHB_DYNS pMsg;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetMethod(%p, %p, %p)", pObject, pMessage, pfPopSuper));

   s_pMethod = NULL;
   if( pfPopSuper )
      *pfPopSuper = FALSE;

   pMsg = pMessage->pDynSym;

   if( HB_IS_ARRAY( pObject ) )
   {
      USHORT uiClass = pObject->item.asArray.value->uiClass;

      if( pfPopSuper && pObject->item.asArray.value->uiPrevCls )
      {
         hb_objPushSuperCast( pObject );
         *pfPopSuper = TRUE;
      }

      if( uiClass && uiClass <= s_uiClasses )
      {
         USHORT uiAt, uiMask, uiLimit;

         pClass  = s_pClasses + ( uiClass - 1 );
         uiAt    = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pClass->uiHashKey ) * BUCKET );
         uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

         while( uiAt != uiLimit )
         {
            if( pClass->pMethods[ uiAt ].pMessage == pMsg )
            {
               PMETHOD pMethod = pClass->pMethods + uiAt;
               /* hb_clsScope( pObject, pMethod ); */  /* debug */
               s_pMethod = pMethod ;
               return pMethod->pFuncSym;
            }
            uiAt++;
            if( uiAt == uiMask )
               uiAt = 0;
         }
      }
   }
   else if( HB_IS_BLOCK( pObject ) )
   {
      if( pMessage == &hb_symEval )
         return pMessage;
      else if( pMsg == s___msgEval.pDynSym )
         return &hb_symEval;
   }
   else if( HB_IS_BYREF( pObject ) )
   {
      /* method of enumerator variable from FOR EACH statement
       */
      PHB_ITEM pEnum = hb_itemUnRefOnce( pObject );

      if( HB_IS_ENUM( pEnum ) )
      {
         /*
          * Do actions here - we alrady have unreferences pEnum so
          * it will be a little bit faster but in the future it'
          * s possible that I'll move it to separate funcions when
          * I'll add enumerators overloading. [druzus]
          */
         if( pMsg == s___msgEnumIndex.pDynSym )
         {
            hb_itemPutNL( hb_stackReturnItem(), pEnum->item.asEnum.offset );
            return &s___msgEnumIndex;
         }
         else if( pMsg == s___msgEnumBase.pDynSym )
         {
            hb_itemCopy( hb_stackReturnItem(), pEnum->item.asEnum.basePtr );
            return &s___msgEnumBase;
         }
         else if( pMsg == s___msgEnumValue.pDynSym )
         {
            hb_itemCopy( hb_stackReturnItem(), hb_itemUnRefOnce( pEnum ) );
            return &s___msgEnumValue;
         }
      }
   }

   /* Default messages here */

   if( pMsg == s___msgWithObjectPush.pDynSym )
   {
      PHB_ITEM pItem = hb_stackWithObjectItem();
      if( pItem )
      {
         /* push current WITH OBJECT object */
         hb_itemCopy( hb_stackReturnItem(), pItem );
         return &s___msgWithObjectPush;
      }
   }
   else if( pMsg == s___msgWithObjectPop.pDynSym )
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

   else if( pMsg == s___msgClassName.pDynSym )
      return &s___msgClassName;

   else if( pMsg == s___msgClassH.pDynSym )
      return &s___msgClassH;

   else if( pMsg == s___msgClassSel.pDynSym )
      return &s___msgClassSel;

/*
   else if( pMsg == s___msgEval.pDynSym )
      return &s___msgEval;

   else if( pMsg == s___msgClsParent.pDynSym )
      return &s___msgClsParent;

   else if( pMsg == s___msgClass.pDynSym )
      return &s___msgClass;
*/
   if( pfPopSuper )
   {
      if( pClass && pClass->pFunError )
         return pClass->pFunError;

      /* remove this line if you want default HVM error message */
      return &s___msgNoMethod;
   }
   return NULL;
}

/*
 * return TRUE if object has a given message
 */
BOOL hb_objHasMessage( PHB_ITEM pObject, PHB_DYNS pMessage )
{
   return hb_objGetMethod( pObject, pMessage->pSymbol, NULL ) != NULL;
}

#ifndef HB_CLS_ENFORCERO
/*
 * This function is only for backward binary compatibility
 * It will be removed in the future so please do not use it.
 * Use hb_objHasMessage() instead.
 */
#if defined(__cplusplus)
   extern "C" BOOL hb_objGetpMethod( PHB_ITEM pObject, PHB_SYMB pMessage );
#endif
BOOL hb_objGetpMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   return hb_objGetMethod( pObject, pMessage, NULL ) != NULL;
}
#endif

#ifdef HB_CLS_ENFORCERO
static PMETHOD hb_objGetpMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   USHORT uiClass;
   PHB_DYNS pMsg = pMessage->pDynSym;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetpMethod(%p, %p)", pObject, pMessage));

   if( pObject->type == HB_IT_ARRAY )
      uiClass = pObject->item.asArray.value->uiClass;
   else
      uiClass = 0;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = s_pClasses + ( uiClass - 1 );
      USHORT uiAt    = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pClass->uiHashKey ) * BUCKET );
      USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

      while( uiAt != uiLimit )
      {
         if( pClass->pMethods[ uiAt ].pMessage == pMsg )
           return (pClass->pMethods + uiAt) ;

         uiAt++;
         if( uiAt == uiMask )
            uiAt = 0;
      }
   }

   return NULL;
}
#endif

static PHB_SYMB hb_objFuncParam( int iParam )
{
   PHB_ITEM pItem = hb_param( iParam, HB_IT_SYMBOL );

   if( pItem )
      return pItem->item.asSymbol.value;

   return NULL;
}

/*
 * Check if object has a given operator
 */
BOOL hb_objHasOperator( PHB_ITEM pObject, USHORT uiOperator )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_objHasOperator(%p,%hu)", pObject, uiOperator));

   if( pObject->type == HB_IT_ARRAY &&
       pObject->item.asArray.value->uiClass != 0 )
   {
      PCLASS pClass = s_pClasses + pObject->item.asArray.value->uiClass - 1;
      return ( pClass->ulOpFlags & ( 1UL << uiOperator ) ) != 0;
   }

   return FALSE;
}

/*
 * Call object operator. If pMsgArg is NULL then operator is unary.
 * Function return TRUE when object class overloads given operator
 * and FALSE otherwise. [druzus]
 */
BOOL hb_objOperatorCall( USHORT uiOperator, HB_ITEM_PTR pResult,
                         PHB_ITEM pObject, PHB_ITEM pMsgArg )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_objOperatorCall(%hu,%p,%p,%p)", uiOperator, pResult, pObject, pMsgArg));

   if( hb_objHasOperator( pObject, uiOperator ) )
   {
      hb_vmPushSymbol( s_opSymbols + uiOperator );
      hb_vmPush( pObject );
      if( HB_IS_COMPLEX( hb_stackReturnItem() ) )
         hb_itemClear( hb_stackReturnItem() );
      else
         hb_stackReturnItem()->type = HB_IT_NIL;
      if( pMsgArg )
      {
         hb_vmPush( pMsgArg );
         hb_vmSend( 1 );
      }
      else
         hb_vmSend( 0 );

      /* store the return value */
      hb_itemCopy( pResult, hb_stackReturnItem() );
      return TRUE;
   }
   return FALSE;
}

/*
 * <bool> = hb_objHasMsg( <pObject>, <szString> )
 *
 * Check whether <szString> is an existing message for object.
 *
 * <uPtr> should be read as a boolean
 */
BOOL hb_objHasMsg( PHB_ITEM pObject, char *szString )
{
   PHB_DYNS pDynSym = hb_dynsymFindName( szString );

   HB_TRACE(HB_TR_DEBUG, ("hb_objHasMsg(%p, %s)", pObject, szString));

   if( pDynSym )
   {
      return hb_objGetMethod( pObject, pDynSym->pSymbol, NULL ) != NULL;
   }
   else
   {
      return FALSE;
   }
}


/* ================================================ */

/*
 * __clsAddMsg( <hClass>, <cMessage>, <pFunction>, <nType>, [xInit], <uiScope>, <lPersistent> )
 *
 * Add a message to the class.
 *
 * <hClass>    Class handle
 * <cMessage>  Message
 * <pFunction> HB_OO_MSG_METHOD    : Pointer to function
 *             HB_OO_MSG_DATA      : Index number in array
 *             HB_OO_MSG_CLASSDATA : Index number in array
 *             HB_OO_MSG_INLINE    : Code block
 *             HB_OO_MSG_SUPER     : Handle of super class
 *
 * <nType>     see HB_OO_MSG_*
 *
 * <xInit>     HB_OO_MSG_DATA      : Optional initializer for DATA
 *             HB_OO_MSG_CLASSDATA : Optional initializer for DATA
 *             HB_OO_MSG_SUPER     : Index number in array (for instance SuperObject)
 *
 * <uiScope>   HB_OO_CLSTP_EXPORTED        1 : default for data and method
 *             HB_OO_CLSTP_PROTECTED       2 : method or data protected
 *             HB_OO_CLSTP_HIDDEN          4 : method or data hidden
 *             HB_OO_CLSTP_CTOR            8 : method constructor
 *             HB_OO_CLSTP_READONLY       16 : data read only
 *             HB_OO_CLSTP_SHARED         32 : (method or) data shared
 *             HB_OO_CLSTP_CLASS          64 : message is the name of a superclass
 *             HB_OO_CLSTP_SUPER         128 : message is herited
 *             HB_OO_CLSTP_CLASSCTOR     256 : Class method constructor
 *             HB_OO_CLSTP_CLASSMETH     512 : Class method
 */


HB_FUNC( __CLSADDMSG )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   USHORT uiScope = ( USHORT ) ( ISNUM( 6 ) ? hb_parni( 6 ) : HB_OO_CLSTP_EXPORTED );
   BOOL   bPersistent = hb_parl( 7 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );

      PHB_DYNS pMessage;
      char *   szMessage = hb_parc( 2 );

      USHORT   uiBucket;

      USHORT   wType    = ( USHORT ) hb_parni( 4 );
      USHORT   uiAt;
      PMETHOD  pNewMeth;

      USHORT   uiOperator;
      PHB_SYMB pOpSym;
      ULONG    ulOpFlags = 0;

      /* translate names of operator overloading messages */
      if      (strcmp("+", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_PLUS )->pDynSym;
      else if (strcmp("-", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_MINUS )->pDynSym;
      else if (strcmp("*", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_MULT )->pDynSym;
      else if (strcmp("/", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_DIVIDE )->pDynSym;
      else if (strcmp("%", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_MOD )->pDynSym;
      else if (strcmp("^", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_POWER )->pDynSym;
      else if (strcmp("**", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_POWER )->pDynSym;
      else if (strcmp("++", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_INC )->pDynSym;
      else if (strcmp("--", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_DEC )->pDynSym;
      else if (strcmp("==", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_EXACTEQUAL )->pDynSym;
      else if (strcmp("=", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_EQUAL )->pDynSym;
      else if (strcmp("!=", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_NOTEQUAL )->pDynSym;
      else if (strcmp("<>", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_NOTEQUAL )->pDynSym;
      else if (strcmp("#", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_NOTEQUAL )->pDynSym;
      else if (strcmp("<", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_LESS )->pDynSym;
      else if (strcmp("<=", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_LESSEQUAL )->pDynSym;
      else if (strcmp(">", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_GREATER )->pDynSym;
      else if (strcmp(">=", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_GREATEREQUAL )->pDynSym;
      else if (strcmp(":=", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_ASSIGN )->pDynSym;
      else if (strcmp("$", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_INSTRING )->pDynSym;
      else if (strcmp("!", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_NOT )->pDynSym;
      else if (hb_stricmp(".NOT.", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_NOT )->pDynSym;
      else if (hb_stricmp(".AND.", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_AND )->pDynSym;
      else if (hb_stricmp(".OR.", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_OR )->pDynSym;
      else if( strcmp("[]", szMessage) == 0)
         pMessage = ( s_opSymbols + HB_OO_OP_ARRAYINDEX )->pDynSym;
      else
         pMessage = hb_dynsymGet( szMessage );

      for( uiOperator = 0, pOpSym = s_opSymbols;
           uiOperator <= HB_OO_MAX_OPERATOR; ++uiOperator, ++pOpSym )
      {
         if( pOpSym->pDynSym == pMessage )
         {
            ulOpFlags |= 1UL << uiOperator;
            break;
         }
      }

      if( wType == HB_OO_MSG_INLINE && hb_param( 3, HB_IT_BLOCK ) == NULL )
      {
         hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSADDMSG", 0 );
         return;
      }

      if( pClass->uiMethods > ( pClass->uiHashKey * BUCKET * 2 / 3 ) )
         hb_clsDictRealloc( pClass );

      do
      {
         uiAt = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMessage ) ) % pClass->uiHashKey ) * BUCKET );

         /* Find either the existing message or an open spot for a new message */
         for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
         {
            if( !pClass->pMethods[ uiAt+uiBucket ].pMessage
                ||
                ( pClass->pMethods[ uiAt+uiBucket ].pMessage == pMessage )
              )
               break;
         }

         if( uiBucket >= BUCKET )
            hb_clsDictRealloc( pClass );

      } while( uiBucket >= BUCKET );

      pNewMeth = pClass->pMethods + ( uiAt + uiBucket );

      if( ! pNewMeth->pMessage )
      {
         pNewMeth->pMessage = pMessage;
         pClass->uiMethods++;           /* One more message */
      }


      pNewMeth->uiSprClass = uiClass  ; /* now used !! */
      pNewMeth->bClsDataInitiated = 0 ; /* reset state */
      pNewMeth->ulCalls = 0;
      pNewMeth->ulTime = 0;
      pNewMeth->ulRecurse = 0;
      pNewMeth->bIsPersistent = bPersistent ? 1 : 0;

      /* in case of re-used message */
      if ( pNewMeth->pInitValue )
      {
         hb_itemRelease(pNewMeth->pInitValue) ;
         pNewMeth->pInitValue = 0 ;
      }

      switch( wType )
      {
         case HB_OO_MSG_METHOD:

            pNewMeth->pFuncSym = hb_objFuncParam( 3 );
            pNewMeth->uiScope = uiScope;
            pNewMeth->uiData = 0;
            break;

         case HB_OO_MSG_DATA:

            pNewMeth->uiData = ( USHORT ) hb_parnl( 3 );
            pNewMeth->uiScope = uiScope;

            if( pMessage->pSymbol->szName[ 0 ] == '_' )
               pNewMeth->pFuncSym   = &s___msgSetData;
            else
            {
               PHB_ITEM pInit = hb_param( 5, HB_IT_ANY );

               pNewMeth->pFuncSym   = &s___msgGetData;

               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  pNewMeth->pInitValue = hb_itemClone( pInit );
               }
            }
            break;

         case HB_OO_MSG_CLASSDATA:

            pNewMeth->uiData  = ( USHORT ) hb_parnl( 3 );
            pNewMeth->uiDataShared = pNewMeth->uiData ;

            pNewMeth->uiScope = uiScope;

            if( ( USHORT ) hb_arrayLen( pClass->pClassDatas ) < pNewMeth->uiData )
              hb_arraySize( pClass->pClassDatas, pNewMeth->uiData );

            if( pMessage->pSymbol->szName[ 0 ] != '_' )
            {
               PHB_ITEM pInit = hb_param( 5, HB_IT_ANY );

               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  pNewMeth->pInitValue = hb_itemClone( pInit );
               }
            }

            if( ( pNewMeth->uiScope & HB_OO_CLSTP_SHARED ) != HB_OO_CLSTP_SHARED )
            {
               if( pMessage->pSymbol->szName[ 0 ] == '_' )
                  pNewMeth->pFuncSym = &s___msgSetClsData;
               else
                  pNewMeth->pFuncSym = &s___msgGetClsData;

            }
            else
            {
               if( pMessage->pSymbol->szName[ 0 ] == '_' )
                {
                  pNewMeth->pFuncSym = &s___msgSetShrData;
                  pClass->uiDatasShared++;
                }
               else
                  pNewMeth->pFuncSym = &s___msgGetShrData;
            }

            break;

         case HB_OO_MSG_INLINE:

            pNewMeth->uiData = ( USHORT ) ( hb_arrayLen( pClass->pInlines ) + 1 );
            pNewMeth->uiScope = uiScope;
            hb_arraySize( pClass->pInlines, pNewMeth->uiData );
            hb_arraySet( pClass->pInlines, pNewMeth->uiData, hb_param( 3, HB_IT_BLOCK ) );
            pNewMeth->pFuncSym = &s___msgEvalInline;
            break;

         case HB_OO_MSG_VIRTUAL:

            pNewMeth->pFuncSym = &s___msgVirtual;
            break;

         case HB_OO_MSG_SUPER:

            pNewMeth->uiData = ( USHORT ) hb_parnl( 3 );
            pNewMeth->uiSprClass = ( USHORT ) hb_parnl( 5 ); /* store the super handel */
            pNewMeth->uiScope = uiScope;
            pNewMeth->pFuncSym = &s___msgSuper;
            break;

         case HB_OO_MSG_ONERROR:

            pClass->pFunError = hb_objFuncParam( 3 );
            break;

         default:

            hb_errInternal( HB_EI_CLSINVMETHOD, NULL, "__clsAddMsg", NULL );
            return;
      }

      pClass->ulOpFlags |= ulOpFlags;
   }
}


/*
 * <hClass> := __clsNew( <cClassName>, <nDatas>, [ahSuper|aoSuper] )
 *
 * Create a new class
 *
 * <cClassName> Name of the class
 * <nDatas>     Number of DATAs in the class
 * <ahSuper>    Optional handle(s) of superclass(es)
 * <aoSuper>    Optional superclass(es) Object instance -
 *              seems it's not implemented
 */
HB_FUNC( __CLSNEW )
{
   PCLASS pNewCls;
   ULONG ulSize;  /* USHORT is small. Maximum 409 methods. In some
                           cases it is enough. This eliminate random GPFs
                           in this function for big classes */

   PHB_ITEM pahSuper;
   USHORT i, uiSuper;
   /*USHORT nLenShrDatas = 0;*/
   USHORT nLenClsDatas = 0;
   USHORT nLenInlines = 0;
   USHORT nLenDatas = 0;

   pahSuper = hb_itemParam( 3 );      /* Replace the initial uiSuper   */
   uiSuper  = ( USHORT ) hb_itemSize( pahSuper ); /* Number of Super class present */

   if( s_pClasses )
      s_pClasses = ( PCLASS ) hb_xrealloc( s_pClasses, sizeof( CLASS ) * ( s_uiClasses + 1 ) );
   else
      s_pClasses = ( PCLASS ) hb_xgrab( sizeof( CLASS ) );

   pNewCls = s_pClasses + s_uiClasses;
   pNewCls->szName = hb_strdup( hb_parc( 1 ) );
   pNewCls->uiDataFirst = 0;
   pNewCls->uiDatas = 0;
   pNewCls->uiMethods = 0;
   pNewCls->uiDatasShared = 0;
   pNewCls->ulOpFlags = 0;

   if( uiSuper )
   {
      for( i = 1; i <= uiSuper; i++ )
      {
         PHB_DYNS pMsg;
         PHB_ITEM pSuper;
         PHB_ITEM pClsAnyTmp;
         USHORT nSuper;
         USHORT ui, uiAt, uiLimit, uiCurrent ;
         PCLASS pSprCls;
         USHORT nLen;
         BOOL bResize ;

         pSuper  =  hb_itemNew( NULL );
         hb_arrayGet( pahSuper, i, pSuper);
         nSuper  = ( USHORT ) hb_itemGetNL( pSuper );
         pSprCls = s_pClasses + ( nSuper - 1 );
         uiLimit = ( USHORT ) ( pSprCls->uiHashKey * BUCKET );

         hb_itemRelease( pSuper );

         pNewCls->uiDataFirst += pSprCls->uiDatas;
         pNewCls->uiDatas      = ( USHORT ) ( pNewCls->uiDataFirst + hb_parni( 2 ) );
         pNewCls->ulOpFlags   |= pSprCls->ulOpFlags;

         if( i == 1 ) /* This is the first superclass */
         {
            pNewCls->uiHashKey = pSprCls->uiHashKey;

            /* CLASS DATA Not Shared ( new array, new value ) */
            pNewCls->pClassDatas  = hb_arrayClone( pSprCls->pClassDatas );

            pNewCls->pInlines = hb_arrayClone( pSprCls->pInlines );

            pNewCls->uiDatasShared = pSprCls->uiDatasShared;

         }
         else
         {
            /* Ok add now the previous len to the offset */
            nLenClsDatas  = ( USHORT ) hb_itemSize( pNewCls->pClassDatas );
            nLenInlines   = ( USHORT ) hb_itemSize( pNewCls->pInlines );
            nLenDatas     = ( USHORT ) pNewCls->uiDatas;

            /* ClassDatas */
            pClsAnyTmp = hb_arrayClone( pSprCls->pClassDatas );
            nLen = ( USHORT ) hb_itemSize( pClsAnyTmp );
            for( ui = 1; ui <= nLen; ui++ )
            {
                PHB_ITEM pTmp = hb_itemNew( NULL );
                hb_arrayGet( pClsAnyTmp, ui, pTmp );
                hb_arrayAdd( pNewCls->pClassDatas, pTmp );
                hb_itemRelease( pTmp );
            }
            hb_itemRelease( pClsAnyTmp );

            /* SharedDatas */
            pNewCls->uiDatasShared += pSprCls->uiDatasShared;

            /* Inlines */
            pClsAnyTmp = hb_arrayClone( pSprCls->pInlines );
            nLen = ( USHORT ) hb_itemSize( pClsAnyTmp );
            for( ui = 1; ui <= nLen; ui++ )
            {
                PHB_ITEM pTmp = hb_itemNew( NULL );
                hb_arrayGet( pClsAnyTmp, ui, pTmp );
                hb_arrayAdd( pNewCls->pInlines, pTmp );
                hb_itemRelease( pTmp );
            }
            hb_itemRelease( pClsAnyTmp );
         }


         /* Now working on pMethods */

         if( i == 1 )
         {
            ulSize = pNewCls->uiHashKey * BUCKET * sizeof( METHOD );
            pNewCls->pMethods = ( PMETHOD ) hb_xgrab( ulSize );
            memset( pNewCls->pMethods, 0, ulSize );
            pNewCls->pFunError = pSprCls->pFunError;
         }

         bResize = ( ( pNewCls->uiMethods + pSprCls->uiMethods ) > ( pNewCls->uiHashKey * BUCKET * 2 / 3 ) ) ;
         uiCurrent = 0 ;

         do
         {

          if( bResize )
          {
            hb_clsDictRealloc( pNewCls );
            bResize=FALSE;
          }

          /* When doing the eventual second pass after call to hb_clsDictRealloc */
          /* We review only messages not already treated */

          for( ui = uiCurrent ; ui < uiLimit; ui++ )
          {
            USHORT uiBucket;

            pMsg = ( PHB_DYNS ) pSprCls->pMethods[ ui ].pMessage;

            if( pMsg )
            {
               uiAt = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pNewCls->uiHashKey ) * BUCKET );

               for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
               {

                  /* Ok, this bucket is empty */
                  if( pNewCls->pMethods[ uiAt+uiBucket ].pMessage == 0 )
                  {

                     /* Now, we can increment the msg count */
                     pNewCls->uiMethods++;

                     hb_xmemcpy(pNewCls->pMethods + ( uiAt+uiBucket ), pSprCls->pMethods + ui, sizeof( METHOD ) );

                     if(
                         pNewCls->pMethods[ uiAt+uiBucket ].pFuncSym == &s___msgSetClsData
                         ||
                         pNewCls->pMethods[ uiAt+uiBucket ].pFuncSym == &s___msgGetClsData
                       )
                        pNewCls->pMethods[ uiAt+uiBucket ].uiData += nLenClsDatas;

                     if(
                         pNewCls->pMethods[ uiAt+uiBucket ].pFuncSym == &s___msgSetData
                         ||
                         pNewCls->pMethods[ uiAt+uiBucket ].pFuncSym == &s___msgGetData
                         ||
                         pNewCls->pMethods[ uiAt+uiBucket ].pFuncSym == &s___msgSuper
                       )
                     {
                       pNewCls->pMethods[ uiAt+uiBucket ].uiData     += nLenDatas;
                     }

                     if( pNewCls->pMethods[ uiAt+uiBucket ].pFuncSym == &s___msgEvalInline )
                        pNewCls->pMethods[ uiAt+uiBucket ].uiData += nLenInlines;

                     if( ( pSprCls->pMethods[ ui ].uiScope & HB_OO_CLSTP_SUPER ) != HB_OO_CLSTP_SUPER )
                        pNewCls->pMethods[ uiAt+uiBucket ].uiScope = ( USHORT ) ( pSprCls->pMethods[ ui ].uiScope + HB_OO_CLSTP_SUPER );
                     else
                        pNewCls->pMethods[ uiAt+uiBucket ].uiScope = pSprCls->pMethods[ ui ].uiScope;

                     if( pSprCls->pMethods[ ui ].pInitValue )
                     {
                        pNewCls->pMethods[ uiAt + uiBucket ].pInitValue =
                           hb_itemClone( pSprCls->pMethods[ ui ].pInitValue );
                     }
                     break;
                  }
                  else if( pNewCls->pMethods[ uiAt + uiBucket ].pMessage == pMsg )
                     break;
               }

               /* No space found for this message, call hb_dicrealloc() */
               if (uiBucket == BUCKET)
                {
                 bResize=TRUE;
                 uiCurrent = ui ;
                 break;
                }

            }

          }

         } while ( ui < uiLimit );
      }
   }
   else
   {
      pNewCls->uiDatas      = ( USHORT ) hb_parni( 2 );
      pNewCls->uiDataFirst  = 0;
      pNewCls->uiDatasShared= 0;

      pNewCls->pMethods     = ( PMETHOD ) hb_xgrab( BASE_METHODS * sizeof( METHOD ) );
      memset( pNewCls->pMethods, 0, BASE_METHODS * sizeof( METHOD ) );

      pNewCls->uiMethods    = 0;
      pNewCls->uiHashKey    = HASH_KEY;

      pNewCls->pClassDatas  = hb_itemArrayNew( 0 );
      pNewCls->pInlines     = hb_itemArrayNew( 0 );
      pNewCls->pFunError    = NULL;
   }
   hb_itemRelease( pahSuper );

   hb_retni( ++s_uiClasses );
}


/*
 * __clsDelMsg( <oObj>, <cMessage> )
 *
 * Delete message (only for INLINE and METHOD)
 *
 * <oObj>     Object
 * <cMessage> Message
 */
HB_FUNC( __CLSDELMSG )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS pClass  = s_pClasses + ( uiClass - 1 );
         USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         USHORT uiAt    = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pClass->uiHashKey ) * BUCKET );
         USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

         while( ( uiAt != uiLimit ) &&
                ( pClass->pMethods[ uiAt ].pMessage &&
                ( pClass->pMethods[ uiAt ].pMessage != pMsg ) ) )
         {
            uiAt++;
            if( uiAt == uiMask )
               uiAt = 0;
         }
         if( uiAt != uiLimit )
         {                                         /* Requested method found   */
            PHB_SYMB pFuncSym = pClass->pMethods[ uiAt ].pFuncSym;

            if( pFuncSym == &s___msgEvalInline )    /* INLINE method deleted    */
            {
               hb_arrayDel( pClass->pInlines, pClass->pMethods[ uiAt ].uiData );
                                                   /* Delete INLINE block      */
            }
                                                /* Move messages            */
            while( pClass->pMethods[ uiAt ].pMessage && uiAt != uiLimit )
            {
               hb_xmemcpy( pClass->pMethods + uiAt, pClass->pMethods + ( uiAt == uiMask ? 0 : uiAt + 1 ), sizeof( METHOD ) );
               uiAt++;

               if( uiAt == uiMask )
               {
                  uiAt = 0;
               }
            }
            memset( pClass->pMethods + uiAt, 0, sizeof( METHOD ) );
            pClass->uiMethods--;                    /* Decrease number messages */
         }
      }
   }
}


/*
 * <oNewObject> := __clsInst( <hClass> )
 *
 * Create a new object from class definition <hClass>
 */
HB_FUNC( __CLSINST )
{
   PHB_ITEM pSelf ;

   pSelf = hb_clsInst( ( USHORT ) hb_parni( 1 ));

   if( pSelf )
   {
      hb_itemRelease( hb_itemReturn( pSelf ) );
   }

}

/*
 * [<o(Super)Object>] := hb_clsInst( <hClass> )
 *
 * Create a (super)object from class definition <hClass>
 */
static PHB_ITEM hb_clsInst( USHORT uiClass )
{
   PHB_ITEM pSelf = NULL;

   if( uiClass <= s_uiClasses )
   {
      PCLASS   pClass = s_pClasses + ( uiClass - 1 );

      USHORT   uiAt;
      USHORT   uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      PMETHOD  pMeth ;

      pSelf = hb_itemNew( NULL );
      hb_arrayNew( pSelf, pClass->uiDatas );

      pSelf->item.asArray.value->uiClass    = uiClass;
      pSelf->item.asArray.value->uiPrevCls  = 0;

      pSelf->item.asArray.value->puiClsTree   = NULL;

      /* Initialise value if initialisation was requested */
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
      {

         /* Init Classdata (inherited and not) if needed */
         if( pMeth->pInitValue )
         {

            if( pMeth->pFuncSym == &s___msgGetClsData && !( pMeth->bClsDataInitiated ) )
            {
               PHB_ITEM pInit;

               pInit = hb_arrayGetItemPtr( pClass->pClassDatas, pMeth->uiData );
               if( HB_IS_NIL( pInit ) )
               {
                  pInit = hb_itemClone( pMeth->pInitValue );

                  hb_arraySet( pClass->pClassDatas, pMeth->uiData, pInit );
                  hb_itemRelease( pInit );
                  pMeth->bClsDataInitiated = 1;
               }
            }
            else if( pMeth->pFuncSym == &s___msgGetData ) /* is a DATA but not herited */
            {
               PHB_ITEM pInit = hb_itemClone( pMeth->pInitValue );

               hb_arraySet( pSelf, pMeth->uiData, pInit );
               hb_itemRelease( pInit );
            }
            else if( pMeth->pFuncSym == &s___msgGetShrData && !( pMeth->bClsDataInitiated ) )
            {
               /* Init Shared Classdata as needed, we only need to init the first */
               /* not inherited classdata array where all shared will point to    */
               PHB_ITEM pInit;

               pInit = hb_arrayGetItemPtr( pClass->pClassDatas, pMeth->uiData );
               if( HB_IS_NIL( pInit ) )
               {
                  pInit = hb_itemClone( pMeth->pInitValue );
                  hb_arraySet( pClass->pClassDatas, pMeth->uiData, pInit );
                  hb_itemRelease( pInit );
                  pMeth->bClsDataInitiated = 1;
               }
            }
         }
      }
   }

   return pSelf;
}

/*
 * __clsModMsg( <oObj>, <cMessage>, <pFunc> )
 *
 * Modify message (only for INLINE and METHOD)
 */
HB_FUNC( __CLSMODMSG )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
         USHORT   uiAt     = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pClass->uiHashKey ) * BUCKET );
         USHORT   uiMask   = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         USHORT   uiLimit  = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

         while( ( uiAt != uiLimit ) &&
                ( pClass->pMethods[ uiAt ].pMessage &&
                ( pClass->pMethods[ uiAt ].pMessage != pMsg ) ) )
         {
            uiAt++;
            if( uiAt == uiMask )
               uiAt = 0;
         }

         if( uiAt != uiLimit )
         {                                         /* Requested method found   */
            PHB_SYMB pFuncSym = pClass->pMethods[ uiAt ].pFuncSym;

            if( pFuncSym == &s___msgEvalInline )      /* INLINE method changed    */
            {
               PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK );

               if( pBlock == NULL )
               {
                  hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSMODMSG", 0 );
               }
               else
               {
                  hb_arraySet( pClass->pInlines, pClass->pMethods[ uiAt ].uiData, pBlock );
               }
            }
            else if( pFuncSym == &s___msgSetData || pFuncSym == &s___msgGetData )
            {                                      /* Not allowed for DATA     */
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DATA item", "__CLSMODMSG", 0 );
            }
            else                                   /* Modify METHOD            */
            {
               pClass->pMethods[ uiAt ].pFuncSym = hb_objFuncParam( 3 );
            }
         }
      }
   }
}


/*
 * <cClassName> := ClassName( <hClass> )
 *
 * Returns class name of <hClass>
 */
HB_FUNC( __OBJGETCLSNAME )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   USHORT uiClass;

   if( pObject && pObject->item.asArray.value->uiClass )
   {
      uiClass = pObject->item.asArray.value->uiClass;

      hb_retc( s_pClasses[ uiClass - 1 ].szName );
   }
   else
   {
      uiClass = ( USHORT ) hb_parni( 1 );

      if( uiClass <= s_uiClasses )
         hb_retc( s_pClasses[ uiClass - 1 ].szName );
      else
         hb_retc( "" );
   }
}


/*
 * <lRet> := __objHasMsg( <oObj>, <cSymbol> )
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
HB_FUNC( __OBJHASMSG )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( pObject && pString )
   {
      hb_retl( hb_objHasMsg( pObject, pString->item.asString.value ) );
   }
   else
   {
      /*hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJHASMSG", 0 );*/
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "__ObjHasMsg", HB_ERR_ARGS_BASEPARAMS );
   }
}


/*
 * <oNew> := __objClone( <oOld> )
 *
 * Clone an object. Note the similarity with aClone ;-)
 */
HB_FUNC( __OBJCLONE )
{
   PHB_ITEM pSrcObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pDstObject ;

   if( pSrcObject )
   {
      pDstObject= hb_arrayClone( pSrcObject );

      /* pDstObject->item.asArray.value->puiClsTree = NULL; */
      /* pDstObject->item.asArray.value->puiClsTree = ( USHORT * ) hb_xgrab( sizeof( USHORT ) ); */
      /* pDstObject->item.asArray.value->puiClsTree[0]=0; */

      hb_itemRelease( hb_itemReturn( pDstObject ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3001, NULL, "__OBJCLONE", 0 );
   }
}

void hb_objSendMsg( PHB_ITEM pObject, char *sMsg, ULONG ulArg, ... )
{
   hb_vmPushSymbol( hb_dynsymGet( sMsg )->pSymbol );
   hb_vmPush( pObject );
   if( ulArg )
   {
      unsigned long i;
      va_list ap;

      va_start( ap, ulArg );
      for( i = 0; i < ulArg; i++ )
      {
         hb_vmPush( va_arg( ap, PHB_ITEM ) );
      }
      va_end( ap );
   }
   hb_vmSend( (USHORT) ulArg );
}

void hb_objSendMessage( PHB_ITEM pObject, PHB_DYNS pMsgSym, ULONG ulArg, ... )
{
   if( pObject && pMsgSym )
   {
      hb_vmPushSymbol( pMsgSym->pSymbol );
      hb_vmPush( pObject );

      if( ulArg )
      {
         unsigned long i;
         va_list ap;

         va_start( ap, ulArg );
         for( i = 0; i < ulArg; i++ )
         {
            hb_vmPush( va_arg( ap, PHB_ITEM ) );
         }
         va_end( ap );
      }
      hb_vmSend( (USHORT) ulArg );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__ObjSendMessage()", 0 );
   }
}

/*
 * <xRet> = __objSendMsg( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a message to an object
 */
HB_FUNC( __OBJSENDMSG )
{
   PHB_ITEM pObject  = hb_param( 1, HB_IT_OBJECT );
   char * szMsg = hb_parc( 2 );

   if( pObject && szMsg && *szMsg )    /* Object & message passed */
   {
      USHORT uiPCount = hb_pcount();
      USHORT uiParam;

      hb_vmPushSymbol( hb_dynsymGet( szMsg )->pSymbol );    /* Push message symbol */
      hb_vmPush( pObject );                                 /* Push object */

      for( uiParam = 3; uiParam <= uiPCount; ++uiParam )    /* Push arguments on stack */
      {
         hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
      }

      hb_vmSend( ( USHORT ) ( uiPCount - 2 ) );             /* Execute message */
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJSENDMSG", 0 );
   }
}

/*
 * <hClass> := __clsInstSuper( <cName> )
 *
 * Instance super class and return class handle
 */
HB_FUNC( __CLSINSTSUPER )
{
   char * szString = hb_parc( 1 );
   BOOL bFound = FALSE;

   if( szString && *szString )
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( szString );

      if( pDynSym )                                   /* Find function            */
      {
         USHORT uiClass;

         hb_vmPushSymbol( pDynSym->pSymbol );         /* Push function name       */
         hb_vmPushNil();
         hb_vmFunction( 0 );                          /* Execute super class      */

         if( HB_IS_OBJECT( hb_stackReturnItem() ) )
         {
            for( uiClass = 0; ! bFound && uiClass < s_uiClasses; uiClass++ )
            {                                         /* Locate the entry         */
               if( hb_stricmp( szString , s_pClasses[ uiClass ].szName ) == 0 )
               {
                  hb_retni( uiClass + 1 );            /* Entry + 1 = hb___msgClsH    */
                  bFound = TRUE;
               }
            }
         }
         else
         {
            hb_errRT_BASE( EG_ARG, 3002, "Super class does not return an object", "__CLSINSTSUPER", 0 );
         }
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 3003, "Cannot find super class", "__CLSINSTSUPER", 0 );
      }
   }

   if( ! bFound )
   {
      hb_retni( 0 );
   }
}

/*
 * <nSeq> = __cls_CntClsData( <hClass> )
 *
 * Return number of class datas
 */
HB_FUNC( __CLS_CNTCLSDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   hb_retni( uiClass && uiClass <= s_uiClasses ?
                  hb_arrayLen( s_pClasses[ uiClass - 1 ].pClassDatas ) : 0 );
}


/*
 * <nSeq> = __cls_CntData( <hClass> )
 *
 * Return number of datas
 */
HB_FUNC( __CLS_CNTDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   hb_retni( uiClass && uiClass <= s_uiClasses ?
             s_pClasses[ uiClass - 1 ].uiDatas : 0 );
}


/*
 * <nSeq> = __cls_DecData( <hClass> )
 *
 * Return number of datas and decrease
 */
HB_FUNC( __CLS_DECDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses && s_pClasses[ uiClass - 1 ].uiDatas )
      hb_retni( s_pClasses[ uiClass - 1 ].uiDatas-- );
   else
      hb_retni( 0 );
}


/*
 * <nSeq> = __cls_IncData( <hClass> )
 *
 * Return number of datas and increase
 */
HB_FUNC( __CLS_INCDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
      /* TOFIX: fix the description or change preincrementation to postinc */
      hb_retni( ++s_pClasses[ uiClass - 1 ].uiDatas );
   else
      hb_retni( 0 );
}

/* NOTE: Undocumented Clipper function */

/* see for parameter compatibility with Clipper. */
HB_FUNC( __CLASSNEW )
{
   HB_FUNCNAME( __CLSNEW )();
}


/* NOTE: Undocumented Clipper function */

HB_FUNC( __CLASSINSTANCE )
{
   HB_FUNCNAME( __CLSINST )();
}


/* NOTE: Undocumented Clipper function */

HB_FUNC( __CLASSADD )
{
   HB_FUNCNAME( __CLSADDMSG )();
}


/* NOTE: Undocumented Clipper function */

HB_FUNC( __CLASSNAME )
{
   HB_FUNCNAME( __OBJGETCLSNAME )();
}

/* NOTE: Undocumented Clipper function */
/* NOTE: Based on hb___msgClsSel() */

HB_FUNC( __CLASSSEL )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiPos = 0;
      USHORT uiAt;

      hb_arrayNew( pReturn, pClass->uiMethods ); /* Create a transfer array */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;
         if( pMessage )                         /* Hash Entry used ?        */
         {
            PHB_ITEM pItem = hb_arrayGetItemPtr( pReturn, ++uiPos );
            if( pItem )
               hb_itemPutC( pItem, pMessage->pSymbol->szName );
            else
               break;  /* Generate internal error? */
         }
      }
      if( uiPos < pClass->uiMethods )
         hb_arraySize( pReturn, uiPos );
   }

   hb_itemRelease( hb_itemReturn( pReturn ) );
}

/* to be used from Classes ERROR HANDLER method */
HB_FUNC( __GETMESSAGE )
{
   hb_retc( hb_stackItem( hb_stackBaseItem()->item.asSymbol.stackstate->lBaseItem )->item.asSymbol.value->szName );
}

HB_FUNC( __CLSPARENT )
{
   hb_retl( hb_clsIsParent( hb_parni( 1 ) , hb_parc( 2 ) ) );
}

HB_FUNC( __SENDER )
{
   USHORT iLevel = 3;
   PHB_ITEM oSender = NULL;
   LONG lOffset = hb_stackBaseOffset();

   while( iLevel > 0 && lOffset > 1 )
   {
      lOffset = hb_stackItem( lOffset )->item.asSymbol.stackstate->lBaseItem;
      oSender = hb_stackItem( lOffset + 1 );

      if( ( iLevel-- == 2 && oSender->type != HB_IT_BLOCK ) || oSender->type == HB_IT_NIL )
         break;
   }

   if( iLevel == 0 && oSender != NULL && oSender->type == HB_IT_OBJECT )
   {
      hb_itemReturn( oSender );
   }
}

/*
 * Added by RâC&JfL
 *
 * based on hb___msgClsH( void )
 */
HB_FUNC( __CLASSH )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );

   hb_retni( pObject ? pObject->item.asArray.value->uiClass : 0 );
}

/*
 * based on hb___msgEval( void )
 */
HB_FUNC( __EVAL )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ANY );
   USHORT uiPCount = hb_pcount();

   if( pObject && HB_IS_BLOCK( pObject ) )
   {
      USHORT uiParam;

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pObject );                /* Push block              */
      for( uiParam = 1; uiParam <= uiPCount; ++uiParam )
         hb_vmPush( hb_stackItemFromBase( uiParam ) );

      hb_vmDo( ( USHORT ) uiPCount );
   }
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, "EVAL", pObject ? 1 : 0, pObject );
}

/* ================================================ */

/*
 * <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
static HARBOUR hb___msgClsH( void )
{
   if( HB_IS_ARRAY( hb_stackSelfItem() ) )
      hb_retni( ( hb_stackSelfItem() )->item.asArray.value->uiClass );
   else
      hb_retni( 0 );
}


/*
 * <cClassName> := <obj>:ClassName()
 *
 * Return class name of <obj>. Can also be used for all types.
 */
static HARBOUR hb___msgClsName( void )
{
   PHB_ITEM pItemRef = hb_stackSelfItem();

   if( HB_IS_BYREF( pItemRef ) )            /* Variables by reference   */
      pItemRef = hb_itemUnRef( pItemRef );

   hb_retc( hb_objGetClsName( pItemRef ) );
}


/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
static HARBOUR hb___msgClsSel( void )
{
   PHB_ITEM pSelf = hb_stackSelfItem();
   USHORT uiClass;

   if( HB_IS_BYREF( pSelf ) )
      pSelf =  hb_itemUnRef( pSelf );

   uiClass = hb_objGetClass( pSelf );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PHB_ITEM pReturn = hb_itemNew( NULL );
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiPos = 0, uiAt;
      USHORT nParam;

      nParam = hb_pcount() > 0 ? ( USHORT ) hb_parni( 1 ) : HB_MSGLISTALL;
      hb_arrayNew( pReturn, pClass->uiMethods );

      for( uiAt = 0; uiAt < uiLimit && uiPos < pClass->uiMethods; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

         if( pMessage )                         /* Hash Entry used ?        */
         {
            PMETHOD pMethod = pClass->pMethods + uiAt;

            if( ( nParam == HB_MSGLISTALL )  ||
                ( nParam == HB_MSGLISTCLASS &&
                  (
                    ( pMethod->pFuncSym == &s___msgSetClsData ) ||
                    ( pMethod->pFuncSym == &s___msgGetClsData ) ||
                    ( pMethod->pFuncSym == &s___msgSetShrData ) ||
                    ( pMethod->pFuncSym == &s___msgGetShrData )
                  )
                ) ||
                ( nParam == HB_MSGLISTPURE &&
                  !(
                    ( pMethod->pFuncSym == &s___msgSetClsData ) ||
                    ( pMethod->pFuncSym == &s___msgGetClsData ) ||
                    ( pMethod->pFuncSym == &s___msgSetShrData ) ||
                    ( pMethod->pFuncSym == &s___msgGetShrData )
                   )
                )
              )
            {
               hb_itemPutC( hb_arrayGetItemPtr( pReturn, ++uiPos ),
                            pMessage->pSymbol->szName );
            }
         }
      }
      if( uiPos < pClass->uiMethods )
         hb_arraySize( pReturn, uiPos );
      hb_itemRelease( hb_itemReturn( pReturn ) );
   }
   else
      hb_ret();
}

#if 0

/*
 * __msgClass()
 *
 * Internal function to return Self at Self:Class call (classy compatibility)
 */
static HARBOUR hb___msgClass( void )
{
   hb_itemReturn( hb_stackSelfItem() );
}

/* Added by JfL&RaC
 * <logical> <= <obj>:IsDerivedFrom( xParam )
 *
 * Return true if <obj> is derived from xParam.
 * xParam can be either an obj or a classname
 */
static HARBOUR hb___msgClsParent( void )
{
   PHB_ITEM pItemRef;
   PHB_ITEM pItemParam;
   char * szParentName = 0;
   USHORT uiClass, i;
   BOOL lClass=FALSE;

   if( HB_IS_BYREF( hb_stackSelfItem() ) )            /* Variables by reference   */
      pItemRef = hb_itemUnRef( hb_stackSelfItem() );
   else
      pItemRef = hb_stackSelfItem();

   uiClass = pItemRef->item.asArray.value->uiClass;

   pItemParam = hb_stackItemFromBase( 1 );

   if( HB_IS_OBJECT( pItemParam ) )
      szParentName = hb_objGetClsName( pItemParam );
   else if( HB_IS_STRING( pItemParam ) )
   {
      szParentName = hb_itemGetC( pItemParam );
      lClass=TRUE;
   }

   for( i = 0; szParentName[ i ] != '\0'; i++ )
      szParentName[ i ] = ( char ) toupper( szParentName[ i ] );

   hb_retl( hb_clsIsParent( uiClass , szParentName ) );

   if (lClass)
      hb_itemFreeC( szParentName );
}

#endif


/*
 * __msgEvalInline()
 *
 * Internal function executed for inline methods
 */
static HARBOUR hb___msgEvalInline( void )
{
   USHORT uiClass = ( hb_stackSelfItem() )->item.asArray.value->uiClass;
   USHORT uiParam;
   USHORT uiPCount = hb_pcount();

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( hb_arrayGetItemPtr( s_pClasses[ uiClass - 1 ].pInlines,
                                  s_pMethod->uiData ) );
   hb_vmPush( hb_stackSelfItem() );              /* Push self                */

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_vmPush( hb_stackItemFromBase( uiParam ) );
   }

   hb_vmDo( ( USHORT ) ( uiPCount + 1 ) );       /* Self is also an argument */
}

/*
 * __msgEval()
 *
 * Internal function for the internal EVAL method.
 */
static HARBOUR hb___msgEval( void )
{
   HB_ITEM_PTR pSelf = hb_stackSelfItem();

   if( HB_IS_BLOCK( pSelf ) )
   {
      USHORT uiParam;
      USHORT uiPCount = hb_pcount();

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pSelf );
      for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
         hb_vmPush( hb_stackItemFromBase( uiParam ) );

      hb_vmDo( ( USHORT ) uiPCount );
   }
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, "EVAL", HB_ERR_ARGS_SELFPARAMS );
}

/*
 * __msgNoMethod()
 *
 * Internal function for generating error when not existing message is sent
 */
static HARBOUR hb___msgNoMethod( void )
{
   PHB_SYMB pSym = hb_itemGetSymbol( hb_stackBaseItem() );

#if 1  /* Clipper compatible error message */
   if( pSym->szName[ 0 ] == '_' )
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1, HB_ERR_ARGS_SELFPARAMS );
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, pSym->szName, HB_ERR_ARGS_SELFPARAMS );
#else
   char szDesc[ 128 ];

   if( pSym->szName[ 0 ] == '_' )
   {
      sprintf( szDesc, "Class: '%s' has no property", hb_objGetClsName( hb_stackSelfItem() ) );
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, szDesc, pSym->szName + 1, HB_ERR_ARGS_BASEPARAMS );
   }
   else
   {
      sprintf( szDesc, "Class: '%s' has no exported method", hb_objGetClsName( hb_stackSelfItem() ) );
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, szDesc, pSym->szName, HB_ERR_ARGS_BASEPARAMS );
   }
#endif
}

/*
 * __msgSuper()
 *
 * Internal function to return a superobject
 */
/*
static HARBOUR hb___msgSuper( void )
{
   PHB_ITEM pObject = hb_stackSelfItem();

   pObject->item.asArray.value->uiPrevCls  = pObject->item.asArray.value->uiClass; / * backup of actual handel * /
   pObject->item.asArray.value->uiClass    = s_pMethod->uiSprClass;                / * superclass handel casting * /

   hb_itemReturn( pObject );
}
*/

static HARBOUR hb___msgSuper( void )
{
   PHB_ITEM pObject = hb_stackSelfItem();
   /*ULONG ulLen = pObject->item.asArray.value->ulLen; */
   PHB_ITEM pCopy = hb_itemArrayNew(1);

   /* Now save the Self object as the 1st elem. */
   hb_itemArrayPut( pCopy, 1 , pObject );

   /* Or Store original object as 1st elem */
   /* hb_itemCopy( pCopy->item.asArray.value->pItems , pObject) ; */

   /* And transform it into a fake object */
   pCopy->item.asArray.value->uiPrevCls  = pObject->item.asArray.value->uiClass; /* backup of actual handel */
   pCopy->item.asArray.value->uiClass    = s_pMethod->uiSprClass;                /* superclass handel casting */
   pCopy->item.asArray.value->puiClsTree = NULL ;

   hb_itemRelease( hb_itemReturn( pCopy ) );
}

/*
 * __msgGetClsData()
 *
 * Internal function to return a CLASSDATA
 */
static HARBOUR hb___msgGetClsData( void )
{
   USHORT uiClass = ( hb_stackSelfItem() )->item.asArray.value->uiClass;

   if( uiClass && uiClass <= s_uiClasses )
      hb_arrayGet( s_pClasses[ uiClass - 1 ].pClassDatas, s_pMethod->uiData, hb_stackReturnItem() );
}


/*
 * __msgSetClsData()
 *
 * Internal function to set a CLASSDATA
 */
static HARBOUR hb___msgSetClsData( void )
{
   USHORT uiClass = ( hb_stackSelfItem() )->item.asArray.value->uiClass;

   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      hb_arraySet( s_pClasses[ uiClass - 1 ].pClassDatas, s_pMethod->uiData, pReturn );
   }

   hb_itemReturn( pReturn );
}

/*
 * __msgGetShrData()
 *
 * Internal function to return a SHAREDDATA
 */
static HARBOUR hb___msgGetShrData( void )
{
   USHORT uiSprCls = s_pMethod->uiSprClass;

   if( uiSprCls && uiSprCls <= s_uiClasses )
      hb_arrayGet( s_pClasses[ uiSprCls - 1 ].pClassDatas, s_pMethod->uiDataShared, hb_stackReturnItem() );
}

/*
 * __msgSetShrData()
 *
 * Internal function to set a SHAREDDATA
 */
static HARBOUR hb___msgSetShrData( void )
{
   USHORT uiSprCls = s_pMethod->uiSprClass;

   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );

   if( uiSprCls && uiSprCls <= s_uiClasses )
   {
      hb_arraySet( s_pClasses[ uiSprCls - 1 ].pClassDatas, s_pMethod->uiDataShared, pReturn );
   }

   hb_itemReturn( pReturn );
}

/*
 * __msgGetData()
 *
 * Internal function to return a DATA
 */
static HARBOUR hb___msgGetData( void )
{
   PHB_ITEM pObject = hb_stackSelfItem();
   USHORT uiIndex = s_pMethod->uiData;

   /* will arise only if the class has been modified after first instance */
   if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) ) /* Resize needed */
      hb_arraySize( pObject, uiIndex ); /* Make large enough */

   hb_arrayGet( pObject, uiIndex, hb_stackReturnItem() );
}

/*
 * __msgSetData()
 *
 * Internal function to set a DATA
 */
static HARBOUR hb___msgSetData( void )
{
   PHB_ITEM pObject = hb_stackSelfItem();
   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );
   USHORT uiIndex = s_pMethod->uiData;

   /* will arise only if the class has been modified after first instance */
   if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) ) /* Resize needed ? */
   {
      hb_arraySize( pObject, uiIndex ); /* Make large enough */
   }

   hb_arraySet( pObject, uiIndex, pReturn );

   hb_itemReturn( pReturn );
}

/* No comment :-) */
static HARBOUR hb___msgVirtual( void )
{
   /* hb_ret(); */ /* NOTE: It's safe to comment this out */
   ;
}

static HARBOUR hb___msgNull( void )
{
   ;
}


/* NOTE: Used by the preprocessor to implement Classy compatibility to Harbour
         Receive an variable number of param and return an array of it.
         No param will return a NULL array */


HB_FUNC( __CLS_PARAM )
{
   PHB_ITEM array;
   USHORT uiParam = ( USHORT ) hb_pcount();
   USHORT n;

   if( uiParam >= 1 )
   {
      array = hb_itemArrayNew( uiParam );
      for( n = 1; n <= uiParam; n++ )
      {
         PHB_ITEM iTmp = hb_itemParam( n );
         hb_itemArrayPut( array, n, iTmp );
         hb_itemRelease( iTmp );
      }
   }
   else
   {
      PHB_ITEM iTmp = hb_itemPutC( NULL, (char *) "HBObject" );
      array = hb_itemArrayNew( 1 );
      hb_itemArrayPut( array, 1, iTmp );
      hb_itemRelease( iTmp );
   }

   hb_itemRelease( hb_itemReturn( array ) );
}

/* This one is used when HB_NOTOBJECT is defined before HBCLASS.CH */
/* it will avoid any default object to be inherited */
HB_FUNC( __CLS_PAR00 )
{
   PHB_ITEM array;
   USHORT uiParam = ( USHORT ) hb_pcount();
   USHORT n;

   array = hb_itemArrayNew( uiParam );
   for( n = 1; n <= uiParam; n++ )
    {
         PHB_ITEM iTmp = hb_itemParam( n );
         hb_itemArrayPut( array, n, iTmp );
         hb_itemRelease( iTmp );
    }

   hb_itemRelease( hb_itemReturn( array ) );
}

#ifndef HB_NO_PROFILER
/* profiler: It provides to the HVM the just requested method pointer */
void * hb_mthRequested( void )
{
   return ( void * ) s_pMethod;
}

void hb_mthAddTime( void * pMethod, ULONG ulClockTicks )
{
   if( pMethod != NULL )
   {
      ( ( PMETHOD ) pMethod )->ulCalls++; /* Profiler */
      ( ( PMETHOD ) pMethod )->ulTime += ulClockTicks;
   }
}
#endif

HB_FUNC( __GETMSGPRF ) /* profiler: returns a method called and consumed times */
                       /* ( nClass, cMsg ) --> aMethodInfo { nTimes, nTime } */
{
#ifndef HB_NO_PROFILER
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   char * cMsg    = hb_parc( 2 );

   hb_reta( 2 );
   if( uiClass && uiClass <= s_uiClasses && cMsg && *cMsg )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( cMsg );

      if( pMsg )
      {
         PCLASS pClass  = s_pClasses + ( uiClass - 1 );
         USHORT uiAt    = ( USHORT ) ( ( hb_cls_MsgToNum( pMsg ) % pClass->uiHashKey ) * BUCKET );
         USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );
         PMETHOD pMethod;

         while( uiAt != uiLimit )
         {
            if( pClass->pMethods[ uiAt ].pMessage->pSymbol->pDynSym = pMsg )
            {
               pMethod = pClass->pMethods + uiAt;
               hb_stornl( pMethod->ulCalls, -1, 1 );
               hb_stornl( pMethod->ulTime, -1, 2 );
               return;
            }
            uiAt++;
            if( uiAt == uiMask )
               uiAt = 0;
         }
      }
   }
#else
   hb_reta( 2 );
#endif
   hb_stornl( 0, -1, 1 );
   hb_stornl( 0, -1, 2 );
}

/* __ClsGetProperties( nClassHandle ) --> aPropertiesNames
 * Notice that this function works quite similar to __CLASSSEL()
 * except that just returns the name of the datas and methods
 * that have been declared as PROPERTY (or PERSISTENT) */

HB_FUNC( __CLSGETPROPERTIES )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiAt;

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( 0 );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

         if( pMessage && pClass->pMethods[ uiAt ].bIsPersistent ) /* Hash Entry used ? */
         {
            PHB_ITEM pItem = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                                                /* Add to array */
            hb_arrayAdd( pReturn, pItem );
            hb_itemRelease( pItem );
         }
      }
   }

   hb_itemRelease( hb_itemReturn( pReturn ) );
}

/* Real dirty function, though very usefull under certain circunstances:
 * It allows to change the class handle of an object into another class handle,
 * so the object behaves like a different Class of object.
 * Based on objects.lib SetClsHandle() */

HB_FUNC( HB_SETCLSHANDLE ) /* ( oObject, nClassHandle ) --> nPrevClassHandle */
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   USHORT uiPrevClassHandle = 0;

   if( pObject )
   {
      uiPrevClassHandle = pObject->item.asArray.value->uiClass;
      pObject->item.asArray.value->uiClass = ( USHORT ) hb_parnl( 2 );
   }

   hb_retnl( uiPrevClassHandle );
}

/* Harbour equivalent for Clipper internal __mdCreate() */
USHORT hb_clsCreate( USHORT usSize, char * szClassName )
{
   PHB_DYNS pDynSym = hb_dynsymGet( "__CLSNEW" );

   hb_vmPushSymbol( pDynSym->pSymbol );
   hb_vmPushNil();
   hb_vmPushString( szClassName, strlen( szClassName ) );
   hb_vmPushLong( usSize );
   hb_vmFunction( 2 );

   return ( USHORT ) hb_parnl( -1 );
}

/* Harbour equivalent for Clipper internal __mdAdd() */
void hb_clsAdd( USHORT usClassH, char * szMethodName, PHB_SYMB pFuncSym )
{
   PHB_DYNS pDynSym = hb_dynsymGet( "__CLSADDMSG" );

   hb_vmPushSymbol( pDynSym->pSymbol );
   hb_vmPushNil();
   hb_vmPushLong( usClassH );
   hb_vmPushString( szMethodName, strlen( szMethodName ) );
   hb_vmPushSymbol( pFuncSym );
   hb_vmFunction( 3 );
}

/* Harbour equivalent for Clipper internal __mdAssociate() */
void hb_clsAssociate( USHORT usClassH )
{
   PHB_DYNS pDynSym = hb_dynsymGet( "__CLSINST" );

   hb_vmPushSymbol( pDynSym->pSymbol );
   hb_vmPushNil();
   hb_vmPushLong( usClassH );
   hb_vmFunction( 1 );
}
