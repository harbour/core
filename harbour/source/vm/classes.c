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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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
 * Copyright 2000  JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
 *    hb_clsDictRealloc()   New version
 *
 * Copyright 2000 JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
 *    Now support of shared and not shared class data
 *    Multiple inheritence fully implemented
 *    Multiple datas declaration fully supported
 *    Super msg correctly respond by a super object
 *    Scoping : working for protected, hidden and readonly
 *    Define of HB_MASKHIDDEN allow subclass to not inherit of hidden message
 *    This implie a message not found error in place of protection error hidden
 *
 *    06/07/2000
 *    Now, Each object instance will hold one object instance of all its parents
 *    The first one has the same pointer as SUPER and __SUPER msgs.
 *    See hb___msgSuper()
 *    Each inherited data will now has a pointer linked to it's original object's position
 *
 *    hb___msgGetShrData()
 *    hb___msgSetShrData()
 *    hb___msgClsParent()
 *    __CLS_PARAM() (Now, all class inherit automaticly from TObject Class)
 *    __CLSPARENT()
 *    __SENDER()
 *    __CLSINST() (Call to hb_clsInst())
 *    hb_cls_MsgToNum() (New Hashing method to allow a better use of buckets)
 *    hb_clsIsParent()
 *    hb_clsScope()
 *    hb_clsInst() (Mostly new one, called recursively)
 *    hb___msgSuper() (New one)
 *    hb___msgSetData() (Enhanced regarding herited datas)
 *    hb___msgGetData() (Enhanced regarding herited datas)
 *
 *    ...and many minors (and not so minors ;-) modifications ( for TObject by ex.)
 *
 *    1.16 06/13/2000 JFL&RAC
 *    Initialisation is now working correctly
 *    as with autoinit for Logical (.F.) and Numerical (0) from tClass.prg
 *
 *    1.17 06/14/2000 JFL&RAC
 *    temporary workAround for Self bad referenced when calling super object
 *    hb___msgSuper() temporary modified
 *    hb___msgClass() implemented to allow a better compatibility with classy
 *    Now, calling Self:xClassDataVar is the same as Self:Class:xClassDataVar
 *
 *    1.18 06/??/2000 ?
 *    1.19 06/??/2000 ?
 *
 *    1.20 06/23/2000 JFL&RAC
 *    Correction made relative to CLASSDATA SHARED !
 *    Completly new method
 *
 *    1.21 06/23/2000 JFL&RAC
 *    Correction made relative to CLASSDATA SHARED !
 *    Fixed init when redefining on subclass
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hboo.ch"

#include <ctype.h>

/* DEBUG only*/
/*#include <windows.h>*/

typedef struct
{
   PHB_DYNS pMessage;          /* Method Symbolic name */
   PHB_FUNC pFunction;         /* Function 'pointer' */
   USHORT   uiData;            /* Item position for data (Harbour like, begin from 1) */
   USHORT   uiDataShared;      /* Item position for datashared (original pos within Shared Class) */
   USHORT   uiSprClass;        /* Originalclass'handel (super or current class'handel if not herited). */ /*Added by RAC&JF*/
   USHORT   uiScope;           /* Scoping value */
   PHB_ITEM pInitValue;        /* Item Value and value for data (could be initiated by INIT KeyWord) */
   BYTE     bClsDataInitiated; /* There is one value assigned at init time */
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
   PHB_FUNC pFunError;      /* error handler for not defined messages */
} CLASS, * PCLASS;

#define BASE_METHODS    255 /* starting maximum number of message */
#define BUCKET          5
#define HASH_KEY        ( BASE_METHODS / BUCKET ) /* Idealy, here we want a "nombre premier" */

static PCLASS   s_pClasses     = NULL;
static USHORT   s_uiClasses    = 0;
static PMETHOD  s_pMethod      = NULL; /* TOFIX: The object engine is not thread safe because of this. [vszakats] */
static PHB_DYNS s_msgClassName = NULL;
static PHB_DYNS s_msgClassH    = NULL;
static PHB_DYNS s_msgEval      = NULL;
static PHB_DYNS s_msgClassSel  = NULL;
static PHB_DYNS s_msgClsParent = NULL;
static PHB_DYNS s_msgClass     = NULL;

/* All functions contained in classes.c */

static PHB_ITEM hb_clsInst( USHORT uiClass, BOOL bInit );
static void     hb_clsScope( PHB_ITEM pObject, PMETHOD pMethod );
static ULONG    hb_cls_MsgToNum( PHB_DYNS pMsg );
static BOOL     hb_clsIsParent( PCLASS pClass, char * szParentName );
static void     hb_clsDictRealloc( PCLASS pClass );
static void     hb_clsRelease( PCLASS );
       void     hb_clsReleaseAll( void );

       char *   hb_objGetClsName( PHB_ITEM pObject );
       PHB_FUNC hb_objGetMethod( PHB_ITEM, PHB_SYMB );
       ULONG    hb_objHasMsg( PHB_ITEM pObject, char * szString );

static HARBOUR  hb___msgClsH( void );
static HARBOUR  hb___msgClsName( void );
static HARBOUR  hb___msgClsSel( void );
static HARBOUR  hb___msgClass( void );
static HARBOUR  hb___msgSuper( void );
static HARBOUR  hb___msgEvalInline( void );
static HARBOUR  hb___msgClsParent( void );
static HARBOUR  hb___msgEval( void );
static HARBOUR  hb___msgVirtual( void );
static HARBOUR  hb___msgGetClsData( void );
static HARBOUR  hb___msgSetClsData( void );
static HARBOUR  hb___msgGetShrData( void );
static HARBOUR  hb___msgSetShrData( void );
static HARBOUR  hb___msgGetData( void );
static HARBOUR  hb___msgSetData( void );

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
      USHORT  uiNewHashKey = pClass->uiHashKey;
      USHORT  ui;
      USHORT  uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );

      do
      {
         uiNewHashKey += ( USHORT ) HASH_KEY;

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
      if( pMeth->pInitValue )
         hb_itemRelease( pMeth->pInitValue );

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
   USHORT uiClass;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsReleaseAll()"));

   for( uiClass = 0; uiClass < s_uiClasses; uiClass++ )
      hb_clsRelease( s_pClasses + uiClass );

   if( s_pClasses )
      hb_xfree( s_pClasses );
   s_uiClasses = 0;
   s_pClasses  = NULL;
}

/* Check if passed memory block pointer is referenced by some class
   internal data.
   This is called from the garbage collector.
*/
BOOL hb_clsIsClassRef( void *pBlock )
{
   USHORT uiClass = s_uiClasses;
   PCLASS pClass = s_pClasses;
   USHORT uiAt;
   USHORT uiLimit;
   PMETHOD pMeth;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsIsClassRef(%p)", pBlock));

   while( uiClass-- )
   {
      if( pClass->pInlines )
         if( hb_gcItemRef( pClass->pInlines, pBlock ) )
             return TRUE;
      if( pClass->pClassDatas )
         if( hb_gcItemRef( pClass->pClassDatas, pBlock ) )
             return TRUE;
             
      uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
         if( pMeth->pInitValue )
            if( hb_gcItemRef( pMeth->pInitValue, pBlock ) )
               return TRUE;
               
      ++pClass;
   }
   return FALSE;    /* passed block is not referenced in any class */
}

void hb_clsScope( PHB_ITEM pObject, PMETHOD pMethod )
{
   PHB_ITEM pBase = hb_stack.pBase;
   LONG iLevel = 1;
   BOOL bRetVal = FALSE;
   USHORT uiScope = pMethod->uiScope;
   PHB_DYNS pMessage = pMethod->pMessage;
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];
   char * szNameBase;
   char * szNameObject;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   szNameBase = hb_objGetClsName( pBase + 1 );
   szNameObject = hb_objGetClsName( pObject );

   if( iLevel == -1 )
   {
      if( ( pBase + 1 )->type == HB_IT_ARRAY )  /* it is a message */
      {
         if( ( uiScope & HB_OO_CLSTP_PROTECTED ) == HB_OO_CLSTP_PROTECTED )
         {
            bRetVal = ( strcmp( szNameBase, szNameObject ) != 0 );

            if( bRetVal )
            {
               strcpy( szName, szNameObject );
               strcat( szName, ":" );
               strcat( szName, pMessage->pSymbol->szName );
               hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (protected)", szName );
            }
         }

         /* if HB_CLS_MASKHIDDEN defined, a call to a hidden msg will result to a msg not found error. */

#ifndef HB_CLS_MASKHIDDEN
         if( ( uiScope & HB_OO_CLSTP_HIDDEN ) == HB_OO_CLSTP_HIDDEN )
         {
            if( ( uiScope & HB_OO_CLSTP_SUPER ) == HB_OO_CLSTP_SUPER )
               bRetVal = TRUE;
            else
               bRetVal = ( strcmp( szNameBase, szNameObject ) != 0 );

            if( bRetVal )
            {
               strcpy( szName, szNameObject );
               strcat( szName, ":" );
               strcat( szName, pMessage->pSymbol->szName );
               hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (hidden)", szName );
            }
         }
#endif
      }
      else if( ( uiScope & HB_OO_CLSTP_PROTECTED ) == HB_OO_CLSTP_PROTECTED )
      {
         strcpy( szName, szNameObject );
         strcat( szName, ":" );
         strcat( szName, pMessage->pSymbol->szName );
         hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (protected)", szName );
      }
#ifndef HB_CLS_MASKHIDDEN
      else if( ( uiScope & HB_OO_CLSTP_HIDDEN ) == HB_OO_CLSTP_HIDDEN )
      {
         strcpy( szName, szNameObject );
         strcat( szName, ":" );
         strcat( szName, pMessage->pSymbol->szName );
         hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (hidden)", szName );
      }
#endif

      if( ( uiScope & HB_OO_CLSTP_READONLY ) == HB_OO_CLSTP_READONLY )
      {
         if(
             ( pMethod->pFunction == hb___msgSetData    ) ||
             ( pMethod->pFunction == hb___msgSetClsData ) ||
             ( pMethod->pFunction == hb___msgSetShrData )
           )
            bRetVal = TRUE;

         if( bRetVal )
         {
            strcpy( szName, szNameObject );
            strcat( szName, ":" );
            strcat( szName, pMessage->pSymbol->szName );
            hb_errRT_BASE( EG_NOMETHOD, 1004, "Scope violation (readonly)", szName );
         }
      }
   }
}

ULONG hb_cls_MsgToNum( PHB_DYNS pMsg )
{
   USHORT i;
   ULONG nRetVal = 0;

   for( i = 0; pMsg->pSymbol->szName[ i ] != '\0'; i++)
      nRetVal = ( nRetVal << 1 ) + pMsg->pSymbol->szName[ i ];

   return nRetVal;
}

BOOL hb_clsIsParent( PCLASS pClass, char * szParentName )
{
   USHORT uiAt, uiLimit;

   uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );

   for( uiAt = 0; uiAt < uiLimit; uiAt++)
   {
      if( ( pClass->pMethods[ uiAt ].uiScope & HB_OO_CLSTP_CLASS ) == HB_OO_CLSTP_CLASS )
      {
         if( strcmp( pClass->pMethods[ uiAt ].pMessage->pSymbol->szName, szParentName ) == 0 )
            return TRUE;
      }
   }

   return FALSE;
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
   else                                         /* built in types           */
   {
      switch( pObject->type )
      {
         case HB_IT_NIL:
              szClassName = "NIL";
              break;

         case HB_IT_STRING:
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
 * <pFunc> = hb_objGetMethod( <pObject>, <pMessage> )
 *
 * Internal function to the function pointer of a message of an object
 */
PHB_FUNC hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   USHORT uiClass;
   PHB_DYNS pMsg = pMessage->pDynSym;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetMethod(%p, %p)", pObject, pMessage));

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

      s_pMethod = NULL;                            /* Current method pointer   */

      while( uiAt != uiLimit )
      {
         if( pClass->pMethods[ uiAt ].pMessage == pMsg )
         {

            s_pMethod = pClass->pMethods + uiAt;
            hb_clsScope( pObject, s_pMethod );
            return s_pMethod->pFunction;
         }
         uiAt++;
         if( uiAt == uiMask )
            uiAt = 0;
      }
   }

   /*Compatibility issue (and for 'HardCoded Object') !
     should never be used as we autoinhertit from TObject. See New __cls_param. [R‚C&JfL]*/

   if( s_msgClassName == NULL )
   {
      s_msgClassName = hb_dynsymGet( "CLASSNAME" );  /* Standard messages        */
      s_msgClassH    = hb_dynsymGet( "CLASSH" );     /* Not present in classdef. */
      s_msgClassSel  = hb_dynsymGet( "CLASSSEL" );
      s_msgEval      = hb_dynsymGet( "EVAL" );
      s_msgClsParent = hb_dynsymGet( "ISDERIVEDFROM" );
      s_msgClass     = hb_dynsymGet( "CLASS" );
   }

   if( pMsg == s_msgClassName )
      return hb___msgClsName;

   else if( pMsg == s_msgClassH )
      return hb___msgClsH;

   else if( pMsg == s_msgClassSel )
      return hb___msgClsSel;

   else if( pMsg == s_msgEval )
      return hb___msgEval;

   else if( pMsg == s_msgClsParent )
      return hb___msgClsParent;

   else if( pMsg == s_msgClass )
      return hb___msgClass;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = s_pClasses + ( uiClass - 1 );

      if( pClass->pFunError )
         return pClass->pFunError;
   }

   return NULL;
}


/*
 * <uPtr> = hb_objHasMsg( <pObject>, <szString> )
 *
 * Check whether <szString> is an existing message for object.
 *
 * <uPtr> should be read as a boolean
 */
ULONG hb_objHasMsg( PHB_ITEM pObject, char *szString )
{
   PHB_DYNS pDynSym = hb_dynsymFindName( szString );

   HB_TRACE(HB_TR_DEBUG, ("hb_objHasMsg(%p, %s)", pObject, szString));

   if( pDynSym )
      return ( ULONG ) hb_objGetMethod( pObject, pDynSym->pSymbol );
   else
      return 0;
}                                                /* Get funcptr of message   */


/* ================================================ */

/*
 * __clsAddMsg( <hClass>, <cMessage>, <pFunction>, <nType>, [xInit], <uiScope> )
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
 */
HB_FUNC( __CLSADDMSG )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   USHORT uiScope = ( USHORT ) ( ISNUM( 6 ) ? hb_parni( 6 ) : HB_OO_CLSTP_EXPORTED );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
      PHB_DYNS pMessage = hb_dynsymGet( hb_parc( 2 ) );
      USHORT   uiBucket;

      USHORT   wType    = ( USHORT ) hb_parni( 4 );
      USHORT   uiAt;
      PMETHOD  pNewMeth;

      if( wType == HB_OO_MSG_INLINE && hb_param( 3, HB_IT_BLOCK ) == NULL )
         hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSADDMSG" );

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

      switch( wType )
      {
         case HB_OO_MSG_METHOD:

            pNewMeth->pFunction = ( PHB_FUNC ) hb_parnl( 3 );
            pNewMeth->uiScope = uiScope;
            pNewMeth->uiData = 0;
            break;

         case HB_OO_MSG_DATA:

            pNewMeth->uiData = ( USHORT ) hb_parnl( 3 );
            pNewMeth->uiScope = uiScope;

            if( pMessage->pSymbol->szName[ 0 ] == '_' )
               pNewMeth->pFunction  = hb___msgSetData;
            else
            {
               PHB_ITEM pInit = hb_param( 5, HB_IT_ANY );

               pNewMeth->pFunction  = hb___msgGetData;

               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  if( HB_IS_ARRAY( pInit ) )
                     pNewMeth->pInitValue = hb_arrayClone( pInit );
                  else
                  {
                     pNewMeth->pInitValue = hb_itemNew( NULL );
                     hb_itemCopy( pNewMeth->pInitValue, pInit );
                  }
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
                  if( HB_IS_ARRAY( pInit ) )
                     pNewMeth->pInitValue = hb_arrayClone( pInit );
                  else
                  {
                     pNewMeth->pInitValue = hb_itemNew( NULL );
                     hb_itemCopy( pNewMeth->pInitValue, pInit );
                  }
               }
            }

            if( ( pNewMeth->uiScope & HB_OO_CLSTP_SHARED ) != HB_OO_CLSTP_SHARED )
            {
               if( pMessage->pSymbol->szName[ 0 ] == '_' )
                  pNewMeth->pFunction = hb___msgSetClsData;
               else
                  pNewMeth->pFunction = hb___msgGetClsData;

            }
            else
            {
               if( pMessage->pSymbol->szName[ 0 ] == '_' )
                {
                  pNewMeth->pFunction = hb___msgSetShrData;
                  pClass->uiDatasShared++;
                }
               else
                  pNewMeth->pFunction = hb___msgGetShrData;

/*             if (TRUE)
*               {
*                char cTmp[255];
*                wsprintf(cTmp, "Class %s, Message %s, uidata %d\n", pClass->szName, pMessage->pSymbol->szName, pNewMeth->uiData);
*                MessageBox(0,cTmp,"AddMsg ClassData Shared",0);
*               }
*/

            }

            break;

         case HB_OO_MSG_INLINE:

            pNewMeth->uiData = ( USHORT ) ( hb_arrayLen( pClass->pInlines ) + 1 );
            pNewMeth->uiScope = uiScope;
            hb_arraySize( pClass->pInlines, pNewMeth->uiData );
            hb_arraySet( pClass->pInlines, pNewMeth->uiData, hb_param( 3, HB_IT_BLOCK ) );
            pNewMeth->pFunction = hb___msgEvalInline;
            break;

         case HB_OO_MSG_VIRTUAL:

            pNewMeth->pFunction = hb___msgVirtual;
            break;

         case HB_OO_MSG_SUPER:

            pNewMeth->uiData = ( USHORT ) hb_parnl( 3 );
            pNewMeth->uiSprClass = ( USHORT ) hb_parnl( 5 ); /* store the super handel */
            pNewMeth->uiScope = uiScope;
            pNewMeth->pFunction = hb___msgSuper;
            break;

         case HB_OO_MSG_ONERROR:

            pClass->pFunError = ( PHB_FUNC ) hb_parnl( 2 );
            break;

         default:

            hb_errInternal( IE_CLSINVMETHOD, NULL, "__clsAddMsg", NULL );
            break;
      }
   }
}


/*
 * <hClass> := __clsNew( <cClassName>, <nDatas>, [ahSuper,aoSuper] )
 *
 * Create a new class
 *
 * <cClassName> Name of the class
 * <nDatas>     Number of DATAs in the class
 * <ahSuper>    Optional handle(s) of superclass(es)
 * <ahSuper>    Optional superclass(es) Object instance
 */
HB_FUNC( __CLSNEW )
{
   PCLASS pNewCls;
   USHORT uiSize;

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
   pNewCls->szName = ( char * ) hb_xgrab( hb_parclen( 1 ) + 1 );

   memset(pNewCls->szName, 0, hb_parclen( 1 ) + 1);

   strcpy( pNewCls->szName, hb_parc( 1 ) );
   pNewCls->uiDataFirst = 0;
   pNewCls->uiDatas = 0;
   pNewCls->uiMethods = 0;
   pNewCls->uiDatasShared = 0;

   if( uiSuper )
   {
      for( i = 1; i <= uiSuper; i++ )
      {
         PHB_DYNS pMsg;
         PHB_ITEM pSuper;
         PHB_ITEM pClsAnyTmp;
         USHORT nSuper;
         USHORT ui, uiAt, uiLimit;
         PCLASS pSprCls;
         USHORT nLen;

         pSuper  =  hb_itemNew( NULL );
         hb_arrayGet( pahSuper, i, pSuper);
         nSuper  = ( USHORT ) hb_itemGetNL( pSuper );
         pSprCls = s_pClasses + ( nSuper - 1 );
         uiLimit = ( USHORT ) ( pSprCls->uiHashKey * BUCKET );

         hb_itemRelease( pSuper );

         pNewCls->uiDataFirst += pSprCls->uiDatas;
         pNewCls->uiDatas      = ( USHORT ) ( pNewCls->uiDataFirst + hb_parni( 2 ) );

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
/*          nLenShrDatas += pNewCls->uiDatasShared;
            nLenClsDatas += ( USHORT ) hb_itemSize( pNewCls->pClassDatas );
            nLenInlines  += ( USHORT ) hb_itemSize( pNewCls->pInlines );
            nLenDatas    += ( USHORT ) pNewCls->uiDatas; */

            /*nLenShrDatas  = pNewCls->uiDatasShared;*/
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
         if( ( pNewCls->uiMethods + 1 ) > ( pNewCls->uiHashKey * BUCKET * 2 / 3 ) )
            hb_clsDictRealloc( pNewCls );

         if( i == 1 )
         {
            uiSize = ( USHORT ) ( pNewCls->uiHashKey * BUCKET * sizeof( METHOD ) );
            pNewCls->pMethods = ( PMETHOD ) hb_xgrab( uiSize );
            memset( pNewCls->pMethods, 0, uiSize );
            pNewCls->pFunError = pSprCls->pFunError;
         }

         for( ui = 0; ui < uiLimit; ui++ )
         {
             USHORT uiBucket;

             pMsg = ( PHB_DYNS ) pSprCls->pMethods[ ui ].pMessage;

             if( pMsg )
             {
                uiAt = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pNewCls->uiHashKey ) * BUCKET );

                for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
                {

#ifdef HB_CLS_MASKHIDDEN /* no hidden methods allowed by the inheritence. */
                   if( ( pSprCls->pMethods[ ui ].uiScope & HB_OO_CLSTP_HIDDEN ) == HB_OO_CLSTP_HIDDEN )
                      break;
#endif

                   if( pNewCls->pMethods[ uiAt+uiBucket ].pMessage == 0 ) /* this message position is empty */
                   {
                      /* Now, we can increment the msg count */
                      pNewCls->uiMethods++;

                      hb_xmemcpy(pNewCls->pMethods + ( uiAt+uiBucket ), pSprCls->pMethods + ui, sizeof( METHOD ) );

                      if(
                          pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgSetClsData
                          ||
                          pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgGetClsData
                        )
                         pNewCls->pMethods[ uiAt+uiBucket ].uiData += nLenClsDatas;

                      if(
                          pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgSetData
                          ||
                          pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgGetData
                          ||
                          pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgSuper
                        )
                      {
                        pNewCls->pMethods[ uiAt+uiBucket ].uiData     += nLenDatas;
                      }

                      if( pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgEvalInline )
                         pNewCls->pMethods[ uiAt+uiBucket ].uiData += nLenInlines;

                      if( ( pSprCls->pMethods[ ui ].uiScope & HB_OO_CLSTP_SUPER ) != HB_OO_CLSTP_SUPER )
                         pNewCls->pMethods[ uiAt+uiBucket ].uiScope = ( USHORT ) ( pSprCls->pMethods[ ui ].uiScope + HB_OO_CLSTP_SUPER );
                      else
                         pNewCls->pMethods[ uiAt+uiBucket ].uiScope = pSprCls->pMethods[ ui ].uiScope;

                      if( pSprCls->pMethods[ ui ].pInitValue )
                      {
                         PHB_ITEM pInitValue;

                         if( HB_IS_ARRAY( pSprCls->pMethods[ ui ].pInitValue ) )
                            pNewCls->pMethods[ uiAt + uiBucket ].pInitValue = hb_arrayClone( pSprCls->pMethods[ ui ].pInitValue );
                         else
                         {
                            pInitValue = hb_itemNew( NULL );

                            hb_itemCopy( pInitValue, pSprCls->pMethods[ ui ].pInitValue );
                            pNewCls->pMethods[ uiAt + uiBucket ].pInitValue = pInitValue;
                         }
                      }
                      break;
                   }
                   else if( pNewCls->pMethods[ uiAt + uiBucket ].pMessage == pMsg ) /*if( strcmp( pNewCls->pMethods[ uiAt+uiBucket ].pMessage->pSymbol->szName, pMsg->pSymbol->szName ) == 0 )*/
                      break;
                }
             }
         }
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
            PHB_FUNC pFunc = pClass->pMethods[ uiAt ].pFunction;

            if( pFunc == hb___msgEvalInline )      /* INLINE method deleted    */
            {
               hb_arrayDel( pClass->pInlines, pClass->pMethods[ uiAt ].uiData );
                                                   /* Delete INLINE block      */
            }
                                                /* Move messages            */
            while( pClass->pMethods[ uiAt ].pMessage && uiAt != uiLimit )
            {
               hb_xmemcpy( pClass->pMethods + uiAt,
                       pClass->pMethods + ( ( uiAt == uiMask ) ? 0 : uiAt + 1 ),
                       sizeof( METHOD ) );
               uiAt++;
               if( uiAt == uiMask )
                  uiAt = 0;
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
   pSelf = hb_clsInst( ( USHORT ) hb_parni( 1 ), TRUE );

   if( pSelf )
   {
      hb_itemReturn( pSelf );
      hb_itemRelease( pSelf );
   }

   /* Now release memory */
   hb_clsInst( s_uiClasses + 1 , TRUE );
}


/*
 * [<o(Super)Object>] := hb_clsInst( <hClass>, bInit )
 *
 * Create a (super)object from class definition <hClass>
 */
static PHB_ITEM hb_clsInst( USHORT uiClass, BOOL bInit )
{
   static PHB_ITEM * s_ppObjects;
   static USHORT s_uiSize;

   PHB_ITEM pSelf = NULL;

   if( bInit )
   {
      if( s_ppObjects )
         hb_xfree( s_ppObjects );

      s_ppObjects = NULL;
      s_uiSize = 0;
   }

   if( uiClass <= s_uiClasses )
   {
      PCLASS   pClass = s_pClasses + ( uiClass - 1 );
      PHB_ITEM pSprObj, pTmp;
      USHORT   uiAt;
      USHORT   uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      PMETHOD  pMeth ;

      pSelf = hb_itemNew( NULL );
      hb_arrayNew( pSelf, pClass->uiDatas );

      pSelf->item.asArray.value->uiClass   = uiClass;
      pSelf->item.asArray.value->uiPrevCls = 0;

      /* Phase I Instanciate all Herited object recursively               */
      /* A:B ==> oB                                                       */
      /*   Then    B:C ==> oC   ...                                       */
      /* Do not try to work on A:C (inherited class super object from B)  */
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
      {
          if( ( pMeth->uiScope & HB_OO_CLSTP_CLASS ) == HB_OO_CLSTP_CLASS
               &&
              ( pMeth->uiScope & HB_OO_CLSTP_SUPER ) != HB_OO_CLSTP_SUPER
            )
          {
             pSprObj = hb_clsInst( pMeth->uiSprClass, FALSE );  /*instance super object*/
             hb_arraySet( pSelf, pMeth->uiData, pSprObj );
             hb_itemRelease( pSprObj );

             ++s_uiSize;
             if( s_ppObjects == NULL )
                s_ppObjects = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) );
             else
                s_ppObjects = ( PHB_ITEM * ) hb_xrealloc( s_ppObjects, s_uiSize * sizeof( PHB_ITEM ) );

             pTmp = hb_arrayGetItemPtr( pSelf, pMeth->uiData );
             s_ppObjects[ s_uiSize - 1 ] = pTmp;
          }
      }

      /* Phase II Create link between A:C and A:B:C !! */
      /* They must point to the same object            */
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
      {
         if( ( pMeth->uiScope & HB_OO_CLSTP_CLASS ) == HB_OO_CLSTP_CLASS
              &&
             ( pMeth->uiScope & HB_OO_CLSTP_SUPER ) == HB_OO_CLSTP_SUPER
           )
         {
            USHORT uiCnt;

            for( uiCnt = 1; uiCnt <= s_uiSize; uiCnt++ )
            {
               pTmp = s_ppObjects[ uiCnt - 1 ];

               if( pTmp->item.asArray.value->uiClass == pMeth->uiSprClass )
               {
                  PHB_ITEM pPtrNum = hb_itemPutNL( NULL, ( ULONG ) pTmp );
                  hb_itemArrayPut( pSelf, pMeth->uiData, pPtrNum );
                  hb_itemRelease( pPtrNum );
                  break;
               }
            }
         }
      }

      /* Phase III Create link between instancied object and SuperDataMessages */
      /* Initialise value if initialisation was requested                      */
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
      {

         /* Init Classdata (inherited and not) if needed */
         if( pMeth->pInitValue  && pMeth->pFunction == hb___msgGetClsData && !( pMeth->bClsDataInitiated ) )
           {
               HB_ITEM init;
               hb_arrayGet( pClass->pClassDatas, pMeth->uiData, &init );
               if( init.type == HB_IT_NIL )
               {
                  hb_arraySet( pClass->pClassDatas, pMeth->uiData, pMeth->pInitValue );
                  pMeth->bClsDataInitiated = 1;
               }
               hb_itemClear( &init );
           }


         if( ( pMeth->uiScope & HB_OO_CLSTP_SUPER ) == HB_OO_CLSTP_SUPER )
         {
            if( pMeth->pFunction == hb___msgGetData )
            {
               USHORT uiCnt;

               for( uiCnt = 1; uiCnt <= s_uiSize; uiCnt++ )
               {
                  pTmp = s_ppObjects[ uiCnt - 1 ];

                  if( pTmp->item.asArray.value->uiClass == pMeth->uiSprClass )
                  {
                     USHORT ui, uiBucket;
                     PHB_DYNS pMsg;
                     PCLASS pSprCls;
                     PHB_ITEM pDataHrtd, pPtrNum;

                     pMsg = ( PHB_DYNS ) pMeth->pMessage;

                     pSprCls = s_pClasses + ( pMeth->uiSprClass - 1 );

                     ui = ( USHORT ) ( ( ( hb_cls_MsgToNum( pMsg ) ) % pSprCls->uiHashKey ) * BUCKET );

                     for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
                     {
                         if( pMsg == pSprCls->pMethods[ ui+uiBucket ].pMessage /*strcmp( pMsg->pSymbol->szName, pSprCls->pMethods[ ui+uiBucket ].pMessage->pSymbol->szName ) == 0*/ )
                            break;
                     }

                     pDataHrtd = ( PHB_ITEM ) hb_arrayGetItemPtr( pTmp, pSprCls->pMethods[ ui+uiBucket ].uiData );

                     pPtrNum = hb_itemPutNL( NULL, ( ULONG ) pDataHrtd );
                     hb_itemArrayPut( pSelf , pMeth->uiData, pPtrNum );
                     hb_itemRelease( pPtrNum );
                     break;

                  }
               }
            }
         }
         else if( pMeth->pInitValue )
         {
            if( pMeth->pFunction == hb___msgGetData ) /* is a DATA but not herited */
            {
               if( HB_IS_ARRAY( pMeth->pInitValue ) )
               {
                  PHB_ITEM pInitValue = hb_arrayClone( pMeth->pInitValue );
                  hb_arraySet( pSelf, pMeth->uiData, pInitValue );
                  hb_itemRelease( pInitValue );
               }
               else
                  hb_arraySet( pSelf, pMeth->uiData,
                                   pMeth->pInitValue );
            }
            else if( pMeth->pFunction == hb___msgGetShrData && !( pMeth->bClsDataInitiated ) )
            {
               /* Init Shared Classdata as needed, we only need to init the first */
               /* not inherited classdata array where all shared will point to    */
               HB_ITEM init;
               hb_arrayGet( pClass->pClassDatas, pMeth->uiData, &init );
               if( init.type == HB_IT_NIL )
               {
                  hb_arraySet( pClass->pClassDatas, pMeth->uiData, pMeth->pInitValue );
                  pMeth->bClsDataInitiated = 1;
               }
               hb_itemClear( &init );

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
            PHB_FUNC pFunc = pClass->pMethods[ uiAt ].pFunction;

            if( pFunc == hb___msgEvalInline )      /* INLINE method changed    */
            {
               PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK );

               if( pBlock == NULL )
                  hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSMODMSG" );
               else
                  hb_arraySet( pClass->pInlines, pClass->pMethods[ uiAt ].uiData, pBlock );
            }
            else if( ( pFunc == hb___msgSetData ) || ( pFunc == hb___msgGetData ) )
            {                                      /* Not allowed for DATA     */
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DATA item", "__CLSMODMSG" );
            }
            else                                   /* Modify METHOD            */
               pClass->pMethods[ uiAt ].pFunction = ( PHB_FUNC ) hb_parnl( 3 );
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
      hb_retl( hb_objHasMsg( pObject, pString->item.asString.value ) != 0 );
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJHASMSG" );
}


/*
 * <oNew> := __objClone( <oOld> )
 *
 * Clone an object. Note the similarity with aClone ;-)
 */
HB_FUNC( __OBJCLONE )
{
   PHB_ITEM pSrcObject = hb_param( 1, HB_IT_OBJECT );

   if( pSrcObject )
   {
      PHB_ITEM pDstObject = hb_arrayClone( pSrcObject );
      hb_itemReturn( pDstObject );
      hb_itemRelease( pDstObject );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, "__OBJCLONE" );
}


/*
 * <xRet> = __objSendMsg( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a message to an object
 */
HB_FUNC( __OBJSENDMSG )
{
   PHB_ITEM pObject  = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pMessage = hb_param( 2, HB_IT_STRING );

   if( pMessage && pObject )                /* Object & message passed      */
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pMessage->item.asString.value );

      if( pMsg )
      {
         USHORT uiParam;

         hb_vmPush( pObject );                      /* Push object                  */
         hb_vmMessage( pMsg->pSymbol );
                                            /* Push char symbol as message  */
         for( uiParam = 3; uiParam <= hb_pcount(); uiParam++ )   /* Push arguments on stack      */
            hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
         hb_vmDo( ( USHORT ) ( hb_pcount() - 2 ) );             /* Execute message              */
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJSENDMSG" );
}


/*
 * <hClass> := __clsInstSuper( <cName> )
 *
 * Instance super class and return class handle
 */
HB_FUNC( __CLSINSTSUPER )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );
   BOOL bFound = FALSE;

   if( pString )
   {
      PHB_DYNS pDynSym = hb_dynsymFind( pString->item.asString.value );

      if( pDynSym )                             /* Find function            */
      {
         USHORT uiClass;

         hb_vmPushSymbol( pDynSym->pSymbol );        /* Push function name       */
         hb_vmPushNil();
         hb_vmFunction( 0 );                         /* Execute super class      */

         if( HB_IS_OBJECT( &hb_stack.Return ) )
         {
            for( uiClass = 0; ! bFound && uiClass < s_uiClasses; uiClass++ )
            {                                      /* Locate the entry         */
               if( hb_stricmp( pString->item.asString.value, s_pClasses[ uiClass ].szName ) == 0 )
               {
                  hb_retni( uiClass + 1 );               /* Entry + 1 = hb___msgClsH    */
                  bFound = TRUE;
               }
            }
         }
         else
            hb_errRT_BASE( EG_ARG, 3002, "Super class does not return an object", "__CLSINSTSUPER" );
      }
      else
         hb_errRT_BASE( EG_ARG, 3003, "Cannot find super class", "__CLSINSTSUPER" );
   }

   if( ! bFound )
      hb_retni( 0 );
}



/*
 * <nSeq> = __cls_CntClsData( <hClass> )
 *
 * Return number of class datas
 */
HB_FUNC( __CLS_CNTCLSDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      hb_retni( hb_arrayLen( pClass->pClassDatas ) );
   }
   else hb_retni( 0 );
}


/*
 * <nSeq> = __cls_CntData( <hClass> )
 *
 * Return number of datas
 */
HB_FUNC( __CLS_CNTDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
      hb_retni( uiClass != 0 ? s_pClasses[ uiClass - 1 ].uiDatas : 0 );
}


/*
 * <nSeq> = __cls_DecData( <hClass> )
 *
 * Return number of datas and decrease
 */
HB_FUNC( __CLS_DECDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
      hb_retni( s_pClasses[ uiClass - 1 ].uiDatas-- );
}


/*
 * <nSeq> = __cls_IncData( <hClass> )
 *
 * Return number of datas and decrease
 */
HB_FUNC( __CLS_INCDATA )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
      hb_retni( uiClass != 0 ? ++s_pClasses[ uiClass - 1 ].uiDatas : 0 );
}

/* NOTE: Undocumented Clipper function */

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

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->uiMethods );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;
         if( pMessage )                         /* Hash Entry used ?        */
         {
            PHB_ITEM pItem = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                                                /* Add to array             */
            hb_itemArrayPut( pReturn, ++uiPos, pItem );
            hb_itemRelease( pItem );
         }
      }
   }
   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}

/* to be used from Classes ERROR HANDLER method */
HB_FUNC( __GETMESSAGE )
{
   PHB_ITEM pBase = hb_stack.pBase;

   pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   hb_retc( pBase->item.asSymbol.value->szName );
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
      PHB_ITEM iTmp = hb_itemPutC( NULL, (char *) "TObject" );
      array = hb_itemArrayNew( 1 );
      hb_itemArrayPut( array, 1, iTmp );
      hb_itemRelease( iTmp );
   }

   hb_itemReturn( array );
   hb_itemRelease( array );
}


HB_FUNC( __CLSPARENT )
{
   hb_retl( hb_clsIsParent( s_pClasses + ( hb_parni( 1 ) - 1 ), hb_parc( 2 ) ) );
}


HB_FUNC( __SENDER )
{
   PHB_ITEM pBase = hb_stack.pBase;
   PHB_ITEM oSender = NULL;
   USHORT iLevel = 3;

   while( iLevel > 0 && pBase != hb_stack.pItems )
   {
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;
      oSender = pBase + 1;

      if( ( iLevel-- == 2 && oSender->type != HB_IT_BLOCK ) || ( oSender )->type == HB_IT_NIL )
         break;
   }

   if( iLevel == 0 && oSender != NULL && oSender->type == HB_IT_OBJECT )
      hb_itemReturn( oSender );
}

/*
 * Added by R‚C&JfL
 *
 * based on hb___msgClsH( void )
 */
HB_FUNC( __CLASSH )
{
   PHB_ITEM pObject = hb_itemParam( 1 );

   hb_retni( HB_IS_OBJECT( pObject ) ? pObject->item.asArray.value->uiClass : 0 );

   hb_itemRelease( pObject );
}

/* Work in progress.
 * Added by R‚C&JfL
 *
 * based on hb___msgEval( void )
 */
HB_FUNC( __EVAL )
{
   PHB_ITEM pObject = hb_itemParam( 1 );

   if( HB_IS_BLOCK( pObject ) )
   {
      USHORT uiParam;

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pObject );                     /* Push block               */
      for( uiParam = 1; uiParam <= hb_pcount(); uiParam++ )
         hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
      hb_vmDo( ( USHORT ) hb_pcount() );                       /* Self is also an argument */
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_NOMETHOD, 1004, NULL, "EVAL" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }

   hb_itemRelease( pObject );
}

/* ================================================ */

/*
 * <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
static HARBOUR hb___msgClsH( void )
{
   if( HB_IS_ARRAY( hb_stack.pBase + 1 ) )
      hb_retni( ( hb_stack.pBase + 1 )->item.asArray.value->uiClass );
   else
      hb_retni( 0 );
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
   PCLASS pClass;
   char * szParentName = 0;
   USHORT uiClass, i;

   if( HB_IS_BYREF( hb_stack.pBase + 1 ) )            /* Variables by reference   */
      pItemRef = hb_itemUnRef( hb_stack.pBase + 1 );
   else
      pItemRef = hb_stack.pBase + 1;

   uiClass = pItemRef->item.asArray.value->uiClass;

   pClass = s_pClasses + ( uiClass - 1 );

   pItemParam = hb_stack.pBase + 2;

   if( HB_IS_OBJECT( pItemParam ) )
      szParentName = hb_objGetClsName( pItemParam );
   else if( HB_IS_STRING( pItemParam ) )
      szParentName = hb_itemGetC( pItemParam );

   for( i = 0; szParentName[ i ] != '\0'; i++ )
      szParentName[ i ] = ( char ) toupper( szParentName[ i ] );

   hb_retl( hb_clsIsParent( pClass, szParentName ) );

   hb_itemFreeC( szParentName );
}


/*
 * <cClassName> := <obj>:ClassName()
 *
 * Return class name of <obj>. Can also be used for all types.
 */
static HARBOUR hb___msgClsName( void )
{
   PHB_ITEM pItemRef;

   if( HB_IS_BYREF( hb_stack.pBase + 1 ) )            /* Variables by reference   */
      pItemRef = hb_itemUnRef( hb_stack.pBase + 1 );
   else
      pItemRef = hb_stack.pBase + 1;

   hb_retc( hb_objGetClsName( pItemRef ) );
}


/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
static HARBOUR hb___msgClsSel( void )
{
   USHORT uiClass = ( USHORT ) ( HB_IS_ARRAY( hb_stack.pBase + 1 )
                                 ? ( hb_stack.pBase + 1 )->item.asArray.value->uiClass : 0 );
                                                /* Get class word           */
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( ( ! uiClass ) && HB_IS_BYREF( hb_stack.pBase + 1 ) )
   {                                            /* Variables by reference   */
      PHB_ITEM pItemRef = hb_itemUnRef( hb_stack.pBase + 1 );
      if( HB_IS_ARRAY( pItemRef ) )
         uiClass = pItemRef->item.asArray.value->uiClass;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiPos = 0;
      USHORT uiAt;

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->uiMethods );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

         if( pMessage )                         /* Hash Entry used ?        */
         {
            PHB_ITEM pItem = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                                                /* Add to array             */
            hb_itemArrayPut( pReturn, ++uiPos, pItem );
            hb_itemRelease( pItem );
         }
      }
   }

   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}


/*
 * __msgEvalInline()
 *
 * Internal function executed for inline methods
 */
static HARBOUR hb___msgEvalInline( void )
{
   HB_ITEM block;
   USHORT uiClass = ( hb_stack.pBase + 1 )->item.asArray.value->uiClass;
   USHORT uiParam;

   hb_arrayGet( s_pClasses[ uiClass - 1 ].pInlines, s_pMethod->uiData, &block );

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( &block );
   hb_vmPush( hb_stack.pBase + 1 );                     /* Push self                */
   for( uiParam = 1; uiParam <= hb_pcount(); uiParam++ )
      hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
   hb_vmDo( ( USHORT ) (hb_pcount() + 1 ) );     /* Self is also an argument */

   hb_itemClear( &block );                       /* Release block            */
}


/*
 * __msgEval()
 *
 * Internal function for the internal EVAL method.
 */
static HARBOUR hb___msgEval( void )
{
   if( HB_IS_BLOCK( hb_stack.pBase + 1 ) )
   {
      USHORT uiParam;

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( hb_stack.pBase + 1 );                     /* Push block               */
      for( uiParam = 1; uiParam <= hb_pcount(); uiParam++ )
         hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
      hb_vmDo( ( USHORT ) hb_pcount() );                       /* Self is also an argument */
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_NOMETHOD, 1004, NULL, "EVAL" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}


/*
 * __msgGetClsData()
 *
 * Internal function to return a CLASSDATA
 */
static HARBOUR hb___msgGetClsData( void )
{
   USHORT uiClass = ( hb_stack.pBase + 1 )->item.asArray.value->uiClass;

   if( uiClass && uiClass <= s_uiClasses )
      hb_arrayGet( s_pClasses[ uiClass - 1 ].pClassDatas, s_pMethod->uiData, &hb_stack.Return );
}



/*
 * __msgSuper()
 *
 * Internal function to return a superobject
 */
static HARBOUR hb___msgSuper( void )
{
   PHB_ITEM pObject = hb_stack.pBase + 1;
/* USHORT uiIndex = s_pMethod->uiData; */

   pObject->item.asArray.value->uiPrevCls = pObject->item.asArray.value->uiClass; /* backup of actual handel */
   pObject->item.asArray.value->uiClass   = s_pMethod->uiSprClass; /* superclass handel casting */

   hb_itemReturn( pObject );

/* This one would return a real superObject but with the side effect to  */
/* also set a bad Self pointer for the supermessages                     */
/* Please don't erase those lines until I do it myself (JF)              */
/* I have yet to think about a better solution where I could return a    */
/* real Object as keeping the good Self value within all the class tree  */
/* if( ( s_pMethod->uiScope & HB_OO_CLSTP_SUPER ) == HB_OO_CLSTP_SUPER ) */
/* {                                                                     */
/*    PHB_ITEM pPtrNum = ( PHB_ITEM ) hb_arrayGetNL( pObject, uiIndex ); */
/*                                                                       */
/*    hb_itemReturn( pPtrNum );                                          */
/* }                                                                     */
/* else                                                                  */
/*    hb_itemReturn( hb_arrayGetItemPtr( pObject, s_pMethod->uiData ) ); */
}

/*
 * __msgClass()
 *
 * Internal function to return Self at Self:Class call (classy compatibility)
 */
static HARBOUR hb___msgClass( void )
{
   hb_itemReturn( hb_stack.pBase + 1 );
}

/*
 * __msgSetClsData()
 *
 * Internal function to set a CLASSDATA
 */
static HARBOUR hb___msgSetClsData( void )
{
   USHORT uiClass = ( hb_stack.pBase + 1 )->item.asArray.value->uiClass;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PHB_ITEM pReturn = hb_stack.pBase + 2;
      hb_arraySet( s_pClasses[ uiClass - 1 ].pClassDatas,
                   s_pMethod->uiData, pReturn );
      hb_itemReturn( pReturn );
   }
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
      hb_arrayGet( s_pClasses[ uiSprCls - 1 ].pClassDatas, s_pMethod->uiDataShared, &hb_stack.Return );
}

/*
 * __msgSetShrData()
 *
 * Internal function to set a SHAREDDATA
 */
static HARBOUR hb___msgSetShrData( void )
{
   USHORT uiSprCls = s_pMethod->uiSprClass;

   if( uiSprCls && uiSprCls <= s_uiClasses )
   {
      PHB_ITEM pReturn = hb_stack.pBase + 2;
      hb_arraySet( s_pClasses[ uiSprCls - 1 ].pClassDatas,
                   s_pMethod->uiDataShared, pReturn );
      hb_itemReturn( pReturn );
   }
}

/*
 * __msgGetData()
 *
 * Internal function to return a DATA
 */
static HARBOUR hb___msgGetData( void )
{
   PHB_ITEM pObject = hb_stack.pBase + 1;
   USHORT uiIndex = s_pMethod->uiData;

   /* If it's an herited data, the data reside within it's original Super object */
   /* So we use the know pointer */
   if( ( s_pMethod->uiScope & HB_OO_CLSTP_SUPER ) == HB_OO_CLSTP_SUPER )
   {
      PHB_ITEM pPtrNum;
      pPtrNum = ( PHB_ITEM ) hb_arrayGetNL( pObject, uiIndex );

      hb_itemReturn( pPtrNum );
   }
   else
   {
      if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) ) /* Resize needed */
      {
         hb_arraySize( pObject, uiIndex ); /* Make large enough */
      }
      hb_arrayGet( pObject, uiIndex, &hb_stack.Return );
   }
}

/*
 * __msgSetData()
 *
 * Internal function to set a DATA
 */
static HARBOUR hb___msgSetData( void )
{
   PHB_ITEM pObject = hb_stack.pBase + 1;
   PHB_ITEM pReturn = hb_stack.pBase + 2;
   USHORT uiIndex = s_pMethod->uiData;

   /* If it's an herited data, the data reside within it's original Super object */
   /* So we use the know pointer */
   if( ( s_pMethod->uiScope & HB_OO_CLSTP_SUPER ) == HB_OO_CLSTP_SUPER )
   {
      PHB_ITEM pPtrNum;
      pPtrNum = ( PHB_ITEM ) hb_arrayGetNL( pObject, uiIndex );

      hb_itemCopy( pPtrNum, pReturn );
   }
   else
   {
      if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) ) /* Resize needed ? */
         hb_arraySize( pObject, uiIndex ); /* Make large enough */

      hb_arraySet( pObject, uiIndex, pReturn );
   }

   hb_itemReturn( pReturn );
}

/* No comment :-) */
static HARBOUR hb___msgVirtual( void )
{
   /* hb_ret(); */ /* NOTE: It's safe to comment this out */
   ;
}

