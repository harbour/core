/*
 * $Id$
 */

/*
 * Classes.C contains the base-routines for OOPS system
 *
 * Copyright(C) 1999 by Antonio Linares.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * You can contact me at: alinares@fivetech.com
 *
 *
 * Partial Copyright (C) 1999 Eddie Runia ( eddie@runia.com )
 *   partial copyright regarding the following functions :
 *      CLASSDEL
 *      CLASSMOD
 *      :CLASSSEL()
 *      ISMESSAGE
 *      OCLONE
 *      OSEND
 *      SELECTSUPER
 *      __INSTSUPER
 *      __WCLSDATAS
 *      __WDATAS
 *      __WDATADEC
 *      __WDATAINC
 */

#include "extend.h"
#include "errorapi.h"
#include "itemapi.h"
#include "ctoharb.h"
#include "init.h"

#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4
#define MET_SUPER     5

typedef struct
{
   void *   pMessage;   /* pointer to dynamic symbol when they get ready */
   PHB_FUNC pFunction;
   WORD     wData;
   WORD     wScope;
   PHB_ITEM pInitValue;
} METHOD, * PMETHOD;

typedef struct
{
   char *   szName;
   WORD     wDatas;
   WORD     wDataFirst;   /* First wData from this class */
   PMETHOD  pMethods;
   WORD     wMethods;
   WORD     wHashKey;
   PHB_ITEM pClassDatas;  /* Array for ClassDatas */
   PHB_ITEM pInlines;     /* Array for inline codeblocks */
} CLASS, * PCLASS;

#define BASE_METHODS  200
#define BUCKET        4
#define HASH_KEY      (BASE_METHODS / BUCKET)

PCLASS   pClasses     = 0;
WORD     wClasses     = 0;
PMETHOD  pMethod      = 0;
PHB_DYNS msgClassName = 0;
PHB_DYNS msgClassH    = 0;
PHB_DYNS msgEval      = 0;
PHB_DYNS msgClassSel  = 0;

/* All functions contained in classes.c */

       HARBOUR  HB_CLASSADD( void );
       HARBOUR  HB_CLASSCREATE( void );
       HARBOUR  HB_CLASSDEL( void );
static HARBOUR  ClassH( void );
       HARBOUR  HB_CLASSINSTANCE( void );
       HARBOUR  HB_CLASSMOD( void );
static HARBOUR  ClassName( void );
       HARBOUR  HB_CLASSNAME( void );
static HARBOUR  ClassSel( void );
static void     DictRealloc( PCLASS );
static HARBOUR  EvalInline( void );
       char *   hb_GetClassName( PHB_ITEM pObject );
static HARBOUR  GetClassData( void );
static HARBOUR  GetData( void );
       PHB_FUNC hb_GetMethod( PHB_ITEM, PHB_SYMB );
       ULONG    hb_isMessage( PHB_ITEM, char *);
       HARBOUR  HB_ISMESSAGE( void );
       HARBOUR  HB_OCLONE( void );
       HARBOUR  HB_OSEND( void );
       void     ReleaseClass( PCLASS );
       void     ReleaseClasses( void );
static HARBOUR  SelectSuper( void );
static HARBOUR  SetClassData( void );
static HARBOUR  SetData( void );
static HARBOUR  Virtual( void );
       HARBOUR  HB___INSTSUPER( void );
       HARBOUR  HB___WCLSDATAS(void);
       HARBOUR  HB___WDATAS( void );
       HARBOUR  HB___WDATADEC( void );
       HARBOUR  HB___WDATAINC( void );

/* All function contained in matching objfunc.prg */

HARBOUR HB_AODATA( void );
HARBOUR HB_AOGET( void );
HARBOUR HB_AOMETHOD( void );
HARBOUR HB_AOSET( void );
HARBOUR HB_ISDATA( void );
HARBOUR HB_ISMETHOD( void );
HARBOUR HB_OADDDATA( void );
HARBOUR HB_OADDINLINE( void );
HARBOUR HB_OADDMETHOD( void );
HARBOUR HB_ODELDATA( void );
HARBOUR HB_ODELINLINE( void );
HARBOUR HB_ODELMETHOD( void );
HARBOUR HB_OMODINLINE( void );
HARBOUR HB_OMODMETHOD( void );

/* Exported symbols of both classes.c and objfunc.prg */

HB_INIT_SYMBOLS_BEGIN( Classes__InitSymbols )
{ "AODATA"        , FS_PUBLIC, HB_AODATA         , 0 },
{ "AOGET"         , FS_PUBLIC, HB_AOGET          , 0 },
{ "AOMETHOD"      , FS_PUBLIC, HB_AOMETHOD       , 0 },
{ "AOSET"         , FS_PUBLIC, HB_AOSET          , 0 },
{ "CLASSADD"      , FS_PUBLIC, HB_CLASSADD       , 0 },
{ "CLASSCREATE"   , FS_PUBLIC, HB_CLASSCREATE    , 0 },
{ "CLASSDEL"      , FS_PUBLIC, HB_CLASSDEL       , 0 },
{ "CLASSINSTANCE" , FS_PUBLIC, HB_CLASSINSTANCE  , 0 },
{ "CLASSMOD"      , FS_PUBLIC, HB_CLASSMOD       , 0 },
{ "CLASSNAME"     , FS_PUBLIC, HB_CLASSNAME      , 0 },
{ "ISDATA"        , FS_PUBLIC, HB_ISDATA         , 0 },
{ "ISMESSAGE"     , FS_PUBLIC, HB_ISMESSAGE      , 0 },
{ "ISMETHOD"      , FS_PUBLIC, HB_ISMETHOD       , 0 },
{ "OADDDATA"      , FS_PUBLIC, HB_OADDDATA       , 0 },
{ "OADDINLINE"    , FS_PUBLIC, HB_OADDINLINE     , 0 },
{ "OADDMETHOD"    , FS_PUBLIC, HB_OADDMETHOD     , 0 },
{ "OCLONE"        , FS_PUBLIC, HB_OCLONE         , 0 },
{ "ODELDATA"      , FS_PUBLIC, HB_ODELDATA       , 0 },
{ "ODELINLINE"    , FS_PUBLIC, HB_ODELINLINE     , 0 },
{ "ODELMETHOD"    , FS_PUBLIC, HB_ODELMETHOD     , 0 },
{ "OMODINLINE"    , FS_PUBLIC, HB_OMODINLINE     , 0 },
{ "OMODMETHOD"    , FS_PUBLIC, HB_OMODMETHOD     , 0 },
{ "OSEND"         , FS_PUBLIC, HB_OSEND          , 0 }
HB_INIT_SYMBOLS_END( Classes__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup Classes__InitSymbols
#endif

/*
 * ClassAdd( <hClass>, <cMessage>, <pFunction>, <nType>, [xInit] )
 *
 * Add a message to the class.
 *
 * <hClass>    Class handle
 * <cMessage>  Message
 * <pFunction> MET_METHOD    : Pointer to function
 *             MET_DATA      : Index number in array
 *             MET_CLASSDATA : Index number in array
 *             MET_INLINE    : Code block
 *             MET_SUPER     : Handle of super class
 * <nType>     see MET_*
 * <xInit>     Optional initializer for DATA
 */
HARBOUR HB_CLASSADD(void)
{
   WORD     wClass = hb_parnl( 1 );
   WORD     wType  = hb_parni( 4 );
   WORD     wAt, wMask;

   PHB_ITEM pInit  = hb_param( 5, IT_ANY );
   PCLASS   pClass;
   PHB_DYNS pMessage;
   PMETHOD  pNewMeth;

   if( wClass && wClass <= wClasses )
   {
      pClass   = &pClasses[ wClass - 1 ];
      pMessage = hb_dynsymGet( hb_parc( 2 ) );
      wAt      = ( ( ( unsigned ) pMessage ) % pClass->wHashKey ) * BUCKET;
      wMask    = pClass->wHashKey * BUCKET;

      if( pClass->wMethods > ( pClass->wHashKey * BUCKET * 2/3 ) )
         DictRealloc( pClass );

      /* Find either the existing message or an open spot for a new message */

      while(   pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMessage ) )
         wAt = ( wAt == wMask ) ? 0 : wAt + 1;

      pNewMeth = pClass->pMethods + wAt;
      if( !pNewMeth->pMessage )
      {
         pNewMeth->pMessage = pMessage;
         pClass->wMethods++;                    /* One more message         */
      }

      switch( wType )
      {
         case MET_METHOD:
              pNewMeth->pFunction = ( PHB_FUNC ) hb_parnl( 3 );
              break;

         case MET_DATA:
              pNewMeth->wData = hb_parnl( 3 );
              if( pMessage->pSymbol->szName[ 0 ] == '_' )
                 pNewMeth->pFunction = SetData;
              else
              {
                 pNewMeth->pFunction  = GetData;
                 if( pInit && !IS_NIL( pInit )) /* Initializer found        */
                 {
                    pNewMeth->pInitValue = hb_itemNew( NULL );
                    hb_itemCopy( pNewMeth->pInitValue, pInit );
                 }
              }
              break;

         case MET_CLASSDATA:
              pNewMeth->wData = hb_parnl( 3 );
              if( ( WORD ) hb_arrayLen( pClass->pClassDatas ) < hb_parnl( 3 ) )
                 hb_arraySize( pClass->pClassDatas, hb_parnl( 3 ) );

              if( pMessage->pSymbol->szName[ 0 ] == '_' )
                 pNewMeth->pFunction = SetClassData;
              else
                 pNewMeth->pFunction = GetClassData;
              break;

         case MET_INLINE:
              pNewMeth->wData = hb_arrayLen( pClass->pInlines ) + 1;
              hb_arraySize( pClass->pInlines, pNewMeth->wData );
              hb_arraySet(  pClass->pInlines, pNewMeth->wData,
                            hb_param( 3, IT_BLOCK ) );
              pNewMeth->pFunction = EvalInline;
              break;

         case MET_VIRTUAL:
              pNewMeth->pFunction = Virtual;
              break;

         case MET_SUPER:
              pNewMeth->wData     = hb_parnl( 3 );
              pNewMeth->pFunction = SelectSuper;
              break;

         default:
              printf( "Invalid method type from ClassAdd\n" );
              exit( 1 );
              break;
      }
   }
}


/*
 * <hClass> := ClassCreate( <cClassName>, <nDatas>, [hSuper] )
 *
 * Create a new class
 *
 * <cClassName> Name of the class
 * <nDatas>     Number of DATAs in the class
 * <hSuper>     Optional handle of superclass
 */
HARBOUR HB_CLASSCREATE(void)
{
   WORD   wSuper = hb_parni( 3 );               /* Super class present      */
   WORD   wSize;
   PCLASS pNewCls;
   PCLASS pSprCls;

   if( pClasses )
      pClasses = ( PCLASS ) hb_xrealloc( pClasses,
                 sizeof( CLASS ) * ( wClasses + 1 ) );
   else
      pClasses = ( PCLASS ) hb_xgrab( sizeof( CLASS ) );

   pNewCls = pClasses + wClasses;
   pNewCls->szName = ( char * ) hb_xgrab( hb_parclen( 1 ) + 1 );
   strcpy( pNewCls->szName, hb_parc( 1 ) );

   if( wSuper )
   {
      pSprCls = pClasses + wSuper - 1;

      pNewCls->wDataFirst  = pSprCls->wDatas;
      pNewCls->wDatas      = pSprCls->wDatas + hb_parni(2);
      pNewCls->wMethods    = pSprCls->wMethods;

      pNewCls->pClassDatas = hb_arrayClone( pSprCls->pClassDatas );
      pNewCls->pInlines    = hb_arrayClone( pSprCls->pInlines );

      pNewCls->wHashKey    = pSprCls->wHashKey;

      wSize = pSprCls->wHashKey * BUCKET * sizeof( METHOD );
      pNewCls->pMethods = ( PMETHOD ) hb_xgrab( wSize );
      memcpy( pNewCls->pMethods, pSprCls->pMethods, wSize );
   }                                            /* Copy all super methods   */
   else
   {
      pNewCls->wDatas     = hb_parni( 2 );
      pNewCls->wDataFirst = 0;
      pNewCls->pMethods   = ( PMETHOD ) hb_xgrab( BASE_METHODS * sizeof( METHOD ) );
      pNewCls->wMethods   = 0;
      pNewCls->wHashKey   = HASH_KEY;           /* BUCKET = 4 repetitions   */

      pNewCls->pClassDatas = hb_itemArrayNew( 0 );
      pNewCls->pInlines    = hb_itemArrayNew( 0 );

      memset( pNewCls->pMethods, 0, BASE_METHODS * sizeof( METHOD ) );
   }
   hb_retni( ++wClasses );
}


/*
 * ClassDel( <oObj>, <cMessage> )
 *
 * Delete message (only for INLINE and METHOD)
 *
 * <oObj>     Object
 * <cMessage> Message
 */
HARBOUR HB_CLASSDEL(void)
{
   PHB_ITEM pString  = hb_param( 2, IT_STRING );
   PHB_SYMB pMessage = hb_dynsymGet( pString->item.asString.value )->pSymbol;
   PHB_DYNS pMsg     = pMessage->pDynSym;
   PCLASS   pClass;

   WORD     wClass   = hb_parni( 1 );
   WORD     wAt;
   WORD     wLimit;
   WORD     wMask;

   PHB_FUNC pFunc;

   if( wClass && wClass <= wClasses )
   {
      pClass = pClasses + wClass - 1;
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wMask  = pClass->wHashKey * BUCKET;
      wLimit = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      while( ( wAt != wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
      {
         wAt++;
         if( wAt == wMask)
            wAt = 0;
      }

      if( wAt != wLimit )
      {                                         /* Requested method found   */
         pFunc = pClass->pMethods[ wAt ].pFunction;
         if( pFunc == EvalInline )              /* INLINE method deleted    */
         {
            hb_arrayDel( pClass->pInlines, pClass->pMethods[ wAt ].wData );
                                                /* Delete INLINE block      */
         }
                                                /* Move messages            */
         while( pClass->pMethods[ wAt ].pMessage && wAt != wLimit )
         {
            memcpy( pClass->pMethods + wAt,
                    pClass->pMethods + ( ( wAt == wMask ) ? 0 : wAt + 1 ),
                    sizeof( METHOD ) );
            wAt++;
            if( wAt == wMask)
               wAt = 0;
         }

         memset( pClass->pMethods + wAt, 0, sizeof( METHOD ) );

         pClass->wMethods--;                    /* Decrease number messages */
      }
   }
}


/*
 * <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
static HARBOUR ClassH( void )
{
   hb_retni( ( stack.pBase + 1 )->item.asArray.value->wClass );
}


/*
 * <oNewObject> := ClassInstance( <hClass> )
 *
 * Create a new object from class definition <hClass>
 */
HARBOUR HB_CLASSINSTANCE(void)
{
   WORD    wClass = hb_parni( 1 );
   WORD    wAt, wLimit;
   PCLASS  pClass;
   PMETHOD pMeth;

   if( wClass <= wClasses )
   {
      pClass = pClasses + ( wClass - 1 );
      hb_arrayNew( &stack.Return, pClass->wDatas );
      stack.Return.item.asArray.value->wClass = wClass;

      pMeth  = pClass->pMethods;                /* Initialize DATA          */
      wLimit = pClass->wHashKey * BUCKET;
      for( wAt = 0; wAt < wLimit; wAt++, pMeth++ )
         if( pMeth->pInitValue )
            hb_itemArrayPut( &stack.Return, pMeth->wData, pMeth->pInitValue );
   }
   else
      hb_ret();
}

/*
 * ClassMod( <oObj>, <cMessage>, <pFunc> )
 *
 * Modify message (only for INLINE and METHOD)
 */
HARBOUR HB_CLASSMOD(void)
{
   PHB_ITEM pString  = hb_param( 2, IT_STRING );
   PHB_SYMB pMessage = hb_dynsymGet( pString->item.asString.value )->pSymbol;
   PHB_DYNS pMsg     = pMessage->pDynSym;
   PCLASS   pClass;

   WORD     wClass   = hb_parni( 1 );
   WORD     wAt;
   WORD     wLimit;
   WORD     wMask;

   PHB_FUNC pFunc;

   if( wClass && wClass <= wClasses )
   {
      pClass = pClasses + wClass - 1;
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wMask  = pClass->wHashKey * BUCKET;
      wLimit = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      while( ( wAt != wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
      {
         wAt++;
         if( wAt == wMask)
            wAt = 0;
      }

      if( wAt != wLimit )
      {                                         /* Requested method found   */
         pFunc = pClass->pMethods[ wAt ].pFunction;
         if( pFunc == EvalInline )              /* INLINE method changed    */
            hb_arraySet( pClass->pInlines, pClass->pMethods[ wAt ].wData,
                         hb_param( 3, IT_BLOCK ) );
         else if( ( pFunc == SetData ) || ( pFunc == GetData ) )
         {                                      /* Not allowed for DATA     */
            /*hb_errPutDescription(pError, "CLASSMOD: Cannot modify a DATA item");*/
            hb_errorRT_BASE(EG_ARG, 3004, NULL, "CLASSMOD");
         }
         else                                   /* Modify METHOD            */
            pClass->pMethods[ wAt ].pFunction = ( PHB_FUNC ) hb_parnl( 3 );
      }
   }
}


/*
 * <cClassName> := <obj>:ClassName()
 *
 * Return class name of <obj>. Can also be used for all types.
 */
static HARBOUR ClassName( void )
{
   PHB_ITEM pItemRef;

   if( IS_BYREF( stack.pBase + 1 ) )            /* Variables by reference   */
      pItemRef = hb_itemUnRef( stack.pBase + 1 );
   else
      pItemRef = stack.pBase + 1;

   hb_retc( hb_GetClassName( pItemRef ) );
}


/*
 * <cClassName> := ClassName( <hClass> )
 *
 * Returns class name of <hClass>
 */
HARBOUR HB_CLASSNAME(void)
{
   PHB_ITEM pObject = hb_param( 0, IT_OBJECT );
   WORD     wClass;

   if( pObject && pObject->item.asArray.value->wClass )
   {
      wClass = pObject->item.asArray.value->wClass;
      hb_retc( pClasses[ wClass - 1 ].szName );
   }
   else
   {
      wClass = hb_parni( 1 );
      if( wClass <= wClasses )
         hb_retc( pClasses[ wClass - 1 ].szName );
      else
         hb_retc( "" );
   }
}


/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
static HARBOUR ClassSel(void)
{
   WORD     wClass = IS_ARRAY( stack.pBase + 1 ) ?
                     ( stack.pBase + 1 )->item.asArray.value->wClass : 0;
                                                /* Get class word           */
   WORD     wLimit;                             /* Number of Hash keys      */
   WORD     wAt;
   WORD     wPos = 0;
   PCLASS   pClass;
   PHB_DYNS pMessage;
   PHB_ITEM pReturn = hb_itemNew( NULL );
   PHB_ITEM pItem;
   PHB_ITEM pItemRef;

   if( ( ! wClass ) && IS_BYREF( stack.pBase + 1 ) )
   {                                            /* Variables by reference   */
      pItemRef = hb_itemUnRef( stack.pBase + 1 );
      if( IS_ARRAY( pItemRef ) )
         wClass = pItemRef->item.asArray.value->wClass;
   }

   if( wClass && wClass <= wClasses )
   {
      pClass   = &pClasses[ wClass - 1 ];
      wLimit   = pClass->wHashKey * BUCKET;
      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->wMethods );
                                                /* Create a transfer array  */
      for( wAt = 0; wAt < wLimit ; wAt++ )
      {
         pMessage = ( PHB_DYNS ) pClass->pMethods[ wAt ].pMessage;
         if( pMessage )                         /* Hash Entry used ?        */
         {
            pItem  = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                                                /* Add to array             */
            hb_itemArrayPut( pReturn, ++wPos, pItem );
            hb_itemRelease( pItem );
         }
      }
   }
   hb_itemReturn ( pReturn );
   hb_itemRelease( pReturn );
}


/*
 * DictRealloc( PCLASS )
 *
 * Realloc (widen) class
 */
static void DictRealloc( PCLASS pClass )
{
   /* TODO: Implement it for very large classes */
   if( pClass )
   {
      printf( "classes.c DictRealloc not implemented yet\n" );
      exit( 1 );
   }
}


/*
 * EvalInline()
 *
 * Internal function executed for inline methods
 */
static HARBOUR EvalInline( void )
{
   HB_ITEM block;
   WORD    wClass = ( stack.pBase + 1 )->item.asArray.value->wClass;
   WORD    w;

   hb_arrayGet( pClasses[ wClass - 1 ].pInlines, pMethod->wData, &block );

   hb_vmPushSymbol( &symEval );
   hb_vmPush( &block );
   hb_vmPush( stack.pBase + 1 );                     /* Push self                */
   for( w = 1; w <= hb_pcount(); w++ )
      hb_vmPush( hb_param( w, IT_ANY ) );
   hb_vmDo( hb_pcount() + 1 );                       /* Self is also an argument */

   hb_itemClear( &block );                       /* Release block            */
}


/*
 * <szName> = hb_GetClassName( pObject )
 *
 * Get the class name of an object
 *
 */
char * hb_GetClassName( PHB_ITEM pObject )
{
   char * szClassName;

   if( IS_ARRAY( pObject ) )
   {
      if( ! pObject->item.asArray.value->wClass )
         szClassName = "ARRAY";
      else
         szClassName =
            ( pClasses + pObject->item.asArray.value->wClass - 1 )->szName;
   }
   else                                         /* built in types           */
   {
      switch( pObject->type )
      {
         case IT_NIL:
              szClassName = "NIL";
              break;

         case IT_STRING:
              szClassName = "CHARACTER";
              break;

         case IT_BLOCK:
              szClassName = "BLOCK";
              break;

         case IT_SYMBOL:
              szClassName = "SYMBOL";
              break;

         case IT_DATE:
              szClassName = "DATE";
              break;

         case IT_INTEGER:
         case IT_LONG:
         case IT_DOUBLE:
              szClassName = "NUMERIC";
              break;

         case IT_LOGICAL:
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
 * GetClassData()
 *
 * Internal function to return a CLASSDATA
 */
static HARBOUR GetClassData( void )
{
   WORD wClass = ( stack.pBase + 1 )->item.asArray.value->wClass;
   WORD wIndex = pMethod->wData;

   if( wClass && wClass <= wClasses )
      hb_arrayGet( pClasses[ wClass - 1 ].pClassDatas, wIndex,
                   &stack.Return );
}


/*
 * GetData()
 *
 * Internal function to return a DATA
 */
static HARBOUR GetData( void )
{
   PHB_ITEM pObject = stack.pBase + 1;
   WORD     wIndex  = pMethod->wData;

   if( wIndex > ( WORD ) hb_arrayLen ( pObject ) )
                                                /* Resize needed            */
      hb_arraySize( pObject, wIndex );          /* Make large enough        */
   hb_arrayGet( pObject, wIndex, &stack.Return );
}


/*
 * <pFunc> = hb_GetMethod( <pObject>, <pMessage> )
 *
 * Internal function to the function pointer of a message of an object
 */
PHB_FUNC hb_GetMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   WORD     wAt, wLimit, wMask;
   WORD     wClass;
   PCLASS   pClass;
   PHB_DYNS pMsg = pMessage->pDynSym;

   if( pObject->type == IT_OBJECT )
      wClass = pObject->item.asArray.value->wClass;
   else
      wClass = 0;

   if( ! msgClassName )
   {
      msgClassName = hb_dynsymGet( "CLASSNAME" );  /* Standard messages        */
      msgClassH    = hb_dynsymGet( "CLASSH" );     /* Not present in classdef. */
      msgClassSel  = hb_dynsymGet( "CLASSSEL" );
      msgEval      = hb_dynsymGet( "EVAL" );
   }

   if( wClass && wClass <= wClasses )
   {
      pClass = &pClasses[ wClass - 1 ];
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wMask  = pClass->wHashKey * BUCKET;
      wLimit = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      pMethod = 0;                              /* Current method pointer   */

      while( wAt != wLimit )
      {
         if( pClass->pMethods[ wAt ].pMessage == pMsg )
         {
            pMethod = pClass->pMethods + wAt;
            return pMethod->pFunction;
         }
         wAt++;
         if( wAt == wMask)
            wAt = 0;
      }
   }
   if( pMsg == msgClassName )
      return ClassName;

   else if( pMsg == msgClassH )
      return ClassH;

   else if( pMsg == msgClassSel )
      return ClassSel;

   else if( pMsg == msgEval )
      return EvalInline;

   return 0;
}


/*
 * <uPtr> = hb_isMessage( <pObject>, <szString> )
 *
 * Check whether <szString> is an existing message for object.
 *
 * <uPtr> should be read as a boolean
 */
ULONG hb_isMessage( PHB_ITEM pObject, char *szString )
{
   PHB_SYMB pMessage = hb_dynsymGet( szString )->pSymbol;
   return( (ULONG) hb_GetMethod( pObject, pMessage ) );
}                                                /* Get funcptr of message   */


/*
 * <lRet> := IsMessage( <oObj>, <cSymbol> )
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
HARBOUR HB_ISMESSAGE(void)
{
   PHB_ITEM   pObject  = hb_param( 1, IT_OBJECT );
   PHB_ITEM   pString  = hb_param( 2, IT_STRING );

   if( pObject && pString )
      hb_retl( hb_isMessage( pObject, pString->item.asString.value ) != 0 );
   else
   {
      hb_errorRT_BASE(EG_ARG, 3000, NULL, "ISMESSAGE");
   }
}


/*
 * <oNew> := oClone( <oOld> )
 *
 * Clone an object. Note the similarity with aClone ;-)
 */
HARBOUR HB_OCLONE( void )
{
   PHB_ITEM pSrcObject  = hb_param( 1, IT_OBJECT );

   if ( pSrcObject )
   {
      PHB_ITEM pDstObject = hb_arrayClone( pSrcObject );

      hb_itemCopy( &stack.Return, pDstObject );
      hb_itemRelease( pDstObject );
   }
   else
   {
      hb_errorRT_BASE(EG_ARG, 3001, NULL, "OCLONE");
   }
}


/*
 * <xRet> = oSend( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a message to an object
 */
HARBOUR HB_OSEND(void)
{
   PHB_ITEM pObject  = hb_param( 1, IT_OBJECT );
   PHB_ITEM pMessage = hb_param( 2, IT_STRING );
   WORD     w;

   if( pMessage && pObject )                /* Object & message passed      */
   {
      hb_vmPush( pObject );                      /* Push object                  */
      hb_vmMessage( hb_dynsymGet( pMessage->item.asString.value )->pSymbol );
                                            /* Push char symbol as message  */
      for( w = 3; w <= hb_pcount(); w++ )   /* Push arguments on stack      */
         hb_vmPush( hb_param( w, IT_ANY ) );
      hb_vmDo( hb_pcount()-2 );                  /* Execute message              */
   }
   else
   {
      hb_errorRT_BASE(EG_ARG, 3000, NULL, "OSEND");
   }
}


/*
 * ReleaseClass( <pClass> )
 *
 * Release a class from memory
 */
void ReleaseClass( PCLASS pClass )
{
   WORD    wAt;
   WORD    wLimit;
   PMETHOD pMeth;

   wLimit = pClass->wHashKey * BUCKET;
   pMeth  = pClass->pMethods;
   for( wAt = 0; wAt < wLimit; wAt++, pMeth++ ) /* Release initializers     */
      if( pMeth->pInitValue && pMeth->wData > pClass->wDataFirst )
         hb_itemRelease( pMeth->pInitValue );

   hb_xfree( pClass->szName );
   hb_xfree( pClass->pMethods );

   hb_itemRelease( pClass->pClassDatas );
   hb_itemRelease( pClass->pInlines );
}


/*
 * ReleaseClasses()
 *
 * Release all classes
 */
void ReleaseClasses( void )
{
   WORD w;

   for( w = 0; w < wClasses; w++ )
      ReleaseClass( pClasses + w );

   if( pClasses )
      hb_xfree( pClasses );
}


/*
 * SelectSuper()
 *
 * Internal function to cast to a super method
 */
static HARBOUR SelectSuper( void )
{
   PHB_ITEM   pObject   = stack.pBase + 1;
   PHB_ITEM   pSuper    = (PHB_ITEM) hb_xgrab( sizeof( HB_ITEM ) );
   PBASEARRAY pNewBase  = (PBASEARRAY) hb_xgrab( sizeof( BASEARRAY ) );
   WORD       wSuperCls = pMethod->wData;       /* Get handle of superclass */

   memcpy( pSuper,   pObject, sizeof( HB_ITEM ) );
                                                /* Allocate new structures  */
   memcpy( pNewBase, pObject->item.asArray.value, sizeof( BASEARRAY ) );

   pSuper->item.asArray.value = pNewBase;

   pNewBase->wClass     = wSuperCls;
   pNewBase->wHolders   = 1;                    /* New item is returned     */
   pNewBase->wSuperCast = TRUE;                 /* Do not dispose pItems !! */
                                                /* A bit dirty, but KISS.   */
   hb_itemCopy( &stack.Return, pSuper );
   hb_itemRelease( pSuper );
}


/*
 * SetClassData()
 *
 * Internal function to set a CLASSDATA
 */
static HARBOUR SetClassData( void )
{
   WORD     wClass  = ( stack.pBase + 1 )->item.asArray.value->wClass;
   PHB_ITEM pReturn = stack.pBase + 2;

   if( wClass && wClass <= wClasses )
   {
      hb_arraySet( pClasses[ wClass - 1 ].pClassDatas,
                   pMethod->wData, pReturn );
      hb_itemCopy( &stack.Return, pReturn );
   }
}


/*
 * SetData()
 *
 * Internal function to set a DATA
 */
static HARBOUR SetData( void )
{
   PHB_ITEM pObject = stack.pBase + 1;
   PHB_ITEM pReturn = stack.pBase + 2;
   WORD     wIndex  = pMethod->wData;

   if( wIndex > ( WORD ) hb_arrayLen( pObject ) )
                                                /* Resize needed            */
   hb_arraySize( pObject, wIndex );             /* Make large enough        */
   hb_arraySet( pObject, wIndex, pReturn );
   hb_itemCopy( &stack.Return, pReturn );
}


/* No comment :-) */
static HARBOUR Virtual( void )
{
   hb_ret();
}


/*
 * <hClass> := hb__InstSuper( <cName> )
 *
 * Instance super class and return class handle
 */
HARBOUR HB___INSTSUPER( void )
{
   PHB_ITEM pString = hb_param( 1, IT_STRING );
   PHB_DYNS pDynSym;
   BYTE     bFound  = FALSE;
   WORD     w;

   if( pString )
   {
      pDynSym = hb_dynsymFind( pString->item.asString.value );
      if( pDynSym )                             /* Find function            */
      {
         hb_vmPushSymbol( pDynSym->pSymbol );        /* Push function name       */
         hb_vmPushNil();
         hb_vmFunction( 0 );                         /* Execute super class      */

         if( !IS_OBJECT( &stack.Return ) )
         {
            /* hb_errPutDescription(pError, "INSTSUPER : Super class does not return an object"); */
            hb_errorRT_BASE(EG_ARG, 3002, NULL, "__INSTSUPER");
         }

         for( w = 0; !bFound && w < wClasses; w++ )
         {                                      /* Locate the entry         */
            if( !hb_stricmp( pString->item.asString.value, pClasses[ w ].szName ) )
            {
               hb_retni( w + 1 );               /* Entry + 1 = ClassH       */
               bFound = TRUE;
            }
         }
      }
      else
      {
         /* hb_errPutDescription(pError, "INSTSUPER : Cannot find super class"); */
         hb_errorRT_BASE(EG_ARG, 3003, NULL, "__INSTSUPER");
      }
   }
   if( !bFound )
      hb_retni( 0 );
}


/*
 * <nSeq> = hb__wClsDatas( <hClass> )
 *
 * Return number of class datas
 */
HARBOUR HB___WCLSDATAS(void)
{
   WORD   wClass = hb_parnl( 1 );
   PCLASS pClass;

   if( wClass )
   {
      pClass = &pClasses[ wClass - 1 ];
      hb_retni( hb_arrayLen( pClass->pClassDatas ) );
   }
}

/*
 * <nSeq> = hb__wDataDec( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR HB___WDATADEC(void)
{
   WORD wClass = hb_parnl( 1 );

   if( wClass )
      hb_retni( pClasses[ wClass - 1 ].wDatas-- );
}


/*
 * <nSeq> = hb__wDataInc( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR HB___WDATAINC(void)
{
   WORD wClass = hb_parnl( 1 );

   if( wClass )
      hb_retni( ++pClasses[ wClass - 1 ].wDatas );
}


/*
 * <nSeq> = hb__wDatas( <hClass> )
 *
 * Return number of datas
 */
HARBOUR HB___WDATAS(void)
{
   WORD wClass = hb_parnl( 1 );

   if( wClass )
      hb_retni( pClasses[ wClass - 1 ].wDatas );
}

