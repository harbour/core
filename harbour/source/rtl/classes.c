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
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    hb___msgEval()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"
#include "errorapi.h"
#include "itemapi.h"
#include "ctoharb.h"
#include "hboo.ch"

typedef struct
{
   void *   pMessage;   /* pointer to dynamic symbol when they get ready */
   PHB_FUNC pFunction;
   USHORT   uiData;
   USHORT   uiScope;
   PHB_ITEM pInitValue;
} METHOD, * PMETHOD;

typedef struct
{
   char *   szName;
   USHORT   uiDatas;
   USHORT   uiDataFirst;   /* First uiData from this class */
   PMETHOD  pMethods;
   USHORT   uiMethods;
   USHORT   uiHashKey;
   PHB_ITEM pClassDatas;  /* Array for ClassDatas */
   PHB_ITEM pInlines;     /* Array for inline codeblocks */
} CLASS, * PCLASS;

#define BASE_METHODS    200
#define BUCKET          4
#define HASH_KEY        ( BASE_METHODS / BUCKET )

static PCLASS   s_pClasses     = NULL;
static USHORT   s_uiClasses    = 0;
static PMETHOD  s_pMethod      = NULL; /* TOFIX: The object engine is not thread safe because of this. */
static PHB_DYNS s_msgClassName = NULL;
static PHB_DYNS s_msgClassH    = NULL;
static PHB_DYNS s_msgEval      = NULL;
static PHB_DYNS s_msgClassSel  = NULL;

/* All functions contained in classes.c */

static void     hb_clsDictRealloc( PCLASS pClass );
static void     hb_clsRelease( PCLASS );
       void     hb_clsReleaseAll( void );

       char *   hb_objGetClsName( PHB_ITEM pObject );
       PHB_FUNC hb_objGetMethod( PHB_ITEM, PHB_SYMB );
       ULONG    hb_objHasMsg( PHB_ITEM pObject, char *szString );

static HARBOUR  hb___msgClsH( void );
static HARBOUR  hb___msgClsName( void );
static HARBOUR  hb___msgClsSel( void );
static HARBOUR  hb___msgSuper( void );
static HARBOUR  hb___msgEvalInline( void );
static HARBOUR  hb___msgEval( void );
static HARBOUR  hb___msgVirtual( void );
static HARBOUR  hb___msgGetClsData( void );
static HARBOUR  hb___msgSetClsData( void );
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
   /* TODO: Implement it for very large classes */
   if( pClass )
      hb_errInternal( 9999, "classes.c hb_clsDictRealloc() not implemented yet", NULL, NULL );
}


/*
 * hb_clsRelease( <pClass> )
 *
 * Release a class from memory
 */
static void hb_clsRelease( PCLASS pClass )
{
   USHORT uiAt;
   USHORT uiLimit = pClass->uiHashKey * BUCKET;
   PMETHOD pMeth = pClass->pMethods;

   for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ ) /* Release initializers     */
      if( pMeth->pInitValue && pMeth->uiData > pClass->uiDataFirst )
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

   for( uiClass = 0; uiClass < s_uiClasses; uiClass++ )
      hb_clsRelease( s_pClasses + uiClass );

   if( s_pClasses )
      hb_xfree( s_pClasses );
}

/* ================================================ */


/*
 * <szName> = hb_objGetClsName( pObject )
 *
 * Get the class name of an object
 *
 */
char * hb_objGetClsName( PHB_ITEM pObject )
{
   char * szClassName;

   if( IS_ARRAY( pObject ) )
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
 * <pFunc> = hb_objGetMethod( <pObject>, <pMessage> )
 *
 * Internal function to the function pointer of a message of an object
 */
PHB_FUNC hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   USHORT uiClass;
   PHB_DYNS pMsg = pMessage->pDynSym;

   if( pObject->type == IT_ARRAY )
      uiClass = pObject->item.asArray.value->uiClass;
   else
      uiClass = 0;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = &s_pClasses[ uiClass - 1 ];
      USHORT uiAt    = ( ( ( unsigned ) pMsg ) % pClass->uiHashKey ) * BUCKET;
      USHORT uiMask  = pClass->uiHashKey * BUCKET;
      USHORT uiLimit = uiAt ? ( uiAt - 1 ) : ( uiMask - 1 );

      s_pMethod = NULL;                            /* Current method pointer   */

      while( uiAt != uiLimit )
      {
         if( pClass->pMethods[ uiAt ].pMessage == pMsg )
         {
            s_pMethod = pClass->pMethods + uiAt;
            return s_pMethod->pFunction;
         }
         uiAt++;
         if( uiAt == uiMask )
            uiAt = 0;
      }
   }

   if( s_msgClassName == NULL )
   {
      s_msgClassName = hb_dynsymGet( "CLASSNAME" );  /* Standard messages        */
      s_msgClassH    = hb_dynsymGet( "CLASSH" );     /* Not present in classdef. */
      s_msgClassSel  = hb_dynsymGet( "CLASSSEL" );
      s_msgEval      = hb_dynsymGet( "EVAL" );
   }

   if( pMsg == s_msgClassName )
      return hb___msgClsName;

   else if( pMsg == s_msgClassH )
      return hb___msgClsH;

   else if( pMsg == s_msgClassSel )
      return hb___msgClsSel;

   else if( pMsg == s_msgEval )
      return hb___msgEval;

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
   if( pDynSym )
      return ( ULONG ) hb_objGetMethod( pObject, pDynSym->pSymbol );
   else
      return 0;
}                                                /* Get funcptr of message   */


/* ================================================ */

/*
 * __clsAddMsg( <hClass>, <cMessage>, <pFunction>, <nType>, [xInit] )
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
HARBOUR HB___CLSADDMSG( void )
{
   USHORT uiClass = hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = &s_pClasses[ uiClass - 1 ];
      PHB_DYNS pMessage = hb_dynsymGet( hb_parc( 2 ) );
      USHORT   wType    = hb_parni( 4 );
      USHORT   uiAt     = ( ( ( unsigned ) pMessage ) % pClass->uiHashKey ) * BUCKET;
      USHORT   uiMask   = pClass->uiHashKey * BUCKET;
      PMETHOD  pNewMeth;

      if( wType == MET_INLINE && hb_param( 3, IT_BLOCK ) == NULL )
      {
         hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSADDMSG" );
      }

      if( pClass->uiMethods > ( pClass->uiHashKey * BUCKET * 2/3 ) )
         hb_clsDictRealloc( pClass );

      /* Find either the existing message or an open spot for a new message */

      while(   pClass->pMethods[ uiAt ].pMessage &&
             ( pClass->pMethods[ uiAt ].pMessage != pMessage ) )
         uiAt = ( uiAt == uiMask ) ? 0 : uiAt + 1;

      pNewMeth = pClass->pMethods + uiAt;
      if( !pNewMeth->pMessage )
      {
         pNewMeth->pMessage = pMessage;
         pClass->uiMethods++;                    /* One more message         */
      }

      switch( wType )
      {
         case MET_METHOD:
              pNewMeth->pFunction = ( PHB_FUNC ) hb_parnl( 3 );
              break;

         case MET_DATA:
              pNewMeth->uiData = hb_parnl( 3 );
              if( pMessage->pSymbol->szName[ 0 ] == '_' )
                 pNewMeth->pFunction = hb___msgSetData;
              else
              {
                 PHB_ITEM pInit = hb_param( 5, IT_ANY );

                 pNewMeth->pFunction  = hb___msgGetData;

                 if( pInit && !IS_NIL( pInit )) /* Initializer found        */
                 {
                    pNewMeth->pInitValue = hb_itemNew( NULL );
                    hb_itemCopy( pNewMeth->pInitValue, pInit );
                 }
              }
              break;

         case MET_CLASSDATA:
              pNewMeth->uiData = hb_parnl( 3 );

              if( ( USHORT ) hb_arrayLen( pClass->pClassDatas ) < pNewMeth->uiData )
                 hb_arraySize( pClass->pClassDatas, pNewMeth->uiData );

              if( pMessage->pSymbol->szName[ 0 ] == '_' )
                 pNewMeth->pFunction = hb___msgSetClsData;
              else
                 pNewMeth->pFunction = hb___msgGetClsData;
              break;

         case MET_INLINE:

              pNewMeth->uiData = hb_arrayLen( pClass->pInlines ) + 1;
              hb_arraySize( pClass->pInlines, pNewMeth->uiData );
              hb_arraySet(  pClass->pInlines, pNewMeth->uiData,
                            hb_param( 3, IT_BLOCK ) );
              pNewMeth->pFunction = hb___msgEvalInline;
              break;

         case MET_VIRTUAL:
              pNewMeth->pFunction = hb___msgVirtual;
              break;

         case MET_SUPER:
              pNewMeth->uiData    = hb_parnl( 3 );
              pNewMeth->pFunction = hb___msgSuper;
              break;

         default:
              hb_errInternal( 9999, "Invalid method type from __clsAddMsg", NULL, NULL );
              break;
      }
   }
}


/*
 * <hClass> := __clsNew( <cClassName>, <nDatas>, [hSuper] )
 *
 * Create a new class
 *
 * <cClassName> Name of the class
 * <nDatas>     Number of DATAs in the class
 * <hSuper>     Optional handle of superclass
 */
HARBOUR HB___CLSNEW( void )
{
   USHORT uiSuper = hb_parni( 3 );               /* Super class present      */
   PCLASS pNewCls;

   if( s_pClasses )
      s_pClasses = ( PCLASS ) hb_xrealloc( s_pClasses,
                 sizeof( CLASS ) * ( s_uiClasses + 1 ) );
   else
      s_pClasses = ( PCLASS ) hb_xgrab( sizeof( CLASS ) );

   pNewCls = s_pClasses + s_uiClasses;
   pNewCls->szName = ( char * ) hb_xgrab( hb_parclen( 1 ) + 1 );
   strcpy( pNewCls->szName, hb_parc( 1 ) );

   if( uiSuper )
   {
      PCLASS pSprCls = s_pClasses + uiSuper - 1;
      USHORT uiSize;

      pNewCls->uiDataFirst  = pSprCls->uiDatas;
      pNewCls->uiDatas      = pSprCls->uiDatas + hb_parni( 2 );
      pNewCls->uiMethods    = pSprCls->uiMethods;

      pNewCls->pClassDatas = hb_arrayClone( pSprCls->pClassDatas );
      pNewCls->pInlines    = hb_arrayClone( pSprCls->pInlines );

      pNewCls->uiHashKey    = pSprCls->uiHashKey;

      uiSize = pSprCls->uiHashKey * BUCKET * sizeof( METHOD );
      pNewCls->pMethods = ( PMETHOD ) hb_xgrab( uiSize );
      memcpy( pNewCls->pMethods, pSprCls->pMethods, uiSize );
   }                                            /* Copy all super methods   */
   else
   {
      pNewCls->uiDatas     = hb_parni( 2 );
      pNewCls->uiDataFirst = 0;
      pNewCls->pMethods    = ( PMETHOD ) hb_xgrab( BASE_METHODS * sizeof( METHOD ) );
      pNewCls->uiMethods   = 0;
      pNewCls->uiHashKey   = HASH_KEY;           /* BUCKET = 4 repetitions   */

      pNewCls->pClassDatas = hb_itemArrayNew( 0 );
      pNewCls->pInlines    = hb_itemArrayNew( 0 );

      memset( pNewCls->pMethods, 0, BASE_METHODS * sizeof( METHOD ) );
   }
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
HARBOUR HB___CLSDELMSG( void )
{
   USHORT uiClass = hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass   = s_pClasses + uiClass - 1;
         USHORT   uiMask   = pClass->uiHashKey * BUCKET;
         USHORT   uiAt     = ( ( ( unsigned ) pMsg ) % pClass->uiHashKey ) * BUCKET;
         USHORT   uiLimit  = uiAt ? ( uiAt - 1 ) : ( uiMask - 1 );

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
               memcpy( pClass->pMethods + uiAt,
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
HARBOUR HB___CLSINST( void )
{
   USHORT uiClass = hb_parni( 1 );

   if( uiClass <= s_uiClasses )
   {
      PCLASS  pClass = s_pClasses + ( uiClass - 1 );
      USHORT  uiAt;
      USHORT  uiLimit = pClass->uiHashKey * BUCKET;
      PMETHOD pMeth  = pClass->pMethods;                /* Initialize DATA          */

      hb_arrayNew( &stack.Return, pClass->uiDatas );
      stack.Return.item.asArray.value->uiClass = uiClass;

      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
         if( pMeth->pInitValue )
            hb_itemArrayPut( &stack.Return, pMeth->uiData, pMeth->pInitValue );
   }
}


/*
 * __clsModMsg( <oObj>, <cMessage>, <pFunc> )
 *
 * Modify message (only for INLINE and METHOD)
 */
HARBOUR HB___CLSMODMSG( void )
{
   USHORT uiClass = hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass   = s_pClasses + uiClass - 1;
         USHORT   uiAt     = ( ( ( unsigned ) pMsg ) % pClass->uiHashKey ) * BUCKET;
         USHORT   uiMask   = pClass->uiHashKey * BUCKET;
         USHORT   uiLimit  = uiAt ? ( uiAt - 1 ) : ( uiMask - 1 );

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
               PHB_ITEM pBlock = hb_param( 3, IT_BLOCK );

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
HARBOUR HB___OBJGETCLSNAME( void )
{
   PHB_ITEM pObject = hb_param( 0, IT_OBJECT );
   USHORT uiClass;

   if( pObject && pObject->item.asArray.value->uiClass )
   {
      uiClass = pObject->item.asArray.value->uiClass;

      hb_retc( s_pClasses[ uiClass - 1 ].szName );
   }
   else
   {
      uiClass = hb_parni( 1 );

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
HARBOUR HB___OBJHASMSG( void )
{
   PHB_ITEM pObject = hb_param( 1, IT_OBJECT );
   PHB_ITEM pString = hb_param( 2, IT_STRING );

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
HARBOUR HB___OBJCLONE( void )
{
   PHB_ITEM pSrcObject = hb_param( 1, IT_OBJECT );

   if( pSrcObject )
   {
      PHB_ITEM pDstObject = hb_arrayClone( pSrcObject );

      hb_itemCopy( &stack.Return, pDstObject );
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
HARBOUR HB___OBJSENDMSG( void )
{
   PHB_ITEM pObject  = hb_param( 1, IT_OBJECT );
   PHB_ITEM pMessage = hb_param( 2, IT_STRING );

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
            hb_vmPush( hb_param( uiParam, IT_ANY ) );
         hb_vmDo( hb_pcount() - 2 );                  /* Execute message              */
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
HARBOUR HB___CLSINSTSUPER( void )
{
   PHB_ITEM pString = hb_param( 1, IT_STRING );
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

         if( !IS_OBJECT( &stack.Return ) )
         {
            hb_errRT_BASE( EG_ARG, 3002, "Super class does not return an object", "__CLSINSTSUPER" );
         }

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
         hb_errRT_BASE( EG_ARG, 3003, "Cannot find super class", "__CLSINSTSUPER" );
   }
   if( !bFound )
      hb_retni( 0 );
}


/*
 * <nSeq> = __cls_CntClsData( <hClass> )
 *
 * Return number of class datas
 */
HARBOUR HB___CLS_CNTCLSDATA( void )
{
   USHORT uiClass = hb_parni( 1 );

   if( uiClass )
   {
      PCLASS pClass = &s_pClasses[ uiClass - 1 ];
      hb_retni( hb_arrayLen( pClass->pClassDatas ) );
   }
}


/*
 * <nSeq> = __cls_CntData( <hClass> )
 *
 * Return number of datas
 */
HARBOUR HB___CLS_CNTDATA( void )
{
   USHORT uiClass = hb_parni( 1 );

   if( uiClass )
      hb_retni( s_pClasses[ uiClass - 1 ].uiDatas );
}


/*
 * <nSeq> = __cls_DecData( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR HB___CLS_DECDATA( void )
{
   USHORT uiClass = hb_parni( 1 );

   if( uiClass )
      hb_retni( s_pClasses[ uiClass - 1 ].uiDatas-- );
}


/*
 * <nSeq> = __cls_IncData( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR HB___CLS_INCDATA( void )
{
   USHORT uiClass = hb_parni( 1 );

   if( uiClass )
      hb_retni( ++s_pClasses[ uiClass - 1 ].uiDatas );
}


/* ================================================ */

/*
 * <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
static HARBOUR hb___msgClsH( void )
{
   if( IS_ARRAY( stack.pBase + 1 ) )
      hb_retni( ( stack.pBase + 1 )->item.asArray.value->uiClass );
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
   PHB_ITEM pItemRef;

   if( IS_BYREF( stack.pBase + 1 ) )            /* Variables by reference   */
      pItemRef = hb_itemUnRef( stack.pBase + 1 );
   else
      pItemRef = stack.pBase + 1;

   hb_retc( hb_objGetClsName( pItemRef ) );
}


/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
static HARBOUR hb___msgClsSel( void )
{
   USHORT uiClass = IS_ARRAY( stack.pBase + 1 ) ?
                 ( stack.pBase + 1 )->item.asArray.value->uiClass : 0;
                                                /* Get class word           */
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( ( ! uiClass ) && IS_BYREF( stack.pBase + 1 ) )
   {                                            /* Variables by reference   */
      PHB_ITEM pItemRef = hb_itemUnRef( stack.pBase + 1 );
      if( IS_ARRAY( pItemRef ) )
         uiClass = pItemRef->item.asArray.value->uiClass;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = &s_pClasses[ uiClass - 1 ];
      USHORT uiLimit = pClass->uiHashKey * BUCKET; /* Number of Hash keys      */
      USHORT uiPos = 0;
      USHORT uiAt;

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->uiMethods );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit ; uiAt++ )
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
   USHORT uiClass = ( stack.pBase + 1 )->item.asArray.value->uiClass;
   USHORT uiParam;

   hb_arrayGet( s_pClasses[ uiClass - 1 ].pInlines, s_pMethod->uiData, &block );

   hb_vmPushSymbol( &symEval );
   hb_vmPush( &block );
   hb_vmPush( stack.pBase + 1 );                     /* Push self                */
   for( uiParam = 1; uiParam <= hb_pcount(); uiParam++ )
      hb_vmPush( hb_param( uiParam, IT_ANY ) );
   hb_vmDo( hb_pcount() + 1 );                       /* Self is also an argument */

   hb_itemClear( &block );                       /* Release block            */
}


/*
 * __msgEval()
 *
 * Internal function for the internal EVAL method.
 */
static HARBOUR hb___msgEval( void )
{
   if( IS_BLOCK( stack.pBase + 1 ) )
   {
      USHORT uiParam;

      hb_vmPushSymbol( &symEval );
      hb_vmPush( stack.pBase + 1 );                     /* Push block               */
      for( uiParam = 1; uiParam <= hb_pcount(); uiParam++ )
         hb_vmPush( hb_param( uiParam, IT_ANY ) );
      hb_vmDo( hb_pcount() );                       /* Self is also an argument */
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
   USHORT uiClass = ( stack.pBase + 1 )->item.asArray.value->uiClass;

   if( uiClass && uiClass <= s_uiClasses )
      hb_arrayGet( s_pClasses[ uiClass - 1 ].pClassDatas, s_pMethod->uiData, &stack.Return );
}


/*
 * __msgGetData()
 *
 * Internal function to return a DATA
 */
static HARBOUR hb___msgGetData( void )
{
   PHB_ITEM pObject = stack.pBase + 1;
   USHORT uiIndex = s_pMethod->uiData;

   if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) )
                                                /* Resize needed            */
      hb_arraySize( pObject, uiIndex );         /* Make large enough        */

   hb_arrayGet( pObject, uiIndex, &stack.Return );
}


/*
 * __msgSuper()
 *
 * Internal function to cast to a super method
 */
static HARBOUR hb___msgSuper( void )
{
   PHB_ITEM   pObject    = stack.pBase + 1;
   PHB_ITEM   pSuper     = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
   PBASEARRAY pNewBase   = ( PBASEARRAY ) hb_xgrab( sizeof( BASEARRAY ) );
   USHORT     uiSuperCls = s_pMethod->uiData;     /* Get handle of superclass */

   memcpy( pSuper,   pObject, sizeof( HB_ITEM ) );
                                                /* Allocate new structures  */
   memcpy( pNewBase, pObject->item.asArray.value, sizeof( BASEARRAY ) );

   pSuper->item.asArray.value = pNewBase;

   pNewBase->uiClass    = uiSuperCls;
   pNewBase->uiHolders  = 1;                    /* New item is returned     */
   pNewBase->bSuperCast = TRUE;                 /* Do not dispose pItems !! */
                                                /* A bit dirty, but KISS.   */
   hb_itemCopy( &stack.Return, pSuper );
   hb_itemRelease( pSuper );
}


/*
 * __msgSetClsData()
 *
 * Internal function to set a CLASSDATA
 */
static HARBOUR hb___msgSetClsData( void )
{
   USHORT uiClass = ( stack.pBase + 1 )->item.asArray.value->uiClass;
   PHB_ITEM pReturn = stack.pBase + 2;

   if( uiClass && uiClass <= s_uiClasses )
   {
      hb_arraySet( s_pClasses[ uiClass - 1 ].pClassDatas,
                   s_pMethod->uiData, pReturn );
      hb_itemCopy( &stack.Return, pReturn );
   }
}


/*
 * __msgSetData()
 *
 * Internal function to set a DATA
 */
static HARBOUR hb___msgSetData( void )
{
   PHB_ITEM pObject = stack.pBase + 1;
   PHB_ITEM pReturn = stack.pBase + 2;
   USHORT   uiIndex = s_pMethod->uiData;

   /* Resize needed ? */
   if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) )
      /* Make large enough */
      hb_arraySize( pObject, uiIndex );

   hb_arraySet( pObject, uiIndex, pReturn );
   hb_itemCopy( &stack.Return, pReturn );
}


/* No comment :-) */
static HARBOUR hb___msgVirtual( void )
{
   hb_ret();
}

