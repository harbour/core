/*
 * $Id$
 */

/*
   Harbour Project source code

   Base-routines for OOPS system

   Copyright 1999  Antonio Linares <alinares@fivetech.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

/* Harbour Project source code
   http://www.Harbour-Project.org/

   The following functions are Copyright 1999 Eddie Runia <eddie@runia.com>:
   partial copyright regarding the following functions :
      :CLASSSEL()
      __clsDelMsg()
      __clsModMsg()
      __clsInstSuper()
      __cls_CntClsData()
      __cls_CntData()
      __cls_DecData()
      __cls_IncData()
      __objClone()
      __objHasMsg()
      __objSendMsg()

   The following functions are Copyright 1999 Victor Szel <info@szelvesz.hu>:
      hb___msgEval()

   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
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
#define HASH_KEY      ( BASE_METHODS / BUCKET )

static PCLASS   s_pClasses     = NULL;
static WORD     s_wClasses     = 0;
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
   WORD    wAt;
   WORD    wLimit = pClass->wHashKey * BUCKET;
   PMETHOD pMeth = pClass->pMethods;

   for( wAt = 0; wAt < wLimit; wAt++, pMeth++ ) /* Release initializers     */
      if( pMeth->pInitValue && pMeth->wData > pClass->wDataFirst )
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
   WORD w;

   for( w = 0; w < s_wClasses; w++ )
      hb_clsRelease( s_pClasses + w );

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
      if( ! pObject->item.asArray.value->wClass )
         szClassName = "ARRAY";
      else
         szClassName =
            ( s_pClasses + pObject->item.asArray.value->wClass - 1 )->szName;
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
   WORD     wClass;
   PHB_DYNS pMsg = pMessage->pDynSym;

   if( pObject->type == IT_ARRAY )
      wClass = pObject->item.asArray.value->wClass;
   else
      wClass = 0;

   if( wClass && wClass <= s_wClasses )
   {
      PCLASS pClass = &s_pClasses[ wClass - 1 ];
      WORD   wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      WORD   wMask  = pClass->wHashKey * BUCKET;
      WORD   wLimit = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      s_pMethod = NULL;                            /* Current method pointer   */

      while( wAt != wLimit )
      {
         if( pClass->pMethods[ wAt ].pMessage == pMsg )
         {
            s_pMethod = pClass->pMethods + wAt;
            return s_pMethod->pFunction;
         }
         wAt++;
         if( wAt == wMask )
            wAt = 0;
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
   WORD wClass = hb_parnl( 1 );

   if( wClass && wClass <= s_wClasses )
   {
      PCLASS   pClass   = &s_pClasses[ wClass - 1 ];
      PHB_DYNS pMessage = hb_dynsymGet( hb_parc( 2 ) );
      WORD     wType    = hb_parni( 4 );
      WORD     wAt      = ( ( ( unsigned ) pMessage ) % pClass->wHashKey ) * BUCKET;
      WORD     wMask    = pClass->wHashKey * BUCKET;
      PMETHOD  pNewMeth;

      if( wType == MET_INLINE && hb_param( 3, IT_BLOCK ) == NULL )
      {
         hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSADDMSG" );
      }

      if( pClass->wMethods > ( pClass->wHashKey * BUCKET * 2/3 ) )
         hb_clsDictRealloc( pClass );

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
              pNewMeth->wData = hb_parnl( 3 );
              if( ( WORD ) hb_arrayLen( pClass->pClassDatas ) < hb_parnl( 3 ) )
                 hb_arraySize( pClass->pClassDatas, hb_parnl( 3 ) );

              if( pMessage->pSymbol->szName[ 0 ] == '_' )
                 pNewMeth->pFunction = hb___msgSetClsData;
              else
                 pNewMeth->pFunction = hb___msgGetClsData;
              break;

         case MET_INLINE:

              pNewMeth->wData = hb_arrayLen( pClass->pInlines ) + 1;
              hb_arraySize( pClass->pInlines, pNewMeth->wData );
              hb_arraySet(  pClass->pInlines, pNewMeth->wData,
                            hb_param( 3, IT_BLOCK ) );
              pNewMeth->pFunction = hb___msgEvalInline;
              break;

         case MET_VIRTUAL:
              pNewMeth->pFunction = hb___msgVirtual;
              break;

         case MET_SUPER:
              pNewMeth->wData     = hb_parnl( 3 );
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
   WORD   wSuper = hb_parni( 3 );               /* Super class present      */
   PCLASS pNewCls;

   if( s_pClasses )
      s_pClasses = ( PCLASS ) hb_xrealloc( s_pClasses,
                 sizeof( CLASS ) * ( s_wClasses + 1 ) );
   else
      s_pClasses = ( PCLASS ) hb_xgrab( sizeof( CLASS ) );

   pNewCls = s_pClasses + s_wClasses;
   pNewCls->szName = ( char * ) hb_xgrab( hb_parclen( 1 ) + 1 );
   strcpy( pNewCls->szName, hb_parc( 1 ) );

   if( wSuper )
   {
      PCLASS pSprCls = s_pClasses + wSuper - 1;
      WORD   wSize;

      pNewCls->wDataFirst  = pSprCls->wDatas;
      pNewCls->wDatas      = pSprCls->wDatas + hb_parni( 2 );
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
   hb_retni( ++s_wClasses );
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
   WORD     wClass   = hb_parni( 1 );
   PHB_ITEM pString  = hb_param( 2, IT_STRING );

   if( wClass && wClass <= s_wClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );
      
      if( pMsg )
      {
         PCLASS   pClass   = s_pClasses + wClass - 1;
         WORD     wMask    = pClass->wHashKey * BUCKET;
         PHB_SYMB pMessage = pMsg->pSymbol;
         WORD     wAt      = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
         WORD     wLimit   = wAt ? ( wAt - 1 ) : ( wMask - 1 );
    
         while( ( wAt != wLimit ) &&
                ( pClass->pMethods[ wAt ].pMessage &&
                ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
         {
            wAt++;
            if( wAt == wMask )
               wAt = 0;
         }

         if( wAt != wLimit )
         {                                         /* Requested method found   */
            PHB_FUNC pFunc = pClass->pMethods[ wAt ].pFunction;

            if( pFunc == hb___msgEvalInline )      /* INLINE method deleted    */
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
               if( wAt == wMask )
                  wAt = 0;
            }

            memset( pClass->pMethods + wAt, 0, sizeof( METHOD ) );

            pClass->wMethods--;                    /* Decrease number messages */
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
   WORD wClass = hb_parni( 1 );

   if( wClass <= s_wClasses )
   {
      PCLASS  pClass = s_pClasses + ( wClass - 1 );
      WORD    wAt;
      WORD    wLimit = pClass->wHashKey * BUCKET;
      PMETHOD pMeth  = pClass->pMethods;                /* Initialize DATA          */

      hb_arrayNew( &stack.Return, pClass->wDatas );
      stack.Return.item.asArray.value->wClass = wClass;

      for( wAt = 0; wAt < wLimit; wAt++, pMeth++ )
         if( pMeth->pInitValue )
            hb_itemArrayPut( &stack.Return, pMeth->wData, pMeth->pInitValue );
   }
}


/*
 * __clsModMsg( <oObj>, <cMessage>, <pFunc> )
 *
 * Modify message (only for INLINE and METHOD)
 */
HARBOUR HB___CLSMODMSG( void )
{
   WORD     wClass   = hb_parni( 1 );
   PHB_ITEM pString  = hb_param( 2, IT_STRING );

   if( wClass && wClass <= s_wClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass   = s_pClasses + wClass - 1;
         PHB_SYMB pMessage = pMsg->pSymbol;
         WORD     wAt      = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
         WORD     wMask    = pClass->wHashKey * BUCKET;
         WORD     wLimit   = wAt ? ( wAt - 1 ) : ( wMask - 1 );

         while( ( wAt != wLimit ) &&
                ( pClass->pMethods[ wAt ].pMessage &&
                ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
         {
            wAt++;
            if( wAt == wMask )
               wAt = 0;
         }

         if( wAt != wLimit )
         {                                         /* Requested method found   */
            PHB_FUNC pFunc = pClass->pMethods[ wAt ].pFunction;

            if( pFunc == hb___msgEvalInline )      /* INLINE method changed    */
            {
               PHB_ITEM pBlock = hb_param( 3, IT_BLOCK );

               if( pBlock == NULL )
                  hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSMODMSG" );
               else
                  hb_arraySet( pClass->pInlines, pClass->pMethods[ wAt ].wData, pBlock );
            }
            else if( ( pFunc == hb___msgSetData ) || ( pFunc == hb___msgGetData ) )
            {                                      /* Not allowed for DATA     */
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DATA item", "__CLSMODMSG" );
            }
            else                                   /* Modify METHOD            */
               pClass->pMethods[ wAt ].pFunction = ( PHB_FUNC ) hb_parnl( 3 );
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
   WORD wClass;

   if( pObject && pObject->item.asArray.value->wClass )
   {
      wClass = pObject->item.asArray.value->wClass;

      hb_retc( s_pClasses[ wClass - 1 ].szName );
   }
   else
   {
      wClass = hb_parni( 1 );

      if( wClass <= s_wClasses )
         hb_retc( s_pClasses[ wClass - 1 ].szName );
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
         WORD w;
	 
         hb_vmPush( pObject );                      /* Push object                  */
         hb_vmMessage( pMsg->pSymbol );
                                            /* Push char symbol as message  */
         for( w = 3; w <= hb_pcount(); w++ )   /* Push arguments on stack      */
            hb_vmPush( hb_param( w, IT_ANY ) );
         hb_vmDo( hb_pcount()-2 );                  /* Execute message              */
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
   BOOL     bFound = FALSE;

   if( pString )
   {
      PHB_DYNS pDynSym = hb_dynsymFind( pString->item.asString.value );

      if( pDynSym )                             /* Find function            */
      {
         WORD w;

         hb_vmPushSymbol( pDynSym->pSymbol );        /* Push function name       */
         hb_vmPushNil();
         hb_vmFunction( 0 );                         /* Execute super class      */

         if( !IS_OBJECT( &stack.Return ) )
         {
            hb_errRT_BASE( EG_ARG, 3002, "Super class does not return an object", "__CLSINSTSUPER" );
         }

         for( w = 0; !bFound && w < s_wClasses; w++ )
         {                                      /* Locate the entry         */
            if( !hb_stricmp( pString->item.asString.value, s_pClasses[ w ].szName ) )
            {
               hb_retni( w + 1 );               /* Entry + 1 = hb___msgClsH    */
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
   WORD wClass = hb_parnl( 1 );

   if( wClass )
   {
      PCLASS pClass = &s_pClasses[ wClass - 1 ];
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
   WORD wClass = hb_parnl( 1 );

   if( wClass )
      hb_retni( s_pClasses[ wClass - 1 ].wDatas );
}


/*
 * <nSeq> = __cls_DecData( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR HB___CLS_DECDATA( void )
{
   WORD wClass = hb_parnl( 1 );

   if( wClass )
      hb_retni( s_pClasses[ wClass - 1 ].wDatas-- );
}


/*
 * <nSeq> = __cls_IncData( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR HB___CLS_INCDATA( void )
{
   WORD wClass = hb_parnl( 1 );

   if( wClass )
      hb_retni( ++s_pClasses[ wClass - 1 ].wDatas );
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
      hb_retni( ( stack.pBase + 1 )->item.asArray.value->wClass );
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
   WORD wClass = IS_ARRAY( stack.pBase + 1 ) ?
                 ( stack.pBase + 1 )->item.asArray.value->wClass : 0;
                                                /* Get class word           */
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( ( ! wClass ) && IS_BYREF( stack.pBase + 1 ) )
   {                                            /* Variables by reference   */
      PHB_ITEM pItemRef = hb_itemUnRef( stack.pBase + 1 );
      if( IS_ARRAY( pItemRef ) )
         wClass = pItemRef->item.asArray.value->wClass;
   }

   if( wClass && wClass <= s_wClasses )
   {
      PCLASS   pClass = &s_pClasses[ wClass - 1 ];
      WORD     wLimit = pClass->wHashKey * BUCKET; /* Number of Hash keys      */
      WORD     wPos   = 0;
      WORD     wAt;

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->wMethods );
                                                /* Create a transfer array  */
      for( wAt = 0; wAt < wLimit ; wAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ wAt ].pMessage;
         if( pMessage )                         /* Hash Entry used ?        */
         {
            PHB_ITEM pItem = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                                                /* Add to array             */
            hb_itemArrayPut( pReturn, ++wPos, pItem );
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
   WORD    wClass = ( stack.pBase + 1 )->item.asArray.value->wClass;
   WORD    w;

   hb_arrayGet( s_pClasses[ wClass - 1 ].pInlines, s_pMethod->wData, &block );

   hb_vmPushSymbol( &symEval );
   hb_vmPush( &block );
   hb_vmPush( stack.pBase + 1 );                     /* Push self                */
   for( w = 1; w <= hb_pcount(); w++ )
      hb_vmPush( hb_param( w, IT_ANY ) );
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
   WORD wClass = ( stack.pBase + 1 )->item.asArray.value->wClass;

   if( wClass && wClass <= s_wClasses )
      hb_arrayGet( s_pClasses[ wClass - 1 ].pClassDatas, s_pMethod->wData, &stack.Return );
}


/*
 * __msgGetData()
 *
 * Internal function to return a DATA
 */
static HARBOUR hb___msgGetData( void )
{
   PHB_ITEM pObject = stack.pBase + 1;
   WORD     wIndex  = s_pMethod->wData;

   if( wIndex > ( WORD ) hb_arrayLen( pObject ) )
                                                /* Resize needed            */
      hb_arraySize( pObject, wIndex );          /* Make large enough        */

   hb_arrayGet( pObject, wIndex, &stack.Return );
}


/*
 * __msgSuper()
 *
 * Internal function to cast to a super method
 */
static HARBOUR hb___msgSuper( void )
{
   PHB_ITEM   pObject   = stack.pBase + 1;
   PHB_ITEM   pSuper    = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
   PBASEARRAY pNewBase  = ( PBASEARRAY ) hb_xgrab( sizeof( BASEARRAY ) );
   WORD       wSuperCls = s_pMethod->wData;     /* Get handle of superclass */

   memcpy( pSuper,   pObject, sizeof( HB_ITEM ) );
                                                /* Allocate new structures  */
   memcpy( pNewBase, pObject->item.asArray.value, sizeof( BASEARRAY ) );

   pSuper->item.asArray.value = pNewBase;

   pNewBase->wClass     = wSuperCls;
   pNewBase->wHolders   = 1;                    /* New item is returned     */
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
   WORD     wClass  = ( stack.pBase + 1 )->item.asArray.value->wClass;
   PHB_ITEM pReturn = stack.pBase + 2;

   if( wClass && wClass <= s_wClasses )
   {
      hb_arraySet( s_pClasses[ wClass - 1 ].pClassDatas,
                   s_pMethod->wData, pReturn );
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
   WORD     wIndex  = s_pMethod->wData;

   /* Resize needed ? */
   if( wIndex > ( WORD ) hb_arrayLen( pObject ) )
      /* Make large enough */
      hb_arraySize( pObject, wIndex );

   hb_arraySet( pObject, wIndex, pReturn );
   hb_itemCopy( &stack.Return, pReturn );
}


/* No comment :-) */
static HARBOUR hb___msgVirtual( void )
{
   hb_ret();
}

