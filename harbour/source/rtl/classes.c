/*
 * $Id$
 */

#include <extend.h>
#include <itemapi.h>
#include <ctoharb.h>

void ProcessSymbols( SYMBOL *, WORD );

#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4
#define MET_SUPER     5

typedef struct
{
   void * pMessage;      /* pointer to dynamic symbol when they get ready */
   HARBOURFUNC pFunction;
   WORD    wData;
   WORD    wScope;
   PHB_ITEM   pInitValue;
} METHOD, * PMETHOD;

typedef struct
{
   char *  szName;
   WORD    wDatas;
   WORD    wDataFirst;   /* First wData from this class */
   PMETHOD pMethods;
   WORD    wMethods;
   WORD    wHashKey;
   PHB_ITEM   pClassDatas;  /* Array for ClassDatas */
   PHB_ITEM   pInlines;     /* Array for inline codeblocks */
} CLASS, * PCLASS;

#define BUCKET 4

extern STACK stack;
extern SYMBOL symEval;

PCLASS  pClasses = 0;
WORD    wClasses = 0;
PMETHOD pMethod  = 0;
PDYNSYM msgClassName = 0, msgClassH = 0, msgEval = 0, msgClassSel = 0;

/* All functions contained in classes.c */

       HARBOUR     CLASSADD( void );
       HARBOUR     CLASSCREATE( void );
       HARBOUR     CLASSDEL( void );
static HARBOUR     ClassH( void );
       HARBOUR     CLASSINSTANCE( void );
       HARBOUR     CLASSMOD( void );
static HARBOUR     ClassName( void );
       HARBOUR     CLASSNAME( void );
static HARBOUR     ClassSel( void );
static void        DictRealloc( PCLASS );
static HARBOUR     EvalInline( void );
static HARBOUR     GetClassData( void );
static HARBOUR     GetData( void );
       HARBOURFUNC GetMethod( PHB_ITEM, PSYMBOL );
       ULONG       hb_isMessage( PHB_ITEM, char *);
       HARBOUR     ISMESSAGE( void );
       HARBOUR     OCLONE( void );
       HARBOUR     OSEND( void );
       void        ReleaseClass( PCLASS );
       void        ReleaseClasses( void );
static HARBOUR     SelectSuper( void );
static HARBOUR     SetClassData( void );
static HARBOUR     SetData( void );
static HARBOUR     Virtual( void );
       HARBOUR     __INSTSUPER( void );
       HARBOUR     __WDATAS( void );
       HARBOUR     __WDATADEC( void );
       HARBOUR     __WDATAINC( void );

/* All function contained in matching objfunc.prg */

HARBOUR AODATA( void );
HARBOUR AOGET( void );
HARBOUR AOMETHOD( void );
HARBOUR AOSET( void );
HARBOUR ISDATA( void );
HARBOUR ISMETHOD( void );
HARBOUR OADDDATA( void );
HARBOUR OADDINLINE( void );
HARBOUR OADDMETHOD( void );
HARBOUR ODELDATA( void );
HARBOUR ODELINLINE( void );
HARBOUR ODELMETHOD( void );
HARBOUR OMODINLINE( void );
HARBOUR OMODMETHOD( void );

/* Exported symbols of both classes.c and objfunc.prg */

static SYMBOL symbols[] = {
{ "AODATA"        , FS_PUBLIC, AODATA         , 0 },
{ "AOGET"         , FS_PUBLIC, AOGET          , 0 },
{ "AOMETHOD"      , FS_PUBLIC, AOMETHOD       , 0 },
{ "AOSET"         , FS_PUBLIC, AOSET          , 0 },
{ "CLASSADD"      , FS_PUBLIC, CLASSADD       , 0 },
{ "CLASSCREATE"   , FS_PUBLIC, CLASSCREATE    , 0 },
{ "CLASSDEL"      , FS_PUBLIC, CLASSDEL       , 0 },
{ "CLASSINSTANCE" , FS_PUBLIC, CLASSINSTANCE  , 0 },
{ "CLASSMOD"      , FS_PUBLIC, CLASSMOD       , 0 },
{ "CLASSNAME"     , FS_PUBLIC, CLASSNAME      , 0 },
{ "ISDATA"        , FS_PUBLIC, ISDATA         , 0 },
{ "ISMESSAGE"     , FS_PUBLIC, ISMESSAGE      , 0 },
{ "ISMETHOD"      , FS_PUBLIC, ISMETHOD       , 0 },
{ "OADDDATA"      , FS_PUBLIC, OADDDATA       , 0 },
{ "OADDINLINE"    , FS_PUBLIC, OADDINLINE     , 0 },
{ "OADDMETHOD"    , FS_PUBLIC, OADDMETHOD     , 0 },
{ "OCLONE"        , FS_PUBLIC, OCLONE         , 0 },
{ "ODELDATA"      , FS_PUBLIC, ODELDATA       , 0 },
{ "ODELINLINE"    , FS_PUBLIC, ODELINLINE     , 0 },
{ "ODELMETHOD"    , FS_PUBLIC, ODELMETHOD     , 0 },
{ "OMODINLINE"    , FS_PUBLIC, OMODINLINE     , 0 },
{ "OMODMETHOD"    , FS_PUBLIC, OMODMETHOD     , 0 },
{ "OSEND"         , FS_PUBLIC, OSEND          , 0 }
};

void Classes__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}


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
HARBOUR CLASSADD()
{
   WORD    wClass = _parnl( 1 );
   WORD    wType  = _parni( 4 );
   WORD    wAt, wMask;

   PHB_ITEM   pInit  = _param( 5, IT_ANY );
   PCLASS  pClass;
   PDYNSYM pMessage;
   PMETHOD pNewMeth;

   if( wClass && wClass <= wClasses )
   {
      pClass   = &pClasses[ wClass - 1 ];
      pMessage = GetDynSym( _parc( 2 ) );
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
         pClass->wMethods++;                    /* One more message         */
      pNewMeth->pMessage = pMessage;

      switch( wType )
      {
         case MET_METHOD:
              pNewMeth->pFunction = ( HARBOURFUNC ) _parnl( 3 );
              break;

         case MET_DATA:
              pNewMeth->wData = _parnl( 3 );
              if( pMessage->pSymbol->szName[ 0 ] == '_' )
                 pNewMeth->pFunction = SetData;
              else
              {
                 pNewMeth->pFunction  = GetData;
                 if( pInit && !IS_NIL( pInit )) /* Initializer found        */
                 {
                    pNewMeth->pInitValue = hb_itemNew( NULL );
                    ItemCopy( pNewMeth->pInitValue, pInit );
                 }
              }
              break;

         case MET_CLASSDATA:
              pNewMeth->wData = _parnl( 3 );
              if( ( WORD ) hb_arrayLen( pClass->pClassDatas ) < _parnl( 3 ) )
                 hb_arraySize( pClass->pClassDatas, _parnl( 3 ) );

              if( pMessage->pSymbol->szName[ 0 ] == '_' )
                 pNewMeth->pFunction = SetClassData;
              else
                 pNewMeth->pFunction = GetClassData;
              break;

         case MET_INLINE:
              pNewMeth->wData = hb_arrayLen( pClass->pInlines ) + 1;
              hb_arraySize( pClass->pInlines, pNewMeth->wData );
              hb_arraySet(  pClass->pInlines, pNewMeth->wData,
                            _param( 3, IT_BLOCK ) );
              pNewMeth->pFunction = EvalInline;
              break;

         case MET_VIRTUAL:
              pNewMeth->pFunction = Virtual;
              break;

         case MET_SUPER:
              pNewMeth->wData     = _parnl( 3 );
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
HARBOUR CLASSCREATE()
{
   WORD   wSuper = _parni( 3 );                 /* Super class present      */
   WORD   wSize;
   PHB_ITEM  pItem;
   PCLASS pNewCls;
   PCLASS pSprCls;

   if( pClasses )
      pClasses = ( PCLASS ) _xrealloc( pClasses,
                 sizeof( CLASS ) * ( wClasses + 1 ) );
   else
      pClasses = ( PCLASS ) _xgrab( sizeof( CLASS ) );

   pNewCls = pClasses + wClasses;
   pNewCls->szName = ( char * ) _xgrab( _parclen( 1 ) + 1 );
   strcpy( pNewCls->szName, _parc( 1 ) );

   if( wSuper )
   {
      pSprCls = pClasses + wSuper - 1;

      pNewCls->wDataFirst  = pSprCls->wDatas;
      pNewCls->wDatas      = pSprCls->wDatas + _parni(2);
      pNewCls->wMethods    = pSprCls->wMethods;

      pNewCls->pClassDatas = hb_arrayClone( pSprCls->pClassDatas );
      pNewCls->pInlines    = hb_arrayClone( pSprCls->pInlines );

      pNewCls->wHashKey    = pSprCls->wHashKey;

      wSize = pSprCls->wHashKey * BUCKET * sizeof( METHOD );
      pNewCls->pMethods = ( PMETHOD ) _xgrab( wSize );
      memcpy( pNewCls->pMethods, pSprCls->pMethods, wSize );
   }                                            /* Copy all super methods   */
   else
   {
      pNewCls->wDatas     = _parni( 2 );
      pNewCls->wDataFirst = 0;
      pNewCls->pMethods   = ( PMETHOD ) _xgrab( 100 * sizeof( METHOD ) );
      pNewCls->wMethods   = 0;
      pNewCls->wHashKey   = 25;                 /* BUCKET = 4 repetitions   */

      pNewCls->pClassDatas = hb_itemArrayNew( 0 );
      pNewCls->pInlines    = hb_itemArrayNew( 0 );

      memset( pNewCls->pMethods, 0, 100 * sizeof( METHOD ) );
   }
   _retni( ++wClasses );
}


/*
 * ClassDel( <oObj>, <cMessage> )
 *
 * Delete message (only for INLINE and METHOD)
 *
 * <oObj>     Object
 * <cMessage> Message
 */
HARBOUR CLASSDEL()
{
   PHB_ITEM   pString  = _param( 2, IT_STRING );
   PSYMBOL pMessage = GetDynSym( pString->value.szText )->pSymbol;
   PDYNSYM pMsg     = ( PDYNSYM ) pMessage->pDynSym;
   PCLASS  pClass;

   WORD    wClass   = _parni( 1 );
   WORD    wAt;
   WORD    wLimit;
   WORD    wMask;

   HARBOURFUNC pFunc;

   if( wClass && wClass <= wClasses )
   {
      pClass = pClasses + wClass - 1;
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wMask  = pClass->wHashKey * BUCKET;
      wLimit = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      while( ( wAt != wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
         wAt = ( wAt == wMask ) ? 0 : wAt + 1;

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
            wAt = ( wAt == wMask ) ? 0 : wAt + 1;
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
   _retni( ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass );
}


/*
 * <oNewObject> := ClassInstance( <hClass> )
 *
 * Create a new object from class definition <hClass>
 */
HARBOUR CLASSINSTANCE()
{
   WORD    wClass = _parni( 1 );
   WORD    wAt, wLimit;
   PCLASS  pClass;
   PMETHOD pMeth;

   if( wClass <= wClasses )
   {
      pClass = pClasses + ( wClass - 1 );
      hb_arrayNew( &stack.Return, pClass->wDatas );
      ( ( PBASEARRAY ) stack.Return.value.pBaseArray )->wClass = wClass;

      pMeth  = pClass->pMethods;                /* Initialize DATA          */
      wLimit = pClass->wHashKey * BUCKET;
      for( wAt = 0; wAt < wLimit; wAt++, pMeth++ )
         if( pMeth->pInitValue )
            hb_itemArrayPut( &stack.Return, pMeth->wData, pMeth->pInitValue );
   }
   else
      _ret();
}

/*
 * ClassMod( <oObj>, <cMessage>, <pFunc> )
 *
 * Modify message (only for INLINE and METHOD)
 */
HARBOUR CLASSMOD()
{
   PHB_ITEM   pString  = _param( 2, IT_STRING );
   PSYMBOL pMessage = GetDynSym( pString->value.szText )->pSymbol;
   PDYNSYM pMsg     = ( PDYNSYM ) pMessage->pDynSym;
   PCLASS  pClass;

   WORD    wClass   = _parni( 1 );
   WORD    wAt;
   WORD    wLimit;
   WORD    wMask;

   HARBOURFUNC pFunc;

   if( wClass && wClass <= wClasses )
   {
      pClass = pClasses + wClass - 1;
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wMask  = pClass->wHashKey * BUCKET;
      wLimit = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      while( ( wAt != wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
         wAt = ( wAt == wMask ) ? 0 : wAt + 1;

      if( wAt != wLimit )
      {                                         /* Requested method found   */
         pFunc = pClass->pMethods[ wAt ].pFunction;
         if( pFunc == EvalInline )              /* INLINE method changed    */
            hb_arraySet( pClass->pInlines, pClass->pMethods[ wAt ].wData,
                         _param( 3, IT_BLOCK ) );
         else if( ( pFunc == SetData ) || ( pFunc == GetData ) )
         {                                      /* Not allowed for DATA     */
            PHB_ITEM pError = _errNew();
            _errPutDescription(pError, "CLASSMOD: Cannot modify a DATA item");
            _errLaunch(pError);
            _errRelease(pError);
         }
         else                                   /* Modify METHOD            */
            pClass->pMethods[ wAt ].pFunction = ( HARBOURFUNC ) _parnl( 3 );
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
      pItemRef = stack.pItems + ( stack.pBase + 1 )->value.wItem;
   else
      pItemRef = stack.pBase + 1;

   _retc( _GetClassName( pItemRef ) );
}


/*
 * <cClassName> := ClassName( <hClass> )
 *
 * Returns class name of <hClass>
 */
HARBOUR CLASSNAME()
{
   PHB_ITEM pObject = _param( 0, IT_OBJECT );
   WORD wClass;

   if( pObject && ( ( PBASEARRAY ) pObject->value.pBaseArray )->wClass )
   {
      wClass = ( ( PBASEARRAY ) pObject->value.pBaseArray )->wClass;
      _retc( pClasses[ wClass - 1 ].szName );
   }
   else
   {
      wClass = _parni( 1 );
      if( wClass <= wClasses )
         _retc( pClasses[ wClass - 1 ].szName );
      else
         _retc( "" );
   }
}


/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
static HARBOUR ClassSel()
{
   WORD    wClass = IS_ARRAY( stack.pBase + 1 ) ?
        ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass : 0;
                                                /* Get class word           */
   WORD    wLimit;                              /* Number of Hash keys      */
   WORD    wAt;
   WORD    wPos = 0;
   PCLASS  pClass;
   PDYNSYM pMessage;
   PHB_ITEM   pReturn = hb_itemNew( NULL );
   PHB_ITEM   pItem;
   PHB_ITEM   pItemRef;

   /* Variables by reference */
   if( ( ! wClass ) && IS_BYREF( stack.pBase + 1 ) )
   {
      pItemRef = stack.pItems + ( stack.pBase + 1 )->value.wItem;
      if( IS_ARRAY( pItemRef ) )
         wClass = ( ( PBASEARRAY ) pItemRef->value.pBaseArray )->wClass;
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
         pMessage = (PDYNSYM) pClass->pMethods[ wAt ].pMessage;
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
   WORD wClass = ( (PBASEARRAY) ( stack.pBase + 1 )->value.pBaseArray )->wClass;
   WORD w;

   hb_arrayGet( pClasses[ wClass - 1 ].pInlines, pMethod->wData, &block );

   PushSymbol( &symEval );
   Push( &block );
   Push( stack.pBase + 1 );                     /* Push self                */
   for( w = 1; w <= _pcount(); w++ )
      Push( _param( w, IT_ANY ) );
   Do( _pcount() + 1 );                         /* Self is also an argument */

   ItemRelease( &block );                       /* Release block            */
}


/*
 * <szName> = _GetClassName( pObject )
 *
 * Get the class name of an object
 *
 */
char * _GetClassName( PHB_ITEM pObject )
{
   char * szClassName;

   if( IS_ARRAY( pObject ) )
   {
      if( ! ( ( PBASEARRAY ) pObject->value.pBaseArray )->wClass )
         szClassName = "ARRAY";
      else
         szClassName = ( pClasses + ( ( PBASEARRAY ) pObject->value.pBaseArray )->wClass - 1 )->szName;
   }
   else  /* built in types */
   {
      switch( pObject->wType )
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
   WORD wClass = ( (PBASEARRAY) ( stack.pBase + 1 )->value.pBaseArray )->wClass;

   if( wClass && wClass <= wClasses )
      hb_arrayGet( pClasses[ wClass - 1 ].pClassDatas, pMethod->wData,
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
   WORD  wIndex  = pMethod->wData;

   if( wIndex > ( WORD ) hb_arrayLen ( pObject ) )
                                                /* Resize needed            */
      hb_arraySize( pObject, wIndex );          /* Make large enough        */
   hb_arrayGet( pObject, wIndex, &stack.Return );
}


/*
 * <pFunc> = GetMethod( <pObject>, <pMessage> )
 *
 * Internal function to the function pointer of a message of an object
 */
HARBOURFUNC GetMethod( PHB_ITEM pObject, PSYMBOL pMessage )
{
   WORD    wAt, wLimit, wMask;
   WORD    wClass = ( ( PBASEARRAY ) pObject->value.pBaseArray )->wClass;
   PCLASS  pClass;
   PDYNSYM pMsg = ( PDYNSYM ) pMessage->pDynSym;

   if( ! msgClassName )
   {
      msgClassName = GetDynSym( "CLASSNAME" );  /* Standard messages        */
      msgClassH    = GetDynSym( "CLASSH" );     /* Not present in classdef. */
      msgClassSel  = GetDynSym( "CLASSSEL" );
      msgEval      = GetDynSym( "EVAL" );
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
         wAt = ( wAt == wMask ) ? 0 : wAt + 1;
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
   PSYMBOL pMessage = GetDynSym( szString )->pSymbol;
   return( (ULONG) GetMethod( pObject, pMessage ) );
}                                                /* Get funcptr of message   */


/*
 * <lRet> := IsMessage( <oObj>, <cSymbol> )
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
HARBOUR ISMESSAGE()
{
   PHB_ITEM   pObject  = _param( 1, IT_OBJECT );
   PHB_ITEM   pString  = _param( 2, IT_STRING );

   if( pObject && pString )
      _retl( hb_isMessage( pObject, pString->value.szText ) != 0 );
   else
   {
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Argument error: ISMESSAGE");
      _errLaunch(pError);
      _errRelease(pError);
   }
}


/*
 * <oNew> := oClone( <oOld> )
 *
 * Clone an object. Note the similarity with aClone ;-)
 */
HARBOUR OCLONE( void )
{
   PHB_ITEM pSrcObject  = _param( 1, IT_OBJECT );

   if ( pSrcObject )
   {
      PHB_ITEM pDstObject = hb_arrayClone( pSrcObject );

      ItemCopy( &stack.Return, pDstObject );
      hb_itemRelease( pDstObject );
   }
   else
   {
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Argument error: OCLONE");
      _errLaunch(pError);
      _errRelease(pError);
   }
}


/*
 * <xRet> = oSend( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a message to an object
 */
HARBOUR OSEND()
{
   PHB_ITEM pObject  = _param( 1, IT_OBJECT );
   PHB_ITEM pMessage = _param( 2, IT_STRING );
   WORD  w;

   if( pMessage && pObject )                /* Object & message passed      */
   {
      Push( pObject );                      /* Push object                  */
      Message( GetDynSym( pMessage->value.szText )->pSymbol );
                                            /* Push char symbol as message  */
      for( w = 3; w <= _pcount(); w++ )     /* Push arguments on stack      */
         Push( _param( w, IT_ANY ) );
      Do( _pcount()-2 );                    /* Execute message              */
   }
   else
   {
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Argument error: OSEND");
      _errLaunch(pError);
      _errRelease(pError);
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

   _xfree( pClass->szName );
   _xfree( pClass->pMethods );

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
      _xfree( pClasses );
}


/*
 * SelectSuper()
 *
 * Internal function to cast to a super method
 */
static HARBOUR SelectSuper( void )
{
   PHB_ITEM      pObject   = stack.pBase + 1;
   PHB_ITEM      pSuper    = (PHB_ITEM) _xgrab( sizeof( HB_ITEM ) );
   PBASEARRAY pNewBase  = (PBASEARRAY) _xgrab( sizeof( BASEARRAY ) );
   WORD       wSuperCls = pMethod->wData;       /* Get handle of superclass */

   memcpy( pSuper,   pObject, sizeof( HB_ITEM ) ); /* Allocate new structures  */
   memcpy( pNewBase, pObject->value.pBaseArray, sizeof( BASEARRAY ) );

   pSuper->value.pBaseArray = pNewBase;

   pNewBase->wClass     = wSuperCls;
   pNewBase->wHolders   = 1;                    /* New item is returned     */
   pNewBase->wSuperCast = TRUE;                 /* Do not dispose pItems !! */
                                                /* A bit dirty, but KISS.   */
   ItemCopy( &stack.Return, pSuper );
   hb_itemRelease( pSuper );
}


/*
 * SetClassData()
 *
 * Internal function to set a CLASSDATA
 */
static HARBOUR SetClassData( void )
{
   WORD wClass = ( (PBASEARRAY) ( stack.pBase + 1 )->value.pBaseArray )->wClass;
   PHB_ITEM pReturn = stack.pBase + 2;

   if( wClass && wClass <= wClasses )
   {
      hb_arraySet( pClasses[ wClass - 1 ].pClassDatas,
                   pMethod->wData, pReturn );
      ItemCopy( &stack.Return, pReturn );
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
      hb_arraySize( pObject, wIndex );          /* Make large enough        */
   hb_arraySet( pObject, wIndex, pReturn );
   ItemCopy( &stack.Return, pReturn );
}


/* No comment :-) */
static HARBOUR Virtual( void )
{
   _ret();
}


/*
 * <hClass> := __InstSuper( <cName> )
 *
 * Instance super class and return class handle
 */
HARBOUR __INSTSUPER( void )
{
   PHB_ITEM   pString = _param( 1, IT_STRING );
   PDYNSYM pDynSym;
   PHB_ITEM   pSuperCls;
   BYTE    bFound  = FALSE;
   WORD    w;

   if( pString )
   {
      pDynSym = FindDynSym( pString->value.szText );
      if( pDynSym )                             /* Find function            */
      {
         PushSymbol( pDynSym->pSymbol );        /* Push function name       */
         PushNil();
         Function( 0 );                         /* Execute super class      */

         if( !IS_OBJECT( &stack.Return ) )
         {
            PHB_ITEM pError = _errNew();
            _errPutDescription(pError, "INSTSUPER : Super class does not return an object");
            _errLaunch(pError);
            _errRelease(pError);
         }

         for( w = 0; !bFound && w < wClasses; w++ )
         {                                      /* Locate the entry         */
            if( !stricmp( pString->value.szText, pClasses[ w ].szName ) )
            {
               _retni( w + 1 );                 /* Entry + 1 = ClassH       */
               bFound = TRUE;
            }
         }
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "INSTSUPER : Cannot find super class");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   if( !bFound )
      _retni( 0 );
}


/*
 * <nSeq> = __wDataDec( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR __WDATADEC()
{
   WORD wClass = _parnl( 1 );

   if( wClass )
      _retni( pClasses[ wClass - 1 ].wDatas-- );
}


/*
 * <nSeq> = __wDataInc( <hClass> )
 *
 * Return number of datas and decrease
 */
HARBOUR __WDATAINC()
{
   WORD wClass = _parnl( 1 );

   if( wClass )
      _retni( ++pClasses[ wClass - 1 ].wDatas );
}


/*
 * <nSeq> = __wDatas( <hClass> )
 *
 * Return number of datas
 */
HARBOUR __WDATAS()
{
   WORD wClass = _parnl( 1 );

   if( wClass )
      _retni( pClasses[ wClass - 1 ].wDatas );
}


