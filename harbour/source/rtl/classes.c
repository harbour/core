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
   PITEM   pInitValue;
} METHOD, * PMETHOD;

typedef struct
{
   char *  szName;
   WORD    wDatas;
   WORD    wDataFirst;   /* First wData from this class */
   PMETHOD pMethods;
   WORD    wMethods;
   WORD    wHashKey;
   PITEM   pClassDatas;  /* Array for ClassDatas */
   PITEM   pInlines;     /* Array for inline codeblocks */
} CLASS, * PCLASS;

#define BUCKET 4

extern STACK stack;
extern SYMBOL symEval;

PCLASS  pClasses = 0;
WORD    wClasses = 0;
PMETHOD pMethod  = 0;
PDYNSYM msgClassName = 0, msgClassH = 0, msgEval = 0, msgClassSel = 0;

       HARBOUR     CLASSADD();
       HARBOUR     CLASSCREATE();
       HARBOUR     CLASSDEL();
static HARBOUR     ClassH();
       HARBOUR     CLASSINSTANCE();
       HARBOUR     CLASSMOD();
static HARBOUR     ClassName();
       HARBOUR     CLASSNAME();
static HARBOUR     ClassSel();
static void        DictRealloc( PCLASS );
static HARBOUR     EvalInline();
static HARBOUR     GetClassData();
static HARBOUR     GetData();
       HARBOURFUNC GetMethod( PITEM, PSYMBOL );
       ULONG       hb_isMessage( PITEM, char *);
       HARBOUR     ISMESSAGE();
       HARBOUR     OCLONE();
       HARBOUR     OSEND();
       void        ReleaseClass( PCLASS );
       void        ReleaseClasses();
static HARBOUR     SelectSuper();
static HARBOUR     SetClassData();
static HARBOUR     SetData();
static HARBOUR     Virtual();
       HARBOUR     __INSTSUPER();
       HARBOUR     __WDATAS();
       HARBOUR     __WDATADEC();
       HARBOUR     __WDATAINC();

static SYMBOL symbols[] = {
{ "CLASSADD"      , FS_PUBLIC, CLASSADD       , 0 },
{ "CLASSCREATE"   , FS_PUBLIC, CLASSCREATE    , 0 },
{ "CLASSDEL"      , FS_PUBLIC, CLASSDEL       , 0 },
{ "CLASSINSTANCE" , FS_PUBLIC, CLASSINSTANCE  , 0 },
{ "CLASSMOD"      , FS_PUBLIC, CLASSMOD       , 0 },
{ "CLASSNAME"     , FS_PUBLIC, CLASSNAME      , 0 },
{ "ISMESSAGE"     , FS_PUBLIC, ISMESSAGE      , 0 },
{ "OCLONE"        , FS_PUBLIC, OCLONE         , 0 },
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
   WORD    wAt, wLimit, wMask;

   PITEM   pInit  = _param( 5, IT_ANY );
   PCLASS  pClass;
   PDYNSYM pMessage;

   if( wClass && wClass <= wClasses )
   {
      pClass   = &pClasses[ wClass - 1 ];
      pMessage = GetDynSym( _parc( 2 ) );
      wAt      = ( ( ( unsigned ) pMessage ) % pClass->wHashKey ) * BUCKET;
      wMask    = pClass->wHashKey * BUCKET;
      wLimit   = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      if( pClass->wMethods > ( pClass->wHashKey * BUCKET * 2/3 ) )
         DictRealloc( pClass );

      while( ( wAt != wLimit ) &&
               pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMessage ) )
         wAt = ( wAt == wMask ) ? 0 : wAt + 1;

      if( wAt != wLimit )
      {
         if( !pClass->pMethods[ wAt ].pMessage )
            pClass->wMethods++;
         pClass->pMethods[ wAt ].pMessage = pMessage;
         switch( wType )
         {
            case MET_METHOD:
                 pClass->pMethods[ wAt ].pFunction = ( HARBOURFUNC ) _parnl( 3 );
                 break;

            case MET_DATA:
                 pClass->pMethods[ wAt ].wData = _parnl( 3 );
                 if( pMessage->pSymbol->szName[ 0 ] == '_' )
                    pClass->pMethods[ wAt ].pFunction = SetData;
                 else
                 {
                    pClass->pMethods[ wAt ].pFunction  = GetData;
                    if( pInit && !IS_NIL( pInit ) )
                    {                           /* Initializer found        */
                       pClass->pMethods[ wAt ].pInitValue = hb_itemNew( NULL );
                       ItemCopy( pClass->pMethods[ wAt ].pInitValue, pInit );
                    }
                 }
                 break;

            case MET_CLASSDATA:
                 pClass->pMethods[ wAt ].wData = _parnl( 3 );
                 if( ( WORD ) hb_arrayLen( pClass->pClassDatas ) < _parnl( 3 ) )
                    hb_arraySize( pClass->pClassDatas, _parnl( 3 ) );

                 if( pMessage->pSymbol->szName[ 0 ] == '_' )
                    pClass->pMethods[ wAt ].pFunction = SetClassData;
                 else
                    pClass->pMethods[ wAt ].pFunction = GetClassData;
                 break;

            case MET_INLINE:
                 pClass->pMethods[ wAt ].wData = hb_arrayLen( pClass->pInlines ) + 1;
                 hb_arraySize( pClass->pInlines, pClass->pMethods[ wAt ].wData );
                 hb_arraySet( pClass->pInlines, pClass->pMethods[ wAt ].wData,
                           _param( 3, IT_BLOCK ) );
                 pClass->pMethods[ wAt ].pFunction = EvalInline;
                 break;

            case MET_VIRTUAL:
                 pClass->pMethods[ wAt ].pFunction = Virtual;
                 break;

            case MET_SUPER:
                 pClass->pMethods[ wAt ].wData     = _parnl( 3 );
                 pClass->pMethods[ wAt ].pFunction = SelectSuper;
                 break;

            default:
                 printf( "Invalid method type from ClassAdd\n" );
                 exit( 1 );
                 break;
         }
         return;
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
   PITEM  pItem;
   PCLASS pNewCls;
   PCLASS pSprCls;

   if( pClasses )
      pClasses = ( PCLASS ) _xrealloc( pClasses, sizeof( CLASS ) * ( wClasses + 1 ) );
   else
      pClasses = ( PCLASS ) _xgrab( sizeof( CLASS ) );

   pNewCls = pClasses + wClasses;
   pNewCls->szName = ( char * ) _xgrab( _parclen( 1 ) + 1 );
   strcpy( pNewCls->szName, _parc( 1 ) );

   if( wSuper )
   {
      pSprCls = pClasses + wSuper - 1;

      pNewCls->wDataFirst = pSprCls->wDatas;
      pNewCls->wDatas     = pSprCls->wDatas + _parni(2);
      pNewCls->wMethods   = pSprCls->wMethods;

      pNewCls->pClassDatas = hb_arrayClone( pSprCls->pClassDatas );
      pNewCls->pInlines    = hb_arrayClone( pSprCls->pInlines );

      pNewCls->wHashKey = pSprCls->wHashKey;

      wSize = pSprCls->wHashKey * BUCKET * sizeof( METHOD );
      pNewCls->pMethods = ( PMETHOD ) _xgrab( wSize );
      memcpy( pNewCls->pMethods, pSprCls->pMethods, wSize );
   }
   else
   {
      pNewCls->wDatas = _parni( 2 );
      pNewCls->wDataFirst = 0;
      pNewCls->pMethods = ( PMETHOD ) _xgrab( 100 * sizeof( METHOD ) );
      pNewCls->wMethods = 0;
      pNewCls->wHashKey = 25; /* BUCKET = 4 repetitions */

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
   PITEM   pString  = _param( 2, IT_STRING );
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
      wLimit = ( wAt - 1 ) % wMask;

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
         }                                      /* Move messages            */
         for( ; pClass->pMethods[ wAt ].pMessage && wAt < wLimit; wAt ++ )
            memcpy( &( pClass->pMethods[ wAt ]     ),
                    &( pClass->pMethods[ wAt + 1 ] ), sizeof( METHOD ) );

         pClass->pMethods[ wAt ].pFunction  = NULL;
         pClass->pMethods[ wAt ].pMessage   = NULL;
         pClass->pMethods[ wAt ].wData      = NULL;
         pClass->pMethods[ wAt ].wScope     = NULL;
         pClass->pMethods[ wAt ].pInitValue = NULL;

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
   WORD   wClass = _parni( 1 );
   WORD   wAt, wLimit;
   PCLASS pClass;

   if( wClass <= wClasses )
   {
      pClass = pClasses + ( wClass - 1 );
      hb_arrayNew( &stack.Return, pClass->wDatas );
      ( ( PBASEARRAY ) stack.Return.value.pBaseArray )->wClass = wClass;
                                                /* Initialize DATA          */
      wLimit = pClass->wHashKey * BUCKET;
      for( wAt = 0; wAt < wLimit; wAt++ )
      {
         if(  pClass->pMethods[ wAt ].pInitValue )
         {
            hb_itemArrayPut( &stack.Return,
                             pClass->pMethods[ wAt ].wData,
                             pClass->pMethods[ wAt ].pInitValue );
         }
      }
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
   PITEM   pString  = _param( 2, IT_STRING );
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
      wLimit   = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      while( ( wAt != wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
         wAt = ( wAt == wMask ) ? 0 : wAt + 1;

      if( wAt != wLimit )
      {                                         /* Requested method found   */
         pFunc = pClass->pMethods[ wAt ].pFunction;
         if( pFunc == EvalInline )              /* INLINE method changed    */
         {
            hb_arraySet( pClass->pInlines, pClass->pMethods[ wAt ].wData,
                       _param( 3, IT_BLOCK ) );
         }
         else if( ( pFunc == SetData ) || ( pFunc == GetData ) )
         {                                      /* Not allowed for DATA     */
            printf( "\nCannot modify a DATA item" );
            exit(1);
         }                                      /* TODO : Real error        */
         else                                   /* Modify METHOD            */
         {
            pClass->pMethods[ wAt ].pFunction = ( HARBOURFUNC ) _parnl( 3 );
         }
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
   WORD wClass = IS_ARRAY( stack.pBase + 1 ) ?
                 ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass: 0;
   PITEM pItemRef;

   /* Variables by reference */
   if( ( ! wClass ) && IS_BYREF( stack.pBase + 1 ) )
   {
      pItemRef = stack.pItems + ( stack.pBase + 1 )->value.wItem;
      if( IS_ARRAY( pItemRef ) )
         wClass = ( ( PBASEARRAY ) pItemRef->value.pBaseArray )->wClass;
   }

   if( wClass && ( wClass <= wClasses ) )
      _retc( pClasses[ wClass - 1 ].szName );
   else
   {
      switch( ( stack.pBase )->wType & ~IT_BYREF )
      {
         case IT_ARRAY:
              _retc( "ARRAY" );
              break;

         case IT_BLOCK:
              _retc( "BLOCK" );
              break;

         case IT_STRING:
              _retc( "CHARACTER" );
              break;

         case IT_DATE:
              _retc( "DATE" );
              break;

         case IT_LOGICAL:
              _retc( "LOGICAL" );
              break;

         case IT_INTEGER:
         case IT_LONG:
         case IT_DOUBLE:
              _retc( "NUMERIC" );
              break;

         default:
              _retc( "NIL" );
              break;
      }
   }
}


/*
 * <cClassName> := ClassName( <hClass> )
 *
 * Returns class name of <hClass>
 */
HARBOUR CLASSNAME() 
{
   PITEM pObject = _param( 0, IT_OBJECT );
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
        ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass: 0;
                                                /* Get class word           */
   WORD    wLimit;                              /* Number of Hash keys      */
   WORD    wAt;
   WORD    wPos = 0;
   PCLASS  pClass;
   PDYNSYM pMessage;
   PITEM   pReturn = hb_itemNew( NULL );
   PITEM   pItem;
   PITEM   pItemRef;

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
   hb_itemReturn( pReturn );
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
   ITEM block;
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;
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
 * GetClassData()
 *
 * Internal function to return a CLASSDATA
 */
static HARBOUR GetClassData( void )
{
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;

   if( wClass && wClass <= wClasses )
      hb_arrayGet( pClasses[ wClass - 1 ].pClassDatas, pMethod->wData, &stack.Return );
}


/*
 * GetData()
 *
 * Internal function to return a DATA
 */
static HARBOUR GetData( void )
{
   PITEM pObject = stack.pBase + 1;
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
HARBOURFUNC GetMethod( PITEM pObject, PSYMBOL pMessage )
{
   PCLASS pClass;
   WORD   wAt, wLimit, wMask;
   WORD   wClass = ( ( PBASEARRAY ) pObject->value.pBaseArray )->wClass;
   PDYNSYM pMsg = ( PDYNSYM ) pMessage->pDynSym;

   if( ! msgClassName )
   {
      msgClassName = GetDynSym( "CLASSNAME" );
      msgClassH    = GetDynSym( "CLASSH" );
      msgClassSel  = GetDynSym( "CLASSSEL" );
      msgEval      = GetDynSym( "EVAL" );
   }

   if( wClass && wClass <= wClasses )
   {
      pClass = &pClasses[ wClass - 1 ];
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wMask  = pClass->wHashKey * BUCKET;
      wLimit   = wAt ? ( wAt - 1 ) : ( wMask - 1 );

      pMethod = 0;

      while( wAt != wLimit )
      {
         if( pClass->pMethods[ wAt ].pMessage == pMsg )
         {
            pMethod = &pClass->pMethods[ wAt ];
            return pClass->pMethods[ wAt ].pFunction;
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
ULONG hb_isMessage( PITEM pObject, char *szString )
{
   PSYMBOL pMessage = GetDynSym( szString )->pSymbol;
   return( (ULONG) GetMethod( pObject, pMessage ) );
                                                /* Get function pointer of  */
                                                /* message                  */
}


/*
 * <lRet> := IsMessage( <oObj>, <cSymbol> )
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
HARBOUR ISMESSAGE()     
{
   PITEM   pObject  = _param( 1, IT_OBJECT );
   PITEM   pString  = _param( 2, IT_STRING );

   if( pObject && pString )
      _retl( hb_isMessage( pObject, pString->value.szText ) != 0 );
   else
   {
      PITEM pError = _errNew();
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
  PITEM pSrcObject  = _param( 1, IT_OBJECT );

  if ( pSrcObject )
    {
      PITEM pDstObject = hb_arrayClone( pSrcObject );
      ItemCopy( &stack.Return, pDstObject ); /* OClone() returns the new object */
      hb_itemRelease( pDstObject );
    }
  else
    _ret();
}


/*
 * <xRet> = oSend( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a message to an object
 */
HARBOUR OSEND()             
{                           
   PITEM pObject  = _param( 1, IT_OBJECT );
   PITEM pMessage = _param( 2, IT_STRING );
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
      PITEM pError = _errNew();
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

   wLimit   = pClass->wHashKey * BUCKET;
   for( wAt = 0; wAt < wLimit; wAt++ )          /* Release initializers     */
      if( pClass->pMethods[ wAt ].pInitValue &&
          pClass->pMethods[ wAt ].wData      >  pClass->wDataFirst )
      {
         hb_itemRelease( pClass->pMethods[ wAt ].pInitValue );
      }
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
   {
      ReleaseClass( pClasses + w );
   }

   if( pClasses )
      _xfree( pClasses );
}


/*
 * SelectSuper()
 *
 * Internal function to cast to a super method
 */
static HARBOUR SelectSuper( void )              /* Without aClone !!!       */
{
   PITEM      pObject  = stack.pBase + 1;
   PITEM      pSuper   = (PITEM) _xgrab( sizeof( ITEM ) );
   PBASEARRAY pNewBase = (PBASEARRAY) _xgrab( sizeof( BASEARRAY ) );

   WORD wSuperCls = pMethod->wData;             /* Get handle of superclass */

   memcpy( pSuper, pObject, sizeof( ITEM ) );   /* Allocate new structures  */
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
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;

   if( wClass && wClass <= wClasses )
      hb_arraySet( pClasses[ wClass - 1 ].pClassDatas, pMethod->wData, stack.pBase + 2 );
}


/*
 * SetData()
 *
 * Internal function to set a DATA
 */
static HARBOUR SetData( void )
{
   PITEM pObject = stack.pBase + 1;
   WORD  wIndex  = pMethod->wData;

   if( wIndex > ( WORD ) hb_arrayLen( pObject ) )
                                                /* Resize needed            */
      hb_arraySize( pObject, wIndex );          /* Make large enough        */
   hb_arraySet( pObject, wIndex, stack.pBase + 2 );
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
   PITEM   pString = _param( 1, IT_STRING );
   PDYNSYM pDynSym;
   PITEM   pSuperCls;
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
            PITEM pError = _errNew();
            _errPutDescription(pError, "INSTSUPER : Super class does not return an object");
            _errLaunch(pError);
            _errRelease(pError);
         }

         for( w = 0; !bFound && w < wClasses; w++ )
         {                                      /* Locate the entry         */
            if( !stricmp( pString->value.szText, (pClasses + w)->szName ) )
            {
               _retni( w + 1 );                 /* Entry + 1 = ClassH       */
               bFound = TRUE;
            }
         }
      }
      else
      {
         PITEM pError = _errNew();
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
   WORD  wClass   = _parnl( 1 );

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
   WORD  wClass   = _parnl( 1 );

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
   WORD  wClass   = _parnl( 1 );

   if( wClass )
      _retni( pClasses[ wClass - 1 ].wDatas );  
}


