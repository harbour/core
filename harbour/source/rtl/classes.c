/*
 * $Id$
 */

#include <extend.h>
#include <itemapi.h>

void Push( PITEM );
void PushNil( void );
void PushSymbol( PSYMBOL );
void Message( PSYMBOL );
void Do( WORD wParams );
void Function( WORD wParams );

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

HARBOUR CLASSCREATE() /* cClassName, nDatas, xSuper --> hClass */
{
   WORD  hSuper = 0;
   PITEM pSuper = _param( 3, IT_ANY );          /* Super class present      */
   PITEM pItem;

   if( ! pClasses )
      pClasses = ( PCLASS ) _xgrab( sizeof( CLASS ) );
   else
      pClasses = ( PCLASS ) _xrealloc( pClasses, sizeof( CLASS ) * ( wClasses + 1 ) );

   pClasses[ wClasses ].szName = ( char * ) _xgrab( _parclen( 1 ) + 1 );
   strcpy( pClasses[ wClasses ].szName, _parc( 1 ) );


   if( pSuper )
   {
      if( IS_NUMERIC( pSuper ) )
         hSuper = _parni( 3 ) - 1;
      else if( IS_ARRAY( pSuper ) )
      {
         if( hb_arrayLen( pSuper ) != 1 )
            printf( "\nMultiple inheritance not supported yet" );
         else
         {
            pItem = hb_itemArrayGet( pSuper, 1 );
            hSuper = (WORD) pItem->value.iNumber - 1;
            hb_itemRelease( pItem );
         }
      }
      pClasses[ wClasses ].wDataFirst = pClasses[ hSuper ].wDatas;
      pClasses[ wClasses ].wDatas     = pClasses[ hSuper ].wDatas + _parni(2);
      pClasses[ wClasses ].wMethods   = pClasses[ hSuper ].wMethods;

      pClasses[ wClasses ].pClassDatas =
         hb_arrayClone( pClasses[ hSuper ].pClassDatas );
      pClasses[ wClasses ].pInlines =
         hb_arrayClone( pClasses[ hSuper ].pInlines );

      pClasses[ wClasses ].wHashKey = pClasses[ hSuper ].wHashKey;
      pClasses[ wClasses ].pMethods = ( PMETHOD ) _xgrab(
         pClasses[ hSuper ].wHashKey * BUCKET * sizeof( METHOD ) );
      memcpy( pClasses[ wClasses ].pMethods,
              pClasses[ hSuper ].pMethods,
              pClasses[ hSuper ].wHashKey * BUCKET * sizeof( METHOD ) );
   }
   else
   {
      pClasses[ wClasses ].wDatas = _parni( 2 );
      pClasses[ wClasses ].wDataFirst = 0;
      pClasses[ wClasses ].pMethods = ( PMETHOD ) _xgrab( 100 * sizeof( METHOD ) );
      pClasses[ wClasses ].wMethods = 0;
      pClasses[ wClasses ].wHashKey = 25; /* BUCKET = 4 repetitions */

      pClasses[ wClasses ].pClassDatas = hb_itemArrayNew( 0 );
      pClasses[ wClasses ].pInlines    = hb_itemArrayNew( 0 );

      memset( pClasses[ wClasses ].pMethods, 0, 100 * sizeof( METHOD ) );
   }
   _retni( ++wClasses );
}

static HARBOUR ClassH( void )
{
   _retni( ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass );
}

char * _GetClassName( PITEM pObject )
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

static HARBOUR GetData( void )
{
   PITEM pObject = stack.pBase + 1;
   WORD  wIndex  = pMethod->wData;

   if( wIndex > ( WORD ) hb_arrayLen ( pObject ) )
                                                /* Resize needed            */
      hb_arraySize( pObject, wIndex );          /* Make large enough        */
   hb_arrayGet( pObject, wIndex, &stack.Return );
}

static HARBOUR SetData( void )
{
   PITEM pObject = stack.pBase + 1;
   WORD  wIndex  = pMethod->wData;

   if( wIndex > ( WORD ) hb_arrayLen( pObject ) )
                                                /* Resize needed            */
      hb_arraySize( pObject, wIndex );          /* Make large enough        */
   hb_arraySet( pObject, wIndex, stack.pBase + 2 );
}

static HARBOUR GetClassData( void )
{
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;

   if( wClass && wClass <= wClasses )
      hb_arrayGet( pClasses[ wClass - 1 ].pClassDatas, pMethod->wData, &stack.Return );
}

static HARBOUR SetClassData( void )
{
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;

   if( wClass && wClass <= wClasses )
      hb_arraySet( pClasses[ wClass - 1 ].pClassDatas, pMethod->wData, stack.pBase + 2 );
}

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

static HARBOUR Virtual( void )
{
   _ret();
}

static HARBOUR SelectSuper( void )
{
   PITEM pObject   = stack.pBase + 1;
   PITEM pSuper    = hb_arrayClone( pObject );
   WORD  wSuperCls = pMethod->wData;

   ItemCopy( &stack.Return, pSuper );
   ( (PBASEARRAY) (stack.Return.value.pBaseArray) )->wClass = wSuperCls;
   hb_itemRelease( pSuper );
}

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
      switch( ( stack.pBase )->wType )
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

static void DictRealloc( PCLASS pClass )
{
   /* TODO: Implement it for very large classes */
   if( pClass )
   {
      printf( "classes.c DictRealloc not implemented yet\n" );
      exit( 1 );
   }
}

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

HARBOUR CLASSADD() /* hClass, cMessage, pFunction, nType, xInit */
{
   WORD wClass = _parnl( 1 );
   WORD wType  = _parni( 4 );
   WORD    wAt, wLimit;
   PITEM   pInit  = _param( 5, IT_ANY );
   PCLASS pClass;
   PDYNSYM pMessage;

   if( wClass && wClass <= wClasses )
   {
      pClass   = &pClasses[ wClass - 1 ];
      pMessage = GetDynSym( _parc( 2 ) );
      wAt      = ( ( ( unsigned ) pMessage ) % pClass->wHashKey ) * BUCKET;
      wLimit   = pClass->wHashKey * BUCKET;

      if( pClass->wMethods > ( pClass->wHashKey * BUCKET * 2/3 ) )
         DictRealloc( pClass );

      while( ( wAt < wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMessage ) ) )
         wAt++;

      if( wAt < wLimit )
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

HARBOUR CLASSNAME() /* hClass --> cClassName */
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

HARBOUR CLASSINSTANCE() /* hClass --> oNewObject */
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

HARBOURFUNC GetMethod( PITEM pObject, PSYMBOL pMessage )
{
   PCLASS pClass;
   WORD   wAt, wLimit;
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
      wLimit = pClass->wHashKey * BUCKET;

      pMethod = 0;

      while( wAt < wLimit )
      {
         if( pClass->pMethods[ wAt ].pMessage == pMsg )
         {
            pMethod = &pClass->pMethods[ wAt ];
            return pClass->pMethods[ wAt ].pFunction;
         }
         wAt++;
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

ULONG hb_isMessage( PITEM pObject, char *szString )
{
   PSYMBOL pMessage = GetDynSym( szString )->pSymbol;
   return( (ULONG) GetMethod( pObject, pMessage ) );
                                                /* Get function pointer of  */
                                                /* message                  */
}

HARBOUR ISMESSAGE()     /* Is the message valid for the class               */
                        /* <lRet> := IsMessage( <oObj>, <cSymbol> )         */
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



HARBOUR OSEND()             /* <xRet> = oSend( <oObj>, <cSymbol>, <xArg,..> */
{                           /* Send a message to an object                  */
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


HARBOUR __WDATAS()           /* <nSeq> = __wDatas( <hClass> )  */
{
   WORD  wClass   = _parnl( 1 );

   if( wClass )
      _retni( pClasses[ wClass - 1 ].wDatas );  /* Return number of DATAs  */
}


HARBOUR __WDATAINC()         /* <nSeq> = __wDataInc( <hClass> )*/
{
   WORD  wClass   = _parnl( 1 );

   if( wClass )
      _retni( ++pClasses[ wClass - 1 ].wDatas ); /* Return and increase     */
}                                                /* number of DATAs         */


HARBOUR __WDATADEC()         /* <nSeq> = __wDataDec( <hClass> )*/
{
   WORD  wClass   = _parnl( 1 );

   if( wClass )
      _retni( pClasses[ wClass - 1 ].wDatas-- ); /* Return and decrease     */
}                                                /* number of DATAs         */


HARBOUR CLASSMOD()      /* Modify message (only for INLINE and METHOD)      */
                        /* <xOld> := ClassMod( <oObj>, <cSymbol>, <pFunc> ) */
{
   PITEM   pString  = _param( 2, IT_STRING );
   PSYMBOL pMessage = GetDynSym( pString->value.szText )->pSymbol;
   PDYNSYM pMsg     = ( PDYNSYM ) pMessage->pDynSym;
   PCLASS  pClass;

   WORD    wClass   = _parni( 1 );
   WORD    wAt;
   WORD    wLimit;

   HARBOURFUNC pFunc;

   if( wClass && wClass <= wClasses )
   {
      pClass = &pClasses[ wClass - 1 ];
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wLimit = pClass->wHashKey * BUCKET;

      while( ( wAt < wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
         wAt++;

      if( wAt < wLimit )
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

HARBOUR CLASSDEL()      /* Delete message (only for INLINE and METHOD)      */
                        /* <xOld> := ClassDel( <oObj>, <cSymbol> )          */
{
   PITEM   pString  = _param( 2, IT_STRING );
   PSYMBOL pMessage = GetDynSym( pString->value.szText )->pSymbol;
   PDYNSYM pMsg     = ( PDYNSYM ) pMessage->pDynSym;
   PCLASS  pClass;

   WORD    wClass   = _parni( 1 );
   WORD    wAt;
   WORD    wLimit;

   HARBOURFUNC pFunc;

   if( wClass && wClass <= wClasses )
   {
      pClass = &pClasses[ wClass - 1 ];
      wAt    = ( ( ( unsigned ) pMsg ) % pClass->wHashKey ) * BUCKET;
      wLimit = pClass->wHashKey * BUCKET;

      while( ( wAt < wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMsg ) ) )
         wAt++;

      if( wAt < wLimit )
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


HARBOUR __INSTSUPER( void )             /* ClassH := __InstSuper( <cName> ) */
{                                       /* Instance super class and return  */
                                        /* class handle                     */
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



