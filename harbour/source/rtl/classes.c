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

#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4

typedef struct
{
   void * pMessage;      /* pointer to dynamic symbol when they get ready */
   HARBOURFUNC pFunction;
   WORD    wData;
   WORD    wScope;
   WORD    wInitValue;
} METHOD, * PMETHOD;

typedef struct
{
   char *  szName;
   WORD    wDatas;
   PMETHOD pMethods;
   WORD    wMethods;
   WORD    wHashKey;
   ITEM    aClassDatas;  /* Array for ClassDatas */
   ITEM    aInlines;     /* Array for inline codeblocks */
   ITEM    aInitValues;  /* Array for Datas init values */
} CLASS, * PCLASS;

#define BUCKET 4

extern STACK stack;
extern SYMBOL symEval;

PCLASS  pClasses = 0;
WORD    wClasses = 0;
PMETHOD pMethod = 0;
PDYNSYM msgClassName = 0, msgClassH = 0, msgEval = 0, msgClassSel = 0;

HARBOUR CLASSCREATE() /* cClassName, nDatas --> hClass */
{
   if( ! pClasses )
      pClasses = ( PCLASS ) _xgrab( sizeof( CLASS ) );
   else
      pClasses = ( PCLASS ) _xrealloc( pClasses, sizeof( CLASS ) * ( wClasses + 1 ) );

   pClasses[ wClasses ].szName = ( char * ) _xgrab( _parclen( 1 ) + 1 );
   strcpy( pClasses[ wClasses ].szName, _parc( 1 ) );

   pClasses[ wClasses ].wDatas   = _parni( 2 );
   pClasses[ wClasses ].pMethods = ( PMETHOD ) _xgrab( 100 * sizeof( METHOD ) );
   pClasses[ wClasses ].wHashKey = 25; /* BUCKET = 4 repetitions */
   pClasses[ wClasses ].wMethods = 0;

   pClasses[ wClasses ].aClassDatas.wType = IT_NIL;
   pClasses[ wClasses ].aInlines.wType = IT_NIL;
   pClasses[ wClasses ].aInitValues.wType = IT_NIL;

   Array( &pClasses[ wClasses ].aClassDatas, 0 );
   Array( &pClasses[ wClasses ].aInlines,    0 );
   /* Array( &pClasses[ wClasses ].aInitValues,    0 ); */

   memset( pClasses[ wClasses ].pMethods, 0, 100 * sizeof( METHOD ) );

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
   ArrayGet( stack.pBase + 1, pMethod->wData, &stack.Return );
}

static HARBOUR SetData( void )
{
   ArraySet( stack.pBase + 1, pMethod->wData, stack.pBase + 2 );
}

static HARBOUR GetClassData( void )
{
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;

   if( wClass && wClass <= wClasses )
      ArrayGet( &pClasses[ wClass - 1 ].aClassDatas, pMethod->wData, &stack.Return );
}

static HARBOUR SetClassData( void )
{
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;

   if( wClass && wClass <= wClasses )
      ArraySet( &pClasses[ wClass - 1 ].aClassDatas, pMethod->wData, stack.pBase + 2 );
}

static HARBOUR EvalInline( void )
{
   ITEM block;
   WORD wClass = ( ( PBASEARRAY ) ( stack.pBase + 1 )->value.pBaseArray )->wClass;
   WORD w;

   ArrayGet( &pClasses[ wClass - 1 ].aInlines, pMethod->wData, &block );

   PushSymbol( &symEval );
   Push( &block );
   for( w = 1; w <= _pcount(); w++ )
      Push( _param( w, IT_ANY ) );
   Do( _pcount() );
}

static HARBOUR Virtual( void )
{
   _ret();
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
      ItemRelease( pReturn );
      _xfree( pReturn );
      pReturn = hb_itemArrayNew( pClass->wMethods );
                                                /* Create a transfer array  */
      for( wAt = 0; wAt < wLimit ; wAt++ )
      {
         pMessage = (PDYNSYM) pClass->pMethods[ wAt ].pMessage;
         if( pMessage )                         /* Hash Entry used ?        */
         {
            pItem  = hb_itemNew(  NULL );       /* Add to array             */
            pItem  = hb_itemPutC( pItem, pMessage->pSymbol->szName );
            hb_itemArrayPut( pReturn, ++wPos, pItem );
            ItemRelease( pItem );
            _xfree( pItem );
         }
      }
   }
   hb_itemReturn( pReturn );
   ItemRelease( pReturn );
   _xfree( pReturn );
}

HARBOUR CLASSADD() /* hClass, cMessage, pFunction, nType */
{
   WORD wClass = _parnl( 1 );
   WORD wType  = _parni( 4 );
   PCLASS pClass;
   PDYNSYM pMessage;
   WORD wAt, wLimit;

   if( wClass && wClass <= wClasses )
   {
      pClass   = &pClasses[ wClass - 1 ];
      pMessage = GetDynSym( _parc( 2 ) );
      wAt      = ( ( ( unsigned ) pMessage ) % pClass->wHashKey ) * BUCKET;
      wLimit   = wAt + BUCKET;

      if( pClass->wMethods > ( pClass->wHashKey * BUCKET * 2/3 ) )
         DictRealloc( pClass );

      while( ( wAt < wLimit ) &&
             ( pClass->pMethods[ wAt ].pMessage &&
             ( pClass->pMethods[ wAt ].pMessage != pMessage ) ) )
         wAt++;

      if( wAt <= wLimit )
      {
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
                    pClass->pMethods[ wAt ].pFunction = GetData;
                 break;

            case MET_CLASSDATA:
                 pClass->pMethods[ wAt ].wData = _parnl( 3 );
                 if( ArrayLen( &pClass->aClassDatas ) < _parnl( 3 ) )
                    ArraySize( &pClass->aClassDatas, _parnl( 3 ) );

                 if( pMessage->pSymbol->szName[ 0 ] == '_' )
                    pClass->pMethods[ wAt ].pFunction = SetClassData;
                 else
                    pClass->pMethods[ wAt ].pFunction = GetClassData;
                 break;

            case MET_INLINE:
                 pClass->pMethods[ wAt ].wData = ArrayLen( &pClass->aInlines ) + 1;
                 ArraySize( &pClass->aInlines, pClass->pMethods[ wAt ].wData );
                 ArraySet( &pClass->aInlines, pClass->pMethods[ wAt ].wData,
                           _param( 3, IT_BLOCK ) );
                 pClass->pMethods[ wAt ].pFunction = EvalInline;
                 break;

            case MET_VIRTUAL:
                 pClass->pMethods[ wAt ].pFunction = Virtual;
                 break;

            default:
                 printf( "Invalid method type from ClassAdd\n" );
                 exit( 1 );
                 break;
         }
         pClass->wMethods++;
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
   WORD wClass = _parni( 1 );

   if( wClass <= wClasses )
   {
      Array( &stack.Return, pClasses[ wClass - 1 ].wDatas );
      ( ( PBASEARRAY ) stack.Return.value.pBaseArray )->wClass = wClass;
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
      wLimit = wAt + BUCKET;

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

void ReleaseClass( PCLASS pClass )
{
   _xfree( pClass->szName );
   _xfree( pClass->pMethods );

   ArrayRelease( &pClass->aClassDatas );
   ArrayRelease( &pClass->aInlines );
   /* ArrayRelease( &pClass->aInitValues ); */
}

void ReleaseClasses( void )
{
   WORD w;

   for( w = 0; w < wClasses; w++ )
      ReleaseClass( pClasses + w );

   if( pClasses )
      _xfree( pClasses );
}


/* oSend send a message to an object */

HARBOUR OSEND()             /* <xRet> = oSend( <oObj>, <cSymbol>, <xArg,..> */
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
   {                                        /* TODO: Crash code             */
   }
}


