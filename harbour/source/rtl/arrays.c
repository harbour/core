/*
 * $Id$
 */

#include <itemapi.h>
#include <extend.h>
#include <errorapi.h>
#include <ctoharb.h>
#include <init.h>

HARBOUR HB_AADD(void);
HARBOUR HB_ACLONE(void);
HARBOUR HB_ACOPY(void);
HARBOUR HB_ADEL(void);
HARBOUR HB_AEVAL(void);
HARBOUR HB_AFILL(void);
HARBOUR HB_AINS(void);
HARBOUR HB_ARRAY(void);
HARBOUR HB_ASCAN(void);
HARBOUR HB_ASIZE(void);
HARBOUR HB_ASORT(void);
HARBOUR HB_ATAIL(void);

HB_INIT_SYMBOLS_BEGIN( Arrays__InitSymbols )
{ "ACLONE",         FS_PUBLIC, HB_ACLONE        , 0 },
{ "ACOPY",          FS_PUBLIC, HB_ACOPY         , 0 },
{ "ADEL",           FS_PUBLIC, HB_ADEL          , 0 },
{ "AEVAL",          FS_PUBLIC, HB_AEVAL         , 0 },
{ "AFILL",          FS_PUBLIC, HB_AFILL         , 0 },
{ "AINS",           FS_PUBLIC, HB_AINS          , 0 },
{ "ARRAY",          FS_PUBLIC, HB_ARRAY         , 0 },
{ "ASCAN",          FS_PUBLIC, HB_ASCAN         , 0 },
{ "ASIZE",          FS_PUBLIC, HB_ASIZE         , 0 },
{ "ASORT",          FS_PUBLIC, HB_ASORT         , 0 },
{ "ATAIL",          FS_PUBLIC, HB_ATAIL         , 0 },
HB_INIT_SYMBOLS_END( Arrays__InitSymbols );
#pragma startup Arrays__InitSymbols

PHB_ITEM hb_itemNew( PHB_ITEM );
PHB_ITEM hb_itemArrayPut( PHB_ITEM , ULONG , PHB_ITEM );

extern STACK  stack;
extern SYMBOL symEval;

static char *szBoundError    = "Bound error: Array access";
static char *szArgumentError = "Argument error: incorrect type";


/*
 * Internal
 */

void hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ) /* creates a new array */
{
   PBASEARRAY pBaseArray = ( PBASEARRAY ) hb_xgrab( sizeof( BASEARRAY ) );
   ULONG ul;

   ItemRelease( pItem );

   pItem->type = IT_ARRAY;

   if( ulLen )
          pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * ulLen );
   else
          pBaseArray->pItems = 0;

   pBaseArray->ulLen    = ulLen;
   pBaseArray->wHolders = 1;
   pBaseArray->wClass   = 0;
   pBaseArray->wSuperCast = FALSE;

   for( ul = 0; ul < ulLen; ul++ )
     ( pBaseArray->pItems + ul )->type = IT_NIL;

   pItem->item.asArray.value = pBaseArray;
}

void hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pValue )
{
   PBASEARRAY pBaseArray = ( PBASEARRAY ) pArray->item.asArray.value;
   hb_arraySize( pArray, pBaseArray->ulLen + 1 );
   pBaseArray = ( PBASEARRAY ) pArray->item.asArray.value;
   ItemCopy( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );
}

void hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
  if( IS_ARRAY( pArray ) )
    {
      if( ulIndex <= ( unsigned )hb_arrayLen( pArray ) )
        ItemCopy( pItem, pArray->item.asArray.value->pItems + ( ulIndex - 1 ) );
      else
        {
          hb_errorRT_BASE(EG_ARG, 1132, "Bound error", "array access");
        }
    }
  else
    {
      hb_errorRT_BASE(EG_ARG, 1068, "Argument error", "array access");
    }
}

char *hb_arrayGetString( PHB_ITEM pArray, ULONG ulIndex )
{
  if( IS_ARRAY( pArray ) )
    {
      if( ulIndex <= ( unsigned )hb_arrayLen( pArray ) )
        {
          PHB_ITEM pItem = pArray->item.asArray.value->pItems + ulIndex - 1;

          if( IS_STRING( pItem ) )
            return pItem->item.asString.value;
          else
            return "";
        }
      else
        {
          hb_errorRT_BASE(EG_ARG, 1132, "Bound error", "array access");
        }
    }
  else
    {
      hb_errorRT_BASE(EG_ARG, 1068, "Argument error", "array access");
    }
  return "";
}

ULONG hb_arrayGetStringLen( PHB_ITEM pArray, ULONG ulIndex )
{
  if( IS_ARRAY( pArray ) )
    {
      if( ulIndex <= ( unsigned )hb_arrayLen( pArray ) )
        {
          PHB_ITEM pItem = pArray->item.asArray.value->pItems + ulIndex - 1;

          if( IS_STRING( pItem ) )
            return pItem->item.asString.length;
          else
            return 0;
        }
      else
        {
          hb_errorRT_BASE(EG_ARG, 1132, "Bound error", "array access");
        }
    }
  else
    {
      hb_errorRT_BASE(EG_ARG, 1068, "Argument error", "array access");
    }
  return 0;
}

int hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex )
{
  if( IS_ARRAY( pArray ) )
    {
      if( ulIndex <= ( unsigned ) hb_arrayLen( pArray ) )
        {
          PHB_ITEM pItem = pArray->item.asArray.value->pItems + ulIndex - 1;
          return pItem->type;
        }
      else
        {
          hb_errorRT_BASE(EG_ARG, 1132, "Bound error", "array access");
        }
    }
  else
    {
      hb_errorRT_BASE(EG_ARG, 1068, "Argument error", "array access");
    }
  return 0;
}

void hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )
{
  if ( pArray->item.asArray.value->ulLen )
    ItemCopy( pResult, pArray->item.asArray.value->pItems +
                          ( pArray->item.asArray.value->ulLen - 1 ) );
  else
    ItemRelease( pResult );
}

ULONG hb_arrayLen( PHB_ITEM pArray )
{
  if( IS_ARRAY( pArray ) )
    return pArray->item.asArray.value->ulLen;
  else
    {
      hb_errorRT_BASE(EG_ARG, 1068, "Argument error", "array access");
    }
  return 0;
}

void hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
  if( IS_ARRAY( pArray ) )
    {
      if( ulIndex <= ( unsigned )hb_arrayLen( pArray ) )
        ItemCopy( pArray->item.asArray.value->pItems + ( ulIndex - 1 ), pItem );
      else
        {
          hb_errorRT_BASE(EG_ARG, 1133, "Bound error", "array assign");
        }
    }
  else
    {
      hb_errorRT_BASE(EG_ARG, 1069, "Argument error", "array assign");
    }
}

void hb_arraySize( PHB_ITEM pArray, ULONG ulLen )
{
  if ( IS_ARRAY( pArray ) )
    {
      PBASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG      ul;

      if( ! pBaseArray->ulLen )
        {
          pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( ulLen * sizeof( HB_ITEM ) );
          for ( ul = 0; ul < ulLen; ul ++ )
            ( pBaseArray->pItems + ul )->type = IT_NIL;
        }
      else
        {
          if( pBaseArray->ulLen < ulLen )
            {
              pBaseArray->pItems = ( PHB_ITEM )hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulLen );

              /* set value for new items */
              for( ul = pBaseArray->ulLen; ul < ulLen; ul++ )
                ( pBaseArray->pItems + ul )->type = IT_NIL;
            }
          else if( pBaseArray->ulLen > ulLen )
            {
              /* release old items */
              for( ul = ulLen; ul < pBaseArray->ulLen; ul++ )
                ItemRelease( pBaseArray->pItems + ul );

              pBaseArray->pItems = ( PHB_ITEM )hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulLen );
            }
        }
      pBaseArray->ulLen = ulLen;
    }
}

void hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG ulStart, ULONG ulCount )
{
  if ( IS_ARRAY( pArray ) )
    {
      PBASEARRAY pBaseArray;
      ULONG      ulLen = hb_arrayLen( pArray );

      if ( ulStart == 0 )                         /* if parameter is missing */
        ulStart = 1;

      if ( ulCount == 0 )                         /* if parameter is missing */
        ulCount = ulLen - ulStart + 1;

      if ( ulStart + ulCount > ulLen )            /* check range */
        ulCount = ulLen - ulStart + 1;

      pBaseArray = pArray->item.asArray.value;

      for ( ; ulCount > 0; ulCount --, ulStart ++ )     /* set value items */
        ItemCopy( pBaseArray->pItems + ( ulStart - 1 ), pValue );
    }
  else
    {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription( pError, szArgumentError );
      hb_errLaunch( pError );
      hb_errRelease( pError );
    }
}

void hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex )
{
  if ( IS_ARRAY( pArray ) )
    {
      ULONG ulLen = hb_arrayLen( pArray );

      if ( ulIndex > 0 && ulIndex <= ulLen )
        {
          PBASEARRAY pBaseArray = pArray->item.asArray.value;

          ItemRelease( pBaseArray->pItems + ( ulIndex - 1 ) );

          for ( ulIndex --; ulIndex < ulLen; ulIndex ++ )       /* move items */
            ItemCopy( pBaseArray->pItems + ulIndex, pBaseArray->pItems + ( ulIndex + 1 ) );

          ItemRelease( pBaseArray->pItems + ( ulLen - 1 ) );
        }
      else
        {
          PHB_ITEM pError = hb_errNew();
          hb_errPutDescription( pError, szBoundError );
          hb_errLaunch( pError );
          hb_errRelease( pError );
        }
    }
  else
    {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription( pError, szArgumentError );
      hb_errLaunch( pError );
      hb_errRelease( pError );
    }
}

void hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex )
{
  if ( IS_ARRAY( pArray ) )
    {
      ULONG ulLen = hb_arrayLen( pArray );

      if ( ulIndex > 0 && ulIndex <= ulLen )
        {
          PBASEARRAY pBaseArray = pArray->item.asArray.value;

          ItemRelease( pBaseArray->pItems + ( ulLen - 1 ) );

          for ( ulLen --; ulLen >= ulIndex; ulLen -- )          /* move items */
            ItemCopy( pBaseArray->pItems + ulLen, pBaseArray->pItems + ( ulLen - 1 ) );

          ItemRelease( pBaseArray->pItems + ulLen );
        }
      else
        {
          PHB_ITEM pError = hb_errNew();
          hb_errPutDescription( pError, szBoundError );
          hb_errLaunch( pError );
          hb_errRelease( pError );
        }
    }
  else
    {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription( pError, szArgumentError );
      hb_errLaunch( pError );
      hb_errRelease( pError );
    }
}

int hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG ulStart, ULONG ulCount )
{
  if ( IS_ARRAY( pArray ) && pValue->type != IT_NIL )
    {
      int        iRet = 0;
      PBASEARRAY pBaseArray;
      ULONG      ulLen = hb_arrayLen( pArray );

      if ( ulStart == 0 )                      /* if parameter is missing */
        ulStart = 1;

      if ( ulCount == 0 )                      /* if parameter is missing */
        ulCount = ulLen - ulStart + 1;

      if ( ulStart + ulCount > ulLen )         /* check range */
        ulCount = ulLen - ulStart + 1;

      pBaseArray = pArray->item.asArray.value;

      for ( ulStart --; ulCount > 0; ulCount --, ulStart ++ )
        {
          PHB_ITEM pItem = pBaseArray->pItems + ulStart;

          if ( pValue->type == IT_BLOCK )
            {
              PushSymbol( &symEval );
              Push( pValue );
              Push( pItem );
              Do( 1 );
              if ( stack.Return.item.asLogical.value )
                iRet = 1;
            }
          else
            {
              if ( pValue->type == pItem->type )
                {
                  switch( pItem->type )
                    {
                    case IT_INTEGER :
                      iRet = ( pValue->item.asInteger.value == pItem->item.asInteger.value);
                      break;

                    case IT_LONG :
                      iRet = ( pValue->item.asLong.value == pItem->item.asLong.value );
                      break;

                    case IT_DOUBLE :
                      iRet = ( pValue->item.asDouble.value == pItem->item.asDouble.value );
                      break;

                    case IT_DATE :
                      iRet = ( pValue->item.asDate.value == pItem->item.asDouble.value );
                      break;

                    case IT_LOGICAL :
                      iRet = ( pValue->item.asLogical.value == pItem->item.asLogical.value );
                      break;

                    case IT_STRING :
                      iRet = ( hb_itemStrCmp( pValue, pItem, FALSE ) == 0 );
                      break;
                    }
                }
            }
          if ( iRet )
            return ulStart + 1;                  /* arrays start from 1 */
        }
    }
  else
    {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription( pError, szArgumentError );
      hb_errLaunch( pError );
      hb_errRelease( pError );
    }
  return 0;
}

void hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG ulStart, ULONG ulCount )
{
  if ( IS_ARRAY( pArray ) && IS_BLOCK( bBlock ) )
    {
      PBASEARRAY pBaseArray;
      ULONG      ulLen = hb_arrayLen( pArray );

      if ( ulStart == 0 )                         /* if parameter is missing */
        ulStart = 1;

      if ( ulCount == 0 )                         /* if parameter is missing */
        ulCount = ulLen - ulStart + 1;

      if ( ulStart + ulCount > ulLen )            /* check range */
        ulCount = ulLen - ulStart + 1;

      pBaseArray = pArray->item.asArray.value;

      for ( ulStart --; ulCount > 0; ulCount --, ulStart ++ )
        {
          PHB_ITEM pItem = pBaseArray->pItems + ulStart;

          PushSymbol( &symEval );
          Push( bBlock );
          Push( pItem );
          Do( 1 );
        }
    }
  else
    {
      hb_errorRT_BASE(EG_ARG, 2017, "Argument error", "AEVAL");
    }
}

void hb_arrayRelease( PHB_ITEM pArray )
{
  if ( IS_ARRAY( pArray ) )
    {
      ULONG      ul, ulLen  = hb_arrayLen( pArray );
      PBASEARRAY pBaseArray = pArray->item.asArray.value;

      if( !pBaseArray->wSuperCast )
      {
         for ( ul = 0; ul < ulLen; ul ++ )
            ItemRelease( pBaseArray->pItems + ul );

         if( pBaseArray->pItems )
            hb_xfree( pBaseArray->pItems );
      }
      hb_xfree( pBaseArray );

      pArray->type = IT_NIL;
      pArray->item.asArray.value = NULL;
    }
  else
    {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription( pError, szArgumentError );
      hb_errLaunch( pError );
      hb_errRelease( pError );
    }
}

void hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG ulStart,
                                 ULONG ulCount, ULONG ulTarget )
{
  if ( IS_ARRAY( pSrcArray ) && IS_ARRAY( pDstArray ) )
    {
      PBASEARRAY pSrcBaseArray, pDstBaseArray;
      ULONG      ulSrcLen = hb_arrayLen( pSrcArray );
      ULONG      ulDstLen = hb_arrayLen( pDstArray );

      if ( ulStart == 0 )                         /* if parameter is missing */
        ulStart = 1;

      if ( ulTarget == 0 )                        /* if parameter is missing */
        ulTarget = 1;

      if ( ulCount == 0 )                         /* if parameter is missing */
        ulCount = ulSrcLen - ulStart + 1;

      if ( ulStart + ulCount > ulSrcLen )         /* check range */
        ulCount = ulSrcLen - ulStart + 1;

      if ( ulCount > ulDstLen )
        ulCount = ulDstLen;

      pSrcBaseArray = pSrcArray->item.asArray.value;
      pDstBaseArray = pDstArray->item.asArray.value;

      for ( ulTarget --, ulStart --; ulCount > 0; ulCount --, ulStart ++ )
        {
          ItemCopy( pDstBaseArray->pItems + ( ulTarget + ulStart ), pSrcBaseArray->pItems + ulStart );
        }
    }
  else
    {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription( pError, szArgumentError );
      hb_errLaunch( pError );
      hb_errRelease( pError );
    }
}

PHB_ITEM hb_arrayClone( PHB_ITEM pSrcArray )
{
   PHB_ITEM pDstArray = hb_itemNew( 0 );

   if ( IS_ARRAY( pSrcArray ) )
   {
      PBASEARRAY pSrcBaseArray, pDstBaseArray;
      ULONG      ulCount, ulSrcLen  = hb_arrayLen( pSrcArray );

      hb_arrayNew( pDstArray, ulSrcLen );

      pSrcBaseArray = pSrcArray->item.asArray.value;
      pDstBaseArray = pDstArray->item.asArray.value;
      pDstBaseArray->wClass = pSrcBaseArray->wClass;

      for ( ulCount = 0; ulCount < ulSrcLen; ulCount ++ )
      {
         PHB_ITEM pSrcItem = pSrcBaseArray->pItems + ulCount;

         if ( pSrcItem->type == IT_ARRAY )
         {
            PHB_ITEM pClone = hb_arrayClone( pSrcItem );

            hb_itemArrayPut( pDstArray, ulCount + 1, pClone );
            hb_itemRelease( pClone );
         }
         else
            hb_itemArrayPut( pDstArray, ulCount + 1, pSrcItem );
      }
      return pDstArray;
   }
   else
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription( pError, szArgumentError );
      hb_errLaunch( pError );
      hb_errRelease( pError );
   }
   return pDstArray;
}

/*
 * HARBOUR
 */
HARBOUR HB_ARRAY( void )
{
  hb_arrayNew( &stack.Return, hb_parnl( 1 ) );
}

HARBOUR HB_AADD( void )
{
  PHB_ITEM pArray = hb_param( 1, IT_ARRAY );
  PHB_ITEM pValue = hb_param( 2, IT_ANY );

  if ( pArray )
    hb_arrayAdd( pArray, pValue );

  ItemCopy( &stack.Return, pValue );
}

HARBOUR HB_ASIZE( void )
{
  PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

  if ( pArray )
    {
      hb_arraySize( pArray, hb_parnl( 2 ) );
      ItemCopy( &stack.Return, pArray );  /* ASize() returns the array itself */
    }
  else
    hb_ret();    /* QUESTION: Should we raise an error here ? */
}

HARBOUR HB_ATAIL( void )
{
  PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

  if ( pArray )
    hb_arrayLast( pArray, &stack.Return );
  else
    hb_ret();  /* QUESTION: Should we raise an error here ? */
}

HARBOUR HB_AINS( void )
{
  PHB_ITEM pArray  = hb_param( 1, IT_ARRAY );

  if ( pArray )
    {
      hb_arrayIns( pArray, hb_parnl( 2 ) );
      ItemCopy( &stack.Return, pArray );  /* AIns() returns the array itself */
    }
  else
    hb_ret();
}

HARBOUR HB_ADEL( void )
{
  PHB_ITEM pArray  = hb_param( 1, IT_ARRAY );

  if ( pArray )
    {
      hb_arrayDel( pArray, hb_parnl( 2 ) );
      ItemCopy( &stack.Return, pArray ); /* ADel() returns the array itself */
    }
  else
    hb_ret();
}

HARBOUR HB_AFILL( void )
{
  PHB_ITEM pArray  = hb_param( 1, IT_ARRAY );

  if ( pArray )
    {
      hb_arrayFill( pArray, hb_param( 2, IT_ANY ), hb_parnl( 3 ), hb_parnl( 4 ) );
      ItemCopy( &stack.Return, pArray ); /* AFill() returns the array itself */
    }
  else
    hb_ret();
}

HARBOUR HB_ASCAN( void )
{
  PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

  if ( pArray )
    hb_retnl( hb_arrayScan( pArray, hb_param( 2, IT_ANY ), hb_parnl( 3 ), hb_parnl( 4 ) ) );
  else
    hb_retnl( 0 );
}

HARBOUR HB_AEVAL( void )
{
  PHB_ITEM pArray = hb_param( 1, IT_ARRAY );
  PHB_ITEM bBlock = hb_param( 2, IT_BLOCK );

  if ( pArray )
    {
      hb_arrayEval( pArray, bBlock, hb_parnl( 3 ), hb_parnl( 4 ) );
      ItemCopy( &stack.Return, pArray ); /* AEval() returns the array itself */
    }
  else
    hb_ret();
}

HARBOUR HB_ACOPY( void )
{
  PHB_ITEM pSrcArray  = hb_param( 1, IT_ARRAY );
  PHB_ITEM pDstArray  = hb_param( 2, IT_ARRAY );

  if ( pSrcArray && pDstArray )
    {
      hb_arrayCopy( pSrcArray, pDstArray, hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl( 5 ) );
      ItemCopy( &stack.Return, pDstArray ); /* ACopy() returns the target array */
    }
  else
    hb_ret();
}

HARBOUR HB_ACLONE( void )
{
  PHB_ITEM pSrcArray  = hb_param( 1, IT_ARRAY );

  if ( pSrcArray )
    {
      PHB_ITEM pDstArray = hb_arrayClone( pSrcArray );
      ItemCopy( &stack.Return, pDstArray ); /* AClone() returns the new array */
      ItemRelease( pDstArray );
      hb_xfree( pDstArray );
    }
  else
    hb_ret();
}

