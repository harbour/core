/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SETKEY() and related functions
 *
 * Copyright 1999 A White <awhite@user.rose.com>
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
  Either way you have to clean up the memory on exit. The best way to
  do this is to add a hb_setkeyInit() and hb_setkeyExit() function
  and call them from CONSOLE.C Init/Exit functions.
*/

#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#define SK_NO_RETURN FALSE

#define SETKEY_ERRORS

typedef struct SetKey_S
{
  SHORT keycode;
  PHB_ITEM Action;
#if defined( HB_EXTENSION )
  PHB_ITEM IsActive;
#endif
  struct SetKey_S *next;
} SetKey_T, *pSetKey_T;

pSetKey_T sk_list;

#if defined( SETKEY_ERRORS )

#define SK_ERR_NOARGS          3101
#define SK_ERR_ARGTYPE_KEY     3102
#define SK_ERR_ARGTYPE_KEYN    3103
#define SK_ERR_ARGTYPE_ACTION  3104
#define SK_ERR_ARGTYPE_ACTIVE  3105
#define SK_ERR_ARGTYPE_SKSAVE  3106
#define SK_ERR_ACTIVE_RESULTS  3107

static void sk_error( ULONG ulSubCode )
{
  PHB_ITEM pResult;
  char *szDescription;

  switch ( ulSubCode ) {
  case SK_ERR_NOARGS        : szDescription = "Missing parameter(s)"; break;
  case SK_ERR_ARGTYPE_KEY   : szDescription = "anKey non-numeric"; break;
  case SK_ERR_ARGTYPE_KEYN  : szDescription = "anKey non-numeric and non-array of numbers"; break;
  case SK_ERR_ARGTYPE_ACTION: szDescription = "bAction is not a code-block"; break;
  case SK_ERR_ARGTYPE_ACTIVE: szDescription = "bCondition is not a code-block"; break;
  case SK_ERR_ARGTYPE_SKSAVE: szDescription = "OldKeys must be return value from previous hb_SetKeySave(), or NIL"; break;
  case SK_ERR_ACTIVE_RESULTS: szDescription = "bCondition returns non-logical"; break;
  }

  pResult = hb_errRT_BASE_Subst( EG_ARG, ulSubCode, szDescription, "SETKEY" );
  if ( pResult )
  {
    hb_itemReturn( pResult );
    hb_itemRelease( pResult );
  }
}

#else

#define sk_error( n )

#endif

void hb_setkeyInit( void )
{
  sk_list = NULL;
}

void hb_setkeyExit( void )
{
  pSetKey_T sk_list_tmp;

  while ( sk_list != NULL )
  {
    hb_itemRelease( sk_list->Action );

#if defined( HB_EXTENSION )
    if ( sk_list->IsActive )
      hb_itemRelease( sk_list->IsActive );
#endif

    sk_list_tmp = sk_list->next;
    hb_xfree( ( void * )sk_list );
    sk_list = sk_list_tmp;
  }
  sk_list = NULL;
}


static pSetKey_T sk_findkey( SHORT key, pSetKey_T *sk_list_end )
{
  pSetKey_T sk_list_tmp;

  *sk_list_end = NULL;
  for ( sk_list_tmp = sk_list;
        sk_list_tmp && sk_list_tmp->keycode != key;
        sk_list_tmp = sk_list_tmp->next )
  {
    *sk_list_end = sk_list_tmp;
  }

  return sk_list_tmp;
}

static void sk_get( void )
{
  pSetKey_T sk_list_tmp, sk_list_end;
  PHB_ITEM pKeyCode = hb_param( 1, HB_IT_NUMERIC );

  if ( pKeyCode )
  {
    /* sk_list_end is not used in this context */
    sk_list_tmp = sk_findkey( hb_itemGetNI( pKeyCode ), &sk_list_end );
    if ( sk_list_tmp )
    {
#if defined( HB_EXTENSION )
      PHB_ITEM pIsActiveResults;

      if ( sk_list_tmp->IsActive != NULL )
      {
        pIsActiveResults = hb_vmEvalBlockV( sk_list_tmp->IsActive, 1, pKeyCode );
        if ( !HB_IS_LOGICAL( pIsActiveResults ) )
          sk_error( SK_ERR_ACTIVE_RESULTS );
      }
      else
        pIsActiveResults = NULL;

      if ( pIsActiveResults == NULL ||
           hb_itemGetL( pIsActiveResults ) )
      {
#endif
PHB_ITEM pAction = hb_itemNew( sk_list_tmp->Action );
        hb_itemReturn( pAction );
        hb_itemRelease( pAction );

#if defined( HB_EXTENSION )
      }
      else
        hb_ret();
#endif
    }
  }
  else
  {
    hb_ret();
    sk_error( SK_ERR_ARGTYPE_KEYN );
  }
}


#if defined( HB_EXTENSION )
  #define sk_addkey( r, k, a, i ) sk_add( r, k, a, i )
#else
  #define sk_addkey( r, k, a, i ) sk_add( r, k, a )
#endif

static void sk_add( BOOL bReturn, SHORT nkeycode, PHB_ITEM pAction, PHB_ITEM pIsActive )
{
  pSetKey_T sk_list_tmp, sk_list_end;

  sk_list_tmp = sk_findkey( nkeycode, &sk_list_end );
  if ( sk_list_tmp == NULL )
  {
    if ( bReturn )
      hb_ret(); /* return a NIL */

    /* if the action param is nil, and nothing found, no need to do anything */
    if ( HB_IS_NIL( pAction ) )
      return;

    sk_list_tmp = ( pSetKey_T )hb_xgrab( sizeof( SetKey_T ) );
    sk_list_tmp->next = NULL;
    sk_list_tmp->keycode = nkeycode;

    if ( sk_list_end == NULL )
      sk_list = sk_list_tmp;
    else
      sk_list_end->next = sk_list_tmp;
  }
  else
  {
    if ( bReturn )
      hb_itemReturn( sk_list_tmp->Action );
    hb_itemRelease( sk_list_tmp->Action );

#if defined( HB_EXTENSION )
    if ( sk_list_tmp->IsActive )
      hb_itemRelease( sk_list_tmp->IsActive );
#endif

    if ( HB_IS_NIL( pAction ) )
    {
      /* if this is true, then the key found is the first key in the list */
      if ( sk_list_end == NULL )
      {
        sk_list_tmp = sk_list->next;
        hb_xfree( sk_list );
        sk_list = sk_list_tmp;
      }
      else
      {
        sk_list_end->next = sk_list_tmp->next;
        hb_xfree( sk_list_tmp );
      }
      return;
    }
  }

  sk_list_tmp->Action = hb_itemNew( pAction );

#if defined( HB_EXTENSION )
  if ( pIsActive )
    sk_list_tmp->IsActive = hb_itemNew( pIsActive );
  else
    sk_list_tmp->IsActive = NULL;
#endif
}

static void sk_set( void )
{
  PHB_ITEM pKeyCode = hb_param( 1, HB_IT_ANY );
  PHB_ITEM pAction  = hb_param( 2, HB_IT_ANY );
#if defined( HB_EXTENSION )
  PHB_ITEM pIsActive = hb_param( 3, HB_IT_BLOCK );
#endif
  int nitem, nupper, nlower;
  SHORT nkeycode;

  if ( !HB_IS_NUMERIC( pKeyCode ) && !HB_IS_ARRAY( pKeyCode ) )
  {
    sk_error( SK_ERR_ARGTYPE_KEY );
    return;
  }

  if ( !( HB_IS_NIL( pAction ) || HB_IS_BLOCK( pAction ) ) )
  {
    sk_error( SK_ERR_ARGTYPE_ACTION );
    return;
  }

#if defined( HB_EXTENSION )
  if ( hb_pcount() >= 3 && pIsActive == NULL )
  {
    sk_error( SK_ERR_ARGTYPE_ACTIVE );
    return;
  }
#endif

  if ( HB_IS_ARRAY( pKeyCode ) )
  {
    nlower = 1;
    nupper = hb_arrayLen( pKeyCode );
  }
  else
    nlower = nupper = 0;

  /* use a loop to assign hot-keys - this will execute
     only once if anKey [pKeyCode] is a number */

  for ( nitem = nlower; nitem <= nupper; nitem++ )
  {
    if ( HB_IS_ARRAY( pKeyCode ) )
      nkeycode = hb_itemGetNI( hb_arrayGetItemPtr( pKeyCode, nitem ) );
    else
      nkeycode = hb_itemGetNI( pKeyCode );

    sk_addkey( ( BOOL )( nitem == nupper ), nkeycode, pAction, pIsActive );
  }
}

HB_FUNC( SETKEY )
{
  switch( hb_pcount() ) {
  case 0:
    sk_error( SK_ERR_NOARGS );
    break;

  case 1:
    sk_get();
    break;

  default:
    sk_set();
    break;
  }
}

#if defined( HB_EXTENSION )

HB_FUNC( HB_SETKEYGET )
{
  pSetKey_T sk_list_tmp, sk_list_end;
  PHB_ITEM pKeyCode  = hb_param( 1, HB_IT_NUMERIC );
  PHB_ITEM pIsActive = hb_param( 2, HB_IT_ANY );//HB_IT_BYREF );

  if ( pKeyCode )
  {
    /* sk_list_end is not used in this context */
    sk_list_tmp = sk_findkey( hb_itemGetNI( pKeyCode ), &sk_list_end );
    if ( sk_list_tmp )
    {
      hb_itemReturn( sk_list_tmp->Action );
      if ( pIsActive )
        hb_itemCopy( pIsActive, sk_list_tmp->IsActive );
    }
  }
  else
  {
    hb_ret();
    sk_error( SK_ERR_ARGTYPE_KEYN );
  }
}

HB_FUNC( HB_SETKEYSAVE )
{
  PHB_ITEM pKeys, pParam;
  pSetKey_T sk_list_tmp;
  ULONG itemcount, nitem;

  /* build an multi-dimensional array from existing hot-keys, and return it */

  /* count the number of items in the list */
  for ( itemcount = 0, sk_list_tmp = sk_list;
        sk_list_tmp;
        itemcount++, sk_list_tmp = sk_list_tmp->next )
    ;

  pKeys = hb_itemArrayNew( itemcount );

  for ( nitem = 1, sk_list_tmp = sk_list;
        nitem <= itemcount;
        nitem++, sk_list_tmp = sk_list_tmp->next )
  {
    PHB_ITEM pKeyElements, pTmp;

    pKeyElements = hb_itemArrayNew( 3 );

    pTmp = hb_itemPutNI( NULL, sk_list_tmp->keycode );
    hb_itemArrayPut( pKeyElements, 1, pTmp );
    hb_itemRelease( pTmp );

    pTmp = hb_itemNew( sk_list_tmp->Action );
    hb_itemArrayPut( pKeyElements, 2, pTmp );
    hb_itemRelease( pTmp );

    if ( sk_list_tmp->IsActive )
    {
      pTmp = hb_itemNew( sk_list_tmp->IsActive );
      hb_itemArrayPut( pKeyElements, 3, pTmp );
      hb_itemRelease( pTmp );
    }

    hb_itemArrayPut( pKeys, nitem, pKeyElements );
    hb_itemRelease( pKeyElements );
  }

  hb_itemReturn( pKeys );
  hb_itemRelease( pKeys );

  pParam = hb_param( 1, HB_IT_ANY );
  if ( pParam != NULL )
  {
    hb_setkeyExit(); /* destroy the internal list */

    if ( HB_IS_ARRAY( pParam ) )
    {
      itemcount = hb_arrayLen( pParam );

      for ( nitem = 1; nitem <= itemcount; nitem++ )
      {
        PHB_ITEM itmKeyElements, itmKeyCode, itmAction, itmIsActive;

        itmKeyElements = hb_arrayGetItemPtr( pParam, nitem );

        itmKeyCode =     hb_arrayGetItemPtr( itmKeyElements, 1 );
        itmAction =      hb_arrayGetItemPtr( itmKeyElements, 2 );
        itmIsActive =    hb_arrayGetItemPtr( itmKeyElements, 3 );

        sk_addkey( SK_NO_RETURN, ( SHORT )hb_itemGetNI( itmKeyCode ), itmAction, itmIsActive );
      }
    }
    else if ( !HB_IS_NIL( pParam ) )
      sk_error( SK_ERR_ARGTYPE_SKSAVE );
  }
}

HB_FUNC( HB_SETKEYCHECK )
{
  pSetKey_T sk_list_tmp, sk_list_end;
  PHB_ITEM pKeyCode = hb_param( 1, HB_IT_NUMERIC ), pIsActiveResults;
  BOOL IsKeySet = FALSE;

  if ( pKeyCode )
  {
    /* sk_list_end is not used in this context */
    sk_list_tmp = sk_findkey( hb_itemGetNI( pKeyCode ), &sk_list_end );
    if ( sk_list_tmp )
    {
      if ( sk_list_tmp->IsActive == NULL )
        IsKeySet = TRUE;
      else
      {
        pIsActiveResults = hb_vmEvalBlockV( sk_list_tmp->IsActive, 1, pKeyCode );
        if ( HB_IS_LOGICAL( pIsActiveResults ) )
          IsKeySet = hb_itemGetL( pIsActiveResults );
        else
          sk_error( SK_ERR_ACTIVE_RESULTS );
      }
    }
  }
  else
    sk_error( SK_ERR_ARGTYPE_KEYN );

  if ( IsKeySet )
  {
    PHB_ITEM p2 = hb_param( 2, HB_IT_ANY );
    PHB_ITEM p3 = hb_param( 3, HB_IT_ANY );
    PHB_ITEM p4 = hb_param( 4, HB_IT_ANY );
//    PHB_ITEM pAction = hb_itemNew( sk_list_tmp->Action );

    switch( hb_pcount() ) {
    case 1:  hb_vmEvalBlockV( sk_list_tmp->Action /*pAction*/, 1, pKeyCode ); break;
    case 2:  hb_vmEvalBlockV( sk_list_tmp->Action /*pAction*/, 2, p2, pKeyCode ); break;
    case 3:  hb_vmEvalBlockV( sk_list_tmp->Action /*pAction*/, 3, p2, p3, pKeyCode ); break;
    default: hb_vmEvalBlockV( sk_list_tmp->Action /*pAction*/, 4, p2, p3, p4, pKeyCode ); break;
    }
//    hb_itemRelease( pAction );
  }

  hb_retl( IsKeySet );
}

#endif
