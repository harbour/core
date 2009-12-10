/*
  $Id$
*/

/*
 * Harbour Project source code:
 * HB_BTree source.
 *
 * Copyright 2002 April White <april@users.sourceforge.net>
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

/* Changelog

  * Changed, bla-bla
  ! Fixed
  % Optimized
  + Added
  - Removed
  ; Comment

see ChangeLog 2002-07-19 09:10 UTC+0500 April White <april@users.sourceforge.net>
see ChangeLog 2002-07-14 14:14 UTC+0500 April White <april@users.sourceforge.net>

    TODO: impliment ulFlags within hb_btreeopen() - see warning above
          - clear im-memory flag
          - get unique flag from file header

*/

/*
  History :
    - removed use of ftruncate() - wasn't reporting error when it failed

    04/27/02 - add 'buffers' as a parameter to main()
    07/11/01 - minor cosmetic changes
    07/08/01 - added flag IsMultiBuffers & set/clear it in ioBufferAlloc()
    07/07/01 - it works!
             - removed disabled code that refered to the tree->IsDirty
               flag, since it has been replaced by the ioBuffer flag
    07/07/01 - lots of cleanup of ioBufferScan() re. page reads when only
               a single buffer
             - amended Grow() to call ioBufferScan() w/ NULLPAGE
             - replaced all tree IsDirty flags w/ ioBuffer IsDirty flags
    07/04/01 - retry ioBufferAlloc() w/ 1 - insert seems to work
    07/03/01 - try ioBufferAlloc() with a value of 1
               - add's worked w/ no error, delete's failed w/o unique, but
                 the app stopped w/ unique - follow this!
                   - the successor() 'exit(0)' is hit
    06/21/01 - initial code on a link-list io buffer system
    06/16/01 - more coding of key-driven move first/last/next/previous
               - can this code be used for stack-driven movements?
    06/15/01 - based on compiling w/ VC++ at work, these amendments were done:
               * replace NULL with either 0 or a defined constant
               * fix macro var names in FOpen() group of macros
               * added FOpen() for non-gnu compilers
               * if cannot open output file, report that properly - it is
                 reporting an 'out of disk space' error on the long jump
               * replace F_OK in access() call with 0, or define it at the top;
                 check errno for error condition
               * replace FCreate() with:
                 pBTree->hFile      = FCreate( pBTree->szFileName, _A_NORMAL );
                 if ( pBTree->hFile == -1 )
                 {
                   BufferRelease( pBTree );
                   return NULL;
                 }
    06/14/01 - initial coding of key-driven move first/last/next/previous
    06/13/01 - backed up code and moved bBufferIsDirty to a flag in Flags
    06/09/01 - renamed Key_T to hb_KeyData_T throughout most of the code
    06/04/01 - moved NodeGet() & NodeSet() into NodeCopy()
             - add UNIQUE create flag, add to file & mem header, and
               recognize it inside KeyCompare()
    06/01/01 - moved sbRecordNotFound & sbDuplicateKey into the btree structure
             - wrapped sulMax* variables in conditional code
    05/31/01 - remove all usage of TmpKey
    05/30/01 - initial code to include a Data element to the BTree
               - need: UNIQUE parameter for BtreeNew
                     : amend KeyCompare to recoqnize and use the flag
    05/25/01 - initial code to add Data elements to the tree
               - do I want to have a conditional flag, so that space can be saved?
                 - no, since most btree files will being either on external
                   data or something like a spelling dictionary, which might
                   need flags
    05/15/01 - found a bug - Combine() was calling BranchSet() which was
               overwriting the first key - inside BtreeNew() I reduced the
               MaxKey value by 1 and it seems to work
               - test to see how much wasted space there is in the output file
                 - the problem wasn't so much MaxKey but MinKey - I increased
                   MaxKey by 1 and improved block usage without side effects
    05/09/01 - the last big change was to replace use of tree->TmpKey
               with locally allocated buffers, and adding a 'destination'
               pointer to KeyGet() worked once I increased the buffer sizes
               by 1, but the same bug with deleting 'acapatity' still exists,
               and I discovered that deleting 'aelem' creates an empty element
    05/06/01 - by replacing 'tree->ulThisPage' with 'tree->ulRootPage',
               I fixed the bug with deleting 'be', but another popped up
               - when 'range' is deleted, 'zero' is also deleted
                 - I think it was caused by a missing READPAGE_IF_NEEDED()
                   - to be tested - testing failed!
                   - the only pattern is that 'reference' is in the last
                     iPosition of its page, as is 'zero'
    05/05/01 - I have found a simple case [about 196 records] that causes the
               delete code to fail - a non-root page has too few records!
    05/04/01 - when allocating a buffer for writing to the file, zero fill it
    12/28/00 - replaced BufferAlloc() with macro to BufferRealloc()
             - removed PadToSize parameter from HeaderWrite() and moved
               the corresponding code to BTreeNew()
             - added macro FTell() and replaced tell()
             - removed the BTree prefix from several static functions

    09/09/00 - branchget requested a iPosition > page count (84 vs 83)
             - by placing a search before actual delete, problem solved
             - some keys are not deleted for different page sizes, and
               in infinite loop startes for small page sizes (256/512)
               - CountAdj set a page count to 0 - look into this
    09/08/00 - when storing a branch, see if it already exists in the current page
             - a branch > filesize was retrieved/created!
    09/04/00 - replacing binary search in SearchNode() removed the crash
               for 1024 but left 4096
             - there is a problem with the code with 1024 - there was no crash
               at 1024, but a page was corrupted - the Traverse() code was
               messed up and apparently recurisvely calling the same page
             - making 'MaximumKeys' 1 smaller (ie. - 1 - 1) still leaves a
               crash with 4096
    09/02/00 - still crashes
             - increased size of TmpKey and set last char to '\0', as well
               other temporary key holders
             - introduced 'DEADFOOD' as a buffer trailer & BTR as file page
               header - nothing
             - removed all calls to assert(), trying to fprintf() before
               any longjmp's
    09/01/00 - possible crash with some block size may be caused by not
               properly allocating size of buffer
    08/27/00 - had to reduce the max key by 1 because in some cases the
      code crashes (ie. tested with a block of 512 is okay, 1K fataly crashes)

  TODO:
    - determine how to handle page buffers in a multi-user environment
    - replace some of the for..next loops through the key/branches that
      move them around with memmove()
    - this may not be feasible - in a multi-user environ, the stack
      would become invalid; I would have to save the current key and then
      locate the next/prev as needed, locking the file for the duration
      (as well, page buffering would not be allowed because a page in
      memory would become invalid)
    - complete Move() API
      - when move right/left, follow any branch down
      - when hit end of a node, pop from stack
        - increment the position and follow branch down
    - implement code to handle big & little endian systems
*/

#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbinit.h"

#include "hb_btree.h"

HB_EXTERN_BEGIN

#if !defined( DEBUG ) && !defined( NDEBUG )
  #define NDEBUG
#else
#endif

#define PrintCRLF() hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) )

#if defined( __GNUC__ )

  #if 0
    #define STRINGIFY_2( n )  #n
    #define STRINGIFY_1( n )  STRINGIFY_2( n )
    #define SRCLINENO         __FUNCTION__ " (" STRINGIFY_1( __LINE__ ) ")"
    #define FILESRCLINENO     __FILE__ "." SRCLINENO
  #elif 0
    #define SRCLINENO  __FUNCTION__
  #else
    #define SRCLINENO  "%s (%d)", __FUNCTION__, __LINE__
  #endif

#else
/*
  #define STRINGIFY_2( n )  #n
  #define STRINGIFY_1( n )  STRINGIFY_2( n )
  #define SRCLINENO         ( __FILE__ " (" STRINGIFY_1( __LINE__ ) ")" )
*/
  #define SRCLINENO ""
#endif

#define HEADER_ID   ( BYTE * )"BTR\x10"
#define NULLPAGE    0L

#define BTREENODEISNULL( pBTree, node ) ( BOOL )( ( node ) == NULLPAGE )
#define READPAGE_IF_NEEDED( pBTree, node ) if ( node != pBTree->ioBuffer->ulPage )  ioBufferScan( pBTree, node )
#define CLEARKEYDATA( pBTree ) ( ( pBTree )->pThisKeyData->szKey[ 0 ] = '\0', \
                                 ( pBTree )->pThisKeyData->xData.lData = 0, \
                                 ( pBTree )->pThisKeyData->xData.pData = NULL )

#ifndef HB_BTREE_HEADERSIZE
#define HB_BTREE_HEADERSIZE  2048
#endif

/*
  #define MAXKEYS(m) ((m)-1)
  #define MINKEYS(m) (((m)+1)/2-1)
*/

typedef struct stack_item
{
  ULONG  ulNode;
  int    iPosition;
} BTreeStackItem;

typedef struct stack_tag
{
  USHORT usCount;
  BTreeStackItem items[ 1 ];
} BTreeStack;

#define STACKNODE( pStack )     ( *pStack )->items[ ( *pStack )->usCount - 1 ].ulNode
#define STACKPOSITION( pStack ) ( *pStack )->items[ ( *pStack )->usCount - 1 ].iPosition

typedef struct ioBuffer_tag
{
  struct ioBuffer_tag *prev, *next;
  ULONG ulPage;
  BOOL IsDirty;

  /* was: Buffer_T pBuffer; */
  ULONG * pulPageCount;
  ULONG * pulBranch;
  union {
    LONG  * plData;
    PHB_ITEM * ppData;
  } xData;
  BYTE  * szKey;
  BYTE    Buffer[ 1 ];
} ioBuffer_T;

typedef int hb_BTreeFlags_T;


#define IsNormal        0

/* btree file access flags */
#define IsReadOnly      HB_BTREE_READONLY
#define IsExclusive     HB_BTREE_EXCLUSIVE
#define IsShared        HB_BTREE_SHARED

/* btree control flags */
#define IsUnique        HB_BTREE_UNIQUE
#define IsCaseLess      HB_BTREE_CASELESS
#define IsInMemory      HB_BTREE_INMEMORY

/* internal flags */
#define IsRecordFound   ( 1 << 16 )
#define IsDuplicateKey  ( 1 << 17 )
#define IsMultiBuffers  ( 1 << 18 )
#define IsOptimized     ( 1 << 19 )

#define GETFLAG( pBTree, flag )   ( BOOL )( ( ( int ) ( pBTree )->ulFlags & ( flag ) ) == ( flag ) )
#define SETFLAG( pBTree, flag )   ( ( pBTree )->ulFlags |= ( flag ) )
#define RESETFLAG( pBTree, flag ) ( ( pBTree )->ulFlags &= ~( flag ) )

typedef struct hb_KeyData_Tag
{
  union {
    LONG lData;     /* lData is placed first for alignment, thought it is secondary */
    PHB_ITEM pData;
  } xData;
  BYTE szKey[ 1 ];
} hb_KeyData_T;

typedef int ( * BTreeCmpFunc )( const char * l, const char * r, size_t n );

struct hb_BTree
{
  char          * szFileName;
  HB_FHANDLE      hFile;
  ULONG           ulRootPage;
  ULONG           ulFreePage;
  USHORT          usPageSize;
  USHORT          usKeySize;
  USHORT          usMaxKeys;
  USHORT          usMinKeys;
  hb_BTreeFlags_T ulFlags;
  ULONG           ulKeyCount;
  hb_KeyData_T  * pThisKeyData;
  BTreeStack    * pStack;
  ioBuffer_T    * ioBuffer;
  void          * BufferEnd;
  BOOL            IsDirtyFlagAssignment;   /* replaces const TRUE, and !GETFLAG( pBTree, IsInMemory ) */

  BTreeCmpFunc    pStrCompare;

};

#if !defined( DEBUG ) && !defined( NDEBUG )
  BOOL IsDebugging = FALSE;
#else
  #define IsDebugging FALSE
#endif

enum hb_BTree_Error_Codes {
  HB_BTree_Unknown_EC,
  HB_BTreeArgError_EC,
  HB_BTree_TreeHandle_EC,
  HB_BTree_WriteError_EC,
  HB_BTree_StackSkip_EC,
};

static struct hb_BTree **s_BTree_List = NULL;
static int s_BTree_List_Count = 0;

/* forward declarations */

static USHORT CountGet( struct hb_BTree * pBTree, ULONG ulNode );
static hb_KeyData_T *KeyGet( struct hb_BTree * pBTree, ULONG ulNode, int iPosition, hb_KeyData_T *buffer );
static ULONG BranchGet( struct hb_BTree * pBTree, ULONG ulNode, int iPosition );

/* end of forward declarations */

static void hb_RaiseError( enum hb_BTree_Error_Codes ulSubCode, const char * szDescription, const char * szOperation, int uiArguments )
{
    PHB_ITEM pErr = hb_errRT_New(
    ES_ERROR        /* USHORT uiSeverity */,
    "HBBTREE"       /* char * szSubSystem */,
    EG_ARG          /* ULONG  ulGenCode */,
    ulSubCode       /* ULONG  ulSubCode */,
    ( char * )szDescription   /* char * szDescription */,
    ( char * )szOperation     /* char * szOperation */,
    0               /* USHORT uiOsCode */,
    EF_NONE         /* USHORT uiFlags */ );

  if ( uiArguments > 0 )
  {
    hb_errPutArgs( pErr, uiArguments, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ), hb_paramError( 5 ) );
  }

  hb_errLaunch( pErr );
  hb_errRelease( pErr );
}

static void * BufferRealloc( void *buffer, ULONG size )
{
  void *tmpBuffer = hb_xgrab( size );

  if ( buffer )
  {
    hb_xmemcpy( tmpBuffer, buffer, size );
    hb_xfree( buffer );
  }
/*  else
    hb_xmemset( tmpBuffer, '\0', size );*/

  return tmpBuffer;
}

#define BufferAlloc( n )    BufferRealloc( NULL, ( n ) )
#define BufferRelease( b )  hb_xfree( b )


static ioBuffer_T * ioOneBufferAlloc( struct hb_BTree * pBTree, ioBuffer_T * prev, ioBuffer_T * next )
{
  ioBuffer_T * thisptr;

  thisptr = ( ioBuffer_T * ) BufferAlloc( sizeof( ioBuffer_T ) + pBTree->usPageSize );
  hb_xmemset( thisptr, '\0', sizeof( ioBuffer_T ) + pBTree->usPageSize );

  thisptr->pulPageCount = ( ULONG * )( thisptr->Buffer );
  thisptr->pulBranch    = ( ULONG * )&thisptr->pulPageCount[ 1 ];
  if ( GETFLAG( pBTree, IsInMemory ) )
  {
    thisptr->xData.ppData = ( PHB_ITEM * )&thisptr->pulBranch[ pBTree->usMaxKeys + 1 ];
    thisptr->szKey        = ( BYTE * )&thisptr->xData.ppData[ pBTree->usMaxKeys ];
  }
  else
  {
    thisptr->xData.plData = ( LONG * )&thisptr->pulBranch[ pBTree->usMaxKeys + 1 ];
    thisptr->szKey        = ( BYTE * )&thisptr->xData.plData[ pBTree->usMaxKeys ];
  }

  thisptr->ulPage  = NULLPAGE;
  thisptr->IsDirty = FALSE;

  thisptr->prev = prev;
  thisptr->next = next;

  return thisptr;
}

/* a link list 'most recently access' page buffering system */
static void ioBufferAlloc( struct hb_BTree * pBTree, ULONG ulBuffers )
{
  ioBuffer_T *thisptr = NULL, *last = NULL;

  if ( ulBuffers == 0 )
    ulBuffers = 1;

  if ( ulBuffers > 1 || GETFLAG( pBTree, IsInMemory ) )
    SETFLAG( pBTree, IsMultiBuffers );
  else
    RESETFLAG( pBTree, IsMultiBuffers );

  while ( ulBuffers-- > 0 )
  {
    thisptr = ioOneBufferAlloc( pBTree, NULL, last );
    if ( last )  last->prev = thisptr;
    last = thisptr;
  }

  pBTree->ioBuffer = thisptr;
}

static void ioBufferWrite( struct hb_BTree * pBTree, ioBuffer_T *thisptr )
{
  hb_fsSeek( pBTree->hFile, thisptr->ulPage, FS_SET );
  if ( hb_fsWrite( pBTree->hFile, thisptr->Buffer, pBTree->usPageSize ) != pBTree->usPageSize )
  {
    hb_RaiseError( HB_BTree_WriteError_EC, "write error", "ioBufferWrite*", 0 );
  }
  thisptr->IsDirty = FALSE;
}

static void ioBufferRead( struct hb_BTree * pBTree, ULONG ulNode )
{
  ioBuffer_T *thisptr = pBTree->ioBuffer;

  if ( thisptr->IsDirty )
  {
    ioBufferWrite( pBTree, thisptr );
    thisptr->IsDirty = FALSE;
  }
  thisptr->ulPage = ulNode;
  hb_fsSeek( pBTree->hFile, thisptr->ulPage, FS_SET );
  hb_fsRead( pBTree->hFile, thisptr->Buffer, pBTree->usPageSize );
}

static void ioBufferRelease( struct hb_BTree * pBTree )
{
  ioBuffer_T *next, *thisptr;
  ULONG n;

  for ( thisptr = pBTree->ioBuffer; thisptr; thisptr = next )
  {
    next = thisptr->next;
    if ( thisptr->IsDirty ) /* cannot be in-memory in this case */
    {
      ioBufferWrite( pBTree, thisptr );
    }
    else if ( GETFLAG( pBTree, IsInMemory ) )
    {
      for ( n = 0; n < *thisptr->pulPageCount; n++ )
/*        { printf( "%p", thisptr->xData.ppData[ n ] ); PrintCRLF(); }*/
        hb_itemRelease( thisptr->xData.ppData[ n ] );
    }
    BufferRelease( thisptr );
  }
  pBTree->ioBuffer = NULL;
}

#if 0 /* keep just in case, but its functionality is built into ioBufferRelease */
  static void ioBufferFlush( struct hb_BTree * pBTree )
  {
    ioBuffer_T *thisptr;

    for ( thisptr = pBTree->ioBuffer; thisptr; thisptr = thisptr->next )
    {
      if ( thisptr->IsDirty )  ioBufferWrite( pBTree, thisptr );
    }
  }
#endif

static void ioBufferScan( struct hb_BTree * pBTree, ULONG page )
{
  ioBuffer_T *thisptr;
  ioBuffer_T *thisnext, *thisprev;

  /* locate either the buffer the page is in, the first empty buffer,
     or the last buffer in the list */
  for ( thisptr = pBTree->ioBuffer;
        thisptr && !BTREENODEISNULL( pBTree, thisptr->ulPage ) && thisptr->ulPage != page && thisptr->next;
        thisptr = thisptr->next )
    ;

  /* only shuffle the buffers if the target buffer is not the root buffer */
  if ( thisptr != pBTree->ioBuffer )
  {
    /* minimize writes */
    if ( thisptr->ulPage != page && thisptr->IsDirty )
      ioBufferWrite( pBTree, thisptr );

    thisprev = thisptr->prev;
    thisnext = thisptr->next;

    if ( thisnext )    thisnext->prev = thisprev;
    if ( thisprev )    thisprev->next = thisnext;

    thisptr->prev = NULL;
    thisptr->next = pBTree->ioBuffer;
    pBTree->ioBuffer->prev = thisptr;
    pBTree->ioBuffer = thisptr;
  }
  else if ( thisptr->IsDirty )
    ioBufferWrite( pBTree, thisptr );

  /* Grow() will call this with a null page, to flush & shuffle buffers */
  if ( !BTREENODEISNULL( pBTree, page ) && thisptr->ulPage != page && !GETFLAG( pBTree, IsInMemory ) )
    ioBufferRead( pBTree, page );
}

static void StackNew( BTreeStack **pStack )
{
  *pStack = ( BTreeStack * )BufferRealloc( *pStack, sizeof( **pStack ) );
  ( *pStack )->usCount = 0;
}

static void StackPush( BTreeStack **pStack, ULONG ulNode, int iPosition )
{
  *pStack = ( BTreeStack * )BufferRealloc( *pStack, sizeof( **pStack ) + ( ( *pStack )->usCount + 1 ) * sizeof( ( *pStack )->items[ 0 ] ) );
  ( *pStack )->items[ ( *pStack )->usCount ].ulNode    = ulNode;
  ( *pStack )->items[ ( *pStack )->usCount ].iPosition = iPosition;
  ( *pStack )->usCount++;
}

static void StackPop( BTreeStack **pStack, ULONG *pulNode, int *piPosition )
{
  if ( ( *pStack )->usCount )
  {
    ( *pStack )->usCount--;
    *pulNode    = ( *pStack )->items[ ( *pStack )->usCount ].ulNode;
    *piPosition = ( *pStack )->items[ ( *pStack )->usCount ].iPosition;
    *pStack = ( BTreeStack * )BufferRealloc( *pStack, sizeof( **pStack ) + ( ( *pStack )->usCount + 1 ) * sizeof( ( *pStack )->items[ 0 ] ) );
  }
}

static void StackPeek( BTreeStack **pStack, ULONG *pulNode, int *piPosition )
{
  if ( ( *pStack )->usCount )
  {
    *pulNode    = ( *pStack )->items[ ( *pStack )->usCount - 1 ].ulNode;
    *piPosition = ( *pStack )->items[ ( *pStack )->usCount - 1 ].iPosition;
  }
  else
  {
    *pulNode    = 0;
    *piPosition = 0;
  }
}

static LONG StackSkip( struct hb_BTree * pBTree, BTreeStack **pStack, LONG records )
{
  ULONG ulNode;
  int iPosition;
  LONG recordsskipped = 0;

  if ( ( *pStack )->usCount == 0 )
  {
    /* todo: raise an error? */
    hb_RaiseError( HB_BTree_StackSkip_EC, "internal stack skip error", "StackSkip*", 0 );
  }

  if ( records == 0 )
  {
    StackPeek( pStack, &ulNode, &iPosition );
  }
  else if ( records > 0 )
  {
    while ( records-- > 0 )
    {
      ulNode = BranchGet( pBTree, STACKNODE( pStack ), STACKPOSITION( pStack ) );
      if ( !BTREENODEISNULL( pBTree, ulNode ) )
      {
        StackPush( pStack, ulNode, STACKPOSITION( pStack ) );
        while ( !BTREENODEISNULL( pBTree, ( ulNode = BranchGet( pBTree, ulNode, 0 ) ) ) )
        {
/*          StackPush( pStack, BranchGet( pBTree, ulNode, 0 ), 0 );*/
          StackPush( pStack, ulNode, 0 );
        }
        STACKPOSITION( pStack ) = 1;
        recordsskipped++;
      }
      else if ( STACKPOSITION( pStack ) < CountGet( pBTree, STACKNODE( pStack ) ) )
      {
        STACKPOSITION( pStack )++;
        recordsskipped++;
      }
      else
      {
        StackPop( pStack, &ulNode, &iPosition );
        if ( ( *pStack )->usCount == 0 )
        {
          break;
        }
        else if ( STACKPOSITION( pStack ) < CountGet( pBTree, STACKNODE( pStack ) ) )
        {
          STACKPOSITION( pStack )++;
          recordsskipped++;
        }
        else
        {
          STACKPOSITION( pStack )++;
          records++; /* force the loop to retry */
        }
      }
    }
    StackPeek( pStack, &ulNode, &iPosition );
  }
  else /*if ( records < 0 )*/
  {
    while ( records++ < 0 )
    {
      if ( STACKPOSITION( pStack ) > 0 )
        ulNode = BranchGet( pBTree, STACKNODE( pStack ), --STACKPOSITION( pStack ) );
      else
        ulNode = NULLPAGE;
      if ( !BTREENODEISNULL( pBTree, ulNode ) )
      {
        StackPush( pStack, ulNode, CountGet( pBTree, ulNode ) );
        while ( !BTREENODEISNULL( pBTree, ( ulNode = BranchGet( pBTree, ulNode, CountGet( pBTree, ulNode ) ) ) ) )
        {
          StackPush( pStack, ulNode, CountGet( pBTree, ulNode ) );
        }
        recordsskipped--;
      }
      else if ( STACKPOSITION( pStack ) > 0 )
      {
        recordsskipped--;
      }
      else
      {
        StackPop( pStack, &ulNode, &iPosition );
        if ( ( *pStack )->usCount == 0 )
        {
          break;
        }
        else if ( STACKPOSITION( pStack ) > 0 )
        {
          recordsskipped--;
        }
        else
        {
          records--; /* force the loop to retry */
        }
      }
    }
    StackPeek( pStack, &ulNode, &iPosition );
  }

  if ( ulNode == 0 && iPosition == 0 )
    CLEARKEYDATA( pBTree );
  else
    KeyGet( pBTree, ulNode, iPosition, pBTree->pThisKeyData );

  return recordsskipped;
}

static void StackRelease( BTreeStack **pStack )
{
  if ( *pStack )
  {
    BufferRelease( *pStack );
    *pStack = NULL;
  }
}

static void HeaderWrite( struct hb_BTree * pBTree )
{
  /*
    header       [4 bytes]
    ulFreePage   [4 bytes, little endian]
    usPageSize   [2 bytes, little endian]
    usKeySize    [2 bytes, little endian]
    ulRootPage   [4 bytes, little endian]
    ulFlags      [4 bytes, little endian]
    ulKeyCount   [4 bytes, little endian]
  */

/* TODO: store this to a temp buffer and write the buffer */
/* TODO: write this info in a non-endian method */
  hb_fsSeek( pBTree->hFile, 0, FS_SET );

  if ( hb_fsWrite( pBTree->hFile, HEADER_ID, sizeof( HEADER_ID ) ) +
       hb_fsWrite( pBTree->hFile, ( const BYTE * )&pBTree->ulFreePage, sizeof( pBTree->ulFreePage ) ) + /* 4 bytes */
       hb_fsWrite( pBTree->hFile, ( const BYTE * )&pBTree->usPageSize, sizeof( pBTree->usPageSize ) ) + /* 2 bytes */
       hb_fsWrite( pBTree->hFile, ( const BYTE * )&pBTree->usKeySize , sizeof( pBTree->usKeySize  ) ) + /* 4 bytes */
       hb_fsWrite( pBTree->hFile, ( const BYTE * )&pBTree->ulRootPage, sizeof( pBTree->ulRootPage ) ) + /* 4 bytes */
       hb_fsWrite( pBTree->hFile, ( const BYTE * )&pBTree->ulFlags   , sizeof( pBTree->ulFlags    ) ) + /* 4 bytes */
       hb_fsWrite( pBTree->hFile, ( const BYTE * )&pBTree->ulKeyCount, sizeof( pBTree->ulKeyCount ) ) + /* 4 bytes */
       0 != sizeof( HEADER_ID ) +
            sizeof( pBTree->ulFreePage ) +
            sizeof( pBTree->usPageSize ) +
            sizeof( pBTree->usKeySize  ) +
            sizeof( pBTree->ulRootPage ) +
            sizeof( pBTree->ulFlags    ) +
            sizeof( pBTree->ulKeyCount ) +
            0 )
  {
    hb_RaiseError( HB_BTree_WriteError_EC, "write error", "HeaderWrite*", 0 );
  }
}

static ULONG Grow( struct hb_BTree * pBTree )
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  if ( GETFLAG( pBTree, IsInMemory ) )
  {
    ioBuffer_T * thisptr;

    thisptr = ioOneBufferAlloc( pBTree, NULL, pBTree->ioBuffer );
    /* TOFIX: casting pointer to ULONG! */
    thisptr->ulPage = ( ULONG ) thisptr;
    if ( pBTree->ioBuffer )  pBTree->ioBuffer->prev = thisptr;
    pBTree->ioBuffer = thisptr;
  }
  else
  {
    /* locate a page to use */
    ioBufferScan( pBTree, NULLPAGE );

    if ( BTREENODEISNULL( pBTree, pBTree->ulFreePage ) )
    {
      BYTE *buffer = pBTree->ioBuffer->Buffer;

      pBTree->ioBuffer->ulPage = hb_fsSeek( pBTree->hFile, 0, FS_END );
      hb_xmemset( buffer, '\0', pBTree->usPageSize );
      if ( pBTree->usPageSize != hb_fsWrite( pBTree->hFile, buffer, pBTree->usPageSize ) )
      {
        hb_RaiseError( HB_BTree_WriteError_EC, "write error", "Grow*", 0 );
      }
    }
    else
    {
      pBTree->ioBuffer->ulPage = hb_fsSeek( pBTree->hFile, pBTree->ulFreePage, FS_SET );
      hb_fsRead( pBTree->hFile, ( BYTE * )&pBTree->ulFreePage, sizeof( pBTree->ulFreePage ) );
    }
    pBTree->ioBuffer->IsDirty = TRUE;
  }

  return pBTree->ioBuffer->ulPage;
}

static void Prune( struct hb_BTree * pBTree, ULONG ulNode )
{
  if ( GETFLAG( pBTree, IsInMemory ) )
  {
    ioBuffer_T * thisptr;
    ULONG n;

    for ( thisptr = pBTree->ioBuffer; thisptr && thisptr->ulPage != ulNode; thisptr = thisptr->next )
      ;

    if ( thisptr->prev )   thisptr->prev->next = thisptr->next;
    if ( thisptr->next )   thisptr->next->prev = thisptr->prev;

    if ( thisptr == pBTree->ioBuffer ) /* root page */
      pBTree->ioBuffer = thisptr->next;

    for ( n = 0; n < *thisptr->pulPageCount; n++ )
      hb_itemRelease( thisptr->xData.ppData[ n ] );
    hb_xfree( thisptr );
  }
  else
  {
    hb_fsSeek( pBTree->hFile, ulNode, FS_SET );
    if ( hb_fsWrite( pBTree->hFile, ( const BYTE * )&pBTree->ulFreePage, sizeof( pBTree->ulFreePage ) ) != sizeof( pBTree->ulFreePage ) )
    {
      hb_RaiseError( HB_BTree_WriteError_EC, "write error", "Prune*", 0 );
    }
    pBTree->ulFreePage = ulNode;
  }
}

static USHORT CountGet( struct hb_BTree * pBTree, ULONG ulNode )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );
  return ( USHORT )( *pBTree->ioBuffer->pulPageCount );
}

static void CountSet( struct hb_BTree * pBTree, ULONG ulNode, ULONG newPageCount )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );
  *pBTree->ioBuffer->pulPageCount = newPageCount;
  pBTree->ioBuffer->IsDirty = pBTree->IsDirtyFlagAssignment;
}

static USHORT CountAdj( struct hb_BTree * pBTree, ULONG ulNode, LONG adjPageCount )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );

  *pBTree->ioBuffer->pulPageCount += adjPageCount;
  pBTree->ioBuffer->IsDirty = pBTree->IsDirtyFlagAssignment;

  return ( USHORT )*pBTree->ioBuffer->pulPageCount;
}

static void NodeCopy( struct hb_BTree * pBTree, ULONG toNode, int toPosition, ULONG fromNode, int fromPosition, hb_KeyData_T *buffer )
{
  ULONG ulBranch;
  LONG lData = 0;
  PHB_ITEM pData = NULL;

  READPAGE_IF_NEEDED( pBTree, fromNode );
  ulBranch = pBTree->ioBuffer->pulBranch[ fromPosition ];

  if ( GETFLAG( pBTree, IsInMemory ) )
    pData    = pBTree->ioBuffer->xData.ppData[ fromPosition - 1 ];
  else
    lData    = pBTree->ioBuffer->xData.plData[ fromPosition - 1 ];

  hb_xmemcpy( buffer->szKey, pBTree->ioBuffer->szKey + ( fromPosition - 1 ) * pBTree->usKeySize, pBTree->usKeySize );
  buffer->szKey[ pBTree->usKeySize ] = '\0';

  READPAGE_IF_NEEDED( pBTree, toNode );
  pBTree->ioBuffer->pulBranch[ toPosition ] = ulBranch;
  if ( GETFLAG( pBTree, IsInMemory ) )
    pBTree->ioBuffer->xData.ppData[ toPosition - 1 ] = pData;
  else
    pBTree->ioBuffer->xData.plData[ toPosition - 1 ] = lData;
  hb_xmemcpy( pBTree->ioBuffer->szKey + ( toPosition - 1 ) * pBTree->usKeySize, buffer->szKey, pBTree->usKeySize );
  pBTree->ioBuffer->IsDirty = pBTree->IsDirtyFlagAssignment;
}

static ULONG BranchGet( struct hb_BTree * pBTree, ULONG ulNode, int iPosition )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );
  return pBTree->ioBuffer->pulBranch[ iPosition ];
}

#if defined( DEBUG )
static void Chk4Loop( struct hb_BTree * pBTree, ULONG ulNode, int iPosition, ULONG ulBranch )
{
/*
  int i;
  for ( i = 0; i < CountGet( pBTree, ulNode ); i++ )
    if ( i != iPosition && pBTree->ioBuffer->pulBranch[ i ] == ulBranch && ulBranch )
    {
      HB_TRACE( HB_TR_ERROR, "Chk4Loop( nbranch %ld exists in page %ld (position %d, found at %d, count %d) )",
        ( long )ulBranch,
        ( long )ulNode,
        ( int )iPosition,
        ( int )i,
        CountGet( pBTree, ulNode ) );
    }
*/
  if ( ulBranch > filelength( pBTree->hFile ) )
      HB_TRACE( HB_TR_ERROR, "Chk4Loop( nbranch %ld exceeds file size in page %ld (position %d, count %d) )",
        ( long )ulBranch,
        ( long )ulNode,
        ( int )iPosition,
        ( int )CountGet( pBTree, ulNode ) );
}
#endif

static void BranchSet( struct hb_BTree * pBTree, ULONG ulNode, int iPosition, ULONG ulBranch )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );
/*
  if ( ulBranch > filelength( pBTree->hFile ) )
      HB_TRACE( HB_TR_ERROR, "BranchSet( nbranch set %ld exceeds file size in page %ld (position %d, count %d) )",
        ( long )ulBranch,
        ( long )ulNode,
        ( int )iPosition,
        CountGet( pBTree, ulNode ) );
*/

  pBTree->ioBuffer->pulBranch[ iPosition ] = ulBranch;
  pBTree->ioBuffer->IsDirty = pBTree->IsDirtyFlagAssignment;
}

/* if the parameter is null, allocate a buffer - it is the */
/* callers responsibility to release the buffer            */

static hb_KeyData_T *KeyGet( struct hb_BTree * pBTree, ULONG ulNode, int iPosition, hb_KeyData_T *buffer )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );

  if ( buffer == NULL )
    buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );

  hb_xmemcpy( buffer->szKey, pBTree->ioBuffer->szKey + ( iPosition - 1 ) * pBTree->usKeySize, pBTree->usKeySize );
  buffer->szKey[ pBTree->usKeySize ] = '\0';
  if ( GETFLAG( pBTree, IsInMemory ) )
    buffer->xData.pData = pBTree->ioBuffer->xData.ppData[ iPosition - 1 ];
  else
    buffer->xData.lData = pBTree->ioBuffer->xData.plData[ iPosition - 1 ];

  return buffer;
}

static void KeySet( struct hb_BTree * pBTree, ULONG ulNode, int iPosition, hb_KeyData_T *key )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );

/*  if ( pBTree->ioBuffer->szKey + ( iPosition - 1 ) * pBTree->usKeySize + pBTree->usKeySize > pBTree->ioBuffer->Buffer + pBTree->usPageSize ) */
  hb_xmemcpy( pBTree->ioBuffer->szKey + ( iPosition - 1 ) * pBTree->usKeySize, key->szKey, pBTree->usKeySize );
  if ( GETFLAG( pBTree, IsInMemory ) )
    pBTree->ioBuffer->xData.ppData[ iPosition - 1 ] = key->xData.pData;
  else
    pBTree->ioBuffer->xData.plData[ iPosition - 1 ] = key->xData.lData;
  pBTree->ioBuffer->IsDirty = pBTree->IsDirtyFlagAssignment;
}

static LONG KeyCompare( struct hb_BTree * pBTree, hb_KeyData_T *left, hb_KeyData_T *right )
{
/*  LONG lResults = strnicmp( left->szKey, right->szKey, pBTree->usKeySize );*/
  LONG lResults = ( pBTree->pStrCompare )( ( char * ) left->szKey, ( char * ) right->szKey, pBTree->usKeySize );

  if ( lResults == 0 && GETFLAG( pBTree, IsUnique ) )
  {
    lResults = ( left->xData.lData - right->xData.lData );
  }

  return lResults;
}

static BOOL SearchNode( struct hb_BTree * pBTree, hb_KeyData_T *target, ULONG ulNode, int *iPosition )
{
  BOOL results;
  hb_KeyData_T *buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );

  READPAGE_IF_NEEDED( pBTree, ulNode );

  if ( KeyCompare( pBTree, target, KeyGet( pBTree, ulNode, 1, buffer ) ) < 0 )
  {
    *iPosition = 0;
    results = FALSE;
  }
  else
  {
    *iPosition = CountGet( pBTree, ulNode );
    while ( KeyCompare( pBTree, target, KeyGet( pBTree, ulNode, *iPosition, buffer ) ) < 0 && *iPosition > 1 )
      ( *iPosition )--;

    results = ( BOOL )( KeyCompare( pBTree, target, buffer ) == 0 );

/* TODO: change the linear search (above) into a binary search */
#if 0
  if ( 1 || IsDebugging )
  {
    short int lower_, upper_, middle_, results_;

    lower_ = 1;
    upper_ = CountGet( pBTree, ulNode );
    do {
      middle_ = ( upper_ + lower_ ) / 2;
      results_ = KeyCompare( pBTree, target, KeyGet( pBTree, ulNode, middle_, buffer ) );
      if ( results_ < 0 )
        upper_ = middle_-1;
      else if ( results_ > 0 )
        lower_ = middle_+1;
    } while ( results_ != 0 && upper_ >= lower_ );
  if ( !( results == (BOOL)( results_ == 0 ) && *iPosition != results_ == 0  ?  middle_  :  upper_ ) )
    HB_TRACE( HB_TR_ERROR, ( "SearchNode( results=%d results_=%d *iPosition=%d myposition=%d )",
      results, (BOOL)( results_ == 0 ), *iPosition, results_ == 0  ?  middle_  :  upper_ ) );

  /*    results = ( BOOL )( results_ == 0 );
      *iPosition =  results == 0  ?  middle_  :  upper_; */
  }
#endif
  }

  BufferRelease( buffer );
  return results;
}

static ULONG hb_BTreeSearch( struct hb_BTree * pBTree, hb_KeyData_T *target, int *targetpos, BTreeStack **pStack )
{
  ULONG ulNode = pBTree->ulRootPage;

  while ( ulNode && !SearchNode( pBTree, target, ulNode, targetpos ) )
  {
    if ( pStack && *pStack )
    {
      StackPush( pStack, ulNode, *targetpos );
    }
    ulNode = BranchGet( pBTree, ulNode, *targetpos );
  }

  if ( ulNode && pStack && *pStack )
  {
    StackPush( pStack, ulNode, *targetpos );
  }

  return ulNode;
}

static void PushIn( struct hb_BTree * pBTree, hb_KeyData_T *xkey, ULONG xbranch, ULONG ulNode, int iPosition )
{
  hb_KeyData_T *buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );
  int i;

  for ( i = CountGet( pBTree, ulNode ); i > iPosition; i-- )
  {
    NodeCopy( pBTree, ulNode, i + 1, ulNode, i, buffer );
  }

  KeySet(    pBTree, ulNode, iPosition + 1, xkey );
  BranchSet( pBTree, ulNode, iPosition + 1, xbranch );
  ( void )CountAdj( pBTree, ulNode, +1 );
  BufferRelease( buffer );
}

static void Split( struct hb_BTree * pBTree, hb_KeyData_T *xkey, ULONG xbranch, ULONG ulNode, int iPosition, hb_KeyData_T **ykey, ULONG *ybranch )
{
  int i;
  int median;
  hb_KeyData_T *buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );

  if ( iPosition <= pBTree->usMinKeys )
    median = pBTree->usMinKeys;
  else
    median = pBTree->usMinKeys + 1;

  *ybranch = Grow( pBTree );
  for ( i = median + 1; i <= pBTree->usMaxKeys; i++ )
  {
    NodeCopy( pBTree, *ybranch, i - median, ulNode, i, buffer );
  }

  CountSet( pBTree, *ybranch, pBTree->usMaxKeys - median );
  CountSet( pBTree, ulNode, median );

  if ( iPosition <= pBTree->usMinKeys )
    PushIn( pBTree, xkey, xbranch, ulNode, iPosition );
  else
    PushIn( pBTree, xkey, xbranch, *ybranch, iPosition - median );

/*  hb_xmemcpy( *ykey, KeyGet( pBTree, ulNode, CountGet( pBTree, ulNode ), buffer ), pBTree->usKeySize ); */
  KeyGet( pBTree, ulNode, CountGet( pBTree, ulNode ), *ykey );

/*  *ykey = KeyGet( pBTree, ulNode, CountGet( pBTree, ulNode ) );*/
  BranchSet( pBTree, *ybranch, 0, BranchGet( pBTree, ulNode, CountGet( pBTree, ulNode ) ) );
  ( void )CountAdj( pBTree, ulNode, -1 );
  BufferRelease( buffer );
}

#ifdef DEBUG
  static ULONG sulMaxPushDownDepth = 0;
#endif
static BOOL PushDown( struct hb_BTree * pBTree, hb_KeyData_T *newkey, ULONG ulNode, hb_KeyData_T **xkey, ULONG *xbranch )
{
  int iPosition;

#ifdef DEBUG
  static ULONG PushDownDepth = 0;

  if( ++PushDownDepth > sulMaxPushDownDepth )
  {
    sulMaxPushDownDepth = PushDownDepth;
  }
#endif

  if ( BTREENODEISNULL( pBTree, ulNode ) )
  {
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
    hb_xmemcpy( *xkey, newkey, sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );/*    *xkey = newkey;*/
    *xbranch = NULLPAGE;
    return TRUE;
  }
  else
  {
    HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
    if ( SearchNode( pBTree, newkey, ulNode, &iPosition ) )
    {
      SETFLAG( pBTree, IsDuplicateKey );
      return FALSE; /* error */
    }

    if ( PushDown( pBTree, newkey, BranchGet( pBTree, ulNode, iPosition ), xkey, xbranch ) )
    {
      if ( CountGet( pBTree, ulNode ) < pBTree->usMaxKeys )
      {
        PushIn( pBTree, *xkey, *xbranch, ulNode, iPosition );
        return FALSE;
      }
      else
      {
        Split( pBTree, *xkey, *xbranch, ulNode, iPosition, xkey, xbranch );
        return TRUE;
      }
    }
    return FALSE;
  }
}

BOOL hb_BTreeInsert( struct hb_BTree * pBTree, const char * szKey, PHB_ITEM pData )
{
  hb_KeyData_T *xkey = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );
  ULONG xbranch;
  ULONG newnode;

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  RESETFLAG( pBTree, IsDuplicateKey );

  hb_xmemcpy( pBTree->pThisKeyData->szKey, szKey, pBTree->usKeySize );
  pBTree->pThisKeyData->szKey[ pBTree->usKeySize ] = '\0';

  if ( GETFLAG( pBTree, IsInMemory ) )
  {
    pBTree->pThisKeyData->xData.pData = hb_itemNew( pData );
/*printf( "[%p %p] ", pBTree->pThisKeyData->xData.pData, pData );PrintCRLF();*/
  }
  else
  {
    pBTree->pThisKeyData->xData.lData = hb_itemGetNL( pData );
  }

  if ( PushDown( pBTree, pBTree->pThisKeyData, pBTree->ulRootPage, &xkey, &xbranch ) )
  {
    newnode = Grow( pBTree );
    CountSet( pBTree, newnode, 1 );
    KeySet(    pBTree, newnode, 1, xkey );
    BranchSet( pBTree, newnode, 0, pBTree->ulRootPage );
    BranchSet( pBTree, newnode, 1, xbranch );
    pBTree->ulRootPage = newnode;
  }

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  BufferRelease( xkey );
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  if ( !GETFLAG( pBTree, IsDuplicateKey ) )
    pBTree->ulKeyCount++;

  return !GETFLAG( pBTree, IsDuplicateKey );
}

static void MoveRight( struct hb_BTree * pBTree, ULONG ulNode, int iPosition )
{
  int c;
  ULONG tmpnode;
  hb_KeyData_T *buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );

  tmpnode = BranchGet( pBTree, ulNode, iPosition );
  for ( c = CountGet( pBTree, tmpnode ); c > 0; c-- )
  {
    NodeCopy( pBTree, tmpnode, c + 1, tmpnode, c, buffer );
  }

  BranchSet( pBTree, tmpnode, 1, BranchGet( pBTree, tmpnode, 0 ) );
  ( void )CountAdj( pBTree, tmpnode, +1 );
  KeySet(    pBTree, tmpnode, 1, KeyGet( pBTree, ulNode, iPosition, buffer ) );

  tmpnode = BranchGet( pBTree, ulNode, iPosition - 1 );
  KeySet(    pBTree, ulNode, iPosition, KeyGet( pBTree, tmpnode, CountGet( pBTree, tmpnode ), buffer ) );
  BranchSet( pBTree, BranchGet( pBTree, ulNode, iPosition ), 0, BranchGet( pBTree, tmpnode, CountGet( pBTree, tmpnode ) ) );
  ( void )CountAdj( pBTree, tmpnode, -1 );
  BufferRelease( buffer );
}

static void MoveLeft( struct hb_BTree * pBTree, ULONG ulNode, int iPosition )
{
  int c, c_count;
  ULONG tmpnode;
  hb_KeyData_T *buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );

  tmpnode = BranchGet( pBTree, ulNode, iPosition - 1 );
  c_count = CountAdj( pBTree, tmpnode, +1 );
  KeySet(    pBTree, tmpnode, c_count, KeyGet(    pBTree, ulNode, iPosition, buffer ) );
  BranchSet( pBTree, tmpnode, c_count, BranchGet( pBTree, BranchGet( pBTree, ulNode, iPosition ), 0 ) );

  tmpnode = BranchGet( pBTree, ulNode, iPosition );
  KeySet(    pBTree, ulNode, iPosition, KeyGet( pBTree, tmpnode, 1, buffer ) );
  BranchSet( pBTree, tmpnode, 0, BranchGet( pBTree, tmpnode, 1 ) );
  c_count = CountAdj( pBTree, tmpnode, -1 );

  for ( c = 1; c <= c_count; c++ )
  {
    NodeCopy( pBTree, tmpnode, c, tmpnode, c + 1, buffer );
  }

  BufferRelease( buffer );
}

static void Combine( struct hb_BTree * pBTree, ULONG ulNode, int iPosition )
{
  int c, c_count;
  ULONG tmpnode;
  ULONG leftnode;
  ULONG leftpagecount;
  hb_KeyData_T *buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );

  tmpnode  = BranchGet( pBTree, ulNode, iPosition );
  leftnode = BranchGet( pBTree, ulNode, iPosition - 1 );
  leftpagecount = CountAdj( pBTree, leftnode, +1 );
  KeySet(    pBTree, leftnode, leftpagecount, KeyGet(    pBTree, ulNode, iPosition, buffer ) );
  BranchSet( pBTree, leftnode, leftpagecount, BranchGet( pBTree, tmpnode, 0 ) );

  c_count = CountGet( pBTree, tmpnode );
  for ( c = 1; c <= c_count; c++ )
  {
    leftpagecount = CountAdj( pBTree, leftnode, +1 );
    NodeCopy( pBTree, leftnode, leftpagecount, tmpnode, c, buffer );
  }

  c_count = CountGet( pBTree, ulNode );
  for ( c = iPosition; c < c_count; c++ )
  {
    NodeCopy( pBTree, ulNode, c, ulNode, c + 1, buffer );
  }
  ( void )CountAdj( pBTree, ulNode, -1 );

  Prune( pBTree, tmpnode );
  BufferRelease( buffer );
}

static void Restore( struct hb_BTree * pBTree, ULONG ulNode, int iPosition )
{
  READPAGE_IF_NEEDED( pBTree, ulNode );

  if ( iPosition == 0 ) /* left-most key */
  {
    if ( CountGet( pBTree, BranchGet( pBTree, ulNode, 1 ) ) > pBTree->usMinKeys )
    {
      MoveLeft( pBTree, ulNode, 1 );
    }
    else
    {
      Combine( pBTree, ulNode, 1 );
    }
  }
  else if ( iPosition == CountGet( pBTree, ulNode ) ) /* right-most key */
  {
    if ( CountGet( pBTree, BranchGet( pBTree, ulNode, iPosition - 1 ) ) > pBTree->usMinKeys )
    {
      MoveRight( pBTree, ulNode, iPosition );
    }
    else
    {
      Combine( pBTree, ulNode, iPosition );
    }
  }
  else if ( CountGet( pBTree, BranchGet( pBTree, ulNode, iPosition - 1 ) ) > pBTree->usMinKeys )
  {
    MoveRight( pBTree, ulNode, iPosition );
  }
  else if ( CountGet( pBTree, BranchGet( pBTree, ulNode, iPosition + 1 ) ) > pBTree->usMinKeys )
  {
    MoveLeft( pBTree, ulNode, iPosition + 1 );
  }
  else
    Combine( pBTree, ulNode, iPosition );
}

static void Remove( struct hb_BTree * pBTree, ULONG ulNode, int iPosition )
{
  int c, c_count;
  hb_KeyData_T *buffer = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );

  c_count = CountGet( pBTree, ulNode );
  for ( c = iPosition + 1; c <= c_count; c++ )
  {
    NodeCopy( pBTree, ulNode, c - 1, ulNode, c, buffer );
  }
  ( void )CountAdj( pBTree, ulNode, -1 );
  BufferRelease( buffer );
}

static void Successor( struct hb_BTree * pBTree, ULONG ulNode, int iPosition )
{
  ULONG tmpnode;
  hb_KeyData_T *buffer;

  for ( tmpnode = BranchGet( pBTree, ulNode, iPosition );
        !BTREENODEISNULL( pBTree, BranchGet( pBTree, tmpnode, 0 ) );
        tmpnode = BranchGet( pBTree, tmpnode, 0 ) )
    ;
  KeySet(    pBTree, ulNode, iPosition, ( buffer = KeyGet(  pBTree, tmpnode, 1, NULL ) ) );
  BufferRelease( buffer );
}

#ifdef DEBUG
  static ULONG sulMaxRecDeleteDepth = 0;
#endif
static BOOL RecDelete( struct hb_BTree * pBTree, hb_KeyData_T *target, ULONG ulNode )
{
#ifdef DEBUG
  static ULONG RecDeleteDepth = 0;
#endif

  int iPosition;
  BOOL found;

  if ( BTREENODEISNULL( pBTree, ulNode ) )
  {
    /* todo: hitting an empty pBTree is an error */
    return FALSE;
  }
  else
  {
#ifdef DEBUG
    if( ++RecDeleteDepth > sulMaxRecDeleteDepth )
    {
      sulMaxRecDeleteDepth = RecDeleteDepth;
      if ( RecDeleteDepth > 10 )  { HB_TRACE( HB_TR_ERROR, ( "RecDelete( Exiting 2! )" ) ); exit(0); }
    }
#endif
    /* inline assignment & comparision was generating a Borland warning */
    found = SearchNode( pBTree, target, ulNode, &iPosition );
    if ( found )
    {
      if ( !BTREENODEISNULL( pBTree, BranchGet( pBTree, ulNode, iPosition - 1 ) ) )
      {
        Successor( pBTree, ulNode, iPosition ); /* move successor to this iPosition */
        KeyGet( pBTree, ulNode, iPosition, target );
        found = RecDelete( pBTree, target, BranchGet( pBTree, ulNode, iPosition ) );
#if 0
        if ( !found )
          error( "key not found" ); /* key exists, so this shouldn't occur*/
#endif
      }
      else
        Remove( pBTree, ulNode, iPosition );
    }
    else
      found = RecDelete( pBTree, target, BranchGet( pBTree, ulNode, iPosition ) );

    /* the recursive call has returned */
    if ( found && !BTREENODEISNULL( pBTree, BranchGet( pBTree, ulNode, iPosition ) ) )
      if ( CountGet( pBTree, BranchGet( pBTree, ulNode, iPosition ) ) < pBTree->usMinKeys )
        Restore( pBTree, ulNode, iPosition );
#ifdef DEBUG
--RecDeleteDepth;
#endif
    return found;
  }
}

#if 0
 static BOOL __RecDelete( struct hb_BTree * pBTree, hb_KeyData_T target, ULONG ulNode )
 {
   static ULONG RecDeleteDepth = 0;
   BOOL found = FALSE;

   if( ++RecDeleteDepth > sulMaxRecDeleteDepth )
   {
     sulMaxRecDeleteDepth = RecDeleteDepth;
   }

   if ( !BTREENODEISNULL( pBTree, ulNode ) )
   {
      int iPosition;
 /*    hb_KeyData_T tmpTarget;                                                  */
 /*                                                                          */
 /*    tmpTarget = BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 ); */
 /*    hb_xmemcpy( tmpTarget, target, pBTree->usKeySize );                         */

     #define tmpTarget target

     /* inline assignment & comparision was generating a Borland warning */
     found = SearchNode( pBTree, tmpTarget, ulNode, &iPosition )
     if ( found )
     {
       if ( !BTREENODEISNULL( pBTree, BranchGet( pBTree, ulNode, iPosition - 1 ) ) )
       {
         Successor( pBTree, ulNode, iPosition );
         hb_xmemcpy( tmpTarget, KeyGet( pBTree, ulNode, iPosition ), pBTree->usKeySize );
         /*if ( !( found =*/ RecDelete( pBTree, tmpTarget, BranchGet( pBTree, ulNode, iPosition ) );/*) )*/
 /*         RESETFLAG( pBTree, IsRecordFound )*/ /*pBTree->bRecordNotFound = TRUE*/;  /* error */
       }
       else
       {
         Remove( pBTree, ulNode, iPosition );
       }
     }
     else
     {
       if ( !( found = RecDelete( pBTree, tmpTarget, BranchGet( pBTree, ulNode, iPosition ) ) ) )
         RESETFLAG( pBTree, IsRecordFound );  /* error */
     }

 /*    if ( GETFLAG( pBTree, IsRecordFound )*/ /*!tree->bRecordNotFound )*/
     {
       ULONG tmpNode;

       tmpNode = BranchGet( pBTree, ulNode, iPosition );
       if ( !BTREENODEISNULL( pBTree, tmpNode ) )
         if ( CountGet( pBTree, tmpNode ) < pBTree->usMinKeys )
           Restore( pBTree, ulNode, iPosition );
     }

     /*BufferRelease( tmpTarget );*/
     #undef tmpTarget
   }
   else
     RESETFLAG( pBTree, IsRecordFound );  /* error */

   --RecDeleteDepth;

   return found;
 }
#endif

BOOL hb_BTreeDelete( struct hb_BTree * pBTree, const char *target, LONG lData )
{
  ULONG ulNode;
  int iPosition;
  hb_KeyData_T *tmpTarget;
  BOOL found = FALSE;

  SETFLAG( pBTree, IsRecordFound );
  CLEARKEYDATA( pBTree );

  tmpTarget = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );
  hb_xmemcpy( tmpTarget->szKey, target, pBTree->usKeySize );
  tmpTarget->xData.lData = lData;

  if ( hb_BTreeSearch( pBTree, tmpTarget, &iPosition, NULL ) )
  {
    if ( RecDelete( pBTree, tmpTarget, pBTree->ulRootPage ) )
    {
      found = TRUE;
      if ( CountGet( pBTree, pBTree->ulRootPage ) == 0 )
      {
        ulNode = pBTree->ulRootPage;
        pBTree->ulRootPage = BranchGet( pBTree, pBTree->ulRootPage, 0 );
        Prune( pBTree, ulNode );
      }
    }
#if 0
    else
      ;  /* error - key does not exist */
#endif
  }
  else
    RESETFLAG( pBTree, IsRecordFound );

  BufferRelease( tmpTarget );

  if ( found )
    pBTree->ulKeyCount--;

  return found;/*!tree->bRecordNotFound;*/
}

void hb_BTreeGoTop( struct hb_BTree * pBTree )
{
  ULONG ulNode;
  ULONG ulLastNode;

  for ( ulLastNode = ulNode = pBTree->ulRootPage; !BTREENODEISNULL( pBTree, ulNode ); ulLastNode = ulNode, ulNode = BranchGet( pBTree, ulNode, 0 ) )
    ;

  if ( BTREENODEISNULL( pBTree, ulLastNode ) )
    CLEARKEYDATA( pBTree );
  else
    KeyGet( pBTree, ulLastNode, 1, pBTree->pThisKeyData );
}

void hb_BTreeGoBottom( struct hb_BTree * pBTree )
{
  ULONG ulNode;
  ULONG ulLastNode;

  for ( ulLastNode = ulNode = pBTree->ulRootPage; !BTREENODEISNULL( pBTree, ulNode ); ulLastNode = ulNode, ulNode = BranchGet( pBTree, ulNode, CountGet( pBTree, ulNode ) ) )
    ;
  if ( BTREENODEISNULL( pBTree, ulLastNode ) )
    CLEARKEYDATA( pBTree );
  else
    KeyGet( pBTree, ulLastNode, CountGet( pBTree, ulLastNode ), pBTree->pThisKeyData );
}

LONG hb_BTreeSkip( struct hb_BTree * pBTree, LONG records )
{
  LONG results = 0;
  int iPosition;
  hb_KeyData_T *keydata;
  BTreeStack *pStack = NULL;

  if ( BTREENODEISNULL( pBTree, pBTree->ulRootPage ) )
  {
    CLEARKEYDATA( pBTree );
    return 0;
  }

  keydata = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );
  hb_xmemcpy( keydata->szKey, pBTree->pThisKeyData->szKey, pBTree->usKeySize );
  keydata->xData.lData = pBTree->pThisKeyData->xData.lData;

  StackNew( &pStack );
  if ( !BTREENODEISNULL( pBTree, hb_BTreeSearch( pBTree, keydata, &iPosition, &pStack ) ) )
  {
    results = StackSkip( pBTree, &pStack, records );
  }
  else
  {
    CLEARKEYDATA( pBTree );
  }

  StackRelease( &pStack );
  BufferRelease( keydata );

  return results;
}

BOOL hb_BTreeSeek( struct hb_BTree * pBTree, const char *szKey, LONG lData, BOOL bSoftSeek )
{
  BOOL results = FALSE;
  int iPosition;
  hb_KeyData_T *tmpTarget;
  BTreeStack *pStack = NULL;

  tmpTarget = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );
  hb_xmemcpy( tmpTarget->szKey, szKey, pBTree->usKeySize );
  tmpTarget->xData.lData = lData;

  StackNew( &pStack );
  if ( !BTREENODEISNULL( pBTree, hb_BTreeSearch( pBTree, tmpTarget, &iPosition, &pStack ) ) ||
       ( bSoftSeek && pStack->usCount > 0 && ( 1 == StackSkip( pBTree, &pStack, 1 ) ) ) )
  {
    KeyGet( pBTree, STACKNODE( &pStack ), STACKPOSITION( &pStack ), pBTree->pThisKeyData );
    results = TRUE;
  }
  else
  {
    CLEARKEYDATA( pBTree );
  }

  StackRelease( &pStack );
  BufferRelease( tmpTarget );

  return results;
}


static int hb_BTstrncmp( const char *s1, const char *s2, size_t n )
{
   return strncmp( s1, s2, n );
}

/* allocate hb_BTree structure */
struct hb_BTree * hb_BTreeNew( const char * FileName, USHORT usPageSize, USHORT usKeySize, ULONG ulFlags, ULONG ulBuffers )
{
  struct hb_BTree *pBTree = ( struct hb_BTree * ) BufferAlloc( sizeof( struct hb_BTree ) );
  int iMaximumKeys;
  ULONG size;
  int iFileIOmode;
  BYTE *buffer;

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  /* correct parameters to default values */
  if ( usKeySize < 8 )            usKeySize = 8;
  if ( usPageSize % HB_BTREE_HEADERSIZE )  usPageSize -= ( usPageSize % HB_BTREE_HEADERSIZE );
  if ( usPageSize < HB_BTREE_HEADERSIZE )  usPageSize = HB_BTREE_HEADERSIZE;

  /*
    number of keys is:
      ( usPageSize - ( sizeof count + sizeof branch[0] ) ) / ( sizeof branch + sizeof key ) - 1
  */

  iMaximumKeys = ( + usPageSize                                \
                   - sizeof( *pBTree->ioBuffer->pulPageCount ) \
                   - sizeof( *pBTree->ioBuffer->pulBranch ) )  \
                 / ( sizeof( *pBTree->ioBuffer->pulBranch ) +  \
                     ( ulFlags & HB_BTREE_INMEMORY ) == HB_BTREE_INMEMORY ? sizeof( *pBTree->ioBuffer->xData.ppData ) : sizeof( *pBTree->ioBuffer->xData.plData )    +  \
                     usKeySize ) - 1;

  if ( ( ulFlags & HB_BTREE_INMEMORY ) == HB_BTREE_INMEMORY )
  {
    pBTree->hFile = 0;
    pBTree->szFileName = NULL;
    pBTree->IsDirtyFlagAssignment = FALSE; /* replaces const value for assignment */
  }
  else
  {
    pBTree->IsDirtyFlagAssignment = TRUE;  /* replaces const value for assignment */

    if ( ( ulFlags & ( HB_BTREE_READONLY ) ) == HB_BTREE_READONLY )
    {
      iFileIOmode = FC_READONLY;
    }
    else
    {
      iFileIOmode = FC_NORMAL;
    }

    pBTree->hFile = hb_fsCreate( FileName, iFileIOmode );
    if ( pBTree->hFile == -1 )
    {
      BufferRelease( pBTree );
      return NULL;
    }

    if ( ( ulFlags & ( HB_BTREE_SHARED ) ) == HB_BTREE_SHARED )
    {
      hb_fsClose( pBTree->hFile );
      pBTree->hFile = hb_fsOpen( FileName, FO_READWRITE | FO_SHARED );
      if ( pBTree->hFile == -1 )
      {
        BufferRelease( pBTree );
        return NULL;
      }
    }

    pBTree->szFileName = hb_strdup( FileName );
  }

  pBTree->ulFreePage = NULLPAGE;
  pBTree->ulRootPage = NULLPAGE;
  pBTree->usPageSize = usPageSize;
  pBTree->usKeySize  = usKeySize;
  pBTree->usMaxKeys  = ( USHORT ) iMaximumKeys;
  pBTree->usMinKeys  = ( USHORT ) ( ( iMaximumKeys + 1 ) / 2 - iMaximumKeys % 2 );
  pBTree->ulFlags    = ulFlags;
  pBTree->ulKeyCount = 0;
  pBTree->pStack     = NULL;
/* TODO: use stack optimization if flags warrant: if ( flag... ) StackNew( &pBTree->pStack ); */
  pBTree->pThisKeyData = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );
  CLEARKEYDATA( pBTree );
  ioBufferAlloc( pBTree, ulBuffers );

  if ( GETFLAG( pBTree, IsCaseLess ) )
  {
    pBTree->pStrCompare = ( BTreeCmpFunc ) hb_strnicmp;
  }
  else
  {
    pBTree->pStrCompare = ( BTreeCmpFunc ) hb_BTstrncmp;
  }

  if ( GETFLAG( pBTree, IsInMemory ) == FALSE )
  {
  /* HeaderWrite() leaves the file pointer at the end of the usable area,
     so the padding amount is the header size minus current position */
    HeaderWrite( pBTree );
    size = HB_BTREE_HEADERSIZE - hb_fsTell( pBTree->hFile );
    buffer = ( BYTE * ) BufferAlloc( size );
    hb_xmemset( buffer, '\0', size );

    if ( size != hb_fsWriteLarge( pBTree->hFile, buffer, size ) )
    {
      BufferRelease( buffer );
      BufferRelease( pBTree );
      hb_RaiseError( HB_BTree_WriteError_EC, "write error", "hb_btreenew", 0 );
      hb_retni( -1 );
    }
    BufferRelease( buffer );
  }
  else /* IsInMemory == TRUE */
  {
    RESETFLAG( pBTree, HB_BTREE_UNIQUE ); /* clear this flag */
  }

  return pBTree;
}

/* open an existing structure */
struct hb_BTree *hb_BTreeOpen( const char *FileName, ULONG ulFlags, ULONG ulBuffers )
{
  struct hb_BTree *pBTree = ( struct hb_BTree * ) BufferAlloc( sizeof( struct hb_BTree ) );
  int iMaximumKeys;
  BYTE TmpHeader[ sizeof( HEADER_ID ) - 1 ];

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  pBTree->szFileName = hb_strdup( FileName );
  pBTree->hFile      = hb_fsOpen( pBTree->szFileName, FO_READWRITE );
  if ( pBTree->hFile == -1 )
  {
    BufferRelease( pBTree );
    return NULL;
  }

  hb_fsRead( pBTree->hFile, TmpHeader, sizeof( HEADER_ID ) - 1 );
  if ( memcmp( TmpHeader, HEADER_ID, sizeof( HEADER_ID ) - 1 ) != 0 )
  {
    hb_fsClose( pBTree->hFile );
    BufferRelease( pBTree );
    return NULL;
  }

  hb_fsRead( pBTree->hFile, ( BYTE * )&pBTree->ulFreePage, sizeof( pBTree->ulFreePage ) );
  hb_fsRead( pBTree->hFile, ( BYTE * )&pBTree->usPageSize, sizeof( pBTree->usPageSize ) );
  hb_fsRead( pBTree->hFile, ( BYTE * )&pBTree->usKeySize , sizeof( pBTree->usKeySize  ) );
  hb_fsRead( pBTree->hFile, ( BYTE * )&pBTree->ulRootPage, sizeof( pBTree->ulRootPage ) );
  hb_fsRead( pBTree->hFile, ( BYTE * )&pBTree->ulFlags   , sizeof( pBTree->ulFlags    ) );
  hb_fsRead( pBTree->hFile, ( BYTE * )&pBTree->ulKeyCount, sizeof( pBTree->ulKeyCount ) );

  iMaximumKeys = ( + pBTree->usPageSize                    \
                   - sizeof( *pBTree->ioBuffer->pulPageCount ) \
                   - sizeof( *pBTree->ioBuffer->pulBranch ) )  \
                 / ( sizeof( *pBTree->ioBuffer->pulBranch ) +  \
                     sizeof( *pBTree->ioBuffer->xData.plData )    +  \
                     pBTree->usKeySize ) - 1;

  pBTree->usMaxKeys  = ( USHORT ) iMaximumKeys;
  pBTree->usMinKeys  = ( USHORT ) ( ( iMaximumKeys + 1 ) / 2 - iMaximumKeys % 2 );
  pBTree->pThisKeyData = ( hb_KeyData_T * ) BufferAlloc( sizeof( hb_KeyData_T ) + pBTree->usKeySize + 1 );
  CLEARKEYDATA( pBTree );
  pBTree->pStack     = NULL;
/* TODO: use stack optimization if flags warrant: if ( flag... ) StackNew( &pBTree->pStack ); */
  HB_SYMBOL_UNUSED( ulFlags );
  ioBufferAlloc( pBTree, ulBuffers );

  RESETFLAG( pBTree, HB_BTREE_INMEMORY ); /* clear this flag */
  pBTree->IsDirtyFlagAssignment = TRUE;  /* replaces const value for assignment */

  if ( GETFLAG( pBTree, IsCaseLess ) )
  {
    pBTree->pStrCompare = ( BTreeCmpFunc ) hb_strnicmp;
  }
  else
  {
    pBTree->pStrCompare = ( BTreeCmpFunc ) hb_BTstrncmp;
  }

  return pBTree;
}

void hb_BTreeClose( struct hb_BTree * pBTree )
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  ioBufferRelease( pBTree );
  if ( GETFLAG( pBTree, IsInMemory ) == FALSE )  HeaderWrite( pBTree );
  if ( pBTree->hFile != 0 )
    hb_fsClose( pBTree->hFile );
  if ( pBTree->szFileName != NULL )
    BufferRelease( pBTree->szFileName );
  StackRelease( &pBTree->pStack );
  BufferRelease( pBTree->pThisKeyData );
  BufferRelease( pBTree );
}

const char * hb_BTreeKey( struct hb_BTree * pBTree )
{
  return ( const char * )pBTree->pThisKeyData->szKey;
}

LONG hb_BTreeData( struct hb_BTree * pBTree )
{
  return pBTree->pThisKeyData->xData.lData;
}

PHB_ITEM hb_BTreeDataItem( struct hb_BTree * pBTree )
{
  return ( PHB_ITEM ) pBTree->pThisKeyData->xData.pData;
}

static int BTree_SetTreeIndex( struct hb_BTree * pBTree )
{
  int n;

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  if ( pBTree == NULL )
    return -1;

  for ( n = 0; n < s_BTree_List_Count && s_BTree_List[ n ] != NULL; n++ )
    ;

  if ( n == s_BTree_List_Count )
  {
    s_BTree_List = (struct hb_BTree **) BufferRealloc( s_BTree_List, ++s_BTree_List_Count * sizeof( s_BTree_List[ 0 ] ) );
  }

  s_BTree_List[ n ] = pBTree;

  return n + 1;
}

static struct hb_BTree *BTree_GetTreeIndex( const char * GetSource )
{
  int index;

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  index = hb_parni( 1 );
  if ( index < 1 || index > s_BTree_List_Count || s_BTree_List[ index ] == NULL ) \
  {
    hb_RaiseError( HB_BTree_TreeHandle_EC, "Bad BTree handle", GetSource, 1 );
    return NULL;
  }
  else
    return s_BTree_List[ index - 1 ];
}

HB_FUNC( HB_BTREEOPEN  )  /* hb_BTreeOpen( CHAR cFileName, ULONG ulFlags [ , int nBuffers=1 ] )  ->  hb_Btree_Handle */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  if ( HB_ISCHAR( 1 ) )
    hb_retni( BTree_SetTreeIndex( hb_BTreeOpen( hb_parc( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) ) ) );
  else
  {
    hb_RaiseError( HB_BTreeArgError_EC, "Bad argument(s)", HB_ERR_FUNCNAME, hb_pcount() );
    hb_retni( 0 );
  }
}

HB_FUNC( HB_BTREENEW )  /* hb_BTreeNew( CHAR cFileName, int nPageSize, int nKeySize, [ ULONG ulFlags ], [ int nBuffers=1 ] )  ->  hb_Btree_Handle */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  if ( ( ( hb_parnl( 4 ) & HB_BTREE_INMEMORY ) == HB_BTREE_INMEMORY || HB_ISCHAR( 1 ) ) &&
       HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
  {
    hb_retni( BTree_SetTreeIndex( hb_BTreeNew( hb_parc( 1 ), ( USHORT ) hb_parni( 2 ), ( USHORT ) hb_parni( 3 ), hb_parnl( 4 ), hb_parnl( 5 ) ) ) );
  }
  else
  {
    hb_RaiseError( HB_BTreeArgError_EC, "Bad argument(s)", HB_ERR_FUNCNAME, hb_pcount() );
    hb_retni( 0 );
  }
}

HB_FUNC( HB_BTREECLOSE )  /* hb_BTreeClose( hb_BTree_Handle ) -> NIL */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  hb_BTreeClose( BTree_GetTreeIndex( "hb_btreeclose" ) );
  s_BTree_List[ hb_parni( 1 ) - 1 ] = NULL;
}

HB_FUNC( HB_BTREEINSERT )  /* hb_BTreeInsert( hb_BTree_Handle, CHAR cKey, LONG lData | ANY xData ) -> lSuccess */
{
  struct hb_BTree * pBTree = BTree_GetTreeIndex( "hb_btreeinsert" );
  /* PHB_ITEM pKeyCode = hb_param( 1, HB_IT_NUMERIC ); */

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  if ( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && ( hb_pcount() == 2 || GETFLAG( pBTree, IsInMemory ) || HB_ISNUM( 3 ) ) )
    hb_retl( hb_BTreeInsert( BTree_GetTreeIndex( "hb_btreeinsert" ), hb_parc( 2 ), hb_paramError( 3 ) ) );
  else
  {
    hb_RaiseError( HB_BTreeArgError_EC, "Bad argument(s)", HB_ERR_FUNCNAME, hb_pcount() );
    hb_retl( FALSE );
  }
}

HB_FUNC( HB_BTREEDELETE )  /* hb_BTreeDelete( hb_BTree_Handle, CHAR cKey, LONG lData ) -> lSuccess */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  if ( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && ( hb_pcount() == 2 || HB_ISNUM( 3 ) ) )
    hb_retl( hb_BTreeDelete( BTree_GetTreeIndex( "hb_btreedelete" ), hb_parc( 2 ), hb_parnl( 3 ) ) );
  else
  {
    hb_RaiseError( HB_BTreeArgError_EC, "Bad argument(s)", HB_ERR_FUNCNAME, hb_pcount() );
    hb_retl( FALSE );
  }
}

HB_FUNC( HB_BTREEKEY )  /* hb_BTreeKey( hb_BTree_Handle ) -> CHAR cKey */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  hb_retc( ( char * ) BTree_GetTreeIndex( "hb_btreekey" )->pThisKeyData->szKey );
}

HB_FUNC( HB_BTREEDATA )  /* hb_BtreeData( hb_BTree_Handle ) -> LONG lOldData | xOldData */
{  /*, [ LONG lNewData | ANY xNewData ]  ???  */
  struct hb_BTree * pBTree = BTree_GetTreeIndex( "hb_btreeinfo" );
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  if ( GETFLAG( pBTree, IsInMemory ) )
  {
    if ( pBTree->pThisKeyData->xData.pData )
      hb_itemReturn( pBTree->pThisKeyData->xData.pData );
    else
      hb_ret();
  }
  else
  {
    hb_retnl( pBTree->pThisKeyData->xData.lData );

  }
}

HB_FUNC( HB_BTREEGOTOP )  /* hb_BTreeGoTop( hb_BTree_Handle ) --> NIL */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  hb_BTreeGoTop( BTree_GetTreeIndex( "hb_btreegotop" ) );
}

HB_FUNC( HB_BTREEGOBOTTOM )  /* hb_BTreeGoBottom( hb_BTree_Handle ) --> NIL */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  hb_BTreeGoBottom( BTree_GetTreeIndex( "hb_btreegobottom" ) );
}

HB_FUNC( HB_BTREESKIP )  /* hb_BTreeSkip( hb_BTree_Handle, LONG nRecords ) -> LONG nRecordsSkipped */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  if ( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
    hb_retnl( hb_BTreeSkip( BTree_GetTreeIndex( "hb_btreeskip" ), hb_parnl( 2 ) ) );
  else
  {
    hb_RaiseError( HB_BTreeArgError_EC, "Bad argument(s)", HB_ERR_FUNCNAME, hb_pcount() );
    hb_retnl( 0 );
  }
}

HB_FUNC( HB_BTREESEEK )  /* hb_BTreeSeek( hb_BTree_Handle, CHAR cKey, LONG lData, BOOL lSoftSeek ) -> lSuccess */
{
  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );
  if ( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) )
    hb_retl( hb_BTreeSeek( BTree_GetTreeIndex( "hb_btreeseek" ), hb_parc( 2 ), hb_parnl( 3 ), hb_parl( 4 ) ) );
  else
  {
    hb_RaiseError( HB_BTreeArgError_EC, "Bad argument(s)", HB_ERR_FUNCNAME, hb_pcount() );
    hb_retl( FALSE );
  }
}

HB_FUNC( HB_BTREEINFO )  /* hb_BTreeInfo( hb_BTree_Handle, [index] ) -> aResults | cResults | nResults */
{
  struct hb_BTree * pBTree = BTree_GetTreeIndex( "hb_btreeinfo" );

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  if ( pBTree )
    switch ( hb_parni( 2 ) )
    {
    case HB_BTREEINFO_FILENAME:  hb_retc( ( char * ) pBTree->szFileName ); break;
    case HB_BTREEINFO_PAGESIZE:  hb_retni( pBTree->usPageSize ); break;
    case HB_BTREEINFO_KEYSIZE:   hb_retni( pBTree->usKeySize ); break;
    case HB_BTREEINFO_MAXKEYS:   hb_retni( pBTree->usMaxKeys ); break;
    case HB_BTREEINFO_MINKEYS:   hb_retni( pBTree->usMinKeys ); break;
    case HB_BTREEINFO_FLAGS:     hb_retnl( pBTree->ulFlags ); break;
    case HB_BTREEINFO_KEYCOUNT:  hb_retnl( pBTree->ulKeyCount ); break;
    case HB_BTREEINFO_ALL:
    default:  /* build an array and store all elements from above into it */
      {
        PHB_ITEM info = hb_itemArrayNew( HB_BTREEINFO__SIZE );

        hb_arraySetC(  info, HB_BTREEINFO_FILENAME, ( char * ) pBTree->szFileName );
        hb_arraySetNI( info, HB_BTREEINFO_PAGESIZE, pBTree->usPageSize );
        hb_arraySetNI( info, HB_BTREEINFO_KEYSIZE , pBTree->usKeySize );
        hb_arraySetNI( info, HB_BTREEINFO_MAXKEYS , pBTree->usMaxKeys );
        hb_arraySetNI( info, HB_BTREEINFO_MINKEYS , pBTree->usMinKeys );
        hb_arraySetNL( info, HB_BTREEINFO_FLAGS   , pBTree->ulFlags );
        hb_arraySetNL( info, HB_BTREEINFO_KEYCOUNT, pBTree->ulKeyCount );

        hb_itemReturnRelease( info );
      }
    }
}

#if 0
HB_FUNB( HB_BTREEEVAL )  /* hb_BTreeEval( hb_BTree_Handle, bBlock, [bForCondition], [bWhileCondition], [nNextRecords], [nRecord], [lRest] ) -- NIL */
{
  if ( HB_ISNUM( 1 ) && HB_ISBLOCK( 2 ) )
    hb_BTreeEval( BTree_GetTreeIndex( "hb_btreeeval" ), 0 );
  else
    hb_RaiseError( HB_BTreeArgError_EC, "Bad argument(s)", HB_ERR_FUNCNAME, hb_pcount() );
}
#endif


static void hb_BTree_Initialize( void * cargo )
{
  /* TODO: initialization code */

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  HB_SYMBOL_UNUSED( cargo );

}

static void hb_BTree_Terminate( void * cargo )
{
  /* TODO: termination (cleanup) code */

  int n;

  HB_TRACE( HB_TR_DEBUG, ( SRCLINENO ) );

  HB_SYMBOL_UNUSED( cargo );

  for ( n = 0; n < s_BTree_List_Count; n++ )
  {
    if ( s_BTree_List[ n ] )  hb_BTreeClose( s_BTree_List[ n ] );
  }

  if ( s_BTree_List_Count > 0 )
    BufferRelease( s_BTree_List );
}

HB_CALL_ON_STARTUP_BEGIN( _hb_BTree_Initialize_ )
   hb_vmAtInit( hb_BTree_Initialize, NULL );
   hb_vmAtExit( hb_BTree_Terminate, NULL );
HB_CALL_ON_STARTUP_END( _hb_BTree_Initialize_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_BTree_Initialize_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_BTree_Initialize_ )
   #include "hbiniseg.h"
#endif

HB_EXTERN_END
