/*
 *  WARNING !!! DOS specific
 */

#include <dos.h>
#include <gtapi.h>

#define VIDEO_INT 0x10

static WORD  suiRow        = 0;
static WORD  suiCol        = 0;
static WORD  suiDispCount  = 0;
static WORD  suiMaxRow     = 24;
static WORD  suiMaxCol     = 79;
static BYTE *sfpScreenBuffer;
static WORD  suiAttribs[ 5 ] = { 0x07, 0x70, 0x00, 0x00, 0x07 };
static WORD  suiAttrIndex = 0;

HARBOUR TERMINIT( void )
{
#ifdef __DOS__
  WORD uiRectSize;

  _gtRectSize( 0, 0, suiMaxRow, suiMaxCol, &uiRectSize );
  sfpScreenBuffer = ( BYTE * )_xgrab( uiRectSize );
#endif
}

HARBOUR TERMDONE( void )
{
#ifdef __DOS__
  _xfree( sfpScreenBuffer );
#endif
}

ERRORCODE _gtBox( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, BYTE *fpBoxString )
{
#ifdef __DOS__
  int iCount;

  if ( uiTop > _gtMaxRow() || uiBottom > _gtMaxRow() ||
       uiLeft > _gtMaxCol() || uiRight > _gtMaxCol() ||
       uiTop > uiBottom || uiLeft > uiRight )
    return 1;

  _gtDispBegin();

  /* left upper corner */
  _gtSetPos( uiTop, uiLeft );
  _gtWrite( fpBoxString, 1 );

  /* top line */
  _gtRepChar( uiTop, uiLeft + 1, fpBoxString[ 1 ], uiRight - uiLeft - 2 );

  /* right upper corner */
  _gtSetPos( uiTop, uiLeft );
  _gtWrite( &fpBoxString[ 2 ], 1 );

  /* left and right */
  for ( iCount = uiTop; iCount < uiBottom; iCount ++ )
    {
      _gtSetPos( iCount, uiLeft );
      _gtWrite( &fpBoxString[ 3 ], 1 );

      _gtSetPos( iCount, uiRight );
      _gtWrite( &fpBoxString[ 4 ], 1 );
    }

  /* left bottom corner */
  _gtSetPos( uiBottom, uiLeft );
  _gtWrite( &fpBoxString[ 5 ], 1 );

  /* top line */
  _gtRepChar( uiTop, uiLeft + 1, fpBoxString[ 6 ], uiRight - uiLeft - 2 );

  /* right bottom corner */
  _gtSetPos( uiTop, uiLeft );
  _gtWrite( &fpBoxString[ 7 ], 1 );

  _gtDispEnd();
#endif
  return 0;
}

ERRORCODE _gtBoxD( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight )
{
  return _gtBox( uiTop, uiLeft, uiBottom, uiRight, _B_DOUBLE );
}

ERRORCODE _gtBoxS( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight )
{
  return _gtBox( uiTop, uiLeft, uiBottom, uiRight, _B_SINGLE );
}

ERRORCODE _gtColorSelect( WORD uiColorIndex )
{
#ifdef __DOS__
  if ( uiColorIndex > 4 )
    return 1;

  suiAttrIndex = uiColorIndex;
#endif
  return 0;
}

ERRORCODE _gtDispBegin( void )
{
#ifdef __DOS__
  suiDispCount ++;
#endif
  return 0;
}

ERRORCODE _gtDispCount( void )
{
  return suiDispCount;
}

ERRORCODE _gtDispEnd( void )
{
#ifdef __DOS__
  suiDispCount --;

  if ( !suiDispCount )
    {
      _gtFlush();
      _gtUpdateCursor();
    }

#endif
  return 0;
}

ERRORCODE _gtFlush( void )
{
#ifdef __DOS__
  BYTE *fpVideoMemory;

  /* get address of video memory */
  
#endif
  return 0;
}

ERRORCODE _gtGetColorStr( BYTE *fpColorString )
{
  return 0;
}

ERRORCODE _gtGetCursor( WORD *uipCursorShape )
{
  return 0;
}

ERRORCODE _gtGetPos( WORD *uipRow, WORD *uipCol )
{
#ifdef __DOS__
  *uipRow = suiRow;
  *uipCol = suiCol;

#endif
  return 0;
}

BOOL _gtIsColor( void )
{
  return TRUE;
}

WORD _gtMaxCol( void )
{
  return suiMaxCol;
}

WORD _gtMaxRow( void )
{
  return suiMaxRow;
}

void _gtPostExt( void )
{
}

void _gtPreExt( void )
{
}

ERRORCODE _gtRectSize( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, WORD *uipBuffSize )
{
#ifdef __DOS__
  if ( uiTop > _gtMaxRow() || uiBottom > _gtMaxRow() ||
       uiLeft > _gtMaxCol() || uiRight > _gtMaxCol() ||
       uiTop > uiBottom || uiLeft > uiRight )
    return 1;

  *uipBuffSize = ( uiBottom - uiTop  + 1 ) * ( uiRight - uiLeft + 1 ) * 2;
#endif
  return 0;
}

ERRORCODE _gtRepChar( WORD uiRow, WORD uiCol, WORD uiChar, WORD uiCount )
{
#ifdef __DOS__
  char buff[ 255 ];

  memset( buff, uiChar, uiCount );
  buff[ uiCount ] = 0x0;
  _gtSetPos( uiRow, uiCol );
  _gtWrite( buff, uiCount );
#endif
  return 0;
}

ERRORCODE _gtRest( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, BYTE *fpScrBuff )
{
  return 0;
}

ERRORCODE _gtSave( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, BYTE *fpScrBuff )
{
  return 0;
}

ERRORCODE _gtScrDim( WORD *uipHeight, WORD *uipWidth )
{
#ifdef __DOS__
  *uipHeight = suiMaxRow;
  *uipWidth  = suiMaxCol;
#endif
  return 0;
}

ERRORCODE _gtScroll( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, SHORT iRows, SHORT iCols )
{
  return 0;
}

ERRORCODE _gtSetBlink( BOOL bBlink )
{
  return 0;
}

ERRORCODE _gtSetColorStr( BYTE *fpColorString )
{
  return 0;
}

ERRORCODE _gtSetCursor( WORD uiCursorShape )
{
  return 0;
}

ERRORCODE _gtSetMode( WORD uiRows, WORD uiCols )
{
#ifdef __DOS__
  if ( suiMaxRow != uiRows || suiMaxCol != uiCols )
    {
      suiMaxRow = uiRows;
      suiMaxCol = uiCols;

      /* change buffer */
    }

#endif
  return 0;
}

ERRORCODE _gtSetPos( WORD uiRow, WORD uiCol )
{
#ifdef __DOS__
  if ( uiRow > _gtMaxRow() || uiCol > _gtMaxCol() )
    return 1;

  suiRow = uiRow;
  suiCol = uiCol;
  _gtUpdateCursor();

#endif
  return 0;
}

ERRORCODE _gtSetSnowFlag( BOOL bNoSnow )
{
  return 0;
}

ERRORCODE _gtUpdateCursor( void )
{
#ifdef __DOS__
  union REGS regs;

  regs.h.ah = 2;  /* set cursor position */
  regs.h.bh = 0;
  regs.h.dh = suiRow;
  regs.h.dl = suiCol;

  int86( VIDEO_INT, &regs, &regs );

#endif
  return 0;
}

ERRORCODE _gtWrite( BYTE *fpStr, WORD uiLen )
{
#ifdef __DOS__
  int iOffset = suiRow * suiMaxRow + suiCol;
  int iCount;

  for ( iCount = 0; uiLen > 0; iCount ++, uiLen --, iOffset += 2 )
    {
      sfpScreenBuffer[ iOffset ] = fpStr[ iCount ];
      sfpScreenBuffer[ iOffset + 1 ] = suiAttribs[ suiAttrIndex ];
    }

  if ( !suiDispCount )
    _gtFlush();

#endif
  return 0;
}

ERRORCODE _gtWriteAt( WORD uiRow, WORD uiCol, BYTE *fpStr, WORD uiLen )
{
#ifdef __DOS__
  if ( !_gtSetPos( uiRow, uiCol ) )
    return _gtWrite( fpStr, uiLen );

#endif
  return 1;
}

ERRORCODE _gtWriteCon( BYTE *fpStr, WORD uiLen )
{
  return 0;
}

