#ifndef __GTAPI_H__
#define __GTAPI_H__

#include <extend.h>

typedef WORD ERRORCODE;

ERRORCODE _gtBox( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, BYTE *fpBoxString );
ERRORCODE _gtBoxD( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight );
ERRORCODE _gtBoxS( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight );
ERRORCODE _gtColorSelect( WORD uiColorIndex );
ERRORCODE _gtDispBegin( void );
ERRORCODE _gtDispCount( void );
ERRORCODE _gtDispEnd( void );
ERRORCODE _gtFlush( void );
ERRORCODE _gtGetColorStr( BYTE *fpColorString );
ERRORCODE _gtGetCursor( WORD *uipCursorShape );
ERRORCODE _gtGetPos( WORD *uipRow, WORD *uipCol );
BOOL      _gtIsColor( void );
WORD      _gtMaxCol( void );
WORD      _gtMaxRow( void );
void      _gtPostExt( void );
void      _gtPreExt( void );
ERRORCODE _gtRectSize( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, WORD *uipBuffSize );
ERRORCODE _gtRepChar( WORD uiRow, WORD uiCol, WORD uiChar, WORD uiCount );
ERRORCODE _gtRest( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, BYTE *fpScrBuff );
ERRORCODE _gtSave( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, BYTE *fpScrBuff );
ERRORCODE _gtScrDim( WORD *uipHeight, WORD *uiLpWidth );
ERRORCODE _gtScroll( WORD uiTop, WORD uiLeft, WORD uiBottom, WORD uiRight, SHORT iRows, SHORT iCols );
ERRORCODE _gtSetBlink( BOOL bBlink );
ERRORCODE _gtSetColorStr( BYTE *fpColorString );
ERRORCODE _gtSetCursor( WORD uiCursorShape );
ERRORCODE _gtSetMode( WORD uiRows, WORD uiCols );
ERRORCODE _gtSetPos( WORD uiRow, WORD uiCol );
ERRORCODE _gtSetSnowFlag( BOOL bNoSnow );
ERRORCODE _gtUpdateCursor( void );
ERRORCODE _gtWrite( BYTE *fpStr, WORD uiLen );
ERRORCODE _gtWriteAt( WORD uiRow, WORD uiCol, BYTE *fpStr, WORD uiLen );
ERRORCODE _gtWriteCon( BYTE *fpStr, WORD uiLen );


#define _B_SINGLE          "ÚÄ¿³ÙÄÀ³"
#define _B_DOUBLE          "ÉÍ»º¼ÍÈº"
#define _B_SINGLE_DOUBLE   "ÖÄ·º½ÄÓº"
#define _B_DOUBLE_SINGLE   "ÕÍ¸³¾ÍÔ³"

#endif

