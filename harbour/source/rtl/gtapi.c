/*
 *  GTAPI.C: Generic Terminal for Harbour
 */

#include <types.h>
#include <gtapi.h>

/* TODO: functions not implemented yet
int _gtPostExt(void);
int _gtPreExt(void);
int _gtScroll(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols);
int _gtSetBlink(BOOL bBlink);
int _gtSetMode(USHORT uiRows, USHORT uiCols);
*/

static USHORT s_uiCurrentRow = 1;
static USHORT s_uiCurrentCol = 1;
static USHORT s_uiDispCount  = 0;
static USHORT s_uiColorIndex = 0;
static char   s_szColorStr[CLR_STRLEN] = {"W/N, N/W, N/N, N/N, N/W"};

/* Harbour functions */

HARBOUR ROW()
{
    _reti(s_uiCurrentRow);
}

HARBOUR COL()
{
    _reti(s_uiCurrentCol);
}

/* gt API functions */

void _gtInit(void)
{
    gtInit();
}

int _gtBox(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * fpBoxString)
{
    int iCount;

    if(uiTop > _gtMaxRow() || uiBottom > _gtMaxRow() ||
        uiLeft > _gtMaxCol() || uiRight > _gtMaxCol() ||
        uiTop > uiBottom || uiLeft > uiRight)
    {
        return(1);
    }

    _gtDispBegin();

    /* upper left corner */
    _gtSetPos(uiTop, uiLeft);
    _gtWrite(&fpBoxString[0], 1);

    /* top line */
    _gtRepChar(uiTop, uiLeft+1, fpBoxString[1], uiRight-uiLeft-1);

    /* upper right corner */
    _gtSetPos(uiTop, uiRight);
    _gtWrite(&fpBoxString[2], 1);

    /* left and right sides */
    for(iCount = uiTop + 1; iCount<uiBottom; iCount++)
    {
	_gtSetPos(iCount, uiLeft);
	_gtWrite(&fpBoxString[3], 1);

	_gtSetPos(iCount, uiRight);
	_gtWrite(&fpBoxString[3], 1);
    }

    /* left bottom corner */
    _gtSetPos(uiBottom, uiLeft);
    _gtWrite(&fpBoxString[6], 1);

    /* bottom line */
    _gtRepChar(uiBottom, uiLeft+1, fpBoxString[5], uiRight-uiLeft-1);

    /* right bottom corner */
    _gtSetPos(uiBottom, uiRight);
    _gtWrite(&fpBoxString[4], 1);

    _gtDispEnd();

    return(0);
}

int _gtBoxD(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight)
{
    return(_gtBox(uiTop, uiLeft, uiBottom, uiRight, _B_DOUBLE));
}

int _gtBoxS(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight)
{
    return(_gtBox(uiTop, uiLeft, uiBottom, uiRight, _B_SINGLE));
}

int _gtColorSelect(USHORT uiColorIndex)
{
    if(uiColorIndex > _CLR_LASTCOLOR)
    {
        return(1);
    }
    else
    {
	s_uiColorIndex = uiColorIndex;
    }

    return(0);
}

int _gtDispBegin(void)
{
    /* TODO: need to add buffering here */
    s_uiDispCount++;
    return(0);
}

USHORT _gtDispCount(void)
{
    return(s_uiDispCount);
}

int _gtDispEnd(void)
{
    /* TODO: need to add buffering here */
    s_uiDispCount--;
    return(0);
}

int _gtGetColorStr(char * fpColorString)
{
    int iDestLen = strlen(fpColorString);
    int iSrcLen = strlen(s_szColorStr);

    if(iSrcLen > iDestLen)
    {
        return(1);
    }
    else
    {
        strncpy(fpColorString, s_szColorStr, iSrcLen);
    }

    return(0);
}

int _gtGetCursor(USHORT * uipCursorShape)
{
    int i=gtGetCursorStyle();
    int rc=0;

    if(i <= _SC_SPECIAL2)
    {
        *uipCursorShape = i;
    }
    else
    {
        rc=i;
    }

    return(rc);
}

int _gtGetPos(USHORT * uipRow, USHORT * uipCol)
{
    *uipRow = s_uiCurrentRow;
    *uipCol = s_uiCurrentCol;

    return(0);
}

BOOL _gtIsColor(void)
{
    /* TODO: need to call something to do this instead of returning TRUE */
    return(TRUE);
}

USHORT _gtMaxCol(void)
{
    return(gtGetScreenWidth());
}

USHORT _gtMaxRow(void)
{
    return(gtGetScreenHeight());
}

int _gtPostExt(void)
{
    return(0);
}

int _gtPreExt(void)
{
    return(0);
}

int _gtRectSize(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * uipBuffSize)
{
    if(uiTop > _gtMaxRow() || uiBottom > _gtMaxRow() ||
	uiLeft > _gtMaxCol() || uiRight > _gtMaxCol() ||
	uiTop > uiBottom || uiLeft > uiRight)
    {
	return(1);
    }

    *uipBuffSize = (uiBottom - uiTop) * (uiRight - uiLeft) * 2;

    return(0);
}

int _gtRepChar(USHORT uiRow, USHORT uiCol, USHORT uiChar, USHORT uiCount)
{
    int rc;
    char buff[255];

    memset(buff, uiChar, uiCount);
    buff[uiCount] = 0x0;
    rc=_gtSetPos(uiRow, uiCol);
    if(rc != 0)
	return(rc);
    rc=_gtWrite(buff, uiCount);

    return(rc);
}

int _gtRest(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff)
{
    gtPutText(uiLeft, uiTop, uiRight - uiLeft, uiBottom - uiTop, vlpScrBuff);
    return(0);
}

int _gtSave(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff)
{
    USHORT bufsiz;
    int rc;

    rc=_gtRectSize(uiTop, uiLeft, uiBottom, uiRight, &bufsiz);
    if(rc != 0)
    {
        return(rc);
    }

    if(strlen(vlpScrBuff) < bufsiz)
    {
        return(1);
    }

    gtGetText(uiLeft, uiTop, uiRight - uiLeft, uiBottom - uiTop, vlpScrBuff);

    return(0);
}

int _gtScrDim(USHORT * uipHeight, USHORT * uipWidth)
{
    *uipHeight = _gtMaxRow();
    *uipWidth = _gtMaxCol();

    return(0);
}

int _gtScroll(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols)
{
    return(0);
}

int _gtSetBlink(BOOL bBlink)
{
    return(0);
}

int _gtSetColorStr(char * fpColorString)
{
    int iLength = strlen(fpColorString);

    if(iLength > CLR_STRLEN)
    {
        return(1);
    }
    else
    {
        strncpy(s_szColorStr, fpColorString, iLength);
    }

    return(0);
}

int _gtSetCursor(USHORT uiCursorShape)
{
    gtSetCursorStyle(uiCursorShape);
    return(0);
}

int _gtSetMode(USHORT uiRows, USHORT uiCols)
{
    return(0);
}

int _gtSetPos(USHORT uiRow, USHORT uiCol)
{
    if(uiRow > _gtMaxRow() || uiCol > _gtMaxCol())
	return(1);

    s_uiCurrentRow = uiRow;
    s_uiCurrentCol = uiCol;

    gtGotoXY(uiCol, uiRow);

    return(0);
}

int _gtSetSnowFlag(BOOL bNoSnow)
{
    return(0);
}

int _gtWrite(char * fpStr, USHORT uiLen)
{
    /* TODO: need to get current color setting from s_szColorString and
             s_uiColorIndex
    */
    int iRow, iCol, iMaxCol;
    char attr=7;

    gtPuts(s_uiCurrentCol, s_uiCurrentRow, attr, fpStr, uiLen);

    iRow = s_uiCurrentRow;
    iCol = s_uiCurrentCol;
    iMaxCol = _gtMaxCol();

    if(iCol + uiLen > iMaxCol)
    {
        s_uiCurrentCol=(iRow + uiLen) - iMaxCol;
        s_uiCurrentRow++;
    }
    else
    {
	s_uiCurrentCol += uiLen;
    }

    return(0);
}

int _gtWriteAt(USHORT uiRow, USHORT uiCol, char * fpStr, USHORT uiLen)
{
    int rc;

    if((rc=_gtSetPos(uiRow, uiCol)) != 0)
        return(rc);

    return(_gtWrite(fpStr, uiLen));
}

int _gtWriteCon(char * fpStr, USHORT uiLen)
{
    int rc;

    if((rc=_gtWrite(fpStr, uiLen)) != 0)
	return(rc);

    return(0);
}

#ifdef TEST
void main(void)
{
    char *test="Testing GT API Functions";
    char *test2="This message wraps!";
    int iRow, iCol;

    /* NOTE: always have to initialze video subsystem */
    gtInit();

    /* save screen (doesn't work under DOS) */
    /*
    char *scr;
    USHORT size;

    _gtRectSize(1, 1, _gtMaxRow(), _gtMaxCol(), &size);
    scr=(char *)malloc(size);
    _gtSave(1, 1, _gtMaxRow()-1, _gtMaxCol()-1, scr);
    */

    /* writing text */
    _gtSetPos(3, 3);
    _gtWrite(test, strlen(test));
    _gtSetPos(12, 42);
    _gtWrite(test, strlen(test));

    /* wrapping text */
    _gtSetPos(7, 70);
    _gtWrite(test2, strlen(test2));

    /* writing color text (not implemented yet) */
    /*
    _gtSetColorStr( "W+/B, B/W" );
    _gtColorSelect(_CLR_STANDARD);
    _gtWrite( "Enhanced color (B/W)", 20 );
    _gtSetPos(22, 62);
    _gtColorSelect(_CLR_ENHANCED);
    _gtWrite( "Standard Color (W+/B)", 21 );

    /* boxes */
    _gtBoxS(10, 10, 20, 20);
    _gtBoxD(10, 40, 15, 45);

    /* cursor functions */
    _gtSetPos(12, 1);

    /* none */
    _gtSetCursor(_SC_NONE);
    getch();

    /* underline */
    _gtSetCursor(_SC_NORMAL);
    getch();

    /* lower half block */
    _gtSetCursor(_SC_INSERT);
    getch();

    /* full block */
    _gtSetCursor(_SC_SPECIAL1);
    getch();

    /* upper half block */
    _gtSetCursor(_SC_SPECIAL2);
    getch();

    /* restore screen (doesn't work under DOS) */
    /*
    _gtRest(1, 1, _gtMaxRow()-1, _gtMaxCol()-1, scr);
    free(scr);
    */
}
#endif
