/*
 * $Id$
 */

/*
 *  GTAPI.C: Generic Terminal for Harbour
 */

#include <extend.h>
#include <gtapi.h>

/* TODO: functions not implemented yet
int _gtPostExt(void);
int _gtPreExt(void);
int _gtScroll(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols);
int _gtSetBlink(BOOL bBlink);
int _gtSetMode(USHORT uiRows, USHORT uiCols);
*/

static USHORT s_uiCurrentRow = 0;
static USHORT s_uiCurrentCol = 0;
static USHORT s_uiDispCount  = 0;
static USHORT s_uiColorIndex = 0;
static char   s_szColorStr[CLR_STRLEN] = {"W/N, N/W, N/N, N/N, N/W"};

/* Harbour functions */

HARBOUR ROW()
{
    _retni(s_uiCurrentRow);
}

HARBOUR COL()
{
    _retni(s_uiCurrentCol);
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
    return(gtGetScreenWidth() - 1);
}

USHORT _gtMaxRow(void)
{
    return(gtGetScreenHeight() - 1);
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
    gtPutText(uiLeft, uiTop, uiRight, uiBottom, vlpScrBuff);
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

    gtGetText(uiLeft, uiTop, uiRight, uiBottom, vlpScrBuff);

    return(0);
}

int _gtScrDim(USHORT * uipHeight, USHORT * uipWidth)
{
    *uipHeight = _gtMaxRow();
    *uipWidth = _gtMaxCol();

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
    int iRow, iCol, iMaxCol, iMaxRow, iSize, iTemp;
    char attr=7, *fpPointer = fpStr;

    /* Determine where the cursor is going to end up */
    iRow = s_uiCurrentRow;
    iCol = s_uiCurrentCol;
    iMaxCol = _gtMaxCol();
    iMaxRow = _gtMaxRow();
    iSize = uiLen;
    if (iCol + iSize > iMaxCol)
    {
       /* Calculate eventual row position and the remainder size for the column adjust */
       iRow += (iSize / (iMaxCol + 1));
       iSize = iSize % (iMaxCol + 1);
    }
    iCol += iSize;
    if (iCol > iMaxCol)
    {
       /* Column movement overflows onto next row */
       iRow++;
       iCol -= (iMaxCol + 1);
    }
    /* If needed, prescroll the display to the new position and adjust the current row
       position to account for the prescroll */
    if (iRow > iMaxRow)
    {
       _gtScroll(0, 0, iMaxRow, iMaxCol, iRow - iMaxRow, 0);
       iTemp = s_uiCurrentRow - (iRow - iMaxRow);
       if (iTemp < 0)
       {
          /* The string is too long to fit on the screen. Only display part of it. */
          fpPointer += iMaxCol * abs (iTemp);
          iTemp = 0;
          if (s_uiCurrentCol > 0)
          {
             /* Ensure that the truncated text will fill the screen */
             fpPointer -= s_uiCurrentCol;
             s_uiCurrentCol = 0;
          }
       }
       else iSize = uiLen;

       /* Save the new starting row and the new ending row */
       s_uiCurrentRow = iTemp;
       iRow = iMaxRow;
    }
    else iSize = uiLen;

    /* Now the text string can be displayed */
    gtPuts(s_uiCurrentCol, s_uiCurrentRow, attr, fpPointer, iSize);

    /* Finally, save the new cursor position */
    _gtSetPos (iRow, iCol);

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
    int rc = 0;
    USHORT uiCount, uiRow = s_uiCurrentRow, uiCol = s_uiCurrentCol;
    char ch[2];

    ch[1] = 0;
    for(uiCount = 0; uiCount < uiLen; uiCount++)
    {
       ch [0] = fpStr[uiCount];
       switch(ch [0])
       {
          case 7:
             break;
          case 8:
             if(uiRow > 0) uiCol--;
             else if(uiRow > 0)
             {
                uiRow--;
                uiCol=_gtMaxCol();
             }
             else
             {
                _gtScroll(0, 0, _gtMaxRow(), _gtMaxCol(), -1, 0);
                uiCol=_gtMaxCol();
             }
             _gtSetPos (uiRow, uiCol);
             break;
          case 10:
             if(uiRow < _gtMaxRow()) uiRow++;
             else
             {
                _gtScroll(0, 0, _gtMaxRow(), _gtMaxCol(), 1, 0);
             }
             _gtSetPos (uiRow, uiCol);
             break;
          case 13:
             uiCol = 0;
             _gtSetPos (uiRow, uiCol);
             break;
          default:
             rc = _gtWrite(ch, 1);
             _gtGetPos (&uiRow, &uiCol);
       }
       if(rc)
          return(rc);
    }
    return(0);
}

int _gtScroll(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols)
{
   USHORT uiRow = s_uiCurrentRow, uiCol = s_uiCurrentCol, uiSize;
   int iLength = (uiRight - uiLeft) + 1;
   int iCount, iColOld, iColNew, iColSize;

   if (_gtRectSize (uiTop, uiLeft, uiBottom, uiRight, &uiSize) == 0)
   {
      char * fpBlank = (char *)_xgrab (iLength);
      char * fpBuff = (char *)_xgrab (iLength * 2);
      if (fpBlank && fpBuff)
      {
         for (iCount = 0; iCount < iLength; iCount++)
            fpBlank [iCount] = 0;
         if (iCols >= 0)
         {
            iColOld = iColNew = uiLeft;
            iColOld += iCols;
            iColSize = uiRight - uiLeft;
            iColSize -= iCols;
         }
         else
         {
            iColOld = iColNew = uiLeft;
            iColNew -= iCols;
            iColSize = uiRight - uiLeft;
            iColSize += iCols;
         }
         for (iCount = (iRows >= 0 ? uiTop : uiBottom);
              (iRows >= 0 ? iCount <= uiBottom : iCount >= uiTop);
              (iRows >= 0 ? iCount++ : iCount--))
         {
             /* TODO: need to get current color setting from s_szColorString and
                      s_uiColorIndex
             */
            int iRowPos = iCount + iRows;
            char attr=7;
            /* Blank the scroll region in the current row */
            gtPuts (uiLeft, iCount, attr, fpBlank, iLength);

            if ((iRows || iCols) && iRowPos <= uiBottom && iRowPos >= uiTop)
            {
               /* Read the text to be scrolled into the current row */
               gtGetText (iColOld, iRowPos, iColOld + iColSize, iRowPos, fpBuff);

               /* Write the scrolled text to the current row */
               gtPutText (iColNew, iCount, iColNew + iColSize, iCount, fpBuff);
            }
         }
      }
      if (fpBlank) _xfree (fpBlank);
      if (fpBuff) _xfree (fpBuff);
   }
   s_uiCurrentRow = uiRow;
   s_uiCurrentCol = uiCol;
   return(0);
}

HARBOUR MAXROW( void ) /* Return the maximum screen row number (zero origin) */
{
   _retni( _gtMaxRow () );
}

HARBOUR MAXCOL( void ) /* Return the maximum screen column number (zero origin) */
{
   _retni( _gtMaxCol () );
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
    */

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
