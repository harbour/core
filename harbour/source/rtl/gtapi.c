/*
 * $Id$
 */

/*
 *  GTAPI.C: Generic Terminal for Harbour
 */

#include <gtapi.h>

/* TODO: functions not implemented yet
int hb_gtPostExt(void);
int hb_gtPreExt(void);
int hb_gtSetBlink(BOOL bBlink);
int hb_gtSetMode(USHORT uiRows, USHORT uiCols);
*/

static USHORT s_uiCurrentRow = 0;
static USHORT s_uiCurrentCol = 0;
static USHORT s_uiDispCount  = 0;
static USHORT s_uiColorIndex = 0;
static char   s_szColorStr[CLR_STRLEN] = {"W/N, N/W, N/N, N/N, N/W"};

/* gt API functions */

void hb_gtInit(void)
{
    gtInit();
}

int hb_gtBox (USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char* pbyFrame)
{
    char pszBox [10];
    char cPadChar;

    USHORT uiRow = uiTop;
    USHORT uiCol = uiLeft;
    USHORT tmp;

    if (uiTop > hb_gtMaxRow() || uiBottom > hb_gtMaxRow() ||
       uiLeft > hb_gtMaxCol() || uiRight > hb_gtMaxCol() ||
       uiTop > uiBottom || uiLeft > uiRight)
    {
        return 1;
    }

    /* For full compatibility, pad box string with last char if too short */

    cPadChar = ' ';
    for (tmp = 0; *pbyFrame && tmp < 9; tmp++) cPadChar = pszBox[tmp] = *pbyFrame++;
    while (tmp < 8) pszBox[tmp++] = cPadChar;
    pszBox[tmp] = '\0';

    hb_gtDispBegin();

    hb_gtWriteAt(uiRow, uiCol, pszBox + 0, sizeof(BYTE));
    hb_gtWriteAt(uiRow, uiRight, pszBox + 2, sizeof(BYTE));
    hb_gtWriteAt(uiBottom, uiCol, pszBox + 6, sizeof(BYTE));
    hb_gtWriteAt(uiBottom, uiRight, pszBox + 4, sizeof(BYTE));

    for (tmp = uiCol + 1; tmp < uiRight; tmp++)
    {
        hb_gtWriteAt(uiRow, tmp, pszBox + 1, sizeof(BYTE));
        hb_gtWriteAt(uiBottom, tmp, pszBox + 5, sizeof(BYTE));
    }

    if (pszBox[8])
    {
        for (++uiRow; uiRow < uiBottom; uiRow++)
        {
            tmp = uiCol;
            hb_gtWriteAt(uiRow, tmp++, pszBox + 7, sizeof(BYTE));
            while (tmp < uiRight) hb_gtWriteAt(uiRow, tmp++, pszBox + 8, sizeof(BYTE));
            hb_gtWriteAt(uiRow, tmp, pszBox + 3, sizeof(BYTE));
        }
    }
    else
    {
        for (++uiRow; uiRow < uiBottom; uiRow++)
        {
            hb_gtWriteAt(uiRow, uiCol, pszBox + 7, sizeof(BYTE));
            hb_gtWriteAt(uiRow, uiRight, pszBox + 3, sizeof(BYTE));
        }
    }

    hb_gtDispEnd();

    hb_gtSetPos(uiTop + 1, uiLeft + 1);

    return 0;
}

int hb_gtBoxD(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight)
{
    return(hb_gtBox(uiTop, uiLeft, uiBottom, uiRight, B_DOUBLE));
}

int hb_gtBoxS(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight)
{
    return(hb_gtBox(uiTop, uiLeft, uiBottom, uiRight, B_SINGLE));
}

int hb_gtColorSelect(USHORT uiColorIndex)
{
    if(uiColorIndex > CLR_LASTCOLOR)
    {
        return(1);
    }
    else
    {
        s_uiColorIndex = uiColorIndex;
    }

    return(0);
}

int hb_gtDispBegin(void)
{
    /* TODO: need to add buffering here */
    s_uiDispCount++;
    return(0);
}

USHORT hb_gtDispCount(void)
{
    return(s_uiDispCount);
}

int hb_gtDispEnd(void)
{
    /* TODO: need to add buffering here */
    s_uiDispCount--;
    return(0);
}

int hb_gtSetColorStr(char * fpColorString)
{
    if (strlen(fpColorString) > CLR_STRLEN)
    {
        return(1);
    }
    else
    {
        strcpy(s_szColorStr, fpColorString);
    }

    return(0);
}

int hb_gtGetColorStr(char * fpColorString)
{
    if (fpColorString)
    {
        strcpy(fpColorString, s_szColorStr);
    }
    else
    {
        return(1);
    }

    return(0);
}

int hb_gtGetCursor(USHORT * uipCursorShape)
{
    int i=gtGetCursorStyle();
    int rc=0;

    if(i <= SC_SPECIAL2)
    {
        *uipCursorShape = i;
    }
    else
    {
        rc=i;
    }

    return(rc);
}

int hb_gtGetPos(USHORT * uipRow, USHORT * uipCol)
{
    *uipRow = s_uiCurrentRow;
    *uipCol = s_uiCurrentCol;

    return(0);
}

BOOL hb_gtIsColor(void)
{
    /* TODO: need to call something to do this instead of returning TRUE */
    return(TRUE);
}

USHORT hb_gtMaxCol(void)
{
    return(gtGetScreenWidth() - 1);
}

USHORT hb_gtMaxRow(void)
{
    return(gtGetScreenHeight() - 1);
}

int hb_gtPostExt(void)
{
    return(0);
}

int hb_gtPreExt(void)
{
    return(0);
}

int hb_gtRectSize(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * uipBuffSize)
{
    if(uiTop > hb_gtMaxRow() || uiBottom > hb_gtMaxRow() ||
        uiLeft > hb_gtMaxCol() || uiRight > hb_gtMaxCol() ||
        uiTop > uiBottom || uiLeft > uiRight)
    {
        return(1);
    }

    *uipBuffSize = (uiBottom - uiTop) * (uiRight - uiLeft) * 2;

    return(0);
}

int hb_gtRepChar(USHORT uiRow, USHORT uiCol, USHORT uiChar, USHORT uiCount)
{
    int rc;
    char buff[255];

    memset(buff, uiChar, uiCount);
    buff[uiCount] = 0x0;
    rc=hb_gtSetPos(uiRow, uiCol);
    if(rc != 0)
        return(rc);
    rc=hb_gtWrite(buff, uiCount);

    return(rc);
}

int hb_gtRest(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff)
{
    gtPutText(uiLeft, uiTop, uiRight, uiBottom, vlpScrBuff);
    return(0);
}

int hb_gtSave(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff)
{
    USHORT bufsiz;
    int rc;

    rc=hb_gtRectSize(uiTop, uiLeft, uiBottom, uiRight, &bufsiz);
    if(rc != 0)
    {
        return(rc);
    }

    gtGetText(uiLeft, uiTop, uiRight, uiBottom, vlpScrBuff);

    return(0);
}

int hb_gtScrDim(USHORT * uipHeight, USHORT * uipWidth)
{
    *uipHeight = hb_gtMaxRow();
    *uipWidth = hb_gtMaxCol();

    return(0);
}

int hb_gtSetBlink(BOOL bBlink)
{
   HB_SYMBOL_UNUSED( bBlink );
    return(0);
}

int hb_gtSetCursor(USHORT uiCursorShape)
{
    gtSetCursorStyle(uiCursorShape);
    return(0);
}

int hb_gtSetMode(USHORT uiRows, USHORT uiCols)
{
   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );
    return(0);
}

int hb_gtSetPos(USHORT uiRow, USHORT uiCol)
{
    if(uiRow > hb_gtMaxRow() || uiCol > hb_gtMaxCol())
        return(1);

    s_uiCurrentRow = uiRow;
    s_uiCurrentCol = uiCol;

    gtGotoXY(uiCol, uiRow);

    return(0);
}

int hb_gtSetSnowFlag(BOOL bNoSnow)
{
   HB_SYMBOL_UNUSED( bNoSnow );
    return(0);
}

int hb_gtWrite(char * fpStr, ULONG length)
{
    /* TODO: need to get current color setting from s_szColorString and
             s_uiColorIndex
    */
    int iRow, iCol, iMaxCol, iMaxRow, iTemp;
    ULONG size;
    char attr=7, *fpPointer = fpStr;

    /* Determine where the cursor is going to end up */
    iRow = s_uiCurrentRow;
    iCol = s_uiCurrentCol;
    iMaxCol = hb_gtMaxCol();
    iMaxRow = hb_gtMaxRow();
    size = length;
    if (iCol + size > iMaxCol)
    {
       /* Calculate eventual row position and the remainder size for the column adjust */
       iRow += (size / (iMaxCol + 1));
       size = size % (iMaxCol + 1);
    }
    iCol += size;
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
       hb_gtScroll(0, 0, iMaxRow, iMaxCol, iRow - iMaxRow, 0);
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
       else size = length;

       /* Save the new starting row and the new ending row */
       s_uiCurrentRow = iTemp;
       iRow = iMaxRow;
    }
    else size = length;

    /* Now the text string can be displayed */
    gtPuts(s_uiCurrentCol, s_uiCurrentRow, attr, fpPointer, size);

    /* Finally, save the new cursor position */
    hb_gtSetPos (iRow, iCol);

    return(0);
}

int hb_gtWriteAt(USHORT uiRow, USHORT uiCol, char * fpStr, ULONG length)
{
    int rc;

    if((rc=hb_gtSetPos(uiRow, uiCol)) != 0)
        return(rc);

    return(hb_gtWrite(fpStr, length));
}

int hb_gtWriteCon(char * fpStr, ULONG length)
{
    int rc = 0;
    USHORT uiRow = s_uiCurrentRow, uiCol = s_uiCurrentCol;
    ULONG count;
    char ch[2];
    char * fpPtr = fpStr;

    ch[1] = 0;
    for(count = 0; count < length; count++)
    {
       ch [0] = *fpPtr++;
       switch(ch [0])
       {
          case 7:
             break;
          case 8:
             if(uiRow > 0) uiCol--;
             else if(uiRow > 0)
             {
                uiRow--;
                uiCol=hb_gtMaxCol();
             }
             else
             {
                hb_gtScroll(0, 0, hb_gtMaxRow(), hb_gtMaxCol(), -1, 0);
                uiCol=hb_gtMaxCol();
             }
             hb_gtSetPos (uiRow, uiCol);
             break;
          case 10:
             if(uiRow < hb_gtMaxRow()) uiRow++;
             else
             {
                hb_gtScroll(0, 0, hb_gtMaxRow(), hb_gtMaxCol(), 1, 0);
             }
             hb_gtSetPos (uiRow, uiCol);
             break;
          case 13:
             uiCol = 0;
             hb_gtSetPos (uiRow, uiCol);
             break;
          default:
             rc = hb_gtWrite(ch, 1);
             hb_gtGetPos (&uiRow, &uiCol);
       }
       if(rc)
          return(rc);
    }
    return(0);
}

int hb_gtScroll(USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols)
{
   USHORT uiRow = s_uiCurrentRow, uiCol = s_uiCurrentCol, uiSize;
   int iLength = (uiRight - uiLeft) + 1;
   int iCount, iColOld, iColNew, iColSize;

   if (hb_gtRectSize (uiTop, uiLeft, uiBottom, uiRight, &uiSize) == 0)
   {
      char * fpBlank = (char *)hb_xgrab (iLength);
      char * fpBuff = (char *)hb_xgrab (iLength * 2);
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
      if (fpBlank) hb_xfree (fpBlank);
      if (fpBuff) hb_xfree (fpBuff);
   }
   s_uiCurrentRow = uiRow;
   s_uiCurrentCol = uiCol;
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

    hb_gtRectSize(1, 1, hb_gtMaxRow(), hb_gtMaxCol(), &size);
    scr=(char *)malloc(size);
    hb_gtSave(1, 1, hb_gtMaxRow()-1, hb_gtMaxCol()-1, scr);
    */

    /* writing text */
    hb_gtSetPos(3, 3);
    hb_gtWrite(test, strlen(test));
    hb_gtSetPos(12, 42);
    hb_gtWrite(test, strlen(test));

    /* wrapping text */
    hb_gtSetPos(7, 70);
    hb_gtWrite(test2, strlen(test2));

    /* writing color text (not implemented yet) */
    /*
    hb_gtSetColorStr( "W+/B, B/W" );
    hb_gtColorSelect(_CLR_STANDARD);
    hb_gtWrite( "Enhanced color (B/W)", 20 );
    hb_gtSetPos(22, 62);
    hb_gtColorSelect(_CLR_ENHANCED);
    hb_gtWrite( "Standard Color (W+/B)", 21 );
    */

    /* boxes */
    hb_gtBoxS(10, 10, 20, 20);
    hb_gtBoxD(10, 40, 15, 45);

    /* cursor functions */
    hb_gtSetPos(12, 1);

    /* none */
    hb_gtSetCursor(_SC_NONE);
    getch();

    /* underline */
    hb_gtSetCursor(_SC_NORMAL);
    getch();

    /* lower half block */
    hb_gtSetCursor(_SC_INSERT);
    getch();

    /* full block */
    hb_gtSetCursor(_SC_SPECIAL1);
    getch();

    /* upper half block */
    hb_gtSetCursor(_SC_SPECIAL2);
    getch();

    /* restore screen (doesn't work under DOS) */
    /*
    hb_gtRest(1, 1, hb_gtMaxRow()-1, hb_gtMaxCol()-1, scr);
    free(scr);
    */
}
#endif
