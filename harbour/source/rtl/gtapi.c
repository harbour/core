/*
 * $Id$
 */

/*
 *  GTAPI.C: Generic Terminal for Harbour
 *
 * Latest mods:
 * 1.29   19990719   ptucker   Minor change to catch last color parameter
 *                             that may be empty
 * 1.28   19990719   ptucker   Added support for numeric color strings
 *                             like "1/7,8/15"
 * 1.26   19990719   ptucker   Changed call in hb_gtinit() to pass the
 *                             literal initial color setting in case
 *                             the GT system is initialised prior to Set.
 *                             Skipped color params in a string now keep
 *                             their previous value.  ie ",,,r/b"
 * 1.25   19990718   dholm     Moved calls to various gtFunctions out of
 *                             InitializeConsole() in console.c and put
 *                             them in hb_gtInit() in this module. Use
 *                             hb_set.HB_SET_COLOR to initialize the GT
 *                             API color string. Converted // comments.
 * 1.24   19990718   ptucker   corrected returned color strings so ordering
 *                             is the same as clipper.
 * 1.23   19990718   ptucker   implimented surface for gtGet/SetColorStr()
 *                             changed to allow unlimited color pairs.
 */

#include <set.h>
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

int *_Color;
int _ColorCount;

/* gt API functions */

void hb_gtInit(void)
{
    gtInit();
    hb_gtSetPos( gtWhereY(), gtWhereX() );
    _Color = (int *)hb_xgrab(5*sizeof(int));
    _ColorCount = 5;
    hb_gtSetColorStr( "W/N,N/W,N/N,N/N,N/W" );
/*  hb_gtSetColorStr( hb_set.HB_SET_COLOR ); */
}

void hb_gtExit(void)
{
    hb_xfree( _Color );
}

int hb_gtBox (USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char* pbyFrame)
{
    char pszBox [10];
    char cPadChar;

    USHORT uiRow = uiTop;
    USHORT uiCol = uiLeft;
    USHORT height, width, tmp;

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
    
    /* Ensure that box is drawn from top left to bottom right. */
    if( uiTop > uiBottom )
    {
       tmp = uiTop;
       uiTop = uiBottom;
       uiBottom = tmp;
    }
    if( uiLeft > uiRight )
    {
       tmp = uiLeft;
       uiLeft = uiRight;
       uiRight = tmp;
    }

    /* Draw the box or line as specified */
    height = uiBottom - uiTop + 1;
    width  = uiRight - uiLeft + 1;
    hb_gtDispBegin();

    if( height > 1 && width > 1 )
    {
       hb_gtWriteAt(uiRow, uiCol, pszBox + 0, sizeof(BYTE));
       hb_gtWriteAt(uiRow, uiRight, pszBox + 2, sizeof(BYTE));
       hb_gtWriteAt(uiBottom, uiCol, pszBox + 6, sizeof(BYTE));
       hb_gtWriteAt(uiBottom, uiRight, pszBox + 4, sizeof(BYTE));
    }

    for (uiCol = (height > 1 ? uiLeft + 1 : uiLeft); uiCol < (height > 1 ? uiRight : uiRight + 1 ); uiCol++)
    {
        hb_gtWriteAt(uiRow, uiCol, pszBox + 1, sizeof(BYTE));
        if( height > 1 ) hb_gtWriteAt(uiBottom, uiCol, pszBox + 5, sizeof(BYTE));
    }

    if( pszBox[8] && height > 2 && width > 2 )
    {
        for (uiRow = uiTop + 1; uiRow < uiBottom; uiRow++)
        {
            uiCol = uiLeft;
            hb_gtWriteAt(uiRow, uiCol++, pszBox + 7, sizeof(BYTE));
            while (uiCol < uiRight) hb_gtWriteAt(uiRow, uiCol++, pszBox + 8, sizeof(BYTE));
            hb_gtWriteAt(uiRow, uiCol, pszBox + 3, sizeof(BYTE));
        }
    }
    else
    {
        for( uiRow = (width > 1 ? uiTop + 1 : uiTop); uiRow < (width > 1 ? uiBottom : uiBottom + 1); uiRow++ )
        {
            hb_gtWriteAt(uiRow, uiLeft, pszBox + 7, sizeof(BYTE));
            if( width > 1 ) hb_gtWriteAt(uiRow, uiRight, pszBox + 3, sizeof(BYTE));
        }
    }

    hb_gtDispEnd();

    hb_gtSetPos(uiBottom + 1, uiRight + 1);

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
    if(uiColorIndex > _ColorCount )
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
    char c, buff[6];
    int nPos = 0, nSkip = 0, nCount=-1, i=0, y;
    int nBack = 0, nColor = 0, nHasX=0;

    do
    {
        if( ( c=*fpColorString++  ) > 'A' )
            c &= 0x5f;			/* convert to upper case */

        while( c <= '9' && c >= '0' && i < 6 )
        {
           if( i==0 )
              memset( buff, '\0', 6 );

           buff[i++] = c;
           c = *fpColorString++;
        }
        if( i )
        {
            i--;
            nColor = 0;
            /* TODO: this can probably be replaced with atoi() */
            /* ie: nColor = atoi(buff); */
            for( y=1; i+1; y *= 10, i-- )
            {
                if( buff[ i ] )
                    nColor += ( ( buff[i] - '0' ) * y );
            }
        
            i=0;
        }

        if( nSkip && !( c=='\0' || c==','  ))
            continue;

        nSkip = 0;
        ++nCount;
        switch (c) {
            case '/':
                nBack  |= nColor;
            case 'N':
                nColor  = 0;
                break;
            case 'B':
            case 'U':
                nColor |= 1;
                break;
            case 'G':
                nColor |= 2;
                break;
            case 'R':
                nColor |= 4;
                break;
            case 'W':
                nColor |= 7;
                break;
            case '+':
                nBack  |= 8;
                break;
            case '*':
                nBack  |= 128;
                break;
            case 'I':			/* =N/W */
                if( nPos == _ColorCount )
                {
                   _Color = (int *)hb_xrealloc( _Color, sizeof(int)*(nPos +1) );
                   ++ _ColorCount;
                }
                _Color[nPos++] = ( 112 | ( nBack & 136 ));
                nBack = nColor = 0;
                nSkip = 1;
                break;
            case 'X':			/* always sets forground to 'N' */
                nHasX = 1;
                break;
            case ',':
            case '\0':
                if(!nCount)
                   nBack = _Color[nPos];
                nCount = -1;
                if( nPos == _ColorCount )
                {
                   _Color = (int *)hb_xrealloc( _Color, sizeof(int)*(nPos +1) );
                   ++ _ColorCount;
                }
                if( nHasX )
                {
                   nBack &= 136;
                   nHasX = 0;
                }

                _Color[nPos++] = ( nColor << 4 ) | nBack;
                nColor=nBack=0;
        }
    }
    while( c );

    return(0);
}

int hb_gtGetColorStr(char * fpColorString)
{
    char *sColors;
    int i,j=0,k = 0, nColor;

    sColors = (char *)hb_xgrab( _ColorCount * 8 + 1 ); /* max possible */

    for( i=0; i<_ColorCount; i++ )
    {
        j = 0;
        nColor = _Color[i] & 7;
        do
        {
            if( nColor == 7 )
                sColors[k++] = 'W';
            else
            {
                if( nColor == 0 )
                    sColors[k++] = 'N';
                else
                {
                    if( nColor & 1 )
                        sColors[k++] = 'B';

                    if( nColor & 2 )
                        sColors[k++] = 'G';

                    if( nColor & 4 )
                        sColors[k++] = 'R';
                }
            }
            if( j == 0 )
            {
                if( _Color[i] & 8 )
                    sColors[k++] = '+';
                sColors[k++] = '/';
            }
            else
                if( _Color[i] & 128 )
                    sColors[k++] = '*';

            nColor = (_Color[i] >> 4) & 7;
        }
        while( ++j < 2 );
        if( i+1 < _ColorCount )
           sColors[k++] = ',';
    }
    sColors[k++] = '\0';

    strcpy( fpColorString, sColors );
    hb_xfree( sColors );

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
