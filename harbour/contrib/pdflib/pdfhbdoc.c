/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PDF low level Api for HBDOC
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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


#include <hbapi.h>
#include <hbapiitm.h>
#include "pdflib.h"

PDF *szPDFFile;
#define FONTBOLD "Courier-Bold"
#define FONTNORMAL "Courier"
#define FONTSIZE 10.0/*10.0*/
#define FONTSIZEBOLD 15.0
#define FONTSIZESMALL 10.0     /*10.0*/
#define FONTSIZETABLE 8.5
#define LEAD ((int) (FONTSIZESMALL * 1.0))
#define LEADLINK ((int) (FONTSIZESMALL * 1.3))
#define LEADTABLE ((int) (FONTSIZESMALL * 1.2))
static float iRow=800;
static float fOldPos;
static float iWidth;
static float iCol;
static int sziFontBold=0;
static int sziFont=0;
static int iPage=1;
static ULONG uiLen;
static ULONG uiCount;
static BOOL bTItems;
static BOOL bFItems;
PHB_ITEM pArray=NULL;
static float hb_checkStringWidth(const char *szString);
static float hb_pdfGetHeight(const char *szText);
static void hb_ProcessTableItem(PHB_ITEM p1,PHB_ITEM p2,PHB_ITEM p3,PHB_ITEM p4);
float getText(PDF *p,const char *szText,int iFont,float irow,float icol,float iw);
void setText(PDF *p,const char *szText,float irow,float icol,float h,float iw);

HB_FUNC(HB_PDFNEW)
{
const char *szFileResult;
szFileResult= (char *) hb_parc(1);
if (szPDFFile ==NULL) {
    szPDFFile=PDF_new();
    PDF_set_value(szPDFFile,"compress", (float) 9);
}

if (PDF_open_file(szPDFFile, szFileResult) == -1) {
    hb_retl(1);
    }
    PDF_set_info(szPDFFile, "Keywords", "Harbour compiler");
    PDF_set_info(szPDFFile, "Subject", "Harbour");
    PDF_set_info(szPDFFile, "Title", "Harbour doc guide");
    PDF_set_info(szPDFFile, "Creator", "HBDOC Harbour Document Extractor");
    PDF_set_info(szPDFFile, "Author", "Luiz Rafael Culik");
    sziFont = PDF_findfont(szPDFFile, FONTNORMAL, "host", 0);
    sziFontBold = PDF_findfont(szPDFFile, FONTBOLD, "host", 0);
    hb_retl(0);
}

HB_FUNC(HB_PDFNEWPAGE)
{
    const char *szTitleT;
    szTitleT= (char *) hb_parc(1);


    PDF_begin_page(szPDFFile, a4_width, a4_height); /* start a new page */
    PDF_setfont(szPDFFile, sziFontBold, FONTSIZEBOLD);
    PDF_show_xy(szPDFFile, szTitleT,50,iRow-=LEAD);
//    PDF_show_xy(szPDFFile, szKeyWordT, 50, iRow-=LEAD);
}

HB_FUNC(HB_PDFENDPAGE)
{
    iRow=800;
    PDF_end_page(szPDFFile);                /* close page       */
    iPage++;
}

HB_FUNC(HB_PDFWRITEBOLDTEXT)
{
    const char *szTextT;


    szTextT=(char *) hb_parc(1);

    if (iRow<=40) {
        iRow=800;
        PDF_end_page(szPDFFile);
        sziFont = PDF_findfont(szPDFFile, FONTNORMAL, "host", 0);
        sziFontBold = PDF_findfont(szPDFFile, FONTBOLD, "host", 0);
        PDF_begin_page(szPDFFile, a4_width, a4_height); /* start a new page */
        iPage++;
}
          PDF_setfont(szPDFFile, sziFontBold, FONTSIZESMALL);

    PDF_show_xy(szPDFFile, szTextT,25, iRow-=LEAD);
    PDF_setfont(szPDFFile, sziFont, FONTSIZESMALL);
 }
HB_FUNC(HB_PDFWRITEBOLDTEXT1)
{
    const char *szTextT;
    szTextT=(char *) hb_parc(1);
    if (iRow<=40) {

        iRow=800;
        PDF_end_page(szPDFFile);
        PDF_begin_page(szPDFFile, a4_width, a4_height); /* start a new page */
          sziFont = PDF_findfont(szPDFFile, FONTNORMAL, "host", 0);
          sziFontBold = PDF_findfont(szPDFFile, FONTBOLD, "host", 0);  
        iPage++;
}
          PDF_setfont(szPDFFile, sziFontBold, FONTSIZEBOLD);
    PDF_show_xy(szPDFFile, szTextT,25, iRow-=LEAD);
     PDF_setfont(szPDFFile, sziFont, FONTSIZESMALL);
 }

HB_FUNC(HB_PDFWRITETEXT)
{
    const char *szTextT;
    szTextT=(char *) hb_parc(1);
    if (iRow<=40) {
        iRow=800;
        PDF_end_page(szPDFFile);
        sziFont = PDF_findfont(szPDFFile, FONTNORMAL, "host", 0);
        sziFontBold = PDF_findfont(szPDFFile, FONTBOLD, "host", 0);
        PDF_begin_page(szPDFFile, a4_width, a4_height); /* start a new page */
        iPage++;
}      

    PDF_setfont(szPDFFile, sziFont, FONTSIZESMALL);
    PDF_show_xy(szPDFFile,szTextT,25,iRow-=LEAD);
}

HB_FUNC(HB_PDFCLOSE)
{
    hb_itemRelease(pArray);
    PDF_close(szPDFFile);               /* close PDF document   */
    PDF_delete(szPDFFile);             /* delete the PDF object */
    szPDFFile=NULL;
    sziFontBold=NULL;
    sziFont=NULL;

}
HB_FUNC(HB_PDFWRITEARG)
{
    const char *szTextT;
    const char *szBTextT;

    szTextT=(char *) hb_parc(2);
    szBTextT=(char *) hb_parc(1);

    if (iRow<=40) {
        iRow=800;
        PDF_end_page(szPDFFile);
        sziFont = PDF_findfont(szPDFFile, FONTNORMAL, "host", 0);
        sziFontBold = PDF_findfont(szPDFFile, FONTBOLD, "host", 0);  
        PDF_begin_page(szPDFFile, a4_width, a4_height); /* start a new page */
        iPage++;
}
    PDF_setfont(szPDFFile, sziFontBold, FONTSIZESMALL);
    PDF_show_xy(szPDFFile,szBTextT,25,iRow-=LEAD);
    PDF_setfont(szPDFFile, sziFont, FONTSIZESMALL);
    PDF_show(szPDFFile,szTextT);               
}

HB_FUNC(HB_PDFADDLINK)
{
int iPagetoGo=hb_parni(2);
const char *szLink;

szLink=(char *) hb_parc(1);
    if (iRow<=40) {
        iRow=800;
        PDF_end_page(szPDFFile);
        sziFont = PDF_findfont(szPDFFile, FONTNORMAL, "host", 0);
        sziFontBold = PDF_findfont(szPDFFile, FONTBOLD, "host", 0);  
        PDF_begin_page(szPDFFile, a4_width, a4_height); /* start a new page */
        PDF_setfont(szPDFFile, sziFont, FONTSIZESMALL);
        iPage++;
    }
iCol= (float) iRow;
iWidth=hb_checkStringWidth(szLink);

PDF_set_border_color(szPDFFile, (float) 1 ,(float)  1 ,(float) 1);
PDF_add_locallink(szPDFFile, 49, iRow-=LEADLINK,60+iWidth, iCol, iPagetoGo, "fitwidth");
PDF_setrgbcolor(szPDFFile, (float) 0, (float) 0, (float) 1);
PDF_set_parameter(szPDFFile, "underline","true");
PDF_show_xy(szPDFFile,szLink,50,iCol-=LEAD);
PDF_setrgbcolor(szPDFFile, (float) 0, (float) 0, (float) 0);
PDF_set_parameter(szPDFFile, "underline","false");
iWidth=0;
}

HB_FUNC(HB_PDFINITBOOK)
{
PHB_ITEM pItems;
pItems=hb_param(1,HB_IT_ARRAY);
uiLen=hb_arrayLen(pItems);
if (pArray == NULL){
   pArray=hb_itemArrayNew( uiLen );
   }
for (uiCount=1;uiCount<=uiLen;uiCount++)
 {
    const char *szBook = (char *) hb_arrayGetCPtr(pItems,uiCount);
    int iLevel;
    PHB_ITEM pTempArray=hb_itemArrayNew(2);
    PHB_ITEM pTemp;
    PHB_ITEM pTempDesc;

    iLevel=PDF_add_bookmark(szPDFFile, szBook, 0, 0);
    pTempDesc=hb_itemPutC(NULL,hb_arrayGetCPtr(pItems,uiCount)) ;
    hb_itemArrayPut(pTempArray,1,pTempDesc);
    hb_itemRelease(pTempDesc);
    pTemp=hb_itemPutNI(NULL,iLevel);
    hb_itemArrayPut(pTempArray,2,pTemp);
    hb_itemRelease(pTemp);
    hb_itemArrayPut(pArray,uiCount,pTempArray);
    hb_itemRelease(pTempArray) ;

}
}
HB_FUNC(HB_PDFBOOKMARK)
{
const char *szBook1=(char *)hb_parc(2);
ULONG uiPos;
PHB_ITEM pTempArray;
int iParent=0;
PHB_ITEM pBlock=hb_param(3,HB_IT_BLOCK);
uiPos=hb_arrayScan(pArray,pBlock,NULL,NULL);
if (uiPos>0) {
   pTempArray=hb_itemArrayGet(pArray,uiPos);
   iParent=hb_arrayGetNI(pTempArray,2);
}
   PDF_add_bookmark(szPDFFile, szBook1, iParent, 0);
hb_itemRelease(pTempArray);   
}
HB_FUNC(HB_GETPAGE)
{
  hb_retni(iPage);
  }
static  BOOL hb_checkRow(float iLine)
{
    if (iLine<=20) {
        iRow=800;
        PDF_end_page(szPDFFile);
        PDF_begin_page(szPDFFile, a4_width, a4_height); /* start a new page */
        iPage++;
        return TRUE;
    }
    return FALSE;
}
static  float hb_checkStringWidth(const char *szString)
{
float fReturn;
fReturn= PDF_stringwidth(szPDFFile,szString,sziFont,FONTSIZESMALL);
return fReturn;
}
HB_FUNC(HB_PDFTABLE)
{
PHB_ITEM pTableItem  ;
PHB_ITEM pTableItem1 ;
PHB_ITEM pTableItem2 ;
PHB_ITEM pTableItem3 ;
ULONG ulPos;

pTableItem =hb_param(1,HB_IT_ARRAY);
pTableItem1 =hb_param(2,HB_IT_ARRAY);
pTableItem2 =hb_param(3,HB_IT_ARRAY);
pTableItem3 =hb_param(4,HB_IT_ARRAY);
if (pTableItem2 != NULL){
   bTItems=TRUE;
}
if (pTableItem3 != NULL){
   bFItems=TRUE;
   bTItems=FALSE;
}
iRow-=LEAD;
for (ulPos=1;ulPos<=hb_arrayLen(pTableItem);ulPos++) {
PHB_ITEM pTempArray  ;
PHB_ITEM pTempArray1 ;

iCol=iRow;
      pTempArray=hb_itemArrayGet(pTableItem,ulPos);
      pTempArray1=hb_itemArrayGet(pTableItem1,ulPos);
     if (!bTItems && !bFItems) {
/*         if (ulPos<2) {
            iRow-=LEAD;
            }*/
         hb_ProcessTableItem(pTempArray,pTempArray1,NULL,NULL);
         hb_itemRelease(pTempArray);
         hb_itemRelease(pTempArray1);
     }
     if (bTItems && !bFItems) {
      PHB_ITEM pTempArray2;

      pTempArray2=hb_itemArrayGet(pTableItem2,ulPos);
         hb_ProcessTableItem(pTempArray,pTempArray1,pTempArray2,NULL);
         hb_itemRelease(pTempArray);
         hb_itemRelease(pTempArray1);
         hb_itemRelease(pTempArray2);
     }
     if (!bTItems && bFItems) {
PHB_ITEM pTempArray2 ;
PHB_ITEM pTempArray3 ;

      pTempArray2=hb_itemArrayGet(pTableItem2,ulPos);
      pTempArray3=hb_itemArrayGet(pTableItem3,ulPos);
         hb_ProcessTableItem(pTempArray,pTempArray1,pTempArray2,pTempArray3);
         hb_itemRelease(pTempArray);
         hb_itemRelease(pTempArray1);
         hb_itemRelease(pTempArray2);
         hb_itemRelease(pTempArray3);
     }
}
bTItems=FALSE;
bFItems=FALSE;
      fOldPos=0;
}
static  void hb_ProcessTableItem(PHB_ITEM p1,PHB_ITEM p2,PHB_ITEM p3,PHB_ITEM p4)
{
   ULONG ulTempPos;
   ULONG ulLen;
   float fHeight;
   float fI;
   float iitem,iitem1,iitem2,iitem3;
   
   ulLen=hb_arrayLen(p1);
   for (ulTempPos=1;ulTempPos<=ulLen;ulTempPos++){
      const char *szTemp=(char *) hb_arrayGetCPtr(p1,ulTempPos);
      const char *szTemp1=(char *) hb_arrayGetCPtr(p2,ulTempPos);

      if (!bTItems && !bFItems) {
         PDF_setfont(szPDFFile, sziFont, FONTSIZETABLE);
       iitem=getText(szPDFFile,szTemp1,sziFont,293,iRow,261);
       iitem1=getText(szPDFFile,szTemp,sziFont,27,iRow,261);
       if (iitem <iitem1) {
         setText(szPDFFile ,szTemp1,293,iRow,iitem1,261);
         setText(szPDFFile ,szTemp,27,iRow,iitem1,261);
         iRow-=iitem1;
      }
         else {
         setText(szPDFFile ,szTemp1,293,iRow,iitem,261);
         setText(szPDFFile ,szTemp,27,iRow,iitem,261);
         iRow-=iitem;
      }

   }
     if (bTItems && !bFItems) {
         const char * szTemp2=(char *) hb_arrayGetCPtr(p3,ulTempPos);
         PDF_setfont(szPDFFile, sziFont, FONTSIZETABLE);
       iitem=getText(szPDFFile,szTemp2,sziFont,381,iRow,173);
       iitem1=getText(szPDFFile,szTemp1,sziFont,204,iRow,172);
       iitem2=getText(szPDFFile,szTemp,sziFont,27,iRow,172);
      if (iitem > iitem1 && iitem >iitem2) {
         setText(szPDFFile  ,szTemp2,381,iRow,iitem,173);
         setText(szPDFFile  ,szTemp1,204,iRow,iitem,172);
         setText(szPDFFile  ,szTemp,27,iRow,iitem,172);
         iRow-=iitem;
      }

      if (iitem2> iitem && iitem2>iitem1){
         setText(szPDFFile  ,szTemp2,381,iRow,iitem2,173);
         setText(szPDFFile  ,szTemp1,204,iRow,iitem2,172);
         setText(szPDFFile  ,szTemp,27,iRow,iitem2,172);
         iRow-=iitem2;
      }

      if (iitem1> iitem && iitem1>iitem2 ){
         setText(szPDFFile  ,szTemp2,381,iRow,iitem1,173);
         setText(szPDFFile  ,szTemp1,204,iRow,iitem1,172);
         setText(szPDFFile  ,szTemp,27,iRow,iitem1,172);
         iRow-=iitem1;
   }

}
     if (!bTItems && bFItems) {
         const char * szTemp2=(char *) hb_arrayGetCPtr(p3,ulTempPos);
         const char * szTemp3=(char *) hb_arrayGetCPtr(p4,ulTempPos);
         PDF_setfont(szPDFFile, sziFont, FONTSIZETABLE);
    iitem1=getText(szPDFFile ,szTemp3,sziFont,426,iRow,128);
    iitem2=getText(szPDFFile ,szTemp2,sziFont,293,iRow,128);
    iitem=getText(szPDFFile ,szTemp1,sziFont,160,iRow,128);
    iitem3=getText(szPDFFile ,szTemp,sziFont,27,iRow,128);
   if (iitem > iitem1 && iitem >iitem2 && iitem>iitem3) {
      setText(szPDFFile  ,szTemp3,426,iRow,iitem,128);
      setText(szPDFFile  ,szTemp2,293,iRow,iitem,128);
      setText(szPDFFile  ,szTemp1,160,iRow,iitem,128);
      setText(szPDFFile  ,szTemp,27,iRow,iitem,128);
      iRow-=iitem;
   }
   else { 
      if (iitem2> iitem && iitem2>iitem1 && iitem2>iitem3){
         setText(szPDFFile  ,szTemp3,426,iRow,iitem2,128);
         setText(szPDFFile  ,szTemp2,293,iRow,iitem2,128);
         setText(szPDFFile  ,szTemp1,160,iRow,iitem2,128);
         setText(szPDFFile  ,szTemp,27,iRow,iitem2,128);
         iRow-=iitem2;
      }
   else {
      if (iitem1> iitem && iitem1>iitem2 && iitem1>iitem3){
         setText(szPDFFile  ,szTemp3,426,iRow,iitem1,128);
         setText(szPDFFile  ,szTemp2,293,iRow,iitem1,128);
         setText(szPDFFile  ,szTemp1,160,iRow,iitem1,128);
         setText(szPDFFile  ,szTemp,27,iRow,iitem1,128);
         iRow-=iitem1;
      }
   else {
      if (iitem3> iitem && iitem3>iitem2 && iitem3>iitem){
         setText(szPDFFile  ,szTemp3,426,iRow,iitem3,128);
         setText(szPDFFile  ,szTemp2,293,iRow,iitem3,128);
         setText(szPDFFile  ,szTemp1,160,iRow,iitem3,128);
         setText(szPDFFile  ,szTemp,27,iRow,iitem3,128);
         iRow-=iitem3;
}
      else {
         setText(szPDFFile  ,szTemp3,426,iRow,iitem3,128);
         setText(szPDFFile  ,szTemp2,293,iRow,iitem3,128);
         setText(szPDFFile  ,szTemp1,160,iRow,iitem3,128);
         setText(szPDFFile  ,szTemp,27,iRow,iitem3,128);
         iRow-=iitem3;

      }
      }
      }
      }
}

}
}

float getText(PDF *p,const char *szText,int iFont,float irow,float icol,float iw)
{
float h;
float w;
int c;
h=FONTSIZETABLE*2;
PDF_setfont(p, iFont, 7.0);
c=PDF_show_boxed(p, szText, irow, icol, iw, h, "justify", "blind");
while (c>0){
   h+= FONTSIZETABLE;  
   c=PDF_show_boxed(p, szText, irow, icol, iw, h, "justify", "blind");
}
return h;
}

void setText(PDF *p,const char *szText,float irow,float icol,float h,float iw)
{

      PDF_show_boxed(p, szText, irow, icol, iw, h, "justify", "");
      PDF_rect(p,irow,icol,iw+5,h);
      PDF_stroke(p);
}
