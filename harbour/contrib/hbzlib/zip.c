/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour zip file compress function,
 *
 * Copyright 2000-2001 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include <hbzip2.h>

extern PHB_ITEM pArray;
                                                                                                                    
HB_FUNC(HB_ZIPFILE)
{
    if( ISCHAR(1) && ISCHAR(2))        {
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        hb_retl(hb_CompressFileStd(hb___CheckFile(szFile),hb_parc(2),ISNUM(3) ? hb_parni(3) : (-1) ,hb_param( 4, HB_IT_BLOCK) ,ISLOG(5) ? hb_parl(5) : 0,hb_parc(6),ISLOG(7) ? hb_parl(7) : 0 ,ISLOG(8) ? hb_parl(8) : 0,hb_itemParam(9)));
        }

    if(ISCHAR(1) && ISARRAY(2))        {
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
            hb_retl(hb_CompressFile(hb___CheckFile(szFile),hb_param( 2, HB_IT_ARRAY ),ISNUM(3) ? hb_parni(3) : (-1) ,hb_param( 4,HB_IT_BLOCK),ISLOG(5) ? hb_parl(5) : 0,hb_parc(6),ISLOG(7) ? hb_parl(7) : 0,ISLOG(8) ? hb_parl(8) : 0,hb_itemParam(9)));

        }
      
}
HB_FUNC(HB_GETFILESINZIP)
{
    if( ISCHAR(1)  ){
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        hb___GetFilesNamesFromZip(hb___CheckFile(szFile),ISLOG(2) ? hb_parl(2) : 0);
        hb_itemReturn(pArray);
        hb_itemRelease(pArray);
}
}
/*
HB_FUNC(HB_ZIPGETPASSWORD)
{
char *szName=hb_parc(1);
char *szPassWord;
    szPassWord=hb_getPassWord(szName);
hb_retc(szPassWord);
}
*/
HB_FUNC(HB_GETUNZIPFILE)
{
    if( ISCHAR(1)  ){
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

        hb_retni(hb___GetNumbersofFilestoUnzip(hb___CheckFile(szFile)));
}
}
HB_FUNC(HB_ZIPFILEBYTDSPAN)
{
    if( ISCHAR(1) && ISCHAR(2))        {
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
            hb_retl(hb_CmpTdSpanStd(hb___CheckFile(szFile),hb_parc(2),ISNUM(3) ? hb_parni(3) : (-1),hb_param( 4, HB_IT_BLOCK) ,ISLOG(5) ? hb_parl(5) : 0, hb_parc(6) ,hb_param( 7,HB_IT_BLOCK),ISNUM(8) ? hb_parni(8) : 0,ISLOG(9) ? hb_parl(9) : 0,ISLOG(10) ? hb_parl(10) : 0,hb_itemParam(11)));
    }

    if(ISCHAR(1) && ISARRAY(2))        {
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        hb_retl(hb_CmpTdSpan(hb___CheckFile(szFile),hb_param( 2, HB_IT_ARRAY ),ISNUM(3) ? hb_parni(3) : (-1) ,hb_param( 4,HB_IT_BLOCK),ISLOG(5) ? hb_parl(5) : 0 ,hb_parc(6)   ,hb_param( 7,HB_IT_BLOCK),ISNUM(8) ? hb_parni(8) :0,ISLOG(9) ? hb_parl(9) : 0,ISLOG(10) ? hb_parl(10) : 0,hb_itemParam(11)));
    }
     
}
HB_FUNC(HB_ZIPFILEBYPKSPAN)
{
    if( ISCHAR(1) && ISCHAR(2))        {
        char szFile[_POSIX_PATH_MAX];
         strcpy(szFile,hb_parc(1));
         hb_retl(hb_CmpPkSpanStd(hb___CheckFile(szFile),hb_parc(2),ISNUM(3) ? hb_parni(3) : (-1),hb_param( 4, HB_IT_BLOCK) ,ISLOG(5) ? hb_parl(5) : 0, hb_parc(6),ISLOG(7) ? hb_parl(7) : 0,ISLOG(8) ? hb_parl(8) : 0,hb_itemParam(9)));
        }

    if(ISCHAR(1) && ISARRAY(2))        {
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

            hb_retl(hb_CmpPkSpan(hb___CheckFile(szFile),hb_param( 2, HB_IT_ARRAY ),ISNUM(3) ? hb_parni(3) : (-1),hb_param( 4,HB_IT_BLOCK),ISLOG(5) ? hb_parl(5) : 0,hb_parc(6),ISLOG(7) ? hb_parl(7) : 0,ISLOG(8) ? hb_parl(8) : 0,hb_itemParam(9)));
}
     
}
HB_FUNC(HB_UNZIPFILE)

{
    if( ISCHAR(1) && ISCHAR(6) ){
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

        hb_retl(hb_UnzipOne(hb___CheckFile(szFile),hb_param( 2, HB_IT_BLOCK),ISLOG(3) ? hb_parl(3) : 0 ,hb_parc(4),hb_parc(5),hb_parc(6),hb_itemParam(7)));
            }
    if( ISCHAR(1) && ISARRAY(6) ){
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

        hb_retl(hb_UnzipSel(hb___CheckFile(szFile),hb_param( 2, HB_IT_BLOCK),ISLOG(3) ? hb_parl(3) : 0 ,hb_parc(4),hb_parc(5),hb_param(6,HB_IT_ARRAY),hb_itemParam(7)));
       }

    if ( ! ISCHAR(6) && ! ISARRAY(6) ) { 
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        hb_retl(hb_UnzipAll(hb___CheckFile(szFile),hb_param( 2, HB_IT_BLOCK),ISLOG(3) ? hb_parl(3) : 0 ,hb_parc(4),hb_parc(5),hb_param( 6, HB_IT_BLOCK),hb_itemParam(7)));
        }
}
HB_FUNC(HB_SETDISKZIP)
{
hb_retl(hb___SetCallbackFunc(hb_itemParam(1)));
}
HB_FUNC(HB_ZIPDELETEFILES)
{
    if (ISCHAR(1)&& ISCHAR(2)){
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        hb_retl(hb_DeleteOne(hb___CheckFile(szFile),hb_parc(2)));
    }
    if (ISCHAR(1)&&ISARRAY(2)) {
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        hb_retl(hb_DeleteSel(hb___CheckFile(szFile),hb_param(2,HB_IT_ARRAY),ISLOG(3) ? hb_parl(3) : 0));
}
}
HB_FUNC(HB_ZIPTESTPK)
{
      char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        hb_retni(hb_TestForPKS(hb___CheckFile(szFile)));

}
HB_FUNC(HB_SETBUFFER)
    {
    hb_SetZipBuff(ISNUM(1) ? hb_parni(1) : 65535,ISNUM(2) ? hb_parni(2) : 16384,ISNUM(3) ? hb_parni(3) : 32768);
}
HB_FUNC(HB_SETZIPCOMMENT)
    {
    hb_SetZipComment(hb_parc(1));
}
HB_FUNC(HB_GETZIPCOMMENT)
    {
    hb_retc(hb_GetZipComment(hb_parc(1)));
}
