/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_ZIPFILE() FUNCTION Harbour zip file compress function
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

#include "hbzip.h"
HB_FUNC(HB_ZIPFILE)
{
    if( ISCHAR(1) && ISCHAR(2))        {
        int iCompLevel;
        BOOL iOverWrite;
        BOOL lExist;
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));
        lExist =hb_fsFile(hb___CheckFile(szFile));
        if( ISNUM(3)) {
            iCompLevel=hb_parni(3);
        }
        else {
            iCompLevel= (-1);
            }
        if( ISLOG(5)) {
            iOverWrite=hb_parl(5);

        }
        else {
            iOverWrite= 1;
        }
        if (lExist){
            PHB_ITEM pArray=hb___GetFilesNamesFromZip(hb___CheckFile(szFile),0);
            PHB_ITEM pItem = hb_itemPutC(NULL,hb_parc(2));
            hb_arrayAdd(pArray,pItem);
            hb_itemRelease(pItem);
            hb_retl(hb___CompressMultipleFile(hb___CheckFile(szFile),pArray,iCompLevel,hb_param( 4,HB_IT_BLOCK),iOverWrite));
            hb_itemRelease(pArray);
            }
        else {
            hb_retl(hb___CompressOneFile(hb___CheckFile(szFile),hb_parc(2),iCompLevel,hb_param( 4, HB_IT_BLOCK) ,iOverWrite));
        }

}

    if(ISCHAR(1) && ISARRAY(2))        {
        int iCompLevel;
        BOOL iOverWrite;
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

        if( ISNUM(3)) {
            iCompLevel=hb_parni(3);

        }
        else {
            iCompLevel= (-1);
            }
        if( ISLOG(5)) {
            iOverWrite=hb_parl(5);

        }
        else {
            iOverWrite= 1;
            }
            hb_retl(hb___CompressMultipleFile(hb___CheckFile(szFile),hb_param( 2, HB_IT_ARRAY ),iCompLevel,hb_param( 4,HB_IT_BLOCK),iOverWrite));

        }
      
}
HB_FUNC(HB_UNZIPFILE)

{
    if( ISCHAR(1)  ){
        BOOL iExtractPath;
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

        if( ISLOG(3)) {
          iExtractPath=hb_parl(3);
                       }
        else {
            iExtractPath= 0;
              }
        hb_retl(hb___unZipFiles(hb___CheckFile(szFile),hb_param( 2, HB_IT_BLOCK),iExtractPath));

            }
}

HB_FUNC(HB_GETUNZIPFILE)
{
    if( ISCHAR(1)  ){
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

        hb_retni(hb___GetNumbersofFilestoUnzip(hb___CheckFile(szFile)));
}
}

HB_FUNC(HB_GETFILESINZIP)
{
    if( ISCHAR(1)  ){
        PHB_ITEM pArray=NULL;
        BOOL bExtended;
        char szFile[_POSIX_PATH_MAX];
        strcpy(szFile,hb_parc(1));

        if( ISLOG(2)) {
            bExtended=hb_parl(2);
        }
        else {
            bExtended= 0;
            }
        pArray=hb___GetFilesNamesFromZip(hb___CheckFile(szFile),bExtended);
        hb_itemReturn(pArray);
        hb_itemRelease(pArray);

}
}

