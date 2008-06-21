/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * STRFORMAT() function
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

#include "hbapi.h"

#define HB_STRFORMAT_PARNUM_MAX_ 9

HB_FUNC( STRFORMAT )
{
   char* pszMask = hb_parc(1);
   ULONG nMaskLen = hb_parclen(1);
   ULONG nMaskPos;
   ULONG nParNum = hb_pcount();
   ULONG nLenTable [HB_STRFORMAT_PARNUM_MAX_];
   char* pszVarTable [HB_STRFORMAT_PARNUM_MAX_];

   ULONG nRetValLen;
   char* pszRetVal;
   char* pszRetValSave;

   ULONG tmp;

   if (nParNum < 1)
   {
      hb_retc("");
      return;
   }

   nParNum--;

   if (nParNum > HB_STRFORMAT_PARNUM_MAX_) nParNum = HB_STRFORMAT_PARNUM_MAX_;

   for (tmp = 0; tmp < nParNum; tmp++)
   {
      nLenTable[tmp] = hb_parclen(tmp + 2);
      pszVarTable[tmp] = hb_parc(tmp + 2);
   }

   nMaskPos = 0;

   nRetValLen = 0;
   while (nMaskPos < nMaskLen)
   {
      if (pszMask[nMaskPos] == '%')
      {
         nMaskPos++;

         if (pszMask[nMaskPos] == '\0')
         {
            break;
         }
         else if (pszMask[nMaskPos] == '%')
         {
            nRetValLen++;
         }
         else if (pszMask[nMaskPos] >= '1' && pszMask[nMaskPos] <= (int)(nParNum + '0'))
         {
            nRetValLen += nLenTable[pszMask[nMaskPos] - '1'];
         }
         else
         {
            /* ; do nothing */
         }
      }
      else
      {
         nRetValLen++;
      }

      nMaskPos++;
   }

   nMaskPos = 0;

   pszRetVal = pszRetValSave = (char *) hb_xgrab(nRetValLen + 1);
   while (nMaskPos < nMaskLen)
   {
      if (pszMask[nMaskPos] == '%')
      {
         nMaskPos++;

         if (pszMask[nMaskPos] == '\0')
         {
            break;
         }
         else if (pszMask[nMaskPos] == '%')
         {
            *pszRetVal++ = pszMask[nMaskPos];
         }
         else if (pszMask[nMaskPos] >= '1' && pszMask[nMaskPos] <= (int)(nParNum + '0'))
         {
            ULONG nPos = pszMask[nMaskPos] - '1';

            memcpy(pszRetVal, pszVarTable[nPos], nLenTable[nPos]);
            pszRetVal += nLenTable[nPos];
         }
         else
         {
            /* ; do nothing */
         }
      }
      else
      {
         *pszRetVal++ = pszMask[nMaskPos];
      }

      nMaskPos++;
   }

   hb_retclen(pszRetValSave, nRetValLen);

   hb_xfree(pszRetValSave);
}
