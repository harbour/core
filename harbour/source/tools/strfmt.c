/*
 * $Id$
 */

/*
   Harbour Project source code

   Simple string formatter.

   Copyright 1997-1999  Victor Szel <info@szelvesz.hu>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

#include "extend.h"

/*  $DOC$
 *  $FUNCNAME$
 *     StrFormat()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     ...
 *  $SYNTAX$
 *     StrFormat(<cMask>[, <cPar1>[, <cParn>[, ...]])
 *  $ARGUMENTS$
 *     <cMask> Holds the mask for the resulting string
 *     <cParn> Holds the strings to be inserted in the mask
 *             maximum 9 of them can be specified.
 *  $RETURNS$
 *     Return the mask with all the parameters inserted.
 *  $DESCRIPTION$
 *     String replacment, can be useful when writing international
 *     apps. You can separate the constant strings from the variable ones.
 *     Each %1 - %9 marks will be replaced with the appropriate parameter
 *     from the parameter list.
 *     Marks can be in any order, and can be duplicated.
 *     You can print "%" character with "%%".
 *  $EXAMPLES$
 *     StrFormat("Please insert disk %1 to drive %2", LTrim(Str(2)), "A:")
 *     StrFormat("This is %1 from %2", "Victor", "Hungary")
 *     StrFormat("%2 %1 %2", "Param1", "Param2")
 *  $TESTS$
 *     ? StrFormat("Please insert disk %1 to drive %2", LTrim(Str(2)), "A:")
 *     ? StrFormat("This is %1 from %2", "Victor", "Hungary")
 *     ? StrFormat("%2 %1 %2", "Param1", "Param2")
 *     ? StrFormat("Hello")
 *     ? StrFormat("%1 - %2", "one")
 *     ? StrFormat("%1 - %2", "one", "two")
 *     ? StrFormat("%2 - %1", "one", "two")
 *     ? StrFormat("%2 - %", "one", "two")
 *     ? StrFormat("%% - %", "one", "two")
 *     ? StrFormat("%9 - %", "one", "two")
 *  $STATUS$
 *     Done
 *  $COMPLIANCE$
 *     All platforms
 *  $SEEALSO$
 *
 *  $END$
 */

#define HB_STRFORMAT_PARNUM_MAX_ 9

HARBOUR HB_STRFORMAT (void) /* StrFormat() */
{
   char* pszMask = hb_parc(1);
   ULONG nMaskLen = hb_parclen(1);
   ULONG nMaskPos;
   ULONG nParNum = PCOUNT;
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

   pszRetVal = pszRetValSave = hb_xgrab(nRetValLen + 1);
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
