/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __STRTOKEN() helper routine for TEDITOR.PRG
 *
 * Copyright 2000 Harbour project.
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

#include "hbapi.h"

char *hb_strtoken(char *szText,
                  long lText,
                  long lIndex,
                  char cDelimiter,
                  long *lLen)
{
  long wStart;
  long wEnd = 0;
  long wCounter = 0;

  HB_TRACE(HB_TR_DEBUG, ("hb_strtoken(%s, %ld, %ld, %d, %p)", szText, lText, lIndex, (int) cDelimiter, lLen));

  do
    {
      wStart = wEnd;

      if( cDelimiter != ' ' )
        {
          if( szText[wStart] == cDelimiter )
            wStart++;
        }
      else
        {
          while( wStart < lText && szText[wStart] == cDelimiter )
            wStart++;
        }

      if( wStart < lText && szText[wStart] != cDelimiter )
        {
          wEnd = wStart + 1;

          while( wEnd < lText && szText[wEnd] != cDelimiter )
            wEnd++;
        }
      else
        wEnd = wStart;
    } while( wCounter++ < lIndex - 1 && wEnd < lText );

  if( wCounter < lIndex )
    {
      *lLen = 0;
      return "";
    }
  else
    {
      *lLen = wEnd - wStart;
      return szText + wStart;
    }
}

/* returns the nth occurence of a substring within a token-delimited string */
HB_FUNC( __STRTOKEN )
{
  char *szText;
  long lIndex = hb_parnl(2);
  char cDelimiter = *hb_parc(3);
  long lLen;

  if( !cDelimiter )
    cDelimiter = ' ';

  szText = hb_strtoken(hb_parc(1), hb_parclen(1), lIndex, cDelimiter, &lLen);

  hb_stornl(lLen, 4);
  hb_retclen(szText, lLen);
}

