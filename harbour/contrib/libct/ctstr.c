/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   internal functions for CT3 string functions
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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


#include "ct.h"


/* -------------------------- */
/* search for exact substring */
/* -------------------------- */
char *ct_at_exact_forward (char *pcString, size_t sStrLen,
                           char *pcMatch, size_t sMatchLen,
                           size_t *psMatchStrLen)
{

  size_t sPos;
  
  HB_TRACE(HB_TR_DEBUG, ("ct_at_exact_forward (\"%s\", %u, \"%s\", %u, %p)",
                          pcString, sStrlen, pcMatch, sMatchLen, psMatchStrLen));
  
  if ((sMatchLen == 0) || (sStrLen < sMatchLen))
    return (NULL);

  sPos = hb_strAt (pcMatch, sMatchLen, pcString, sStrLen);
  if (sPos == 0)
  {
    return (NULL);
  }
  else
  {
    if (psMatchStrLen != NULL)
      *psMatchStrLen = sMatchLen;
    return (pcString+sPos-1);
  }

}
  

/* ------------------------------------------------ */
/* search for exact substring in backward direction */
/* ------------------------------------------------ */
char *ct_at_exact_backward (char *pcString, size_t sStrLen,
                            char *pcMatch, size_t sMatchLen,
                            size_t *psMatchStrLen)
{

  size_t sIndex;
  char *pcRet;

  HB_TRACE(HB_TR_DEBUG, ("ct_at_exact_backward (\"%s\", %u, \"%s\", %u, %p)",
                         pcString, sStrLen, pcMatch, sMatchLen, psMatchStrLen));
  
  if ((sMatchLen == 0) || (sStrLen < sMatchLen))
    return (NULL);
  
  for (pcRet = pcString+sStrLen-sMatchLen; pcRet >= pcString; pcRet--)
  {
    for (sIndex = 0; sIndex < sMatchLen; sIndex++)
      if (*(pcRet+sIndex) != *(pcMatch+sIndex))
        break;
    if (sIndex == sMatchLen)
    {
      /* last match found */
      if (psMatchStrLen != NULL)
        *psMatchStrLen = sMatchLen;
      return (pcRet);
    }
  }

  return (NULL);

}


/* ----------------------------------- */
/* search for substring using wildcard */
/* ----------------------------------- */
char *ct_at_wildcard_forward (char *pcString, size_t sStrLen,
                              char *pcMatch, size_t sMatchLen,
                              char cWildCard, size_t *psMatchStrLen)
{

  size_t sIndex;
  char *pcRet, *pcStop;

  HB_TRACE(HB_TR_DEBUG, ("ct_at_wildcard_forward (\"%s\", %u, \"%s\", %u, \'%c\', %p)",
                         pcString, sStrLen, pcMatch, sMatchLen, cWildCard, psMatchStrLen));

  if ((sMatchLen == 0) || (sStrLen < sMatchLen))
    return (NULL);

  pcStop = pcString+sStrLen-sMatchLen;
  for (pcRet = pcString; pcRet < pcStop; pcRet++)
  {
    for (sIndex = 0; sIndex < sMatchLen; sIndex++)
    {
      char c = *(pcMatch+sIndex);
      if ((c != cWildCard) && (c != *(pcRet+sIndex)))
        break;
    }
    if (sIndex == sMatchLen)
    {
      if (psMatchStrLen != NULL)
        *psMatchStrLen = sMatchLen;
      return (pcRet);
    }
  }

  return (NULL);

}


/* --------------------------------------------------------- */
/* search for substring using wildcard in backward direction */
/* --------------------------------------------------------- */
char *ct_at_wildcard_backward (char *pcString, size_t sStrLen,
                               char *pcMatch, size_t sMatchLen,
                               char cWildCard, size_t *psMatchStrLen)
{

  size_t sIndex;
  char *pcRet;

  HB_TRACE(HB_TR_DEBUG, ("ct_at_wildcard_backward (\"%s\", %u, \"%s\", %u, \'%c\', %p)",
                         pcString, sStrLen, pcMatch, sMatchLen, cWildCard, psMatchStrLen));

  if ((sMatchLen == 0) || (sStrLen < sMatchLen))
    return (NULL);

  for (pcRet = pcString+sStrLen-sMatchLen; pcRet >= pcString; pcRet--)
  {
    for (sIndex = 0; sIndex < sMatchLen; sIndex++)
    {
      char c = *(pcMatch+sIndex);
      if ((c != cWildCard) && (c != *(pcRet+sIndex)))
        break;
    }
    if (sIndex == sMatchLen)
    {
      /* last match found */
      if (psMatchStrLen != NULL)
        *psMatchStrLen = sMatchLen;
      return (pcRet);
    }
  }
  
  return (NULL);

}


/* ------------------------------- */
/* search for character from a set */
/* ------------------------------- */
char *ct_at_charset_forward (char *pcString, size_t sStrLen,
                             char *pcCharSet, size_t sCharSetLen,
                             size_t *psMatchedCharPos)
{
  
  char *pcRet, *pcSet, *pcStop1, *pcStop2;

  HB_TRACE(HB_TR_DEBUG, ("ct_at_charset_forward (\"%s\", %u, \"%s\", %u, %p)",
                          pcString, sStrlen, pcCharSet, sCharSetLen, psMatchedCharPos));
  
  *(psMatchedCharPos) = sCharSetLen;
                        
  if ((sCharSetLen == 0) || (sStrLen == 0))
    return (NULL);
  
  pcStop1 = pcString+sStrLen;
  pcStop2 = pcCharSet+sCharSetLen;
  for (pcRet = pcString; pcRet < pcStop1; pcRet++)
  {
    for (pcSet = pcCharSet; pcSet < pcStop2; pcSet++)
      if (*pcSet == *pcRet)
      {
        if (psMatchedCharPos != NULL)
          *(psMatchedCharPos) = pcSet-pcCharSet;
        return (pcRet);
      }
  }
  
  return (NULL);

}


/* ----------------------------------------------------- */
/* search for character from a set in backward direction */
/* ----------------------------------------------------- */
char *ct_at_charset_backward (char *pcString, size_t sStrLen,
                              char *pcCharSet, size_t sCharSetLen,
                              size_t *psMatchedCharPos)
{

  char *pcRet, *pcSet, *pcStop;

  HB_TRACE(HB_TR_DEBUG, ("ct_at_charset_backward (\"%s\", %u, \"%s\", %u, %p)",
                          pcString, sStrlen, pcCharSet, sCharSetLen, psMatchedCharPos));
  
  *(psMatchedCharPos) = sCharSetLen;

  if ((sCharSetLen == 0) || (sStrLen == 0))
    return (NULL);
  
  pcStop = pcCharSet+sCharSetLen;
  for (pcRet = pcString+sStrLen-1; pcRet >= pcString; pcRet--)
  {
    for (pcSet = pcCharSet; pcSet < pcStop; pcSet++)
      if (*pcSet == *pcRet)
      {
        if (psMatchedCharPos != NULL)
          *(psMatchedCharPos) = pcSet-pcCharSet;
        return (pcRet);
      }
  }
  
  return (NULL);

}






