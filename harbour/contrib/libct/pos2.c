/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   POSCHAR(), POSDEL(), POSINS() and POSREPL() CT3 functions
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


/*  $DOC$
 *  $FUNCNAME$
 *      POSCHAR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Replace character at a certain position within a string
 *  $SYNTAX$
 *      POSCHAR (<[@]cString>, <cCharacter|nCharacter>, [<nPosition>]) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSCHAR() is compatible with CT3's POSCHAR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos2.c, library is libct.
 *  $SEEALSO$
 *      POSDEL(),POSINS(),POSREPL(),CSETREF()
 *  $END$
 */

HB_FUNC (POSCHAR)
{

  int iNoRet;

  iNoRet = ct_getref();

  if (hb_parclen (1) > 0)
  {
    if ((hb_parclen (2) > 0) || ISNUM (2))
    {
      char *pcString = hb_parc (1);
      size_t sStrLen = hb_parclen (1);
      char *pcRet;
      char cReplace;
      size_t sPosition;

      if (ISCHAR (2))
      {
        cReplace = *(hb_parc (2));
      }
      else
      {
        cReplace = hb_parnl (2)%256;
      }

      if (ISNUM (3))
      {
        sPosition = hb_parnl (3);
        if (sPosition == 0)
        {
          sPosition = sStrLen;
        }
      }
      else
      {
        sPosition = sStrLen;
      }

      pcRet = hb_xgrab (sStrLen);
      hb_xmemcpy (pcRet, pcString, sStrLen);
      *(pcRet+sPosition-1) = cReplace;

      if (ISBYREF (1))
      {
        hb_storclen (pcRet, sStrLen, 1);
      }
      
      if (iNoRet)
      {
        hb_ret();
      }
      else
      {
        hb_retclen (pcRet, sStrLen);
      }

      hb_xfree (pcRet);

    }
    else  /* (hb_parclen (2) > 0) || ISNUM (2) */
    {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSCHAR,
                                 NULL, "POSCHAR", 0, EF_CANSUBSTITUTE, 3,
                                 hb_paramError (1), hb_paramError (2), hb_paramError (3));
      }
    
      if (pSubst != NULL)
      {
        hb_itemReturn (pSubst);
        hb_itemRelease (pSubst);
      }
      else
      {
        if (iNoRet)
        {
          hb_ret();
        }
        else
        {
          hb_retclen (hb_parc (1), hb_parclen (1));
        }
      }
    }
  }
  else  /* hb_parclen (1) > 0 */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSCHAR,
                               NULL, "POSCHAR", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2), hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      if (iNoRet)
        hb_ret();
      else
        hb_retc ("");
    }
  }

}


/*  $DOC$
 *  $FUNCNAME$
 *      POSDEL()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Delete characters at a certain position within a string
 *  $SYNTAX$
 *      POSDEL (<cString>, [<nStartPosition>], <nLength>) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSDEL() is compatible with CT3's POSDEL().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos2.c, library is libct.
 *  $SEEALSO$
 *      POSCHAR(),POSINS(),POSREPL()
 *  $END$
 */

HB_FUNC (POSDEL)
{
  
  if (ISCHAR (1))
  {  

    char *pcString = hb_parc (1);
    size_t sStrLen = hb_parclen (1);
    size_t sStartPos, sDelLen;
    char *pcRet;

    if (ISNUM (3))
    {
      sDelLen = hb_parnl (3);
    }
    else
    {
      sDelLen = 1;  /* set new standard behavior */
    }

    if (ISNUM (2))
    {
      sStartPos = hb_parnl (2);
      if (sStartPos == 0)
      {
        sStartPos = sStrLen-sDelLen+1;
      }
    }
    else
    {
      sStartPos = sStrLen-sDelLen+1;
    }
    
    pcRet = hb_xgrab (sStrLen-sDelLen);

    /* copy first part */
    if (sStartPos > 1)
    {
      hb_xmemcpy (pcRet, pcString, sStartPos-1);
    }
    
    /* copy second part */
    if (sStrLen > (sStartPos-1+sDelLen))
    {
      hb_xmemcpy (pcRet+sStartPos-1, pcString+sStartPos-1+sDelLen,
                  sStrLen-(sStartPos-1+sDelLen));
    }

    hb_retclen (pcRet, sStrLen-sDelLen);
    hb_xfree (pcRet);

  }
  else  /* ISCHAR (1) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSDEL,
                               NULL, "POSDEL", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2), hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retc ("");
    }
  }

}


/*  $DOC$
 *  $FUNCNAME$
 *      POSINS()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Insert characters at a certain position within a string
 *  $SYNTAX$
 *      POSINS (<cString>, <cInsert>, [<nPosition>]) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSINS() is compatible with CT3's POSINS().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos2.c, library is libct.
 *  $SEEALSO$
 *      POSCHAR,POSDEL(),POSREPL()
 *  $END$
 */

HB_FUNC (POSINS)
{
  
  if (ISCHAR (1))
  {

    char *pcString = hb_parc (1);
    size_t sStrLen = hb_parclen (1);
    char *pcInsert;
    size_t sInsLen;

    if ((sInsLen = hb_parclen (2)) > 0)
    {
      
      size_t sStartPos;
      char *pcRet;

      pcInsert = hb_parc (2);
      
      if (ISNUM (3))
      {
        sStartPos = hb_parnl (3);
        if (sStartPos == 0)
        {
          sStartPos = sStrLen;
        }
      }
      else
      {
        sStartPos = sStrLen;
      }

      /* check for false sStartPos */
      if (sStartPos > sStrLen+1)
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSINS,
                    NULL, "POSINS", 0, EF_CANDEFAULT, 3,
                    hb_paramError (1), hb_paramError (2),
                    hb_paramError (3));
        }

        hb_retclen (pcString, sStrLen);
        return;
      }

      pcRet = hb_xgrab (sStrLen+sInsLen);

      /* copy first part */
      if (sStartPos > 1)
      {
        hb_xmemcpy (pcRet, pcString, sStartPos-1);
      }
    
      /* insert string */
      hb_xmemcpy (pcRet+sStartPos-1, pcInsert, sInsLen);

      /* copy second part */
      if (sStrLen > (sStartPos-1))
      {
        hb_xmemcpy (pcRet+sStartPos-1+sInsLen, pcString+sStartPos-1,
                    sStrLen-(sStartPos-1));
      }

      hb_retclen (pcRet, sStrLen+sInsLen);
      hb_xfree (pcRet);

    }
    else   /* hb_parclen (2) > 0 */
    {
      hb_retclen (pcString, sStrLen);
    }

  }
  else  /* ISCHAR (1) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSINS,
                               NULL, "POSINS", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2), hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retc ("");
    }
  }

}

  
/*  $DOC$
 *  $FUNCNAME$
 *      POSREPL()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Replace characters at a certain position within a string
 *  $SYNTAX$
 *      POSREPL (<[@]cString>, <cReplacement>, [<nStartPosition>]) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSREPL() is compatible with CT3's POSREPL().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos2.c, library is libct.
 *  $SEEALSO$
 *      POSCHAR(),POSDEL(),POSINS(),CSETREF()
 *  $END$
 */

HB_FUNC (POSREPL)
{

  int iNoRet;

  iNoRet = ct_getref();

  if (ISCHAR (1))
  {

    char *pcString = hb_parc (1);
    size_t sStrLen = hb_parclen (1);
    char *pcReplace;
    size_t sReplLen;

    if ((sReplLen = hb_parclen (2)) > 0)
    {
      
      size_t sStartPos;
      char *pcRet;
      size_t sRetLen;

      pcReplace = hb_parc (2);
      
      if (ISNUM (3))
      {
        sStartPos = hb_parnl (3);
        if (sStartPos == 0)
        {
          if (sReplLen > sStrLen)
          {
            sStartPos = 1;
          }
          else
          {
            sStartPos = sStrLen-sReplLen+1;
          }
        }
      }
      else
      {
        if (sReplLen > sStrLen)
        {
          sStartPos = 1;
        }
        else
        {
          sStartPos = sStrLen-sReplLen+1;
        }
      }

      /* check for false sStartPos */
      if (sStartPos > sStrLen+1)
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSREPL,
                    NULL, "POSREPL", 0, EF_CANDEFAULT, 3,
                    hb_paramError (1), hb_paramError (2),
                    hb_paramError (3));
        }

        if (iNoRet)
        {
          hb_ret();
        }
        else
        {
          hb_retclen (pcString, sStrLen);
        }

        return;
      }

      if (sStrLen > (sStartPos+sReplLen-1))
      {
        sRetLen = sStrLen;
      }
      else
      {
        sRetLen = sStartPos+sReplLen-1;
      }

      pcRet = hb_xgrab (sRetLen);

      /* copy first part */
      if (sStartPos > 1)
      {
        hb_xmemcpy (pcRet, pcString, sStartPos-1);
      }
    
      /* insert replacement string */
      hb_xmemcpy (pcRet+sStartPos-1, pcReplace, sReplLen);

      /* copy second part */
      if (sStrLen > (sStartPos-1+sReplLen))
      {
        hb_xmemcpy (pcRet+sStartPos-1+sReplLen, pcString+sStartPos-1+sReplLen,
                    sStrLen-(sStartPos-1+sReplLen));
      }

      if (iNoRet)
      {
        hb_ret();
      }
      else
      {
        hb_retclen (pcRet, sRetLen);
      }

      if (ISBYREF (1))
      {
        hb_storclen (pcRet, sRetLen, 1);
      }

      hb_xfree (pcRet);

    }
    else   /* hb_parclen (2) > 0 */
    {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSREPL,
                                 NULL, "POSREPL", 0, EF_CANSUBSTITUTE, 3,
                                 hb_paramError (1), hb_paramError (2), hb_paramError (3));
      }
    
      if (pSubst != NULL)
      {
        hb_itemReturn (pSubst);
        hb_itemRelease (pSubst);
      }
      else
      {
        if (iNoRet)
        {
          hb_ret();
        }
        else
        {
          hb_retclen (pcString, sStrLen);
        }
      }
    }

  }
  else  /* ISCHAR (1) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSREPL,
                               NULL, "POSREPL", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2), hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      if (iNoRet)
      {
        hb_ret();
      }
      else
      {
        hb_retc ("");
      }
    }

  }

}




