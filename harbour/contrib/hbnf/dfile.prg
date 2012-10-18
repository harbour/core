/*
 * $Id$
 */

/*
 * Author....: Mike Taylor
 * CIS ID....: ?
 *
 * This is an original work by Mike Taylor and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:24:14   GLENN
 * Don Caton corrected some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:03:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:32   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:08   GLENN
 * Nanforum Toolkit
 *
 */

#include "fileio.ch"

THREAD STATIC t_nHandle := F_ERROR

FUNCTION FT_DFSETUP( cInFile, nTop, nLeft, nBottom, nRight, ;
      nStart, nCNormal, nCHighlight, cExitKeys, ;
      lBrowse, nColSkip, nRMargin, nBuffSize )

   LOCAL rval

   IF hb_FileExists( cInFile )

      hb_default( @nTop        , 0 )
      hb_default( @nLeft       , 0 )
      hb_default( @nBottom     , MaxRow() )
      hb_default( @nRight      , MaxCol() )
      hb_default( @nCNormal    , 7 )
      hb_default( @nCHighlight , 15 )
      hb_default( @nStart      , 1 )
      hb_default( @nColSkip    , 1 )
      hb_default( @lBrowse     , .F. )
      hb_default( @nRMargin    , 255 )
      hb_default( @nBuffSize   , 4096 )

      IF HB_ISARRAY( cExitKeys ) /* Harbour extension */
         IF Len( cExitKeys ) > 25
            ASize( cExitKeys, 25 )
         ENDIF
      ELSEIF HB_ISSTRING( cExitKeys )
         cExitKeys := Left( cExitKeys, 25 )
      ELSE
         cExitKeys := {}
      ENDIF

      t_nHandle := FOpen( cInFile )

      rval := FError()

      IF rval == 0
         rval := _FT_DFINIT( t_nHandle, nTop, nLeft, nBottom, nRight, ;
            nStart, nCNormal, nCHighlight, cExitKeys, ;
            lBrowse, nColSkip, nRMargin, nBuffSize )
      ENDIF
   ELSE
      rval := 2       // simulate a file-not-found DOS file error
   ENDIF

   RETURN rval

FUNCTION FT_DFCLOSE()

   IF t_nHandle != F_ERROR
      _FT_DFCLOS()

      FClose( t_nHandle )

      t_nHandle := F_ERROR
   ENDIF

   RETURN NIL
