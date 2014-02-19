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

FUNCTION ft_DFSetup( cInFile, nTop, nLeft, nBottom, nRight, ;
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

      DO CASE
      CASE HB_ISARRAY( cExitKeys ) /* Harbour extension */
         IF Len( cExitKeys ) > 25
            ASize( cExitKeys, 25 )
         ENDIF
      CASE HB_ISSTRING( cExitKeys )
         cExitKeys := Left( cExitKeys, 25 )
      OTHERWISE
         cExitKeys := {}
      ENDCASE

      t_nHandle := FOpen( cInFile )

      IF ( rval := FError() ) == 0
         rval := _ft_DFInit( t_nHandle, nTop, nLeft, nBottom, nRight, ;
            nStart, nCNormal, nCHighlight, cExitKeys, ;
            lBrowse, nColSkip, nRMargin, nBuffSize )
      ENDIF
   ELSE
      rval := 2       // simulate a file-not-found DOS file error
   ENDIF

   RETURN rval

PROCEDURE ft_DFClose()

   IF t_nHandle != F_ERROR
      _ft_DFClos()

      FClose( t_nHandle )

      t_nHandle := F_ERROR
   ENDIF

   RETURN
