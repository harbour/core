/*
 * This is an original work by Mike Taylor and is placed in the
 * public domain.
 *
 * Modification history:
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

      DO CASE
      CASE HB_ISARRAY( cExitKeys )  /* HB_EXTENSION - Harbour extension */
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
         rval := _ft_DFInit( t_nHandle, ;
            hb_defaultValue( nTop        , 0 ), ;
            hb_defaultValue( nLeft       , 0 ), ;
            hb_defaultValue( nBottom     , MaxRow() ), ;
            hb_defaultValue( nRight      , MaxCol() ), ;
            hb_defaultValue( nStart      , 1 ), ;
            hb_defaultValue( nCNormal    , 7 ), ;
            hb_defaultValue( nCHighlight , 15 ), ;
            cExitKeys, ;
            hb_defaultValue( lBrowse     , .F. ), ;
            hb_defaultValue( nColSkip    , 1 ), ;
            hb_defaultValue( nRMargin    , 255 ), ;
            hb_defaultValue( nBuffSize   , 4096 ) )
      ENDIF
   ELSE
      rval := 2  // simulate a file-not-found MS-DOS file error
   ENDIF

   RETURN rval

PROCEDURE ft_DFClose()

   IF t_nHandle != F_ERROR
      _ft_DFClos()

      FClose( t_nHandle )

      t_nHandle := F_ERROR
   ENDIF

   RETURN
