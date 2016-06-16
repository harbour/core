/* This is an original work by Mike Taylor and is placed in the public domain.

      Rev 1.3   17 Aug 1991 15:24:14   GLENN
   Don Caton corrected some spelling errors in the doc

      Rev 1.2   15 Aug 1991 23:03:24   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:51:32   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:01:08   GLENN
   Nanforum Toolkit
 */

#include "fileio.ch"

THREAD STATIC t_hFile

FUNCTION ft_DFSetup( cInFile, nTop, nLeft, nBottom, nRight, ;
      nStart, nCNormal, nCHighlight, cExitKeys, ;
      lBrowse, nColSkip, nRMargin, nBuffSize )

   LOCAL rval

   IF hb_vfExists( cInFile )

      DO CASE
      CASE HB_ISARRAY( cExitKeys )  // HB_EXTENSION - Harbour extension
         IF Len( cExitKeys ) > 25
            ASize( cExitKeys, 25 )
         ENDIF
      CASE HB_ISSTRING( cExitKeys )
         cExitKeys := Left( cExitKeys, 25 )
      OTHERWISE
         cExitKeys := {}
      ENDCASE

      t_hFile := hb_vfOpen( cInFile, FO_READ )

      IF ( rval := FError() ) == 0
         rval := _ft_DFInit( t_hFile, nTop, nLeft, nBottom, nRight, ;
            nStart, nCNormal, nCHighlight, cExitKeys, ;
            lBrowse, nColSkip, nRMargin, nBuffSize )
      ENDIF
   ELSE
      rval := 2  // simulate a file-not-found MS-DOS file error
   ENDIF

   RETURN rval

PROCEDURE ft_DFClose()

   IF t_hFile != NIL
      _ft_DFClos()

      hb_vfClose( t_hFile )

      t_hFile := NIL
   ENDIF

   RETURN
