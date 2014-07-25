/* This is an original work from 2012 by Viktor Szakats (vszakats.net/harbour)
   and is placed in the public domain. */

#include "fileio.ch"

#define ERR_WRONG_PARAMETERS  -1
#define ERR_NO_ERROR          0
#define ERR_DRIVE_NOT_READY   1  // unused
#define ERR_UNFORMATTED       2  // unused
#define ERR_WRITE_PROTECTED   3

/* NOTE: Harbour port accepts a path as a string for checking
         for writability. It also won't detected unformatted
         state and "not ready" state. [vszakats] */

FUNCTION ft_FlopTst( nDriveNum )

   LOCAL cFileName
   LOCAL fhnd

   DO CASE
   CASE HB_ISNUMERIC( nDriveNum )
      cFileName := Chr( Asc( "A" ) + nDriveNum ) + hb_osDriveSeparator()
   CASE HB_ISSTRING( nDriveNum )
      cFileName := nDriveNum
   OTHERWISE
      RETURN ERR_WRONG_PARAMETERS
   ENDCASE

   cFileName := hb_DirSepAdd( cFileName ) + "nf$rwtst.tmp"

   IF hb_FileExists( cFileName )
      IF ( fhnd := FOpen( cFileName, FO_DENYNONE + FO_READWRITE ) ) == F_ERROR
         RETURN ERR_WRITE_PROTECTED
      ENDIF
      FClose( fhnd )
   ELSE
      IF ( fhnd := hb_FCreate( cFileName,, FO_DENYNONE + FO_READWRITE ) ) == F_ERROR
         RETURN ERR_WRITE_PROTECTED
      ENDIF
      FClose( fhnd )

      FErase( cFileName )
   ENDIF

   RETURN ERR_NO_ERROR
