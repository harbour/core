// HBAWS example 'download'
// From \contrib\hbaws\tests\harbour
// Download a file from AWS-S3.
// ..\..\..\..\bin\win\mingw64\hbmk2 download.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 download.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 download.prg credentials.prg hbaws.hbc
#include "hbaws.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
STAT FUNCTION LOCAL_FILE( C_FileName )
***********************************
#if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
    return DiskName() + ":\" + CurDir() + "\" + C_FileName
#else
    return "/" + CurDir() + "/" + C_FileName
#endif

***********************************
PROCEDURE Main()
***********************************
LOCAL L_OK := .F.
LOCAL C_KEY := ""
LOCAL C_LOCAL_FILE := ""
LOCAL C_ERR := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'download' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

C_KEY := "img/standard_streams.svg"
C_LOCAL_FILE := LOCAL_FILE("standard_streams.svg")

? "Running 'HBAWS_S3_DOWNLOAD' key: '" + C_KEY  + "' from bucket '" + AWS_Bucket() + "' into '" + C_LOCAL_FILE + "'"
L_OK := HBAWS_S3_DOWNLOAD(@C_ERR, AWS_Bucket(), C_KEY, C_LOCAL_FILE)

IF L_OK
    ? "Download success"
ELSE
    ? "Error: " + C_ERR
ENDIF

HBAWS_FINISH()
RETURN
