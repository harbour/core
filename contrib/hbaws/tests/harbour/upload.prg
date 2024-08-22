// HBAWS example 'upload'
// From \contrib\hbaws\tests\harbour
// List the content of a AWS bucket page by page, using continuation tokens.
// ..\..\..\..\bin\win\mingw64\hbmk2 upload.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 upload.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 upload.prg credentials.prg hbaws.hbc
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
LOCAL C_LOCAL_FILE := ""
LOCAL C_KEY := ""
LOCAL C_CONTENT_TYPE := ""
LOCAL C_ERR := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'upload simple' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

// Put the file to upload in 'hbaws/tests/harbour/upload'
C_LOCAL_FILE := LOCAL_FILE("upload/heap_free.svg")
C_KEY := "upload/heap_free.svg"
C_CONTENT_TYPE := "image/svg+xml"

? "Running 'HBAWS_S3_UPLOAD_SIMPLE' in bucket '" + AWS_Bucket() + "'"
? "Upload file: '" +  C_LOCAL_FILE + "' with key '" + C_KEY + "'"

L_OK := HBAWS_S3_UPLOAD_SIMPLE(@C_ERR, AWS_Bucket(), C_LOCAL_FILE, C_KEY, C_CONTENT_TYPE, STORAGE_STANDARD)

IF L_OK
    ? "Upload success"
ELSE
    ? "Error: " + C_ERR
ENDIF

HBAWS_FINISH()
RETURN
