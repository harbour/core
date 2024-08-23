// HBAWS example 'copy'
// From \contrib\hbaws\tests\harbour
// Copy a file from AWS-S3 bucket to other bucket in a single request.
// ..\..\..\..\bin\win\mingw64\hbmk2 copy.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 copy.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 copy.prg credentials.prg hbaws.hbc
#include "hbaws.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
PROCEDURE Main()
***********************************
LOCAL L_OK := .F.
LOCAL C_SRC_KEY := ""
LOCAL C_DEST_KEY := ""
LOCAL C_DEST_CONTENT_TYPE := ""
LOCAL C_ERR := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'copy simple' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

C_SRC_KEY := "upload/heap_free.svg"
C_DEST_KEY := "copy/heap_free.svg"
C_DEST_CONTENT_TYPE := "image/svg+xml"

? "Running 'HBAWS_S3_COPY_SIMPLE' in bucket '" + AWS_Bucket() + "'"
? "Copy file: '" +  C_SRC_KEY + "' to '" + C_DEST_KEY + "'"
L_OK := HBAWS_S3_COPY_SIMPLE(@C_ERR, AWS_Bucket(), C_SRC_KEY, AWS_Bucket(), C_DEST_KEY, C_DEST_CONTENT_TYPE, STORAGE_STANDARD)

IF L_OK
    ? "Copy success"
ELSE
    ? "Error: " + C_ERR
ENDIF

HBAWS_FINISH()
RETURN
