// HBAWS example 'copym'
// From \contrib\hbaws\tests\harbour
// Upload a file to AWS-S3, using multipart serveral request.
// ..\..\..\..\bin\win\mingw64\hbmk2 copym.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 copym.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 copym.prg credentials.prg hbaws.hbc
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
LOCAL N_CHUNK_SIZE := 0
LOCAL N_RETRIES := 0
LOCAL C_ERR := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'copy multipart' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

C_SRC_KEY := "upload/ubuntu-22.04-desktop-amd64.iso"
C_DEST_KEY := "copy/ubuntu-22.04-desktop-amd64.iso"
C_DEST_CONTENT_TYPE := "application/zip"
N_CHUNK_SIZE := 1024 * 1024  // In bytes
N_RETRIES := 2

? "Running 'HBAWS_S3_COPY_MULTIPART' in bucket '" + AWS_Bucket() + "'"
? "Copy file: '" +  C_SRC_KEY + "' to '" + C_DEST_KEY + "'"
L_OK := HBAWS_S3_COPY_MULTIPART(@C_ERR, AWS_Bucket(), C_SRC_KEY, AWS_Bucket(), C_DEST_KEY, C_DEST_CONTENT_TYPE, STORAGE_STANDARD, N_CHUNK_SIZE, N_RETRIES)

IF L_OK
    ? "Copy success"
ELSE
    ? "Error: " + C_ERR
ENDIF

HBAWS_FINISH()
RETURN
