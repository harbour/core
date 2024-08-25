// HBAWS example 'delete'
// From \contrib\hbaws\tests\harbour
// Delete a file from AWS-S3.
// ..\..\..\..\bin\win\mingw64\hbmk2 delete.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 delete.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 delete.prg credentials.prg hbaws.hbc
#include "hbaws.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
PROCEDURE Main()
***********************************
LOCAL L_OK := .F.
LOCAL C_KEY := ""
LOCAL C_ERR := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'delete' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

C_KEY := "copy/ubuntu-22.04-desktop-amd64.iso"

? "Running 'HBAWS_S3_DELETE' key: '" + C_KEY  + "' from bucket '" + AWS_Bucket() + "'"
L_OK := HBAWS_S3_DELETE(@C_ERR, AWS_Bucket(), C_KEY)

IF L_OK
    ? "Delete success"
ELSE
    ? "Error: " + C_ERR
ENDIF

HBAWS_FINISH()
RETURN
