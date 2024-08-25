// HBAWS example 'restore'
// From \contrib\hbaws\tests\harbour
// Restore a file from AWS-S3.
// ..\..\..\..\bin\win\mingw64\hbmk2 restore.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 restore.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 restore.prg credentials.prg hbaws.hbc
#include "hbaws.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
PROCEDURE Main()
***********************************
LOCAL L_OK := .F.
LOCAL C_KEY := ""
LOCAL N_NUM_DAYS := 0
LOCAL C_ERR := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'restore' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

C_KEY := "img/str_vs_ptr.svg"
N_NUM_DAYS := 5
? "Running 'HBAWS_S3_RESTORE' key: '" + C_KEY  + "' from bucket '" + AWS_Bucket() + "' " + hb_ntos(N_NUM_DAYS) + " days."
L_OK := HBAWS_S3_RESTORE(@C_ERR, AWS_Bucket(), C_KEY, N_NUM_DAYS, TIER_BULK)

IF L_OK
    ? "Restore success"
ELSE
    ? "Error: " + C_ERR
ENDIF

HBAWS_FINISH()
RETURN
