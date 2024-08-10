// HBAWS example 'list'
// From \contrib\hbaws\tests\harbour
// ..\..\..\..\bin\win\mingw64\hbmk2 list.prg credentials.prg hbaws.hbc -comp=mingw64
#include "hbaws.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
PROCEDURE Main()
***********************************
LOCAL L_OK := .F.
LOCAL C_ERR := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'list' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + HBAWS_LAST_ERROR()
    RETURN
ENDIF

L_OK := HBAWS_S3_LIST(AWS_Bucket(), "")

IF L_OK == .F.
    ? "Error in S3 List: " + HBAWS_LAST_ERROR()
    RETURN
ENDIF

HBAWS_FINISH()
RETURN
