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
LOCAL V_OBJS := {}
LOCAL V_ITEM := {}
LOCAL N_CONT := 1

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'list' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + HBAWS_LAST_ERROR()
    RETURN
ENDIF

? "Running HBAWS_S3_LIST_ALL"
V_OBJS := HBAWS_S3_LIST_ALL(C_ERR, AWS_Bucket(), "")

IF Len(V_OBJS) != 0
    ? "Num Files found: " + hb_ntos(LEN(V_OBJS))
    FOR N_Cont := 1 TO LEN(V_OBJS)
        V_Item := V_OBJS[N_Cont]
        ? "ITEM: " + hb_ntos(N_Cont)
        ? " * " + V_Item[1]
    NEXT
ELSE
    IF LEN(C_ERR)==0
        ? "No files found"
    ELSE
        ? "Error: " + C_ERR
    ENDIF
ENDIF

HBAWS_FINISH()
RETURN
