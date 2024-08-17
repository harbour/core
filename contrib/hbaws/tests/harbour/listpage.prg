// HBAWS example 'listpage'
// From \contrib\hbaws\tests\harbour
// List the content of a AWS bucket page by page, using continuation tokens.
// ..\..\..\..\bin\win\mingw64\hbmk2 listpage.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 listpage.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 listpage.prg credentials.prg hbaws.hbc
#include "hbaws.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
PROCEDURE Main()
***********************************
LOCAL L_OK := .F.
LOCAL C_ERR := ""
LOCAL C_PREFIX := ""
LOCAL C_START_AFTER := NIL
LOCAL C_NEXT_CONTINUATION_TOKEN := ""
LOCAL V_OBJS := {}
LOCAL V_ITEM := {}
LOCAL N_CONT := 1
LOCAL N_MAX_KEYS := 30
LOCAL N_TOTAL_KEYS := 0
LOCAL N_PAGE := 1

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'listpage' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

C_PREFIX := ""
? "Running 'HBAWS_S3_LIST_PAGE' in bucket '" + AWS_Bucket() + "' with prefix '" + C_PREFIX + "'"

DO WHILE C_NEXT_CONTINUATION_TOKEN # NIL

    V_OBJS := HBAWS_S3_LIST_PAGINATED(@C_ERR, AWS_Bucket(), C_PREFIX, C_START_AFTER, N_MAX_KEYS, C_NEXT_CONTINUATION_TOKEN, @C_NEXT_CONTINUATION_TOKEN)
    IF Len(V_OBJS) != 0
        ? ""
        ? "--> Page: " + hb_ntos(N_PAGE) + " (" + hb_ntos(N_TOTAL_KEYS + 1) + "-" + hb_ntos(N_TOTAL_KEYS + Len(V_OBJS)) + ")"
        N_PAGE := N_PAGE + 1
        FOR N_Cont := 1 TO LEN(V_OBJS)
            V_Item := V_OBJS[N_Cont]
            ? "ITEM: " + hb_ntos(N_TOTAL_KEYS + N_Cont)
            ? " * S3Key: " + V_Item[OBJ_S3KEY]
        //     ? " * ContentSize: " + hb_ntos(V_Item[OBJ_CONTENT_SIZE])
        //     ? " * ContentType: " + V_Item[OBJ_CONTENT_TYPE]
        //     ? " * Date: " + DToC(V_Item[OBJ_DATE])
        //     ? " * Time: " + V_Item[OBJ_TIME]
        //     ? " * TimeZone: " + V_Item[OBJ_TIMEZONE]
        //     ? " * StorageClass: " + V_Item[OBJ_STORAGE_CLASS]
        //     ? " * IsRestore: " + hb_ValToStr(V_Item[OBJ_IS_RESTORE])
        //     ? " * RestoreDate: " + DToC(V_Item[OBJ_RESTORE_DATE])
        //     ? " * RestoreTime: " + V_Item[OBJ_RESTORE_TIME]
        //     ? " * RestoreTimeZone: " + V_Item[OBJ_RESTORE_TIMEZONE]
        //     ? " * ChecksumAlgorithm: " + V_Item[OBJ_CHECKSUM_ALGORITHM]
        //     ? " * ETag: " + V_Item[OBJ_ETAG]
        NEXT
        ? ""
        N_TOTAL_KEYS := N_TOTAL_KEYS + Len(V_OBJS)

    ELSE
        IF LEN(C_ERR)==0
            ? "No files found in this page. Finish"
        ELSE
            ? "Error: " + C_ERR
        ENDIF
        C_NEXT_CONTINUATION_TOKEN := NIL
    ENDIF
ENDDO

HBAWS_FINISH()
RETURN
