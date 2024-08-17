// HBAWS example 'listall'
// From \contrib\hbaws\tests\harbour
// List all content of a AWS bucket, breaking the 1000 items limit.
// ..\..\..\..\bin\win\mingw64\hbmk2 listall.prg credentials.prg hbaws.hbc -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 listall.prg credentials.prg hbaws.hbc -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 listall.prg credentials.prg hbaws.hbc
#include "hbaws.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
PROCEDURE Main()
***********************************
LOCAL L_OK := .F.
LOCAL C_ERR := ""
LOCAL C_PREFIX := ""
LOCAL V_OBJS := {}
LOCAL V_ITEM := {}
LOCAL N_CONT := 1

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "Running AWS 'listall' example"
? "Initializing AWS client"
L_OK := HBAWS_INIT(@C_ERR, AWS_AccessKey(), AWS_Secret())

IF L_OK == .F.
    ? "Error init AWS: " + C_ERR
    RETURN
ENDIF

C_PREFIX := ""
? "Running 'HBAWS_S3_LIST_ALL' in bucket '" + AWS_Bucket() + "' with prefix '" + C_PREFIX + "'"
V_OBJS := HBAWS_S3_LIST_ALL(@C_ERR, AWS_Bucket(), C_PREFIX)

IF Len(V_OBJS) != 0
    ? "Num Files found: " + hb_ntos(LEN(V_OBJS))
    FOR N_Cont := 1 TO LEN(V_OBJS)
        V_Item := V_OBJS[N_Cont]
        ? "ITEM: " + hb_ntos(N_Cont)
        ? " * S3Key: " + V_Item[OBJ_S3KEY]
        // ? " * ContentSize: " + hb_ntos(V_Item[OBJ_CONTENT_SIZE])
        // ? " * ContentType: " + V_Item[OBJ_CONTENT_TYPE]
        // ? " * Date: " + DToC(V_Item[OBJ_DATE])
        // ? " * Time: " + V_Item[OBJ_TIME]
        // ? " * TimeZone: " + V_Item[OBJ_TIMEZONE]
        // ? " * StorageClass: " + V_Item[OBJ_STORAGE_CLASS]
        // ? " * IsRestore: " + hb_ValToStr(V_Item[OBJ_IS_RESTORE])
        // ? " * RestoreDate: " + DToC(V_Item[OBJ_RESTORE_DATE])
        // ? " * RestoreTime: " + V_Item[OBJ_RESTORE_TIME]
        // ? " * RestoreTimeZone: " + V_Item[OBJ_RESTORE_TIMEZONE]
        // ? " * ChecksumAlgorithm: " + V_Item[OBJ_CHECKSUM_ALGORITHM]
        // ? " * ETag: " + V_Item[OBJ_ETAG]
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
