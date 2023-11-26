/* LibreOffice-SDK C-wrapper */

#ifndef __OFFICESDK_HXX__
#define __OFFICESDK_HXX__

#include <core/core.hxx>

typedef struct _sheetdoc_t SheetDoc;
typedef struct _sheet_t Sheet;
typedef struct _sheetcell_t SheetCell;

typedef enum _sdkres_t
{
    ekSDKRES_OK     = 1,
    ekSDKRES_NO_ENVAR,
    ekSDKRES_PROC_KILL_FAIL,
    ekSDKRES_PROC_INIT_FAIL,
    ekSDKRES_CONECT_FAIL,
    ekSDKRES_COMPONENT_LOADER,
    ekSDKRES_OPEN_FILE_ERROR,
    ekSDKRES_SAVE_FILE_ERROR
} sdkres_t;

typedef enum _fileformat_t
{
    ekFORMAT_OPEN_OFFICE,
    ekFORMAT_PDF
} fileformat_t;

#endif
