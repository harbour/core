/* LibreOffice-SDK C-wrapper */

#ifndef __OFFICESDK_HXX__
#define __OFFICESDK_HXX__

#include <core/core.hxx>

typedef struct _write_t Writer;
typedef struct _sheet_t Sheet;
typedef struct _ddraw_t Draw;
typedef struct _acces_t Access;

typedef enum _sdkres_t
{
    ekSDKRES_OK     = 1,
    ekSDKRES_NO_ENVAR,
    ekSDKRES_PROC_KILL_FAIL,
    ekSDKRES_PROC_INIT_FAIL,
    ekSDKRES_CONECT_FAIL,
    ekSDKRES_COMPONENT_LOADER,
    ekSDKRES_CREATE_FILE_ERROR,
    ekSDKRES_OPEN_FILE_ERROR,
    ekSDKRES_SAVE_FILE_ERROR,
    ekSDKRES_CLOSE_DOC_ERROR,
    ekSDKRES_ACCESS_DOC_ERROR,
    ekSDKRES_ACCESS_CELL_ERROR,
    ekSDKRES_EDIT_CELL_ERROR,
    ekSDKRES_FORMAT_CELL_ERROR,
    ekSDKRES_ACCESS_COLUMN_ERROR,
    ekSDKRES_FORMAT_COLUMN_ERROR,
    ekSDKRES_ACCESS_ROW_ERROR,
    ekSDKRES_FORMAT_ROW_ERROR,
    ekSDKRES_TEXT_PROPERTY_ERROR,
    ekSDKRES_TEXT_ADD_ERROR,
    ekSDKRES_PAGE_PROPERTY_ERROR,
    ekSDKRES_PRINTER_CONFIG_ERROR,
    ekSDKRES_PRINT_ERROR
} sdkres_t;

typedef enum _halign_t
{
    ekHALIGN_LEFT               = 1,
    ekHALIGN_CENTER,
    ekHALIGN_RIGHT,
    ekHALIGN_JUSTIFY
} halign_t;

typedef enum _valign_t
{
    ekVALIGN_TOP                = 1,
    ekVALIGN_CENTER,
    ekVALIGN_BOTTOM
} valign_t;

typedef enum _numformat_t
{
    ekNUMFORMAT_INT         = 1,
    ekNUMFORMAT_INT_1000,
    ekNUMFORMAT_DEC2,
    ekNUMFORMAT_DEC2_1000,
    ekNUMFORMAT_PERC_INT,
    ekNUMFORMAT_PERC_DEC2,

    ekNUMFORMAT_DATE_SYS_DDMMM,
    ekNUMFORMAT_DATE_SYS_DDMMYY,
    ekNUMFORMAT_DATE_SYS_DDMMYYYY,
    ekNUMFORMAT_DATE_SYS_DMMMMYYYY,
    ekNUMFORMAT_DATE_SYS_DMMMYY,
    ekNUMFORMAT_DATE_SYS_DMMMYYYY,
    ekNUMFORMAT_DATE_SYS_MMYY,
    ekNUMFORMAT_DATE_SYS_NNDMMMMYYYY,
    ekNUMFORMAT_DATE_SYS_NNDMMMYY,
    ekNUMFORMAT_DATE_SYS_NNNNDMMMMYYYY

} numformat_t;

typedef enum _linestyle_t
{
    ekLINE_STYLE_NONE = 1,
    ekLINE_STYLE_SOLID,
    ekLINE_STYLE_DOTTED,
    ekLINE_STYLE_DASHED,
    ekLINE_STYLE_DOUBLE,
    ekLINE_STYLE_THINTHICK_SMALLGAP,
    ekLINE_STYLE_THINTHICK_MEDIUMGAP,
    ekLINE_STYLE_THINTHICK_LARGEGAP,
    ekLINE_STYLE_THICKTHIN_SMALLGAP,
    ekLINE_STYLE_THICKTHIN_MEDIUMGAP,
    ekLINE_STYLE_THICKTHIN_LARGEGAP,
    ekLINE_STYLE_EMBOSSED,
    ekLINE_STYLE_ENGRAVED,
    ekLINE_STYLE_OUTSET,
    ekLINE_STYLE_INSET,
    ekLINE_STYLE_FINE_DASHED,
    ekLINE_STYLE_DOUBLE_THIN,
    ekLINE_STYLE_DASH_DOT,
    ekLINE_STYLE_DASH_DOT_DOT
} linestyle_t;

typedef enum _fileformat_t
{
    ekFORMAT_OPEN_OFFICE = 1,
    ekFORMAT_PDF
} fileformat_t;

typedef enum _textspace_t
{
    ekTEXT_SPACE_HEADER = 1,
    ekTEXT_SPACE_FOOTER,
    ekTEXT_SPACE_PAGE
} textspace_t;

typedef enum _anchortype_t
{
    ekANCHOR_AT_PARAGRAPH = 1,
    ekANCHOR_AS_CHARACTER,
    ekANCHOR_AT_PAGE,
    ekANCHOR_AT_FRAME,
    ekANCHOR_AT_CHARACTER
} anchortype_t;

typedef enum _paperorient_t
{
    ekPAPERORIENT_PORTRAIT = 1,
    ekPAPERORIENT_LANSCAPE
} paperorient_t;

typedef enum _paperformat_t
{
    ekPAPERFORMAT_A3 = 1,
    ekPAPERFORMAT_A4,
    ekPAPERFORMAT_A5,
    ekPAPERFORMAT_B4,
    ekPAPERFORMAT_B5,
    ekPAPERFORMAT_LETTER,
    ekPAPERFORMAT_LEGAL,
    ekPAPERFORMAT_TABLOID,
    ekPAPERFORMAT_USER
} paperformat_t;

#endif
