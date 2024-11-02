/* NAppGUI Designer types */

#include <gui/gui.hxx>

#ifndef __DESIGNER_HXX__
#define __DESIGNER_HXX__

typedef struct _desiger_t Designer;
typedef struct _dlabel_t DLabel;
typedef struct _dcolumn_t DColumn;
typedef struct _drow_t DRow;
typedef struct _dcell_t DCell;
typedef struct _dcell_content_t DCellContent;
typedef struct _dlayout_t DLayout;
typedef struct _dform_t DForm;
typedef struct _dselect_t DSelect;

typedef enum _widget_t
{
    ekWIDGET_SELECT = 0,
    ekWIDGET_GRID_LAYOUT,
    ekWIDGET_LABEL,
    ekWIDGET_BUTTON,
    ekWIDGET_CHECKBOX,
    ekWIDGET_EDITBOX
} widget_t;

typedef enum _celltype_t
{
    ekCELL_TYPE_EMPTY,
    ekCELL_TYPE_LABEL,
    ekCELL_TYPE_LAYOUT
} celltype_t;

typedef enum _layelem_t
{
    ekLAYELEM_MARGIN_LEFT,
    ekLAYELEM_MARGIN_TOP,
    ekLAYELEM_MARGIN_RIGHT,
    ekLAYELEM_MARGIN_BOTTOM,
    ekLAYELEM_MARGIN_COLUMN,
    ekLAYELEM_MARGIN_ROW,
    ekLAYELEM_CELL
} layelem_t;

typedef enum _halign_t
{
    ekHALIGN_LEFT,
    ekHALIGN_CENTER,
    ekHALIGN_RIGHT,
    ekHALIGN_JUSTIFY
} halign_t;

typedef enum _valign_t
{
    ekVALIGN_TOP,
    ekVALIGN_CENTER,
    ekVALIGN_BOTTOM,
    ekVALIGN_JUSTIFY
} valign_t;

struct _dlabel_t
{
    String *text;
};

struct _dcolumn_t
{
    real32_t margin_right;

    /* Non-editable properties */
    real32_t width;
    R2Df margin_rect;
};

struct _drow_t
{
    real32_t margin_bottom;

    /* Non-editable properties */
    real32_t height;
    R2Df margin_rect;
};

struct _dcell_content_t
{
    DLabel *label;
    DLayout *layout;
};

struct _dcell_t
{
    String *name;
    celltype_t type;
    halign_t halign;
    valign_t valign;
    DCellContent content;

    /* Non-editable properties */
    R2Df rect;
    R2Df content_rect;
};

struct _dlayout_t
{
    String *name;
    real32_t margin_left;
    real32_t margin_top;
    real32_t margin_right;
    real32_t margin_bottom;
    ArrSt(DColumn) *cols;
    ArrSt(DRow) *rows;
    ArrSt(DCell) *cells;

    /* Non-editable properties */
    R2Df rect;
    R2Df rect_left;
    R2Df rect_top;
    R2Df rect_right;
    R2Df rect_bottom;
};

struct _dselect_t
{
    DLayout *layout;
    layelem_t elem;
    uint32_t col;
    uint32_t row;
};

DeclSt(DColumn);
DeclSt(DRow);
DeclSt(DCell);
DeclSt(DSelect);

#endif
