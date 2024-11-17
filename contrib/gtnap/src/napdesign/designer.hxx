/* NAppGUI Designer types */

#include <nform/nform.hxx>

#ifndef __DESIGNER_HXX__
#define __DESIGNER_HXX__

typedef struct _desiger_t Designer;
typedef struct _dcolumn_t DColumn;
typedef struct _drow_t DRow;
typedef struct _dcell_t DCell;
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

struct _dcolumn_t
{
    real32_t width;
    R2Df margin_rect;
};

struct _drow_t
{
    real32_t height;
    R2Df margin_rect;
};

struct _dcell_t
{
    DLayout *sublayout;
    R2Df rect;
    R2Df content_rect;
};

struct _dlayout_t
{
    FLayout *flayout;
    ArrSt(DColumn) *cols;
    ArrSt(DRow) *rows;
    ArrSt(DCell) *cells;
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
