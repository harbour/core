/* NAppGUI Designer types */

#include <gui/gui.hxx>

#ifndef __DESIGNER_HXX__
#define __DESIGNER_HXX__

typedef struct _dlabel_t DLabel;
typedef struct _dcolumn_t DColumn;
typedef struct _drow_t DRow;
typedef struct _dcell_t DCell;
typedef struct _dcell_content_t DCellContent;
typedef struct _dlayout_t DLayout;
typedef struct _dform_t DForm;

typedef enum _celltype_t
{
    ekCELL_TYPE_EMPTY,
    ekCELL_TYPE_LABEL,
    ekCELL_TYPE_LAYOUT
} celltype_t;

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
    celltype_t type;
    DCellContent content;
    align_t halign;
    align_t valign;

    /* Non-editable properties */
    R2Df rect;
};

struct _dlayout_t
{
    real32_t margin_left;
    real32_t margin_top;
    ArrSt(DColumn) *cols;
    ArrSt(DRow) *rows;
    ArrSt(DCell) *cells;

    /* Non-editable properties */
    R2Df rect_left;
    R2Df rect_top;
};

DeclSt(DColumn);
DeclSt(DRow);
DeclSt(DCell);

#endif 

