/* Design layout (editable parameters) */

#include "dlayout.h"
#include <gui/label.h>
#include <gui/layout.h>
#include <gui/layouth.h>
#include <geom2d/r2d.h>
#include <geom2d/v2d.h>
#include <draw2d/color.h>
#include <draw2d/draw.h>
#include <draw2d/drawg.h>
#include <core/arrst.h>
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/bmem.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

static void i_init_empty_column(DColumn *col)
{
    cassert_no_null(col);
    col->margin_right = 0;
}

/*---------------------------------------------------------------------------*/

static void i_init_empty_row(DRow *row)
{
    cassert_no_null(row);
    row->margin_bottom = 0;
}

/*---------------------------------------------------------------------------*/

static void i_remove_column(DColumn *column)
{
    dbind_remove(column, DColumn);
}

/*---------------------------------------------------------------------------*/

static void i_remove_row(DRow *row)
{
    dbind_remove(row, DRow);
}

/*---------------------------------------------------------------------------*/

static void i_remove_cell(DCell *cell)
{
    dbind_remove(cell, DCell);
}

/*---------------------------------------------------------------------------*/

static void i_init_empty_cell(DCell *cell)
{
    cassert_no_null(cell);
    bmem_zero(cell, DCell);
    cell->type = ekCELL_TYPE_EMPTY;
    cell->halign = ekLEFT;
    cell->valign = ekTOP;
}

/*---------------------------------------------------------------------------*/

DLayout *dlayout_create(const uint32_t ncols, const uint32_t nrows)
{
    DLayout *layout = dbind_create(DLayout);
    cassert(ncols > 0);
    cassert(nrows > 0);

    /* Add columns to layout */
    {
        uint32_t i = 0;
        for (i = 0; i < ncols; ++i)
        {
            DColumn *col = arrst_new(layout->cols, DColumn);
            i_init_empty_column(col);
        }
    }

    /* Add rows to layout */
    {
        uint32_t i = 0;
        for (i = 0; i < nrows; ++i)
        {
            DRow *row = arrst_new(layout->rows, DRow);
            i_init_empty_row(row);
        }
    }

    /* Add cells to layout */
    {
        uint32_t n = ncols * nrows, i = 0;
        for (i = 0; i < n; ++i)
        {
            DCell *cell = arrst_new(layout->cells, DCell);
            i_init_empty_cell(cell);
        }
    }

    return layout;
}

/*---------------------------------------------------------------------------*/

void dlayout_destroy(DLayout **layout)
{
    dbind_destroy(layout, DLayout);
}

/*---------------------------------------------------------------------------*/

void dlayout_margin_top(DLayout *layout, const real32_t margin)
{
    cassert_no_null(layout);
    layout->margin_top = margin;
}

/*---------------------------------------------------------------------------*/

void dlayout_margin_bottom(DLayout *layout, const real32_t margin)
{
    DRow *row = NULL;
    cassert_no_null(layout);
    row = arrst_last(layout->rows, DRow);
    row->margin_bottom = margin;
}

/*---------------------------------------------------------------------------*/

void dlayout_margin_left(DLayout *layout, const real32_t margin)
{
    cassert_no_null(layout);
    layout->margin_left = margin;
}

/*---------------------------------------------------------------------------*/

void dlayout_margin_right(DLayout *layout, const real32_t margin)
{
    DColumn *col = NULL;
    cassert_no_null(layout);
    col = arrst_last(layout->cols, DColumn);
    col->margin_right = margin;
}

/*---------------------------------------------------------------------------*/

void dlayout_margin_col(DLayout *layout, const uint32_t col, const real32_t margin)
{
    DColumn *dcol = NULL;
    cassert_no_null(layout);
    cassert(col < arrst_size(layout->cols, DColumn) - 1);
    dcol= arrst_get(layout->cols, col, DColumn);
    dcol->margin_right = margin;
}

/*---------------------------------------------------------------------------*/

void dlayout_margin_row(DLayout *layout, const uint32_t row, const real32_t margin)
{
    DRow *drow = NULL;
    cassert_no_null(layout);
    cassert(row < arrst_size(layout->rows, DRow) - 1);
    drow = arrst_get(layout->rows, row, DRow);
    drow->margin_bottom = margin;
}

/*---------------------------------------------------------------------------*/

void dlayout_insert_col(DLayout *layout, const uint32_t col)
{
    uint32_t ncols = 0, nrows = 0, i = 0;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, DColumn);
    nrows = arrst_size(layout->rows, DRow);

    /* Insert and init (empty) the new cells */
    for (i = 0; i < nrows; ++i)
    {
        uint32_t inspos = ((ncols + 1) * i) + col;
        DCell *cell = arrst_insert_n(layout->cells, inspos, 1, DCell);
        i_init_empty_cell(cell);
    }

    /* Add a new column */
    {
        DColumn *ncol = arrst_insert_n(layout->cols, col, 1, DColumn);
        i_init_empty_column(ncol);
    }
}

/*---------------------------------------------------------------------------*/

void dlayout_remove_col(DLayout *layout, const uint32_t col)
{
    uint32_t ncols = 0, nrows = 0, i = 0;
    cassert_no_null(layout);
    cassert(col < arrst_size(layout->cols, DColumn));
    ncols = arrst_size(layout->cols, DColumn);
    nrows = arrst_size(layout->rows, DRow);

    /* Destroy the column cells */
    for (i = 0; i < nrows; ++i)
    {
        uint32_t delrow = nrows - i - 1;
        uint32_t delpos = (ncols * delrow) + col;
        arrst_delete(layout->cells, delpos, i_remove_cell, DCell);
    }

    /* Destroy the column */
    arrst_delete(layout->cols, col, i_remove_column, DColumn);
}

/*---------------------------------------------------------------------------*/

void dlayout_insert_row(DLayout *layout, const uint32_t row)
{
    uint32_t ncols = 0, nrows = 0, i = 0;
    uint32_t inspos = 0;
    DCell *cells = NULL;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, DColumn);
    nrows = arrst_size(layout->rows, DRow);
    /* Cells insert position */
    inspos = row * ncols;
    /* Cells array is in row-major order. All row cells are together in memory */
    cells = arrst_insert_n(layout->cells, inspos, ncols, DCell);

    /* Initialize the new cells (empty) */
    for (i = 0; i < ncols; ++i)
    {
        i_init_empty_cell(cells + i);
    }

    /* Add a new row */
    {
        DRow *nrow = arrst_insert_n(layout->rows, row, 1, DRow);
        i_init_empty_row(nrow);
    }
}

/*---------------------------------------------------------------------------*/

void dlayout_remove_row(DLayout *layout, const uint32_t row)
{
    uint32_t i, ncols = 0, nrows = 0;
    cassert_no_null(layout);
    cassert(row < arrst_size(layout->rows, DRow));
    ncols = arrst_size(layout->cols, DColumn);
    nrows = arrst_size(layout->rows, DRow);

    /* Destroy the row cells */
    for (i = 0; i < ncols; ++i)
    {
        uint32_t delcol = ncols - i - 1;
        uint32_t delpos = (ncols * row) + delcol;
        arrst_delete(layout->cells, delpos, i_remove_cell, DCell);
    }

    /* Destroy the row */
    arrst_delete(layout->rows, row, i_remove_row, DRow);
}

/*---------------------------------------------------------------------------*/

void dlayout_add_layout(DLayout *layout, DLayout *sublayout, const uint32_t col, const uint32_t row)
{
    uint32_t i, ncols = 0;
    DCell *cell = NULL;
    cassert_no_null(layout);
    cassert_no_null(sublayout);
    ncols = arrst_size(layout->cols, DColumn);
    i = row * ncols + col;
    cell = arrst_get(layout->cells, i, DCell);
    i_remove_cell(cell);
    i_init_empty_cell(cell);
    cell->type = ekCELL_TYPE_LAYOUT;
    cell->content.layout = sublayout;
    cell->valign = ekJUSTIFY;
    cell->halign = ekJUSTIFY;
}

/*---------------------------------------------------------------------------*/

void dlayout_add_label(DLayout *layout, DLabel *label, const uint32_t col, const uint32_t row)
{
    uint32_t i, ncols = 0;
    DCell *cell = NULL;
    cassert_no_null(layout);
    cassert_no_null(label);
    ncols = arrst_size(layout->cols, DColumn);
    i = row * ncols + col;
    cell = arrst_get(layout->cells, i, DCell);
    i_remove_cell(cell);
    i_init_empty_cell(cell);
    cell->type = ekCELL_TYPE_LABEL;
    cell->content.label = label;
    cell->valign = ekLEFT;
    cell->halign = ekCENTER;
}

/*---------------------------------------------------------------------------*/

static color_t i_color(void)
{
    static uint32_t index = 0;
    switch(index++ % 3) {
    case 0:
        return kCOLOR_RED;
    case 1:
        return kCOLOR_BLUE;
    case 2:
        return kCOLOR_GREEN;
    cassert_default();
    }

    return kCOLOR_DEFAULT;
}

/*---------------------------------------------------------------------------*/

Layout *dlayout_gui_layout(const DLayout *layout)
{
    uint32_t ncols = 0, nrows = 0;
    Layout *glayout = NULL;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, DColumn);
    nrows = arrst_size(layout->rows, DRow);
    glayout = layout_create(ncols, nrows);

    /* Layout border margins */
    {
        real32_t mt = layout->margin_top;
        real32_t ml = layout->margin_left;
        real32_t mr = 0;
        real32_t mb = 0;

        {
            const DColumn *col = arrst_get_const(layout->cols, ncols - 1, DColumn);
            const DRow *row = arrst_get_const(layout->rows, nrows - 1, DRow);
            mr = col->margin_right;
            mb = row->margin_bottom;
        }

        layout_margin4(glayout, mt, mr, mb, ml);
    }

    /* Inter-column margins */
    {
        uint32_t i = 0;
        for (i = 0; i < ncols - 1; ++i)
        {
            const DColumn *col = arrst_get_const(layout->cols, i, DColumn);
            layout_hmargin(glayout, i, col->margin_right);
        }
    }

    /* Inter-row margins */
    {
        uint32_t i = 0;
        for (i = 0; i < nrows - 1; ++i)
        {
            const DRow *row = arrst_get_const(layout->rows, i, DRow);
            layout_vmargin(glayout, i, row->margin_bottom);
        }
    }

    /* Cells */
    {
        uint32_t i, j;
        const DCell *cells = arrst_all_const(layout->cells, DCell);
        for (j = 0; j < nrows; ++j)
        {
            for(i = 0; i < ncols; ++i)
            {
                switch(cells->type) {
                case ekCELL_TYPE_EMPTY:
                    break;

                case ekCELL_TYPE_LABEL:
                {
                    DLabel *dlabel = cells->content.label;
                    Label *glabel = label_create();
                    label_text(glabel, tc(dlabel->text));
                    layout_label(glayout, glabel, i, j);
                    break;
                }

                case ekCELL_TYPE_LAYOUT:
                {
                    Layout *gsublayout = dlayout_gui_layout(cells->content.layout);
                    layout_layout(glayout, gsublayout, i, j);
                    break;
                }

                }

                cells += 1;
            }
        }
    }

    layout_bgcolor(glayout, i_color());
    return glayout;
}

/*---------------------------------------------------------------------------*/

void dlayout_synchro_visual(DLayout *layout, const Layout *glayout, const V2Df origin)
{
    DColumn *col = NULL;
    DRow *row = NULL;
    DCell *cell = NULL;
    uint32_t ncols, nrows, i, j;
    real32_t total_width = 0, total_height = 0;
    real32_t inner_width = 0, inner_height = 0;
    real32_t x, y;

    cassert_no_null(layout);
    col = arrst_all(layout->cols, DColumn);
    row = arrst_all(layout->rows, DRow);
    cell = arrst_all(layout->cells, DCell);
    ncols = arrst_size(layout->cols, DColumn); 
    nrows = arrst_size(layout->rows, DRow); 

    /* Compute the columns width and total layout widths */
    total_width = 0;
    inner_width = 0;
    for (i = 0; i < ncols; ++i)
    {
        col[i].width = layout_get_hsize(glayout, i);
        inner_width += col[i].width;
        if (i < ncols - 1)
            inner_width += col[i].margin_right;
        else
            total_width = inner_width + layout->margin_left + col[i].margin_right;
    }

    /* Compute the rows height and total layout heights */
    total_height = 0;
    inner_height = 0;
    for (j = 0; j < nrows; ++j)
    {
        row[j].height = layout_get_vsize(glayout, j);
        inner_height += row[j].height;
        if (j < nrows - 1)
            inner_height += row[j].margin_bottom;
        else
            total_height = inner_height + layout->margin_top + row[j].margin_bottom;
    }

    /* Global layout rectangle */
    layout->rect = r2df(origin.x, origin.y, total_width, total_height);

    /* Compute the margin rectangles */
    layout->rect_left = r2df(origin.x, origin.y, layout->margin_left, total_height);
    layout->rect_top = r2df(origin.x, origin.y, total_width, layout->margin_top);

    x = origin.x + layout->margin_left;
    for (i = 0; i < ncols; ++i)
    {
        x += col[i].width;
        col[i].margin_rect = r2df(x, origin.y, col[i].margin_right, total_height);
        x += col[i].margin_right;
    }

    y = origin.y + layout->margin_top;
    for (j = 0; j < nrows; ++j)
    {
        y += row[j].height;
        row[j].margin_rect = r2df(origin.x, y, total_width, row[j].margin_bottom);
        y += row[j].margin_bottom;
    }

    /* Compute the cells rectangles */
    {
        DCell *dcell = cell;
        y = origin.y + layout->margin_top;

        for (j = 0; j < nrows; ++j)
        {
            x = origin.x + layout->margin_left;
            for(i = 0; i < ncols; ++i)
            {
                dcell->rect = r2df(x, y, col[i].width, row[j].height);
                switch(dcell->type) {
                case ekCELL_TYPE_EMPTY:
                    break;

                case ekCELL_TYPE_LABEL:
                    break;

                case ekCELL_TYPE_LAYOUT:
                {
                    DLayout *sublayout = dcell->content.layout;
                    Layout *subglayout = layout_get_layout(cast(glayout, Layout), i, j);
                    dlayout_synchro_visual(sublayout, subglayout, v2df(x, y));
                    break;
                }

                }

                dcell += 1;
                x += col[i].width;
                x += col[i].margin_right;
            }

            y += row[j].height;
            y += row[j].margin_bottom;
        }
    }
}

/*---------------------------------------------------------------------------*/

void dlayout_elem_at_pos(const DLayout *layout, const real32_t x, const real32_t y, DSelect *sel)
{
    cassert_no_null(layout); 
    cassert_no_null(sel);

    if (r2d_containsf(&layout->rect, x, y) == FALSE)
    {
        sel->layout = NULL;
        sel->elem = ENUM_MAX(layelem_t);
        sel->col = UINT32_MAX;
        sel->row = UINT32_MAX;
        return;
    }

    if (r2d_containsf(&layout->rect_left, x, y) == TRUE)
    {
        sel->layout = cast(layout, DLayout);
        sel->elem = ekLAYELEM_MARGIN_LEFT;
        sel->col = UINT32_MAX;
        sel->row = UINT32_MAX;
        return;
    }

    if (r2d_containsf(&layout->rect_top, x, y) == TRUE)
    {
        sel->layout = cast(layout, DLayout);
        sel->elem = ekLAYELEM_MARGIN_TOP;
        sel->col = UINT32_MAX;
        sel->row = UINT32_MAX;
        return;
    }

    arrst_foreach_const(col, layout->cols, DColumn)
        if (r2d_containsf(&col->margin_rect, x, y) == TRUE)
        {
            sel->layout = cast(layout, DLayout);
            sel->row = UINT32_MAX;

            if (col_i == col_total - 1)
            {
                sel->elem = ekLAYELEM_MARGIN_RIGHT;
                sel->col = UINT32_MAX;
            }
            else
            {
                sel->elem = ekLAYELEM_MARGIN_COLUMN;
                sel->col = col_i;
            }
            return;
        }
    arrst_end()

    arrst_foreach_const(row, layout->rows, DRow)
        if (r2d_containsf(&row->margin_rect, x, y) == TRUE)
        {
            sel->layout = cast(layout, DLayout);
            sel->col = UINT32_MAX;

            if (row_i == row_total - 1)
            {
                sel->elem = ekLAYELEM_MARGIN_BOTTOM;
                sel->row = UINT32_MAX;
            }
            else
            {
                sel->elem = ekLAYELEM_MARGIN_ROW;
                sel->row = row_i;
            }
            return;
        }
    arrst_end()

    arrst_foreach_const(cell, layout->cells, DCell)
        if (r2d_containsf(&cell->rect, x, y) == TRUE)
        {
            switch(cell->type) {
            case ekCELL_TYPE_EMPTY:
            case ekCELL_TYPE_LABEL:
            {
                uint32_t ncols = arrst_size(layout->cols, DColumn);
                sel->layout = cast(layout, DLayout);
                sel->elem = ekLAYELEM_CELL;
                sel->col = cell_i % ncols;
                sel->row = cell_i / ncols;
                break;
            }
            
            case ekCELL_TYPE_LAYOUT:
                dlayout_elem_at_pos(cell->content.layout, x, y, sel);
                break;
            }

            return;
        }
    arrst_end()
}

/*---------------------------------------------------------------------------*/

static R2Df i_get_rect(const DLayout *layout, const DSelect *sel)
{
    cassert_no_null(layout);
    cassert_no_null(sel);
    cassert(layout == sel->layout);
    switch(sel->elem) {
    case ekLAYELEM_MARGIN_LEFT:
        return layout->rect_left;

    case ekLAYELEM_MARGIN_TOP:
        return layout->rect_top;

    case ekLAYELEM_MARGIN_RIGHT:
    {
        const DColumn *col = arrst_last_const(layout->cols, DColumn);
        return col->margin_rect;
    }
    
    case ekLAYELEM_MARGIN_BOTTOM:
    {
        const DRow *row = arrst_last_const(layout->rows, DRow);
        return row->margin_rect;
    }

    case ekLAYELEM_MARGIN_COLUMN:
    {
        const DColumn *col = arrst_get_const(layout->cols, sel->col, DColumn);
        return col->margin_rect;
    }
    
    case ekLAYELEM_MARGIN_ROW:
    {
        const DRow *row = arrst_get_const(layout->rows, sel->row, DRow);
        return row->margin_rect;
    }

    case ekLAYELEM_CELL:
    {
        uint32_t ncols = arrst_size(layout->cols, DColumn);
        uint32_t pos = sel->row * ncols + sel->col;
        const DCell *cell = arrst_get_const(layout->cells, pos, DCell);
        return cell->rect;
    }

    }

    return kR2D_ZEROf;
}

/*---------------------------------------------------------------------------*/

void dlayout_draw(const DLayout *layout, const DSelect *sel, DCtx *ctx)
{
    cassert_no_null(layout);
    cassert_no_null(sel);

    draw_fill_color(ctx, kCOLOR_BLACK);

    draw_r2df(ctx, ekFILL, &layout->rect_left);
    draw_r2df(ctx, ekFILL, &layout->rect_top);

    arrst_foreach_const(col, layout->cols, DColumn)
        draw_r2df(ctx, ekFILL, &col->margin_rect);
    arrst_end()

    arrst_foreach_const(row, layout->rows, DRow)
        draw_r2df(ctx, ekFILL, &row->margin_rect);
    arrst_end()

    arrst_foreach_const(cell, layout->cells, DCell)
        switch(cell->type) {
        case ekCELL_TYPE_EMPTY:
            break;
        case ekCELL_TYPE_LABEL:
            draw_text(ctx, tc(cell->content.label->text), cell->rect.pos.x, cell->rect.pos.y);
            break;
        case ekCELL_TYPE_LAYOUT:
            dlayout_draw(cell->content.layout, sel, ctx);
            break;
        }
    arrst_end()

    /* This layout has a selected element */
    if (sel->layout == layout)
    {
        R2Df rect = i_get_rect(layout, sel);
        draw_fill_color(ctx, kCOLOR_RED);
        draw_r2df(ctx, ekFILL, &rect);
    }
}
