/* Designer layout */

#include "dlayout.h"
#include <nform/flayout.h>
#include <gui/button.h>
#include <gui/button.h>
#include <gui/buttonh.h>
#include <gui/label.h>
#include <gui/labelh.h>
#include <gui/layout.h>
#include <gui/layouth.h>
#include <gui/edit.h>
#include <gui/cell.h>
#include <gui/drawctrl.inl>
#include <geom2d/r2d.h>
#include <geom2d/v2d.h>
#include <draw2d/color.h>
#include <draw2d/draw.h>
#include <draw2d/drawg.h>
#include <draw2d/font.h>
#include <draw2d/image.h>
#include <core/arrst.h>
#include <core/heap.h>
#include <core/strings.h>
#include <sewer/bmem.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

static void i_remove_cell(DCell *cell)
{
    cassert_no_null(cell);
    if (cell->sublayout != NULL)
        dlayout_destroy(&cell->sublayout);
}

/*---------------------------------------------------------------------------*/

DLayout *dlayout_create(const uint32_t ncols, const uint32_t nrows)
{
    DLayout *layout = heap_new(DLayout);
    layout->cols = arrst_create(DColumn);
    layout->rows = arrst_create(DRow);
    layout->cells = arrst_create(DCell);
    cassert(ncols > 0);
    cassert(nrows > 0);
    arrst_new_n0(layout->cols, ncols, DColumn);
    arrst_new_n0(layout->rows, nrows, DRow);
    arrst_new_n0(layout->cells, ncols * nrows, DCell);
    return layout;
}

/*---------------------------------------------------------------------------*/

DLayout *dlayout_from_flayout(const FLayout *flayout)
{
    DLayout *layout = heap_new(DLayout);
    uint32_t i, ncols = flayout_ncols(flayout);
    uint32_t j, nrows = flayout_nrows(flayout);
    DCell *dcell = NULL;
    cassert_no_null(flayout);
    layout->cols = arrst_create(DColumn);
    layout->rows = arrst_create(DRow);
    layout->cells = arrst_create(DCell);
    arrst_new_n0(layout->cols, ncols, DColumn);
    arrst_new_n0(layout->rows, nrows, DRow);
    arrst_new_n0(layout->cells, ncols * nrows, DCell);
    dcell = arrst_all(layout->cells, DCell);
    for (j = 0; j < nrows; ++j)
    {
        for (i = 0; i < ncols; ++i)
        {
            const FCell *fcell = flayout_ccell(flayout, i, j);
            if (fcell->type == ekCELL_TYPE_LAYOUT)
                dcell->sublayout = dlayout_from_flayout(fcell->widget.layout);
            dcell += 1;
        }
    }

    return layout;
}

/*---------------------------------------------------------------------------*/

void dlayout_destroy(DLayout **layout)
{
    cassert_no_null(layout);
    cassert_no_null(*layout);
    arrst_destroy(&(*layout)->cols, NULL, DColumn);
    arrst_destroy(&(*layout)->rows, NULL, DRow);
    arrst_destroy(&(*layout)->cells, i_remove_cell, DCell);
    heap_delete(layout, DLayout);
}

/*---------------------------------------------------------------------------*/

void dlayout_insert_col(DLayout *layout, const uint32_t col)
{
    cassert_no_null(layout);
    arrst_insert_n0(layout->cols, col, 1, DColumn);
}

/*---------------------------------------------------------------------------*/

void dlayout_remove_col(DLayout *layout, const uint32_t col)
{
    cassert_no_null(layout);
    arrst_delete(layout->cols, col, NULL, DColumn);
}

/*---------------------------------------------------------------------------*/

void dlayout_insert_row(DLayout *layout, const uint32_t row)
{
    cassert_no_null(layout);
    arrst_insert_n0(layout->rows, row, 1, DRow);
}

/*---------------------------------------------------------------------------*/

void dlayout_remove_row(DLayout *layout, const uint32_t row)
{
    cassert_no_null(layout);
    arrst_delete(layout->rows, row, NULL, DRow);
}

/*---------------------------------------------------------------------------*/

static ___INLINE DCell *i_cell(DLayout *layout, const uint32_t col, const uint32_t row)
{
    uint32_t ncols = UINT32_MAX;
    uint32_t pos = UINT32_MAX;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, DColumn);
    pos = row * ncols + col;
    return arrst_get(layout->cells, pos, DCell);
}

/*---------------------------------------------------------------------------*/

void dlayout_remove_cell(DLayout *layout, const uint32_t col, const uint32_t row)
{
    DCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    i_remove_cell(cell);
}

//*---------------------------------------------------------------------------*/

void dlayout_add_layout(DLayout *layout, DLayout *sublayout, const uint32_t col, const uint32_t row)
{
    DCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    cassert_no_null(sublayout);
    cassert(cell->sublayout == NULL);
    cell->sublayout = sublayout;
}

/*---------------------------------------------------------------------------*/

bool_t dlayout_empty_cell(const DSelect *sel)
{
    cassert_no_null(sel);
    if (sel->flayout != NULL)
    {
        cassert_no_null(sel->glayout);
        cassert_no_null(sel->dlayout);
        if (sel->elem == ekLAYELEM_CELL)
        {
            const FCell *cell = flayout_ccell(sel->flayout, sel->col, sel->row);
            cassert_no_null(cell);
            if (cell->type == ekCELL_TYPE_EMPTY)
                return TRUE;
        }
    }
    else
    {
        cassert(sel->glayout == NULL);
        cassert(sel->dlayout == NULL);
    }

    return FALSE;
}

/*---------------------------------------------------------------------------*/

FCell *dlayout_sel_fcell(const DSelect *sel)
{
    cassert_no_null(sel);
    if (sel->flayout != NULL)
    {
        cassert_no_null(sel->glayout);
        cassert_no_null(sel->dlayout);
        if (sel->elem == ekLAYELEM_CELL)
        {        
            return flayout_cell(sel->flayout, sel->col, sel->row);
        }
    }
    else
    {
        cassert(sel->glayout == NULL);
        cassert(sel->dlayout == NULL);
    }

    return NULL;
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
    real32_t mtop = 0, mbottom = 0, mleft = 0, mright = 0;
    real32_t x, y;
    cassert_no_null(layout);
    col = arrst_all(layout->cols, DColumn);
    row = arrst_all(layout->rows, DRow);
    cell = arrst_all(layout->cells, DCell);
    ncols = arrst_size(layout->cols, DColumn);
    nrows = arrst_size(layout->rows, DRow);
    cassert(ncols == layout_ncols(glayout));
    cassert(nrows == layout_nrows(glayout));
    mtop = layout_get_margin_top(glayout);
    mbottom = layout_get_margin_bottom(glayout);
    mleft = layout_get_margin_left(glayout);
    mright = layout_get_margin_right(glayout);

    /* Compute the columns width and total layout widths */
    total_width = 0;
    inner_width = 0;
    for (i = 0; i < ncols; ++i)
    {
        col[i].width = layout_get_hsize(glayout, i);
        inner_width += col[i].width;
        if (i < ncols - 1)
            inner_width += layout_get_hmargin(glayout, i);
        else
            total_width = inner_width + mleft + mright;
    }

    /* Compute the rows height and total layout heights */
    total_height = 0;
    inner_height = 0;
    for (j = 0; j < nrows; ++j)
    {
        row[j].height = layout_get_vsize(glayout, j);
        inner_height += row[j].height;
        if (j < nrows - 1)
            inner_height += layout_get_vmargin(glayout, j);
        else
            total_height = inner_height + mtop + mbottom;
    }

    /* Global layout rectangle */
    layout->rect = r2df(origin.x, origin.y, total_width, total_height);

    /* Compute the vertical rectangles */
    layout->rect_left = r2df(origin.x, origin.y, mleft, total_height);
    x = origin.x + mleft;
    for (i = 0; i < ncols; ++i)
    {
        x += col[i].width;
        if (i < ncols - 1)
        {
            real32_t cmright = layout_get_hmargin(glayout, i);
            col[i].margin_rect = r2df(x, origin.y, cmright, total_height);
            x += cmright;
        }
        else
        {
            col[i].margin_rect = kR2D_ZEROf;
        }
    }
    layout->rect_right = r2df(x, origin.y, mright, total_height);

    /* Compute the horizontal rectangles */
    layout->rect_top = r2df(origin.x, origin.y, total_width, mtop);
    y = origin.y + mtop;
    for (j = 0; j < nrows; ++j)
    {
        y += row[j].height;
        if (j < nrows - 1)
        {
            real32_t cmbottom = layout_get_vmargin(glayout, j);
            row[j].margin_rect = r2df(origin.x, y, total_width, cmbottom);
            y += cmbottom;
        }
        else
        {
            row[j].margin_rect = kR2D_ZEROf;
        }
    }
    layout->rect_bottom = r2df(origin.x, y, total_width, mbottom);

    /* Compute the cells rectangles */
    {
        DCell *dcell = cell;
        y = origin.y + mtop;

        for (j = 0; j < nrows; ++j)
        {
            x = origin.x + mleft;
            for (i = 0; i < ncols; ++i)
            {
                const Cell *gcell = layout_cell(cast(glayout, Layout), i, j);
                real32_t hsize = cell_get_hsize(gcell);
                real32_t vsize = cell_get_vsize(gcell);
                align_t halign = cell_get_halign(gcell);
                align_t valign = cell_get_valign(gcell);
                real32_t cellx = 0;
                real32_t celly = 0;
                cassert_no_null(gcell);
                cassert(hsize <= col[i].width);
                cassert(vsize <= row[j].height);

                switch (halign)
                {
                case ekLEFT:
                    cellx = x;
                    break;
                case ekCENTER:
                    cellx = x + (col[i].width - hsize) / 2;
                    break;
                case ekRIGHT:
                    cellx = x + (col[i].width - hsize);
                    break;
                case ekJUSTIFY:
                    cellx = x;
                    break;
                    cassert_default();
                }

                switch (valign)
                {
                case ekTOP:
                    celly = y;
                    break;
                case ekCENTER:
                    celly = y + (row[j].height - vsize) / 2;
                    break;
                case ekBOTTOM:
                    celly = y + (row[j].height - vsize);
                    break;
                case ekJUSTIFY:
                    celly = y;
                    break;
                    cassert_default();
                }

                if (dcell->sublayout != NULL)
                {
                    Layout *subglayout = layout_get_layout(cast(glayout, Layout), i, j);
                    dlayout_synchro_visual(dcell->sublayout, subglayout, v2df(cellx, celly));
                }

                dcell->rect = r2df(x, y, col[i].width, row[j].height);
                dcell->content_rect = r2df(cellx, celly, hsize, vsize);
                dcell += 1;

                x += col[i].width;
                if (i < ncols - 1)
                    x += layout_get_hmargin(glayout, i);
            }

            y += row[j].height;
            if (j < nrows - 1)
                y += layout_get_vmargin(glayout, j);
        }
    }
}

/*---------------------------------------------------------------------------*/

void dlayout_elem_at_pos(const DLayout *dlayout, const FLayout *flayout, const Layout *glayout, const real32_t x, const real32_t y, ArrSt(DSelect) *selpath)
{
    cassert_no_null(dlayout);
    if (r2d_containsf(&dlayout->rect, x, y) == TRUE)
    {
        DSelect *sel = arrst_new(selpath, DSelect);
        sel->dlayout = cast(dlayout, DLayout);
        sel->flayout = cast(flayout, FLayout);
        sel->glayout = cast(glayout, Layout);

        if (r2d_containsf(&dlayout->rect_left, x, y) == TRUE)
        {
            sel->elem = ekLAYELEM_MARGIN_LEFT;
            sel->col = UINT32_MAX;
            sel->row = UINT32_MAX;
            return;
        }

        if (r2d_containsf(&dlayout->rect_top, x, y) == TRUE)
        {
            sel->elem = ekLAYELEM_MARGIN_TOP;
            sel->col = UINT32_MAX;
            sel->row = UINT32_MAX;
            return;
        }

        if (r2d_containsf(&dlayout->rect_right, x, y) == TRUE)
        {
            sel->elem = ekLAYELEM_MARGIN_RIGHT;
            sel->col = UINT32_MAX;
            sel->row = UINT32_MAX;
            return;
        }

        if (r2d_containsf(&dlayout->rect_bottom, x, y) == TRUE)
        {
            sel->elem = ekLAYELEM_MARGIN_BOTTOM;
            sel->col = UINT32_MAX;
            sel->row = UINT32_MAX;
            return;
        }

        arrst_foreach_const(col, dlayout->cols, DColumn)
            if (col_i < col_total - 1)
            {
                if (r2d_containsf(&col->margin_rect, x, y) == TRUE)
                {
                    sel->elem = ekLAYELEM_MARGIN_COLUMN;
                    sel->col = col_i;
                    sel->row = UINT32_MAX;
                    return;
                }
            }
        arrst_end()

        arrst_foreach_const(row, dlayout->rows, DRow)
            if (row_i < row_total - 1)
            {
                if (r2d_containsf(&row->margin_rect, x, y) == TRUE)
                {
                    sel->elem = ekLAYELEM_MARGIN_ROW;
                    sel->col = UINT32_MAX;
                    sel->row = row_i;
                    return;
                }
            }
        arrst_end()

        arrst_foreach_const(cell, dlayout->cells, DCell)
            if (r2d_containsf(&cell->rect, x, y) == TRUE)
            {
                uint32_t ncols = arrst_size(dlayout->cols, DColumn);
                sel->elem = ekLAYELEM_CELL;
                sel->col = cell_i % ncols;
                sel->row = cell_i / ncols;
                if (cell->sublayout != NULL)
                {
                    const FCell *fcell = flayout_ccell(flayout, sel->col, sel->row);
                    Layout *gsublayout = layout_get_layout(cast(glayout, Layout), sel->col, sel->row);
                    dlayout_elem_at_pos(cell->sublayout, fcell->widget.layout, gsublayout, x, y, selpath);
                }

                return;
            }
        arrst_end()
    }
}

/*---------------------------------------------------------------------------*/

static R2Df i_cell_rect(const DLayout *dlayout, const DSelect *sel)
{
    uint32_t ncols, pos;
    const DCell *cell = NULL;
    cassert_no_null(dlayout);
    cassert_no_null(sel);
    cassert(dlayout == sel->dlayout);
    cassert(sel->elem == ekLAYELEM_CELL);
    ncols = arrst_size(dlayout->cols, DColumn);
    pos = sel->row * ncols + sel->col;
    cell = arrst_get_const(dlayout->cells, pos, DCell);
    return cell->rect;
}

/*---------------------------------------------------------------------------*/

static R2Df i_get_rect(const DLayout *dlayout, const DSelect *sel)
{
    cassert_no_null(dlayout);
    cassert_no_null(sel);
    cassert(dlayout == sel->dlayout);
    switch (sel->elem)
    {
    case ekLAYELEM_MARGIN_LEFT:
        return dlayout->rect_left;

    case ekLAYELEM_MARGIN_TOP:
        return dlayout->rect_top;

    case ekLAYELEM_MARGIN_RIGHT:
        return dlayout->rect_right;

    case ekLAYELEM_MARGIN_BOTTOM:
        return dlayout->rect_bottom;

    case ekLAYELEM_MARGIN_COLUMN:
    {
        const DColumn *col = arrst_get_const(dlayout->cols, sel->col, DColumn);
        return col->margin_rect;
    }

    case ekLAYELEM_MARGIN_ROW:
    {
        const DRow *row = arrst_get_const(dlayout->rows, sel->row, DRow);
        return row->margin_rect;
    }

    case ekLAYELEM_CELL:
        return i_cell_rect(dlayout, sel);
    }

    return kR2D_ZEROf;
}

/*---------------------------------------------------------------------------*/

static R2Df i_get_full_rect(const DLayout *dlayout, const DSelect *sel)
{
    cassert_no_null(dlayout);
    cassert_no_null(sel);
    cassert(dlayout == sel->dlayout);
    switch (sel->elem)
    {
    case ekLAYELEM_MARGIN_LEFT:
    case ekLAYELEM_MARGIN_TOP:
    case ekLAYELEM_MARGIN_RIGHT:
    case ekLAYELEM_MARGIN_BOTTOM:
    case ekLAYELEM_MARGIN_COLUMN:
    case ekLAYELEM_MARGIN_ROW:
        return dlayout->rect;

    case ekLAYELEM_CELL:
        return i_cell_rect(dlayout, sel);
    }

    return kR2D_ZEROf;
}

/*---------------------------------------------------------------------------*/

static bool_t i_is_cell_sel(const DSelect *sel, const DLayout *dlayout, const uint32_t col, const uint32_t row)
{
    cassert_no_null(sel);
    if (sel->dlayout != dlayout)
        return FALSE;
    if (sel->elem != ekLAYELEM_CELL)
        return FALSE;
    if (sel->col != col)
        return FALSE;
    if (sel->row != row)
        return FALSE;
    return TRUE;
}

/*---------------------------------------------------------------------------*/

void dlayout_draw(const DLayout *dlayout, const FLayout *flayout, const Layout *glayout, const DSelect *hover, const DSelect *sel, const widget_t swidget, const Image *add_icon, DCtx *ctx)
{
    uint32_t ncols, nrows, i, j;
    const DCell *dcell = NULL;
    cassert_no_null(dlayout);
    cassert_no_null(flayout);
    cassert_no_null(hover);
    ncols = arrst_size(dlayout->cols, DColumn);
    nrows = arrst_size(dlayout->rows, DRow);
    dcell = arrst_all_const(dlayout->cells, DCell);
    cassert(ncols == flayout_ncols(flayout));
    cassert(nrows == flayout_nrows(flayout));
    draw_line_color(ctx, kCOLOR_BLACK);
    draw_fill_color(ctx, kCOLOR_BLACK);
    draw_r2df(ctx, ekSTROKE, &dlayout->rect);
    // draw_r2df(ctx, ekFILL, &layout->rect_left);
    // draw_r2df(ctx, ekFILL, &layout->rect_top);

    // arrst_foreach_const(col, layout->cols, DColumn)
    //     draw_r2df(ctx, ekFILL, &col->margin_rect);
    // arrst_end()

    // arrst_foreach_const(row, layout->rows, DRow)
    //     draw_r2df(ctx, ekFILL, &row->margin_rect);
    // arrst_end()

    for (j = 0; j < nrows; ++j)
    {
        for (i = 0; i < ncols; ++i)
        {
            const FCell *fcell = flayout_ccell(flayout, i, j);
            Cell *gcell = layout_cell(cast(glayout, Layout), i, j);
            draw_r2df(ctx, ekSTROKE, &dcell->rect);
            switch (fcell->type)
            {
            case ekCELL_TYPE_EMPTY:
                break;

            case ekCELL_TYPE_LABEL:
            {
                color_t color = i_is_cell_sel(hover, dlayout, i, j) ? kCOLOR_RED : kCOLOR_BLACK;
                color_t bgcolor = color_rgb(225, 225, 0);
                const Label *glabel = cell_label(gcell);
                const Font *gfont = label_get_font(glabel);
                draw_font(ctx, gfont);
                draw_fill_color(ctx, bgcolor);
                draw_text_color(ctx, color);
                draw_rect(ctx, ekFILL, dcell->content_rect.pos.x, dcell->content_rect.pos.y, dcell->content_rect.size.width, dcell->content_rect.size.height);
                drawctrl_text(ctx, tc(fcell->widget.label->text), (int32_t)dcell->content_rect.pos.x, (int32_t)dcell->content_rect.pos.y, ekCTRL_STATE_NORMAL);
                break;
            }

            case ekCELL_TYPE_BUTTON:
            {
                color_t color = i_is_cell_sel(hover, dlayout, i, j) ? kCOLOR_RED : kCOLOR_BLACK;
                color_t bgcolor = color_rgb(225, 225, 0);
                const Button *gbutton = cell_button(gcell);
                const Font *gfont = button_get_font(gbutton);
                real32_t twidth, theight;
                real32_t tx, ty;
                draw_font(ctx, gfont);
                draw_line_color(ctx, color);
                draw_text_color(ctx, color);
                draw_fill_color(ctx, bgcolor);
                draw_line_width(ctx, 3);
                draw_rect(ctx, ekFILLSK, dcell->content_rect.pos.x, dcell->content_rect.pos.y, dcell->content_rect.size.width, dcell->content_rect.size.height);
                draw_line_width(ctx, 1);
                draw_line_color(ctx, kCOLOR_BLACK);
                font_extents(gfont, tc(fcell->widget.button->text), -1.f, &twidth, &theight);
                tx = dcell->content_rect.pos.x + ((dcell->content_rect.size.width - twidth) / 2);
                ty = dcell->content_rect.pos.y + ((dcell->content_rect.size.height - theight) / 2);
                drawctrl_text(ctx, tc(fcell->widget.button->text), (int32_t)tx, (int32_t)ty, ekCTRL_STATE_NORMAL);
                break;
            }

            case ekCELL_TYPE_CHECK:
            {
                color_t color = i_is_cell_sel(hover, dlayout, i, j) ? kCOLOR_RED : kCOLOR_BLACK;
                color_t bgcolor = color_rgb(225, 225, 0);
                const Button *gcheck = cell_button(gcell);
                const Font *gfont = button_get_font(gcheck);
                real32_t cwidth = (real32_t)drawctrl_check_width(ctx) - 2;
                real32_t cheight = (real32_t)drawctrl_check_height(ctx) - 2;
                real32_t twidth, theight;
                real32_t tx;
                draw_font(ctx, gfont);
                draw_line_color(ctx, color);
                draw_text_color(ctx, color);
                draw_fill_color(ctx, bgcolor);
                font_extents(gfont, tc(fcell->widget.check->text), -1.f, &twidth, &theight);
                tx = dcell->content_rect.pos.x + (dcell->content_rect.size.width - twidth);
                draw_rect(ctx, ekFILL, dcell->content_rect.pos.x, dcell->content_rect.pos.y, dcell->content_rect.size.width, dcell->content_rect.size.height);
                drawctrl_text(ctx, tc(fcell->widget.check->text), (int32_t)tx, (int32_t)dcell->content_rect.pos.y, ekCTRL_STATE_NORMAL);
                draw_rect(ctx, ekSTROKE, dcell->content_rect.pos.x, dcell->content_rect.pos.y, cwidth, cheight);
                draw_line_color(ctx, kCOLOR_BLACK);
                break;
            }

            case ekCELL_TYPE_EDIT:
            {
                color_t color = i_is_cell_sel(hover, dlayout, i, j) ? kCOLOR_RED : kCOLOR_BLACK;
                color_t bgcolor = color_rgb(225, 225, 0);
                real32_t pattern[2] = {1, 2};
                draw_line_color(ctx, color);
                draw_fill_color(ctx, bgcolor);
                draw_line_width(ctx, 2);
                draw_rect(ctx, ekFILLSK, dcell->content_rect.pos.x, dcell->content_rect.pos.y, dcell->content_rect.size.width, dcell->content_rect.size.height);
                draw_line_width(ctx, 1);
                draw_line_dash(ctx, pattern, 2);
                draw_rect(ctx, ekSTROKE, dcell->content_rect.pos.x + 5, dcell->content_rect.pos.y + 5, dcell->content_rect.size.width - 10, dcell->content_rect.size.height - 10);
                draw_line_dash(ctx, NULL, 0);
                draw_line_color(ctx, kCOLOR_BLACK);
                break;
            }

            case ekCELL_TYPE_LAYOUT:
            {
                Layout *gsublayout = cell_layout(gcell);
                dlayout_draw(dcell->sublayout, fcell->widget.layout, gsublayout, hover, sel, swidget, add_icon, ctx);
                break;
            }
            }

            dcell += 1;
        }
    }

    /* This layout has the hover element */
    if (hover->dlayout == dlayout)
    {
        R2Df rect = i_get_rect(dlayout, hover);
        draw_line_color(ctx, kCOLOR_RED);
        if (hover->elem != ekLAYELEM_CELL)
        {
            draw_r2df(ctx, ekSTROKE, &rect);
            draw_line_color(ctx, kCOLOR_BLACK);
        }
        else
        {
            const FCell *fcell = flayout_ccell(flayout, hover->col, hover->row);
            draw_r2df(ctx, ekSTROKE, &rect);
            if (fcell->type == ekCELL_TYPE_EMPTY && swidget != ekWIDGET_SELECT)
            {
                uint32_t iw = image_width(add_icon);
                uint32_t ih = image_height(add_icon);
                real32_t x = rect.pos.x + (rect.size.width - iw) / 2;
                real32_t y = rect.pos.y + (rect.size.height - ih) / 2;
                draw_image(ctx, add_icon, x, y);
            }
        }
        draw_line_color(ctx, kCOLOR_BLACK);
    }

    /* This layout has the selected element */
    if (sel->dlayout == dlayout)
    {
        R2Df rect = i_get_full_rect(dlayout, sel);
        real32_t rsize = 5;
        real32_t x1 = rect.pos.x;
        real32_t x2 = rect.pos.x + rect.size.width;
        real32_t y1 = rect.pos.y;
        real32_t y2 = rect.pos.y + rect.size.height;
        draw_fill_color(ctx, color_rgb(0, 40, 85));
        draw_rect(ctx, ekFILL, x1, y1, rsize, rsize);
        draw_rect(ctx, ekFILL, x2 - rsize, y1, rsize, rsize);
        draw_rect(ctx, ekFILL, x1, y2 - rsize, rsize, rsize);
        draw_rect(ctx, ekFILL, x2 - rsize, y2 - rsize, rsize, rsize);
        draw_rect(ctx, ekFILL, (x1 + x2 - rsize) / 2, y1, rsize, rsize);
        draw_rect(ctx, ekFILL, (x1 + x2 - rsize) / 2, y2 - rsize, rsize, rsize);
        draw_rect(ctx, ekFILL, x1, (y1 + y2 - rsize) / 2, rsize, rsize);
        draw_rect(ctx, ekFILL, x2 - rsize, (y1 + y2 - rsize) / 2, rsize, rsize);
    }
}
