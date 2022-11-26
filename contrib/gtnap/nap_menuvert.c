/*
    This is part of gtnap
    TODO: More info
*/

/*
* objeto MENU VERTICAL
*
*/

#include "gtnap.h"
#include "nappgui.h"
#include "hbapiitm.h"

typedef struct _menuopt_t MenuOpt;
typedef struct _menuvert_t MenuVert;
typedef struct _gui_component_t GuiComponent;

struct _menuopt_t
{
    String *text;
    PHB_ITEM codeBlock;
    S2Df size;
};

DeclSt(MenuOpt);

struct _menuvert_t
{
    View *view;
    Font *font;
    Layout *layout;
    uint32_t mouse_row;
    uint32_t row_height;
    real32_t total_height;
    uint32_t visible_opts;
    uint32_t control_height;
    ArrSt(MenuOpt) *opts;
    uint32_t selected;
    bool_t launch_sel;
    bool_t autoclose;
};

/*---------------------------------------------------------------------------*/

__EXTERN_C

Window *_component_window(const GuiComponent *component);

__END_C

/*---------------------------------------------------------------------------*/

static MenuVert *i_create(void)
{
    MenuVert *menu = heap_new0(MenuVert);
    menu->opts = arrst_create(MenuOpt);
    return menu;
}

/*---------------------------------------------------------------------------*/

static void i_remove_opt(MenuOpt *opt)
{
    str_destroy(&opt->text);

    if (opt->codeBlock)
    {
        hb_itemRelease(opt->codeBlock);
        opt->codeBlock = NULL;
    }
}

/*---------------------------------------------------------------------------*/

static void i_destroy(MenuVert **menu)
{
    cassert_no_null(menu);
    cassert_no_null(*menu);
    arrst_destroy(&(*menu)->opts, i_remove_opt, MenuOpt);
    heap_delete(menu, MenuVert);
}

/*---------------------------------------------------------------------------*/

static void i_OnDraw(Panel *panel, Event *e)
{
    const EvDraw *p = event_params(e, EvDraw);
    MenuVert *menu = panel_get_data(panel, MenuVert);
    real32_t xpos = 0, ypos = 0;
    draw_font(p->ctx, menu->font);

    arrst_foreach(opt, menu->opts, MenuOpt)

        if (opt_i == menu->selected)
        {
            draw_fill_color(p->ctx, kCOLOR_CYAN);
            draw_rect(p->ctx, ekFILL, xpos, ypos, opt->size.width + 20, opt->size.height);

            // To be removed, just for debug
            // draw_line_color(p->ctx, kCOLOR_RED);
            // draw_rect(p->ctx, ekSTROKE, 0, 0, p->width - 1, menu->total_height - 1);
        }

        if (opt_i == menu->mouse_row)
        {
            draw_line_color(p->ctx, kCOLOR_BLACK);
            draw_line(p->ctx, xpos, ypos + opt->size.height - 1, xpos + opt->size.width + 20, ypos + opt->size.height - 1);
        }

        draw_text(p->ctx, tc(opt->text), xpos, ypos);
        ypos += opt->size.height;

    arrst_end();
}

/*---------------------------------------------------------------------------*/

static void i_OnMove(Panel *panel, Event *e)
{
    const EvMouse *p = event_params(e, EvMouse);
    MenuVert *menu = panel_get_data(panel, MenuVert);
    menu->mouse_row = (uint32_t)p->y / menu->row_height;
    view_update(menu->view);
}

/*---------------------------------------------------------------------------*/

static void i_OnDown(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    const EvMouse *p = event_params(e, EvMouse);
    uint32_t n = arrst_size(menu->opts, MenuOpt);

    if (n > 0 && p->button == ekMLEFT)
    {
        uint32_t y = (uint32_t)p->y;
        uint32_t sel = y / menu->row_height;

        if (sel >= n)
            sel = n - 1;

        menu->selected = sel;
        menu->launch_sel = TRUE;
        view_update(menu->view);
    }
}

/*---------------------------------------------------------------------------*/

static void i_run_option(MenuVert *menu)
{
    const MenuOpt *opt = arrst_get_const(menu->opts, menu->selected, MenuOpt);
    if (opt->codeBlock != NULL)
    {
        PHB_ITEM pReturn = hb_itemDo(opt->codeBlock, 0);

        if (menu->autoclose == TRUE)
        {
            if (HB_IS_LOGICAL(pReturn))
            {
                if (hb_itemGetL(pReturn))
                {
                    Window *window = _component_window((const GuiComponent*)menu->view);
                    window_stop_modal(window, 1000);
                }
            }
        }

        hb_itemRelease(pReturn);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnUp(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    unref(e);
    if (menu->launch_sel == TRUE)
    {
        i_run_option(menu);
        menu->launch_sel = FALSE;
    }
}

/*---------------------------------------------------------------------------*/

static void i_update_sel_top(MenuVert *menu)
{
    V2Df scroll_pos;

    view_viewport(menu->view, &scroll_pos, NULL);

    if (scroll_pos.y > 0)
    {
        real32_t ypos = (real32_t)(menu->selected * menu->row_height);
        if (scroll_pos.y > ypos)
            view_scroll_y(menu->view, ypos);
    }

    view_update(menu->view);
}

/*---------------------------------------------------------------------------*/

static void i_update_sel_bottom(MenuVert *menu)
{
    V2Df scroll_pos;
    uint32_t ypos = (menu->selected + 1) * menu->row_height;
    real32_t scroll_height = 0;

    view_viewport(menu->view, &scroll_pos, NULL);

    view_scroll_size(menu->view, NULL, &scroll_height);
    if (scroll_pos.y + menu->control_height - scroll_height < ypos)
        view_scroll_y(menu->view, (real32_t)ypos - (real32_t)menu->control_height + scroll_height);

    view_update(menu->view);
}

/*---------------------------------------------------------------------------*/

static void i_OnKeyDown(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    const EvKey *p = event_params(e, EvKey);
    uint32_t n = arrst_size(menu->opts, MenuOpt);
    bool_t update = FALSE;

    if (n > 0)
    {
        if (p->key == ekKEY_UP)
        {
            if (menu->selected > 0)
            {
                menu->selected -= 1;
                update = TRUE;
            }

            if (update == TRUE)
                i_update_sel_top(menu);
        }
        else if (p->key == ekKEY_DOWN)
        {
            if (menu->selected < n - 1)
            {
                menu->selected += 1;
                update = TRUE;
            }

            if (update == TRUE)
                i_update_sel_bottom(menu);
        }
        else if (p->key == ekKEY_RETURN)
        {
            i_run_option(menu);
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_view_size(MenuVert *menu)
{
    real32_t width = 0, height = 0, n = 0;
    cassert_no_null(menu);

    arrst_foreach(opt, menu->opts, MenuOpt)

        font_extents(menu->font, tc(opt->text), -1, &opt->size.width, &opt->size.height);

        if (opt->size.width > width)
            width = opt->size.width;

        height += opt->size.height;
        n += 1;

    arrst_end();

    menu->total_height = height;
    view_content_size(menu->view, s2df(width + 20, height + 1), s2df(0, (real32_t)menu->row_height));

    if (menu->visible_opts == 0 || menu->visible_opts >= n)
        view_size(menu->view, s2df(width + 20, height + 1));
    else
        view_size(menu->view, s2df(width + 20, (real32_t)(menu->visible_opts * menu->row_height) + 1));
}

/*---------------------------------------------------------------------------*/

static void i_OnSize(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    const EvSize *p = event_params(e, EvSize);
    menu->control_height = (uint32_t)p->height;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENUVERT_CREATE )
{
    Panel *panel = panel_create();
    Layout *layout = layout_create(1,1);
    View *view = view_scroll();
    MenuVert *menu = i_create();
    menu->font = hb_gtnap_global_font();
    menu->layout = layout;
    menu->view = view;
    menu->row_height = (uint32_t)bmath_ceilf(font_height(menu->font));
    menu->visible_opts = hb_parni(1);
    menu->selected = 0;
    menu->mouse_row = 0;
    menu->launch_sel = FALSE;
    menu->autoclose = FALSE;
    i_view_size(menu);
    view_OnDraw(view, listener(panel, i_OnDraw, Panel));
    view_OnSize(view, listener(panel, i_OnSize, Panel));
    view_OnMove(view, listener(panel, i_OnMove, Panel));
    view_OnDown(view, listener(panel, i_OnDown, Panel));
    view_OnUp(view, listener(panel, i_OnUp, Panel));
    view_OnKeyDown(view, listener(panel, i_OnKeyDown, Panel));
    panel_data(panel, &menu, i_destroy, MenuVert);
    layout_view(layout, view, 0, 0);
    layout_tabstop(layout, 0, 0, TRUE);
    panel_layout(panel, layout);
    hb_retptr(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENUVERT_ADD )
{
    Panel *panel = (Panel*)hb_parptr(1);
    const char_t *text = hb_gtnap_parText(2);
    PHB_ITEM codeBlock = hb_param(3, HB_IT_BLOCK);
    MenuVert *menu = panel_get_data(panel, MenuVert);
    MenuOpt *opt = arrst_new0(menu->opts, MenuOpt);
    opt->text = str_c(text);
    opt->codeBlock = codeBlock ? hb_itemNew(codeBlock) : NULL;
    i_view_size(menu);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENUVERT_AUTOCLOSE )
{
    Panel *panel = (Panel*)hb_parptr(1);
    MenuVert *menu = panel_get_data(panel, MenuVert);
    menu->autoclose = TRUE;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENUVERT_SELECTED )
{
    Panel *panel = (Panel*)hb_parptr(1);
    MenuVert *menu = panel_get_data(panel, MenuVert);
    hb_retni(menu->selected + 1);
}

/*---------------------------------------------------------------------------*/

// HB_FUNC( NAP_MENUVERT_FOCUS )
// {
//     Panel *panel = (Panel*)hb_parptr(1);
//     MenuVert *menu = panel_get_data(panel, MenuVert);
//     Cell *cell = layout_cell(menu->layout, 0, 0);
//     cell_focus(cell);
// }

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENUVERT_CUALIB )
{
    Panel *panel = (Panel*)hb_parptr(1);
    uint32_t nTop = hb_parni(2);
    uint32_t nLeft = hb_parni(3);
    uint32_t nBottom = hb_parni(4);
    uint32_t nRight = hb_parni(5);
    hb_gtnap_cualib_menuvert(panel, nTop, nLeft, nBottom, nRight);
}


