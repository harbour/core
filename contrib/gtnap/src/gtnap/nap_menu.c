/*
    This is part of gtnap
    TODO: More info
*/

/*
* objeto MENU VERTICAL
*
*/

#include "gtnap.h"
#include "gtnap.inl"
#include "gtnap.ch"
#include "nap_menu.inl"
#include "hbapiitm.h"
#include "nappgui.h"
#include <gui/drawctrl.inl>
#include <gui/view.inl>

typedef struct _menuopt_t MenuOpt;
typedef struct _menuvert_t MenuVert;

struct _menuopt_t
{
    String *text;
    PHB_ITEM block;
    S2Df size;
    uint32_t kpos;
    vkey_t key;
    uint32_t hotmodif;
};

struct _menuvert_t
{
    Window *window;
    View *view;
    const Font *font;
    Layout *layout;
    uint32_t mouse_row;
    real32_t row_heightf;
    real32_t total_height;
    real32_t control_widthf;
    real32_t control_heightf;
    real32_t cell_x_sizef;
    real32_t cell_y_sizef;
    ArrSt(MenuOpt) *opts;
    uint32_t selected;
    bool_t launch_sel;
    bool_t autoclose;
};

/*---------------------------------------------------------------------------*/

DeclSt(MenuOpt);

static const vkey_t KEY_ASCII_TABLE[] = {
    ekKEY_A, ekKEY_B, ekKEY_C, ekKEY_D,
    ekKEY_E, ekKEY_F, ekKEY_G, ekKEY_H,
    ekKEY_I, ekKEY_J, ekKEY_K, ekKEY_L,
    ekKEY_M, ekKEY_N, ekKEY_O, ekKEY_P,
    ekKEY_Q, ekKEY_R, ekKEY_S, ekKEY_T,
    ekKEY_U, ekKEY_V, ekKEY_W, ekKEY_X,
    ekKEY_Y, ekKEY_Z
};

static const vkey_t KEY_ASCII_NUMBERS[] = {
    ekKEY_0, ekKEY_1, ekKEY_2, ekKEY_3,
    ekKEY_4, ekKEY_5, ekKEY_6, ekKEY_7,
    ekKEY_8, ekKEY_9
};

/*---------------------------------------------------------------------------*/

__EXTERN_C

Window *_component_window(const GuiComponent *component);
void _component_visible(GuiComponent *component, const bool_t visible);
void _component_taborder(GuiComponent *component, Window *window);

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
    if (opt->block)
    {
        hb_itemRelease(opt->block);
        opt->block = NULL;
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
    draw_text_color(p->ctx, kCOLOR_DEFAULT);
    draw_line_color(p->ctx, gui_label_color());

    arrst_foreach(opt, menu->opts, MenuOpt)

        real32_t yoffset = (menu->row_heightf - opt->size.height) / 2.f;

        if (opt_i == menu->selected)
        {
            drawctrl_fill(p->ctx, (int32_t)xpos, (int32_t)ypos, (uint32_t)menu->control_widthf, (uint32_t)menu->row_heightf, ekCTRL_STATE_PRESSED);
            drawctrl_focus(p->ctx, (int32_t)xpos, (int32_t)ypos, (uint32_t)menu->control_widthf, (uint32_t)menu->row_heightf, ekCTRL_STATE_PRESSED);
        }

        if (opt_i == menu->mouse_row)
        {
            real32_t stx = xpos + menu->cell_x_sizef;
            draw_line(p->ctx, stx, ypos + opt->size.height - 1, stx + opt->size.width, ypos + opt->size.height);
        }

        drawctrl_text(p->ctx, tc(opt->text), (int32_t)(xpos + menu->cell_x_sizef), (int32_t)(ypos + yoffset), ekCTRL_STATE_NORMAL);

        if (opt->kpos != UINT32_MAX)
        {
            real32_t stx = (xpos + (opt->kpos + 1)) * menu->cell_x_sizef;
            real32_t edx = stx + menu->cell_x_sizef;
            draw_line(p->ctx, stx, ypos + opt->size.height - 1, edx, ypos + opt->size.height - 1);
        }

        ypos += menu->row_heightf;

    arrst_end();

    /* draw_rect(p->ctx, ekSTROKE, 0, 0, p->width - 1, p->height - 1); */
}

/*---------------------------------------------------------------------------*/

static void i_OnMove(Panel *panel, Event *e)
{
    const EvMouse *p = event_params(e, EvMouse);
    MenuVert *menu = panel_get_data(panel, MenuVert);
    menu->mouse_row = (uint32_t)(p->y / menu->row_heightf);
    view_update(menu->view);
}

/*---------------------------------------------------------------------------*/

static void i_OnDown(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    const EvMouse *p = event_params(e, EvMouse);
    uint32_t n = arrst_size(menu->opts, MenuOpt);

    if (n > 0 && p->button == ekGUI_MOUSE_LEFT)
    {
        uint32_t sel = (uint32_t)(p->y / menu->row_heightf);

        if (sel < n)
        {
            menu->selected = sel;
            menu->launch_sel = TRUE;
        }

        view_update(menu->view);
    }
}

/*---------------------------------------------------------------------------*/

static void i_OnExit(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    menu->mouse_row = UINT32_MAX;
    view_update(menu->view);
    unref(e);
}

/*---------------------------------------------------------------------------*/

static void i_run_option(MenuVert *menu)
{
    const MenuOpt *opt = arrst_get_const(menu->opts, menu->selected, MenuOpt);
    bool_t close = FALSE;

    if (opt->block != NULL)
    {
        PHB_ITEM ritem = hb_itemDo(opt->block, 0);

        if (menu->autoclose == TRUE)
        {
            if (HB_IS_LOGICAL(ritem))
                close = (bool_t)hb_itemGetL(ritem);
        }

        hb_itemRelease(ritem);
    }
    else
    {
        close = menu->autoclose;
    }

    if (close == TRUE)
    {
        Window *window = menu->window;
        if (window == NULL)
            window = _component_window((const GuiComponent*)menu->view);
        window_stop_modal(window, NAP_MODAL_MENU_AUTOCLOSE + 1 + menu->selected);
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
        real32_t ypos = (real32_t)menu->selected * menu->row_heightf;
        if (scroll_pos.y > ypos)
            view_scroll_y(menu->view, ypos);
    }

    view_update(menu->view);
}

/*---------------------------------------------------------------------------*/

static void i_update_sel_bottom(MenuVert *menu)
{
    V2Df scroll_pos;
    real32_t ypos = (real32_t)(menu->selected + 1) * menu->row_heightf;
    real32_t scroll_height = 0;

    view_viewport(menu->view, &scroll_pos, NULL);

    view_scroll_size(menu->view, NULL, &scroll_height);
    if (scroll_pos.y + menu->control_heightf - scroll_height < ypos)
        view_scroll_y(menu->view, ypos - menu->control_heightf + scroll_height);

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
        else if (p->key == ekKEY_PAGEUP)
        {
            if (menu->selected > 0)
            {
                uint32_t num_visible = (uint32_t)(menu->control_heightf / menu->row_heightf);
                if (menu->selected > num_visible)
                    menu->selected -= num_visible;
                else
                    menu->selected = 0;
                update = TRUE;
            }

            if (update == TRUE)
                i_update_sel_top(menu);
        }
        else if (p->key == ekKEY_PAGEDOWN)
        {
            if (menu->selected < n - 1)
            {
                uint32_t num_visible = (uint32_t)(menu->control_heightf / menu->row_heightf);
                menu->selected += num_visible;
                if (menu->selected >= n)
                    menu->selected = n - 1;
                update = TRUE;
            }

            if (update == TRUE)
                i_update_sel_bottom(menu);
        }
        else if (p->key == ekKEY_RETURN || p->key == ekKEY_NUMRET)
        {
            i_run_option(menu);
        }
    }
}

/*---------------------------------------------------------------------------*/

static void i_view_size(MenuVert *menu)
{
    real32_t width = 0, height = 0;
    cassert_no_null(menu);

    arrst_foreach(opt, menu->opts, MenuOpt)

        font_extents(menu->font, tc(opt->text), -1, &opt->size.width, &opt->size.height);

        if (opt->size.width > width)
            width = opt->size.width;

        height += menu->row_heightf;

    arrst_end();

    menu->total_height = height;
    view_content_size(menu->view, s2df(menu->control_widthf, height), s2df(0, menu->row_heightf));
}

/*---------------------------------------------------------------------------*/

static void i_OnSize(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    const EvSize *p = event_params(e, EvSize);
    menu->control_widthf = p->width;
    menu->control_heightf = p->height;
    i_view_size(menu);
}

/*---------------------------------------------------------------------------*/

Panel *nap_menu_create(const bool_t autoclose, const Font *font, const real32_t cell_x_size, const real32_t cell_y_size)
{
    Panel *panel = panel_create();
    Layout *layout = layout_create(1,1);
    View *view = _view_create(ekVIEW_HSCROLL | ekVIEW_VSCROLL | ekVIEW_CONTROL);
    MenuVert *menu = i_create();
    menu->font = font;
    menu->cell_x_sizef = cell_x_size;
    menu->cell_y_sizef = cell_y_size;
    menu->layout = layout;
    menu->view = view;
    menu->row_heightf = cell_y_size;
    menu->selected = 0;
    menu->mouse_row = UINT32_MAX;
    menu->launch_sel = FALSE;
    menu->autoclose = autoclose;
    view_OnDraw(view, listener(panel, i_OnDraw, Panel));
    view_OnSize(view, listener(panel, i_OnSize, Panel));
    view_OnMove(view, listener(panel, i_OnMove, Panel));
    view_OnDown(view, listener(panel, i_OnDown, Panel));
    view_OnExit(view, listener(panel, i_OnExit, Panel));
    view_OnUp(view, listener(panel, i_OnUp, Panel));
    view_OnKeyDown(view, listener(panel, i_OnKeyDown, Panel));
    _component_visible((GuiComponent*)view, TRUE);
    panel_data(panel, &menu, i_destroy, MenuVert);
    layout_view(layout, view, 0, 0);
    layout_tabstop(layout, 0, 0, TRUE);
    panel_layout(panel, layout);
    return panel;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_codepoint(const char_t *str, const uint32_t pos)
{
    uint32_t i = 0;
    const char_t *it = str;
    while(i != pos)
    {
        it = unicode_next(it, ekUTF8);
        i += 1;
    }
    return unicode_to_u32(it, ekUTF8);
}

/*---------------------------------------------------------------------------*/

static void i_OnHotKey(MenuVert *menu, Event *e)
{
    const EvKey *p = event_params(e, EvKey);
    arrst_foreach(opt, menu->opts, MenuOpt)
        if (opt->key == p->key && opt->hotmodif == p->modifiers)
        {
            /* When an option is activated by hotkey, make sure the option
               will be visible in menu scrolling up or down. */
            V2Df pos;
            S2Df size;
            real32_t mtop, mbottom, otop, obottom;
            view_viewport(menu->view, &pos, &size);
            mtop = pos.y;
            mbottom = pos.y + size.height;
            otop = (real32_t)opt_i * menu->row_heightf;
            obottom = (real32_t)(opt_i + 1) * menu->row_heightf;

            if (mtop > otop)
                view_scroll_y(menu->view, otop);
            else if (mbottom < obottom)
                view_scroll_y(menu->view, obottom - size.height);

            menu->selected = opt_i;
            view_update(menu->view);
            i_run_option(menu);
            break;
        }
    arrst_end();
}

/*---------------------------------------------------------------------------*/

void nap_menu_add(Panel *panel, Window *window, HB_ITEM *text_block, HB_ITEM *click_block, const uint32_t kpos)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    MenuOpt *opt = arrst_new0(menu->opts, MenuOpt);
    opt->text = hb_block_to_utf8(text_block);
    opt->block = click_block ? hb_itemNew(click_block) : NULL;
    opt->kpos = (kpos == 0) ? UINT32_MAX : kpos - 1;
    opt->key = ENUM_MAX(vkey_t);
    opt->hotmodif = UINT32_MAX;

    if (opt->kpos != UINT32_MAX)
    {
        uint32_t cp = i_codepoint(tc(opt->text), opt->kpos);

        /* ASCII uppercase */
        if (cp >= 65 && cp <= 90)
        {
            opt->key = KEY_ASCII_TABLE[cp - 65];
            opt->hotmodif = 0;
        }

        /* ASCII lowercase */
        else if (cp >= 97 && cp <= 122)
        {
            opt->key = KEY_ASCII_TABLE[cp - 97];
            opt->hotmodif = 0;
        }

        /* ASCII numbers */
        else if (cp >= 48 && cp <= 57)
        {
            opt->key = KEY_ASCII_NUMBERS[cp - 48];
            opt->hotmodif = 0;
        }

        if (opt->key != ENUM_MAX(vkey_t))
            window_hotkey(window, opt->key, opt->hotmodif, listener(menu, i_OnHotKey, MenuVert));
    }

    i_view_size(menu);
}

/*---------------------------------------------------------------------------*/

void nap_menuvert_taborder(Panel *panel, Window *window)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    menu->window = window;
    _component_taborder((GuiComponent*)menu->view, window);
}

/*---------------------------------------------------------------------------*/

uint32_t nap_menu_selected(Panel *panel)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    cassert_no_null(menu);
    return menu->selected;
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENU )
{
    uint32_t wid = hb_parni(1);
    int32_t top = hb_parni(2);
    int32_t left = hb_parni(3);
    int32_t bottom = hb_parni(4);
    int32_t right = hb_parni(5);
    bool_t autoclose = (bool_t)hb_parl(6);
    bool_t in_scroll = (bool_t)hb_parl(7);
    uint32_t id = hb_gtnap_menu(wid, top, left, bottom, right, autoclose, in_scroll);
    hb_retni(id);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENU_ADD )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    HB_ITEM *text_block = hb_param(3, HB_IT_BLOCK);
    HB_ITEM *click_block = hb_param(4, HB_IT_BLOCK);
    uint32_t kpos = hb_parni(5);
    hb_gtnap_menu_add(wid, id, text_block, click_block, kpos);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENU_SELECTED )
{
    uint32_t wid = hb_parni(1);
    uint32_t id = hb_parni(2);
    uint32_t sel = hb_gtnap_menu_selected(wid, id);
    hb_retni(sel + 1);
}
