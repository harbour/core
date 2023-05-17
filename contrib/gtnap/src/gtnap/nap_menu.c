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
#include "nap_menu.inl"
#include "hbapiitm.h"
#include "nappgui.h"

typedef struct _menuopt_t MenuOpt;
typedef struct _menuvert_t MenuVert;
typedef struct _gui_component_t GuiComponent;

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
    Font *font;
    Layout *layout;
    uint32_t mouse_row;
    uint32_t row_height;
    real32_t total_height;
    uint32_t control_width;
    uint32_t control_height;
    real32_t cell_x_size;
    real32_t cell_y_size;
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
void _component_set_frame(GuiComponent *component, const V2Df *origin, const S2Df *size);
void _component_taborder(GuiComponent *component, Window *window);
void drawctrl_text(DCtx *ctx, const char_t *text, const int32_t x, const int32_t y, const enum_t state);

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
    arrst_foreach(opt, menu->opts, MenuOpt)

        real32_t yoffset = ((real32_t)menu->row_height - opt->size.height) / 2.f;

        if (opt_i == menu->selected)
        {
            draw_fill_color(p->ctx, kCOLOR_CYAN);
            draw_rect(p->ctx, ekFILL, xpos, ypos, (real32_t)menu->control_width, (real32_t)menu->row_height /* opt->size.height*/);

            // To be removed, just for debug
            // draw_line_color(p->ctx, kCOLOR_RED);
            // draw_rect(p->ctx, ekSTROKE, 0, 0, p->width - 1, menu->total_height - 1);
        }

        if (opt_i == menu->mouse_row)
        {
            real32_t stx = xpos + menu->cell_x_size;
            draw_line_color(p->ctx, kCOLOR_BLACK);
            draw_line(p->ctx, stx, ypos + opt->size.height - 1, stx + opt->size.width, ypos + opt->size.height - 1);
        }

        drawctrl_text(p->ctx, tc(opt->text), (uint32_t)(xpos + menu->cell_x_size), (uint32_t)(ypos + yoffset), (enum_t)0);

        if (opt->kpos != UINT32_MAX)
        {
            real32_t stx = xpos + ((opt->kpos + 1) * menu->cell_x_size);
            real32_t edx = stx + menu->cell_x_size;
            draw_line(p->ctx, stx, ypos + opt->size.height - 1, edx, ypos + opt->size.height - 1);
        }

        ypos += menu->row_height; // opt->size.height;

    arrst_end();

    //draw_rect(p->ctx, ekSTROKE, 0, 0, p->width - 1, p->height - 1);
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

    if (n > 0 && p->button == ekGUI_MOUSE_LEFT)
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
    if (opt->block != NULL)
    {
        PHB_ITEM pReturn = hb_itemDo(opt->block, 0);

        if (menu->autoclose == TRUE)
        {
            bool_t close = TRUE;
            if (HB_IS_LOGICAL(pReturn))
                close = (bool_t)hb_itemGetL(pReturn);

            if (close == TRUE)
            {
                Window *window = menu->window;
                if (window == NULL)
                    window = _component_window((const GuiComponent*)menu->view);
                window_stop_modal(window, WINCLOSE_MENUVERT + menu->selected);
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
        log_printf("MenuVert i_OnUp before i_run_option");
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

        height += menu->row_height;// opt->size.height;
        n += 1;

    arrst_end();

    menu->total_height = height;
    //view_content_size(menu->view, s2df(width + 20, height + 1), s2df(0, (real32_t)menu->row_height));
    view_content_size(menu->view, s2df((real32_t)menu->control_width, height + 1), s2df(0, (real32_t)menu->row_height));

    // if (menu->visible_opts == 0 || menu->visible_opts >= n)
    //     view_size(menu->view, s2df(width + 20, height + 1));
    // else
    //     view_size(menu->view, s2df(width + 20, (real32_t)(menu->visible_opts * menu->row_height) + 1));
}

/*---------------------------------------------------------------------------*/

static void i_OnSize(Panel *panel, Event *e)
{
    MenuVert *menu = panel_get_data(panel, MenuVert);
    const EvSize *p = event_params(e, EvSize);
    log_printf("MenuVert:i_OnSize() (%.2f, %.2f)", p->width, p->height);
    menu->control_width = (uint32_t)p->width;
    menu->control_height = (uint32_t)p->height;
    i_view_size(menu);
}

/*---------------------------------------------------------------------------*/

Panel *nap_menu_create(const bool_t autoclose)
{
    Panel *panel = panel_create();
    Layout *layout = layout_create(1,1);
    View *view = view_scroll();
    MenuVert *menu = i_create();
    uint32_t cell_y_size = hb_gtnap_cell_height();
    menu->font = hb_gtnap_font();
    font_extents(menu->font, "OOOOOO", -1, &menu->cell_x_size, &menu->cell_y_size);
    menu->cell_x_size /= 6;
    menu->layout = layout;
    menu->view = view;
    menu->row_height = max_u32((uint32_t)bmath_ceilf(font_height(menu->font)), cell_y_size);
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
            otop = (real32_t)(opt_i * menu->row_height);
            obottom = (real32_t)((opt_i + 1) * menu->row_height);

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

// HB_FUNC( NAP_MENUVERT_CREATE )
// {
//     Panel *panel = panel_create();
//     Layout *layout = layout_create(1,1);
//     View *view = view_scroll();
//     MenuVert *menu = i_create();
//     uint32_t cell_y_size = hb_gtnap_cell_height();
//     menu->font = hb_gtnap_font();
//     font_extents(menu->font, "OOOOOO", -1, &menu->cell_x_size, &menu->cell_y_size);
//     menu->cell_x_size /= 6;
//     menu->layout = layout;
//     menu->view = view;
//     menu->row_height = max_u32((uint32_t)bmath_ceilf(font_height(menu->font)), cell_y_size);
//     log_printf("Cell HEIGHT: %d  ROW: %d", cell_y_size, menu->row_height);
//     menu->selected = 0;
//     menu->mouse_row = UINT32_MAX;
//     menu->launch_sel = FALSE;
//     menu->autoclose = FALSE;
//     //i_view_size(menu);
//     view_OnDraw(view, listener(panel, i_OnDraw, Panel));
//     view_OnSize(view, listener(panel, i_OnSize, Panel));
//     view_OnMove(view, listener(panel, i_OnMove, Panel));
//     view_OnDown(view, listener(panel, i_OnDown, Panel));
//     view_OnExit(view, listener(panel, i_OnExit, Panel));
//     view_OnUp(view, listener(panel, i_OnUp, Panel));
//     view_OnKeyDown(view, listener(panel, i_OnKeyDown, Panel));
//     _component_visible((GuiComponent*)view, TRUE);
//     panel_data(panel, &menu, i_destroy, MenuVert);
//     layout_view(layout, view, 0, 0);
//     layout_tabstop(layout, 0, 0, TRUE);
//     panel_layout(panel, layout);
//     hb_retptr(panel);
// }

/*---------------------------------------------------------------------------*/

// HB_FUNC( NAP_MENUVERT_ADD )
// {
//     Panel *panel = (Panel*)hb_parptr(1);
//     const char_t *text = hb_gtnap_parText(2);
//     PHB_ITEM block = hb_param(3, HB_IT_BLOCK);
//     MenuVert *menu = panel_get_data(panel, MenuVert);
//     MenuOpt *opt = arrst_new0(menu->opts, MenuOpt);
//     opt->text = str_c(text);
//     opt->block = block ? hb_itemNew(block) : NULL;
//     opt->kpos = UINT32_MAX;
//     i_view_size(menu);
// }

/*---------------------------------------------------------------------------*/

// HB_FUNC( NAP_MENUVERT_CUALIB_ADD )
// {
//     Panel *panel = (Panel*)hb_parptr(1);
//     String *text = hb_gtnap_parstr(2);
//     PHB_ITEM block = hb_param(3, HB_IT_BLOCK);
//     uint32_t kpos = hb_parni(4);
//     MenuVert *menu = panel_get_data(panel, MenuVert);
//     MenuOpt *opt = arrst_new0(menu->opts, MenuOpt);
//     opt->text = text;
//     opt->block = block ? hb_itemNew(block) : NULL;
//     opt->kpos = (kpos == 0) ? UINT32_MAX : kpos - 1;
//     opt->key = ENUM_MAX(vkey_t);
//     opt->hotmodif = UINT32_MAX;
//     log_printf("Added option '%s' to MenuVert", tc(text));
//     if (opt->kpos != UINT32_MAX)
//     {
//         uint32_t cp = i_codepoint(tc(opt->text), opt->kpos);

//         // ASCII uppercase
//         if (cp >= 65 && cp <= 90)
//         {
//             opt->key = KEY_ASCII_TABLE[cp - 65];
//             //opt->hotmodif = ekMKEY_SHIFT;
//             // Hotkeys doesn't use SHIFT
//             opt->hotmodif = 0;
//         }

//         // ASCII lowercase
//         if (cp >= 97 && cp <= 122)
//         {
//             opt->key = KEY_ASCII_TABLE[cp - 97];
//             opt->hotmodif = 0;
//         }

//         // ASCII numbers
//         if (cp >= 48 && cp <= 57)
//         {
//             opt->key = KEY_ASCII_NUMBERS[cp - 48];
//             opt->hotmodif = 0;
//         }

//         if (opt->key != ENUM_MAX(vkey_t))
//         {
//             Window *window = hb_gtnap_cualib_current_window();
//             window_hotkey(window, opt->key, opt->hotmodif, listener(menu, i_OnHotKey, MenuVert));
//         }
//     }
// }

/*---------------------------------------------------------------------------*/

// HB_FUNC( NAP_MENUVERT_AUTOCLOSE )
// {
//     Panel *panel = (Panel*)hb_parptr(1);
//     MenuVert *menu = panel_get_data(panel, MenuVert);
//     menu->autoclose = TRUE;
// }

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


