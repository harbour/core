/*
    This is part of gtnap
    TODO: More info
*/

/*
* objeto MENU VERTICAL
*
*/

#include "hbgtnap.h"
#include "nappgui.h"

typedef struct _menuopt_t MenuOpt;
typedef struct _menuvert_t MenuVert;

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
    ArrSt(MenuOpt) *opts;
    uint32_t selected;
};

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
        hb_itemRelease(opt->codeBlock);
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
            draw_rect(p->ctx, ekFILL, xpos, ypos, opt->size.width + 50, opt->size.height);
        }

        draw_text(p->ctx, tc(opt->text), xpos, ypos);
        ypos += opt->size.height;

    arrst_end();
}

/*---------------------------------------------------------------------------*/

static void i_view_size(MenuVert *menu)
{
    static real32_t MIN_WIDTH = 500;
    static real32_t MIN_HEIGHT = 250;
    real32_t width = 0, height = 0;
    cassert_no_null(menu);

    arrst_foreach(opt, menu->opts, MenuOpt)

        font_extents(menu->font, tc(opt->text), -1, &opt->size.width, &opt->size.height);

        if (opt->size.width > width)
            width = opt->size.width;
        height += opt->size.height;

    arrst_end();

    if (width < MIN_WIDTH)
        width = MIN_WIDTH;

    if (height < MIN_HEIGHT)
        height = MIN_HEIGHT;

    view_size(menu->view, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENUVERT_CREATE )
{
    Panel *panel = panel_create();
    Layout *layout = layout_create(1,1);
    View *view = view_create();
    MenuVert *menu = i_create();
    menu->font = hb_gt_global_font();
    menu->view = view;
    menu->selected = 0;
    i_view_size(menu);
    view_OnDraw(view, listener(panel, i_OnDraw, Panel));
    panel_data(panel, &menu, i_destroy, MenuVert);
    layout_view(layout, view, 0, 0);
    panel_layout(panel, layout);
    hb_retptr(panel);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_MENUVERT_ADD )
{
    Panel *panel = (Panel*)hb_parptr(1);
    const char_t *text = hb_get_nap_text(2);
    PHB_ITEM codeBlock = hb_param(3, HB_IT_BLOCK);
    MenuVert *menu = panel_get_data(panel, MenuVert);
    MenuOpt *opt = arrst_new0(menu->opts, MenuOpt);
    opt->text = str_c(text);
    opt->codeBlock = hb_itemNew(codeBlock);
    i_view_size(menu);
}
