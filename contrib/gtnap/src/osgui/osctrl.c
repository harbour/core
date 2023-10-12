/* Controls shared functionality */

#include "osctrl.inl"
#include <core/arrpt.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

static int32_t i_SCROLL_OFFSET = 10;

/*---------------------------------------------------------------------------*/

bool_t oscontrol_validate(const OSControl *control, const OSControl *next_control)
{
    gui_type_t type = oscontrol_type(control);
    if (type == ekGUI_TYPE_EDITBOX)
        return osedit_validate((OSEdit*)control, next_control);
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static uint32_t i_search_tabstop(const ArrPt(OSControl) *tabstops, OSWidget *widget)
{
    if (widget == NULL)
        return UINT32_MAX;

    arrpt_foreach_const(tabstop, tabstops, OSControl)
        if (oscontrol_focus_widget(tabstop) == widget)
            return tabstop_i;
    arrpt_end();

    return UINT32_MAX;
}

/*---------------------------------------------------------------------------*/

bool_t oscontrol_can_close_window(const ArrPt(OSControl) *tabstops, OSWindow *window)
{
    OSWidget *widget = oscontrol_widget_get_focus(window);
    uint32_t tabindex = i_search_tabstop(tabstops, widget);
    if (tabindex != UINT32_MAX)
    {
        const OSControl *control = arrpt_get_const(tabstops, tabindex, OSControl);
        return oscontrol_validate(control, NULL);
    }

    return TRUE;
}

/*---------------------------------------------------------------------------*/

static OSControl *i_effective_tabstop(const ArrPt(OSControl) *tabstops, const uint32_t index, const bool_t reverse, const bool_t tabstop_cycle)
{
    uint32_t idx = index, i;
    const OSControl **tabstop = arrpt_all_const(tabstops, OSControl);
    uint32_t size = arrpt_size(tabstops, OSControl);
    cassert(index < size);
    for (i = 0; i < size; ++i)
    {
        OSWidget *widget = oscontrol_focus_widget(tabstop[idx]);
        if (widget != NULL && oscontrol_widget_visible(widget) == TRUE && oscontrol_widget_enable(widget) == TRUE)
            return (OSControl*)tabstop[idx];

        if (reverse == TRUE)
        {
            if (idx == 0)
            {
                if (tabstop_cycle == TRUE)
                    idx = size - 1;
            }
            else
            {
                idx -= 1;
            }
        }
        else
        {
            if (idx == size - 1)
            {
                if (tabstop_cycle == TRUE)
                    idx = 0;
            }
            else
            {
                idx += 1;
            }
        }
    }

    return NULL;
}

/*---------------------------------------------------------------------------*/

static void i_set_tabstop(const OSControl *tabstop)
{
    OSWidget *widget = oscontrol_focus_widget(tabstop);
    OSControl *parent = oscontrol_parent(tabstop);
    gui_type_t ptype = ENUM_MAX(gui_type_t);

    cassert_no_null(parent);
    ptype = oscontrol_type(parent);
    oscontrol_widget_set_focus(widget);

    if (parent != NULL && ptype == ekGUI_TYPE_PANEL)
    {
        OSPanel *panel = (OSPanel*)parent;
        /* Automatic panel scrolling if focused control is not completely visible */
        if (ospanel_with_scroll(panel) == TRUE)
        {
            OSFrame prect, crect;
            int32_t scroll_x = INT32_MAX, scroll_y = INT32_MAX;
            ospanel_scroll_frame((OSPanel*)parent, &prect);
            oscontrol_frame(tabstop, &crect);

            if (prect.left > crect.left)
                scroll_x = (crect.left - i_SCROLL_OFFSET);
            else if (prect.right < crect.right)
                scroll_x = (crect.right + i_SCROLL_OFFSET) - (prect.right - prect.left);

            if (prect.top > crect.top)
                scroll_y = (crect.top - i_SCROLL_OFFSET);
            else if (prect.bottom < crect.bottom)
                scroll_y = (crect.bottom + i_SCROLL_OFFSET) - (prect.bottom - prect.top);

            if (scroll_x != INT32_MAX || scroll_y != INT32_MAX)
                ospanel_scroll(panel, scroll_x, scroll_y);
        }
    }
}

/*---------------------------------------------------------------------------*/

void oscontrol_taborder(ArrPt(OSControl) *tabstops, OSControl *control)
{
    if (control != NULL)
    {
        gui_type_t type = oscontrol_type(control);
        cassert_unref(type != ekGUI_TYPE_PANEL, type);
        arrpt_append(tabstops, control, OSControl);
    }
    else
    {
        arrpt_clear(tabstops, NULL, OSControl);
    }
}

/*---------------------------------------------------------------------------*/

void oscontrol_set_next_tabstop(const ArrPt(OSControl) *tabstops, OSWindow *window, const bool_t tabstop_cycle, OSControl **curtabstop)
{
    uint32_t size = arrpt_size(tabstops, OSControl);
    if (size > 0)
    {
        OSWidget *widget = oscontrol_widget_get_focus(window);
        uint32_t tabindex = i_search_tabstop(tabstops, widget);
        uint32_t next_tabindex = tabindex;
        bool_t move_tabstop = TRUE;
        OSControl *next_control = NULL;

        if (next_tabindex == UINT32_MAX)
            next_tabindex = 0;

        if (next_tabindex == size - 1)
        {
            if (tabstop_cycle == TRUE)
                next_tabindex = 0;
        }
        else
        {
            next_tabindex += 1;
        }

        next_control = i_effective_tabstop(tabstops, next_tabindex, FALSE, tabstop_cycle);

        if (tabindex != UINT32_MAX)
        {
            const OSControl *tabstop = arrpt_get_const(tabstops, tabindex, OSControl);
            move_tabstop = oscontrol_validate(tabstop, next_control);
        }

        if (move_tabstop == TRUE)
        {
            if (next_tabindex != tabindex)
            {
                cassert_no_null(curtabstop);
                i_set_tabstop(next_control);
                *curtabstop = next_control;
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

void oscontrol_set_previous_tabstop(const ArrPt(OSControl) *tabstops, OSWindow *window, const bool_t tabstop_cycle, OSControl **curtabstop)
{
    uint32_t size = arrpt_size(tabstops, OSControl);
    if (size > 0)
    {
        OSWidget *widget = oscontrol_widget_get_focus(window);
        uint32_t tabindex = i_search_tabstop(tabstops, widget);
        uint32_t next_tabindex = tabindex;
        bool_t move_tabstop = TRUE;
        OSControl *next_control = NULL;

        if (next_tabindex == UINT32_MAX)
            next_tabindex = 0;

        if (next_tabindex == 0)
        {
            if (tabstop_cycle == TRUE)
                next_tabindex = size - 1;
        }
        else
        {
            next_tabindex -= 1;
        }

        next_control = i_effective_tabstop(tabstops, next_tabindex, TRUE, tabstop_cycle);

        if (tabindex != UINT32_MAX)
        {
            const OSControl *tabstop = arrpt_get_const(tabstops, tabindex, OSControl);
            move_tabstop = oscontrol_validate(tabstop, next_control);
        }

        if (move_tabstop == TRUE)
        {
            if (next_tabindex != tabindex)
            {
                cassert_no_null(curtabstop);
                i_set_tabstop(next_control);
                *curtabstop = next_control;
            }
        }
    }
}

/*---------------------------------------------------------------------------*/

void oscontrol_set_tabstop(const ArrPt(OSControl) *tabstops, const bool_t tabstop_cycle, OSControl **tabstop)
{
    uint32_t size = arrpt_size(tabstops, OSControl);
    cassert_no_null(tabstop);
    if (size > 0)
    {
        OSControl *control = *tabstop;
        OSWidget *widget = NULL;
        uint32_t tabindex = 0;

        if (control == NULL)
            control = (OSControl*)arrpt_first_const(tabstops, OSControl);

        widget = oscontrol_focus_widget(control);

        tabindex = i_search_tabstop(tabstops, widget);
        if (tabindex == UINT32_MAX)
            tabindex = 0;

        control = i_effective_tabstop(tabstops, tabindex, FALSE, tabstop_cycle);
        if (control != NULL)
        {
            i_set_tabstop(control);
            *tabstop = control;
        }
    }
}
