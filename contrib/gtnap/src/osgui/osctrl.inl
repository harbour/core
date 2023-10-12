/* Controls shared functionality */

#include "osgui.ixx"

__EXTERN_C

/* Specific implementation in each platform */

bool_t osedit_validate(const OSEdit *edit, const OSControl *next_control);

bool_t ospanel_with_scroll(const OSPanel *panel);

void ospanel_scroll(OSPanel *panel, const int32_t x, const int32_t y);

void ospanel_scroll_frame(const OSPanel *panel, OSFrame *rect);

gui_type_t oscontrol_type(const OSControl *control);

OSControl *oscontrol_parent(const OSControl *control);

void oscontrol_frame(const OSControl *control, OSFrame *rect);

OSWidget *oscontrol_focus_widget(const OSControl *control);

OSWidget *oscontrol_widget_get_focus(OSWindow *window);

void oscontrol_widget_set_focus(OSWidget *widget);

bool_t oscontrol_widget_visible(const OSWidget *widget);

bool_t oscontrol_widget_enable(const OSWidget *widget);

/* Common implementation in osctrl.c */

bool_t oscontrol_validate(const OSControl *control, const OSControl *next_control);

bool_t oscontrol_can_close_window(const ArrPt(OSControl) *tabstops, OSWindow *window);

void oscontrol_taborder(ArrPt(OSControl) *tabstops, OSControl *control);

void oscontrol_set_next_tabstop(const ArrPt(OSControl) *tabstops, OSWindow *window, const bool_t tabstop_cycle, OSControl **curtabstop);

void oscontrol_set_previous_tabstop(const ArrPt(OSControl) *tabstops, OSWindow *window, const bool_t tabstop_cycle, OSControl **curtabstop);

void oscontrol_set_tabstop(const ArrPt(OSControl) *tabstops, const bool_t tabstop_cycle, OSControl **tabstop);

__END_C
