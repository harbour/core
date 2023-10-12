/*
 * NAppGUI Cross-platform C SDK
 * 2015-2023 Francisco Garcia Collado
 * MIT Licence
 * https://nappgui.com/en/legal/license.html
 *
 * File: osgui.inl
 *
 */

/* Operating system native gui */

#include "osgui.ixx"

__EXTERN_C

void _osgui_start_imp(void);

void _osgui_finish_imp(void);

Font *_osgui_create_default_font(void);

void _osgui_word_size(StringSizeData *data, const char_t *word, real32_t *width, real32_t *height);

void _osgui_text_bounds(StringSizeData *data, const char_t *text, const real32_t refwidth, real32_t *width, real32_t *height);

const char_t *_osgui_component_type(const gui_type_t type);

bool_t _osgui_button_text_allowed(const uint32_t flags);

bool_t _osgui_button_image_allowed(const uint32_t flags);

gui_size_t _osgui_size_font(const real32_t font_size);

vkey_t _osgui_vkey_from_text(const char_t *text);

void _osgui_attach_menubar(OSWindow *window, OSMenu *menu);

void _osgui_detach_menubar(OSWindow *window, OSMenu *menu);

void _osgui_change_menubar(OSWindow *window, OSMenu *previous_menu, OSMenu *new_menu);

void _osgui_message_loop(void);

/* Move to oscontrol.inl */
bool_t _osedit_validate(const OSEdit *edit, const OSControl *next_control);

bool_t _ospanel_with_scroll(const OSPanel *panel);

void _ospanel_scroll(OSPanel *panel, const int32_t x, const int32_t y);

void _ospanel_scroll_frame(const OSPanel *panel, OSFrame *rect);

OSWidget *_osgui_control_focus_widget(const OSControl *control);

OSWidget *_osgui_control_focused(void);

void _osgui_control_set_focused(OSWidget *widget);

bool_t _osgui_control_widget_visible(const OSWidget *widget);

bool_t _osgui_control_widget_enable(const OSWidget *widget);

OSControl *_osgui_control_parent(const OSControl *control);

gui_type_t _osgui_control_type(const OSControl *control);

void _osgui_control_frame(const OSControl *control, OSFrame *rect);



bool_t _osgui_control_can_close_window(const ArrPt(OSControl) *tabstops);

void _osgui_control_set_next_tabstop(const ArrPt(OSControl) *tabstops, const bool_t tabstop_cycle, OSControl **curtabstop);

void _osgui_control_set_previous_tabstop(const ArrPt(OSControl) *tabstops, const bool_t tabstop_cycle, OSControl **curtabstop);

void _osgui_control_set_tabstop(const ArrPt(OSControl) *tabstops, const bool_t tabstop_cycle, OSControl **tabstop);

__END_C
