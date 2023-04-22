/*
 * NAppGUI Cross-platform C SDK
 * 2015-2023 Francisco Garcia Collado
 * MIT Licence
 * https://nappgui.com/en/legal/license.html
 *
 * File: oswindow.inl
 *
 */

/* Operating System native window */

#include "osgui_gtk.ixx"

__EXTERN_C

void _oswindow_set_menubar(OSWindow *window, OSMenu *menu);

void _oswindow_unset_menubar(OSWindow *window, OSMenu *menu);

void _oswindow_gtk_app(GtkApplication *app, GdkPixbuf *icon);

void _oswindow_set_app_terminate(void);

void _oswindow_unset_focus(OSWindow *window);

void _oswindow_unset_defbutton(OSControl *control);

bool_t _oswindow_can_mouse_down(OSControl *control);

bool_t _oswindow_in_tablist(OSControl *control);

void _oswindow_lock_edit_focus_events(OSControl *control);

void _oswindow_unlock_edit_focus_events(OSControl *control);

bool_t _oswindow_can_edit_focus_events(OSControl *control);

__END_C

