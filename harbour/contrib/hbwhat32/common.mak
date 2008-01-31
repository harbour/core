#
# $Id$
#

LIBNAME = $(LIBPREF)hbwhat32

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    commctrl.ch \
    commdlg.ch \
    debug.ch \
    import.ch \
    sqltypes.ch \
    what32.ch \
    wingdi.ch \
    wininet.ch \
    winstruc.ch \
    wintypes.ch \
    winuser.ch \

LIB_OBJS = \
    $(OBJ_DIR)_winbmp$(OBJEXT) \
    $(OBJ_DIR)_winbrsh$(OBJEXT) \
    $(OBJ_DIR)_wincall$(OBJEXT) \
    $(OBJ_DIR)_wincdlg$(OBJEXT) \
    $(OBJ_DIR)_winclpb$(OBJEXT) \
    $(OBJ_DIR)_wincomm$(OBJEXT) \
    $(OBJ_DIR)_wincret$(OBJEXT) \
    $(OBJ_DIR)_windate$(OBJEXT) \
    $(OBJ_DIR)_windc$(OBJEXT) \
    $(OBJ_DIR)_windir$(OBJEXT) \
    $(OBJ_DIR)_windlg$(OBJEXT) \
    $(OBJ_DIR)_windll$(OBJEXT) \
    $(OBJ_DIR)_windraw$(OBJEXT) \
    $(OBJ_DIR)_winfont$(OBJEXT) \
    $(OBJ_DIR)_wingdi$(OBJEXT) \
    $(OBJ_DIR)_winhead$(OBJEXT) \
    $(OBJ_DIR)_winicon$(OBJEXT) \
    $(OBJ_DIR)_winilst$(OBJEXT) \
    $(OBJ_DIR)_wininet$(OBJEXT) \
    $(OBJ_DIR)_winini$(OBJEXT) \
    $(OBJ_DIR)_winkbrd$(OBJEXT) \
    $(OBJ_DIR)_winlv$(OBJEXT) \
    $(OBJ_DIR)_winmain$(OBJEXT) \
    $(OBJ_DIR)_winmapi$(OBJEXT) \
    $(OBJ_DIR)_winmem$(OBJEXT) \
    $(OBJ_DIR)_winmenu$(OBJEXT) \
    $(OBJ_DIR)_winmeta$(OBJEXT) \
    $(OBJ_DIR)_winmisc$(OBJEXT) \
    $(OBJ_DIR)_winmmcap$(OBJEXT) \
    $(OBJ_DIR)_winmous$(OBJEXT) \
    $(OBJ_DIR)_winmsg$(OBJEXT) \
    $(OBJ_DIR)_winpen$(OBJEXT) \
    $(OBJ_DIR)_winprn$(OBJEXT) \
    $(OBJ_DIR)_winrect$(OBJEXT) \
    $(OBJ_DIR)_winreg$(OBJEXT) \
    $(OBJ_DIR)_winrgn$(OBJEXT) \
    $(OBJ_DIR)_winscrlb$(OBJEXT) \
    $(OBJ_DIR)_winserial$(OBJEXT) \
    $(OBJ_DIR)_winshell$(OBJEXT) \
    $(OBJ_DIR)_winsock$(OBJEXT) \
    $(OBJ_DIR)_winsys$(OBJEXT) \
    $(OBJ_DIR)_wintab$(OBJEXT) \
    $(OBJ_DIR)_wintbar$(OBJEXT) \
    $(OBJ_DIR)_wintext$(OBJEXT) \
    $(OBJ_DIR)_wintree$(OBJEXT) \
    $(OBJ_DIR)_winview$(OBJEXT) \
    $(OBJ_DIR)_winwnd$(OBJEXT) \
    \
    $(OBJ_DIR)whatutil$(OBJEXT) \
    $(OBJ_DIR)wincdlg$(OBJEXT) \
    $(OBJ_DIR)wincomm$(OBJEXT) \
    $(OBJ_DIR)wincore$(OBJEXT) \
    $(OBJ_DIR)windebug$(OBJEXT) \
    $(OBJ_DIR)winerror$(OBJEXT) \
    $(OBJ_DIR)winini$(OBJEXT) \
    $(OBJ_DIR)winrbar$(OBJEXT) \
    $(OBJ_DIR)wintabs$(OBJEXT) \
    $(OBJ_DIR)wintbar$(OBJEXT) \

all: \
    $(LIB_PATH) \
