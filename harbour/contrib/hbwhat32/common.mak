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
    $(OBJ_DIR)wincorec$(OBJEXT) \
    $(OBJ_DIR)whtbmp$(OBJEXT) \
    $(OBJ_DIR)whtbrsh$(OBJEXT) \
    $(OBJ_DIR)whtcall$(OBJEXT) \
    $(OBJ_DIR)whtcdlg$(OBJEXT) \
    $(OBJ_DIR)whtclpb$(OBJEXT) \
    $(OBJ_DIR)whtcomm$(OBJEXT) \
    $(OBJ_DIR)whtcret$(OBJEXT) \
    $(OBJ_DIR)whtdate$(OBJEXT) \
    $(OBJ_DIR)whtdc$(OBJEXT) \
    $(OBJ_DIR)whtdir$(OBJEXT) \
    $(OBJ_DIR)whtdlg$(OBJEXT) \
    $(OBJ_DIR)whtdll$(OBJEXT) \
    $(OBJ_DIR)whtdraw$(OBJEXT) \
    $(OBJ_DIR)whtfont$(OBJEXT) \
    $(OBJ_DIR)whtgdi$(OBJEXT) \
    $(OBJ_DIR)whthead$(OBJEXT) \
    $(OBJ_DIR)whticon$(OBJEXT) \
    $(OBJ_DIR)whtilst$(OBJEXT) \
    $(OBJ_DIR)whtinet$(OBJEXT) \
    $(OBJ_DIR)whtini$(OBJEXT) \
    $(OBJ_DIR)whtkbrd$(OBJEXT) \
    $(OBJ_DIR)whtlv$(OBJEXT) \
    $(OBJ_DIR)whtmain$(OBJEXT) \
    $(OBJ_DIR)whtmapi$(OBJEXT) \
    $(OBJ_DIR)whtmem$(OBJEXT) \
    $(OBJ_DIR)whtmenu$(OBJEXT) \
    $(OBJ_DIR)whtmeta$(OBJEXT) \
    $(OBJ_DIR)whtmisc$(OBJEXT) \
    $(OBJ_DIR)whtmmcap$(OBJEXT) \
    $(OBJ_DIR)whtmous$(OBJEXT) \
    $(OBJ_DIR)whtmsg$(OBJEXT) \
    $(OBJ_DIR)whtpen$(OBJEXT) \
    $(OBJ_DIR)whtprn$(OBJEXT) \
    $(OBJ_DIR)whtrect$(OBJEXT) \
    $(OBJ_DIR)whtreg$(OBJEXT) \
    $(OBJ_DIR)whtrgn$(OBJEXT) \
    $(OBJ_DIR)whtscrlb$(OBJEXT) \
    $(OBJ_DIR)whtseria$(OBJEXT) \
    $(OBJ_DIR)whtshell$(OBJEXT) \
    $(OBJ_DIR)whtsock$(OBJEXT) \
    $(OBJ_DIR)whtsys$(OBJEXT) \
    $(OBJ_DIR)whttab$(OBJEXT) \
    $(OBJ_DIR)whttbar$(OBJEXT) \
    $(OBJ_DIR)whttext$(OBJEXT) \
    $(OBJ_DIR)whttree$(OBJEXT) \
    $(OBJ_DIR)whtview$(OBJEXT) \
    $(OBJ_DIR)whtwnd$(OBJEXT) \
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
