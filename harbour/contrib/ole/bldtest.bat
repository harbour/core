@set HB_BIN_INSTALL=..\..\bin
@set HB_LIB_INSTALL=..\..\lib\
@set HB_INC_INSTALL=..\..\include\

   %HB_BIN_INSTALL%\harbour %1.prg -n -i%HB_INC_INSTALL% %2
   bcc32 -O2 -d -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib dbffpt.lib hbsix.lib common.lib codepage.lib hbole.lib
   del %1.c
   del %1.obj
   del %1.tds
