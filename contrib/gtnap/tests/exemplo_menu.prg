#require "gtnap"
#include "gtnap.ch"

PROC EXEMPLO_MENU()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1, V_Label2, V_Label3
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de menu")
    NAP_MENUVERT_ADD(V_MenuVert, "Menu com rolamento", {|| TST_MENU_COM_ROLAMENTO() })
    NAP_MENUVERT_ADD(V_MenuVert, "Menu sem tecla Esc", {|| TST_MENU_SEM_BOTAO_ESC() })
    NAP_MENUVERT_ADD(V_MenuVert, "Menu com AutoClose", {|| TST_MENU_COM_AUTOCLOSE() })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 100, 20, 100)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Escolha o tipo de menu")
    NAP_WINDOW_MODAL(V_Janela)

RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_MENU_COM_ROLAMENTO()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE(3)  // 3 = Visible options (with vertical scroll)
    V_Label1 := NAP_LABEL_WITH_TEXT("Menu com rolamento")
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 1", { || MOSTRAR_OPCAO("1") })
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 2", { || MOSTRAR_OPCAO("2") })
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 3", { || MOSTRAR_OPCAO("3") })
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 4", { || MOSTRAR_OPCAO("4") })
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 5", { || MOSTRAR_OPCAO("5") })
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 6", { || MOSTRAR_OPCAO("6") })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 100, 20, 100)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Menu com rolamento")
    NAP_WINDOW_MODAL(V_Janela)

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE BUTTON_F9_CLICK()
    MOSTRAR("M?????", "Foi seleccionado botão F9", "Informação")
    // Close the current modal window
    NAP_WINDOW_STOP_MODAL(0)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_MENU_SEM_BOTAO_ESC()
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL V_MenuVert, V_Button
    LOCAL V_Label1, V_Image, V_ImageView

    // Create GUI Elements
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 1)
    V_Layout3 := NAP_LAYOUT_CREATE(1, 3)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Menu sem botão ESC")
    V_Image := NAP_IMAGE_FROM_FILE(DIRET_BMPS() + "logaspec.bmp")
    V_ImageView := NAP_IMAGEVIEW_CREATE()
    V_Button := NAP_BUTTON_PUSH()
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 1", { || MOSTRAR_OPCAO("1") })
    NAP_MENUVERT_ADD(V_MenuVert, "Opção 2", { || MOSTRAR_OPCAO("2") })
    NAP_BUTTON_TEXT(V_Button, "F9 = botão adicional")
    NAP_BUTTON_ONCLICK(V_Button, {|| BUTTON_F9_CLICK() })

    // Configure design (layout disposition)
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_IMAGEVIEW_IMAGE(V_ImageView, V_Image)
    NAP_IMAGEVIEW_SIZE(V_ImageView, 100, 100)
    NAP_IMAGEVIEW_SCALE(V_ImageView, ekNAP_SCALE_ASPECTDW)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_IMAGEVIEW(V_Layout2, V_ImageView, 0, 0)
    NAP_LAYOUT_LAYOUT(V_Layout2, V_Layout1, 1, 0)
    NAP_LAYOUT_LAYOUT(V_Layout3, V_Layout2, 0, 0)
    NAP_LAYOUT_BUTTON(V_Layout3, V_Button, 0, 1)

    // Disable tabstop for button
    // Keyboard focus always in MenuVert
    NAP_LAYOUT_TABSTOP(V_Layout3, 0, 1, .F.)

    // Margins and alignment
    // Vertical space between menu title and menu vert
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 25)
    // Horizontal space between image and menu
    NAP_LAYOUT_HMARGIN(V_Layout2, 0, 120)
    // Menu vert is horizontally centered with title
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 1, ekNAP_ALIGN_CENTER)
    // Image in aligned to top
    NAP_LAYOUT_VALIGN(V_Layout2, 0, 0, ekNAP_ALIGN_TOP)
    // Button is alignet to left
    NAP_LAYOUT_HALIGN(V_Layout3, 0, 1, ekNAP_ALIGN_LEFT)
    // Image internal padding
    NAP_LAYOUT_PADDING4(V_Layout2, 0, 0, 25, 0, 0, 0)
    // Button horizontal margin
    NAP_LAYOUT_VMARGIN(V_Layout3, 0, 60)
    // Globar border margin
    NAP_LAYOUT_MARGIN4(V_Layout3, 0, 100, 30, 20)

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout3)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Menu sem botão ESC")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_MENU_COM_AUTOCLOSE()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Menu com AutoClose")
    NAP_MENUVERT_ADD(V_MenuVert, "Procedure sem retorno", { || PROCEDURE_SEM_RETORNO() })
    NAP_MENUVERT_ADD(V_MenuVert, "Função que retorna NIL", { || FUNCAO_RETORNO_NIL() })
    NAP_MENUVERT_ADD(V_MenuVert, "Função que retorna .F.", { || FUNCAO_RETORNO_F() })
    NAP_MENUVERT_ADD(V_MenuVert, "Função que retorna .T.", { || FUNCAO_RETORNO_T() })
    NAP_MENUVERT_AUTOCLOSE(V_MenuVert, { || ON_AUTOCLOSE() })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 100, 20, 100)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Menu com AutoClose")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE ON_AUTOCLOSE()
    NAP_WINDOW_STOP_MODAL()
    MOSTRAR("M?????", "Menu foi fechado pela cláusula AutoClose", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE MOSTRAR_OPCAO( optNum )
    MOSTRAR("M?????", "Foi escolhida a opção " + optNum, "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE PROCEDURE_SEM_RETORNO
    MOSTRAR("M?????", "Procedure sem retorno executada", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC FUNCTION FUNCAO_RETORNO_NIL
    MOSTRAR("M?????", "Função com retorno NIL executada", "Informação")
    RETURN NIL

/*---------------------------------------------------------------------------*/

STATIC FUNCTION FUNCAO_RETORNO_F
    MOSTRAR("M?????", "Função com retorno .F. executada", "Informação")
    RETURN .F.

/*---------------------------------------------------------------------------*/

STATIC FUNCTION FUNCAO_RETORNO_T
    MOSTRAR("M?????","Função com retorno .T. executada."+;
                 "(menu será automaticamente fechado)", "Informação")
    RETURN .T.
