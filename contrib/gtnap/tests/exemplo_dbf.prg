#require "gtnap"
#include "gtnap.ch"

PROC EXEMPLO_BROWSE_DBF()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1

    // Database index
    IF FILE("dados\cotacao.dbf") .AND. .NOT. FILE("dados\cotacao.cdx")
        USE dados\cotacao NEW EXCLUSIVE
        INDEX ON COTACAO->CDINDX+DTOS(COTACAO->DTCOTA) TAG COTACAO1 TO dados\cotacao.cdx
        CLOSE COTACAO
    ELSE
       // MOSTRAR("M?????", "COTACAO index up-to-date", "Informação")
    ENDIF

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de janela de browse de DBF")
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção simples, com grid, com toolbar", {|| TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção múltipla, sem grid, nem toolbar", {|| TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção estendida, com coluna congelada", {|| TST_BROWSE_DBF_COLUNA_CONGELADA() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção, com 'while', sem barras de rolagem", {|| TST_BROWSE_DBF_WHILE() })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 100, 20, 100)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Escolha o tipo de janela")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_Label1, V_Table
    LOCAL V_Button1, V_Button2, V_Button3, V_Button4, V_Button5
    LOCAL C_String := "Browse de arquivo DBF" + hb_eol() + "seleção simples," + hb_eol() + "com grid e com toolbar," + hb_eol() + "com opções adicionais"
    LOCAL total
    // DataBase connection
    USE dados\cotacao NEW SHARED
    //SET INDEX TO dados\cotacao
    GOTO TOP

    total := cotacao->(RecCount())

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 7)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_Table := NAP_TABLEVIEW_CREATE()
    V_Button1 := NAP_BUTTON_PUSH()
    V_Button2 := NAP_BUTTON_PUSH()
    V_Button3 := NAP_BUTTON_PUSH()
    V_Button4 := NAP_BUTTON_PUSH()
    V_Button5 := NAP_BUTTON_PUSH()
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    //NAP_LABEL_BGCOLOR(V_Label1, NAP_COLOR_RED())
    NAP_TABLEVIEW_SIZE(V_Table, 600, 450)
    NAP_TABLEVIEW_BIND_DB(V_Table)
    NAP_BUTTON_TEXT(V_Button1, "Incrementar valor corrente")
    NAP_BUTTON_TEXT(V_Button2, "Incrementar todos os valores")
    NAP_BUTTON_TEXT(V_Button3, "Exclui linha atual")
    NAP_BUTTON_TEXT(V_Button4, "Procura linha atual")
    NAP_BUTTON_TEXT(V_Button5, "Selecionar")
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_TABLEVIEW(V_Layout1, V_Table, 0, 1)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button1, 0, 2)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button2, 0, 3)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button3, 0, 4)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button4, 0, 5)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button5, 0, 6)
    //NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_VMARGIN(V_Layout1, 1, 20)
    NAP_LAYOUT_VMARGIN(V_Layout1, 2, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 3, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 4, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 5, 5)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 20, 20, 20)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 2, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 3, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 4, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 5, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 6, ekNAP_ALIGN_LEFT)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Browse de arquivo DBF" + STR(total))
    NAP_WINDOW_MODAL(V_Janela)

    // DataBase close
    CLOSE COTACAO
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR()
    MOSTRAR("M?????", "Janela seleção múltipla, sem grid, nem toolbar", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_COLUNA_CONGELADA()
    MOSTRAR("M?????", "Janela seleção estendida, com coluna congelada", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_WHILE()
    MOSTRAR("M?????", "Janela seleção, com 'while', sem barras de rolagem", "Informação")
    RETURN
