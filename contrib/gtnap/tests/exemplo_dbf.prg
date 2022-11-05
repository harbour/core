#require "gtnap"
#include "gtnap.ch"

FIELD cdindx, dtcota, vlcota

/*---------------------------------------------------------------------------*/

PROC EXEMPLO_BROWSE_DBF()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1

    // Database index
    // Use '/' and not '\' (problems in Linux)
    IF FILE("dados/cotacao.dbf") .AND. .NOT. FILE("dados/cotacao.cdx")
        USE dados/cotacao NEW EXCLUSIVE
        INDEX ON COTACAO->CDINDX+DTOS(COTACAO->DTCOTA) TAG COTACAO1 TO dados/cotacao.cdx
        CLOSE COTACAO
    ELSE
        // MOSTRAR("M?????", "COTACAO index up-to-date", "Informação")
    ENDIF

    // Crash when INDEX is used
    // USE dados/cotacao NEW SHARED
    // SET INDEX TO dados/cotacao.cdx
    // GOTO TOP
    // MOSTRAR("M?????", "OK!!!", "Informação")

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
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL V_Label1, V_Table
    LOCAL V_Button1, V_Button2, V_Button3, V_Button4, V_Button5
    LOCAL V_Image, V_ImageView
    LOCAL C_String := "Browse de arquivo DBF" + hb_eol() + "seleção simples," + hb_eol() + "com grid e com toolbar," + hb_eol() + "com opções adicionais"

    // DataBase connection
    // Use '/' and not '\' (problems in Linux)
    USE dados/cotacao NEW SHARED
    // SORT ON cdindx, dtcota TO dados/cotacao00
    // CLOSE COTACAO
    // USE dados/cotacao00 NEW SHARED
    GOTO TOP

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 7)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 1)
    V_Image := NAP_IMAGE_FROM_FILE(DIRET_BMPS() + "logaspec.bmp")
    V_ImageView := NAP_IMAGEVIEW_CREATE()
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
    NAP_IMAGEVIEW_IMAGE(V_ImageView, V_Image)
    NAP_IMAGEVIEW_SIZE(V_ImageView, 100, 100)
    NAP_IMAGEVIEW_SCALE(V_ImageView, ekNAP_SCALE_ASPECTDW)
    NAP_TABLEVIEW_SIZE(V_Table, 600, 450)
    NAP_TABLEVIEW_GRID(V_Table, .T., .T.)
    NAP_TABLEVIEW_MULTISEL(V_Table, .F., .F.)
    NAP_TABLEVIEW_BIND_DB(V_Table)
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "D.", 40, ekNAP_ALIGN_LEFT, {|| TESTA_DELECAO(.NOT. EOF())})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Moeda", 60, ekNAP_ALIGN_LEFT, {|| cdindx})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Data", 100, ekNAP_ALIGN_CENTER, {|| dtcota})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Data+1", 100, ekNAP_ALIGN_CENTER, {|| dtcota + 1})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Data+2", 100, ekNAP_ALIGN_CENTER, {|| dtcota + 2})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Cotação", 140, ekNAP_ALIGN_RIGHT, {|| TRANSFORM(vlcota, "@E 999,999,999,999.99999999")})
    NAP_TABLEVIEW_UPDATE(V_Table)
    NAP_BUTTON_TEXT(V_Button1, "Incrementar valor corrente: " + DTOS(COTACAO->DTCOTA))
    NAP_BUTTON_TEXT(V_Button2, "Incrementar todos os valores")
    NAP_BUTTON_TEXT(V_Button3, "Exclui linha atual")
    NAP_BUTTON_TEXT(V_Button4, "Procura linha atual")
    NAP_BUTTON_TEXT(V_Button5, "Selecionar")
    NAP_BUTTON_ONCLICK(V_Button1, {|| INCREMENTA_CORRENTE(V_Table) })
    NAP_BUTTON_ONCLICK(V_Button2, {|| INCREMENTA_TODOS(V_Table) })
    NAP_BUTTON_ONCLICK(V_Button3, {|| EXCLUI_LINHA_ATUAL(V_Table) })
    NAP_LAYOUT_IMAGEVIEW(V_Layout2, V_ImageView, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_Label1, 1, 0)
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 0)
    NAP_LAYOUT_TABLEVIEW(V_Layout1, V_Table, 0, 1)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button1, 0, 2)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button2, 0, 3)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button3, 0, 4)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button4, 0, 5)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button5, 0, 6)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_VMARGIN(V_Layout1, 1, 20)
    NAP_LAYOUT_VMARGIN(V_Layout1, 2, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 3, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 4, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 5, 5)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 20, 20, 20)
    NAP_LAYOUT_HEXPAND(V_Layout2, 1)
    NAP_LAYOUT_HALIGN(V_Layout2, 1, 0, ekNAP_ALIGN_CENTER)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 2, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 3, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 4, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 5, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 6, ekNAP_ALIGN_LEFT)

    V_Layout3 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Calculadora", "Ajuda", "Saida"})
    NAP_PANEL_LAYOUT(V_Panel, V_Layout3)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Browse de arquivo DBF")
    NAP_WINDOW_FOCUS(V_Janela, V_Layout1, 0, 1)  // KeyBoard focus in table by default
    NAP_WINDOW_MODAL(V_Janela)

    // DataBase close
    CLOSE COTACAO
    RETURN

/*---------------------------------------------------------------------------*/

STATIC FUNCTION TESTA_DELECAO(L_NAO_EOF)
    LOCAL C_Str
    IF L_NAO_EOF
        IF DELETED()
            C_Str := "Sim"
            ALTD()
        ELSE
            C_Str := "Não"
        ENDIF
    ELSE
        C_Str := "Eof"
    ENDIF
    Return C_Str

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE INCREMENTA_CORRENTE(V_Table)

    LOCAL N_Selecionado := NAP_TABLEVIEW_SELECTED(V_Table)
    LOCAL N_Cont, N_NumSel := LEN(N_Selecionado)

    FOR N_Cont := 1 TO N_NumSel
        dbGoto(N_Selecionado[N_Cont])
        RLOCK()
        REPL vlcota WITH vlcota+1
        DBCOMMIT()
        UNLOCK
    NEXT

    NAP_TABLEVIEW_UPDATE(V_Table)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE INCREMENTA_TODOS(V_Table)
    FLOCK()
    REPL ALL vlcota WITH vlcota+1
    DBCOMMIT()
    UNLOCK
    NAP_TABLEVIEW_UPDATE(V_Table)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE EXCLUI_LINHA_ATUAL(V_Table)
    LOCAL N_Selecionado := NAP_TABLEVIEW_SELECTED(V_Table)
    LOCAL N_Cont, N_NumSel := LEN(N_Selecionado)

    FOR N_Cont := 1 TO N_NumSel
        dbGoto(N_Selecionado[N_Cont])
        RLOCK()
        DELETE
        DBCOMMIT()
        UNLOCK
    NEXT

    NAP_TABLEVIEW_UPDATE(V_Table)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_Label1, V_Table
    LOCAL V_Button1, V_Button2, V_Button3
    LOCAL C_String := "Browse de arquivo DBF" + hb_eol() + "seleção múltipla," + hb_eol() + "sem grid e sem toolbar"

    // DataBase connection
    // Use '/' and not '\' (problems in Linux)
    USE dados/cotacao NEW SHARED
    //SET INDEX TO dados/cotacao
    GOTO TOP

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 5)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_Table := NAP_TABLEVIEW_CREATE()
    V_Button1 := NAP_BUTTON_PUSH()
    V_Button2 := NAP_BUTTON_PUSH()
    V_Button3 := NAP_BUTTON_PUSH()
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_TABLEVIEW_SIZE(V_Table, 600, 450)
    NAP_TABLEVIEW_GRID(V_Table, .F., .F.)
    NAP_TABLEVIEW_MULTISEL(V_Table, .T., .T.)
    NAP_TABLEVIEW_HEADER_CLICKABLE(V_Table, .T.)
    NAP_TABLEVIEW_HEADER_RESIZABLE(V_Table, .T.)
    NAP_TABLEVIEW_BIND_DB(V_Table)
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "D.", 40, ekNAP_ALIGN_LEFT, {|| TESTA_DELECAO(.NOT. EOF())})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Moeda", 60, ekNAP_ALIGN_LEFT, {|| cdindx})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Data", 100, ekNAP_ALIGN_CENTER, {|| dtcota})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Data+1", 100, ekNAP_ALIGN_CENTER, {|| dtcota + 1})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Data+2", 100, ekNAP_ALIGN_CENTER, {|| dtcota + 2})
    NAP_TABLEVIEW_COLUMN_DB(V_Table, "Cotação", 140, ekNAP_ALIGN_RIGHT, {|| TRANSFORM(vlcota, "@E 999,999,999,999.99999999")})
    NAP_TABLEVIEW_COLUMN_FREEZE(V_Table, 2)
    NAP_TABLEVIEW_UPDATE(V_Table)
    DEFAULT_SELECIONADOS(V_Table)
    NAP_BUTTON_TEXT(V_Button1, "Incrementar registros selecionados")
    NAP_BUTTON_TEXT(V_Button2, "Voltar seleção para o 'default'")
    NAP_BUTTON_TEXT(V_Button3, "Barra de espaço = marcar")
    NAP_BUTTON_ONCLICK(V_Button1, {|| INCREMENTA_CORRENTE(V_Table) })
    NAP_BUTTON_ONCLICK(V_Button2, {|| DEFAULT_SELECIONADOS(V_Table) })
    // NAP_BUTTON_ONCLICK(V_Button3, {| hEv | EXCLUI_LINHA_ATUAL(hEv, V_Table) })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_TABLEVIEW(V_Layout1, V_Table, 0, 1)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button1, 0, 2)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button2, 0, 3)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button3, 0, 4)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_VMARGIN(V_Layout1, 1, 20)
    NAP_LAYOUT_VMARGIN(V_Layout1, 2, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 3, 5)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 20, 20, 20)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 2, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 3, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 4, ekNAP_ALIGN_LEFT)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Browse de arquivo DBF")
    NAP_WINDOW_FOCUS(V_Janela, V_Layout1, 0, 1)  // KeyBoard focus in table by default
    NAP_WINDOW_MODAL(V_Janela)

    // DataBase close
    CLOSE COTACAO
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE DEFAULT_SELECIONADOS(V_Table)
    LOCAL Default := {2, 4, 6, 8}
    NAP_TABLEVIEW_DESELECT_ALL(V_Table)
    NAP_TABLEVIEW_SELECT(V_Table, Default)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_COLUNA_CONGELADA()
    MOSTRAR("M?????", "Janela seleção estendida, com coluna congelada", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_WHILE()
    MOSTRAR("M?????", "Janela seleção, com 'while', sem barras de rolagem", "Informação")
    RETURN
