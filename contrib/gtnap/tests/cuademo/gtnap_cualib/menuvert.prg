/* encoding: cp850 */

#pragma DebugInfo=On

/*
* objeto MENU VERTICAL
*
*/
#INCLUDE "inkey.ch"
#INCLUDE "common.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "janela.ch"       // métodos externos da classe JANELA
#INCLUDE "gtnap.ch"

********************
FUNCTION EspMenuVert( VX_Janela, L_RolaVertical, L_AutoClose )
********************
*
LOCAL VX_Sele
*
IF L_CUA_10
   ? MEMVAR->JANELA_MENU_EXCLUSIVA_DA_CUA_20
ENDIF
*
DEFAULT L_RolaVertical TO .F.
DEFAULT L_AutoClose    TO .F.
IF ASCAN(V_RegiaoBotoes,{|V_Botao|"ENTER=" $ UPPER(V_Botao[_BOTAO_TEXTO_COMANDO])}) # 0
   ? MEMVAR->MENU_NAO_PODE_TER_BOTAO_ENTER
ENDIF
IF ASCAN(V_LstAcoes,{|V_Acao|K_ENTER==V_Acao[_ACAO_KEYBOARD]}) # 0
   ? MEMVAR->MENU_NAO_PODE_TER_ACAO_ENTER
ENDIF
*
AJUSTA_BOTOES(VX_Janela)  // ajusta Lin2Livre à quantidade de botões de função
*

IF L_RolaVertical
    // Fran: In GTNAP, the scroll bar is in widget (not in window)
    * prever espaço para scroll bar vertical
    IF .NOT. SOB_MODO_GRAFICO()
        Col2Livre(VX_Janela)--
        Col2Livre(VX_Janela)--
    ENDIF
    L_ScrollVertical := .T.
ENDIF

*
* No Harbour, foi preciso criar uma subclasse da Tbrowse()
* para poder se ter a lista de posições dos separadores de coluna,
* retornadas pelo método aColumnsSep() (criado dentro da própria CUA).
*
* NOTA: Menu vertical possui uma única coluna, nunca tendo separador de colunas.
*       Decidiu-se instanciar da subclasse somente para ficar igual à seleção em arquivo e em vetor.
VX_Sele := ;
   TBROWSESubClass():New(Lin1Livre(VX_Janela) , Col1Livre(VX_Janela),;
                         Lin2Livre(VX_Janela) , Col2Livre(VX_Janela))

*
* a tabela de cores deverá ter 3 cores :
*   Cor padrão com cor de frente intensificada - que teoricamente
*       seria utilizada em todo o Browse, mas que de fato influencia
*       apenas os cabeçalhos, conforme método AnexeCol ().
*   Cor padrão, cor cursor - herda as mesmas cores do objeto JANELA
*
VX_Sele:COLORSPEC := CorJanInten(VX_Janela) + "," + CorJanela(VX_Janela)
VX_Sele:AUTOLITE  := .F.          // cursor montado via método COLORRECT()
*
#DEFINE B_2LinCorrente  NIL   // será atribuído mais abaixo
#DEFINE L_PriFora      .F.    // indica se primeira linha não está na janela
#DEFINE L_UltFora      .F.    // indica se última linha não está na janela
#DEFINE L_ForcaLerTudo .T.    // forçar um refreshall() + estabilização das setas
#DEFINE L_PrimAtivacao .T.    // indica que é primeira ativação da janela
#DEFINE L_AtivaGui       .F.  // se a tecla de atalho deve ser sublinhada
#DEFINE VN_Selecio      {}    // seleção multipla ou extendida (sem uso em menus)
#DEFINE L_MostraGrade  .F.    // se mostra o grid (sem uso em menus)
#DEFINE N_Congela      0      // colunas a congelar (sem uso em menus)
#DEFINE N_TP_Selecao   _SELE_SIMPLES // modalidade de seleção (simp/mult/ext)
#DEFINE N_AlturaCabec  0      // indica a altura do cabecalho de colunas (sem uso em menus)
#DEFINE N_Selecio      1      // contém a linha corrente do cursor
#DEFINE N_ColunaIniVetor NIL  // posisão inicial da coluna do vetor
#DEFINE V_Opcoes        {}    // Lista de opções
#DEFINE L_TemHotKey     .F.   // se alguma opção tem o caractere "#"
#DEFINE V_Lst_CdOpcao   {}    // lista de opções para rotina de help
#DEFINE L_TeveRolaHorizontal .F.  // Nunca existe rolamento horizontal em menus (conterá sempre .F.)
VX_Sele:CARGO := { B_2LinCorrente , L_PriFora , L_UltFora , ;
                   L_ForcaLerTudo , L_PrimAtivacao , L_AtivaGui,;
                   L_AutoClose, VN_Selecio, L_MostraGrade,;
                   N_Congela , N_TP_Selecao, N_AlturaCabec,;
                   N_Selecio , N_ColunaIniVetor,;
		   V_Opcoes, L_TemHotKey, V_Lst_CdOpcao, L_TeveRolaHorizontal }
#UNDEF B_2LinCorrente
#UNDEF L_PriFora
#UNDEF L_UltFora
#UNDEF L_ForcaLerTudo
#UNDEF L_PrimAtivacao
#UNDEF L_AtivaGui
#UNDEF VN_Selecio
#UNDEF L_MostraGrade
#UNDEF N_Congela
#UNDEF N_TP_Selecao
#UNDEF N_AlturaCabec
#UNDEF N_Selecio
#UNDEF N_ColunaIniVetor
#UNDEF V_Opcoes
#UNDEF L_TemHotKey
#UNDEF V_Lst_CdOpcao
#UNDEF L_TeveRolaHorizontal
*
N_TP_Jan  := _JAN_MENU_VERT       // especializando
VX_SubObj := VX_Sele              // a janela
B_Metodo  := {||Selecionar(VX_Janela)}
*
SETA_SKIPBLOCK_VETOR(VX_Janela)
*
RETURN NIL
*
* DEFINICOES PARA USO GERAL
*
#DEFINE B_LinCorrente        VX_Sele:CARGO[01]      // bloco que retorna o item corrente
#DEFINE L_PriFora            VX_Sele:CARGO[02]      // sinaliza se 1º item não está na tela
#DEFINE L_UltFora            VX_Sele:CARGO[03]      // idem para o último
#DEFINE L_ForcaLerTudo       VX_Sele:CARGO[04]      // sinaliza a remontagem total da tela
#DEFINE L_PrimAtivacao       VX_Sele:CARGO[05]      // Se é a primeira ativação da janela
#DEFINE L_AtivaGui           VX_Sele:CARGO[06]      //
#DEFINE L_AutoClose          VX_Sele:CARGO[07]      // se é para fechar a janela automaticamente (cua 2.0)
#DEFINE VN_Selecio           VX_Sele:CARGO[08]      // vetor de seleção múltipla
#DEFINE L_MostraGrade        VX_Sele:CARGO[09]      // Se mostra o grid
#DEFINE N_Congela            VX_Sele:CARGO[10]      // colunas a congelar
#DEFINE N_TP_Selecao         VX_Sele:CARGO[11]      // modalidade de seleção (simp/mult/ext)
#DEFINE N_AlturaCabec        VX_Sele:CARGO[12]      // Altura do cabecalho de colunas
#DEFINE N_Selecio            VX_Sele:CARGO[13]      // item corrente (só para vetores)
#DEFINE N_ColunaIniVetor     VX_Sele:CARGO[14]      // Coluna inicial do vetor
#DEFINE V_Opcoes             VX_Sele:CARGO[15]      // Lista de opções do vetor
#DEFINE L_TemHotKey          VX_Sele:CARGO[16]      // se alguma opção tem o caractere "#"
#DEFINE V_Lst_CdOpcao        VX_Sele:CARGO[17]      // lista de opções para rotina de help
#DEFINE L_TeveRolaHorizontal VX_Sele:CARGO[18]      // Nunca existe rolamento horizontal em menus (conterá sempre .F.)
*

*************
PROC AddOpcao (VX_Janela,C_TxtOpcao,B_AcaoOpcao,;
               C_CdOpcao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_EofOk, L_HandleMuda,;
               L_MudaDados)
*************
IF L_CUA_10
   ? MEMVAR->ADDBOTAO_EXCLUSIVO_DA_CUA_20
ENDIF
*
IF N_TP_Jan # _JAN_MENU_VERT
   ? MEMVAR->ADDOPCAO_SO_PERMITIDA_EM_JANELA_DE_MENU
ENDIF
*
IF C_TelaCoberta # NIL
   ? MEMVAR->ADDOPCAO_EM_JANELA_JA_ATIVADA
ENDIF
*
IF .NOT. LEFT(C_CdOpcao,1) == "P"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdOpcao,"P0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdOpcao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
//!! no futuro, remover os testes abaixo
ASSUMIR_NIL_OU_FALSE({L_AliasMuda,;
                      L_RecnoMuda,;
                      L_FilterMuda,;
                      L_OrderMuda,;
                      L_EofOk,;
                      L_HandleMuda})
                      //  L_MudaDados --> Este parâmetro não está sendo passado para esta função, pois seu valor
		      // poderá ser é TRUE, caso a OPÇÃO seja indicado pelo parâmetro L_MudaDados.
*
#DEFINE VX_Sele   VX_SubObj
AADD(V_Lst_CdOpcao,{C_CdOpcao,STRTRAN(C_TxtOpcao,"#","")})
#UNDEF  VX_Sele
*
AddOpcaoInterna(VX_Janela,C_TxtOpcao,B_AcaoOpcao,C_CdOpcao,3,;
                L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_EofOk, L_HandleMuda, L_MudaDados)

*
**************************
STATIC FUNCTION Selecionar ( VX_Janela )
*
LOCAL VX_Sele := VX_SubObj
LOCAL L_Retorno
*
IF LEN(V_Opcoes)==0
   ? MEMVAR->JANELA_DE_MENU_SEM_ADDOPCAO
ENDIF
*
ADICIONAR_COLUNA_VETOR_AO_BROWSE(VX_Janela,_SELE_SIMPLES)
*
L_Retorno := Selecao(VX_Janela,VX_Sele)
*
LOGA_AJTELAT(C_CdTela,C_Cabec,V_Lst_CdOpcao)  // LOGAR conteúdo de telas
*
RETURN L_Retorno

*
***********************
STATIC FUNCTION Selecao ( VX_Janela, VX_Sele)
*
#INCLUDE "set.ch"
*
LOCAL L_Mais, N_Tecla, N_Pos, L_RolaCima, L_RolaBaixo
LOCAL L_ForcaParada, N_Cont
LOCAL N_Row_Inicial_Util
LOCAL N_mRow, N_mCol, N_Desloca, N_RegiaoMouse, N_Keyboard
LOCAL N_Desloca_Aux, N_RowPos_Ant

LOCAL N_PaintRefresh_Old, X_Retorno_Eval, L_FechouComAutoClose := .F.
LOCAL L_Executar, V_Botao, V_Imagem, N_Pos_Acao
LOCAL B_Ajuda_Ant


LOCAL N_MenID, L_Coords, X_Retorno

#DEFINE C_CdOpcao VX_Sele:CARGO[17]

IF SOB_MODO_GRAFICO()
    NAP_CUALIB_HOTKEY(K_F1,{||XXHELP(C_CdTela,C_Cabec,V_Lst_CdOpcao[N_Selecio,1],V_Lst_CdOpcao)}, .F.)
ELSE
    B_Ajuda_Ant := SETKEY(K_F1,{||XXHELP(C_CdTela,C_Cabec,V_Lst_CdOpcao[N_Selecio,1],V_Lst_CdOpcao)}) // salvar help anterior
ENDIF
#UNDEF  C_CdOpcao

*
* antes de tudo, remontar a tela caso usuário tenha solicitado
*
IF L_ForcaLerTudo
    *
    IF L_PrimAtivacao
        L_PrimAtivacao := .F.
    *
        IF SOB_MODO_GRAFICO()
            L_Coords := CoordenadasBrowse(VX_Sele)

            // Add an extra column to scrollbar
            IF L_ScrollVertical
                L_Coords[4]++
            ENDIF

            N_MenID := NAP_MENU(N_WindowNum, L_Coords[1], L_Coords[2], L_Coords[3], L_Coords[4], L_AutoClose, .F.)
            N_ItemId := N_MenID

            FOR N_Cont := 1 TO LEN(V_Opcoes)
                NAP_MENU_ADD(N_WindowNum, N_MenID, {||V_Opcoes[N_Cont,_OPCAO_TEXTO_TRATADO]}, V_Opcoes[N_Cont,_OPCAO_BLOCO_ACAO], V_Opcoes[N_Cont,_OPCAO_COL_DESTAQUE])
            NEXT

            X_Retorno := NAP_CUALIB_LAUNCH_MODAL({||.T.}, {||.T.})

            IF X_Retorno == NAP_MODAL_ESC
                L_FechouComAutoClose = .F.
            ELSEIF X_Retorno == NAP_MODAL_X_BUTTON
                L_FechouComAutoClose = .F.
            ELSEIF X_Retorno > NAP_MODAL_BUTTON_AUTOCLOSE .AND. X_Retorno <= NAP_MODAL_BUTTON_AUTOCLOSE + NAP_MAX_BUTTONS
                L_FechouComAutoClose = .T.
            ELSEIF X_Retorno > NAP_MODAL_HOTKEY_AUTOCLOSE .AND. X_Retorno <= NAP_MODAL_HOTKEY_AUTOCLOSE + NAP_MAX_BUTTONS
                L_FechouComAutoClose = .T.
            ELSEIF X_Retorno > NAP_MODAL_IMAGE_AUTOCLOSE .AND. X_Retorno <= NAP_MODAL_IMAGE_AUTOCLOSE + NAP_MAX_IMAGES
                L_FechouComAutoClose = .T.
            ELSEIF X_Retorno > NAP_MODAL_MENU_AUTOCLOSE .AND. X_Retorno <= NAP_MODAL_MENU_AUTOCLOSE + LEN(V_Opcoes)
                L_FechouComAutoClose = .T.
            ELSE
                Alert( "Invalid X_Retorno (" + hb_ntos(X_Retorno) + ") in menuvert")
            ENDIF
        ENDIF   // SOB_MODO_GRAFICO()
    ENDIF   // L_PrimAtivacao
    *

    // FRAN: TBrowse management ONLY in text version
    IF .NOT. SOB_MODO_GRAFICO()

        L_AtivaGui := .F.
        VX_Sele:REFRESHALL()     // forçar a remontagem da tela
        DO WHILE .NOT. VX_Sele:STABILIZE()
        ENDDO
        * não existe forma direta de obter esta coluna...
        N_ColunaIniVetor := COL()
        L_AtivaGui := .T.

    ENDIF // .NOT. SOB_MODO_GRAFICO()
   *
ENDIF   // IF L_ForcaLerTudo

//
// FRAN-GTNAP: From here, the code is ONLY for text-terminal version
//
IF .NOT. SOB_MODO_GRAFICO()
*
L_RolaCima := L_RolaBaixo := .F.
*
L_ForcaParada := .F.
L_Mais := .T.                                    // simula um DO UNTIL
*

DO WHILE L_Mais
    *
    * caso o browse esteja estável e as setas montadas, dar uma parada
    * para facilitar o "garbage collection"
    *        InKey_(.T.) é igual ao INKEY(0), mas ativa SET KEY"s
    *        InKey_(.F.) é igual ao INKEY() , mas ativa SET KEY"s
    *
    N_Tecla := Inkey_(L_ForcaParada,4)
    *
    * se houve movimentação vertical, apagar cursor da linha atual
    *
    IF N_Tecla # 0
        IF N_Tecla # K_RIGHT .AND. N_Tecla # K_LEFT .AND. ;
            N_Tecla # K_END .AND. N_Tecla # K_HOME
            VX_Sele:COLORRECT({VX_Sele:ROWPOS,1,;
                            VX_Sele:ROWPOS,VX_Sele:COLCOUNT} ,{2,3})
        ENDIF
    ENDIF
    *
    L_ForcaParada := .F.
    DO CASE
        CASE N_Tecla == 0                   // nenhuma tecla pressionada
            L_AtivaGui := .F.
            IF VX_Sele:STABLE
                MontarSetas(VX_Janela,L_RolaCima,L_RolaBaixo,_SELE_SIMPLES) // estabilizar setas
                L_RolaCima := L_RolaBaixo := .F.
                L_ForcaParada := .T.
            ELSE
                VX_Sele:STABILIZE()         // fazer estabilização incremental
            ENDIF
            L_AtivaGui := .T.

        CASE N_Tecla == K_DOWN .OR. N_Tecla == K_MWBACKWARD
            IF VX_Sele:ROWPOS == VX_Sele:ROWCOUNT
                L_RolaBaixo := .T.
            ENDIF
            VX_Sele:DOWN()

        CASE N_Tecla == K_UP .OR. N_Tecla == K_MWFORWARD
            IF VX_Sele:ROWPOS == 1
                L_RolaCima := .T.
            ENDIF
            VX_Sele:UP()

        CASE N_Tecla == K_PGDN
            L_RolaBaixo := .T.
            VX_Sele:PAGEDOWN()

        CASE N_Tecla == K_PGUP
            L_RolaCima := .T.
            VX_Sele:PAGEUP()

        CASE N_Tecla == K_CTRL_END
            L_RolaBaixo := .T.
            VX_Sele:GOBOTTOM()

        CASE N_Tecla == K_CTRL_HOME
            L_RolaCima := .T.
            VX_Sele:GOTOP()

        CASE N_Tecla == K_LBUTTONDOWN .OR. ;
            N_Tecla == K_LDBLCLK     .OR. ;
            N_Tecla == K_RBUTTONDOWN .OR. ;
            N_Tecla == K_RDBLCLK
            *
            N_mRow := mRow()
            N_mCol := mCol()
            N_Row_Inicial_Util := VX_Sele:nTop
            N_RegiaoMouse := RegiaoJanela_(VX_Janela,N_mRow,N_mCol,;
                                            N_Row_Inicial_Util,VX_Sele:nLeft,;
                                            VX_Sele:nBottom,VX_Sele:nRight,;
                                            @N_Keyboard,@V_Botao,@V_Imagem)
            #INCLUDE "mousecua.ch"
            *
            IF N_RegiaoMouse == AREA_UTIL
                * clicou dentro do browse (inclusive abaixo do cabecalho de coluna)
                N_Desloca := N_mRow - (N_Row_Inicial_Util + VX_Sele:RowPos - 1)
                N_Desloca_Aux := N_Desloca
                N_RowPos_Ant  := VX_Sele:RowPos
                *
                DO WHILE N_Desloca < 0
                    N_Desloca ++
                    VX_Sele:UP()
                ENDDO

                DO WHILE N_Desloca > 0
                    N_Desloca --
                    VX_Sele:DOWN()
                ENDDO
                *
                L_AtivaGui := .F.
                DO WHILE .NOT. VX_Sele:STABILIZE()
                ENDDO
                *
                * Verificar se a pessoa não clicou em uma linha que,
                * apesar de fazer parte da área do browse, está vazia no
                * final do browse.
                *
                * Isto é descoberto se o deslocamento previsto for
                * diferente do deslocamento ocorrido
                *
                IF N_Desloca_Aux > 0 .AND. ;
                    (VX_Sele:RowPos - N_RowPos_Ant) # N_Desloca_Aux
                    *
                    N_Desloca_Aux := N_RowPos_Ant - VX_Sele:RowPos
                    * voltar cursor para a posição original
                    DO WHILE N_Desloca_Aux < 0
                        N_Desloca_Aux ++
                        VX_Sele:UP()
                    ENDDO
                    *
                    DO WHILE .NOT. VX_Sele:STABILIZE()
                    ENDDO
                    *
                    N_RegiaoMouse := BUSCANDO_REGIAO
                ENDIF  // N_Desloca_Aux > 0 .AND. ;

                L_AtivaGui := .T.
                *
                IF N_RegiaoMouse == AREA_UTIL
                    IF N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
                        * Seleciona e tecla ENTER ao mesmo tempo
                        HB_KeyPut(K_ENTER)
                    ELSEIF N_Tecla == K_RBUTTONDOWN .OR. N_Tecla == K_RDBLCLK
                        * Somente tem o efeito de selecionar
		            ENDIF
	            ENDIF  // N_RegiaoMouse == AREA_UTIL

            // N_RegiaoMouse == AREA_UTIL
            ELSEIF (N_RegiaoMouse == BOTAO_IDENTIFICADO .OR. ;      // N_Keyboard preenchido
                    N_RegiaoMouse == BOTAO_NAO_IDENTIFICADO)        // N_Keyboard não preenchido

                * Atualizar completamente a tela antes de executar o bloco de código
                Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
                *
                //!! no futuro, remover
                ASSUMIR_NIL_OU_FALSE({V_Botao[_BOTAO_ALIAS_MUDA],;
                                    V_Botao[_BOTAO_RECNO_MUDA],;
                                    V_Botao[_BOTAO_FILTER_MUDA],;
                                    V_Botao[_BOTAO_ORDER_MUDA],;
                                    V_Botao[_BOTAO_EOFOK],;
                                    V_Botao[_BOTAO_HANDLE_MUDA]})
                                    // V_Botao[_BOTAO_MUDADADOS]}) -->  //  L_MudaDados --> Este parâmetro não está sendo passado para esta função, pois seu valor
                                    //  poderá ser é TRUE, caso o BOTÃO seja indicado pelo parâmetro L_MudaDados.

                X_Retorno_Eval := EVAL(V_Botao[_BOTAO_BLOCO_ACAO])
                *
                * Logar uso de botões, para ter estatística de uso
                IF V_Botao[_BOTAO_CDBOTAO] # NIL  // Se for CUA 2.0
                    LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Botao[_BOTAO_CDBOTAO],;
                                                C_CdTela,"Botão "+V_Botao[_BOTAO_TEXTO_COMANDO])   // Log de uso de botão no sistema
                ENDIF

                IF V_Botao[_BOTAO_AUTOCLOSE]
                    DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                    IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                        ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                    ENDIF
                    IF X_Retorno_Eval
                        L_Mais := .F.
                        L_FechouComAutoClose := .T.
                    ENDIF
                ENDIF // V_Botao[_BOTAO_AUTOCLOSE]

            // N_RegiaoMouse == AREA_UTIL
            ELSEIF N_RegiaoMouse == SOBRE_IMAGEM
                * Atualizar completamente a tela antes de executar o bloco de código
                Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
                *

                //!! no futuro, remover
                ASSUMIR_NIL_OU_FALSE({V_Imagem[_IMAGEM_ALIAS_MUDA],;
                                    V_Imagem[_IMAGEM_RECNO_MUDA],;
                                    V_Imagem[_IMAGEM_FILTER_MUDA],;
                                    V_Imagem[_IMAGEM_ORDER_MUDA],;
                                    V_Imagem[_IMAGEM_EOFOK],;
                                    V_Imagem[_IMAGEM_HANDLE_MUDA]})

                X_Retorno_Eval := EVAL(V_Imagem[_IMAGEM_BLOCO_ACAO])

                * Logar uso de imagens, para ter estatística de uso
                IF V_Imagem[_IMAGEM_CDBOTAO] # NIL  // Se for CUA 2.0
                    LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Imagem[_IMAGEM_CDBOTAO],;
                                                C_CdTela,"Imagem "+V_Imagem[_IMAGEM_ARQUIVO])   // Log de uso de imagem no sistema
                ENDIF
                *
                IF V_Imagem[_IMAGEM_AUTOCLOSE]
                    DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                    IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                        ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                    ENDIF

                    IF X_Retorno_Eval
                        L_Mais := .F.
                        L_FechouComAutoClose := .T.
                    ENDIF
                ENDIF

            // N_RegiaoMouse == AREA_UTIL
            ELSEIF N_Keyboard # NIL
                HB_KeyPut(N_Keyboard)

            ENDIF       // N_RegiaoMouse == AREA_UTIL
            *

        OTHER                          // tecla de não movimentação/marcação
            IF N_Tecla == K_ESC
                L_Mais := .F.
            ELSE
                L_Executar := .F.
                IF N_Tecla == K_ENTER
                    L_Executar := .T.

                ELSE
                    IF L_TemHotKey .AND. TestaTeclaDestaque(VX_Sele,N_Tecla)
                        L_RolaCima := L_RolaBaixo := .T.  // mudará posição selecionada
                        L_Executar := .T.

                    ELSE
                        N_Pos_Acao := ASCAN(V_LstAcoes,{|V_Acao| ;
                                        V_Acao[_ACAO_KEYBOARD]==N_Tecla .OR. ;
                                        V_Acao[_ACAO_KEYBOARD_CASE]==N_Tecla})

                        IF N_Pos_Acao # 0
                            * Atualizar completamente a tela antes de executar o bloco de código
                            Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
                            *
                            IF N_TP_Selecao # _SELE_SIMPLES
                                * coluna da seleção múltipla ficava com cor errada...
                                VX_Sele:COLORRECT({VX_Sele:ROWPOS,1,;
                                                    VX_Sele:ROWPOS,VX_Sele:COLCOUNT} ,{2,3})
                            ENDIF
                            *
                            //!! no futuro, remover
                            ASSUMIR_NIL_OU_FALSE({V_LstAcoes[N_Pos_Acao,_ACAO_ALIAS_MUDA],;
                                                    V_LstAcoes[N_Pos_Acao,_ACAO_RECNO_MUDA],;
                                                    V_LstAcoes[N_Pos_Acao,_ACAO_FILTER_MUDA],;
                                                    V_LstAcoes[N_Pos_Acao,_ACAO_ORDER_MUDA],;
                                                    V_LstAcoes[N_Pos_Acao,_ACAO_EOFOK],;
                                                    V_LstAcoes[N_Pos_Acao,_ACAO_HANDLE_MUDA]})

                            X_Retorno_Eval := EVAL(V_LstAcoes[N_Pos_Acao,_ACAO_BLOCO_ACAO])

                            * Logar uso de ações, para ter estatística de uso
                            IF V_LstAcoes[N_Pos_Acao,_ACAO_CDBOTAO] # NIL  // Se for CUA 2.0
                                LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_LstAcoes[N_Pos_Acao,_ACAO_CDBOTAO],;
                                                            C_CdTela,"Ação "+STR(V_LstAcoes[N_Pos_Acao,_ACAO_KEYBOARD],5))   // Log de uso de ações de teclado no sistema
                            ENDIF
                            *
                            IF V_LstAcoes[N_Pos_Acao,_ACAO_AUTOCLOSE]
                                DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                                IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                                    ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                                ENDIF
                                IF X_Retorno_Eval
                                    L_Mais := .F.
                                    L_FechouComAutoClose := .T.
                                ENDIF
                            ENDIF  // V_LstAcoes[N_Pos_Acao,_ACAO_AUTOCLOSE]

                        ENDIF // N_Pos_Acao # 0

                    ENDIF // L_TemHotKey .AND. TestaTeclaDestaque(VX_Sele,N_Tecla)

                ENDIF  // N_Tecla == K_ENTER
                *
                IF L_EXECUTAR
                    * Atualizar completamente a tela antes de executar o bloco de código
                    Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
                    *
                    //!! no futuro, remover
                    ASSUMIR_NIL_OU_FALSE({V_Opcoes[N_Selecio,_OPCAO_ALIAS_MUDA],;
                                            V_Opcoes[N_Selecio,_OPCAO_RECNO_MUDA],;
                                            V_Opcoes[N_Selecio,_OPCAO_FILTER_MUDA],;
                                            V_Opcoes[N_Selecio,_OPCAO_ORDER_MUDA],;
                                            V_Opcoes[N_Selecio,_OPCAO_EOFOK],;
                                            V_Opcoes[N_Selecio,_OPCAO_HANDLE_MUDA]})
                                            // V_Opcoes[N_Selecio,_OPCAO_MUDADADOS]}) --> Este parâmetro não está sendo passado para esta função, pois seu valor
                                            //                                            poderá ser é TRUE, caso a OPÇÃO seja indicado pelo parâmetro L_MudaDados.

                    X_Retorno_Eval := EVAL(V_Opcoes[N_Selecio,_OPCAO_BLOCO_ACAO])

                    * Logar uso de opções, para ter estatística de uso
                    IF V_Opcoes[N_Selecio,_OPCAO_CDOPCAO] # NIL  // Se for CUA 2.0
                        LOGAINFO_ID_TELA_RELAT_BOTAO("opção",V_Opcoes[N_Selecio,_OPCAO_CDOPCAO],;
                                                    C_CdTela,"Opção "+V_Opcoes[N_Selecio,_OPCAO_TEXTO])   // Log de uso de opção no sistema
                    ENDIF
                    *
                    IF L_AutoClose
                        DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu

                        IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                            ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                        ENDIF

                        IF X_Retorno_Eval
                            L_Mais := .F.
                            L_FechouComAutoClose := .T.
                        ENDIF

                    ENDIF  // L_AutoClose

                ENDIF // L_EXECUTAR
            *
            ENDIF // N_Tecla == K_ESC

    ENDCASE
*
ENDDO

ENDIF // IF .NOT. SOB_MODO_GRAFICO()

IF .NOT. SOB_MODO_GRAFICO()
    #DEFINE C_CdOpcao VX_Sele:CARGO[17]
    SETKEY(K_F1,B_Ajuda_Ant)   // restaurar help anterior
    #UNDEF  C_CdOpcao
ENDIF

*
RETURN L_FechouComAutoClose
*

**************************
PROC Atualizar_Tela_Browse (VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
**************************
L_AtivaGui := .F.
DO WHILE .NOT. VX_Sele:STABILIZE()            // do corpo da seleção
ENDDO
MontarSetas(VX_Janela,L_RolaCima,L_RolaBaixo,_SELE_SIMPLES)   // dos indicativos de rolamento
L_AtivaGui := .T.
*



*************************
PROC SETA_SKIPBLOCK_VETOR(VX_Janela)
*************************
LOCAL VX_Sele := VX_SubObj
*
VX_Sele:SKIPBLOCK := { | N_Salto | ;
                       N_Salto := SkipVetor(N_Salto,N_Selecio,LEN(V_Opcoes)),;
                       N_Selecio := N_Selecio + N_Salto ,;
                       N_Salto     }
VX_Sele:GOTOPBLOCK    := { || N_Selecio := 1  }
VX_Sele:GOBOTTOMBLOCK := { || N_Selecio := LEN(V_Opcoes) }
*
B_LinCorrente :=  {||N_Selecio}        // bloco que retorna a linha corrente
*
***********************
FUNCTION SkipVetor ( N_Salto , N_Selecio_Aux , N_TamVetor )
*
DO CASE
   CASE N_Selecio_Aux+N_Salto < 1
        RETURN -N_Selecio_Aux+1
   CASE N_Selecio_Aux+N_Salto > N_TamVetor
        RETURN N_TamVetor-N_Selecio_Aux
ENDCASE
RETURN N_Salto
*
******************************
FUNC GetCdOpcao_Atual_Menuvert(VX_Janela)
******************************
#DEFINE VX_Sele   VX_SubObj
RETURN V_Lst_CdOpcao[N_Selecio,1]
#UNDEF  VX_Sele
*

//!! DEPOIS REMOVER
**************************
PROC  ASSUMIR_NIL_OU_FALSE(V_LST_VALORES)
**************************
LOCAL N_CT
FOR N_CT := 1 TO LEN(V_LST_VALORES)
    IF V_LST_VALORES[N_CT] # NIL
       IF V_LST_VALORES[N_CT]
          ? MEMVAR->CONTEUDO_TEM_DE_SER_NIL_OU_FALSE
       ENDIF
    ENDIF
NEXT
*
************* FIM DO menuvert.prg
