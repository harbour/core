/* encoding: cp850 */

#pragma DebugInfo=On

/*
* objeto SELECAO EM VETOR
*
*/

#INCLUDE "inkey.ch"
#INCLUDE "common.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "janela.ch"       // métodos externos da classe JANELA
// *
// ********************
// FUNCTION EspSelVet20 ( VX_Janela, N_TP_Selecao, L_SemGrade, ;
//                        L_NaoRolaVertical, L_NaoRolaHorizontal, L_SemToolBar,;
//                        L_AutoClose, VX_Vetor)
// ********************
// *
// LOCAL VX_Sele, N_Cont, N_Congela := 0
// *
// IF L_CUA_10
//    ? MEMVAR->JANELA_SELECAO_VETOR_DA_CUA_20
// ENDIF
// *
// DEFAULT L_SemGrade          TO .F.
// DEFAULT L_NaoRolaVertical   TO .F.
// DEFAULT L_NaoRolaHorizontal TO .F.
// DEFAULT L_SemToolBar        TO .F.
// DEFAULT L_AutoClose         TO .F.
// *
// IF .NOT. L_SemToolBar
//    SETA_PARA_TER_TOOLBAR(VX_Janela) // ajusta N_LinIni e N_LinLivre
// ENDIF
// *
// IF L_AutoClose
//    IF ASCAN(V_RegiaoBotoes,{|V_Botao|"ENTER=" $ UPPER(V_Botao[_BOTAO_TEXTO_COMANDO])}) # 0
//       ? MEMVAR->JANELA_JA_TEM_BOTAO_ENTER_AUTOMATICO
//    ENDIF
//    IF ASCAN(V_LstAcoes,{|V_Acao|K_ENTER==V_Acao[_ACAO_KEYBOARD]}) # 0
//       ? MEMVAR->JANELA_JA_TEM_ACAO_ENTER_AUTOMATICO
//    ENDIF
// ENDIF
// *
// IF N_TP_Selecao # _SELE_SIMPLES
//    IF ASCAN(V_RegiaoBotoes,;
//        {|V_Botao|"BARRA DE ESPAÇO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) .OR. ;
//                  "BARRA DE ESPACO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) .OR. ;
//                  "ESPAÇO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) .OR. ;
//                  "ESPACO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) }) # 0
//       ? MEMVAR->JANELA_JA_TEM_BOTAO_BARRA_DE_ESPACO_AUTOMATICO
//    ENDIF
//    IF ASCAN(V_LstAcoes,{|V_Acao|K_SPACE==V_Acao[_ACAO_KEYBOARD]}) # 0
//       ? MEMVAR->JANELA_JA_TEM_ACAO_BARRA_DE_ESPACO_AUTOMATICO
//    ENDIF
//    ADDBOTAO(VX_Janela,"Barra de espaço=marcar",{||__Keyboard(CHR(32))},.F.,;
//             "B17861",.F.,.F.,.F.,.F.,.F.,.F.,.F.,;
//             .T.)
// ENDIF
// *
// IF L_AutoClose
//    ADDBOTAO(VX_Janela,"Enter=selecionar",{||.T.},.T.,;
//             "B17862",.F.,.F.,.F.,.F.,.F.,.F.,.F.,;
//             .T.)
// ENDIF
// *
// AJUSTA_BOTOES(VX_Janela)  // ajusta Lin2Livre à quantidade de botões de função
// *
// IF .NOT. L_NaoRolaVertical
//    * prever espaço para scroll bar vertical
//    Col2Livre(VX_Janela)--
//    Col2Livre(VX_Janela)--
//    L_ScrollVertical := .T.
// ENDIF
// *
// IF .NOT. L_NaoRolaHorizontal
//    ? MEMVAR->NAO_IMPLEMENTADO    // nao implementado ainda - falar com Marcos
//    * prever espaço para scroll bar horizontal
//    Lin2Livre(VX_Janela)--
//    Lin2Livre(VX_Janela)--
//    L_ScrollHorizontal := .T.
// ENDIF
// *
// IF Lin1Livre(VX_Janela) > Lin2Livre(VX_Janela)
//    * Se ser erro aqui, é porque uma das duas coisas ocorreram:
//    * - Se o erro ocorrer sempre, provavelmente a janela foi definida
//    *   (pelo programador), com altura insuficiente. Ou subir o início da janela,
//    *   ou baixar o final da janela.
//    * - Se o erro ocorrer somente na exibição de alguns dados (mas em outros não),
//    *   é porque o cabeçalho está contendo o conteúdo de um campo do DBF que,
//    *   por acaso, contém muitos ";", fazendo com que o cabeçalho tome toda a tela.
//    *   Neste caso, remover os ";" do campo da tabela, ao transpor para o título.
//    *   Exemplo:
//    *      De..: C_SUBTITILO := "Descrição;" + TRIM(TABELA->CAMPO)
//    *      Para: C_SUBTITILO := "Descrição;" + STRTRAN(TRIM(TABELA->CAMPO),";",",")
//    ? MEMVAR->JANELA_SEM_ESPACO_LIVRE_PARA_EXIBICAO_LINHAS
// ENDIF
// *
// * No Harbour, foi preciso criar uma subclasse da Tbrowse()
// * para poder se ter a lista de posições dos separadores de coluna,
// * retornadas pelo método aColumnsSep() (criado dentro da própria CUA).
// *
// * NOTA: A seleção em vetores aceita vetores de somente uma coluna.
// *       Mas pode existir separador de colunas, no caso de ser seleção múltipla ou estendida...
// VX_Sele := ;
//    TBROWSESubClass():New(Lin1Livre(VX_Janela) , Col1Livre(VX_Janela),;
//                          Lin2Livre(VX_Janela) , Col2Livre(VX_Janela))
// *
// * a tabela de cores deverá ter 3 cores :
// *   Cor padrão com cor de frente intensificada - que teoricamente
// *       seria utilizada em todo o Browse, mas que de fato influencia
// *       apenas os cabeçalhos, conforme método AnexeCol ().
// *   Cor padrão, cor cursor - herda as mesmas cores do objeto JANELA
// *
// IF .NOT. L_SemGrade .AND. SOB_MODO_GRAFICO()
//    * fixar em preto e branco, com cursor em reverso azul
//    VX_Sele:COLORSPEC := "N+/W*,N/W*,N/BG*"
// ELSE
//    VX_Sele:COLORSPEC := CorJanInten(VX_Janela) + "," + CorJanela(VX_Janela)
// ENDIF
// VX_Sele:AUTOLITE  := .F.          // cursor montado via método COLORRECT()
// *
// IF N_TP_Selecao # _SELE_SIMPLES
//    N_Congela := N_Congela + 1
// ENDIF
// *
// #DEFINE B_2LinCorrente  NIL   // será atribuído mais abaixo
// *
// #DEFINE L_PriFora      .F.    // indica se primeira linha não está na janela
// #DEFINE L_UltFora      .F.    // indica se última linha não está na janela
// #DEFINE L_ForcaLerTudo .T.    // forçar um refreshall() + estabilização das setas
// #DEFINE L_PrimAtivacao .T.    // indica que é primeira ativação da janela
// #DEFINE L_AtivaGui       .F.  // se a tecla de atalho deve ser sublinhada
// #DEFINE VN_Selecio      {}    // seleção multipla ou extendida
// #DEFINE L_MostraGrade  .NOT. L_SemGrade   // se mostra o grid
// #DEFINE N_AlturaCabec  0      // indica a altura do cabecalho de colunas
// #DEFINE N_Selecio      1      // contém a linha corrente do cursor
// #DEFINE N_ColunaIniVetor NIL  // posisão inicial da coluna do vetor
// #DEFINE V_Opcoes        {}    // Lista de opções
// #DEFINE L_TemHotKey     .F.   // se alguma opção tem o caractere "#"
// #DEFINE V_Lst_CdOpcao   NIL   // Manter compatibilidade do vetor CARGO entre os programas: SELECAOA, SELECAOV e MENUVERT
// #DEFINE L_TeveRolaHorizontal .F.  // Nunca existe rolamento horizontal em vetores (conterá sempre .F.)
// VX_Sele:CARGO := { B_2LinCorrente , L_PriFora , L_UltFora , ;
//                    L_ForcaLerTudo , L_PrimAtivacao , L_AtivaGui,;
//                    L_AutoClose, VN_Selecio, L_MostraGrade,;
//                    N_Congela , N_TP_Selecao, N_AlturaCabec,;
//                    N_Selecio , N_ColunaIniVetor,;
// 		   V_Opcoes, L_TemHotKey, V_Lst_CdOpcao, L_TeveRolaHorizontal }
// #UNDEF B_2LinCorrente
// #UNDEF L_PriFora
// #UNDEF L_UltFora
// #UNDEF L_ForcaLerTudo
// #UNDEF L_PrimAtivacao
// #UNDEF L_AtivaGui
// #UNDEF VN_Selecio
// #UNDEF L_MostraGrade
// #UNDEF N_AlturaCabec
// #UNDEF N_Selecio
// #UNDEF N_ColunaIniVetor
// #UNDEF V_Opcoes
// #UNDEF L_TemHotKey
// #UNDEF V_Lst_CdOpcao
// #UNDEF L_TeveRolaHorizontal
// *
// N_TP_Jan  := _JAN_SELE_VETO_20    // especializando
// VX_SubObj := VX_Sele              // a janela
// B_Metodo  := {||Selecionar(VX_Janela)}
// *
// * TEM de ser via DEFINE, pois existe bloco de código
// * citando uma posição específica da variável CARGO
// * (VN_Selecio é "detached local")
// #DEFINE B_LinCorrente vX_Sele:CARGO[01]
// #DEFINE VN_Selecio    VX_Sele:CARGO[08]
// IF N_TP_Selecao # _SELE_SIMPLES
//    AnexeCol(VX_Janela, NIL,;
//               { || IIF(ASCAN(VN_Selecio,EVAL(B_LinCorrente))==0," ","»")})
// ENDIF
// #UNDEF VN_Selecio
// #UNDEF B_LinCorrente
// *
// SETA_SKIPBLOCK_VETOR(VX_Janela)
// *
// * armazena informações e posições das hot-keys
// IF VALTYPE(VX_Vetor[1]) # "C"  // Na CUA 2.0, os vetores tem de conter caracteres
//    ? MEMVAR->CONTEUDO_DO_VETOR_DEVE_SER_CARACTERE
// ENDIF
// *
// FOR N_Cont := 1 TO LEN(VX_Vetor)
//     AddOpcaoInterna(VX_Janela,VX_Vetor[N_Cont],,,3,.f.,.f.,.f.,.f.,.f.,.f.,.f.)
// NEXT
// *
// RETURN NIL
// *
// * DEFINICOES PARA USO GERAL
// *
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
#DEFINE V_Lst_CdOpcao        VX_Sele:CARGO[17]      // Manter compatibilidade do vetor CARGO entre os programas: SELECAOA, SELECAOV e MENUVERT
#DEFINE L_TeveRolaHorizontal VX_Sele:CARGO[18]      // Nunca existe rolamento horizontal em vetores (conterá sempre .F.)
// *
// **************************
// STATIC FUNCTION Selecionar ( VX_Janela )
// *
// LOCAL VX_Sele := VX_SubObj
// LOCAL X_Retorno
// *
// ADICIONAR_COLUNA_VETOR_AO_BROWSE(VX_Janela,N_TP_Selecao)
// *
// X_Retorno := Selecao(VX_Janela,VX_Sele)
// *
// LOGA_AJTELAT(C_CdTela,C_Cabec,NIL)  // LOGAR conteúdo de telas
// *
// RETURN X_Retorno
// *
// ***********************
// STATIC FUNCTION Selecao ( VX_Janela, VX_Sele)
// *
// #INCLUDE "set.ch"
// *
// LOCAL L_Mais, N_Tecla, N_Pos, L_RolaCima, L_RolaBaixo
// LOCAL L_ForcaParada, N_Cont, L_Abortado
// LOCAL N_Row_Inicial_Util
// LOCAL N_mRow, N_mCol, N_Desloca, N_RegiaoMouse, N_Keyboard
// LOCAL N_Desloca_Aux, N_RowPos_Ant
// LOCAL X_Retorno
// LOCAL N_PaintRefresh_Old, X_Retorno_Eval
// LOCAL L_Executar, V_Botao, V_Imagem, N_Pos_Acao, N_TeclaUpper, L_PodeExecutar
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       N_PaintRefresh_Old := WVW_SetPaintRefresh(_REPAINT_DEFAULT)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// *
// *
// L_Abortado := .F.         // se .T. se teclado ESC
// *
// * antes de tudo, remontar a tela caso usuário tenha solicitado
// *
// IF L_ForcaLerTudo
//    *
//    IF L_PrimAtivacao
//       L_PrimAtivacao := .F.
//       *
//       * No harbour, a atribuição da variável FREEZE reexecuta os codes blocks,
//       * o que causava erro quando um registro era deletado. Corrigir este
//       * problema usando a variável L_PrimAtivacao, que faz com que o FREEZE
//       * somente seja setado uma vez por janela (na primeira ativação).
//       *
//       VX_Sele:FREEZE := N_Congela
//       *
//       #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//          IF SOB_MODO_GRAFICO()
//             IF L_MostraGrade
//                * montar grid
//                AddGuiObject(VX_Janela,DesenhaGridH(VX_Janela,VX_Sele),;
//                             CoordenadasBrowse(VX_Sele))
//                AddGuiObject(VX_Janela,DesenhaGridV(VX_Janela,VX_Sele),;
//                             CoordenadasBrowse(VX_Sele))
//                AddGuiObject(VX_Janela,DesenhaBoxExterno(VX_Janela,VX_Sele),;
//                             CoordenadasBrowse(VX_Sele))
//             ELSEIF N_TP_Selecao == _SELE_SIMPLES
//                * dar destaque ao item correntemente selecionado
//                AddGuiObject(VX_Janela,;
//                             DesenhaBoxItemSelecionado(VX_Janela,VX_Sele),;
//                             CoordenadasBrowse(VX_Sele))
//             ENDIF
//             *
//             * colocar sublinhado sob as teclas de atalho
//             FOR N_Cont := 1 TO LEN(V_Opcoes)
//                 IF V_Opcoes[N_Cont,_OPCAO_COL_DESTAQUE] # 0  // tem tecla hotkey
//                    AddGuiObject(VX_Janela,DesenhaAtalho(VX_Janela,VX_Sele,N_Cont),;
//                                 CoordenadasBrowse(VX_Sele))
//                 ENDIF
//             NEXT
//          ENDIF
//       #elif defined(__PLATFORM__LINUX)
//          // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//       #else
//          #erro "Código não adaptado para esta plataforma"
//       #endif
//       *
//    ENDIF
//    *
//    IF N_TP_Selecao # _SELE_SIMPLES .AND. VX_Sele:COLPOS == 1   &&* TALVEZ SAIA
//       VX_Sele:COLPOS := 2          // cursor não acessa indicativo de seleção
//    ENDIF
//    *
//    L_AtivaGui := .F.
//    VX_Sele:REFRESHALL()     // forçar a remontagem da tela
//    DO WHILE .NOT. VX_Sele:STABILIZE()
//    ENDDO
//    * não existe forma direta de obter esta coluna...
//    N_ColunaIniVetor := COL()
//    L_AtivaGui := .T.
//    *
// ENDIF
// *
// L_RolaCima := L_RolaBaixo := .F.
// *
// L_ForcaParada := .F.
// L_Mais := .T.                                    // simula um DO UNTIL
// *
// DO WHILE L_Mais
//    *
//    IF N_TP_Selecao # _SELE_SIMPLES .AND. VX_Sele:COLPOS == 1       &&* TALVEZ SAIA
//       VX_Sele:COLPOS := 2          // cursor não acessa indicativo de seleção
//    ENDIF
//    *
//    * caso o browse esteja estável e as setas montadas, dar uma parada
//    * para facilitar o "garbage collection"
//    *        InKey_(.T.) é igual ao INKEY(0), mas ativa SET KEY"s
//    *        InKey_(.F.) é igual ao INKEY() , mas ativa SET KEY"s
//    *
//    N_Tecla := Inkey_(L_ForcaParada,4)
//    *
//    * se houve movimentação vertical, apagar cursor da linha atual
//    *
//    IF N_Tecla # 0
//       IF N_Tecla # K_RIGHT .AND. N_Tecla # K_LEFT .AND. ;
//          N_Tecla # K_END   .AND. N_Tecla # K_HOME
//          VX_Sele:COLORRECT({VX_Sele:ROWPOS,IIF(N_TP_Selecao==_SELE_SIMPLES,1,2),;
//                             VX_Sele:ROWPOS,VX_Sele:COLCOUNT} ,{2,3})
//       ENDIF
//    ENDIF
//    *
//    L_ForcaParada := .F.
//    DO CASE
//       CASE N_Tecla == 0              // nenhuma tecla pressionada
//            L_AtivaGui := .F.
//            IF VX_Sele:STABLE
//               MontarSetas(VX_Janela,L_RolaCima,L_RolaBaixo,N_TP_Selecao) // estabilizar setas
//               L_RolaCima := L_RolaBaixo := .F.
//               L_ForcaParada := .T.
//            ELSE
//               VX_Sele:STABILIZE()            // fazer estabilização incremental
//            ENDIF
//            L_AtivaGui := .T.
//       CASE N_Tecla == K_DOWN .OR. N_Tecla == K_MWBACKWARD
//            IF VX_Sele:ROWPOS == VX_Sele:ROWCOUNT
//               L_RolaBaixo := .T.
//            ENDIF
//            VX_Sele:DOWN()
//       CASE N_Tecla == K_UP   .OR. N_Tecla == K_MWFORWARD
//            IF VX_Sele:ROWPOS == 1
//               L_RolaCima := .T.
//            ENDIF
//            VX_Sele:UP()
//       CASE N_Tecla == K_PGDN
//            L_RolaBaixo := .T.
//            VX_Sele:PAGEDOWN()
//       CASE N_Tecla == K_PGUP
//            L_RolaCima := .T.
//            VX_Sele:PAGEUP()
//       CASE N_Tecla == K_CTRL_END
//            L_RolaBaixo := .T.
//            VX_Sele:GOBOTTOM()
//       CASE N_Tecla == K_CTRL_HOME
//            L_RolaCima := .T.
//            VX_Sele:GOTOP()
//       CASE N_TP_Selecao # _SELE_SIMPLES .AND. N_Tecla == 32     // barra de espaço
//            N_Pos := ASCAN(VN_Selecio,EVAL(B_LinCorrente))
//            IF N_Pos == 0
//               * não marcado, incluir na lista e aumentar o tamanho do vetor
//               AADD(VN_Selecio,EVAL(B_LinCorrente))
//            ELSE
//               * marcado, excluir da lista e reduzir o tamanho do vetor
//               ADEL(VN_Selecio,N_Pos)
//               ASIZE(VN_Selecio,LEN(VN_Selecio)-1)
//            ENDIF
//            VX_Sele:REFRESHCURRENT()
//       CASE N_Tecla == K_LBUTTONDOWN .OR. ;
//            N_Tecla == K_LDBLCLK     .OR. ;
//            N_Tecla == K_RBUTTONDOWN .OR. ;
//            N_Tecla == K_RDBLCLK
//            *
//            N_mRow := mRow()
//            N_mCol := mCol()
//            N_Row_Inicial_Util := VX_Sele:nTop + N_AlturaCabec
//            N_RegiaoMouse := RegiaoJanela_(VX_Janela,N_mRow,N_mCol,;
//                                           N_Row_Inicial_Util,VX_Sele:nLeft,;
//                                           VX_Sele:nBottom,VX_Sele:nRight,;
//                                           @N_Keyboard,@V_Botao,@V_Imagem)
//            #INCLUDE "mousecua.ch"
//            *
//            IF N_RegiaoMouse == AREA_UTIL
//               * clicou dentro do browse (inclusive abaixo do cabecalho de coluna)
//               N_Desloca := N_mRow - (N_Row_Inicial_Util + VX_Sele:RowPos - 1)
//               N_Desloca_Aux := N_Desloca
//               N_RowPos_Ant  := VX_Sele:RowPos
//               *
//               DO WHILE N_Desloca < 0
//                  N_Desloca ++
//                  VX_Sele:UP()
//               ENDDO
//               DO WHILE N_Desloca > 0
//                  N_Desloca --
//                  VX_Sele:DOWN()
//               ENDDO
//               *
//               L_AtivaGui := .F.
//               DO WHILE .NOT. VX_Sele:STABILIZE()
//               ENDDO
//               *
//               * Verificar se a pessoa não clicou em uma linha que,
//               * apesar de fazer parte da área do browse, está vazia no
//               * final do browse.
//               *
//               * Isto é descoberto se o deslocamento previsto for
//               * diferente do deslocamento ocorrido
//               IF N_Desloca_Aux > 0 .AND. ;
//                  (VX_Sele:RowPos - N_RowPos_Ant) # N_Desloca_Aux
//                  *
//                  N_Desloca_Aux := N_RowPos_Ant - VX_Sele:RowPos
//                  * voltar cursor para a posição original
//                  DO WHILE N_Desloca_Aux < 0
//                     N_Desloca_Aux ++
//                     VX_Sele:UP()
//                  ENDDO
//                  *
//                  DO WHILE .NOT. VX_Sele:STABILIZE()
//                  ENDDO
//                  *
//                  N_RegiaoMouse := BUSCANDO_REGIAO
//               ENDIF
//               L_AtivaGui := .T.
//               *
//               IF N_RegiaoMouse == AREA_UTIL
//                  IF N_TP_Selecao == _SELE_SIMPLES
//                     IF N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
//                        * Seleciona e tecla ENTER ao mesmo tempo
//                        HB_KeyPut(K_ENTER)
//                     ELSEIF N_Tecla == K_RBUTTONDOWN .OR. N_Tecla == K_RDBLCLK
//                        * Somente tem o efeito de selecionar
//                     ENDIF
//                  ELSE   //  marcar linhas clicadas
//                     *
//                     * é necessário dar um refresh de imediato, para que
//                     * o SkipBlock() seja executado e o número do registro ou
//                     * posição do vetor seja armazenado corretamente.
//                     *
//                     N_Pos := ASCAN(VN_Selecio,EVAL(B_LinCorrente))
//                     IF N_Pos == 0
//                        * não marcado, incluir na lista e aumentar o tamanho do vetor
//                        AADD(VN_Selecio,EVAL(B_LinCorrente))
//                     ELSE
//                        * marcado, excluir da lista e reduzir o tamanho do vetor
//                        ADEL(VN_Selecio,N_Pos)
//                        ASIZE(VN_Selecio,LEN(VN_Selecio)-1)
//                     ENDIF
//                     VX_Sele:REFRESHCURRENT()
//                  ENDIF
//               ENDIF
//            ELSEIF (N_RegiaoMouse == BOTAO_IDENTIFICADO .OR. ;  // N_Keyboard preenchido
//                    N_RegiaoMouse == BOTAO_NAO_IDENTIFICADO)    // N_Keyboard não preenchido
//               IF SOB_MODO_GRAFICO()
//                  ? MEMVAR->MODO_GRAFICO_NAO_USA_ESTE_TRECHO_DE_CODIGO
//               ENDIF
//               *
//               * Atualizar completamente a tela antes de executar o bloco de código
//               Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
//               *

//               //!! no futuro, remover
//               ASSUMIR_NIL_OU_FALSE({V_Botao[_BOTAO_ALIAS_MUDA],;
//                                     V_Botao[_BOTAO_RECNO_MUDA],;
//                                     V_Botao[_BOTAO_FILTER_MUDA],;
//                                     V_Botao[_BOTAO_ORDER_MUDA],;
//                                     V_Botao[_BOTAO_EOFOK],;
//                                     V_Botao[_BOTAO_HANDLE_MUDA]})
//                                     // V_Botao[_BOTAO_MUDADADOS]}) --> Este parâmetro não está sendo passado para esta função, pois seu valor
//                                     //                                 poderá ser é TRUE, caso o BOTÃO seja indicado pelo parâmetro L_MudaDados.

//               X_Retorno_Eval := EVAL(V_Botao[_BOTAO_BLOCO_ACAO])
//               *
//               * Logar uso de botões, para ter estatística de uso
//               IF V_Botao[_BOTAO_CDBOTAO] # NIL  // Se for CUA 2.0
//                  LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Botao[_BOTAO_CDBOTAO],;
//                                               C_CdTela,"Botão "+V_Botao[_BOTAO_TEXTO_COMANDO])   // Log de uso de botão no sistema
//               ENDIF

//               IF V_Botao[_BOTAO_AUTOCLOSE]
//                  DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
//                  IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
//                     ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
//                  ENDIF
//                  IF X_Retorno_Eval
//                     L_Mais := .F.
//                  ENDIF
//               ENDIF
//            ELSEIF N_RegiaoMouse == SOBRE_IMAGEM
//               * Atualizar completamente a tela antes de executar o bloco de código
//               Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
//               *
//               #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//                  IF SOB_MODO_GRAFICO()
//                     WVW_SetPaintRefresh(N_PaintRefresh_Old)
//                  ENDIF
//               #elif defined(__PLATFORM__LINUX)
//                  // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//               #else
//                  #erro "Código não adaptado para esta plataforma"
//               #endif
//               *

//               //!! no futuro, remover
//               ASSUMIR_NIL_OU_FALSE({V_Imagem[_IMAGEM_ALIAS_MUDA],;
//                                     V_Imagem[_IMAGEM_RECNO_MUDA],;
//                                     V_Imagem[_IMAGEM_FILTER_MUDA],;
//                                     V_Imagem[_IMAGEM_ORDER_MUDA],;
//                                     V_Imagem[_IMAGEM_EOFOK],;
//                                     V_Imagem[_IMAGEM_HANDLE_MUDA]})

//               X_Retorno_Eval := EVAL(V_Imagem[_IMAGEM_BLOCO_ACAO])

//               * Logar uso de imagens, para ter estatística de uso
//               IF V_Imagem[_IMAGEM_CDBOTAO] # NIL  // Se for CUA 2.0
//                  LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Imagem[_IMAGEM_CDBOTAO],;
//                                               C_CdTela,"Imagem "+V_Imagem[_IMAGEM_ARQUIVO])   // Log de uso de imagem no sistema
//               ENDIF

//               #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//                  IF SOB_MODO_GRAFICO()
//                     WVW_SetPaintRefresh(_REPAINT_DEFAULT)
//                  ENDIF
//               #elif defined(__PLATFORM__LINUX)
//                  // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//               #else
//                  #erro "Código não adaptado para esta plataforma"
//               #endif
//               *
//               IF V_Imagem[_IMAGEM_AUTOCLOSE]
//                  DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
//                  IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
//                     ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
//                  ENDIF
//                  IF X_Retorno_Eval
//                     L_Mais := .F.
//                  ENDIF
//               ENDIF
//            ELSEIF N_Keyboard # NIL
//               HB_KeyPut(N_Keyboard)
//            ENDIF
//            *
//       OTHER                          // tecla de não movimentação/marcação
//            IF N_Tecla == K_ESC
//               L_Abortado := .T.
//               L_Mais := .F.
//            ELSE
//               IF L_TemHotKey .AND. TestaTeclaDestaque(VX_Sele,N_Tecla)
//                  L_RolaCima := L_RolaBaixo := .T. // mudará posição selecionada
//                  IF L_AutoClose
//                     L_Mais := .F.
//                  ENDIF
//               ELSE
//                  *
//                  N_Pos_Acao     := 0
//                  L_PodeExecutar := .T.
//                  *
//                  N_Pos_Acao := ASCAN(V_LstAcoes,{|V_Acao| ;
//                                      V_Acao[_ACAO_KEYBOARD]==N_Tecla .OR. ;
//                                      V_Acao[_ACAO_KEYBOARD_CASE]==N_Tecla})
//                  *
//                  IF N_Pos_Acao # 0 .AND. L_PodeExecutar
//                     * Atualizar completamente a tela antes de executar o bloco de código
//                     Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
//                     *
//                     IF N_TP_Selecao # _SELE_SIMPLES
//                        * coluna da seleção múltipla ficava com cor errada...
//                        VX_Sele:COLORRECT({VX_Sele:ROWPOS,1,;
//                                           VX_Sele:ROWPOS,VX_Sele:COLCOUNT} ,{2,3})
//                     ENDIF
//                     *
//                     #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//                        IF SOB_MODO_GRAFICO()
//                           WVW_SetPaintRefresh(N_PaintRefresh_Old)
//                        ENDIF
//                     #elif defined(__PLATFORM__LINUX)
//                        // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//                     #else
//                        #erro "Código não adaptado para esta plataforma"
//                     #endif

//                     //!! no futuro, remover
//                     ASSUMIR_NIL_OU_FALSE({V_LstAcoes[N_Pos_Acao,_ACAO_ALIAS_MUDA],;
//                                           V_LstAcoes[N_Pos_Acao,_ACAO_RECNO_MUDA],;
//                                           V_LstAcoes[N_Pos_Acao,_ACAO_FILTER_MUDA],;
//                                           V_LstAcoes[N_Pos_Acao,_ACAO_ORDER_MUDA],;
//                                           V_LstAcoes[N_Pos_Acao,_ACAO_EOFOK],;
//                                           V_LstAcoes[N_Pos_Acao,_ACAO_HANDLE_MUDA]})

//                     X_Retorno_Eval := EVAL(V_LstAcoes[N_Pos_Acao,_ACAO_BLOCO_ACAO])

//                     * Logar uso de ações, para ter estatística de uso
//                     IF V_LstAcoes[N_Pos_Acao,_ACAO_CDBOTAO] # NIL  // Se for CUA 2.0
//                        LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_LstAcoes[N_Pos_Acao,_ACAO_CDBOTAO],;
//                                                     C_CdTela,"Ação "+STR(V_LstAcoes[N_Pos_Acao,_ACAO_KEYBOARD],5))   // Log de uso de ações de teclado no sistema
//                     ENDIF

//                     #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//                        IF SOB_MODO_GRAFICO()
//                           WVW_SetPaintRefresh(_REPAINT_DEFAULT)
//                        ENDIF
//                     #elif defined(__PLATFORM__LINUX)
//                        // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//                     #else
//                        #erro "Código não adaptado para esta plataforma"
//                     #endif
//                     *
//                     IF V_LstAcoes[N_Pos_Acao,_ACAO_AUTOCLOSE]
//                        DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
//                        IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
//                           ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
//                        ENDIF
//                        IF X_Retorno_Eval
//                           L_Mais := .F.
//                        ENDIF
//                     ENDIF
//                  ENDIF
//               ENDIF
//               *
//            ENDIF
//    ENDCASE
//    *
// ENDDO
// *
// IF L_Abortado
//    MudeLista(VX_Janela)            // limpa o vetor VN_Selecio
// ENDIF
// *
// * foi pressionada uma tecla de não movimentação, forçar total estabilização
// *
// L_AtivaGui := .F.
// DO WHILE .NOT. VX_Sele:STABILIZE()            // do corpo da seleção
// ENDDO
// MontarSetas(VX_Janela,L_RolaCima,L_RolaBaixo,N_TP_Selecao)   // dos indicativos de rolamento
// L_AtivaGui := .T.
// *
// * retornar o(s) item(ns) selecionado(s)
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       WVW_SetPaintRefresh(N_PaintRefresh_Old)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// *
// IF N_TP_Selecao == _SELE_SIMPLES       // se selecao simples
//    IF L_Abortado
//       X_Retorno := 0
//    ELSE
//       X_Retorno := EVAL(B_LinCorrente)
//    ENDIF
// ELSE
//    IF N_TP_Selecao == _SELE_EXTENDIDA .AND. LEN(VN_Selecio)==0    // se extendida com seleçao implicita
//       IF L_Abortado
//          X_Retorno := VN_Selecio       // está vazio
//       ELSE
//          X_Retorno := {EVAL(B_LinCorrente)}
//       ENDIF
//    ELSE
//       X_Retorno := VN_Selecio      // se selecao multipla ou extendida com selecao explicita
//    ENDIF
// ENDIF
// RETURN X_Retorno
// *
// **********************
// FUNC ItensSelecionados (VX_Janela)
// **********************
// LOCAL X_Retorno
// LOCAL VX_Sele := VX_SubObj
// IF N_TP_Selecao == _SELE_SIMPLES       // se selecao simples
//    X_Retorno := EVAL(B_LinCorrente)
// ELSE
//    IF N_TP_Selecao == _SELE_EXTENDIDA .AND. LEN(VN_Selecio)==0    // se extendida com seleçao implicita
//       X_Retorno := {EVAL(B_LinCorrente)}
//    ELSE
//       X_Retorno := VN_Selecio      // se selecao multipla ou extendida com selecao explicita
//    ENDIF
// ENDIF
// RETURN X_Retorno
*
********************
PROC AddOpcaoInterna (VX_Janela,C_TxtOpcao,B_AcaoOpcao, C_CdOpcao, N_Retrocede_Callstack,;
                      L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_EofOk, L_HandleMuda, L_MudaDados)
********************
LOCAL C_TxtTratado, C_Destaque, C_TeclaAtalho, N_Pos_Destaque
LOCAL N_Keyboard, N_Keyboard_Case
LOCAL VX_Sele := VX_SubObj
*
DEFAULT B_AcaoOpcao TO {||NIL}
*
IF L_MudaDados # NIL
   INABILITA_ADDOPCAO(L_MudaDados, @B_AcaoOpcao)
ENDIF
*
IF "#" $ C_TxtOpcao  // PENDENTE - Futuramente dar erro se atalho (# / cerquila) for seguido de caractere com ASCII >= 128
   L_TemHotKey := .T.
   *
   C_TxtTratado    := STRTRAN(C_TxtOpcao,"#","")
   N_Pos_Destaque  := AT("#",C_TxtOpcao)
   C_Destaque      := SUBSTR(C_TxtOpcao,N_Pos_Destaque+1,1)
   N_Keyboard      := ASC(C_Destaque)
   *
   * Tornar o N_Keyboard "case insensitive"
   IF C_Destaque >= "a" .AND. C_Destaque <= "z"
      N_Keyboard_Case := ASC(UPPER(C_Destaque))
   ELSEIF C_Destaque >= "A" .AND. C_Destaque <= "Z"
      N_Keyboard_Case := ASC(LOWER(C_Destaque))
   ELSE
      N_Keyboard_Case := N_Keyboard  // não é letra
   ENDIF
   *
   IF ASCAN(V_Opcoes,{|V_Subv|V_Subv[_OPCAO_INKEY_DESTAQUE]     ==N_Keyboard .OR. ;
                              V_Subv[_OPCAO_INKEY_DESTAQUE_CASE]==N_Keyboard}) # 0
      LOGAFONT_GENERICO(N_Retrocede_Callstack,"JAN",NIL,NIL,;
                        "Erro 3: Opções tem hotkey duplicada - "+C_TxtOpcao)
   ENDIF
ELSE
   C_TxtTratado    := C_TxtOpcao
   N_Pos_Destaque  := 0
   C_Destaque      := ""
   N_Keyboard      := 0
   N_Keyboard_Case := 0
ENDIF
*
AADD(V_Opcoes,{C_TxtOpcao,;      // _OPCAO_TEXTO
               C_TxtTratado,;    // _OPCAO_TEXTO_TRATADO
               N_Pos_Destaque,;  // _OPCAO_COL_DESTAQUE
               C_Destaque,;      // _OPCAO_TEXTO_DESTAQUE
               B_AcaoOpcao,;     // _OPCAO_BLOCO_ACAO
               C_CdOpcao,;       // _OPCAO_CDOPCAO
               N_Keyboard,;      // _OPCAO_INKEY_DESTAQUE
               N_Keyboard_Case,; // _OPCAO_INKEY_DESTAQUE_CASE
               L_AliasMuda,;     // _OPCAO_ALIAS_MUDA
               L_RecnoMuda,;     // _OPCAO_RECNO_MUDA
               L_FilterMuda,;    // _OPCAO_FILTER_MUDA
               L_OrderMuda,;     // _OPCAO_ORDER_MUDA
               L_EofOk,;         // _OPCAO_EOFOK
               L_HandleMuda,;    // _OPCAO_HANDLE_MUDA
               L_MudaDados})     // _OPCAO_MUDADADOS


*
*****************
FUNCTION AnexeCol ( VX_Janela , C_Titulo , B_Bloco , N_Largura )
*****************
*
LOCAL VX_Coluna, VX_Sele         // objeto do tipo coluna
*
VX_Sele := VX_SubObj
*
IF C_Titulo # NIL
   DO WHILE LEFT(C_Titulo,1)==";"
      C_Titulo := SUBSTR(C_Titulo,2)
   ENDDO
   *
   IF .NOT. EMPTY(C_TITULO)
      N_AlturaCabec := MAX(N_AlturaCabec,NUMAT(";",C_Titulo)+1)
   ENDIF
ENDIF
*
VX_Coluna := TBCOLUMNNEW(C_Titulo,B_Bloco)
*
IF N_Largura # NIL
   VX_Coluna:WIDTH := N_Largura
ENDIF
VX_Coluna:DEFCOLOR   :=     { 1 , 3 }    // fixa Bright para títulos e células
VX_Coluna:COLORBLOCK := {|| { 2 , 3 } }  // faz com que todas as células (corpo)
*                                        //   fique em cor normal (tira o Bright)
VX_Sele:ADDCOLUMN(VX_Coluna)
*
RETURN NIL
*

*************************************
PROC ADICIONAR_COLUNA_VETOR_AO_BROWSE(VX_Janela,N_2TP_Selecao)
*************************************
LOCAL VX_Sele := VX_SubObj
LOCAL N_MaiorLarg, N_Cont, N_ColunasSobrando
*
* adicionar coluna ao browse
*
N_MaiorLarg := 0
FOR N_Cont := 1 TO LEN(V_Opcoes)
    N_MaiorLarg := MAX(N_MaiorLarg,LEN(V_Opcoes[N_Cont,_OPCAO_TEXTO_TRATADO]))
NEXT
AnexeCol(VX_Janela,NIL, {||" "+V_Opcoes[N_Selecio,_OPCAO_TEXTO_TRATADO]+" "},;
         N_MaiorLarg+2)
*
IF N_2TP_Selecao == _SELE_SIMPLES   // só existe uma coluna no browse()
   * Reduzir a área do browse à área máxima efetivamente necessária,
   * para que, no modo gráfico, se possa imprimir imagens à esquerda
   * e à direita da área do browse (área sem uso).
   * ( só existe uma coluna no browse() )
   N_ColunasSobrando := Col2Livre(VX_Janela)-Col1Livre(VX_Janela)+1 ;
                        -N_MaiorLarg-2
   VX_Sele:nLeft  := VX_Sele:nLeft  + ROUND(N_ColunasSobrando/2,0)
   VX_Sele:nRight := VX_Sele:nLeft  + N_MaiorLarg + 1
ENDIF

*
***************************
FUNCTION TestaTeclaDestaque ( VX_Sele, N_Tecla )
***************************
*
LOCAL N_Cont , L_Mais , L_TeclaValida , N_Cont2 , N_PosBarraAnt
*
N_PosBarraAnt := N_Selecio
*
L_TeclaValida  := .F.
N_Cont := 0
L_Mais := .T.          // simula um DO UNTIL
DO WHILE L_Mais
    N_Cont := N_Cont + 1
    IF N_Tecla==V_Opcoes[N_Cont,_OPCAO_INKEY_DESTAQUE] .OR. ;
       N_Tecla==V_Opcoes[N_Cont,_OPCAO_INKEY_DESTAQUE_CASE]
       L_TeclaValida := .T.
    ENDIF
    IF L_TeclaValida .OR. LEN(V_Opcoes) == N_Cont
       L_Mais := .F.
    ENDIF
ENDDO
*
IF L_TeclaValida
   L_AtivaGui := .F.
   FOR N_Cont2 := 1 TO ABS(N_Cont-N_PosBarraAnt)
       IF N_Cont > N_PosBarraAnt
          VX_Sele:DOWN()
       ELSE
          VX_Sele:UP()
       ENDIF
       DO WHILE .NOT. VX_Sele:STABILIZE()
       ENDDO
   NEXT
   L_AtivaGui := .T.
ENDIF
*
RETURN L_TeclaValida
*
******************************
STATIC PROC INABILITA_ADDOPCAO(L_MudaDados, B_AcaoOpcao)
******************************
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
   IF SELECT("XXPREG") # 0
      IF L_MudaDados .AND. (.NOT. EHPRINCIPAL(.F.))
         B_AcaoOpcao := {|| INABILITA_MENSAGEM()}
      ENDIF
   ENDIF
ENDIF
*
******************************
STATIC PROC INABILITA_MENSAGEM()
******************************
ALARME("M28816","Opção não disponível para base de dados reserva!")
// *
// *****************
// FUNC ItemCorrente (VX_Janela)
// *****************
// LOCAL X_Retorno
// LOCAL VX_Sele := VX_SubObj
// *
// X_Retorno := EVAL(B_LinCorrente)
// *
// RETURN X_Retorno
// *
// ************* FIM
