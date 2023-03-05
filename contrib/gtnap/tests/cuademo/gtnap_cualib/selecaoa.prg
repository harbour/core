/* encoding: cp850 */

#pragma DebugInfo=Off

// /*
// * objeto SELECAO
// *
// */

* Conserto de bug na linguagem
* (EXISTE bug na FUNCAO EOF(), usada dentro de objetos TBROWSE, em modo
* mono-usuário, existindo também várias descompassos na atualizaçäo
* das funçöes EOF(), LASTREC(), etc. Para solucionar parte destes problemas
* adotou a posiçäo mais conservadora possível para testar o EOF().)
*
#TRANSLATE XEOF()=>(EOF() .OR. RECNO()==LASTREC()+1)
*
#INCLUDE "inkey.ch"
#INCLUDE "common.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "janela.ch"       // métodos externos da classe JANELA
#INCLUDE "intercep.ch"  // interceptar comandos de manipulação de dados
*
********************
FUNCTION EspSelArq20 ( VX_Janela, N_TP_Selecao, L_SemGrade, ;
                       L_NaoRolaVertical, L_NaoRolaHorizontal, L_SemToolBar,;
                       L_AutoClose, N_Congela, B_While )
*
LOCAL VX_Sele, N_Cont, B_LinCorrente
*
IF L_CUA_10
   ? MEMVAR->JANELA_SELECAO_ARQUIVO_DA_CUA_20
ENDIF
*
DEFAULT L_SemGrade          TO .F.
DEFAULT L_NaoRolaVertical   TO .F.
DEFAULT L_NaoRolaHorizontal TO .F.
DEFAULT L_SemToolBar        TO .F.
DEFAULT L_AutoClose         TO .F.
DEFAULT N_Congela      TO 0
*
IF .NOT. L_SemToolBar
   SETA_PARA_TER_TOOLBAR(VX_Janela) // ajusta N_LinIni e N_LinLivre
ENDIF
*
IF L_AutoClose
   IF ASCAN(V_RegiaoBotoes,{|V_Botao|"ENTER=" $ UPPER(V_Botao[_BOTAO_TEXTO_COMANDO])}) # 0
      ? MEMVAR->JANELA_JA_TEM_BOTAO_ENTER_AUTOMATICO
   ENDIF
   IF ASCAN(V_LstAcoes,{|V_Acao|K_ENTER==V_Acao[_ACAO_KEYBOARD]}) # 0
      ? MEMVAR->JANELA_JA_TEM_ACAO_ENTER_AUTOMATICO
   ENDIF
ENDIF
*
IF N_TP_Selecao # _SELE_SIMPLES
   IF ASCAN(V_RegiaoBotoes,;
       {|V_Botao|"BARRA DE ESPAÇO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) .OR. ;
                 "BARRA DE ESPACO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) .OR. ;
                 "ESPAÇO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) .OR. ;
                 "ESPACO=" $ XUPPER(V_Botao[_BOTAO_TEXTO_COMANDO]) }) # 0
      ? MEMVAR->JANELA_JA_TEM_BOTAO_BARRA_DE_ESPACO_AUTOMATICO
   ENDIF
   IF ASCAN(V_LstAcoes,{|V_Acao|K_SPACE==V_Acao[_ACAO_KEYBOARD]}) # 0
      ? MEMVAR->JANELA_JA_TEM_ACAO_BARRA_DE_ESPACO_AUTOMATICO
   ENDIF
   ADDBOTAO(VX_Janela,"Barra de espaço=marcar",{||__Keyboard(CHR(32))},.F.,;
            "B17859",.F.,.F.,.F.,.F.,.F.,.F.,.F.,;
            .T.)
ENDIF
*
IF L_AutoClose
   ADDBOTAO(VX_Janela,"Enter=selecionar",{||.T.},.T.,;
            "B17860",.F.,.F.,.F.,.F.,.F.,.F.,.F.,;
            .T.)
ENDIF
*
AJUSTA_BOTOES(VX_Janela)  // ajusta Lin2Livre à quantidade de botões de função
*
IF .NOT. L_NaoRolaVertical
   * prever espaço para scroll bar vertical
   Col2Livre(VX_Janela)--
   Col2Livre(VX_Janela)--
   L_ScrollVertical := .T.
ENDIF
*
IF .NOT. L_NaoRolaHorizontal
   * prever espaço para scroll bar horizontal
   Lin2Livre(VX_Janela)--
   Lin2Livre(VX_Janela)--
   L_ScrollHorizontal := .T.
ENDIF
*
IF Lin1Livre(VX_Janela) > Lin2Livre(VX_Janela)
   * Se ser erro aqui, é porque uma das duas coisas ocorreram:
   * - Se o erro ocorrer sempre, provavelmente a janela foi definida
   *   (pelo programador), com altura insuficiente. Ou subir o início da janela,
   *   ou baixar o final da janela.
   * - Se o erro ocorrer somente na exibição de alguns dados (mas em outros não),
   *   é porque o cabeçalho está contendo o conteúdo de um campo do DBF que,
   *   por acaso, contém muitos ";", fazendo com que o cabeçalho tome toda a tela.
   *   Neste caso, remover os ";" do campo da tabela, ao transpor para o título.
   *   Exemplo:
   *      De..: C_SUBTITILO := "Descrição;" + TRIM(TABELA->CAMPO)
   *      Para: C_SUBTITILO := "Descrição;" + STRTRAN(TRIM(TABELA->CAMPO),";",",")
   ? MEMVAR->JANELA_SEM_ESPACO_LIVRE_PARA_EXIBICAO_LINHAS
ENDIF
*
* No Harbour, foi preciso criar uma subclasse da Tbrowse()
* para poder se ter a lista de posições dos separadores de coluna,
* retornadas pelo método aColumnsSep() (criado dentro da própria CUA).
VX_Sele := ;
   TBROWSESubClass():New(Lin1Livre(VX_Janela) , Col1Livre(VX_Janela) ,;
                         Lin2Livre(VX_Janela) , Col2Livre(VX_Janela)  ;
                         - IIF(N_TP_Selecao == _SELE_SIMPLES,0,1)  )     // necessidade experimental
*
IF B_While == NIL
   * Abaixo é necessário definir a forma do SKIP, mesmo que seja igual à
   * forma interna do objeto TBrowse(), para que seja possível interceptar
   * a movimentação do ponteiro no alias corrente (para fins da LOGAFONT.DBF)
   VX_Sele:SKIPBLOCK     := { |N_Salto| DbSkipper(N_Salto) }
   VX_Sele:GOTOPBLOCK    := { || DbGoTop() }
   VX_Sele:GOBOTTOMBLOCK := { || DbGoBottom() }
ELSE
   VX_Sele:SKIPBLOCK     := { |N_Salto| SkipWhile(N_Salto,B_While) }
   VX_Sele:GOTOPBLOCK    := { || TopWhile(B_While) }
   VX_Sele:GOBOTTOMBLOCK := { || BottomWhile(B_While) }
ENDIF
*
* a tabela de cores deverá ter 3 cores :
*   Cor padrão com cor de frente intensificada - que teoricamente
*       seria utilizada em todo o Browse, mas que de fato influencia
*       apenas os cabeçalhos, conforme método AnexeCol ().
*   Cor padrão, cor cursor - herda as mesmas cores do objeto JANELA
*
* Se mudar aqui mudar na "tbrowsesubclass.prg"
IF .NOT. L_SemGrade .AND. SOB_MODO_GRAFICO()
   * fixar em preto e branco, com cursor em reverso azul
   VX_Sele:COLORSPEC := "N+/W*,N/W*,N/BG*"
ELSE
   VX_Sele:COLORSPEC := CorJanInten(VX_Janela) + "," + CorJanela(VX_Janela)
ENDIF
VX_Sele:AUTOLITE  := .F.          // cursor montado via método COLORRECT()
*
IF N_TP_Selecao # _SELE_SIMPLES
   N_Congela := N_Congela + 1
ENDIF
*
B_LinCorrente :=  {||RECNO()}        // bloco que retorna a linha corrente
*
#DEFINE L_PriFora      .F.    // indica se primeira linha não está na janela
#DEFINE L_UltFora      .F.    // indica se última linha não está na janela
#DEFINE L_ForcaLerTudo .T.    // forçar um refreshall() + estabilização das setas
#DEFINE L_PrimAtivacao .T.    // indica que é primeira ativação da janela
#DEFINE L_AtivaGui       .F.
#DEFINE VN_Selecio      {}    // seleção multipla ou extendida
#DEFINE L_MostraGrade  .NOT. L_SemGrade   // se mostra o grid
#DEFINE N_AlturaCabec  0      // indica a altura do cabecalho de colunas
#DEFINE N_Selecio      NIL    // usado somente no browse de vetores
#DEFINE N_ColunaIniVetor NIL  // usado somente no browse de vetores
#DEFINE V_Opcoes        NIL   // usado somente no browse de vetores
#DEFINE L_TemHotKey     .F.   // usado somente no browse de vetores
#DEFINE V_Lst_CdOpcao   NIL   // Manter compatibilidade do vetor CARGO entre os programas: SELECAOA, SELECAOV e MENUVERT
#DEFINE L_TeveRolaHorizontal .F.  // Para saber se teve rolamento horizontal desde última estabilização
VX_Sele:CARGO := { B_LinCorrente , L_PriFora , L_UltFora , ;
                   L_ForcaLerTudo , L_PrimAtivacao , L_AtivaGui,;
                   L_AutoClose, VN_Selecio, L_MostraGrade,;
                   N_Congela , N_TP_Selecao, N_AlturaCabec,;
                   N_Selecio , N_ColunaIniVetor,;
		   V_Opcoes, L_TemHotKey, V_Lst_CdOpcao, L_TeveRolaHorizontal }
#UNDEF L_PriFora
#UNDEF L_UltFora
#UNDEF L_ForcaLerTudo
#UNDEF L_PrimAtivacao
#UNDEF L_AtivaGui
#UNDEF VN_Selecio
#UNDEF L_MostraGrade
#UNDEF N_AlturaCabec
#UNDEF N_Selecio
#UNDEF N_ColunaIniVetor
#UNDEF V_Opcoes
#UNDEF L_TemHotKey
#UNDEF V_Lst_CdOpcao
#UNDEF L_TeveRolaHorizontal
*
N_TP_Jan  := _JAN_SELE_ARQ_20    // especializando
VX_SubObj := VX_Sele              // a janela
B_Metodo  := {||Selecionar(VX_Janela)}
*
* TEM de ser via DEFINE, pois existe bloco de código
* citando uma posição específica da variável CARGO
* (VN_Selecio é "detached local")
#DEFINE VN_Selecio    VX_Sele:CARGO[08]
IF N_TP_Selecao # _SELE_SIMPLES

    IF SOB_MODO_GRAFICO()
        AnexeCol(VX_Janela, NIL,;
            { || IIF(NAP_CUALIB_IS_SELECTED(B_LinCorrente)==.F.," ","»")})

    ELSE
    AnexeCol(VX_Janela, NIL,;
              { || IIF(ASCAN(VN_Selecio,EVAL(B_LinCorrente))==0," ","»")})
    ENDIF
ENDIF
#UNDEF VN_Selecio
*
RETURN NIL
*
*
**************************************************************************************************************************************************
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
#DEFINE V_Lst_CdOpcao        VX_Sele:CARGO[17]      // Manter compatibilidade do vetor CARGO entre os programas: SELECAOA, SELECAOV e MENUVERT
#DEFINE L_TeveRolaHorizontal VX_Sele:CARGO[18]      // Para saber se teve rolamento horizontal desde última estabilização
// *
// **************************************************************************************************************************************************
// *
***********************
STAT FUNCTION SkipWhile ( N_Salto , B_While )
***********************
*
LOCAL N_Saltado := 0, L_PriRegOK
*
#DEFINE L_RegValido   EVAL(B_While) .AND. .NOT. EOF()
*
IF N_Salto == 0
   REFRESH_RECORD()
ENDIF
*
IF (L_PriRegOK := (L_RegValido))   // validade do reg.corrente
   *
   DO CASE
      CASE N_Salto > 0
           DO WHILE N_Saltado < N_Salto .AND. L_RegValido
              SKIP
              N_Saltado := N_Saltado + 1
           ENDDO
           IF L_PriRegOK .AND. .NOT. ( L_RegValido )
              SKIP -1
              N_Saltado := N_Saltado - 1
           ENDIF
      CASE N_Salto < 0
           DO WHILE N_Saltado > N_Salto .AND. .NOT. BOF() .AND. L_RegValido
              SKIP -1
              IF .NOT. BOF()
                 N_Saltado := N_Saltado - 1
              ENDIF
           ENDDO
           IF L_PriRegOK .AND. .NOT. (L_RegValido)
              SKIP
              N_Saltado := N_Saltado + 1
           ENDIF
   ENDCASE
   *
ELSE
   GOTO_EOF()                 // forçar o EOF()
ENDIF
*
#UNDEF L_RegValido
*
RETURN N_Saltado
*
*
**********************
STAT FUNCTION TopWhile ( B_While )
**********************
*
LOCAL L_PriRegOK
*
#DEFINE L_RegValido   EVAL(B_While) .AND. .NOT. EOF()
*
REFRESH_RECORD()
*
IF (L_PriRegOK := ( L_RegValido ))    // validade do reg.corrente
   *
   DO WHILE .NOT. BOF() .AND. L_RegValido
      SKIP -1                           // retrocede nos registros do arquivo
   ENDDO
   *
   IF L_PriRegOK .AND. .NOT. (L_RegValido)   // se existiu algum registro válido
      SKIP                                   // avançar para o último válido
   ENDIF
   *
ELSE
   GOTO_EOF()                 // forçar o EOF()
ENDIF
RETURN NIL
*
#UNDEF L_RegValido
*
*************************
STAT FUNCTION BottomWhile ( B_While )
*************************
*
LOCAL L_PriRegOK
*
#DEFINE L_RegValido   EVAL(B_While) .AND. .NOT. EOF()
*
REFRESH_RECORD()
*
IF (L_PriRegOK := ( L_RegValido ))    // validade do reg.corrente
   *
   DO WHILE L_RegValido
      SKIP                          // avança nos registros do arquivo
   ENDDO
   *
   IF L_PriRegOK                    // se existiu algum registro válido
      SKIP -1                       // retroceder para o último válido
   ENDIF
ELSE
   GOTO_EOF()                 // forçar o EOF()
ENDIF
*
#UNDEF L_RegValido
*
RETURN NIL
*
**************************
STATIC FUNCTION Selecionar ( VX_Janela )
*
LOCAL VX_Sele := VX_SubObj
LOCAL X_Retorno
*
#DEFINE C_Ajuda     VX_Janela[17]
IF EVAL(SETA_ACEJAN(),C_Ajuda)   // usuário acessa janela
   X_Retorno := Selecao(VX_Janela,VX_Sele)
ELSE
  IF N_TP_Selecao == _SELE_SIMPLES       // se selecao simples
     X_Retorno := 0
  ELSE
     X_Retorno := {}
  ENDIF
ENDIF
#UNDEF C_Ajuda
*
LOGA_AJTELAT(C_CdTela,C_Cabec,NIL)  // LOGAR conteúdo de telas
*
RETURN X_Retorno
*


//
// FRAN: A Multi-select TableView control can change the selection using [CTRL+Click] or [SHIFT+UP/DOWN]
// This funcion syncronizes the TableView with the Janela selection
//
STATIC PROC UpdatedSelected()

    // NAP_TABLEVIEW_CUALIB_REFRESH(SET(_SET_DELETED))


    // //         //NAP_TABLEVIEW_UPDATE(V_TableView)

    // LOCAL V_TableView := NIL
    // LOCAL VN_Selection := NIL

    // LOG_PRINT("UPDATED. Len of VX_Janela: " /*+ hb_ntos(LEN(VX_Janela))*/ )
    // // IF SOB_MODO_GRAFICO()
    // //      V_TableView := NAP_CUALIB_CURRENT_TABLEVIEW()

    // //     IF V_TableView # NIL
    // //         VN_Selection := NAP_TABLEVIEW_SELECTED(V_TableView)
    // //     ENDIF

    // //     LOG_PRINT("TABLEVIEW SELECTION WITH " + hb_ntos(LEN(VN_Selection)))
    // //     LOG_PRINT("JANELA VALUE: " + hb_ntos(VX_Janela))
    // //     IF VN_Selection # NIL
    // //         IF VX_Janela == NIL
    // //             LOG_PRINT("VX_Janela == NIL!!!!!!!!!!!!!")
    // //         ELSE

    // //             #DEFINE VX_Sele  VX_SubObj
    // //             // VN_Selecio := VN_Selection
    // //             LOG_PRINT("VN_Selecio Current size: " + hb_ntos(LEN(VN_Selecio)))
    // //              #UNDEF VX_Sele
    // //             // MudeLista ( VX_Janela , VN_Selection )
    // //         ENDIF
    // //         //NAP_TABLEVIEW_UPDATE(V_TableView)
    // //     ENDIF
    // // ENDIF

    RETURN

***********************
STATIC FUNCTION Selecao ( VX_Janela, VX_Sele)
***********************
*
#INCLUDE "set.ch"
*
LOCAL L_Mais, N_Tecla, N_Pos, L_RolaCima, L_RolaBaixo
LOCAL L_ForcaParada, L_Abortado, N_Cont
LOCAL N_Row_Inicial_Util
LOCAL N_mRow, N_mCol, N_Desloca, N_RegiaoMouse, N_Keyboard
LOCAL N_Desloca_Aux, N_RowPos_Ant
LOCAL X_Retorno
LOCAL N_PaintRefresh_Old, X_Retorno_Eval
LOCAL L_Executar, V_Botao, V_Imagem, N_Pos_Acao, L_PodeExecutar
LOCAL V_Ambiente_Alias
LOCAL V_TableView, L_Coords, N_Count, O_Column, C_Title, N_Width

*
//
// FRAN - This code is not necessary in GTNAP
//
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       N_PaintRefresh_Old := WVW_SetPaintRefresh(_REPAINT_DEFAULT)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
*
L_Abortado := .F.         // se .T. se teclado ESC
*
* antes de tudo, remontar a tela caso usuário tenha solicitado
*
IF L_ForcaLerTudo
   *
   IF L_PrimAtivacao
      L_PrimAtivacao := .F.
      *
      * No xHarbour, a atribuição da variável FREEZE reexecuta os codes blocks,
      * o que causava erro quando um registro era deletado. Corrigir este
      * problema usando a variável L_PrimAtivacao, que faz com que o FREEZE
      * somente seja setado uma vez por janela (na primeira ativação).
      *
      VX_Sele:FREEZE := N_Congela
      *
    //   #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
    //      IF SOB_MODO_GRAFICO()
    //         IF L_MostraGrade
    //            * montar grid
    //            AddGuiObject(VX_Janela,DesenhaGridH(VX_Janela,VX_Sele),;
    //                         CoordenadasBrowse(VX_Sele))
    //            AddGuiObject(VX_Janela,DesenhaGridV(VX_Janela,VX_Sele),;
    //                         CoordenadasBrowse(VX_Sele))
    //            AddGuiObject(VX_Janela,DesenhaBoxExterno(VX_Janela,VX_Sele),;
    //                         CoordenadasBrowse(VX_Sele))
    //         ENDIF
    //      ENDIF
    //   #elif defined(__PLATFORM__LINUX)
    //      // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
    //   #else
    //      #erro "Código não adaptado para esta plataforma"
    //   #endif
      *

        IF SOB_MODO_GRAFICO()
            //
            // FRAN: Here we have to create the TableView
            //

            L_Coords := CoordenadasBrowse(VX_Sele)

            // Add an extra column to scrollbar
            IF L_ScrollVertical
                L_Coords[4]++
            ENDIF

            V_TableView := NAP_TABLEVIEW_CREATE()

            NAP_TABLEVIEW_CUALIB_BIND_DB(V_TableView)
            NAP_TABLEVIEW_FONT(V_TableView)

            IF L_MostraGrade
                NAP_TABLEVIEW_GRID(V_TableView, .T., .T.)
            ENDIF

            IF N_TP_Selecao == _SELE_SIMPLES
                NAP_TABLEVIEW_MULTISEL(V_TableView, .F., .F.)
            ELSEIF N_TP_Selecao == _SELE_MULTIPLA .OR. N_TP_Selecao == _SELE_EXTENDIDA
                NAP_TABLEVIEW_MULTISEL(V_TableView, .T., .T.)



            ENDIF

            LOG_PRINT("TableView Vert Coords:" + hb_ntos(L_Coords[1]) + ", " + hb_ntos(L_Coords[2]) + ", " + hb_ntos(L_Coords[3]) + ", " + hb_ntos(L_Coords[4]))

            NAP_CUALIB_TABLEVIEW(V_TableView, L_Coords[1], L_Coords[2], L_Coords[3], L_Coords[4])

            LOG_PRINT("Num cols: " + hb_ntos(VX_Sele:COLCOUNT))
            FOR N_Count := 1 TO VX_Sele:COLCOUNT
                O_Column := VX_Sele:GetColumn(N_Count)
                C_Title := O_Column:HEADING
                N_Width := O_Column:WIDTH

                IF C_Title == NIL
                    C_Title := ""
                ENDIF

                IF N_Width == NIL
                    N_Width := 0
                ENDIF

                // LOG_PRINT(O_Column:HEADING + ":")
                // IF O_Column:WIDTH # NIL
                //     LOG_PRINT("WIDTH " + hb_ntos(O_Column:WIDTH))
                // ELSE
                //     LOG_PRINT("WIDTH IS NULL")
                // ENDIF

                NAP_TABLEVIEW_CUALIB_COLUMN_DB(V_TableView, C_Title,O_Column:BLOCK,N_Width)

            NEXT


            NAP_TABLEVIEW_CUALIB_REFRESH(SET(_SET_DELETED))


            // NAP_TABLEVIEW_UPDATE(V_TableView)

            LOG_PRINT("Current VN_Selecio: " + hb_ntos(LEN(VN_Selecio)))

            //
            // FRAN: Automatic first selection and change selection event
            //
            //NAP_TABLEVIEW_DESELECT_ALL(V_TableView)

            IF N_TP_Selecao == _SELE_SIMPLES
                // NAP_TABLEVIEW_CUALIB_REFRESH do the select
                //NAP_TABLEVIEW_SELECT(V_TableView, 1)
                NAP_TABLEVIEW_CUALIB_ON_SINGLE_SELECT_CHANGE()

            ELSEIF N_TP_Selecao == _SELE_MULTIPLA .OR. N_TP_Selecao == _SELE_EXTENDIDA
                NAP_TABLEVIEW_SELECT(V_TableView, VN_Selecio)
                //NAP_CUALIB_SET_JANELA(VX_Sele)
                //NAP_TABLEVIEW_CUALIB_ON_SELECT_CHANGE({ | VX_Janela | UpdatedSelected(VX_Janela)})
                NAP_TABLEVIEW_CUALIB_ON_SELECT_CHANGE({ || UpdatedSelected()})
                IF N_Congela # 0
                    NAP_TABLEVIEW_COLUMN_FREEZE(V_TableView, N_Congela)
                ENDIF

            ENDIF
            //LOG_PRINT("TableView DEFAULT SEL:" + hb_ntos(LEN(VN_Selecio)) + ", JAJAJAJ")


        ENDIF   // SOB_MODO_GRAFICO()

   ENDIF  // L_PrimAtivacao
   *
   IF N_TP_Selecao # _SELE_SIMPLES .AND. VX_Sele:COLPOS == 1   &&* TALVEZ SAIA
      VX_Sele:COLPOS := 2          // cursor não acessa indicativo de seleção
   ENDIF
   *
   * No harbour, corrigir cor de fundo das colunas inicial e final do Tbrowse().
   * Na ativação inicial da janela, mesmo sem ter tipo nenhum
   * rolamento horizotal, é necessário fazer isto porque a
   * cor de fundo eventualmente fica diferente do resto da área útil
   * do TBrowse(), ficando uma "marca" vertical na tela.
   * Vide explicação detalhada no próprio método abaixo.
//    IF SOB_MODO_GRAFICO()
//       VX_Sele:LimparColunasInicialFinal_do_BugDispBox(L_MostraGrade)
//    ENDIF
   *

    //
    // FRAN: The reading of data on demand will be controlled inside the TableView.
    // This control manages the scroll bars, the keyboard, etc. Calculate the visible
    // area and request the necessary data according to the area of the database
    // visible on the screen. Harbour TBrowse is totally dispensed with for browsing the data.
    //
    IF .NOT. SOB_MODO_GRAFICO()
        LerTudoEmDBF(VX_Sele)
    ENDIF
   *
ENDIF   // L_ForcaLerTudo
*

IF SOB_MODO_GRAFICO()

            // * Constantes do vetor de ações de teclado (V_LstAcoes) das janelas
            // #DEFINE _ACAO_KEYBOARD        1
            // #DEFINE _ACAO_KEYBOARD_CASE   2
            // #DEFINE _ACAO_BLOCO_ACAO      3
            // #DEFINE _ACAO_AUTOCLOSE       4
            // #DEFINE _ACAO_CDBOTAO         5
            // #DEFINE _ACAO_ALIAS_MUDA      6
            // #DEFINE _ACAO_RECNO_MUDA      7
            // #DEFINE _ACAO_FILTER_MUDA     8
            // #DEFINE _ACAO_ORDER_MUDA      9
            // #DEFINE _ACAO_EOFOK          10
            // #DEFINE _ACAO_HANDLE_MUDA    11
            // #DEFINE _ACAO_MUDADADOS      12

    FOR N_Cont := 1 TO LEN(V_LstAcoes)
        NAP_LOG("V_LstAcoes:" + hb_ntos(N_Cont) + " KEy: " + hb_ntos(V_LstAcoes[N_Cont,_ACAO_KEYBOARD]))

        IF V_LstAcoes[N_Cont,_ACAO_KEYBOARD] # NIL
            NAP_CUALIB_HOTKEY(V_LstAcoes[N_Cont,_ACAO_KEYBOARD], V_LstAcoes[N_Cont,_ACAO_BLOCO_ACAO], V_LstAcoes[N_Cont,_ACAO_AUTOCLOSE])
        ENDIF
    NEXT

    IF N_KeyBoard # NIL
        NAP_LOG("BUTTON KEYBOARD " + hb_ntos(N_Keyboard))
        NAP_CUALIB_HOTKEY(N_KeyBoard, V_Botao[_BOTAO_BLOCO_ACAO], V_Botao[_BOTAO_AUTOCLOSE])
    ENDIF


    NAP_LOG("SELECAOA::NAP_CUALIB_LAUNCH_MODAL!!!!!!")
    X_Retorno := NAP_CUALIB_LAUNCH_MODAL({||.T.}, {||.T.})
    NAP_LOG("SELECAOA::FINISH!!!!  NAP_CUALIB_LAUNCH_MODAL!!!!!!")

    //X_Retorno := 0


ELSE

//
// THIS BLOCK ONLY FOR TEXT GTs
//
// FRAN: The reading of data on demand will be controlled inside the TableView.
// This control manages the scroll bars, the keyboard, etc. Calculate the visible
// area and request the necessary data according to the area of the database
// visible on the screen. Harbour TBrowse is totally dispensed with for browsing the data.
//

L_RolaCima := L_RolaBaixo := .F.
*
L_ForcaParada := .F.
L_Mais := .T.                                    // simula um DO UNTIL
*
DO WHILE L_Mais
   *
   IF N_TP_Selecao # _SELE_SIMPLES .AND. VX_Sele:COLPOS == 1       &&* TALVEZ SAIA
      VX_Sele:COLPOS := 2          // cursor não acessa indicativo de seleção
   ENDIF
   *
   // Ativar o código abaixo somente quando se quiser que o Browse
   // seja em "câmara lenta", para facilitar visualizar a ordem
   // em que os elementos da tela são atualizados !
   //#IFDEF _TESTE
   //   PRIVATE N_SECONDS := SECONDS()
   //   DO WHILE N_SECONDS # SECONDS()-2  // atrasar 2 segundos
   //   ENDDO
   //#ENDIF
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
         N_Tecla # K_END   .AND. N_Tecla # K_HOME
         VX_Sele:COLORRECT({VX_Sele:ROWPOS,IIF(N_TP_Selecao==_SELE_SIMPLES,1,2),;
                            VX_Sele:ROWPOS,VX_Sele:COLCOUNT} ,{2,3})
      ENDIF
   ENDIF
   *
   L_ForcaParada := .F.
   DO CASE
      CASE N_Tecla == 0              // nenhuma tecla pressionada
           L_AtivaGui := .F.
           IF VX_Sele:STABLE
              MontarSetas(VX_Janela,L_RolaCima,L_RolaBaixo,N_TP_Selecao) // estabilizar setas
              L_RolaCima := L_RolaBaixo := .F.
              L_ForcaParada := .T.
              *
              IF L_TeveRolaHorizontal
                 L_TeveRolaHorizontal := .F.
                 *
                 * No Harbour, corrigir limpeza das colunas inicial e final do Tbrowse()
                 * Vide explicação detalhada no próprio método abaixo.
                //
                // FRAN. This part only enter in TEXT terminals
                //
                //  IF SOB_MODO_GRAFICO()
                //     VX_Sele:LimparColunasInicialFinal_do_BugDispBox(L_MostraGrade)
                //  ENDIF
              ENDIF
              *
           ELSE
              VX_Sele:STABILIZE()            // fazer estabilização incremental
           ENDIF
           L_AtivaGui := .T.
      CASE N_Tecla == K_DOWN .OR. N_Tecla == K_MWBACKWARD
           IF VX_Sele:ROWPOS == VX_Sele:ROWCOUNT
              L_RolaBaixo := .T.
           ENDIF
           VX_Sele:DOWN()
      CASE N_Tecla == K_UP   .OR. N_Tecla == K_MWFORWARD
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
      CASE N_Tecla == K_RIGHT
           VX_Sele:PANRIGHT()
           L_TeveRolaHorizontal := .T.
      CASE N_Tecla == K_LEFT
           VX_Sele:PANLEFT()
           L_TeveRolaHorizontal := .T.
      CASE N_Tecla == K_CTRL_END
           L_RolaBaixo := .T.
           VX_Sele:GOBOTTOM()
      CASE N_Tecla == K_CTRL_HOME
           L_RolaCima := .T.
           VX_Sele:GOTOP()
      CASE N_Tecla == K_HOME
           VX_Sele:PANHOME()
           L_TeveRolaHorizontal := .T.
      CASE N_Tecla == K_END
           VX_Sele:PANEND()
           L_TeveRolaHorizontal := .T.
      CASE N_Tecla == K_F10   // seta os direitos do usuário sobre a tela
           #DEFINE C_Ajuda     VX_Janela[17]
           EVAL(SETA_DIREITO(),C_Ajuda)
           #UNDEF C_Ajuda
      CASE N_TP_Selecao # _SELE_SIMPLES .AND. N_Tecla == 32    // barra de espaço
           IF .NOT. XEOF()
              N_Pos := ASCAN(VN_Selecio,EVAL(B_LinCorrente))
              IF N_Pos == 0
                 * não marcado, incluir na lista e aumentar o tamanho do vetor
                 AADD(VN_Selecio,EVAL(B_LinCorrente))
              ELSE
                 * marcado, excluir da lista e reduzir o tamanho do vetor
                 ADEL(VN_Selecio,N_Pos)
                 ASIZE(VN_Selecio,LEN(VN_Selecio)-1)
              ENDIF
              VX_Sele:REFRESHCURRENT()
           ENDIF
      case N_Tecla == K_LBUTTONDOWN .OR. ;
           N_Tecla == K_LDBLCLK     .OR. ;
           N_Tecla == K_RBUTTONDOWN .OR. ;
           N_Tecla == K_RDBLCLK
           *
           N_mRow := mRow()
           N_mCol := mCol()
           N_Row_Inicial_Util := VX_Sele:nTop + N_AlturaCabec
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
              ENDIF
              L_AtivaGui := .T.
              *
              IF N_RegiaoMouse == AREA_UTIL
                 IF N_TP_Selecao == _SELE_SIMPLES
                    IF N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
                       * Seleciona e tecla ENTER ao mesmo tempo
                       HB_KeyPut(K_ENTER)
                    ELSEIF N_Tecla == K_RBUTTONDOWN .OR. N_Tecla == K_RDBLCLK
                       * Somente tem o efeito de selecionar
                    ENDIF
                 ELSE   //  marcar linhas clicadas
                    *
                    * é necessário dar um refresh de imediato, para que
                    * o SkipBlock() seja executado e o número do registro ou
                    * posição do vetor seja armazenado corretamente.
                    *
                    N_Pos := ASCAN(VN_Selecio,EVAL(B_LinCorrente))
                    IF N_Pos == 0
                       * não marcado, incluir na lista e aumentar o tamanho do vetor
                       AADD(VN_Selecio,EVAL(B_LinCorrente))
                    ELSE
                       * marcado, excluir da lista e reduzir o tamanho do vetor
                       ADEL(VN_Selecio,N_Pos)
                       ASIZE(VN_Selecio,LEN(VN_Selecio)-1)
                    ENDIF
                    VX_Sele:REFRESHCURRENT()
                 ENDIF
              ENDIF
           ELSEIF (N_RegiaoMouse == BOTAO_IDENTIFICADO .OR. ;  // N_Keyboard preenchido
                   N_RegiaoMouse == BOTAO_NAO_IDENTIFICADO)    // N_Keyboard não preenchido
                //
                // FRAN. This part only enter in TEXT terminals
                //
                //   IF SOB_MODO_GRAFICO()
                //      ? MEMVAR->MODO_GRAFICO_NAO_USA_ESTE_TRECHO_DE_CODIGO
                //   ENDIF
              *
              L_PodeExecutar := .T.
              IF LEFT(UPPER(V_Botao[_BOTAO_TEXTO_COMANDO]),2) $ "I=/A=/E=/C=/L="
                 #DEFINE C_Ajuda     VX_Janela[17]
                 L_PodeExecutar := EVAL(SETA_ACEOPC(),;
                          ASC(UPPER(LEFT(V_Botao[_BOTAO_TEXTO_COMANDO],1))),;
                          C_Ajuda)  // se acessa opcao
                 #UNDEF C_Ajuda
              ENDIF
              *
              IF L_PodeExecutar
                 * Atualizar completamente a tela antes de executar o bloco de código
                 Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
                 *
                 IF .NOT. XEOF() .OR. V_Botao[_BOTAO_EOFOK]
                    V_Ambiente_Alias := Salva_Ambiente_Alias()
                    *
                    * Garantir que, ao executar o EVAL(), os blocos de código das
                    * colunas que apontam para tabelas secundárias sejam também atualizados.
                    VX_Sele:REFRESHCURRENT()
                    DO WHILE .NOT. VX_Sele:STABILIZE()            // do corpo da seleção
                    ENDDO
                    *
                    X_Retorno_Eval := EVAL(V_Botao[_BOTAO_BLOCO_ACAO])
                    Checa_Ambiente_Alias(V_Ambiente_Alias,;
                                         V_Botao[_BOTAO_CDBOTAO],;
                                         V_Botao[_BOTAO_ALIAS_MUDA],;
                                         V_Botao[_BOTAO_RECNO_MUDA],;
                                         V_Botao[_BOTAO_FILTER_MUDA],;
                                         V_Botao[_BOTAO_ORDER_MUDA],;
                                         V_Botao[_BOTAO_HANDLE_MUDA])

                    * Logar uso de botões, para ter estatística de uso
                    IF V_Botao[_BOTAO_CDBOTAO] # NIL  // Se for CUA 2.0
                       LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Botao[_BOTAO_CDBOTAO],;
                                                    C_CdTela,"Botão "+V_Botao[_BOTAO_TEXTO_COMANDO])   // Log de uso de botão no sistema
                    ENDIF

                 ENDIF
                 *
                 IF L_ForcaLerTudo
                    * Usuário pode ter chamado a RELEIA TUDO
                    * de dentro da ação do botão.
                    LerTudoEmDBF(VX_Sele)
                 ENDIF
                 *
                 IF V_Botao[_BOTAO_AUTOCLOSE]
                    DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                    IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                       ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                    ENDIF
                    IF X_Retorno_Eval
                       L_Mais := .F.
                    ENDIF
                 ENDIF
              ENDIF
           ELSEIF N_RegiaoMouse == SOBRE_IMAGEM
              * Atualizar completamente a tela antes de executar o bloco de código
              Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
              *
            //
            // FRAN. This part only enter in TEXT terminals
            //
            //   #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
            //      IF SOB_MODO_GRAFICO()
            //         WVW_SetPaintRefresh(N_PaintRefresh_Old)
            //      ENDIF
            //   #elif defined(__PLATFORM__LINUX)
            //      // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
            //   #else
            //      #erro "Código não adaptado para esta plataforma"
            //   #endif
              *
              IF .NOT. XEOF() .OR. V_Imagem[_IMAGEM_EOFOK]
                 V_Ambiente_Alias := Salva_Ambiente_Alias()
                 *
                 * Garantir que, ao executar o EVAL(), os blocos de código das
                 * colunas que apontam para tabelas secundárias sejam também atualizados.
                 VX_Sele:REFRESHCURRENT()
                 DO WHILE .NOT. VX_Sele:STABILIZE()            // do corpo da seleção
                 ENDDO
                 *
                 X_Retorno_Eval := EVAL(V_Imagem[_IMAGEM_BLOCO_ACAO])
                 Checa_Ambiente_Alias(V_Ambiente_Alias,;
                                      V_Imagem[_IMAGEM_CDBOTAO],;
                                      V_Imagem[_IMAGEM_ALIAS_MUDA],;
                                      V_Imagem[_IMAGEM_RECNO_MUDA],;
                                      V_Imagem[_IMAGEM_FILTER_MUDA],;
                                      V_Imagem[_IMAGEM_ORDER_MUDA],;
                                      V_Imagem[_IMAGEM_HANDLE_MUDA])

                 * Logar uso de imagens, para ter estatística de uso
                 IF V_Imagem[_IMAGEM_CDBOTAO] # NIL  // Se for CUA 2.0
                    LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Imagem[_IMAGEM_CDBOTAO],;
                                                 C_CdTela,"Imagem "+V_Imagem[_IMAGEM_ARQUIVO])   // Log de uso de imagem no sistema
                 ENDIF

              ENDIF
              *

            //
            // FRAN. This part only enter in TEXT terminals
            //
            //   #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
            //      IF SOB_MODO_GRAFICO()
            //         WVW_SetPaintRefresh(_REPAINT_DEFAULT)
            //      ENDIF
            //   #elif defined(__PLATFORM__LINUX)
            //      // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
            //   #else
            //      #erro "Código não adaptado para esta plataforma"
            //   #endif
              *
              IF L_ForcaLerTudo
                 * Usuário pode ter chamado a RELEIA TUDO
                 * de dentro da ação da imagem.
                 LerTudoEmDBF(VX_Sele)
              ENDIF
              *
              IF V_Imagem[_IMAGEM_AUTOCLOSE]
                 DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                 IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                    ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                 ENDIF
                 IF X_Retorno_Eval
                    L_Mais := .F.
                 ENDIF
              ENDIF
           ELSEIF N_Keyboard # NIL
              HB_KeyPut(N_Keyboard)
           ENDIF
           *
      OTHER                          // tecla de não movimentação/marcação
           IF N_Tecla == K_ESC
              L_Abortado := .T.
              L_Mais := .F.
           ELSE
              *
              N_Pos_Acao     := 0
              L_PodeExecutar := .T.
              *
              N_Pos_Acao := ASCAN(V_LstAcoes,{|V_Acao| ;
                                  V_Acao[_ACAO_KEYBOARD]==N_Tecla .OR. ;
                                  V_Acao[_ACAO_KEYBOARD_CASE]==N_Tecla})

              *
              IF N_Pos_Acao # 0
                 * -> Verifica o direito do usuário de acessar a janela
                 IF XUPPER(CHR(N_Tecla)) $ "IAECL"
                    #DEFINE C_Ajuda     VX_Janela[17]
                    L_PodeExecutar := EVAL(SETA_ACEOPC(),N_Tecla,C_Ajuda)  // se acessa opcao
                    #UNDEF C_Ajuda
                 ENDIF
              ENDIF
              *
              IF N_Pos_Acao # 0 .AND. L_PodeExecutar
                 * Atualizar completamente a tela antes de executar o bloco de código
                 Atualizar_Tela_Browse(VX_Janela,VX_Sele,L_RolaCima,L_RolaBaixo)
                 *
                 IF N_TP_Selecao # _SELE_SIMPLES
                    * coluna da seleção múltipla ficava com cor errada...
                    VX_Sele:COLORRECT({VX_Sele:ROWPOS,1,;
                                       VX_Sele:ROWPOS,VX_Sele:COLCOUNT} ,{2,3})
                 ENDIF
                 *

                //
                // FRAN. This part only enter in TEXT terminals
                //
                //  #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
                //     IF SOB_MODO_GRAFICO()
                //        WVW_SetPaintRefresh(N_PaintRefresh_Old)
                //     ENDIF
                //  #elif defined(__PLATFORM__LINUX)
                //     // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
                //  #else
                //     #erro "Código não adaptado para esta plataforma"
                //  #endif
                 *
                 IF .NOT. XEOF() .OR. V_LstAcoes[N_Pos_Acao,_ACAO_EOFOK]
                    V_Ambiente_Alias := Salva_Ambiente_Alias()
                    *
                    * Garantir que, ao executar o EVAL(), os blocos de código das
                    * colunas que apontam para tabelas secundárias sejam também atualizados.
                    VX_Sele:REFRESHCURRENT()
                    DO WHILE .NOT. VX_Sele:STABILIZE()            // do corpo da seleção
                    ENDDO
                    *
                    X_Retorno_Eval := EVAL(V_LstAcoes[N_Pos_Acao,_ACAO_BLOCO_ACAO])
                    Checa_Ambiente_Alias(V_Ambiente_Alias,;
                                      V_LstAcoes[N_Pos_Acao,_ACAO_CDBOTAO],;
                                      V_LstAcoes[N_Pos_Acao,_ACAO_ALIAS_MUDA],;
                                      V_LstAcoes[N_Pos_Acao,_ACAO_RECNO_MUDA],;
                                      V_LstAcoes[N_Pos_Acao,_ACAO_FILTER_MUDA],;
                                      V_LstAcoes[N_Pos_Acao,_ACAO_ORDER_MUDA],;
                                      V_LstAcoes[N_Pos_Acao,_ACAO_HANDLE_MUDA])

                    * Logar uso de ações, para ter estatística de uso
                    IF V_LstAcoes[N_Pos_Acao,_ACAO_CDBOTAO] # NIL  // Se for CUA 2.0
                       LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_LstAcoes[N_Pos_Acao,_ACAO_CDBOTAO],;
                                                    C_CdTela,"Ação "+STR(V_LstAcoes[N_Pos_Acao,_ACAO_KEYBOARD],5))   // Log de uso de ações de teclado no sistema
                    ENDIF

                 ENDIF
                 *

                //
                // FRAN. This part only enter in TEXT terminals
                //
                //  #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
                //     IF SOB_MODO_GRAFICO()
                //        WVW_SetPaintRefresh(_REPAINT_DEFAULT)
                //     ENDIF
                //  #elif defined(__PLATFORM__LINUX)
                //     // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
                //  #else
                //     #erro "Código não adaptado para esta plataforma"
                //  #endif
                 *
                 IF L_ForcaLerTudo
                    * Usuário pode ter chamado a RELEIA TUDO
                    * de dentro da ação do botão.
                    LerTudoEmDBF(VX_Sele)
                 ENDIF
                 *
                 IF V_LstAcoes[N_Pos_Acao,_ACAO_AUTOCLOSE]
                    DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                    IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                       ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                    ENDIF
                    IF X_Retorno_Eval
                       L_Mais := .F.
                    ENDIF
                 ENDIF
              ENDIF
              *
           ENDIF
   ENDCASE
   *
ENDDO   // DO WHILE L_Mais

*
IF L_Abortado
   MudeLista(VX_Janela)            // limpa o vetor VN_Selecio
ENDIF
*
* Se for browse em arquivo .dbf, alguma outra estação pode ter alterado
* o registro corrente (inclusive preenchido o EOF(), se for o caso).
* Atualizar o registro em exibição antes de sair da tela de seleção
* para fornecer ao usuário a informação mais recente possível.
VX_Sele:REFRESHCURRENT()
*
* foi pressionada uma tecla de não movimentação, forçar total estabilização
*
L_AtivaGui := .F.
DO WHILE .NOT. VX_Sele:STABILIZE()            // do corpo da seleção
ENDDO
MontarSetas(VX_Janela,L_RolaCima,L_RolaBaixo,N_TP_Selecao)   // dos indicativos de rolamento
L_AtivaGui := .T.
*
* retornar o(s) item(ns) selecionado(s)
*

//
// FRAN. This part only enter in TEXT terminals
//
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       WVW_SetPaintRefresh(N_PaintRefresh_Old)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
*
IF N_TP_Selecao == _SELE_SIMPLES       // se selecao simples
   IF L_Abortado
      X_Retorno := 0
   ELSE
      X_Retorno := EVAL(B_LinCorrente)
   ENDIF
ELSE
   IF N_TP_Selecao == _SELE_EXTENDIDA .AND. LEN(VN_Selecio)==0    // se extendida com seleçao implicita
      IF L_Abortado
         X_Retorno := VN_Selecio       // está vazio
      ELSE
         X_Retorno := {EVAL(B_LinCorrente)}
      ENDIF
   ELSE
      X_Retorno := VN_Selecio      // se selecao multipla ou extendida com selecao explicita
   ENDIF
ENDIF

ENDIF // NOT SOB_MODO_GRAFICO()

NAP_LOG("SELECAOA::Selecao finish with: " + hb_ntos(X_Retorno))

RETURN X_Retorno
*





********************
FUNCTION MontarSetas (VX_Janela,L_RolaCima,L_RolaBaixo,N_2TP_Selecao)
********************
*
LOCAL N_UltLinTela , N_Cont , N_IndiceVetor
LOCAL N_Salto      , VX_Sele, N_Pos_Destaque
*
VX_Sele := VX_SubObj
*
IF L_TemHotKey  // se alguma opção possui hotkey
   *
   * ********** montar teclas de destaque, pois browse é de vetor de caracteres
   *
   * as duas linhas abaixo tem por objetivo ter a certeza que o cursor está
   * posicionado na célula em destaque, para a correta impressão dos
   * caracteres com atributo intensificado.
   *
   DO WHILE .NOT. VX_Sele:STABILIZE()   // reposiciona o cursor na célula.
   ENDDO
   *
   IF .NOT. SOB_MODO_GRAFICO()
      N_UltLinTela := VX_Sele:ROWPOS + (LEN(V_Opcoes)-N_Selecio)
      FOR N_Cont := 1 TO VX_Sele:ROWCOUNT
          IF N_Cont # VX_Sele:ROWPOS .AND. N_Cont <= N_UltLinTela
             N_IndiceVetor := N_Selecio - VX_Sele:ROWPOS + N_Cont
             *
             N_Pos_Destaque := V_Opcoes[N_IndiceVetor,_OPCAO_COL_DESTAQUE]
             IF N_Pos_Destaque # 0   // se esta opção possui hotkey
                SETPOS(VX_Sele:NTOP+N_Cont-1,N_ColunaIniVetor+N_Pos_Destaque)
                *
                #DEFINE C_Char   V_Opcoes[N_IndiceVetor,_OPCAO_TEXTO_DESTAQUE]
                DISPOUT(C_Char,CorJanInten(VX_Janela))
                #UNDEF C_Char
             ENDIF
          ENDIF
      NEXT
   ENDIF
   *
ENDIF
*
* ********** montar cursor contínuo em toda a linha, para indicar ao usußrio a linha corrente
*
VX_Sele:COLORRECT({VX_Sele:ROWPOS,IIF(N_2TP_Selecao==_SELE_SIMPLES,1,2),;
                  VX_Sele:ROWPOS,VX_Sele:COLCOUNT} ,{3,3})
*

* ********** montar indicativos de rolamento
*
* a apresentação do indicativo de rolamento P/CIMA e P/BAIXO será otimizada
* para reduzir a execução do VX_Sele:SKIPBLOCK, que é lento com registros
* de arquivos .dbf .
*
* atualizar a seta para cima se houve rolamento para cima ou
*   se houve rolamento para baixo que possa esconder os primeiros elementos
*
IF L_RolaCima .OR. ( L_RolaBaixo .AND. .NOT. L_PriFora ) .OR. L_ForcaLerTudo
   *
   * se rolamento para uma posição antes do topo for passível de atendimento
   *    significa que o primeiro elemento está fora do box
   *
   N_Salto    := EVAL(VX_Sele:SKIPBLOCK,-VX_Sele:ROWPOS)
   IF VALTYPE(N_Salto)=="U"   // se DBF vazio, xharbour tem BUG...
      N_Salto := 0
   ENDIF
   L_PriFora  := ( N_Salto == -VX_Sele:ROWPOS )
   *
   EVAL(VX_Sele:SKIPBLOCK,-N_Salto)            // voltar p/ posição anterior
   *
ENDIF
*
* atualizar a seta para baixo se houve rolamento para baixo ou
*   se houve rolamento para cima que possa esconder os últimos elementos
*
IF L_RolaBaixo .OR. ( L_RolaCima .AND. .NOT. L_UltFora ) .OR. L_ForcaLerTudo
   *
   * se rolamento para uma posição após a última linha do box for passível de
   *    atendimento significa que o último elemento está fora do box
   *
   N_Salto    := EVAL(VX_Sele:SKIPBLOCK,VX_Sele:ROWCOUNT-VX_Sele:ROWPOS+1)
   IF VALTYPE(N_Salto)=="U"   // se DBF vazio, xharbour tem BUG...
      N_Salto := 0
   ENDIF
   L_UltFora  := ( N_Salto == VX_Sele:ROWCOUNT-VX_Sele:ROWPOS+1 )
   EVAL(VX_Sele:SKIPBLOCK,-N_Salto)            // voltar p/ posição anterior
   *
ENDIF
*
#DEFINE L_EsqFora  (VX_Sele:LEFTVISIBLE - VX_Sele:FREEZE > 1)
#DEFINE L_DirFora  (VX_Sele:RIGHTVISIBLE # VX_Sele:COLCOUNT)
*
Rolamento_(VX_Janela,L_EsqFora,L_PriFora,L_UltFora,L_DirFora)
*
#UNDEF L_EsqFora
#UNDEF L_DirFora
*
DO WHILE .NOT. VX_Sele:STABILIZE()   // reposiciona o cursor na célula.
ENDDO
*
L_ForcaLerTudo := .F.
*
RETURN NIL
*
**************************
STAT FUNCTION LerTudoEmDBF ( VX_Sele )
**************************
LOCAL N_Salto_unitario, N_Salto_acumulado
LOCAL N_Recno_atual_apos_filtros_mas_antes_stabilize
LOCAL L_Buscar_antigo_registro, L_Encontrou_registro_antigo
LOCAL N_RowPos_ant, N_CT_Up
*
// LOCAL L_CasoEspecial

L_AtivaGui := .F.
*
* Se cursor na primeira posição, a estabilização não usa o SKIPBLOCK
* para remontar a 1ª linha. Isto causa problema no caso de deleção
* do item da 1ª linha, pois a mesma continuará na tela.
* Para resolver este caso, o cursor será retirado da 1ª linha antes
* da estabilização, e depois retornado à posição inicial.
*
* CASO ESPECIAL 1: Quando, num browse:
******************
*   - Está selecionada a primeira linha
*   - Programador dá um DELETE no registro
*   - Programador dá um RELEIA TUDO
*
* PROBLEMA: No Harbour 3.2
*   - O registro não é movimentado, mesmo executando a RefreshAll().
*   - O registro permanece sendo exibido, mesmo estando DELETED() !
* SOLUÇÃO:
*   - A solução será reposicionar "na mão", sem usar as rotinas DOWN() e UP()
*   - Para que o registro não fique posicionado sobre um registro inválido
*     (ex: SET DELETED estar em ON e registro ser DELETED()...)
*
N_Salto_unitario := EVAL(VX_Sele:SKIPBLOCK,+1)
EVAL(VX_Sele:SKIPBLOCK,-N_Salto_unitario)            // voltar p/ posição anterior
*
* O registro atual, se não for EOF(), é sempre um registro válido para exibição.
* O fato de retroceder e avançar 1 registro usando a
* "VX_Sele:SKIPBLOCK" (acima) nos garante que o registro após a movimentação:
*    - Atende a eventual cláusula WHILE da CUA (bloco de código B_While)
*    - Atende a eventual SET FILTER
*    - Atenda a eventual SET SCOPE
*    - Atende ao SET DELETE ON (não posicionado sobre registro deletado)
N_Recno_atual_apos_filtros_mas_antes_stabilize := RECNO()
*
* Código normal de refresh da tela
VX_Sele:REFRESHALL()     // forçar a remontagem da tela
DO WHILE .NOT. VX_Sele:STABILIZE()
ENDDO
*

/* IMPORTANTE: O código abaixo também resolve o BUG, e foi usado já no Clipper.
               Guardar, pois pode ser ativado, caso a sistemática acima tenha desvantagem.
   * No Harbour 3.2
   *   - O tratamento dado pela variável L_CasoEspecial
   *     resolve o problema de posicionar o ponteiro da área de trabalho
   *     em registro não deletado (respeitando o SET DELETE ON) e também
   *     respeitando o filtro da cláusula WHILE. Ou seja, a estabilização
   *     consulta a SkipWhile() para toda movimentação de ponteiro.
   *
   * Se cursor na primeira posição, a estabilização não usa o SKIPBLOCK
   * para remontar a 1ª linha. Isto causa problema no caso de deleção
   * do item da 1ª linha, pois a mesma continuará na tela.
   *
   * SOLUÇÃO:
   * - Para resolver este caso, o cursor será retirado da 1ª linha antes
   *    da estabilização, e depois retornado à posição inicial.
   *
   L_CasoEspecial := (VX_Sele:ROWPOS == 1)
   *
   IF L_CasoEspecial
      VX_Sele:DOWN()
      DO WHILE .NOT. VX_Sele:STABILIZE()
      ENDDO
   ENDIF
   *
   * Código normal de refresh da tela
   VX_Sele:REFRESHALL()     // forçar a remontagem da tela
   DO WHILE .NOT. VX_Sele:STABILIZE()
   ENDDO
   *
   IF L_CasoEspecial
      VX_Sele:UP()
      DO WHILE .NOT. VX_Sele:STABILIZE()
      ENDDO
   ENDIF
*/
*
* CASO ESPECIAL 2: Quando, num browse:
******************
*   - Os registros que estão sendo exibidos são registros da
*     "primeira página" do arquivo, onde um PageUp não exibe
*     informações, pois não tem linhas "antes do topo do Browse"
*   - Está selecionada a quinta linha (ex: RECNO()==5) e
*   - O usuário final usa a opção de "P=procurar", e busca
*     um registro anterior ao que está atualmente posicionado (ex: RECNO()==1)
*   - Programador dá um RELEIA TUDO (o que é correto) para atualizar o browse
*
IF .NOT. XEOF()
   * No Harbour 3.2
   *   - A estabilização (comando "VX_Sele:STABILIZE()" acima) INDEVIDAMENTE
   *     move o registro corrente de volta para a RECNO==5, muito embora
   *     o SEEK da pesquisa tenha posicionado no RECNO=1.
   *   - Ou seja, a TBrowse muda no RECNO do arquivo, na tentativa de manter
   *     visualmente na tela a mesma linha selecionada!
   *
   IF N_Recno_atual_apos_filtros_mas_antes_stabilize # RECNO()  // Ocorreu a mudança indevida de registro!
      *
      N_RowPos_ant := VX_Sele:RowPos
      *
      N_Salto_acumulado := 0 // guardar quantos registros serão retrocedidos
      L_Encontrou_registro_antigo := .F.
      L_Buscar_antigo_registro := .T.
      DO WHILE L_Buscar_antigo_registro
         *
         IF ROUND(-N_Salto_acumulado,0) < ;  // O "-" torna o valor positivo.
            ROUND(N_RowPos_ant-1,0)   // A quantidade máxima de retrocesso é uma unidade a menos que o N_RowPos_ant
            *
            N_Salto_unitario := EVAL(VX_Sele:SKIPBLOCK,-1) // Retroceder registro a registro
            N_Salto_acumulado += N_Salto_unitario
            *
            IF ROUND(N_Salto_unitario,0) == -1
               * Conseguiu retroceder um registro.
               * Checar se é o "antigo" registro.
               IF N_Recno_atual_apos_filtros_mas_antes_stabilize == RECNO()
                  L_Encontrou_registro_antigo := .T.
                  * Parar a busca, pois foi encontrado
                  L_Buscar_antigo_registro := .F.
               ENDIF
               *
            ELSE  // Se N_Salto_unitario for 0
               * Não existe mais registro anterior que atenda todos os filtros.
               * Isto pode ocorrer em ambiente multiusuário, onde outro usuário
               * apague um registro que está "antes" do cursor onde está posicionado
               * o registro do usuário atual.
               L_Buscar_antigo_registro := .F.
            ENDIF
         ELSE
            * Não se pode retroceder mais do que as linhas
            * acima da linha atual (ex: N_RowPos_ant for 7, o retrocesso máximo é -6)
            L_Buscar_antigo_registro := .F.
         ENDIF
         *
      ENDDO
      *
      * Se foi realizado algum retrocesso de registro
      IF ROUND(N_Salto_acumulado,0) < 0  // É sempre um número negativo (retrocesso)
         *
         * Temporarmente, de forma incondicional, retornar ao registro que estava antes,
         * mesmo que seja "errado", avançando o mesmo número de registros que foram retrocedidos.
         EVAL(VX_Sele:SKIPBLOCK,-N_Salto_acumulado) // O "-" torna o valor positivo.
         *
         IF L_Encontrou_registro_antigo
            * Voltar para o registro correto, dando comandos que tanto mudem o RECNO()
            * como mudem ao mesmo tempo o cursor da TBrowse.
            *
            FOR N_CT_Up := 1 TO ROUND(-N_Salto_acumulado,0)  // O "-" torna o valor positivo.
                VX_Sele:UP()
            NEXT
            DO WHILE .NOT. VX_Sele:STABILIZE()
            ENDDO
            *
         ENDIF
      ENDIF
   ENDIF
   *
ENDIF
*
L_AtivaGui := .T.
*
RETURN NIL
*
******************
FUNCTION MudeLista ( VX_Janela , VN_Default )
*
LOCAL VX_Sele
LOCAL V_TableView := NIL
*
VX_Sele := VX_SubObj
*
IF VN_Default == NIL
   VN_Default := {}
ENDIF
*
IF LEN(VN_Selecio) # 0 .OR. LEN(VN_Default) # 0         // algo a fazer
   VN_Selecio := VN_Default
   IF L_PrimAtivacao
      * Janela que não foi ainda aberta não tem nenhum aspecto visual
      * a ser atualizado.
   ELSE
    IF SOB_MODO_GRAFICO()
        V_TableView := NAP_CUALIB_CURRENT_TABLEVIEW()
        IF V_TableView # NIL
            NAP_TABLEVIEW_DESELECT_ALL(V_TableView)
            NAP_TABLEVIEW_SELECT(V_TableView, VN_Selecio)

            IF N_TP_Jan == _JAN_SELE_VETO_20
                NAP_CUALIB_VETOR_SELECT(VN_Selecio)
            ENDIF

        ENDIF
    ELSE

        VX_Sele:REFRESHALL()
        *
        * No Harbour, a chamada da REFRESHALL() muda o registro corrente para
        * topo do Browse(), quando o cursor não está na primeira linha do browse().
        * Isto causava erro quando a MudeLista() era chamada fora
        * da CUA (isto é permitido).
        *
        * O cursor só volta para o registro correto APÓS a estabilização da janela.
        DO WHILE .NOT. VX_Sele:STABILIZE()
        ENDDO
    ENDIF
   ENDIF
ENDIF
*
RETURN NIL
*
*
// ***********************
FUNCTION ReleiaCorrente ( VX_Janela )
*
IF SOB_MODO_GRAFICO()
    NAP_TABLEVIEW_CUALIB_REFRESH(SET(_SET_DELETED))

ELSE
#DEFINE VX_Sele VX_SubObj
VX_Sele:REFRESHCURRENT()
#UNDEF  VX_Sele
ENDIF
*
RETURN NIL
*
*
*******************
FUNCTION ReleiaTudo ( VX_Janela )
*
IF SOB_MODO_GRAFICO()
    NAP_TABLEVIEW_CUALIB_REFRESH(SET(_SET_DELETED))

ELSE

#DEFINE VX_Sele VX_SubObj
L_ForcaLerTudo := .T.      // como pode ter sido apagado algum registro
*                          // este procedimento merece tratamento especial
*                          // (um simples REFRESHALL() não resolve !) .
#UNDEF  VX_Sele
ENDIF

RETURN NIL


// *
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    *
//    #define RGB(nR,nG,nB)  ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )
//    *****************
//    FUNC DesenhaGridH (VX_Janela,VX_Sele)
//    *****************
//    RETURN {|| WVW_SetPen(0,0,rgb(210,1210,210)),;
//               WVW_DrawGridHorz( N_WindowNum,;
//                                 VX_Sele:nTop+MAX(N_AlturaCabec,1), VX_Sele:nLeft,;
//                                 VX_Sele:nRight,;
//                                 VX_Sele:nBottom-(VX_Sele:nTop+MAX(N_AlturaCabec,1))+1 )}
//    *****************
//    FUNC DesenhaGridV (VX_Janela,VX_Sele)
//    *****************
//    LOCAL N_DeslocaHorizontal
//    LOCAL N_TelaWidth  := TelaPrincipalWidth()
//    *
//    * "descolar" a linha vertical de algumas colunas do Tbrowse()
//    *
//    IF N_TelaWidth >=  1280   // resolução HORIZONTAL
//       N_DeslocaHorizontal :=  +6
//    ELSEIF N_TelaWidth >=  1152 .OR. ;
//           N_TelaWidth >=  1088
//       N_DeslocaHorizontal :=  +5
//    ELSEIF N_TelaWidth >=  1024
//       N_DeslocaHorizontal :=  +4
//    ELSEIF N_TelaWidth >=   800
//       N_DeslocaHorizontal :=  +3
//    ELSE
//       N_DeslocaHorizontal :=  +2
//    ENDIF
//    *
//    * No Harbour, foi preciso criar o método aColumnsSep(), para poder
//    * listar as linhas verticais, que necessitam da informação contina em "VX_Sele:aColumnsSep()"
//    RETURN {|| WVW_SetPen(0,0,rgb(210,1210,210)),;
//               WVW_DrawGridVert( N_WindowNum, VX_Sele:nTop, VX_Sele:nBottom,;
//                                 VX_Sele:aColumnsSep(), len(VX_Sele:aColumnsSep()),;
//                                 {0,N_DeslocaHorizontal,0,N_DeslocaHorizontal} ) }
//    *
//    **********************
//    FUNC DesenhaBoxExterno (VX_Janela,VX_Sele)
//    **********************
//    * -1 foi para fazer com que a margem direita do Box "colasse" na
//    * linha horizontal feita pela WVW_DrawGridHorz(). Antes ficada 1 pixel
//    * vazio...
//    *
//    RETURN {|| WVW_SetPen(0,0,rgb(210,1210,210)),;
//               WVW_DrawBoxRecessed( N_WindowNum,;
//                                    VX_Sele:nTop, VX_Sele:nLeft,;
//                                    VX_Sele:nBottom,VX_Sele:nRight,;
//                                    {0,0,0,-1} )}
//    *


**********************
FUNC CoordenadasBrowse (VX_Sele)
**********************
RETURN {VX_Sele:nTop, VX_Sele:nLeft,VX_Sele:nBottom,VX_Sele:nRight }


//    *
//    ******************
//    FUNC DesenhaAtalho(VX_Janela,VX_Sele,N_Cont)
//    ******************
//    LOCAL N_DeslocaVertical, N_AlturaEmPixels
//    LOCAL N_TelaHeight := TelaPrincipalHeight()
//    *
//    * Subir o sublinhado, para ficar mais próximo da respectiva letra
//    *
//    IF N_TelaHeight >= 1024  .OR. ;   // resolução VERTICAL
//       N_TelaHeight >=  960
//       N_DeslocaVertical := -1
//       N_AlturaEmPixels  :=  2
//    ELSEIF N_TelaHeight >=  864
//       N_DeslocaVertical := -1
//       N_AlturaEmPixels  :=  1
//    ELSEIF N_TelaHeight >= 768 .OR. ;
//           N_TelaHeight >= 600
//       N_DeslocaVertical :=  0
//       N_AlturaEmPixels  :=  1
//    ELSE
//       N_DeslocaVertical :=  0
//       N_AlturaEmPixels  :=  1
//    ENDIF
//    *
//    RETURN {||DesenhaAtalho2(VX_Janela,VX_Sele,N_Cont,N_DeslocaVertical,N_AlturaEmPixels)}
//    *
//    ************************
//    STAT PROC DesenhaAtalho2(VX_Janela,VX_Sele,N_Cont,N_DeslocaVertical,N_AlturaEmPixels)
//    ************************
//    LOCAL N_LI,N_CI,N_LF,N_CF
//    *
//    #DEFINE N_LinCobertas  (N_Selecio - VX_Sele:ROWPOS())
//    #DEFINE N_AlturaJanela (Lin2Livre(VX_Janela) - Lin1Livre(VX_Janela) + 1)
//    *
//    IF L_AtivaGui
//       N_LI := N_Cont-N_LinCobertas+Lin1Livre(VX_Janela)-1
//       N_CI := N_ColunaIniVetor+V_Opcoes[N_Cont,_OPCAO_COL_DESTAQUE]
//       N_LF := N_Cont-N_LinCobertas+Lin1Livre(VX_Janela)-1
//       N_CF := N_ColunaIniVetor+V_Opcoes[N_Cont,_OPCAO_COL_DESTAQUE]
//       IF N_Cont - N_LinCobertas - 1 >= 0 .AND. ;
//          N_Cont - N_LinCobertas - N_AlturaJanela - 1 < 0 .AND. ;
//          TEVE_SOBREPOSICAO(VX_Janela,WVW_GetPaintRect(N_WindowNum),{N_LI,N_CI,N_LF,N_CF})
//          *
//          WVW_SetPen(0,0,rgb(210,1210,210))
//          WVW_DrawLine(N_WindowNum,N_LI,N_CI,N_LF,N_CF,;
//                       0,2,2,;  // 0=horizontal, 2=plain e 2=bottom
//                       NIL,N_AlturaEmPixels,NIL,;
//                       {N_DeslocaVertical,0,N_DeslocaVertical,0})
//       ENDIF
//    ENDIF
//    *
//    #UNDEF N_LinCobertas
//    #UNDEF N_AlturaJanela
//    *
//    ******************************
//    FUNC DesenhaBoxItemSelecionado(VX_Janela,VX_Sele)
//    ******************************
//    RETURN {||DesenhaBoxItemSelecionado2(VX_Janela,VX_Sele)}
//    *
//    ************************************
//    STAT PROC DesenhaBoxItemSelecionado2(VX_Janela,VX_Sele)
//    ************************************
//    LOCAL N_LI,N_CI,N_LF,N_CF
//    *
//    IF L_AtivaGui
//       N_LI := VX_Sele:nTop+N_AlturaCabec+VX_Sele:RowPos()-1
//       N_CI := VX_Sele:nLeft
//       N_LF := N_LI
//       N_CF := VX_Sele:nRight
//       IF TEVE_SOBREPOSICAO(VX_Janela,WVW_GetPaintRect(N_WindowNum),;
//                            {N_LI,N_CI,N_LF,N_CF})
//          WVW_SetPen(0,0,rgb(210,1210,210))
//          WVW_DrawBoxRaised(N_WindowNum,N_LI,N_CI,N_LF,N_CF,;
//                            {0,+2,0,-2})
//       ENDIF
//    ENDIF
//    *
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// *
// *
****************
FUNC SETA_ACEJAN  (B_ACEJAN_NEW)   // seta a funcao de teste de acesso a janela
****************
STATIC B_ACEJAN2 := {||.T.}
IF B_ACEJAN_NEW # NIL
   B_ACEJAN2 := B_ACEJAN_NEW
ENDIF
RETURN B_ACEJAN2
*
****************
FUNC SETA_ACEOPC  (B_ACEOPC_NEW)   // seta a funcao de teste de acesso a opçäo
****************
STATIC B_ACEOPC2 := {||.T.}
IF B_ACEOPC_NEW # NIL
   B_ACEOPC2 := B_ACEOPC_NEW
ENDIF
RETURN B_ACEOPC2
*
*****************
FUNC SETA_DIREITO(B_DIREITO_NEW)   // determina a funcao de definicao de direitos do usuário
*****************
STATIC B_DIREITO2 := {||.T.}
IF B_DIREITO_NEW # NIL
   B_DIREITO2 := B_DIREITO_NEW
ENDIF
RETURN B_DIREITO2
*
******************************
STAT FUNC Salva_Ambiente_Alias()
******************************
LOCAL V_Ambiente_Alias
*
* Características que não devem mudar durante a execução da ação
#DEFINE _AMBIENTE_ALIAS        01
#DEFINE _AMBIENTE_SELECT       02
#DEFINE _AMBIENTE_READONLY     03
#DEFINE _AMBIENTE_SHARED       04
#DEFINE _AMBIENTE_HANDLE       05
#DEFINE _AMBIENTE_FULLPATH_DBF 06
#DEFINE _AMBIENTE_SET_DELETED  07
#DEFINE _AMBIENTE_DBRLOCKLIST  08
#DEFINE _AMBIENTE_ISFLOCK      09
* Características relativas a posicionamento do ponteiro
#DEFINE _AMBIENTE_RECNO        10
* Características que implicam em filtro no arquivo
#DEFINE _AMBIENTE_DBFILTER     11
* Características relativas a orders
#DEFINE _AMBIENTE_INDEXORD     12
#DEFINE _AMBIENTE_ORDBAGNAME   13
#DEFINE _AMBIENTE_ORDNAME      14
#DEFINE _AMBIENTE_ORDKEY       15
#DEFINE _AMBIENTE_ORDFOR       16
#DEFINE _AMBIENTE_FULLPATH_CDX 17
#DEFINE _AMBIENTE_SCOPETOP     18
#DEFINE _AMBIENTE_SCOPEBOTTOM  19
*
#DEFINE _AMBIENTE_LEN          19
*
V_Ambiente_Alias := ARRAY(_AMBIENTE_LEN)

#INCLUDE "dbinfo.ch"
*
V_Ambiente_Alias[_AMBIENTE_ALIAS       ] := ALIAS()
V_Ambiente_Alias[_AMBIENTE_SELECT      ] := SELECT()
V_Ambiente_Alias[_AMBIENTE_READONLY    ] := DBINFO(DBI_ISREADONLY)
V_Ambiente_Alias[_AMBIENTE_SHARED      ] := DBINFO(DBI_SHARED)
V_Ambiente_Alias[_AMBIENTE_HANDLE      ] := DBINFO(DBI_FILEHANDLE)
V_Ambiente_Alias[_AMBIENTE_FULLPATH_DBF] := DBINFO(DBI_FULLPATH)
V_Ambiente_Alias[_AMBIENTE_SET_DELETED ] := SET(_SET_DELETED)
V_Ambiente_Alias[_AMBIENTE_DBRLOCKLIST ] := DBRLOCKLIST()
V_Ambiente_Alias[_AMBIENTE_ISFLOCK     ] := DBINFO(DBI_ISFLOCK)
*
V_Ambiente_Alias[_AMBIENTE_DBFILTER    ] := DBFILTER()
*
V_Ambiente_Alias[_AMBIENTE_RECNO       ] := RECNO()
*
V_Ambiente_Alias[_AMBIENTE_INDEXORD    ] := INDEXORD()
V_Ambiente_Alias[_AMBIENTE_ORDBAGNAME  ] := ORDBAGNAME()
V_Ambiente_Alias[_AMBIENTE_ORDNAME     ] := ORDNAME()
V_Ambiente_Alias[_AMBIENTE_ORDKEY      ] := ORDKEY()
V_Ambiente_Alias[_AMBIENTE_ORDFOR      ] := ORDFOR()
V_Ambiente_Alias[_AMBIENTE_FULLPATH_CDX] := DBORDERINFO(DBOI_FULLPATH)
V_Ambiente_Alias[_AMBIENTE_SCOPETOP    ] := DBORDERINFO(DBOI_SCOPETOP)
V_Ambiente_Alias[_AMBIENTE_SCOPEBOTTOM ] := DBORDERINFO(DBOI_SCOPEBOTTOM)
*
RETURN V_Ambiente_Alias
*
******************************
STAT PROC Checa_Ambiente_Alias(V_Ambiente_Alias,;
                               C_CdBotao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_HandleMuda )
******************************
LOCAL N_CT
*
DEFAULT L_HandleMuda TO .F.   //!! DEPOIS REMOVER

IF .NOT. L_AliasMuda
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"Alias()",;
      V_Ambiente_Alias[_AMBIENTE_ALIAS],ALIAS())
   COMPARA_INFO(C_CDBOTAO,"N",.F.,"Select()",;
      V_Ambiente_Alias[_AMBIENTE_SELECT],SELECT())
   COMPARA_INFO(C_CDBOTAO,"L",.F.,"ReadOnly",;
      V_Ambiente_Alias[_AMBIENTE_READONLY],DBINFO(DBI_ISREADONLY))
   COMPARA_INFO(C_CDBOTAO,"L",.F.,"Shared",;
      V_Ambiente_Alias[_AMBIENTE_SHARED],DBINFO(DBI_SHARED))
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"FullPath_DBF",;
      V_Ambiente_Alias[_AMBIENTE_FULLPATH_DBF],DBINFO(DBI_FULLPATH))
   COMPARA_INFO(C_CDBOTAO,"L",.F.,"SetDeleted",;
      V_Ambiente_Alias[_AMBIENTE_SET_DELETED],SET(_SET_DELETED))
   IF LEN(V_Ambiente_Alias[_AMBIENTE_DBRLOCKLIST]) # LEN(DBRLOCKLIST())
      COMPARA_INFO(C_CDBOTAO,"N",.F.,"Len(DBRLockList())",;
         LEN(V_Ambiente_Alias[_AMBIENTE_DBRLOCKLIST]),;
         LEN(DBRLOCKLIST()))
   ELSE
      FOR N_CT := 1 TO LEN(DBRLOCKLIST())
         COMPARA_INFO(C_CDBOTAO,"N",.F.,"DBRLockList()[]",;
            V_Ambiente_Alias[_AMBIENTE_DBRLOCKLIST,N_CT],DBRLOCKLIST()[N_CT])
      NEXT
   ENDIF
   COMPARA_INFO(C_CDBOTAO,"L",.F.,"IsLock",;
      V_Ambiente_Alias[_AMBIENTE_ISFLOCK],DBINFO(DBI_ISFLOCK))
ENDIF
*
IF .NOT. L_RecnoMuda
   COMPARA_INFO(C_CDBOTAO,"N",.F.,"Recno()",;
      V_Ambiente_Alias[_AMBIENTE_RECNO],RECNO())
ENDIF
*
IF .NOT. L_FilterMuda
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"DBFilter()",;
      V_Ambiente_Alias[_AMBIENTE_DBFILTER],DBFILTER())
ENDIF
*
IF .NOT. L_OrderMuda
   COMPARA_INFO(C_CDBOTAO,"N",.F.,"IndexOrd()",;
      V_Ambiente_Alias[_AMBIENTE_INDEXORD],INDEXORD())
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"OrdBagName()",;
      V_Ambiente_Alias[_AMBIENTE_ORDBAGNAME],ORDBAGNAME())
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"OrdName()",;
      V_Ambiente_Alias[_AMBIENTE_ORDNAME],ORDNAME())
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"OrdKey()",;
      V_Ambiente_Alias[_AMBIENTE_ORDKEY],ORDKEY())
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"OrdFor()",;
      V_Ambiente_Alias[_AMBIENTE_ORDFOR],ORDFOR())
   COMPARA_INFO(C_CDBOTAO,"C",.F.,"FullPath_CDX",;
      V_Ambiente_Alias[_AMBIENTE_FULLPATH_CDX],DBORDERINFO(DBOI_FULLPATH))
   COMPARA_INFO(C_CDBOTAO,"C",.T.,"ScopeTop",;
      V_Ambiente_Alias[_AMBIENTE_SCOPETOP],DBORDERINFO(DBOI_SCOPETOP))
   COMPARA_INFO(C_CDBOTAO,"C",.T.,"ScopeBottom",;
      V_Ambiente_Alias[_AMBIENTE_SCOPEBOTTOM],DBORDERINFO(DBOI_SCOPEBOTTOM))
ENDIF
*
IF .NOT. L_HandleMuda
   COMPARA_INFO(C_CDBOTAO,"N",.F.,"FileHandle",;
      V_Ambiente_Alias[_AMBIENTE_HANDLE],DBINFO(DBI_FILEHANDLE))
ENDIF
*
* zerar conteúdo do vetor
AFILL(V_Ambiente_Alias,NIL)
ASIZE(V_Ambiente_Alias,0)
*
**********************
STAT PROC COMPARA_INFO(C_CDBOTAO,C_TYPEVAR,L_PODE_NIL,C_Aspecto,;
                       X_Info_Old,X_Info_New)
**********************
LOCAL C_Info_Old, C_Info_New
LOCAL V_JAN_MENS, L_CANCELAR, L_LOGAR
*
IF X_Info_Old==NIL .AND. .NOT. L_PODE_NIL
   ? MEMVAR->INFORMACAO_OLD_NAO_PODE_CONTER_NIL
ENDIF
IF X_Info_New==NIL .AND. .NOT. L_PODE_NIL
   ? MEMVAR->INFORMACAO_NEW_NAO_PODE_CONTER_NIL
ENDIF
*
IF .NOT. X_Info_Old == X_Info_New
   IF C_TYPEVAR=="C"
      IF X_Info_Old==NIL
         C_Info_Old := "NIL"
      ELSE
         C_Info_Old := X_Info_Old
      ENDIF
      IF X_Info_New==NIL
         C_Info_New := "NIL"
      ELSE
         C_Info_New := X_Info_New
      ENDIF
   ELSEIF C_TYPEVAR=="N"
      IF X_Info_Old==NIL
         C_Info_Old := "NIL"
      ELSE
         C_Info_Old := LTRIM(STR(X_Info_Old))
      ENDIF
      IF X_Info_New==NIL
         C_Info_New := "NIL"
      ELSE
         C_Info_New := LTRIM(STR(X_Info_New))
      ENDIF
   ELSEIF C_TYPEVAR=="L"
      IF X_Info_Old==NIL
         C_Info_Old := "NIL"
      ELSE
         C_Info_Old := IIF(X_Info_Old,".T.",".F.")
      ENDIF
      IF X_Info_New==NIL
         C_Info_New := "NIL"
      ELSE
         C_Info_New := IIF(X_Info_New,".T.",".F.")
      ENDIF
   ELSE
      ? MEMVAR->ERRO_NA_DEFINICAO_DO_TYPEVAR
   ENDIF
   *
   IF SELECT("XXCONG") # 0
      L_LOGAR    := .NOT. XXCONG->SERIE $ "01504/"
      L_CANCELAR := XXCONG->SERIE $ "01504/" // +;
                                    // "02808/02809/02824/02983/03296/03306/"
   ELSE
      L_LOGAR    := .T.
      L_CANCELAR := .T.
   ENDIF
   *
   IF L_LOGAR
      LOGAFONT_AMBIENTE("AMB",;   // Log de AMBIENTE
                        ALIAS(),C_CDBOTAO,;
                        "Erro 40: "+C_Aspecto+":"+;
                            LEFT(C_Info_Old,34)+"/"+LEFT(C_Info_New,34))
   ENDIF
   IF L_CANCELAR
      * Usou-se a MsgAguarde() ao invés da ALARME() para que o conteúdo de C_MENSLOGA
      * venha contido dentro do LOG de erro.
      V_JAN_MENS := MsgAguarde("M23017",,"Erro de modificação de ambiente;"+;
                               "Botão/Ação/Imagem: "+C_CDBOTAO+";"+;
                               "Aspecto: "+C_Aspecto+";"+;
                               "Antes: "+LEFT(C_Info_Old,90)+";"+;
                               "Depois: "+LEFT(C_Info_New,90))
      ? MEMVAR->ERRO_DE_MODIFICACAO_DE_AMBIENTE
      FECHARMSGAGUARDE(V_JAN_MENS)
   ENDIF
ENDIF
*
************* FIM
