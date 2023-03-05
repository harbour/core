/* encoding: cp850 */

#pragma DebugInfo=On

/*

                               Aspec - Informatica
                           Direitos Autorais Reservado

   CUA - Objeto janela

*/
*
MEMVAR INFO_VERSAO
STATIC V_PilhaJanelas := {}
STATIC L_GuiObjectsAtivos := .T.
*
#INCLUDE "inkey.ch"
#INCLUDE "setcurs.ch"
#INCLUDE "color.ch"
#INCLUDE "common.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "set.ch"
#INCLUDE "recursos.ch"
#INCLUDE "mousecua.ch"
#INCLUDE "cua.ch"
*
********************
// N_LinIni --> Top
// N_ColIni --> Left
// N_LinFin --> Bottom
// N_ColFin --> Right
// C_Cabec --> Title
FUNCTION CriarJanela ( N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec, ;
                       VC_TxtBotoes_10, C_CdTela, V_Janela_Pai, C_SubCabec,;
                       N_EspacamentoEmPixels, N_DeslocaCabecalho,;
                       L_CUA_10)  // parâmetro novo na CUA 2.0
*
LOCAL N_Lin1Livre, N_Col1Livre, N_Lin2Livre, N_Col2Livre
LOCAL VC_Titulo, C_CorInten
LOCAL L_DesenhaBox, N_MargemSuperior, N_MargemDemais
LOCAL L_Embutida := (V_Janela_Pai # NIL)
LOCAL L_MainCoord_Atu
LOCAL V_RegiaoBotoes := {}, N_CT
LOCAL B_Metodo
*
DEFAULT C_Cabec       TO ""
DEFAULT VC_TxtBotoes_10  TO {}
DEFAULT L_Embutida    TO .F.
DEFAULT C_SubCabec    TO ""
DEFAULT L_CUA_10      TO .T.
*

LOG_PRINT("CRIAR JANELA N_LinIni:" + hb_ntos(N_LinIni))
IF .NOT. LEFT(C_CdTela,1) == "T"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdTela,"T0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdTela)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
* Logar uso da janela, para ter estatística de uso
LOGAINFO_ID_TELA_RELAT_BOTAO("tela",C_CDTELA,NIL,NIL)   // Log de uso de tela no sistema
*
IF L_CUA_10
   FOR N_CT := 1 TO LEN(VC_TxtBotoes_10)
       AADD(V_RegiaoBotoes,{NIL,;                     // _BOTAO_LIN_INICIAL
                            NIL,;                     // _BOTAO_COL_INICIAL
                            NIL,;                     // _BOTAO_LIN_FINAL
                            NIL,;                     // _BOTAO_COL_FINAL
                            VC_TxtBotoes_10[N_CT],;   // _BOTAO_TEXTO_COMANDO
                            NIL,;                     // _BOTAO_TEXTO_TRATADO_1
                            NIL,;                     // _BOTAO_TEXTO_TRATADO_2
                            NIL,;                     // _BOTAO_COL_DESTAQUE
                            NIL,;                     // _BOTAO_TEXTO_DESTAQUE
                            {||NIL},;                 // _BOTAO_BLOCO_ACAO
                            .F.,;                     // _BOTAO_AUTOCLOSE
                            NIL,;                     // _BOTAO_CDBOTAO
                            NIL,;                     // _BOTAO_ALIAS_MUDA
                            NIL,;                     // _BOTAO_RECNO_MUDA
                            NIL,;                     // _BOTAO_FILTER_MUDA
                            NIL,;                     // _BOTAO_ORDER_MUDA
                            NIL,;                     // _BOTAO_EOFOK
                            NIL,;                     // _BOTAO_HANDLE_MUDA
                            NIL,;                     // _BOTAO_INKEY_DESTAQUE
                            NIL,;                     // _BOTAO_INKEY_DESTAQUE_CASE
                            NIL,;                     // _BOTAO_HANDLE_PUSHBUTTON
                            NIL})                     // _BOTAO_MUDADADOS
   NEXT
ELSE
   IF LEN(VC_TxtBotoes_10) # 0
      ? MEMVAR->CUA_20_NAO_TEM_CLAUSULA_TECLAS
   ENDIF
ENDIF
*
IF SOB_MODO_GRAFICO()
    DEFAULT N_EspacamentoEmPixels TO WVW_SetDefLineSpacing()
ELSE
    DEFAULT N_EspacamentoEmPixels TO 4   // só usado na GTWVW
ENDIF
IF N_EspacamentoEmPixels # 0 .AND. ;    // controles já devidamente adaptados
   N_EspacamentoEmPixels # 4            // para estes 2 espaçamentos
   ? MEMVAR->ESPACAMENTO_INVALIDO
ENDIF
*
IF N_EspacamentoEmPixels # 4
   IF N_LinIni # 0 .OR. ;
      N_ColIni # 0 .OR. ;
      N_LinFin # MAXROW()
      * A chamada da WVW_AddRows() supõe que toda a área vertical
      * da tela está sendo utilizada, senão as linhas adicionadas
      * poderiam ficar "debaixo" da taskbar do windows.
      *
      * A coluna inicial também tem de ser zero, senão dá problema
      * no refresh da tela. A coluna final não precisa ser MAXCOL().
      //? MEMVAR->ESPACAMENTO_OPCIONAL_USA_SEMPRE_LIMITES_MAXIMOS
   ENDIF
ENDIF
*
DEFAULT N_DeslocaCabecalho TO 0
*
* A tela inicial do sistema possui tamanho 35rows/110cols. Caso se especifique
* o espaçamento 0, cabe mais linhas na tela. Mas as rotinas internas da
* GTWVW somente imprimem além do MAXROW() (que é sempre a da janela inicial
* do sistema) se o MainCoord for .F.
* Desta forma, é necessário salvar este status de cada janela.
*
L_MainCoord_Atu := (N_EspacamentoEmPixels # 0)
*
IF ";" $ C_CABEC
   IF SOB_MODO_GRAFICO()
      ALARME("M28746","TITULO com mais de uma linha")
      ? MEMVAR->ERRO_CABECALHO_1
   ELSE
      ALERT("TITULO com mais de uma linha")
      ? MEMVAR->ERRO_CABECALHO_1
   ENDIF
ENDIF
C_SubCabec := STRTRAN(C_SubCabec,"%t",C_Cabec)
C_SubCabec := STRTRAN(C_SubCabec,"%T",C_Cabec)
*
IF EMPTY(C_SubCabec) .AND. .NOT. EMPTY(C_Cabec)
   IF SOB_MODO_GRAFICO()
      ALARME("M28748","TITULO preenchido mas SUBTITULO vazio")
      ? MEMVAR->ERRO_CABECALHO_2
   ELSE
      ALERT("TITULO preenchido mas SUBTITULO vazio")
     ? MEMVAR->ERRO_CABECALHO_2
   ENDIF
ENDIF
*
IF N_LinIni == 0 .AND. N_ColIni == 0 .AND. ;
   N_LinFin == MAXROW() .AND. N_ColFin == MAXCOL()
   L_DesenhaBox     := .F.
   N_MargemSuperior := 0
   N_MargemDemais   := 0
ELSE
   L_DesenhaBox     := .T.
   N_MargemSuperior := 1
   N_MargemDemais   := 1
ENDIF
*
N_MargemDemais++   // aumentar margens para fins puramente estéticos
*
IF SOB_MODO_GRAFICO() .AND. .NOT. L_Embutida
   IF L_DesenhaBox
      * Quem desenha o box é a própria WVW, de forma automática.
      *
      L_DesenhaBox := .F.
      *
      * Deduzir da janela o espaço não utilizado pelo box,
      * para que a aparência fique idêntica entre a GTWIN e a GTWVW.
      *
        N_LinIni++
        N_ColIni++
        N_LinFin--
        N_ColFin--
      *
      N_MargemSuperior--
      N_MargemDemais--
   ENDIF
   *
ENDIF
*
N_Col1Livre := N_ColIni + N_MargemDemais      // coordenadas das colunas livres
N_Col2Livre := N_ColFin - N_MargemDemais
*
* não imprime nada, só monta a matriz de cabeçalho devidamente formatado
*
VC_Titulo := StrToVet_(C_SubCabec)
*
* coordenadas das linhas livres
N_Lin1Livre := N_LinIni + N_MargemSuperior + LEN(VC_Titulo) + 1
N_Lin2Livre := N_LinFin - N_MargemDemais   - 1 // ainda será deduzida as teclas de função na TecFunc()
*
* A cor abaixo é utilizada somente em SAY"s, sendo de uso interno do sistema.
* Por este motivo não precisa ser completa
C_CorInten := _Pega_Cor_(SETCOLOR(),CLR_STANDARD,.T.) + "+" + "/" + ;
              _Pega_Cor_(SETCOLOR(),CLR_STANDARD,.F.)
IF "++" $ C_CorInten
   C_CorInten := STRTRAN(C_CorInten,"++","+")
ENDIF
*
IF L_CUA_10
    // @ 22, 0 SAY ""
   //OutStd("B_Metodo NIL in Janela")

   B_Metodo := {||NIL}   // não faz nada
ELSE
   * trata eventuais botões, ações e imagens, mesmo que janela não tenha especialização
   //OutStd("B_Metodo TrataEventos in Janela")
   B_Metodo := {|VX_Janela2| TrataEventos(VX_Janela2) }
ENDIF
*
#DEFINE C_CorJan     SETCOLOR()   // cor da janela (é a da criação)
#DEFINE C_TelaCoberta  NIL        // preenchida na abertura da janela
#DEFINE N_LinAnt     NIL          // (linha do cursor antes da ativação) - na ativação
#DEFINE N_ColAnt     NIL          // (coluna do cursor antes da ativação) - na ativação
#DEFINE N_LinBotoes  NIL          // (linhas ocupadas pelos botões de opções) - na ativação
#DEFINE N_TP_Jan     NIL          // indica o tipo de especializaçao da janela
#DEFINE VX_SubObj    NIL          // contém o subobjeto na qual foi especializada
#DEFINE N_LinMess    NIL          // linha de mensagens - na especialização
#DEFINE N_WindowNum  NIL          // Posicao da janela na pilha de janelas
#DEFINE aGuiObjects  {}
#DEFINE V_BotoesToolBar {}
#DEFINE L_TemScrollVertical   .F.
#DEFINE L_TemScrollHorizontal .F.
#DEFINE L_RolaCima         .F.
#DEFINE L_RolaBaixo        .F.
#DEFINE L_RolaEsquerda     .F.
#DEFINE L_RolaDireita      .F.
#DEFINE N_LinMarcadorVertical   0
#DEFINE N_ColMarcadorHorizontal 0
#DEFINE L_CriarToolBar     .F.
#DEFINE L_MainCoord_Ant NIL
#DEFINE V_LstImagens    {}
#DEFINE L_JanTipoMsgAguarde .F.
#DEFINE V_LstAcoes  {}
#DEFINE N_IdProgressBar1 NIL // #DEFINE POSICAO_45_A_REUTILIZAR  NIL
#DEFINE N_IdProgressBar2 NIL // #DEFINE POSICAO_48_A_REUTILIZAR  NIL
#DEFINE B_SetInkeyAfterBlock_Old NIL
#DEFINE N_ToolBarCodigoAcao  400   // Convencionou-se que iniciava com 400
#DEFINE N_IdScrollBarVertical   NIL // Identificador da ScrollBar Vertical
#DEFINE N_IdScrollBarHorizontal NIL // Identificador da ScrollBar Horizontal
#DEFINE B_ScrollBarVertical     Nil // Bloco de código que especializa a Scroll Vertical
#DEFINE B_ScrollBarHorizontal   Nil // Bloco de código que especializa a Scroll Horizontal
#DEFINE N_ProgressBar           Nil // Guarda a quantidade de ProgressBar. Observação: Para janela do tipo MsgAguarde
*
RETURN { N_LinIni , N_ColIni , N_LinFin , N_ColFin , ;
         C_TelaCoberta , N_LinAnt , N_ColAnt , VC_Titulo , N_LinBotoes , ;
         N_Lin1Livre , N_Col1Livre , N_Lin2Livre , N_Col2Livre , ;
         N_LinMess, C_CorJan , C_CorInten, C_CdTela,;
         N_TP_Jan, VX_SubObj, B_Metodo, V_RegiaoBotoes,;
         N_WindowNum, aGuiObjects, N_MargemSuperior, L_DesenhaBox, L_Embutida,;
         V_BotoesToolBar, L_TemScrollVertical, L_TemScrollHorizontal,C_Cabec,;
         L_RolaCima, L_RolaBaixo, L_RolaEsquerda, L_RolaDireita,;
         N_LinMarcadorVertical, N_ColMarcadorHorizontal,;
         L_CriarToolBar, V_Janela_Pai, N_EspacamentoEmPixels, L_MainCoord_Atu,;
         L_MainCoord_Ant, V_LstImagens, N_DeslocaCabecalho, L_JanTipoMsgAguarde,;
         N_IdProgressBar1, V_LstAcoes, L_CUA_10, N_IdProgressBar2, ;
         B_SetInkeyAfterBlock_Old,N_ToolBarCodigoAcao,;
         N_IdScrollBarVertical,N_IdScrollBarHorizontal,B_ScrollBarVertical,B_ScrollBarHorizontal,;
         N_ProgressBar}
#UNDEF C_CorJan
#UNDEF C_TelaCoberta
#UNDEF N_LinAnt
#UNDEF N_ColAnt
#UNDEF N_LinBotoes
#UNDEF N_TP_Jan
#UNDEF VX_SubObj
#UNDEF N_LinMess
#UNDEF N_WindowNum
#UNDEF aGuiObjects
#UNDEF V_BotoesToolBar
#UNDEF L_TemScrollVertical
#UNDEF L_TemScrollHorizontal
#UNDEF L_RolaCima
#UNDEF L_RolaBaixo
#UNDEF L_RolaEsquerda
#UNDEF L_RolaDireita
#UNDEF N_LinMarcadorVertical
#UNDEF N_ColMarcadorHorizontal
#UNDEF L_CriarToolBar
#UNDEF L_MainCoord_Ant
#UNDEF V_LstImagens
#UNDEF L_JanTipoMsgAguarde
#UNDEF V_LstAcoes
#UNDEF N_IdProgressBar1 // #UNDEF POSICAO_45_A_REUTILIZAR
#UNDEF N_IdProgressBar2 // #UNDEF POSICAO_48_A_REUTILIZAR
#UNDEF B_SetInkeyAfterBlock_Old
#UNDEF N_ToolBarCodigoAcao
#UNDEF N_IdScrollBarVertical
#UNDEF N_IdScrollBarHorizontal
#UNDEF B_ScrollBarVertical
#UNDEF B_ScrollBarHorizontal
#UNDEF N_ProgressBar
*
******************
FUNC CriarJanela20(N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec,;
                   C_SubCabec,C_CdTela,X_PARAMETRO_A_REUTILIZAR,N_DeslocaCabecalho,;
                   N_EspacamentoEmPixels,V_Janela_Pai)
******************
LOCAL VX_Janela
LOCAL L_CUA_10  := .F.
LOCAL VC_TxtBotoes_10 := {}
*
VX_Janela := CriarJanela ( N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec, ;
                           VC_TxtBotoes_10, C_CdTela, V_Janela_Pai, C_SubCabec,;
                           N_EspacamentoEmPixels, N_DeslocaCabecalho,;
                           L_CUA_10)  // parâmetro novo na CUA 2.0
*
RETURN VX_Janela
*
* DEFINICOES PARA USO GERAL
*
#DEFINE N_LinIni       VX_Janela[01]
#DEFINE N_ColIni       VX_Janela[02]
#DEFINE N_LinFin       VX_Janela[03]
#DEFINE N_ColFin       VX_Janela[04]
#DEFINE C_Cabec        VX_Janela[30]





// Still NON Used

#DEFINE C_TelaCoberta  VX_Janela[05]
#DEFINE N_LinAnt       VX_Janela[06]
#DEFINE N_ColAnt       VX_Janela[07]
#DEFINE VC_Titulo      VX_Janela[08]
#DEFINE N_LinBotoes    VX_Janela[09]
#DEFINE N_Lin1Livre    VX_Janela[10]        // usado também no JANELA.CH
#DEFINE N_Col1Livre    VX_Janela[11]        // idem
#DEFINE N_Lin2Livre    VX_Janela[12]        // idem
#DEFINE N_Col2Livre    VX_Janela[13]        // idem
#DEFINE N_LinMess      VX_Janela[14]        // idem
#DEFINE C_CorJan       VX_Janela[15]        // usado também no JANELA.CH
#DEFINE C_CorInten     VX_Janela[16]        // idem
#DEFINE C_CdTela        VX_Janela[17]        // idem
#DEFINE N_TP_Jan       VX_Janela[18]        // preenchido nos sub-objetos (usado no JANELA.CH)
#DEFINE VX_SubObj      VX_Janela[19]        // idem
#DEFINE B_Metodo       VX_Janela[20]        // tem default, mas pode ser sobreposto nos sub-objetos
#DEFINE V_RegiaoBotoes VX_Janela[21]        // dados sobre os botões de função
#DEFINE N_WindowNum    VX_Janela[22]
#DEFINE aGuiObjects    VX_Janela[23]
#DEFINE N_MargemSuperior VX_Janela[24]
#DEFINE L_DesenhaBox   VX_Janela[25]
#DEFINE L_Embutida     VX_Janela[26]
#DEFINE V_BotoesToolBar    VX_Janela[27]
#DEFINE L_TemScrollVertical   VX_Janela[28]
#DEFINE L_TemScrollHorizontal VX_Janela[29]
#DEFINE L_RolaCima         VX_Janela[31]
#DEFINE L_RolaBaixo        VX_Janela[32]
#DEFINE L_RolaEsquerda     VX_Janela[33]
#DEFINE L_RolaDireita      VX_Janela[34]
#DEFINE N_LinMarcadorVertical    VX_Janela[35]
#DEFINE N_ColMarcadorHorizontal  VX_Janela[36]
#DEFINE L_CriarToolBar     VX_Janela[37]
#DEFINE V_Janela_Pai       VX_Janela[38]
#DEFINE N_EspacamentoEmPixels VX_Janela[39]
#DEFINE L_MainCoord_Atu       VX_Janela[40]
#DEFINE L_MainCoord_Ant       VX_Janela[41]
#DEFINE V_LstImagens          VX_Janela[42]
#DEFINE N_DeslocaCabecalho    VX_Janela[43]
#DEFINE L_JanTipoMsgAguarde   VX_Janela[44]
#DEFINE N_IdProgressBar1      VX_Janela[45]
#DEFINE V_LstAcoes            VX_Janela[46]
#DEFINE L_CUA_10              VX_Janela[47]
#DEFINE N_IdProgressBar2      VX_Janela[48]
#DEFINE B_SetInkeyAfterBlock_Old VX_Janela[49]
#DEFINE N_ToolBarCodigoAcao   VX_Janela[50]
#DEFINE N_IdScrollBarVertical   VX_Janela[51]
#DEFINE N_IdScrollBarHorizontal VX_Janela[52]
#DEFINE B_ScrollBarVertical     VX_Janela[53]
#DEFINE B_ScrollBarHorizontal   VX_Janela[54]
#DEFINE N_ProgressBar VX_Janela[55]

*
*************
PROC AddBotao (VX_Janela,C_TxtBotao,B_AcaoBotao,L_AutoClose,;
               C_CdBotao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda,L_EofOk, L_HandleMuda,;
               L_MudaDados,L_BotaoAutomatico)
*************
DEFAULT L_AutoClose       TO .F.
DEFAULT L_BotaoAutomatico TO .F.
DEFAULT L_MudaDados       TO .F.
*
*
IF L_CUA_10
   ? MEMVAR->ADDBOTAO_EXCLUSIVO_DA_CUA_20
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDBOTAO_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
AADD(V_RegiaoBotoes,{NIL,;          // _BOTAO_LIN_INICIAL
                     NIL,;          // _BOTAO_COL_INICIAL
                     NIL,;          // _BOTAO_LIN_FINAL
                     NIL,;          // _BOTAO_COL_FINAL
                     C_TxtBotao,;   // _BOTAO_TEXTO_COMANDO
                     NIL,;          // _BOTAO_TEXTO_TRATADO_1
                     NIL,;          // _BOTAO_TEXTO_TRATADO_2
                     NIL,;          // _BOTAO_COL_DESTAQUE
                     NIL,;          // _BOTAO_TEXTO_DESTAQUE
                     B_AcaoBotao,;  // _BOTAO_BLOCO_ACAO
                     L_AutoClose,;  // _BOTAO_AUTOCLOSE
                     C_CdBotao,;    // _BOTAO_CDBOTAO
                     L_AliasMuda,;  // _BOTAO_ALIAS_MUDA
                     L_RecnoMuda,;  // _BOTAO_RECNO_MUDA
                     L_FilterMuda,; // _BOTAO_FILTER_MUDA
                     L_OrderMuda,;  // _BOTAO_ORDER_MUDA
                     L_EofOk,;      // _BOTAO_EOFOK
                     L_HandleMuda,; // _BOTAO_HANDLE_MUDA
                     NIL,;          // _BOTAO_INKEY_DESTAQUE
                     NIL,;          // _BOTAO_INKEY_DESTAQUE_CASE
                     NIL,;          // _BOTAO_HANDLE_PUSHBUTTON
                     L_MudaDados})  // _BOTAO_MUDADADOS

*
***************
PROC AddImagem (VX_Janela,C_ArquivoImagem,;
                N_LinIniImagem,N_ColIniImagem,N_LinFinImagem,N_ColFinImagem,;
                N_Keyboard,C_CdBotao)
***************
*
IF .NOT. L_CUA_10
   ? MEMVAR->ADDIMAGEM_ESPECIFICA_PARA_A_CUA_10
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDIMAGEM_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
AADD(V_LstImagens,{C_ArquivoImagem,;  // _IMAGEM_ARQUIVO
                   N_LinIniImagem,;   // _IMAGEM_LIN_INICIAL
                   N_ColIniImagem,;   // _IMAGEM_COL_INICIAL
                   N_LinFinImagem,;   // _IMAGEM_LIN_FINAL
                   N_ColFinImagem,;   // _IMAGEM_COL_FINAL
                   NIL,;              // _IMAGEM_BLOCO_ACAO
                   .F.,;              // _IMAGEM_AUTOCLOSE
                   C_CdBotao,;        // _IMAGEM_CDBOTAO
                   NIL,;              // _IMAGEM_ALIAS_MUDA
                   NIL,;              // _IMAGEM_RECNO_MUDA
                   NIL,;              // _IMAGEM_FILTER_MUDA
                   NIL,;              // _IMAGEM_ORDER_MUDA
                   NIL,;              // _IMAGEM_EOFOK
                   NIL,;              // _IMAGEM_HANDLE_MUDA
                   N_Keyboard,;       // _IMAGEM_KEYBOARD
                   InKeyCaseAlternativo(N_Keyboard)})  // _IMAGEM_KEYBOARD_CASE
*
****************
PROC AddImagem20 (VX_Janela,C_ArquivoImagem,;
                  N_LinIniImagem,N_ColIniImagem,N_LinFinImagem,N_ColFinImagem,;
                  B_AcaoImagem,L_AutoClose,;
                  C_CdBotao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_EofOk, L_HandleMuda, L_MudaDados)
****************
*
DEFAULT L_MudaDados TO .F.
*
IF L_CUA_10
   ? MEMVAR->ADDIMAGEM_ESPECIFICA_PARA_A_CUA_20
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDIMAGEM_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
IF INABILITA_IMAGEM(L_MudaDados)
   B_AcaoImagem := {||.F.}  // Inabilitar a ação da imagem.
ENDIF
*
AADD(V_LstImagens,{C_ArquivoImagem,;  // _IMAGEM_ARQUIVO
                   N_LinIniImagem,;   // _IMAGEM_LIN_INICIAL
                   N_ColIniImagem,;   // _IMAGEM_COL_INICIAL
                   N_LinFinImagem,;   // _IMAGEM_LIN_FINAL
                   N_ColFinImagem,;   // _IMAGEM_COL_FINAL
                   B_AcaoImagem,;     // _IMAGEM_BLOCO_ACAO
                   L_AutoClose,;      // _IMAGEM_AUTOCLOSE
                   C_CdBotao,;        // _IMAGEM_CDBOTAO
                   L_AliasMuda,;      // _IMAGEM_ALIAS_MUDA
                   L_RecnoMuda,;      // _IMAGEM_RECNO_MUDA
                   L_FilterMuda,;     // _IMAGEM_FILTER_MUDA
                   L_OrderMuda,;      // _IMAGEM_ORDER_MUDA
                   L_EofOk,;          // _IMAGEM_EOFOK
                   L_HandleMuda,;     // _IMAGEM_HANDLE_MUDA
                   NIL,;              // _IMAGEM_KEYBOARD
                   NIL,;              // _IMAGEM_KEYBOARD_CASE
                   L_MudaDados})      // _IMAGEM_MUDADADOS
*
************
PROC AddAcao (VX_Janela,N_Keyboard,B_AcaoBotao,L_AutoClose,;
              C_CdBotao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_EofOk, L_HandleMuda, L_MudaDados)
************
*
DEFAULT L_MudaDados TO .F.
*
* A função AddAcao sé chamada pelo COMANDO ADDCAO ou pela função AJUSTA_BOTOES()
*
IF L_CUA_10
   ? MEMVAR->ADDACAO_ESPECIFICA_PARA_A_CUA_20
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDACAO_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
IF INABILITA_ADDACAO(L_MudaDados)
    B_AcaoBotao := {||.F.}  // Inabilitar a ação do comando ADDACAO.
 ENDIF
 *
AADD(V_LstAcoes,{N_Keyboard,;       // _ACAO_KEYBOARD
                 InKeyCaseAlternativo(N_Keyboard),;  // _ACAO_KEYBOARD_CASE
                 B_AcaoBotao,;      // _ACAO_BLOCO_ACAO
                 L_AutoClose,;      // _ACAO_AUTOCLOSE
                 C_CdBotao,;        // _ACAO_CDBOTAO
                 L_AliasMuda,;      // _ACAO_ALIAS_MUDA
                 L_RecnoMuda,;      // _ACAO_RECNO_MUDA
                 L_FilterMuda,;     // _ACAO_FILTER_MUDA
                 L_OrderMuda,;      // _ACAO_ORDER_MUDA
                 L_EofOk,;          // _ACAO_EOFOK
                 L_HandleMuda,;     // _ACAO_HANDLE_MUDA
                 L_MudaDados})      // _ACAO_MUDADADOS
*
******************************
STAT FUNC InKeyCaseAlternativo (N_Keyboard)
******************************
LOCAL N_Keyboard_Case
LOCAL C_Char_Equiv
IF N_KeyBoard # NIL
   * Tornar o N_Keyboard "case insensitive"
   C_Char_Equiv := CHR(N_Keyboard)
   IF C_Char_Equiv >= "a" .AND. C_Char_Equiv <= "z"
      N_Keyboard_Case := ASC(UPPER(C_Char_Equiv))
   ELSEIF C_Char_Equiv >= "A" .AND. C_Char_Equiv <= "Z"
      N_Keyboard_Case := ASC(LOWER(C_Char_Equiv))
   ELSE
      N_Keyboard_Case := N_Keyboard  // não é letra
   ENDIF
ENDIF
RETURN N_Keyboard_Case
*
**************
FUNCTION Ative ( VX_Janela )
**************
LOCAL N_Largura, N_LinImp, X_Retorno
LOCAL N_CursorAnt := SET(_SET_CURSOR,SC_NONE)        // salvar modo do cursor
LOCAL C_CorAnt    := SETCOLOR(C_CorJan)              // salvar cor anterior
LOCAL B_Ajuda_Ant  // salvar help anterior, se existir novo
LOCAL N_Cont, N_AddRows
LOCAL C_Cabec_Aux
LOCAL L_AcrescentarSeparadorSubtitulo, L_MostraGrade
LOCAL L_AutoClose := .F.

// Window flags
LOCAL L_CLOSE_WITH_RETURN := .F.
LOCAL L_CLOSE_WITH_ESC := .F.
LOCAL L_MINIMIZE_BUTTON := .F.



// FRAN: A NAppGUI/GTNAP application owns the event cicle.
// The hotkey should be asigned when Window is created after NAP_CUALIB_WINDOW()
IF .NOT. SOB_MODO_GRAFICO()
    B_Ajuda_Ant := SETKEY(K_F1,{||XXHELP(C_CdTela,C_Cabec,NIL,NIL)})
ENDIF

*
IF N_LinBotoes == NIL
// FRAN-TODO Review segmentation fault
//   ? MEMVAR->AJUSTA_BOTOES_DEVE_SER_CHAMADA_UMA_VEZ
ENDIF
*

IF C_TelaCoberta == NIL    // se janela ainda não foi aberta, abrí-la
    *
    N_LinAnt    := ROW()
    N_ColAnt    := COL()
    *
    IF SOB_MODO_GRAFICO()
        * A restauração do trecho da tela anterior que vai ser coberta
        * pela tela atual é feita automaticamente pelo Windows.

        C_TelaCoberta := ""

        // Window is embedded?
        IF L_Embutida
            #DEFINE N_WindowNum_Pai V_Janela_Pai[22]
            N_WindowNum := N_WindowNum_Pai   // vincular com a janela pai
            *
            * espaçamento da janela filha deve ser idêntico à da janela pai
            IF N_EspacamentoEmPixels # WVW_SetLineSpacing(N_WindowNum_Pai)
               ? MEMVAR->ERRO_DE_ESPACAMENTO_DE_JANELA_EMBUTIDA
            ENDIF

        ELSE // L_Embutida  --> Window in not embedded
            L_MainCoord_Ant := WvW_SetMainCoord()
            AADD(V_PilhaJanelas,{LEN(V_PilhaJanelas),VX_Janela})

            // FRAN: GTNAP doesn't create any window by default
            // IF LEN(V_PilhaJanelas)==1
            //     * espaçamento da janela principal deve ser idêntico
            //     * ao espaçamento default da aplicação
            //     IF N_EspacamentoEmPixels # WVW_SetDefLineSpacing()
            //        ? MEMVAR->ERRO_DE_ESPACAMENTO_DE_JANELA_PRINCIPAL
            //     ENDIF
            // ELSE // LEN(V_PilhaJanelas) != 1

            IF LEN(V_PilhaJanelas)==1
                // The first window takes the title from 'Setup_nap' (compatible with GTWVW/Cualib)
                C_Cabec_Aux := NIL
            ELSE
                //OutStd("Version: " + Version())
                IF Version()=="Harbour 3.2.0dev (r1703241902)"
                    IF CABEC_TESTE_AUTOMATICO()
                        C_Cabec_Aux := StrTran(C_Cabec," ","_")
                        C_Cabec_Aux := subs(Tiracen(C_Cabec_Aux),1,15)+"_"+C_CdTela+"_"
                    ELSE
                        C_Cabec_Aux := HB_OEMtoANSI(C_Cabec)
                    ENDIF
                ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // PENDENTE_LINUX
                    IF CABEC_TESTE_AUTOMATICO()
                        C_Cabec_Aux := StrTran(C_Cabec," ","_")
                        C_Cabec_Aux := subs(Tiracen(C_Cabec_Aux),1,15)+"_"+C_CdTela+"_"
                    ELSE
                        C_Cabec_Aux := C_Cabec
                    ENDIF
                ELSE
                    // FRAN: Harbour 3.2.0dev (r2104281802)
                    C_Cabec_Aux := C_Cabec
                ENDIF //ERRO // IF Version()
            *
            ENDIF

            //NAP_LOG("AQUI ATIVE!!! " + hb_ntos(N_LinIni) + ":" + hb_ntos(N_LinFin) + ":" + hb_ntos(N_ColFin))

            // IF .NOT. (N_LinIni == 0 .AND. N_LinFin == MAXROW() .AND. ;
            //     N_ColIni == 0)
            //     //
            //     // FRAN
            //     //
            //     // The main window ends at MaxRow()-1, MaxCol()-1. Child windows end in N_LinFin, N_ColFin
            //     N_LinIni--
            //     N_ColIni--
            //     N_LinFin++
            //     N_ColFin++
            // ENDIF

            // //
            // // FRAN: Perhaps this block can be avoided
            // //
            // IF N_EspacamentoEmPixels # WVW_SetDefLineSpacing()
            //     *
            //     IF N_EspacamentoEmPixels > WVW_SetDefLineSpacing()
            //         * Código a seguir supõe que o espaçamento default é sempre
            //         * maior que o espaçamento opcional, ou seja, que a
            //         * WVW_AddRows() acrescentará linhas.
            //         ? MEMVAR->ESPACAMENTO_OPCIONAL_EH_SEMPRE_ZERO
            //     ENDIF
            //     *
            //     IF N_LinIni == 1 .AND. N_LinFin == MAXROW()-1 .AND. ;
            //         N_ColIni == 1
            //         * A janela foi criada com parâmetros (00,MAXROW(),00,xx),
            //         * mas, pelo fato de não ser direita MAXCOL(),
            //         * a CriaJanela() acrescentou o espaço do BOX, para ficar
            //         * posicionalmente idêntico à versão texto.
            //         * Como, quando o ESPACOPIXELS for 0, não se quer que seja
            //         * igual à versão texto, desfazer este ajuste automático.
            //         N_LinIni--
            //         N_ColIni--
            //         N_LinFin++
            //         N_ColFin++
            //     ENDIF
            // *
            // ENDIF // N_EspacamentoEmPixels # WVW_SetDefLineSpacing()
            //
            //
            //
            *
            // ENDIF // LEN(V_PilhaJanelas)==1

            //
            // FRAN: GTNAP don't open any window automatically
            //
            // IF LEN(V_PilhaJanelas)==1 .AND. EH_PRODUCAO() Avoided
            IF L_JanTipoMsgAguarde
                IF N_ProgressBar == 1
                    N_LinFin := N_LinFin + 2
                ELSEIF N_ProgressBar == 2
                    N_LinFin := N_LinFin + 4
                Endif
            ENDIF

            // Windows flags based on specializations
            IF N_TP_Jan == NIL
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .T.

            ELSEIF N_TP_Jan == _JAN_TEXTO_10
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_ARQTEXTO_10
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_ENTRADA_10
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_MENU_VERT
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_SELE_VETO_20
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_SELE_ARQ_20
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ENDIF

            IF LEN(V_PilhaJanelas)==1
                L_MINIMIZE_BUTTON := .T.
            ELSE
                L_MINIMIZE_BUTTON := .F.
            ENDIF

            N_WindowNum := NAP_CUALIB_WINDOW(N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec_Aux, L_CLOSE_WITH_RETURN, L_CLOSE_WITH_ESC, L_MINIMIZE_BUTTON)

            NAP_CUALIB_HOTKEY(K_F1,{||XXHELP(C_CdTela,C_Cabec,NIL,NIL)}, .F.)


            // IF N_TP_Jan == _JAN_SELE_ARQ_20
            //     NAP_LOG("AQUI!!!!! _JAN_SELE_ARQ_20")
            // ENDIF

            //#DEFINE VX_Sele  VX_SubObj
            // OJO!!! FRAN PROGRAM CRASH IF 'VX_SubObj:CARGO[07]'
            //L_AutoClose  := VX_SubObj:CARGO[07]      // se é para fechar a janela automaticamente (cua 2.0)
            L_AutoClose  := .T.
            //#UNDEF VX_Sele

            // IF L_AutoClose == .T.
            //     NAP_LOG("AQUI!!!!! L_AutoClose == .T.")
            // ENDIF

            IF N_TP_Jan == _JAN_SELE_ARQ_20 .AND. L_AutoClose == .T.
                NAP_CUALIB_HOTKEY(K_ENTER,{||.T.}, .T.)
            ENDIF

            // N_WindowNum := WVW_nOpenWindow(C_Cabec_Aux,;
            //     N_LinIni,N_ColIni,N_LinFin,N_ColFin)
            //WvW_SetMainCoord(L_MainCoord_Atu)
            *

            // FRAN: At the moment this block is avoided
            // IF N_EspacamentoEmPixels # WVW_SetDefLineSpacing()
            //     *
            //     WVW_SetLineSpacing(N_WindowNum,N_EspacamentoEmPixels)
            //     N_AddRows := WVW_MaxMaxRow()-MAXROW()
            //     N_AddRows -= 1 // evitar que o final da tela fique fora do monitor
            //     IF L_CriarToolBar
            //        * 2 linhas deslocadas da linha inicial, para dar espaço à ToolBar
            //        N_AddRows -= 2
            //     ENDIF
            //     IF N_AddRows < 0
            //        * Como o espaçamento foi diminuído, com certeza é para
            //        * caber mais linhas na tela.
            //        ? MEMVAR->HOUVE_REDUCAO_DE_LINHAS_NA_TELA
            //     ENDIF
            //     *
            //     * Como o espaço entre linhas passou a ser 0, cabe mais linhas
            //     * na tela (mais que 35). Apesar disto, a função MAXROW()
            //     * continuará a retornar 35, pois é baseada na janela principal
            //     * do sistema.
            //     *
            //     * A WVW_AddRows(), quando MainCoord está setado, obedece ao
            //     * limite da MaxRow(). Portanto, o MainCoord deve ser
            //     * temporariamente desabilitado, para que a WVW_AddRows() possa
            //     * acrescentar linhas além do limite de 35 linhas.
            //     *
            //     * A quantidade de linhas possível de ser adicionada não é
            //     * criticada pela WVW_AddRows(), devendo-se passar um valor
            //     * compatível.
            //     *
            //     WVW_ADDROWS(N_WindowNum,N_AddRows)
            //     *
            //     * Mover os elementos inferiores da janela para as novas
            //     * coordenadas...
            //     *
            //     N_LinFin    := N_LinFin + N_AddRows
            //     N_Lin2Livre := N_Lin2Livre + N_AddRows
            //     N_LinMess   := N_LinMess + N_AddRows
            //     *
            // ENDIF  // N_EspacamentoEmPixels # WVW_SetDefLineSpacing()



            *
            // ENDIF // LEN(V_PilhaJanelas)==1 .AND. EH_PRODUCAO()


            * Ativou-se a rotina abaixo, mas não se viu nenhum efeito prático...
            //WVW_EnableShortCuts(N_WindowNum,.T.)      // FRAN Review HoyKeys in NAPWINDOW
            * ATENÇÃO: a sintaxe correta é:
            *    Wvw_SetIcon(janela,"path"+"arquivo de ícone.ico") ou
            *    Wvw_SetIcon(janela,"número do recurso","arquivo de ícone.ico")
            *
            // FRAN Window ICON can be simplified in GTNAP
            // IF Version()=="Harbour 3.2.0dev (r1703241902)"
            //     // Wvw_SetIcon(N_WindowNum, _ICONE_ASPEC)  FRAN ---> TODO Window ICON
            // ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // PENDENTE_LINUX
            //     * Nesta versão do compilador, a função Wvw_SetIcon() passa a utilizar a própria imagem.
            //     // IF FILE(DIREXE()+"aspec.ico")   FRAN ---> TODO Window ICON
            //     //     Wvw_SetIcon(N_WindowNum, DIREXE()+"aspec.ico")
            //     // ENDIF
            // ENDIF ERRO
        *
        ENDIF // L_Embutida

        IF N_WindowNum # LEN(V_PilhaJanelas)-IIF(EH_PRODUCAO(),1,0)
            ALARME("M28750","Alguma janela aberta não foi fechada - passo 1...")
            * ALERT("Alguma janela aberta não foi fechada - passo 1...")
        ENDIF

    ELSE // SOB_MODO_GRAFICO()

        C_TelaCoberta := SAVESCREEN(N_LinIni,N_ColIni,N_LinFin,N_ColFin)
        N_WindowNum := LEN(V_PilhaJanelas)

        IF .NOT. L_Embutida
            AADD(V_PilhaJanelas,{LEN(V_PilhaJanelas),VX_Janela})
        ENDIF

    ENDIF // SOB_MODO_GRAFICO()

    // FRAN: Here the code is compatible GTNAP/Text terminals GTXXX
    SCROLL(N_LinIni,N_ColIni,N_LinFin,N_ColFin)      // limpar área

    IF L_DesenhaBox
        IF SOB_MODO_GRAFICO() .AND. L_Embutida
            // FRAN REview and TODO
            AddGuiObject(V_Janela_Pai,{||DesenhaBoxEmbutida(VX_Janela)},;
                           {N_LinIni,N_ColIni,N_LinFin,N_ColFin})
        ELSE
           @ N_LinIni,N_ColIni TO N_LinFin,N_ColFin
        ENDIF
    ENDIF // L_DesenhaBox
    *

    // FRAN In GTWIN the text is not displayed
    IF SOB_MODO_GRAFICO()
        DispBegin()
    ENDIF

    *
    * montar cabeçalho
    *
    N_Largura := N_Col2Livre-N_Col1Livre+1
    N_LinImp  := N_LinIni+N_MargemSuperior
    *
    FOR N_Cont := 1 TO LEN(VC_Titulo)
        SETPOS(N_LinImp-1+N_Cont,N_Col1Livre+N_DeslocaCabecalho)
        DISPOUT(PADC(VC_Titulo[N_Cont],N_Largura-N_DeslocaCabecalho))
    NEXT
    *
    * montar área de função
    *
    N_LinImp  := N_LinMess
    *

    //
    // Fran: Labels
    //
    IF SOB_MODO_GRAFICO()
        IF N_WindowNum = 0 .AND. EH_PRODUCAO()
            DesenhaDrawLabe(VX_Janela)
            //AddGuiObject(VX_Janela,DesenhaDrawLabe(VX_Janela),CoordenadasDrawLabel())
        ELSEIF N_WindowNum = 1 .AND. .NOT. EH_PRODUCAO()
            DesenhaDrawLabe(VX_Janela)
            //AddGuiObject(VX_Janela,DesenhaDrawLabe(VX_Janela),CoordenadasDrawLabel())
        ENDIF
    ENDIF
    *

    //
    // Fran: Adding TOOLBAR
    //
    IF SOB_MODO_GRAFICO() .AND. L_CriarToolBar
        * O Windows coloca a ToolBar "acima" da janela do sistema.
        * Isto causou o efeito da ToolBar ficar "fora" da tela do
        * usuário, porque simplemente não coube na tela visível.
        * Para minimizar este problema, sem ter que olhar todas as
        * telas do sistema, optou-se para que, quando existir ToolBar:
        *   - A janela do sistema será "baixada" em 1 posição
        *   - Para que o subtítulo não fique "colado" na área útil,
        *     será acrescentado uma linha separadora entre o título da janela e
        *     a área útil da janela.
        *
        * Não exibir separador, pois a grid já serve como separador visual
        L_AcrescentarSeparadorSubtitulo := .T.
        IF N_TP_Jan == _JAN_SELE_ARQ_20
            #DEFINE VX_Sele  VX_SubObj
            L_MostraGrade  := VX_Sele:CARGO[09]      // Se mostra o grid
            #UNDEF VX_Sele
            IF L_MostraGrade
                L_AcrescentarSeparadorSubtitulo := .F.
            ENDIF
        ENDIF

        IF N_TP_Jan == _JAN_SELE_VETO_20
           #DEFINE VX_Sele  VX_SubObj
           L_MostraGrade  := VX_Sele:CARGO[09]      // Se mostra o grid
           #UNDEF VX_Sele
           IF L_MostraGrade
              L_AcrescentarSeparadorSubtitulo := .F.
           ENDIF
        ENDIF
        *
        * Não exibir saparador, pois o box do texto já serve como separador visual
        IF N_TP_Jan == _JAN_TEXTO_10 .OR. ;
            N_TP_Jan == _JAN_ARQTEXTO_10
            L_AcrescentarSeparadorSubtitulo := .F.
        ENDIF
        *
        *
        * Primeiramente definir a toolbar
        ADDGUI_TOOLBAR(VX_Janela)

        // #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
        //    IF L_AcrescentarSeparadorSubtitulo
        //       AddGuiObject(VX_Janela,DesenhaSeparadorSubtitulo(VX_Janela),;
        //                    CoordenadasSeparadorSubtitulo(VX_Janela))
        //    ENDIF
        //    *
        //    * Primeiramente definir a toolbar
        //    ADDGUI_TOOLBAR(VX_Janela)
        //    *
        //    * A função hb_gtInfo(), ao ser ativada com parâmetro HB_GTI_INKEYFILTER,
        //    * lê / seta um bloco de código a ser executado após a cada INKEY() ser processado.
        //    * O retorno deste bloco de código substitui a tecla que o usuário final digitou.
        //    * Este recurso é usado para tratar adequadamente a execução de ações da ToolBar.
        //    *
        //    * Seja lá qual for o conteúdo anterior deste bloco de código, salvá-lo
        //    * para restaurar o conteúdo original, quando a janela for fechada.
        //    #include "hbgtinfo.ch"
        //    B_SetInkeyAfterBlock_Old := ;
        //       hb_gtInfo( HB_GTI_INKEYFILTER, {|nkey| ProcessaBotaoToolbarKey( VX_Janela, nkey ) } )
        // #elif defined(__PLATFORM__LINUX)
        //    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
        // #else
        //    #erro "Código não adaptado para esta plataforma"
        // #endif
    ENDIF   // SOB_MODO_GRAFICO() .AND. L_CriarToolBar

    //
    // Fran: Adding buttons
    //
    IF SOB_MODO_GRAFICO()
        *
        * ATENÇÃO: No xHarbour e no Harbour antigo (compilador 6.3.0),
        *          a criação da PushButton era antes da criação da ToolBar.
        *          Se isto for feito no Harbour novo (compilador 10.1.0), a
        *          WVW_TBCREATE() desloca os PushButton para "baixo", ficando
        *          fora da janela visível.
        * "SOLUÇÃO" - Inverter a ordem, criado os PushButton depois da ToolBar.
        *             Assim o código ficou o mesmo para todos os compiladores.
        *
        //NAP_LOG("Adding buttons")
        FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
            //NAP_LOG("Adding button " + hb_ntos(N_Cont))
            ADICIONA_BOTAO_PUSH(VX_Janela,N_Cont)
            //NAP_LOG("Added button " + hb_ntos(N_Cont))
        NEXT

    ELSE    // Buttons in text mode
        * imprimir o texto do botao
        FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
            SETPOS(N_LinImp+V_RegiaoBotoes[N_Cont,_BOTAO_LIN_INICIAL],;
            N_Col1Livre+V_RegiaoBotoes[N_Cont,_BOTAO_COL_INICIAL])
            DISPOUT(V_RegiaoBotoes[N_Cont,_BOTAO_TEXTO_TRATADO_2])
        NEXT
        *
        * dar destaque as teclas de função
        FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
            SETPOS(N_LinImp+V_RegiaoBotoes[N_Cont,_BOTAO_LIN_INICIAL],;
            N_Col1Livre+V_RegiaoBotoes[N_Cont,_BOTAO_COL_DESTAQUE])
            DISPOUT(V_RegiaoBotoes[N_Cont,_BOTAO_TEXTO_DESTAQUE],C_CorInten)
        NEXT

    ENDIF   // SOB_MODO_GRAFICO() Buttons

     *
    //
    // Fran: Adding images
    //
    IF SOB_MODO_GRAFICO()
        FOR N_Cont := 1 TO LEN(V_LstImagens)

            NAP_CUALIB_IMAGE(V_LstImagens[N_Cont,_IMAGEM_ARQUIVO],;
                            V_LstImagens[N_Cont,_IMAGEM_BLOCO_ACAO],;
                            N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_INICIAL],;
                            N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_INICIAL],;
                            N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_FINAL  ],;
                            N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_FINAL  ],;
                            V_LstImagens[N_Cont,_IMAGEM_AUTOCLOSE])

            // OutStd("Image: " + V_LstImagens[N_Cont,_IMAGEM_ARQUIVO])
            // OutStd("STLin: " + hb_ntos(N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_INICIAL]))
            // OutStd("STCol: " + hb_ntos(N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_INICIAL]))
            // OutStd("EdCol: " + hb_ntos(N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_FINAL  ]))
            // OutStd("EdLin: " + hb_ntos(N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_FINAL  ]))

            // AddGuiObject(VX_Janela,DesenhaImagem(VX_Janela,N_Cont),;
            //             {N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_INICIAL],;
            //                 N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_INICIAL],;
            //                 N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_FINAL  ],;
            //                 N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_FINAL  ]})
        NEXT
    ENDIF // SOB_MODO_GRAFICO() Images

    // FRAN In GTWIN the text is not displayed
    IF SOB_MODO_GRAFICO()
        DispEnd()
    ENDIF

ENDIF // C_TelaCoberta == NIL

* executar o método de ativação da janela, conforme a sua especialização
*
// FRAN: Some testing about standard harbour text
// FOR N_Cont := 0 TO 35
//     @ N_Cont, N_Cont SAY "SAY in " + hb_ntos(N_Cont) + "," + hb_ntos(N_Cont)
// NEXT


// FRAN
// Testing about window and widget dimensions
LOG_PRINT("HELLLO FRAN!!!!. Window_NUM: " + hb_ntos(N_WindowNum))
LOG_PRINT("Window LOCATION: " + hb_ntos(N_LinIni) + ", " + hb_ntos(N_ColIni) + ", " + hb_ntos(N_LinFin) + ", " + hb_ntos(N_ColFin))
//SCROLL(N_LinIni,N_ColIni,N_LinFin,N_ColFin)      // limpar área

IF N_WindowNum = 0
    FOR N_Cont := 0 TO 35
    @ N_Cont, 0 SAY "**" + hb_ntos(N_Cont)
    @ N_Cont, 107 SAY "*" + hb_ntos(N_Cont)
    NEXT

    @ 1, 4 SAY "****** ********* ********* ********* ********* ********* ********* ********* ********* ********* *******"
    @ 34, 4 SAY "****** ********* ********* ********* ********* ********* ********* ********* ********* ********* *******"
    @ 0, 107 SAY "109"

    @ 26, 87 SAY "@ [26,87]"

ENDIF

//@ 4, 20 SAY "***************************************************************************************************" // + hb_ntos(N_Cont)
//@ 3, 41 SAY "*******" // + hb_ntos(N_Cont)
// @ 12, 20 SAY "20" // + hb_ntos(N_Cont)
// @ 12, 70 SAY "70" // + hb_ntos(N_Cont)

// @ 33, 73 SAY "**" // + hb_ntos(N_Cont)


// testing child windows position coherence
//@ 21, 40 SAY "******************************************************************" // + hb_ntos(N_Cont)

// @ 0, 0 SAY "[" // + hb_ntos(N_Cont)
// @ 34, 109 SAY "]" // + hb_ntos(N_Cont)

//@ N_Cont, N_Cont SAY "****"

// FRAN: Testing the image position
// @ 5, 5 SAY "5,5"
// @ 10, 18 SAY "10,18"

// @ 22, 0 SAY ""
// OutStd( "JAJAJAJAJAJ!!!!!!!!!!!!!!!!!!!!!!!!!!!  Hello" )

// FRAN: At the moment, events are managed by GTNAP
// IF SOB_MODO_GRAFICO()
//     //X_Retorno := NAP_CUALIB_LAUNCH_MODAL()

//     IF N_TP_Jan == NIL
//         X_Retorno := EVAL(B_Metodo,VX_Janela)
//     ELSE
//         X_Retorno := EVAL(B_Metodo)
//     ENDIF


// ELSE

//     // FRAN: Event management for text GT's
//     // DO WHILE 1 # 0
//     // ENDDO

//     IF N_TP_Jan == NIL
//         X_Retorno := EVAL(B_Metodo,VX_Janela)
//     ELSE
//         X_Retorno := EVAL(B_Metodo)
//     ENDIF
// ENDIF

//OutStd("Num Window: " + hb_ntos(N_WindowNum))

// IF N_WindowNum = 1
//     X_Retorno := NAP_CUALIB_LAUNCH_MODAL()
// ENDIF

// @ 22, 0 SAY ""
// IF B_Metodo == NIL
//     OutStd("ATIVE B_Metodo NIL")
//     X_Retorno := NAP_CUALIB_LAUNCH_MODAL()
// ELSE
//     OutStd("ATIVE B_Metodo VALID")
// ENDIF

NAP_LOG("FRAN Janela ATIVE antes de EVAL()")
IF N_TP_Jan == NIL
    //OutStd("Before EVAL(B_Metodo,VX_Janela)")
    X_Retorno := EVAL(B_Metodo,VX_Janela)
ELSE
    //OutStd("Before EVAL(B_Metodo)")
    X_Retorno := EVAL(B_Metodo)
ENDIF
NAP_LOG("FRAN Janela ATIVE despues de EVAL()")

 *
 SETCOLOR(C_CorAnt)                    // restaurar cor anterior
 SET(_SET_CURSOR,N_CursorAnt)          // restaurar modo do cursor

 IF .NOT. SOB_MODO_GRAFICO()
    SETKEY(K_F1,B_Ajuda_Ant)            // restaurar ajuda anterior
 ENDIF

 NAP_LOG("FRAN Janela ATIVE IF .NOT. L_CUA_10")

 *
 IF .NOT. L_CUA_10
    NAP_LOG("Antes de DestruaJan:")

    DestruaJan(VX_Janela,.T.)  // Na CUA 2.0, a janela sempre fecha após ativação
    NAP_LOG("Despues de DestruaJan:")
 ENDIF
 *

 //NAP_LOG("FINISH ATIVE:")
 //OutStd("FINISH ATIVE: " + hb_ntos(N_WindowNum))
RETURN X_Retorno   // Ative ( VX_Janela )
*
*
*

*********************
STAT FUNC EH_PRODUCAO
*********************
LOCAL L_EH_PRODUCAO := .T.
#IFDEF _TESTE
   L_EH_PRODUCAO := .F.
#ENDIF
RETURN L_EH_PRODUCAO
*

******************
PROC AJUSTA_BOTOES(VX_Janela)
******************
LOCAL N_Cont
* não imprime nada, só retorna as teclas devidamente formatadas
*
//NAP_LOG("AJUSTA_BOTOES!!!!!")
IF N_LinBotoes # NIL
   ? MEMVAR->AJUSTA_BOTOES_SO_PODE_SER_CHAMADA_UMA_VEZ
ENDIF
*
N_LinBotoes := TecFunc(N_Col2Livre-N_Col1Livre+1,@V_RegiaoBotoes)
N_Lin2Livre := N_Lin2Livre - N_LinBotoes
N_LinMess   := N_Lin2Livre+1 // linha de mensagens
*
FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
   IF V_RegiaoBotoes[N_Cont,_BOTAO_INKEY_DESTAQUE] # NIL .AND. ;  // deu para descobrir o INKEY
      .NOT. L_CUA_10
      * Permitir disparar o evento também através do teclado
      AddAcao(VX_Janela,V_RegiaoBotoes[N_Cont,_BOTAO_INKEY_DESTAQUE],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_BLOCO_ACAO],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_AUTOCLOSE],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_CDBOTAO],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_ALIAS_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_RECNO_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_FILTER_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_ORDER_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_EOFOK],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_HANDLE_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_MUDADADOS])
   ENDIF
NEXT
*
IF SOB_MODO_GRAFICO() .AND. N_LinBotoes > 1 .AND. N_EspacamentoEmPixels == 0
    NAP_LOG("MEMVAR->SOBREPOSICAO_DE_BOTOES AJUSTA_BOTOES!!!!!")

   * Com espaçamento 0, somente é possível ter uma única "fileira" de botões,
   * senão ocorrerá sobreposição das bordas.
   //? MEMVAR->SOBREPOSICAO_DE_BOTOES
ENDIF
*
**************************
PROC SETA_PARA_TER_TOOLBAR (VX_Janela)
**************************
IF L_CriarToolBar
   ? MEMVAR->CONFIGURACAO_PARA_TER_TOOLBAR_JA_FEITA
ENDIF
L_CriarToolBar := .T.
*
IF SOB_MODO_GRAFICO()
   * O Windows posiciona a "ToolBar" "acima" da coordenada
   * da janela já criada.
   *
   * Para que a ToolBar inicie "mais ou menos" (tamanho da ToolBar
   * não é sempre proporcional) no local definido pelo programador
   * na criação da janela, baixar a tela em uma linha.
   *
   * 2 linhas deslocadas da linha inicial, para dar espaço à ToolBar

   //
   //  FRAN: The toolbar extra height will be managed by GTNAP
   //  It's not a real equivalence between lines and toolbar buttons
   //

//    LOG_PRINT("JAJAJA! TOOLBAR")
//    N_LinIni++
//    N_LinIni++
ELSE
   * O modo texto não tem ToolBar, mas o cabeçalho será acrescentado
   * uma linha em branco(no topo), de forma que o programador,
   * ao usar o modo texto durante a fase de desenvolvimento,
   * já não veja, APROXIMADAMENTE, o que a ToolBar esconderá da
   * da tela anterior.
   ASIZE(VC_Titulo,LEN(VC_Titulo)+1)
   AINS(VC_Titulo,1)
   VC_Titulo[1] := ""
ENDIF
N_Lin1Livre++
*
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)

************************
STAT PROC ADDGUI_TOOLBAR(VX_Janela)
************************
LOCAL L_PermiteEdicao
//LOCAL hWndToolBar
LOCAL L_TOOLBAR_AINDA_SEM_BOTOES := .T.
LOCAL N_TelaHeight := TelaPrincipalHeight()
LOCAL N_PixelsBotao
LOCAL L_MudaDados
*
* Setar o tamanho do botão de forma proporcional,
* de forma que a altura da ToolBar não ultrapasse
* 2 linhas do sistema.
* Isto torna o tamanho da ToolBar mais proporcional,
* quando usado principalmente em resoluções pequenas
* (ex: 800 x 600 de projetores)
*
IF N_TelaHeight >= 960
    * A partir deste ponto, a ToolBar já é menor que 2 linhas
    * de texto, mesmo usando o tamanho real do BITMAP.
    N_PixelsBotao := 32 // Tamanho real das imagens é 32 x 32
ELSEIF N_TelaHeight >=  864
    N_PixelsBotao := 30
ELSEIF N_TelaHeight >=  768
    N_PixelsBotao := 28
ELSEIF N_TelaHeight >=  720
    N_PixelsBotao := 25
ELSEIF N_TelaHeight >= 600
    N_PixelsBotao := 20
ELSE
    N_PixelsBotao := 18
ENDIF
*
* A criação da toolbar caso for .T. se não apresenta mensagem.
* No Xharbour e no Harbour 3.2.0 (GCC 6.3.0), a WVW_TBCREATE() retorna um número.
* O retorno podia ser testado.
*  IF ( hWndToolBar := WVW_TBCREATE( N_WindowNum, .F., NIL, 0, N_PixelsBotao, N_PixelsBotao ) ) == 0
*     ALARME("M28028","Falha ao criar a 'ToolBar'")
*    ? MEMVAR->TOOLBAR_NAO_CRIADA
*  ENDIF
* Mas, no Harbour 3.2.0 (GCC 10.1.0), a WVW_TBCREATE() retorna um ponteiro.
* O retorno não mais será testado, portanto.
//
// FRAN: TODO Create a toolbar in GTNAP
//
//hWndToolBar := WVW_TBCREATE( N_WindowNum, .F., NIL, 0, N_PixelsBotao, N_PixelsBotao )
*
//NAP_LOG("CREATE TOOLBAR!!!!")
NAP_CUALIB_TOOLBAR(N_PixelsBotao)

IF TEM_BOTAO(VX_Janela,{"Incluir","Alterar","Excluir","Consultar"})
    *
    IF TEM_BOTAO(VX_Janela,{"Incluir"},.T.,@L_MudaDados)
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_INCLUI,_BITMAP_INCLUI_DESAB},"Incluir",{||__Keyboard("I")},0,L_MudaDados)
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Alterar"},.T.,@L_MudaDados)
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_ALTERA,_BITMAP_ALTERA_DESAB},"Alterar",{||__Keyboard("A")},1,L_MudaDados)
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Excluir"},.T.,@L_MudaDados)
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_EXCLUI,_BITMAP_EXCLUI_DESAB},"Excluir",{||__Keyboard("E")},2,L_MudaDados)
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Consultar"})
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_CONSULTA},"Consultar",{||__Keyboard("C")})
    ENDIF
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
    *
ENDIF
*
IF TEM_BOTAO(VX_JANELA,{"Listar","Procurar"})
    *
    IF TEM_BOTAO(VX_Janela,{"Listar"})
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_PRINTER} ,"Listar",{||__Keyboard("L")})
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Procurar"})
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_PESQUISE},"Procurar",{||__Keyboard("P")})
    ENDIF
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
    *
IF TEM_BOTAO(VX_Janela,{"F2=ok"})
    IF TEM_BOTAO(VX_Janela,{"F2=salvar"})
        ? MEMVAR->BOTAO_OK_E_SALVAR_AO_MESMO_TEMPO
    ENDIF
    *
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_OK},"Ok",{||HB_KeyPut(K_F2)})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
*
IF TEM_BOTAO(VX_Janela,{"F2=salvar"},.T.,@L_MudaDados)
    IF TEM_BOTAO(VX_Janela,{"F2=ok"})
        ? MEMVAR->BOTAO_OK_E_SALVAR_AO_MESMO_TEMPO
    ENDIF
    *
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_SALVAR,_BITMAP_SALVAR_DESAB},"Salvar",{||HB_KeyPut(K_F2)},,L_MudaDados)
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
*
IF N_TP_Jan == _JAN_TEXTO_10
    #DEFINE B_Edita   VX_SubObj[10]
    L_PermiteEdicao := EVAL(B_Edita)
    #UNDEF B_Edita
ENDIF
IF N_TP_Jan == _JAN_ENTRADA_10
    #DEFINE VX_Edicao      VX_SubObj
    #DEFINE B_EditaGlobal  VX_Edicao[15]
    L_PermiteEdicao := EVAL(B_EditaGlobal)
    #UNDEF B_EditaGlobal
    #UNDEF VX_Edicao
ENDIF
IF N_TP_Jan == _JAN_ARQTEXTO_10
    L_PermiteEdicao := .F.
ENDIF
*
IF N_TP_Jan == _JAN_TEXTO_10 .OR. ;
    N_TP_Jan == _JAN_ENTRADA_10 .OR. ;
    N_TP_Jan == _JAN_ARQTEXTO_10
    *
    IF L_PermiteEdicao
        * No teclado será colocado, de fato, a K_CTRL_F11, mas a AjustaTecla() voltará o conteúdo para K_CTRL_X.
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_RECORTAR},"Recortar",{||HB_KeyPut(K_CTRL_X_TROCADO_POR_K_CTRL_F11)})
    ENDIF
    *
    * No teclado será colocado, de fato, a K_CTRL_F9, mas a AjustaTecla() voltará o conteúdo para K_CTRL_C.
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_COPIA},"Copiar",{||HB_KeyPut(K_CTRL_C_TROCADO_POR_K_CTRL_F9)})
    *
    IF L_PermiteEdicao
        * No teclado será colocado, de fato, a K_CTRL_F10, mas a AjustaTecla() voltará o conteúdo para K_CTRL_V.
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_COLAR},"Colar",{||HB_KeyPut(K_CTRL_V_TROCADO_POR_K_CTRL_F10)})
    ENDIF
    *
    IF L_PermiteEdicao
        * No teclado será colocado, de fato, a K_CTRL_F12, mas a AjustaTecla() voltará o conteúdo para K_CTRL_Z.
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_DESFAZER},"Desfazer",{||HB_KeyPut(K_CTRL_Z_TROCADO_POR_K_CTRL_F12)})
    ENDIF
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
    *
ENDIF
*
IF C_CdTela == "T03221"  // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                {||EXIBEVIDEO("V","aPfO9X_r63k","Cotacoes_de_Precos")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T01306" .AND. LEN(VC_Titulo) > 1    // DESCONSIDERAR_CHECA_ID
    IF VC_Titulo[2] == "PREGÃO"
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                                {||EXIBEVIDEO("V","aYae8t-oviM","Corona_Interno")})
        ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
        L_TOOLBAR_AINDA_SEM_BOTOES := .F.

    ELSEIF VC_Titulo[2] == "TOMADA DE PREÇOS"
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                                {||EXIBEVIDEO("V","05Bukz8ymck","Definicao_de_Lotes_de_Licitacao")})
        ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
        L_TOOLBAR_AINDA_SEM_BOTOES := .F.
    ENDIF

ELSEIF C_CdTela == "T00380"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","8J_vOoVMeOs","Empenho")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T00452"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","6xlfNLDbMP4","Pagamento")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T00757"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","txoHEuXjxvk","Receita")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T03240"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","RzLdTT7eYPg","Equivalencia_Dotacao")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T06101"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","p85Wk3xjlfI","Equivalencia_Dotacao_em_registro_preco")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T18125" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","XvdCnede-pQ","Cadastro_das_comissoes_de_avaliacao_e_inventario")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T17261" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","tx7C_-sRJ84","Cadastro_das_localizacoes")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T17257" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","tx7C_-sRJ84","Cadastro_das_localizacoes")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T17535" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","tx7C_-sRJ84","Cadastro_das_localizacoes")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T03253" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","wgDezwHfHFE","Dotacoes_e_controle_de_saldos")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T06290" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_VIDEO},"Vídeo Aula",;
                            {||EXIBEVIDEO("V","wgDezwHfHFE","Dotacoes_e_controle_de_saldos")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ENDIF
*
IF N_TP_Jan # NIL   // indica que janela foi especializada
    * Colocar em todas as telas ?
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_CALCULA} ,"Calculadora",{||HB_KeyPut(K_F5)})
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_AJUDA} ,"Ajuda",{||HB_KeyPut(K_F1)})
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    *
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_SAIDA} ,"Saida",{||HB_KeyPut(K_ESC)})
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
*
* A rotina que fecha a janela sempre destroi a ToolBar.
* Portanto, está se supondo que não exista ToolBar sem botões.
IF L_TOOLBAR_AINDA_SEM_BOTOES
    ? MEMVAR->TOOLBAR_SEM_BOTOES
ENDIF



*
********************************
STAT PROC ADICIONA_BOTAO_TOOLBAR (VX_Janela,V_TOOLBAR_COD_BITMAP,;
                                    C_TOOLBAR_TOOLTIP,B_TOOLBAR_BLOCO_ACAO, N_SEQUENCIA, L_MudaDados)
********************************
LOCAL N_TOOLBAR_COD_BITMAP
LOCAL C_BASE_PATH := "../imgtbar/"
LOCAL C_ICON_PATHNAME := ""
*
* Este número vai de 400 em diante, e é necessário ao Windows
N_ToolBarCodigoAcao++
*
IF INABILITA_BOTAO_TOOLBAR(L_MudaDados)
    N_TOOLBAR_COD_BITMAP := V_TOOLBAR_COD_BITMAP[2] // Índice 2, imagem indicando botão desabilitado
    B_TOOLBAR_BLOCO_ACAO := {||.F.}   // Inabilitar a ação do botão.
ELSE
    N_TOOLBAR_COD_BITMAP := V_TOOLBAR_COD_BITMAP[1] // Índice 1, imagem indicando botão habilitado
ENDIF
*
AADD(V_BotoesToolBar,{N_ToolBarCodigoAcao,N_TOOLBAR_COD_BITMAP,;
                        C_TOOLBAR_TOOLTIP,B_TOOLBAR_BLOCO_ACAO})

IF N_TOOLBAR_COD_BITMAP == _BITMAP_INCLUI
    C_ICON_PATHNAME := C_BASE_PATH + "inclui.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_ALTERA
    C_ICON_PATHNAME := C_BASE_PATH + "altera.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_EXCLUI
    C_ICON_PATHNAME := C_BASE_PATH + "exclui.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_CONSULTA
    C_ICON_PATHNAME := C_BASE_PATH + "consulta.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_OK
    C_ICON_PATHNAME := C_BASE_PATH + "ok.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_PESQUISE
    C_ICON_PATHNAME := C_BASE_PATH + "pesquise.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_PRINTER
    C_ICON_PATHNAME := C_BASE_PATH + "printer.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_SALVAR
    C_ICON_PATHNAME := C_BASE_PATH + "salvar.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_RECORTAR
    C_ICON_PATHNAME := C_BASE_PATH + "recortar.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_COPIA
    C_ICON_PATHNAME := C_BASE_PATH + "copia.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_COLAR
    C_ICON_PATHNAME := C_BASE_PATH + "colar.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_DESFAZER
    C_ICON_PATHNAME := C_BASE_PATH + "desfazer.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_CALCULA
    C_ICON_PATHNAME := C_BASE_PATH + "calcula.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_AJUDA
    C_ICON_PATHNAME := C_BASE_PATH + "ajuda.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_SAIDA
    C_ICON_PATHNAME := C_BASE_PATH + "saida.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_VIDEO
    C_ICON_PATHNAME := C_BASE_PATH + "video.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_ESPACOVAZIO
    C_ICON_PATHNAME := C_BASE_PATH + "espacovazio.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_INCLUI_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "inclui_i.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_ALTERA_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "altera_i.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_EXCLUI_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "exclui_i.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_SALVAR_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "salvar_i.bmp"
ENDIF

//NAP_LOG(C_ICON_PATHNAME + C_TOOLBAR_TOOLTIP)
NAP_CUALIB_TOOLBAR_BUTTON(C_ICON_PATHNAME, C_TOOLBAR_TOOLTIP)

//NAP_LOG("ADD TOOLBAR BUTTON: " + hb_ntos(N_ToolBarCodigoAcao) + ":" + C_TOOLBAR_TOOLTIP + ":" + hb_ntos(N_TOOLBAR_COD_BITMAP))

//
//  FRAN: TODO Adding button
//
*
// WVW_TBAddButton(N_WindowNum,N_ToolBarCodigoAcao,N_TOOLBAR_COD_BITMAP,;
//                 C_TOOLBAR_TOOLTIP,0,.T.)


*
*
***************************************
STAT PROC ADICIONA_SEPARADOR_AO_TOOLBAR (VX_Janela)
***************************************
* Este número vai de 400 em diante, e é necessário ao Windows
N_ToolBarCodigoAcao++
*
* A chamada no formato WVW_TBAddButton(N_WindowNum)
* coloca uma vertical na ToolBar, para separar grupos
* de botões. Mas visualmente é quase imperceptível.
* Por isto se está adicionando um botão sem conteúdo, para servir de separador.
* Desabilitá-lo, para que não mude a aparência ao cursor passar em cima.
* Existe o "-1" porque o primeiro botão é o "0".
*WVW_TBAddButton(N_WindowNum,N_ToolBarCodigoAcao,_BITMAP_ESPACOVAZIO,"",{||NIL})
*WVW_TBEnableButton(N_WindowNum,WVW_TBButtonCount()-1,.F.)
*
* O separador padrão da WVW possui uma aparência melhor
//
// FRAN TODO
//
//NAP_LOG("ADD TOOLBAR SEPARATOR")
//WVW_TBAddButton(N_WindowNum)
NAP_CUALIB_TOOLBAR_SEPARATOR()
*
*******************
STAT FUNC TEM_BOTAO(VX_Janela,VC_TeclasBusca, L_ChecaMudaDados, L_MudaDados)
*******************
LOCAL N_Cont
LOCAL L_TemBotao := .F.
LOCAL N_PosBotao
*
DEFAULT L_ChecaMudaDados TO .F.
DEFAULT L_MudaDados      TO .F.
*
FOR N_Cont := 1 TO LEN(VC_TeclasBusca)

    N_PosBotao :=  ASCAN(V_RegiaoBotoes,;
                        {|V_SUBV|XUPPER(VC_TeclasBusca[N_Cont]) == ;
                                    TROCA(XUPPER(ALLTRIM(V_SUBV[_BOTAO_TEXTO_TRATADO_2])),"&","")})

    IF N_PosBotao # 0
        L_TemBotao := .T.
        IF L_ChecaMudaDados
            IF V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS] # NIL
            L_MudaDados := V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS]
            ENDIF
            IF UPPER(VC_TeclasBusca[1]) == "F2=SALVAR"
            L_MudaDados := .T.
            ENDIF
        ENDIF
    ENDIF
NEXT
*
RETURN L_TemBotao
//    *
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
// *
*
*******************
FUNCTION DestruaJan ( VX_Janela, L_Permitir_CUA_20 )
*******************
LOCAL N_CT
DEFAULT L_Permitir_CUA_20 TO .F.

* se janela foi ativada ao menos uma vez, fechá-la
*
IF .NOT. L_Permitir_CUA_20 .AND. .NOT. L_CUA_10
   ? MEMVAR->DESTRUA_JAN_NAO_EXISTE_NA_CUA_20
ENDIF
*
IF L_JanTipoMsgAguarde
   ? MEMVAR->ERRO_FECHAMENTO_COM_DESTRUAJAN
ENDIF
*
IF C_TelaCoberta # NIL
   *
   IF SOB_MODO_GRAFICO()
      IF L_Embutida
         IF N_WindowNum # LEN(V_PilhaJanelas)-IIF(EH_PRODUCAO(),1,0)  // destruir sempre a última ativada
            ALARME("M28752","Alguma janela aberta nï¿½o foi fechada - passo 2...")
            *ALERT("Alguma janela aberta nï¿½o foi fechada - passo 2...")
         ENDIF
      ELSE

        //#IFDEF _TESTE
        //   IMPRIME_PARA_DEBUGAR(DIRSAIDA()+"destruajan.txt",LEN(V_PilhaJanelas),V_PilhaJanelas[LEN(V_PilhaJanelas),2])
        //#ENDIF

        //
        // FRAN: In GTNAP All widgets are destroyed with the window
        //
        // IF L_CriarToolBar
        //     * Restaurar o conteï¿½do anterior do filtro de Inkeys, seja lï¿½ qual for.
        //     hb_gtInfo( HB_GTI_INKEYFILTER, B_SetInkeyAfterBlock_Old )
        //     *
        //     * A rigor, os exemplo da ToolBar da GTWVW nunca
        //     * incluem a exclusï¿½o prï¿½via dos botï¿½es, e o manual
        //     * informa que a destruiï¿½ï¿½o da ToolBar jï¿½ destroi os botï¿½es.
        //     * Mas fazer a exclusï¿½o manual prï¿½via, para ficar mais
        //     * documentado e se fazer o inverso do que foi feito na
        //     * criaï¿½ï¿½o da janela.
        //     FOR N_CT := WVW_TBButtonCount() TO 1 STEP -1
        //         * Existe o "-1" porque o primeiro botï¿½o ï¿½ o "0".
        //         WVW_TBDelButton(N_WindowNum,N_CT-1)
        //     NEXT
        //     *
        //     WVW_TBDESTROY(N_WindowNum)
        // ENDIF
        *

        //
        // FRAN: In GTNAP All widgets are destroyed with the window
        //
        // FOR N_CT := 1 TO LEN(V_RegiaoBotoes)
        //     WVW_PBDESTROY(N_WindowNum,V_RegiaoBotoes[N_CT,_BOTAO_HANDLE_PUSHBUTTON])
        // NEXT
        *
        *
        //
        // FRAN: In GTNAP All widgets are destroyed with the window
        //
        // IF L_JanTipoMsgAguarde
        //     IF N_ProgressBar == 2
        //         WVW_PGDESTROY(N_WindowNum,N_IdProgressBar1)
        //         WVW_PGDESTROY(N_WindowNum,N_IdProgressBar2)
        //         N_ProgressBar := 0
        //     ELSEIF N_ProgressBar == 1
        //         WVW_PGDESTROY(N_WindowNum,N_IdProgressBar1)
        //         N_ProgressBar := 0
        //     ENDIF
        // ENDIF

        *
        //
        // FRAN: GTNAP doesn't create any window by default
        //
        // IF LEN(V_PilhaJanelas)==1 .AND. EH_PRODUCAO()
        //     * A GTWVW jï¿½ fecha a janela 0 automaticamente.
        //     * Fazer com que a ï¿½ltima janela fechada pela CUA
        //     * seja a mesma.
        // ELSE
        //     WVW_lCloseWindow()
        // ENDIF
        // WvW_SetMainCoord(L_MainCoord_Ant)
        *
        NAP_CUALIB_DESTROY_WINDOW()


        IF N_WindowNum # LEN(V_PilhaJanelas)-IIF(EH_PRODUCAO(),1,0)  // destruir sempre a ï¿½ltima ativada
            ALARME("M28754","Alguma janela aberta nï¿½o foi fechada - passo 2...")
            *ALERT("Alguma janela aberta nï¿½o foi fechada - passo 2...")
        ENDIF
        ASIZE(V_PilhaJanelas,LEN(V_PilhaJanelas)-1)



      ENDIF // L_Embutida

   ELSE // NOT SOB_MODO_GRAFICO()
      RESTSCREEN(N_LinIni,N_ColIni,N_LinFin,N_ColFin,C_TelaCoberta)  // restaurar tela
      IF .NOT. L_Embutida
         ASIZE(V_PilhaJanelas,LEN(V_PilhaJanelas)-1)
      ENDIF

   ENDIF // SOB_MODO_GRAFICO()
   *
   SETPOS(N_LinAnt,N_ColAnt)            // posiciona o cursor nas coordenadas
   *
   C_TelaCoberta := NIL
   N_LinAnt := N_ColAnt := NIL    // não tem conteúdo em janelas fechadas
   *
ENDIF // C_TelaCoberta # NIL
*
* destruir as matrizes chamadas diretamente pelo objeto janela,
* de modo a facilitar o "garbage collection" automático do clipper.
*
IF VX_SubObj # NIL
   ASIZE(VX_SubObj,0)
ENDIF
ASIZE(VC_Titulo,0)
*
ASIZE(VX_Janela,0)
*
RETURN NIL


//
//  FRAN: TODO
//
*
*******************
FUNCTION Rolamento_ ( VX_Janela , L_Esq , L_Cima , L_Baixo , L_Dir )
*
RETURN NIL

// *
// *******************
// FUNCTION Rolamento_ ( VX_Janela , L_Esq , L_Cima , L_Baixo , L_Dir )
// *
// LOCAL N_CursorAnt , N_LinhaAnt , N_ColunaAnt, N_Cont
// LOCAL L_ScrollVerticalMudou := (L_RolaCima  # L_Cima ) .OR. ;
//                                (L_RolaBaixo # L_Baixo)
// LOCAL L_ScrollHorizontalMudou := (L_RolaEsquerda # L_Esq ) .OR.;
//                                  (L_RolaDireita  # L_Dir )
// *
// * atualizar atributos do objeto janela
// L_RolaCima      := L_Cima
// L_RolaBaixo     := L_Baixo
// L_RolaEsquerda  := L_Esq
// L_RolaDireita   := L_Dir
// *
// IF (L_Cima .OR. L_Baixo) .AND. .NOT. L_TemScrollVertical
//    IF SOB_MODO_GRAFICO()
//       ALARME("M28756","Erro no rolamento vertical")
//       ? MEMVAR->ERRO_VERTICAL
//    ELSE
//       ALERT("Erro no rolamento vertical")
//       ? MEMVAR->ERRO_VERTICAL
//    ENDIF
// ENDIF
// *
// IF (L_Esq .OR. L_Dir) .AND. .NOT. L_TemScrollHorizontal
//    IF SOB_MODO_GRAFICO()
//       ALARME("M28758","Erro no rolamento horizontal")
//       ? MEMVAR->ERRO_HORIZONTAL
//    ELSE
//      ALERT("Erro no rolamento horizontal")
//      ? MEMVAR->ERRO_HORIZONTAL
//    ENDIF
// ENDIF
// *
// * imprimir indicativos de rolamento
// *
// IF L_ScrollVerticalMudou .OR. ;
//    L_ScrollHorizontalMudou
//    N_CursorAnt := SET(_SET_CURSOR,SC_NONE)          // salvar modo do cursor
//    N_LinhaAnt  := ROW()
//    N_ColunaAnt := COL()
// ENDIF
// *
// IF L_TemScrollVertical .AND. L_ScrollVerticalMudou
//    IF SOB_MODO_GRAFICO()
//       #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//          * - Apesar de ser um elemento grï¿½fico, o desenho tem de ser imediato,
//          *   pois o CONTEï¿½DO mudou.
//          * - O redesenho em backgroup tambï¿½m continua a ser feito,
//          *   atravï¿½s da WVW_PAINT(), para os casos do prï¿½prio windows
//          *   solicitar o redesenho
//          DesenhaScrollVertical(VX_Janela)
//       #elif defined(__PLATFORM__LINUX)
//          // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//       #else
//          #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
//       #endif
//    ELSE
//       DispBegin()
//       IF .NOT. L_Cima .AND. .NOT. L_Baixo
//          * limpar ï¿½rea da barra de rolagem
//          SCROLL(N_Lin1Livre,N_Col2Livre+2,N_Lin2Livre,N_Col2Livre+2)
//          N_LinMarcadorVertical := 0
//       ELSE
//          * montar seta para cima
//          SETPOS(N_Lin1Livre,N_Col2Livre+2)
//          DISPOUT(CHR(30),C_CorInten)      //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//          *
//          IF .NOT. L_Cima      // no topo
//             N_LinMarcadorVertical := N_Lin1Livre+1
//          ELSEIF .NOT. L_Baixo // embaixo
//             N_LinMarcadorVertical := N_Lin2Livre-1
//          ELSE                 // no meio
//             N_LinMarcadorVertical := ROUND((N_Lin1Livre+N_Lin2Livre)/2,0)
//          ENDIF
//          *
//          * montar barra de rolagem
//          FOR N_Cont := N_Lin1Livre+1 TO N_Lin2Livre-1
//             SETPOS(N_Cont,N_Col2Livre+2)
//             IF N_Cont == N_LinMarcadorVertical
//                DISPOUT(CHR(219),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//             ELSE
//                DISPOUT(CHR(176),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//             ENDIF
//          NEXT
//          *
//          * montar seta para baixo
//          SETPOS(N_Lin2Livre,N_Col2Livre+2)
//          DISPOUT( CHR(31),C_CorInten)     //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//          *
//       ENDIF
//       DispEnd()
//    ENDIF
// ENDIF
// *
// IF L_TemScrollHorizontal .AND. L_ScrollHorizontalMudou
//    IF SOB_MODO_GRAFICO()
//       #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//          * - Apesar de ser um elemento grï¿½fico, o desenho tem de ser imediato,
//          *   pois o CONTEï¿½DO mudou.
//          * - O redesenho em backgroup tambï¿½m continua a ser feito,
//          *   atravï¿½s da WVW_PAINT(), para os casos do prï¿½prio windows
//          *   solicitar o redesenho
//          DesenhaScrollHorizontal(VX_Janela)
//       #elif defined(__PLATFORM__LINUX)
//          // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//       #else
//          #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
//       #endif
//    ELSE
//       DispBegin()
//       IF .NOT. L_Esq .AND. .NOT. L_Dir
//          * limpar ï¿½rea da barra de rolagem
//          SCROLL(N_Lin2Livre+2,N_Col1Livre,N_Lin2Livre+2,N_Col2Livre)
//          N_ColMarcadorHorizontal := 0
//       ELSE
//          * montar seta para esquerda
//          SETPOS(N_Lin2Livre+2,N_Col1Livre+1)
//          DISPOUT(CHR(17),C_CorInten)      //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//          *
//          IF .NOT. L_Esq       // na margem esquerda
//             N_ColMarcadorHorizontal := N_Col1Livre+2
//          ELSEIF .NOT. L_Dir   // na margem direita
//             N_ColMarcadorHorizontal := N_Col2Livre-2-1
//          ELSE                 // no meio
//             N_ColMarcadorHorizontal := ROUND((N_Col1Livre+N_Col2Livre)/2,0)
//          ENDIF
//          *
//          * montar barra de rolagem
//          FOR N_Cont := N_Col1Livre+2 TO N_Col2Livre-2
//             SETPOS(N_Lin2Livre+2,N_Cont)
//             IF N_Cont == N_ColMarcadorHorizontal .OR. ;
//                N_Cont == N_ColMarcadorHorizontal+1
//                DISPOUT(CHR(219),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//             ELSE
//                DISPOUT(CHR(177),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//             ENDIF
//          NEXT
//          *
//          * montar seta para direita
//          SETPOS(N_Lin2Livre+2,N_Col2Livre-1)
//          DISPOUT( CHR(16),C_CorInten)
//          *                                //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
//       ENDIF
//       DispEnd()
//    ENDIF
// ENDIF
// *
// IF L_ScrollVerticalMudou .OR. ;
//    L_ScrollHorizontalMudou
//    SET(_SET_CURSOR,N_CursorAnt)          // restaurar modo do cursor
//    SETPOS(N_LinhaAnt,N_ColunaAnt)
// ENDIF
// *
// RETURN NIL
// *

*********************
FUNC SOB_MODO_GRAFICO
*********************
LOCAL L_ModoGrafico := .F.
IF HB_GTVERSION()=="NAP"
    #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows) || defined(__PLATFORM__LINUX)
        L_ModoGrafico := .T.
    #else
       #erro "Código não adaptado para esta plataforma"
    #endif
   L_ModoGrafico := .T.
ENDIF
*
RETURN L_ModoGrafico



*
**********************
FUNCTION RegiaoJanela_ ( VX_Janela, N_MRow, N_MCol, ;
                         N_RowTop_Util,  N_ColLeft_Util,;
                         N_RowDown_Util, N_ColRight_Util,;
                         N_Keyboard, V_Botao, V_Imagem )   // passados por referência
*
LOCAL N_RegiaoMouse := BUSCANDO_REGIAO
LOCAL N_PosBotao, N_PosImagem
*
N_Keyboard := NIL
V_Botao    := NIL
V_Imagem   := NIL
*
IF (N_MCol==N_Col2Livre+1 .OR. ;
    N_MCol==N_Col2Livre+2 .OR. ;
    N_MCol==N_Col2Livre+3) .AND. ;
    N_MRow >= N_Lin1Livre .AND. ;
    N_MRow <= N_Lin2Livre
   *
   * clicou na barra de rolagem vertical
   *
   IF N_MRow==N_Lin1Livre
      N_RegiaoMouse := ACIMA_UMA_LINHA
      N_Keyboard := K_UP
   ELSEIF N_MRow==N_Lin2Livre
      N_RegiaoMouse := ABAIXO_UMA_LINHA
      N_Keyboard := K_DOWN
   ELSEIF N_MRow==N_Lin1Livre+1
      N_RegiaoMouse := ACIMA_TOTAL
      IF N_TP_Jan == _JAN_TEXTO_10
         N_Keyboard := K_CTRL_PGUP
      ELSE
         N_Keyboard := K_CTRL_HOME
      ENDIF
   ELSEIF N_MRow==N_Lin2Livre-1
      N_RegiaoMouse := ABAIXO_TOTAL
      IF N_TP_Jan == _JAN_TEXTO_10
         N_Keyboard := K_CTRL_PGDN
      ELSE
         N_Keyboard := K_CTRL_END
      ENDIF
   ELSEIF N_MRow < N_LinMarcadorVertical
      N_RegiaoMouse := ACIMA_UMA_PAGINA
      N_Keyboard := K_PGUP
   ELSEIF N_MRow > N_LinMarcadorVertical
      N_RegiaoMouse := ABAIXO_UMA_PAGINA
      N_Keyboard := K_PGDN
   ELSE
      * clicou sobre o marcador.
   ENDIF
   *
ENDIF
*
IF (N_MRow==N_Lin2Livre+1 .OR. ;
    N_MRow==N_Lin2Livre+2 ).AND. ;
    N_MCol >= N_Col1Livre .AND. ;
    N_MCol <= N_Col2Livre
   *
   * clicou na barra de rolagem horizontal
   *
   IF N_MCol==N_Col1Livre .OR. N_MCol==N_Col1Livre+1
      N_RegiaoMouse := ESQUERDA_UMA_COLUNA
      N_Keyboard := K_LEFT
   ELSEIF N_MCol==N_Col2Livre .OR. N_MCol==N_Col2Livre-1
      N_RegiaoMouse := DIREITA_UMA_COLUNA
      N_Keyboard := K_RIGHT
   ELSEIF N_MCol==N_Col1Livre+2
      N_RegiaoMouse := ESQUERDA_TOTAL
      N_Keyboard := K_HOME
   ELSEIF N_MCol==N_Col2Livre-2
      N_RegiaoMouse := DIREITA_TOTAL
      N_Keyboard := K_END
   ELSEIF N_MCol < N_ColMarcadorHorizontal
      N_RegiaoMouse := ESQUERDA_UMA_PAGINA
      IF N_TP_Jan == _JAN_ARQTEXTO_10
         N_Keyboard := K_CTRL_LEFT
      ELSE
         N_Keyboard := K_LEFT
      ENDIF
   ELSEIF N_MCol > N_ColMarcadorHorizontal+1   // marcador ocupa 2 bytes
      N_RegiaoMouse := DIREITA_UMA_PAGINA
      IF N_TP_Jan == _JAN_ARQTEXTO_10
         N_Keyboard := K_CTRL_RIGHT
      ELSE
         N_Keyboard := K_RIGHT
      ENDIF
   ELSE
      * clicou sobre o marcador.
   ENDIF
   *
ENDIF
*
IF .NOT. SOB_MODO_GRAFICO() .AND. ;
   N_MRow >= N_LinMess+1 .AND. N_MRow <= N_LinMess + N_LinBotoes
   * Clicou nas linhas da área de botoes
   *
   N_PosBotao := ASCAN(V_RegiaoBotoes,{|V_Botao| ;
                       N_MRow >= N_LinMess+V_Botao[_BOTAO_LIN_INICIAL] .AND. ;
                       N_MRow <= N_LinMess+V_Botao[_BOTAO_LIN_FINAL]   .AND. ;
                       N_MCol >= N_Col1Livre+V_Botao[_BOTAO_COL_INICIAL] .AND. ;
                       N_MCol <= N_Col1Livre+V_Botao[_BOTAO_COL_FINAL] })
   *
   IF N_PosBotao # 0   // clicou em cima de um botão
      *
      N_RegiaoMouse := BOTAO_IDENTIFICADO
      V_Botao := V_RegiaoBotoes[N_PosBotao]
      *
      N_Keyboard := V_Botao[_BOTAO_INKEY_DESTAQUE]  // podia ser o _BOTAO_INKEY_DESTAQUE_CASE
      IF N_Keyboard == NIL
         N_RegiaoMouse := BOTAO_NAO_IDENTIFICADO
      ENDIF
      *
   ENDIF
   *
ENDIF
*
IF N_RegiaoMouse == BUSCANDO_REGIAO
   IF N_MRow < N_RowTop_Util
      * clicou acima da area útil
   ELSEIF N_MCol < N_ColLeft_Util
      * clicou à esquerda da area util
   ELSEIF N_MRow > N_RowDown_Util
      * clicou abaixo da area util
   ELSEIF N_MCol > N_ColRight_Util
      * clicou à direita da area útil
   ELSE
      N_RegiaoMouse := AREA_UTIL
   ENDIF
ENDIF
*
IF N_RegiaoMouse == BUSCANDO_REGIAO
   IF N_MRow < N_LinIni
      * clicou acima da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSEIF N_MCol < N_ColIni
      * clicou à esquerda da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSEIF N_MRow > N_LinFin
      * clicou abaixo da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSEIF N_MCol > N_ColFin
      * clicou à direita da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSE
      N_RegiaoMouse := REGIAO_SEM_EVENTO
   ENDIF
ENDIF
*
IF SOB_MODO_GRAFICO()
   * Imagens adicionadas pelo programador de forma manual (comando IMAGEM)
   * tem precedência sobre qualquer outro tipo de região da tela.
   * Por exemplo, se uma imagem estiver sobre um browse, o click do mouse não
   * será repassado à tbrowse().
   N_PosImagem := ASCAN(V_LstImagens,{|V_Imagem| ;
                       N_MRow >= N_LinIni+V_Imagem[_IMAGEM_LIN_INICIAL] .AND. ;
                       N_MRow <= N_LinIni+V_Imagem[_IMAGEM_LIN_FINAL]   .AND. ;
                       N_MCol >= N_ColIni+V_Imagem[_IMAGEM_COL_INICIAL] .AND. ;
                       N_MCol <= N_ColIni+V_Imagem[_IMAGEM_COL_FINAL] })
   *
   IF N_PosImagem # 0   // clicou em cima de uma imagem
      * se existir algum evento vinculado ï¿½ imagem
      IF V_LstImagens[N_PosImagem,_IMAGEM_BLOCO_ACAO] # NIL .OR. ;
         V_LstImagens[N_PosImagem,_IMAGEM_KEYBOARD] # NIL  // podia ser o _IMAGEM_KEYBOARD_CASE
         V_Imagem := V_LstImagens[N_PosImagem]
         N_Keyboard := V_Imagem[_IMAGEM_KEYBOARD]  // podia ser o _IMAGEM_KEYBOARD_CASE
         N_RegiaoMouse := SOBRE_IMAGEM
      ENDIF
   ENDIF
ENDIF
*
RETURN N_RegiaoMouse
*





***************
FUNC _Pega_Cor_ ( C_CorCompleta, N_Indice, L_Frente )
*
LOCAL C_CorPar, N_Pos, C_Cor
*
C_CorCompleta := ","+C_CorCompleta+","     // facilita o algoritmo
*
C_CorCompleta := STRTRAN(C_CorCompleta,",","#",,N_Indice+1)
N_Pos    := AT(",",C_CorCompleta)
C_CorPar := LEFT(C_CorCompleta,N_Pos-1)
N_Pos    := RAT("#",C_CorPar)
C_CorPar := SUBSTR(C_CorPar,N_Pos+1)
*
IF .NOT. "/" $ C_CorPar
   C_CorPar := C_CorPar + "/"         // facilita o algoritmo
ENDIF
*
IF L_Frente ==  NIL
   C_Cor := C_CorPar
ELSEIF L_Frente
   C_Cor := LEFT(C_CorPar,AT("/",C_CorPar)-1)
ELSE
   C_Cor := SUBSTR(C_CorPar,AT("/",C_CorPar)+1)
ENDIF
*
RETURN C_Cor
*
***********************
STATIC FUNCTION TecFunc ( N_Largura, V_2RegiaoBotao )
*
LOCAL N_Quant, N_Cont, N_Brancos := 2
LOCAL C_LinhaTxt, C_LinhaWin, N_2LinBotoes
LOCAL C_TxtBotao, L_BotaoNoPadrao
*
* calcular acumulado do tamanho das mensagens
*
FOR N_Cont := 1 TO LEN(V_2RegiaoBotao)
    C_TxtBotao := V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_COMANDO]
    *
    L_BotaoNoPadrao := .T.
    IF LEFT(UPPER(C_TxtBotao),2) $ "I=/A=/E=/C=/X=/L=/P=/"
       IF .NOT. C_TxtBotao == "I=incluir" .AND. ;
          .NOT. C_TxtBotao == "A=alterar" .AND. ;
          .NOT. C_TxtBotao == "E=excluir" .AND. ;
          .NOT. C_TxtBotao == "C=consultar" .AND. ;
          .NOT. C_TxtBotao == "X=anexar" .AND. ;
          .NOT. C_TxtBotao == "L=listar" .AND. ;
          .NOT. C_TxtBotao == "P=procurar"
          L_BotaoNoPadrao := .F.
       ENDIF
    ENDIF
    *
    IF LEFT(XUPPER(C_TxtBotao),7) $ "ESPAÇO=/"
        IF LEN(C_TxtBotao) == 13
            IF .NOT. C_TxtBotao == "Espaço=marcar"
                L_BotaoNoPadrao := .F.
          ENDIF
       ELSEIF LEN(C_TxtBotao) == 23
        IF .NOT. C_TxtBotao == "Espaço=marcar/desmarcar"
            L_BotaoNoPadrao := .F.
          ENDIF
       ELSE
          L_BotaoNoPadrao := .F.
       ENDIF
    ENDIF
    *
    IF LEFT(XUPPER(C_TxtBotao),16) $ "BARRA DE ESPAÇO=/"
        IF LEN(C_TxtBotao) == 22
           IF .NOT. C_TxtBotao == "Barra de espaço=marcar"
              L_BotaoNoPadrao := .F.
           ENDIF
        ELSEIF LEN(C_TxtBotao) == 32
           IF .NOT. C_TxtBotao == "Barra de espaço=marcar/desmarcar"
              L_BotaoNoPadrao := .F.
           ENDIF
        ELSE
           L_BotaoNoPadrao := .F.
        ENDIF
     ENDIF
     *
    IF .NOT. L_BotaoNoPadrao
       *
       // Esta função GS_SERIE(), foi criada temporariamente com a finalidade de testar
       // a serie no janela.prg (testa TEXTO do botão)
       IF GS_SERIE() # NIL
        IF GS_SERIE() $ SERIE_TESTE()+;
                        SERIE_DESENVOLVIMENTO()+;
                        SERIE_SAN()+;
                        SERIE_UNIDADE_CONVERSAO()+;
                        SERIE_PRODUCAO()
           ///? MEMVAR->ERRO_BOTAO_FORA_DO_PADRAO
        ELSE
           LOGAFONT_GENERICO(4,"JAN",NIL,NIL,"Erro 7: Botão fora do padrão: "+C_TxtBotao)
        ENDIF
     ELSE
        LOGAFONT_GENERICO(4,"JAN",NIL,NIL,"Erro 7: Botão fora do padrão: "+C_TxtBotao)
     ENDIF
     *
    ENDIF
    *
    V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1] := " "+C_TxtBotao+" "
NEXT
*
N_2LinBotoes := 0
N_Quant := LEN(V_2RegiaoBotao)
N_Cont := 1
DO WHILE N_Cont <= N_Quant
   N_2LinBotoes++
   *
   * Acumulado de texto dos botões para a versão TEXTO. A quebra de linha
   * é baseada nesta variável, para que os botões fiquem sempre na mesma linha,
   * seja qual for a versão usada.
   C_LinhaTxt := ""
   * Acumulado de texto dos botões para a versão GRÁFICA. Serve para evitar
   * espaçamento diferente de botões na versão GRÁFICA.
   C_LinhaWin := ""
   CalculaRegiaoBotao(N_2LinBotoes,N_Cont,@V_2RegiaoBotao,@C_LinhaTxt,@C_LinhaWin)
   *
   N_Cont++
   DO WHILE IIF(N_Cont <= N_Quant,  ;
      N_Brancos+LEN(C_LinhaTxt+V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1])+2 <= N_Largura , .F.)
      *
      C_LinhaTxt += SPACE(N_Brancos)
      C_LinhaWin += SPACE(N_Brancos)
      CalculaRegiaoBotao(N_2LinBotoes,N_Cont,@V_2RegiaoBotao,@C_LinhaTxt,@C_LinhaWin)
      *
      N_Cont++
   ENDDO
ENDDO
*
RETURN N_2LinBotoes
*
****************************
STAT PROC CalculaRegiaoBotao(N_Nulin,N_Cont,;
                            V_2RegiaoBotao,C_LinhaTxt,C_LinhaWin)   // por referência
****************************
LOCAL N_Pos
LOCAL C_Destaque, C_TeclaAtalho, N_Pos_Destaque := 1
LOCAL C_TxtTratado_2 := V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1]
LOCAL N_Keyboard
*
IF (N_Pos := AT("=",C_TxtTratado_2)) == 0
   IF SOB_MODO_GRAFICO()
        ALARME("M28760","Barra de ações inválida")
        ? MEMVAR->ERRO_FALTA_SINAL_DE_IGUAL
   ELSE
        ALERT("Barra de ações inválida")
        ? MEMVAR->ERRO_FALTA_SINAL_DE_IGUAL
   ENDIF
ENDIF
*
C_Destaque := LEFT(C_TxtTratado_2,N_Pos-1)
C_Destaque := SUBSTR(C_Destaque,2)  // tirar o espaço inicial
*
C_TeclaAtalho := XUPPER(C_Destaque)
IF LEN(C_TeclaAtalho)==1 .AND. (ISALPHA(C_TeclaAtalho) .OR. ISDIGIT(C_TeclaAtalho))
   * Parte esquerda do comando é uma só letra ou só um número
   *
   N_Keyboard := ASC(C_TeclaAtalho)
   *
   * O SUBSTR(,4) é para tirar da busca o " I="   (espaço + letra + igual)
   N_Pos := AT(C_TeclaAtalho,XUPPER(SUBSTR(C_TxtTratado_2,4)))
   IF N_Pos # 0
    IF SOB_MODO_GRAFICO() // reposicionar o sublinhado dentro do botão
        * Remover a letra e o sinal de igual.
        * Colocar a primeira letra em maiúsculo.
        C_TxtTratado_2 := " "+XUPPER(SUBSTR(C_TxtTratado_2,4,1))+SUBSTR(C_TxtTratado_2,5)
        N_Pos := N_Pos+1   // acrescentar o " " também na posição
        *
         * Pegar novamente a tecla de destaque, pois pode estar em minúsculo...
         C_Destaque := SUBSTR(C_TxtTratado_2,N_Pos,1)
         *
         N_Pos_Destaque := N_Pos-1
      ENDIF
   ELSE
      #IFDEF _TESTE   //!! DEPOIS PASSAR A DAR ERRO TAMBï¿½M NA VERSï¿½O DE PRODUCAO
          ? MEMVAR->HOTKEY_NAO_CONSEGUIU_SER_DEDUZIDA_01
      #ENDIF
   ENDIF
   *
ELSEIF C_TeclaAtalho=="ESC"
    N_Keyboard := K_ESC
 ELSEIF C_TeclaAtalho=="ENTER"
    N_Keyboard := K_ENTER
 ELSEIF C_TeclaAtalho=="ESPAÇO"   .OR. ;
        C_TeclaAtalho=="BARRA DE ESPAÇO"
   N_Keyboard := ASC(" ")
ELSEIF C_TeclaAtalho=="F1"
   N_Keyboard := K_F1
ELSEIF C_TeclaAtalho=="F2"
   N_Keyboard := K_F2
ELSEIF C_TeclaAtalho=="F3"
   N_Keyboard := K_F3
ELSEIF C_TeclaAtalho=="F4"
   N_Keyboard := K_F4
ELSEIF C_TeclaAtalho=="F5"
   N_Keyboard := K_F5
ELSEIF C_TeclaAtalho=="F6"
   N_Keyboard := K_F6
ELSEIF C_TeclaAtalho=="F7"
   N_Keyboard := K_F7
ELSEIF C_TeclaAtalho=="F8"
   N_Keyboard := K_F8
ELSEIF C_TeclaAtalho=="F9"
   N_Keyboard := K_F9
ELSEIF C_TeclaAtalho=="F10"
   N_Keyboard := K_F10
ELSEIF C_TeclaAtalho=="F11"
   N_Keyboard := K_F11
ELSEIF C_TeclaAtalho=="F12"
   N_Keyboard := K_F12
ELSE
   #IFDEF _TESTE   //!! DEPOIS PASSAR A DAR ERRO TAMBï¿½M NA VERSï¿½O DE PRODUCAO
       ? MEMVAR->HOTKEY_NAO_CONSEGUIU_SER_DEDUZIDA_02
   #ENDIF
ENDIF
*
C_LinhaTxt += V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1]
C_LinhaWin += C_TxtTratado_2
*
V_2RegiaoBotao[N_Cont,_BOTAO_LIN_INICIAL    ] := N_Nulin
IF SOB_MODO_GRAFICO()
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_INICIAL    ] := LEN(C_LinhaWin)-LEN(C_TxtTratado_2)
ELSE
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_INICIAL    ] := LEN(C_LinhaTxt)-LEN(C_TxtTratado_2)
ENDIF
V_2RegiaoBotao[N_Cont,_BOTAO_LIN_FINAL      ] := N_Nulin
IF SOB_MODO_GRAFICO()
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_FINAL      ] := LEN(C_LinhaWin)-1
ELSE
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_FINAL      ] := LEN(C_LinhaTxt)-1
ENDIF
V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_2] := C_TxtTratado_2
IF SOB_MODO_GRAFICO()
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_DESTAQUE   ] := LEN(C_LinhaWin)-LEN(C_TxtTratado_2)+N_Pos_Destaque
ELSE
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_DESTAQUE   ] := LEN(C_LinhaTxt)-LEN(C_TxtTratado_2)+N_Pos_Destaque
ENDIF
V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_DESTAQUE ] := C_Destaque
V_2RegiaoBotao[N_Cont,_BOTAO_INKEY_DESTAQUE ] := N_Keyboard
V_2RegiaoBotao[N_Cont,_BOTAO_INKEY_DESTAQUE_CASE ] := InKeyCaseAlternativo(N_Keyboard)
*
******************
FUNCTION StrToVet_ ( C_Titulo )
*
LOCAL VC_Resultado , N_Tamanho , N_InicioLin
*
* tranforma um string com separador de linhas ";" em um vetor de caracteres
*
N_InicioLin  := 1
VC_Resultado := {}
DO WHILE LEN(SUBSTR(C_Titulo,N_InicioLin)) # 0
   IF (N_Tamanho := AT(";",SUBSTR(C_Titulo,N_InicioLin)) ) == 0
       N_Tamanho := LEN(SUBSTR(C_Titulo,N_InicioLin))+1
   ENDIF
   AADD(VC_Resultado,SUBSTR(C_Titulo,N_InicioLin,N_Tamanho-1))
   N_InicioLin := N_InicioLin + N_Tamanho
ENDDO
*
RETURN VC_Resultado
*
***************************
FUNC CABEC_TESTE_AUTOMATICO  (L_EXIBE_AJUDA_NEW)
***************************
//!! CODIGO QUEBRA GALHO
LOCAL C_SERIE
//!!

STATIC L_EXIBE_AJUDA := .F.
IF L_EXIBE_AJUDA_NEW # NIL
    L_EXIBE_AJUDA := L_EXIBE_AJUDA_NEW
ENDIF

//!! CODIGO QUEBRA GALHO
C_SERIE := "XXXXX"
IF SELECT("XXCONG") # 0
    C_SERIE := XXCONG->SERIE
ENDIF
IF SERIE_EQUIPE_DE_TESTE() .OR. ;   // Equipe de teste
    "/"+TRIM(GS_CDUSUA())+"/" $ "/RICARDO.REGIS/"
    L_EXIBE_AJUDA := .T.
ENDIF
//!! CODIGO QUEBRA GALHO
*
RETURN L_EXIBE_AJUDA
*
    // *
// ******************
// PROC MudeSubtitulo (VX_Janela,C_SubCabec_Novo)
// ******************
// LOCAL N_Cont, N_Largura, N_LinImp
// LOCAL N_TAM_CAB_ANT := LEN(VC_Titulo)
// LOCAL C_Cor_Aux
// LOCAL N_POS1, N_POS2
// LOCAL L_EH_PROGRESSBAR := GetProgressBar( VX_Janela ) != 0
// *
// IF C_TelaCoberta == NIL
//    ? MEMVAR->JANELA_AINDA_NAO_ATIVADA_NENHUMA_VEZ
// ENDIF
// *
// #IFDEF _TESTE
// IF L_EH_PROGRESSBAR                          /// Janela do Tipo ProgressBar, sï¿½ pode ser
//    ? MEMVAR->JANELA_E_DO_TIPO_PROGRESSBAR    /// Manipulada pela funï¿½ï¿½o, ANDAMENTO_PROGRESSBAR!
// ENDIF
// #ENDIF
// *
// * nï¿½o imprime nada, sï¿½ monta a matriz de cabeï¿½alho devidamente formatado
// *
// C_SubCabec_Novo := STRTRAN(C_SubCabec_Novo,"%t",C_Cabec)
// C_SubCabec_Novo := STRTRAN(C_SubCabec_Novo,"%T",C_Cabec)
// VC_Titulo := StrToVet_(C_SubCabec_Novo)
// *
// IF N_TAM_CAB_ANT # LEN(VC_Titulo)
//    ? MEMVAR->CABECALHO_COM_NUMERO_DE_LINHAS_DIFERENTE
// ENDIF
// *
// * montar cabeï¿½alho
// *
// N_Largura := N_Col2Livre-N_Col1Livre+1
// N_LinImp  := N_LinIni+N_MargemSuperior
// *
// C_Cor_Aux := SETCOLOR(C_CorJan)  // mudar para a cor da janela
// FOR N_Cont := 1 TO LEN(VC_Titulo)
//     SETPOS(N_LinImp-1+N_Cont,N_Col1Livre+N_DeslocaCabecalho)
//     DISPOUT(PADC(VC_Titulo[N_Cont],N_Largura-N_DeslocaCabecalho))
// NEXT
// SETCOLOR(C_Cor_Aux)
// *
// ********************
// FUNC GetPilhaJanelas
// ********************
// RETURN V_PilhaJanelas     // rotina chamadora nï¿½o pode alterar este vetor...
// *
// ******************
FUNC GetCdTelaTopo
******************
LOCAL C_CdTelaTopo := SPACE(06)   // Tnnnnn
LOCAL VX_Janela
*
* Se ALARME() tiver uma tela anterior a ela
IF LEN(V_PilhaJanelas) > 0
    VX_Janela := V_PilhaJanelas[LEN(V_PilhaJanelas),2]  // Janela do topo
    C_CdTelaTopo := C_CdTela
ENDIF
*
RETURN C_CdTelaTopo
*
**************************
FUNC GetCdGET_ou_Menu_Topo
**************************
LOCAL C_CdGET_ou_Menu_Atual := SPACE(06)  // formato Tnnnnn ou Mnnnnn ou Cnnnnn
LOCAL VX_Janela
*
* Se ALARME() tiver uma tela anterior a ela
IF LEN(V_PilhaJanelas) > 0
   VX_Janela := V_PilhaJanelas[LEN(V_PilhaJanelas),2]  // Janela do topo
   IF N_TP_Jan == _JAN_ENTRADA_10
      C_CdGET_ou_Menu_Atual := GetCdGET_Atual_Entrada(VX_Janela)
   ELSEIF N_TP_Jan == _JAN_MENU_VERT
      C_CdGET_ou_Menu_Atual := GetCdOpcao_Atual_Menuvert(VX_Janela)
   ENDIF
ENDIF
RETURN C_CdGET_ou_Menu_Atual
*
***********************

// // FRAN: At the moment, events are internally managed by GTNAP
// STATIC FUNCTION TrataEventos ( VX_Janela )
//     LOCAL L_FechouComAutoClose := .F.

//     @ 22, 0 SAY ""
//     OutStd("TrataEventos ( VX_Janela ) function called")

//     RETURN L_FechouComAutoClose

*
***********************
STATIC FUNCTION TrataEventos ( VX_Janela )
*
// #INCLUDE "set.ch"
*
LOCAL L_Mais, N_Tecla, N_Pos
LOCAL N_mRow, N_mCol, N_RegiaoMouse, N_Keyboard

LOCAL /*N_PaintRefresh_Old, */X_Retorno_Eval, L_FechouComAutoClose := .F.
LOCAL V_Botao, V_Imagem, N_Pos_Acao

//
// FRAN: This code is not necesary
//
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       N_PaintRefresh_Old := WVW_SetPaintRefresh(_REPAINT_DEFAULT)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//     #erro "Código não adaptado para esta plataforma"
// #endif
*

//
//  FRAN: Events are managed inside a GTNAP/NAppGUI window.
//
IF SOB_MODO_GRAFICO()

    X_Retorno_Eval := NAP_CUALIB_LAUNCH_MODAL({||.T.}, {||.T.})

    IF X_Retorno_Eval == 1000
        L_FechouComAutoClose = .T.
    ENDIF

ELSE

//
// FRAN: Manual event management ONLY in TEXT terminal versions
//
L_Mais := .T.                                    // simula um DO UNTIL
*
DO WHILE L_Mais
   *
   * InKey_(.T.) é igual ao INKEY(0), mas ativa SET KEY"s
   N_Tecla := Inkey_(.T.,4)
   *
   DO CASE
      CASE N_Tecla == K_LBUTTONDOWN .OR. ;
           N_Tecla == K_LDBLCLK     .OR. ;
           N_Tecla == K_RBUTTONDOWN .OR. ;
           N_Tecla == K_RDBLCLK
           *
           N_mRow := mRow()
           N_mCol := mCol()
           N_RegiaoMouse := RegiaoJanela_(VX_Janela,N_mRow,N_mCol,;
                                          N_Lin1Livre,N_Col1Livre,;
                                          N_Lin2Livre,N_Col2Livre,;
                                          @N_Keyboard,@V_Botao,@V_Imagem)
           *
           // #INCLUDE "mousecua.ch"
           IF (N_RegiaoMouse == BOTAO_IDENTIFICADO .OR. ;  // N_Keyboard preenchido
               N_RegiaoMouse == BOTAO_NAO_IDENTIFICADO)    // N_Keyboard não preenchido
                //
                // Fran: Only TEXT terminal version enter here
                //
                //   IF SOB_MODO_GRAFICO()
                //      ? MEMVAR->MODO_GRAFICO_NAO_USA_ESTE_TRECHO_DE_CODIGO
                //   ENDIF

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
              ENDIF
           ELSEIF N_RegiaoMouse == SOBRE_IMAGEM
              *
            //
            // Fran: Only TEXT terminal version enter here
            //
            //   #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
            //      IF SOB_MODO_GRAFICO()
            //         WVW_SetPaintRefresh(N_PaintRefresh_Old)
            //      ENDIF
            //   #elif defined(__PLATFORM__LINUX)
            //      // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
            //   #else
            //     #erro "Código não adaptado para esta plataforma"
            //   #endif

              X_Retorno_Eval := EVAL(V_Imagem[_IMAGEM_BLOCO_ACAO])

              * Logar uso de imagens, para ter estatística de uso
              IF V_Imagem[_IMAGEM_CDBOTAO] # NIL  // Se for CUA 2.0
                 LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Imagem[_IMAGEM_CDBOTAO],;
                                              C_CdTela,"Imagem "+V_Imagem[_IMAGEM_ARQUIVO])   // Log de uso de imagem no sistema
              ENDIF

            //
            // Fran: Only TEXT terminal version enter here
            //
            //   #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
            //      IF SOB_MODO_GRAFICO()
            //         WVW_SetPaintRefresh(_REPAINT_DEFAULT)
            //      ENDIF
            //   #elif defined(__PLATFORM__LINUX)
            //      // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
            //   #else
            //     #erro "Código não adaptado para esta plataforma"
            //   #endif
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
           ELSEIF N_Keyboard # NIL
              HB_KeyPut(N_Keyboard)
           ENDIF
           *
      OTHER
           IF N_Tecla == K_ESC
              L_Mais := .F.
           ELSE
              N_Pos_Acao := ASCAN(V_LstAcoes,{|V_Acao| ;
                                  V_Acao[_ACAO_KEYBOARD]==N_Tecla .OR. ;
                                  V_Acao[_ACAO_KEYBOARD_CASE]==N_Tecla})
              IF N_Pos_Acao # 0

                //
                // Fran: Only TEXT terminal version enter here
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

                 X_Retorno_Eval := EVAL(V_LstAcoes[N_Pos_Acao,_ACAO_BLOCO_ACAO])

                //
                // Fran: Only TEXT terminal version enter here
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
                 IF V_LstAcoes[N_Pos_Acao,_ACAO_AUTOCLOSE]
                    DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                    IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                       ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                    ENDIF
                    IF X_Retorno_Eval
                       L_Mais := .F.
                       L_FechouComAutoClose := .T.
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
   ENDCASE
   *
ENDDO
*
//
// Fran: Only TEXT terminal version enter here
//
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       WVW_SetPaintRefresh(N_PaintRefresh_Old)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
// #erro "Código não adaptado para esta plataforma"
// #endif
*

ENDIF  //  SOB_MODO_GRAFICO()

RETURN L_FechouComAutoClose
// *
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    ********************************
//    STATIC FUNC COORDENADASDRAWLABEL()
//    ********************************
//    RETURN {32,91,32,108}
//    *
//    ***************************************
//    STAT FUNC CoordenadasSeparadorSubtitulo(VX_Janela)
//    ***************************************
//    LOCAL N_LinIniTraco := N_LinIni+LEN(VC_Titulo)-1
//    LOCAL N_ColIniTraco := N_ColIni
//    LOCAL N_LinFinTraco := N_LinIniTraco
//    LOCAL N_ColFinTraco := N_ColFin
//    *
//    RETURN {N_LinIniTraco,N_ColIniTraco,N_LinFinTraco,N_ColFinTraco}
//    *
//    ***********************************
//    STAT FUNC DesenhaSeparadorSubtitulo(VX_Janela)
//    ***********************************
//    LOCAL N_LinIniTraco := N_LinIni+LEN(VC_Titulo)-1
//    LOCAL N_ColIniTraco := N_ColIni
//    LOCAL N_LinFinTraco := N_LinIniTraco
//    LOCAL N_ColFinTraco := N_ColFin
//    LOCAL N_DeslocaVertical, N_AlturaEmPixels
//    LOCAL N_TelaHeight := TelaPrincipalHeight()
//    *
//    * Subir o sublinhado, para "descolar" da margem inferior do botï¿½o.
//    *
//    IF N_TelaHeight >= 1024 .OR. ;  // resoluï¿½ï¿½o VERTICAL
//       N_TelaHeight >=  960
//       N_DeslocaVertical := -1
//       N_AlturaEmPixels  :=  1
//    ELSEIF N_TelaHeight >=  864 .OR. ;
//           N_TelaHeight >=  768
//       N_DeslocaVertical := -1
//       N_AlturaEmPixels  :=  1
//    ELSEIF N_TelaHeight >= 600
//       N_DeslocaVertical :=  0
//       N_AlturaEmPixels  :=  1
//    ELSE
//       N_DeslocaVertical :=  0
//       N_AlturaEmPixels  :=  1
//    ENDIF
//    *
//    RETURN {|| Wvw_DrawLine(N_WindowNum,N_LinIniTraco,N_ColIniTraco,;
//                                        N_LinFinTraco,N_ColFinTraco,;
//               0,2,2,;   // 0=horizontal, 2=plain e 2=bottom
//               NIL,N_AlturaEmPixels,NIL,;
//               {N_DeslocaVertical,0,N_DeslocaVertical,0}) }
//    *


//    //   --> SCROLL_V_H
//    *******************************
//    STAT PROC DesenhaScrollVertical(VX_Janela)
//    *******************************
//    *
//    IF .NOT. L_RolaCima .AND. .NOT. L_RolaBaixo
//       * Limpar eventual barra de rolagem existente antes.
//       WVW_DrawColorRect(N_WindowNum,;
//                         N_Lin1Livre,N_Col2Livre+1,;
//                         N_Lin2Livre,N_Col2Livre+2,;
//                         {-2,+5,+4,+6},;
//                         Wvw_GetRGBColor(HB_ColorTON("W")))
//       * limpar ï¿½rea da barra de rolagem
//       N_LinMarcadorVertical := 0
//    ELSE
//       * montar seta para cima
//       WVW_DrawScrollButton(N_WindowNum,;
//                            N_Lin1Livre,N_Col2Livre+1,N_Lin1Livre,N_Col2Livre+2,;
//                            {-1,+5,+3,+6},1,.F.)
//       *
//       IF .NOT. L_RolaCima      // no topo
//          N_LinMarcadorVertical := N_Lin1Livre+1
//       ELSEIF .NOT. L_RolaBaixo // embaixo
//          N_LinMarcadorVertical := N_Lin2Livre-1
//       ELSE                 // no meio
//          N_LinMarcadorVertical := ROUND((N_Lin1Livre+N_Lin2Livre)/2,0)
//       ENDIF
//       *
//       * Limpar marcador vertical impressos antes.
//       WVW_DrawColorRect(N_WindowNum,;
//                         N_Lin1Livre+1,N_Col2Livre+1,;
//                         N_Lin2Livre-1,N_Col2Livre+2,;
//                         {-2,+5,+4,+6},;
//                         Wvw_GetRGBColor(HB_ColorTON("W")))
//       *
//       * montar barra de rolagem
//       Wvw_DrawBoxGroup(N_WindowNum,N_Lin1Livre+1,N_Col2Livre+1,;
//                                    N_Lin2Livre-1,N_Col2Livre+2,;
//                                    {0,+7,+1,+3})
//       *
//       * montar marcador vertical
//       Wvw_DrawBoxRaised(N_WindowNum,N_LinMarcadorVertical,N_Col2Livre+1,;
//                                     N_LinMarcadorVertical,N_Col2Livre+2,;
//                                     {0,+7,+1,+3})
//       *
//       * montar seta para baixo
//       WVW_DrawScrollButton(N_WindowNum,;
//                            N_Lin2Livre,N_Col2Livre+1,N_Lin2Livre,N_Col2Livre+2,;
//                            {-1,+5,+3,+6},3,.F.)
//       *
//    ENDIF
//    *
//    ***********************************
//    STAT FUNC CoordenadasScrollVertical(VX_Janela)
//    ***********************************
//    * Teve ser ser acrescentado o "+1" abaixo, na coluna final.
//    RETURN {N_Lin1Livre,N_Col2Livre+1,N_Lin2Livre,N_Col2Livre+2+1}
//    *
//    *********************************
//    STAT PROC DesenhaScrollHorizontal(VX_Janela)
//    *********************************
//    IF .NOT. L_RolaEsquerda .AND. .NOT. L_RolaDireita
//       * Limpar eventual barra de rolagem existente antes.
//       WVW_DrawColorRect(N_WindowNum,;
//                         N_Lin2Livre+1,N_Col1Livre,;
//                         N_Lin2Livre+1,N_Col2Livre,;
//                         {+2,+1,+7,+1},;
//                         Wvw_GetRGBColor(HB_ColorTON("W")))
//       N_ColMarcadorHorizontal := 0
//    ELSE
//       * montar seta para esquerda
//       WVW_DrawScrollButton(N_WindowNum,;
//                            N_Lin2Livre+1,N_Col1Livre,N_Lin2Livre+1,N_Col1Livre+1,;
//                            {+2,+0,+7,+2},2,.F.)
//       *
//       IF .NOT. L_RolaEsquerda    // no inicio da linha
//          N_ColMarcadorHorizontal := N_Col1Livre+2
//       ELSEIF .NOT. L_RolaDireita // no final da linha
//          N_ColMarcadorHorizontal := N_Col2Livre-2-1
//       ELSE                 // no meio
//          N_ColMarcadorHorizontal := ROUND((N_Col1Livre+N_Col2Livre)/2,0)
//       ENDIF
//       *
//       * Limpar marcador horizontal impresso antes.
//       WVW_DrawColorRect(N_WindowNum,;
//                         N_Lin2Livre+1,N_Col1Livre+2,;
//                         N_Lin2Livre+1,N_Col2Livre-2,;
//                         {+2,+1,+7,+1},;
//                         Wvw_GetRGBColor(HB_ColorTON("W")))
//       *
//       * montar barra de rolagem
//       Wvw_DrawBoxGroup(N_WindowNum,N_Lin2Livre+1,N_Col1Livre+2,;
//                                    N_Lin2Livre+1,N_Col2Livre-2,;
//                                    {+4,+3,+4,-2})
//       *
//       * montar marcador horizontal
//       Wvw_DrawBoxRaised(N_WindowNum,N_Lin2Livre+1,N_ColMarcadorHorizontal,;
//                                     N_Lin2Livre+1,N_ColMarcadorHorizontal+1,;
//                                     {+4,+3,+4,-2})
//       *
//       * montar seta para direita
//       WVW_DrawScrollButton(N_WindowNum,;
//                            N_Lin2Livre+1,N_Col2Livre-1,N_Lin2Livre+1,N_Col2Livre,;
//                            {+2,+0,+7,+2},4,.F.)
//       *
//    ENDIF
//    *
//    *************************************
//    STAT FUNC CoordenadasScrollHorizontal(VX_Janela)
//    *************************************
//    * Teve ser ser acrescentado o "+1" abaixo, na linha final.
//    RETURN { N_Lin2Livre+1,N_Col1Livre,N_Lin2Livre+1+1,N_Col2Livre }
//    *

***************************
STATIC PROC DesenhaDrawLabe(VX_Janela)
***************************
NAP_CUALIB_LABEL(32, 57, space(26-(LEN(INFO_VERSAO[8])+3))+"Versão "+INFO_VERSAO[5]+"."+INFO_VERSAO[6]+;
                                                                            CHR(VAL(INFO_VERSAO[7]))+"(b"+;
                                                                            INFO_VERSAO[8]+") - S"+TRIM(SERIE_ASPEC_INI()), .T., .F., nil)
NAP_CUALIB_LABEL(33, 39, "www.aspec.com.br   Aspec "+chr(184)+"1993-"+STR(YEAR(DATE()),4,0)+". Todos os direitos reservados", .T., .F., nil)


// RETURN {||WVW_DRAWLABEL(N_WindowNum,32,57,space(26-(LEN(INFO_VERSAO[8])+3))+"Versão "+INFO_VERSAO[5]+"."+INFO_VERSAO[6]+;
//                                                                                                         CHR(VAL(INFO_VERSAO[7]))+"(b"+;
//                                                                                                         INFO_VERSAO[8]+") - S"+TRIM(SERIE_ASPEC_INI())),;
//             WVW_DRAWLABEL(N_WindowNum,33,39,"www.aspec.com.br   Aspec "+chr(184)+"1993-"+STR(YEAR(DATE()),4,0)+". Todos os direitos reservados")}
//    *
//    ***********************
//    STAT FUNC DesenhaImagem(VX_Janela,N_Cont)
//    ***********************
//    LOCAL C_ArquivoImagem  := V_LstImagens[N_Cont,_IMAGEM_ARQUIVO    ]
//    LOCAL N_LinIniImagem   := N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_INICIAL]
//    LOCAL N_ColIniImagem   := N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_INICIAL]
//    LOCAL N_LinFinImagem   := N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_FINAL  ]
//    LOCAL N_ColFinImagem   := N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_FINAL  ]
//    *
//    RETURN {|| Wvw_DrawImage(N_WindowNum,;
//                             N_LinIniImagem,N_ColIniImagem,;
//                             N_LinFinImagem,N_ColFinImagem,;
//                             C_ArquivoImagem,;
//                             .T.,;  // .T.=tight (tamanho da imagem serï¿½ ajustado)
//                             .T.)}  // .T.=transparente
//    *
   ****************************
   STAT PROC DesenhaBoxEmbutida(VX_Janela)
   ****************************
//    WVW_SetPen(0,0,rgb(210,1210,210))
//    Wvw_DrawBoxGroup(N_WindowNum,N_LinIni,N_ColIni,N_LinFin,N_ColFin)
    RETURN

   *
   *****************
   PROC AddGuiObject( VX_Janela,bGuiControl,V_Coordenadas,L_Rastrear,bWhenBlock )
   *****************
   DEFAULT L_Rastrear TO .F.
   * DEFAULT bWhenBlock TO {||.T.}  // bWhenBlock sem uso por enquanto
   AADD(aGuiObjects,{ bGuiControl, V_Coordenadas, L_Rastrear})  // bWhenBlock})
   *
//    **************
//    PROC WVW_Paint (nWinNum)  // A WVW chama esta rotina de forma automï¿½tica.
//    **************            // ï¿½ uma funï¿½ï¿½o de callback. Nï¿½o pode ser STATIC.
//    LOCAL L_DebuggerInativo := .T.
//    LOCAL N_Pos, VX_Janela, V_ItemGui, L_MainCoord_Aux
//    LOCAL V_PaintRect := WVW_GetPaintRect(nWinNum)
//    LOCAL L_Sobrepos_Item, N_Cont
//    *
//    #IFDEF _TESTE
//        LOCAL C_ProcName, N_Pilha := 0
//        DO WHILE .NOT. EMPTY((C_ProcName := PROCNAME(N_Pilha)))
//           IF "TDEBUGGER" $ C_ProcName .OR. ; // xHarbour 1.0.0
//              "HBDEBUGGER" $ C_ProcName       // xHarbour 1.2.1
//              L_DebuggerInativo := .F.
//           ENDIF
//           N_Pilha++
//        ENDDO
//    #ENDIF
//    *
//    IF L_GuiObjectsAtivos .AND. L_DebuggerInativo
//       N_Pos := ASCAN(V_PilhaJanelas,{|V_SubVet|V_SubVet[1]+IIF(.NOT. EH_PRODUCAO(),1,0)==nWinNum})
//       *
//       * ï¿½ necessï¿½rio testar se N_POS ï¿½ diferente de 0, pois a WVW_PAINT
//       * ï¿½ chamada automaticamente no inï¿½cio do sistema,
//       * quando nem sequer nenhuma tela foi criada ainda.
//       *
//       IF N_Pos # 0
//          VX_Janela := V_PilhaJanelas[N_Pos,2]
//          *
//          L_MainCoord_Aux := WVW_SetMainCoord(L_MainCoord_Atu)
//          *
//          * Ao percorrer o vetor do final para o inï¿½cio, ï¿½ desenhado
//          * primeiro o corpo do GETs e o GRID dos browses, o que dï¿½ uma
//          * melhor percepï¿½ao visual, pois ï¿½ o local onde o usuï¿½rio estï¿½ olhando.
//          *
//          FOR N_Cont := LEN(aGuiObjects) TO 1 STEP -1
//              V_ItemGui := aGuiObjects[N_Cont]
//              * IF EVAL( V_ItemGui[4] )   // bWhenBlock
//              * ENDIF
//              *
//              * Sï¿½ imprimir elemento grï¿½fico se o mesmo estiver na ï¿½rea a ser
//              * redesenhada, por questï¿½o de performance.
//              *
//              L_Sobrepos_Item := TEVE_SOBREPOSICAO(VX_Janela,V_PaintRect,;
//                                  { V_ItemGui[2,1],V_ItemGui[2,2],;
//                                    V_ItemGui[2,3],V_ItemGui[2,4] })
//              *
//              IF L_Sobrepos_Item
//                 EVAL( V_ItemGui[1] )
//              ENDIF
//          NEXT
//          *
//          WVW_SetMainCoord(L_MainCoord_Aux)
//          *
//       ENDIF
//    ENDIF
//    *
//    **********************
//    FUNC TEVE_SOBREPOSICAO (VX_Janela,V_COORD_PAINT, V_COORD_ITEM)
//    **********************
//    LOCAL L_Sobrepos_Item
//    LOCAL N_L_INI_PAINT := V_COORD_PAINT[1]
//    LOCAL N_C_INI_PAINT := V_COORD_PAINT[2]
//    LOCAL N_L_FIN_PAINT := V_COORD_PAINT[3]
//    LOCAL N_C_FIN_PAINT := V_COORD_PAINT[4]
//    LOCAL N_L_INI_ITEM  := V_COORD_ITEM[1]-N_LinIni // - Tornar as coordenadas
//    LOCAL N_C_INI_ITEM  := V_COORD_ITEM[2]-N_ColIni //   relativas ï¿½ janela
//    LOCAL N_L_FIN_ITEM  := V_COORD_ITEM[3]-N_LinIni //   (como a PaintRect()
//    LOCAL N_C_FIN_ITEM  := V_COORD_ITEM[4]-N_ColIni //   retorna)
//    *
//    * Testar se alguma coluna do "paint" corta alguma linha do "item"
//    *
//    *         +-------+
//    *       c1ï¿½ Paint ï¿½a1
//    *  +------+-------+-+
//    *  ï¿½ Item ï¿½       ï¿½ ï¿½
//    *  +------+-------+-+
//    *       d1ï¿½       ï¿½b1
//    *         +-------+
//    *
//    #DEFINE _INTERSECAO_A1 ;
//              (N_L_INI_PAINT <= N_L_INI_ITEM  .AND. N_L_INI_ITEM  <= N_L_FIN_PAINT  .AND. ;
//               N_C_INI_ITEM  <= N_C_FIN_PAINT .AND. N_C_FIN_PAINT <= N_C_FIN_ITEM)
//    #DEFINE _INTERSECAO_B1 ;
//              (N_L_INI_PAINT <= N_L_FIN_ITEM  .AND. N_L_FIN_ITEM  <= N_L_FIN_PAINT  .AND. ;
//               N_C_INI_ITEM  <= N_C_FIN_PAINT .AND. N_C_FIN_PAINT <= N_C_FIN_ITEM)
//    #DEFINE _INTERSECAO_C1 ;
//              (N_L_INI_PAINT <= N_L_INI_ITEM  .AND. N_L_INI_ITEM  <= N_L_FIN_PAINT  .AND. ;
//               N_C_INI_ITEM  <= N_C_INI_PAINT .AND. N_C_INI_PAINT <= N_C_FIN_ITEM)
//    #DEFINE _INTERSECAO_D1 ;
//              (N_L_INI_PAINT <= N_L_FIN_ITEM  .AND. N_L_FIN_ITEM  <= N_L_FIN_PAINT  .AND. ;
//               N_C_INI_ITEM  <= N_C_INI_PAINT .AND. N_C_INI_PAINT <= N_C_FIN_ITEM)
//    *
//    * Testar se alguma coluna do "item" corta alguma linha do "paint"
//    *
//    *         +-------+
//    *       c2ï¿½ Item  ï¿½a2
//    *  +------+-------+-+
//    *  ï¿½Paint ï¿½       ï¿½ ï¿½
//    *  +------+-------+-+
//    *       d2ï¿½       ï¿½b2
//    *         +-------+
//    *
//    #DEFINE _INTERSECAO_A2 ;
//              (N_L_INI_ITEM  <= N_L_INI_PAINT .AND. N_L_INI_PAINT <= N_L_FIN_ITEM   .AND. ;
//               N_C_INI_PAINT <= N_C_FIN_ITEM  .AND. N_C_FIN_ITEM  <= N_C_FIN_PAINT )
//    #DEFINE _INTERSECAO_B2 ;
//              (N_L_INI_ITEM  <= N_L_FIN_PAINT .AND. N_L_FIN_PAINT <= N_L_FIN_ITEM   .AND. ;
//               N_C_INI_PAINT <= N_C_FIN_ITEM  .AND. N_C_FIN_ITEM  <= N_C_FIN_PAINT )
//    #DEFINE _INTERSECAO_C2 ;
//              (N_L_INI_ITEM  <= N_L_INI_PAINT .AND. N_L_INI_PAINT <= N_L_FIN_ITEM   .AND. ;
//               N_C_INI_PAINT <= N_C_INI_ITEM  .AND. N_C_INI_ITEM  <= N_C_FIN_PAINT )
//    #DEFINE _INTERSECAO_D2 ;
//              (N_L_INI_ITEM  <= N_L_FIN_PAINT .AND. N_L_FIN_PAINT <= N_L_FIN_ITEM   .AND. ;
//               N_C_INI_PAINT <= N_C_INI_ITEM  .AND. N_C_INI_ITEM  <= N_C_FIN_PAINT )
//    *
//    * Testar se item estï¿½ dentro do paint
//    *
//    *         +---------+
//    *         ï¿½   Paint ï¿½
//    *         ï¿½+------+ ï¿½
//    *         ï¿½ï¿½ Item ï¿½ ï¿½
//    *         ï¿½+------+ ï¿½
//    *         ï¿½         ï¿½
//    *         +---------+
//    *
//    #DEFINE _ITEM_DENTRO_DO_PAINT ;
//              (N_L_INI_PAINT < N_L_INI_ITEM .AND. N_C_INI_PAINT < N_C_INI_ITEM  .AND.;
//               N_L_FIN_PAINT > N_L_FIN_ITEM  .AND. N_C_FIN_PAINT > N_C_FIN_ITEM )
//    *
//    * Testar se paint estï¿½ dentro do item
//    *
//    *         +---------+
//    *         ï¿½   Item  ï¿½
//    *         ï¿½+------+ ï¿½
//    *         ï¿½ï¿½Paint ï¿½ ï¿½
//    *         ï¿½+------+ ï¿½
//    *         ï¿½         ï¿½
//    *         +---------+
//    *
//    #DEFINE _PAINT_DENTRO_DO_ITEM ;
//              (N_L_INI_PAINT > N_L_INI_ITEM  .AND. N_C_INI_PAINT > N_C_INI_ITEM  .AND.;
//               N_L_FIN_PAINT < N_L_FIN_ITEM  .AND. N_C_FIN_PAINT < N_C_FIN_ITEM )
//    *
//    L_SOBREPOS_ITEM := ;
//      _PAINT_DENTRO_DO_ITEM .OR. ;
//      _ITEM_DENTRO_DO_PAINT .OR. ;
//      _INTERSECAO_A1 .OR. ;
//      _INTERSECAO_B1 .OR. ;
//      _INTERSECAO_C1 .OR. ;
//      _INTERSECAO_D1 .OR. ;
//      _INTERSECAO_A2 .OR. ;
//      _INTERSECAO_B2 .OR. ;
//      _INTERSECAO_C2 .OR. ;
//      _INTERSECAO_D2
//    *
//    RETURN L_Sobrepos_Item
//    *
//    ********************
//    PROC AtivaGuiObjects()
//    ********************
//    L_GuiObjectsAtivos := .T.
//    *
//    ********************
//    PROC DesativaGuiObjects()
//    ********************
//    L_GuiObjectsAtivos := .F.
//    *
//    *****************
//    PROC WVW_SetFocus (nWinNum,hWnd)  // A WVW chama esta rotina de forma automï¿½tica.
//    *****************                 // ï¿½ uma funï¿½ï¿½o de callback. Nï¿½o pode ser STATIC.
//    * Se quem ganhou o foco nï¿½o foi a ï¿½ltima janela,
//    * voltar foco para ela (janela do topo).
//    if nWinNum # LEN(V_PilhaJanelas)-IIF(EH_PRODUCAO(),1,0)
//       win_setfocus(wvw_getwindowhandle(wvw_nnumwindows()-1))
//    endif
//    *
*******************************
STATIC FUNC ADICIONA_BOTAO_PUSH(VX_Janela,N_PosBotao)
*******************************
LOCAL V_Botao := V_RegiaoBotoes[N_PosBotao]
//LOCAL N_Handle_PushButton
LOCAL N_Keyboard := V_Botao[_BOTAO_INKEY_DESTAQUE]
LOCAL C_TextoDestaque := V_Botao[_BOTAO_TEXTO_DESTAQUE]
LOCAL C_TextoBotaoAux := V_Botao[_BOTAO_TEXTO_TRATADO_2]
 //LOCAL B_BlocoAux, N_Pos
LOCAL N_Pos
LOCAL C_TextoBotaoAux_CodigoPagina
*
// NAP_LOG("BEGIN BOTAO:")
// V_Botao := V_RegiaoBotoes[N_PosBotao]
// NAP_LOG("BOTAO GET:")
//LOCAL N_Handle_PushButton
// N_Keyboard := V_Botao[_BOTAO_INKEY_DESTAQUE]
// NAP_LOG("BOTAO GET2:")
// C_TextoDestaque := V_Botao[_BOTAO_TEXTO_DESTAQUE]
// NAP_LOG("BOTAO GET3:")
// C_TextoBotaoAux := V_Botao[_BOTAO_TEXTO_TRATADO_2]
// NAP_LOG("BOTAO GET4:")

// IF .NOT. C_TextoDestaque == NIL
// NAP_LOG("Button C_TextoDestaque:" + C_TextoDestaque)
// ELSE
// NAP_LOG("Button C_TextoDestaque NIL")
// ENDIF

// IF .NOT. C_TextoBotaoAux == NIL
// NAP_LOG("Button C_TextoBotaoAux:" + C_TextoBotaoAux)
// ELSE
// NAP_LOG("Button C_TextoBotaoAux NIL")
// ENDIF

// NAP_LOG("N_Keyboard:" + hb_ntos(N_Keyboard))
//!! DEPOIS REMOVER O "IF", DEIXANDO O "ELSE"
//!! (POR ENQUANTO, O N_KEYBOARD PODE CONTER NIL)

//NAP_LOG("C_TextoBotaoAux")
//NAP_LOG(C_TextoBotaoAux)

IF N_Keyboard == NIL
    //NAP_LOG("KeyBOARD NULLLLLL")
    // B_BlocoAux := {||NIL}   // PUSHBUTTON FICARÁ SEM FUNCIONAR !!!
ELSE
    //NAP_LOG("KeyBOARD " + hb_ntos(N_Keyboard))
    IF LEN(C_TextoDestaque)==1 .AND. ; // Se aceleradora tiver 1 byte de tamanho
        (ISALPHA(C_TextoDestaque) .OR. ISDIGIT(C_TextoDestaque))  // Se for uma letra (sem acentos) ou um número
       *
        N_Pos := AT(XUPPER(C_TextoDestaque),XUPPER(C_TextoBotaoAux))

         //!! Porque o código abaixo não funcionou ?????
         //N_Pos := AT(XUPPER(C_TextoDestaque),C_TextoBotaoAux)
         //IF N_Pos == 0
         //   N_Pos := AT(XLOWER(C_TextoDestaque),C_TextoBotaoAux)
         //ENDIF
        *
        IF N_Pos # 0   // Colocar o "&" que torna a caractere um acelerador para o Windows
        C_TextoBotaoAux := LEFT(C_TextoBotaoAux,N_Pos-1)+"&"+;
                            SUBSTR(C_TextoBotaoAux,N_Pos)
        ENDIF
    ENDIF
      *
      * Quando um PushButton fazia uma ação sem abrir nenhuma tela adicional
      *   Exemplo: - Ir para uma tela de seleção múltipla e
      *            - Teclar "barra de espaço"
      * O componente com foco ficava sendo o PushButton. Quando o foco está no
      * PushButton e não na tela "mãe", eventos de teclado
      * (teclas pendentes, digitação de dados, etc) ficam suspensos, dando a
      * impressão que o sistema está travado. Se clicar fora do sistema e clicar
      * no sistema novamente, o problema se resolve, porque a WVW_SetFocus()
      * é executada.
      *
      * A solução para este problema foi tirar o foco do PushButton "na marra",
      * antes de executar a ação dos PushButtons.
      //B_BlocoAux := {||win_setfocus(wvw_getwindowhandle(wvw_nnumwindows()-1)),;
       // HB_KeyPut(N_Keyboard)}
ENDIF  // N_Keyboard == NIL
*
IF Version()=="Harbour 3.2.0dev (r1703241902)"
    C_TextoBotaoAux_CodigoPagina := HB_OEMtoANSI(C_TextoBotaoAux)
ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // ADAPTACAO_LINUX
    C_TextoBotaoAux_CodigoPagina := C_TextoBotaoAux
ELSE
    C_TextoBotaoAux_CodigoPagina := C_TextoBotaoAux
ENDIF
//ENDIF ERRO
*

//NAP_LOG("Before NAP_CUALIB_BUTTON")
//NAP_LOG(hb_ntos(V_Botao[_BOTAO_LIN_INICIAL]))

//IF V_Botao[_BOTAO_LIN_INICIAL] == NIL
//    NAP_LOG("NIIIILLLLL V_Botao[_BOTAO_LIN_INICIAL]")
//ENDIF

//NAP_LOG("Before V_Botao[_BOTAO_LIN_INICIAL]")
NAP_CUALIB_BUTTON(C_TextoBotaoAux_CodigoPagina,;
                    V_Botao[_BOTAO_BLOCO_ACAO],;
                    0,; // BUTTON_ID
                    N_LinMess  +V_Botao[_BOTAO_LIN_INICIAL],;
                    N_Col1Livre+V_Botao[_BOTAO_COL_INICIAL],;
                    N_LinMess  +V_Botao[_BOTAO_LIN_FINAL  ],;
                    N_Col1Livre+V_Botao[_BOTAO_COL_FINAL  ],;
                    V_Botao[_BOTAO_AUTOCLOSE])

//NAP_LOG("Before NAP_CUALIB_HOTKEY")

// FRAN: _JAN_MENU_VERT doen't have V_LstAcoes
IF N_TP_Jan == _JAN_MENU_VERT
// FRAN: Button HotKey
// FRAN: The hotkeys are mapped via V_LstAcoes
IF N_KeyBoard # NIL
    NAP_LOG("BUTTON KEYBOARD " + hb_ntos(N_Keyboard))
    NAP_CUALIB_HOTKEY(N_KeyBoard, V_Botao[_BOTAO_BLOCO_ACAO], V_Botao[_BOTAO_AUTOCLOSE])
ENDIF

ENDIF


// IF ( N_Handle_PushButton := WVW_PBCREATE(N_WindowNum,N_LinMess  +V_Botao[_BOTAO_LIN_INICIAL],;
//                                                     N_Col1Livre+V_Botao[_BOTAO_COL_INICIAL],;
//                                                     N_LinMess  +V_Botao[_BOTAO_LIN_FINAL  ],;
//                                                     N_Col1Livre+V_Botao[_BOTAO_COL_FINAL  ],;
//                                                     C_TextoBotaoAux_CodigoPagina,NIL,;
//                                                     B_BlocoAux) ) == 0
//     ALARME("M28032","Falha ao criar a 'PushButton'")
//     ? MEMVAR->PUSHBUTTON_NAO_CRIADA
// ENDIF
// *
// V_Botao[_BOTAO_HANDLE_PUSHBUTTON] := N_Handle_PushButton
*
//NAP_LOG("Before INABILITA_BOTAO_PUSH")

INABILITA_BOTAO_PUSH(VX_Janela, N_PosBotao)
*
RETURN NIL
*
//    ********************************
//    FUNCTION ProcessaBotaoToolbarKey( VX_Janela, nkey )
//    ********************************
//    LOCAL b_Action
//    LOCAL N_KeyRetorno := nKey
//    LOCAL N_TOOLBAR_COD_ACAO, N_POS
//    *
//    * Define um cï¿½digo INKEY para indicar que o usuï¿½rio clicou em alguma
//    * opï¿½ï¿½o do Menu da GTWVW ou da ToolBar da GTWVW
//    #DEFINE WVW_DEFAULT_MENUKEYEVENT  1024   //! must match with HBGTWVW.H

//    IF nkey == WVW_DEFAULT_MENUKEYEVENT
//      * A WVW_GetLastMenuEvent() retorna o cï¿½digo da aï¿½ï¿½o,
//      * que foi setado ao se criar o Menu da GTWVW ou a ToolBar da GTWVW
//      #DEFINE _TOOLBAR_COD_ACAO   1
//      #DEFINE _TOOLBAR_COD_BITMAP 2
//      #DEFINE _TOOLBAR_TOOLTIP    3
//      #DEFINE _TOOLBAR_BLOCO_ACAO 4
//      *
//      N_TOOLBAR_COD_ACAO := WVW_GetLastMenuEvent()

//      IF N_TOOLBAR_COD_ACAO # NIL
//         N_POS := ASCAN(V_BotoesToolBar,{|V_Subv|V_Subv[_TOOLBAR_COD_ACAO]==;
//                                                 N_TOOLBAR_COD_ACAO})
//         IF N_POS # 0
//            * Executar o bloco de cï¿½digo associado ao botï¿½o
//            b_Action := V_BotoesToolBar[N_POS,_TOOLBAR_BLOCO_ACAO]
//            EVAL(b_Action)
//            *
//            N_KeyRetorno := 0 // Na prï¿½tica, descartar o cï¿½digo inkey recebido pelo bloco de cï¿½digo
//         ENDIF
//      ENDIF
//      *
//    ELSE
//      N_KeyRetorno := nkey  // Deixar a tecla ser processada normalmente pela xHarbour
//    ENDIF

//    RETURN N_KeyRetorno
//    *
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
// *
// *

//
// FRAN: TODO
//
STAT PROC EXIBEVIDEO(C_TIPO,C_ARQUIVO,C_TITULO)
    RETURN
// *************************
// STAT PROC EXIBEVIDEO(C_TIPO,C_ARQUIVO,C_TITULO)
// *************************
// LOCAL N_HANDLE
// DEFAULT C_TIPO TO "V"
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    EXISTENCIA_EXE("EXIBIRTF",GS_EXTENSAO_EXE(),,.F.)
//    N_HANDLE := HB_processOpen(DIREXE()+"exibirtf."+GS_EXTENSAO_EXE()+" "+C_TIPO+" "+C_ARQUIVO+" "+C_TITULO)
//    IF N_HANDLE < 0
//       ALARME("M28320","Nï¿½o foi possï¿½vel exibir a Video Aula!")
//    ENDIF
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)  // PENDENTE_LINUX
//    // ? MEMVAR->NAO_ADAPTADO_PARA_LINUX
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
// INKEYX(2)
// *
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    *********************
//    PROC CRIA_PROGRESSBAR(VX_Janela)
//    *********************
//    LOCAL N_COMPRIMENTO
//    *
//    IF N_ProgressBar == 2
//       N_IdProgressBar1 := WVW_PGCreate(N_WindowNum,N_Lin2Livre+4,N_Col1Livre,N_Lin2Livre+4,N_Col2Livre,,,rgb(112,112,250),.T.,.F.)
//       //@ N_Lin2Livre+5,N_Col1Livre+1 SAY "Tabela(s) a processar"
//       IF N_IdProgressBar1 == 0
//          ALARME("M28546","Falha ao criar a 'ProgressBar1'")
//          ? MEMVAR->ProgressBar1_NAO_CRIADA
//       ENDIF
//       *
//       N_IdProgressBar2 := WVW_PGCreate(N_WindowNum,N_Lin2Livre+2,N_Col1Livre,N_Lin2Livre+2,N_Col2Livre,,,rgb(112,112,250),.T.,.F.)
//       //@ N_Lin2Livre+3,N_Col1Livre+1 SAY "Processo atual"
//       IF N_IdProgressBar2 == 0
//          ALARME("M28548","Falha ao criar a 'ProgressBar2'")
//          ? MEMVAR->ProgressBar2_NAO_CRIADA
//       ENDIF
//    ELSEIF N_ProgressBar == 1
//       N_IdProgressBar1 := WVW_PGCreate(N_WindowNum,N_Lin2Livre+2,N_Col1Livre,N_Lin2Livre+2,N_Col2Livre,,,rgb(112,112,250),.T.,.F.)
//       //@ N_Lin2Livre+3,N_Col1Livre+1 SAY "Registro(s) processado(s)"
//       IF N_IdProgressBar1 == 0
//          ALARME("M28550","Falha ao criar a 'ProgressBar1'")
//          ? MEMVAR->ProgressBar1_NAO_CRIADA
//       ENDIF
//    ENDIF
//    *
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
// *
// *
// **************************
// PROC ANDAMENTO_PROGRESSBAR(VX_Janela, N_ContPosPG1, N_TotalPG1, N_ContPosPG2, N_TotalPG2,C_MENSAGEMBAR_1,C_MENSAGEMBAR_2)
// **************************
// LOCAL N_POS1, N_POS2
// LOCAL C_NOVO_SUBTITULO
// LOCAL N_OLD_VALOR_55  := GetProgressBar( VX_Janela )
// *
// DEFAULT C_MENSAGEMBAR_1 TO "Total a processar"
// DEFAULT C_MENSAGEMBAR_2 TO "Processo atual"
// DEFAULT N_TotalPG1      TO 1    /// Pra evitar error divisao por ZERO
// DEFAULT N_TotalPG2      TO 1
// *
//    N_POS1 := INT((N_ContPosPG1/N_TotalPG1) * 100)
// *
// IF SOB_MODO_GRAFICO()
//    #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//       WVW_PGSetPos(N_WindowNum, N_IdProgressBar1, N_POS1)                             /// Desenha o Andamento da Barra modo grafico
//       @ N_Lin2Livre+IF(VX_Janela[55]>1,5,3),N_Col1Livre    SAY PADR(C_MENSAGEMBAR_1, 47, " ")
//       @ N_Lin2Livre+IF(VX_Janela[55]>1,5,3),N_Col1Livre+46 SAY STR(N_POS1) + "%"
//    #elif defined(__PLATFORM__LINUX)
//       // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//    #else
//       #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
//    #endif
// ELSE
//    #IFDEF _TESTE
//       IF (LEN( VX_Janela[8] ) < 8)    /// ï¿½ progressBar do tipo texto a pos 8 da janela, tem que ser um vetor de 8 elementos!!!
//          ? MEMVAR->JANELA_NAO_EH_PROGRESSBAR
//       ENDIF
//    #ENDIF
//    VX_Janela[8,IF(VX_Janela[55]>1, 6, 4)] := "["+PADR(REPLICATE(CHR(219),N_POS1/2),49,CHR(176))+"]"       /// Seta o Andamento da Barra modo texto
//    VX_Janela[8,IF(VX_Janela[55]>1, 7, 5)] := PADR( C_MENSAGEMBAR_1, 46, " " ) + " " + STR(N_POS1,3) + "%"
// ENDIF
// *
// IF VX_Janela[55] > 1                                                                  /// Testa se ï¿½ ProgressBar e se tem 2 barras
//    N_POS2 := INT((N_ContPosPG2/N_TotalPG2) * 100)
//    *
//    IF SOB_MODO_GRAFICO()
//       #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//          WVW_PGSetPos(N_WindowNum, N_IdProgressBar2, N_POS2)                            /// Desenha o Andamento da Barra modo grafico
//          @ N_Lin2Livre+3,N_Col1Livre    SAY PADR(C_MENSAGEMBAR_2, 47, " ")
//          @ N_Lin2Livre+3,N_Col1Livre+46 SAY STR(N_POS2) + "%"
//       #elif defined(__PLATFORM__LINUX)
//          // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//       #else
//          #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
//       #endif
//    ELSE
//       VX_Janela[8,3] := "["+PADR(REPLICATE(CHR(219),N_POS2/2),49,CHR(176))+"]"       /// Seta o Andamento da Barra modo texto
//       VX_Janela[8,4] := PADR( C_MENSAGEMBAR_2, 46, " " ) + " " + STR(N_POS2,3) + "%"
//    ENDIF
// ENDIF
// *
// IF ! SOB_MODO_GRAFICO()
//    VX_Janela[55] := 0    /// So para garantir que o MudeSubtitulo manipule a Janela ProgressBar
//    C_NOVO_SUBTITULO := VX_Janela[8,1]+";"+VX_Janela[8,2]+";"+VX_Janela[8,3]+";"+VX_Janela[8,4]+";"+VX_Janela[8,5]+;
//                                        ";"+VX_Janela[8,6]+";"+VX_Janela[8,7]+";"+VX_Janela[8,8]
//    MUDE SUBTITULO VX_Janela PARA C_NOVO_SUBTITULO     /// Andamento da(s) Barra(s) em modo texto.
//    VX_Janela[55] := N_OLD_VALOR_55
// ENDIF
*
****************************
STATIC FUNC INABILITA_IMAGEM(L_MUDADADOS)
****************************
LOCAL L_RET := .F.
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
   IF SELECT("XXPREG") # 0
      IF L_MUDADADOS .AND. (.NOT. EHPRINCIPAL(.F.))
         L_RET := .T.
      ENDIF
   ENDIF
ENDIF
*
RETURN L_RET
// *
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    ********************************
STATIC PROC INABILITA_BOTAO_PUSH(VX_Janela, N_PosBotao)
********************************
LOCAL L_MUDADADOS
LOCAL N_TECLA := V_RegiaoBotoes[N_PosBotao,_BOTAO_INKEY_DESTAQUE]
LOCAL C_TEXTO := UPPER(ALLTRIM(V_RegiaoBotoes[N_PosBotao,_BOTAO_TEXTO_TRATADO_2]))
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
    IF V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS] # NIL
        L_MUDADADOS := V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS]
    ELSE
        L_MUDADADOS := .F.
    ENDIF
    *
    * Qualquer botao para poder ser inabiltado, precisa usar o parâmetro L_MudaDados.
    * Perguntar ao Marcos se as teclas F2 e F9 continuam sendo inabilitadas automáticamente. //!!
    IF (N_TECLA == K_F2 .AND. C_TEXTO == "F2=SALVAR") .OR. N_TECLA == K_F9 .OR. L_MUDADADOS
        IF SELECT("XXPREG") # 0
            IF .NOT. EHPRINCIPAL(.F.)
                //
                // FRAN: Implement Enable in GTNAP
                //
                //WVW_PBENABLE(N_WindowNum, V_RegiaoBotoes[N_PosBotao,_BOTAO_HANDLE_PUSHBUTTON], .F.)
                V_RegiaoBotoes[N_PosBotao,_BOTAO_BLOCO_ACAO] := {||.F.}
            ENDIF
        ENDIF
    ENDIF
ENDIF
*
***********************************
STATIC FUNC INABILITA_BOTAO_TOOLBAR(L_MudaDados)
***********************************
LOCAL L_RET := .F.
*
DEFAULT L_MudaDados TO .F.
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
    IF L_MudaDados
        IF SELECT("XXPREG") # 0
            IF .NOT. EHPRINCIPAL(.F.)
                L_RET := .T.
            ENDIF
        ENDIF
    ENDIF
ENDIF
*
RETURN L_RET
//    *
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
// *
// *
*****************************
STATIC FUNC INABILITA_ADDACAO(L_MUDADADOS)
*****************************
LOCAL L_RET := .F.
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
   IF SELECT("XXPREG") # 0
      IF L_MUDADADOS .AND. (.NOT. EHPRINCIPAL(.F.))
         L_RET := .T.
      ENDIF
   ENDIF
ENDIF
*
RETURN L_RET
*
************************************************
FUNC CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
************************************************
* Esta função tem por objetivo fazer com o teste do parâmetro MUDADADOS,
* seja desconsiderado quando se tratar dos sistemas ORÇAMENTO e PPA
LOCAL L_RET := .T.
*
IF G_SGSIST() # NIL
   IF G_SGSIST() $ "OR/PP" // Orçamento ou PPA
      L_RET := .F.
   ENDIF
ENDIF
*
RETURN L_RET
// *
// ********************
// *
// #pragma BEGINDUMP
// #include "hbapi.h"

// // Fonte base obtido no link abaixo:
// //  http://batchloaf.wordpress.com/2012/04/17/simulating-a-keystroke-in-win32-c-or-c-using-sendinput/

// // Simulates a key press and a key release

// // Because the SendInput function is only supported in
// // Windows 2000 and later, WINVER needs to be set as
// // follows so that SendInput gets defined when windows.h
// // is included below.
// #ifdef __WIN32__
// #define WINVER 0x0500
// #include <windows.h>

// HB_FUNC( MANDAR_ESPACO_PARA_BUFFER_WINDOWS )
// {
//    // This structure will be used to create the keyboard
//    // input event.
//    INPUT ip;

//    // Set up a generic keyboard event.
//    ip.type = INPUT_KEYBOARD;
//    ip.ki.wScan = 0; // hardware scan code for key
//     ip.ki.time = 0;
//    ip.ki.dwExtraInfo = 0;

//    // Press the key
//    ip.ki.wVk = 0x20; // virtual-key code for the " " key (VK_SPACE)
//    ip.ki.dwFlags = 0; // 0 for key press
//    SendInput(1, &ip, sizeof(INPUT));

//    // Release the key
//    ip.ki.dwFlags = KEYEVENTF_KEYUP; // KEYEVENTF_KEYUP for key release
//    SendInput(1, &ip, sizeof(INPUT));

//    hb_ret() ;
// }
// #else
// HB_FUNC( MANDAR_ESPACO_PARA_BUFFER_WINDOWS )
// {
//    hb_ret();
// }
// #endif

// #pragma ENDDUMP
// *
// *
// * //!! Ao se ativar a WVW_SIZE() ficou dando um erro de GPF...
// * //!! Tentar depois descobrir o motivo...
// * *************
// * FUNC WVW_SIZE_DEU_ERRO (nWinNum, hWnd, message, wParam, lParam)
// * *************
// * * this function is called by gtwvw AFTER the size is changed
// * * WARNING: screen repainting is not performed completely by gtwvw at this point of call
// *
// * IF WVW_SIZE_READY() .AND. nWinNum == 0
// *    * avoid reentrance
// *    WVW_SIZE_READY(.f.)
// *
// *    do case
// *       case wParam == 2 //SIZE_MAXIMIZED
// *       case wParam == 1 //SIZE_MINIMIZED
// *       case wParam == 0 //SIZE_RESTORED
// *    endcase
// *
// *    * allow next call
// *    WVW_SIZE_READY(.t.)
// * endif
// *
// * RETURN NIL
// *
// * Um local recomendava fazer a WVW_SIZE_READY em cï¿½digo C mesmo,
// * pois rotinas de callback nï¿½o deviam usar variï¿½veis estï¿½ticas.
// * *******************
// * FUNC WVW_SIZE_READY(L_ReSize_New)
// * *******************
// * STAT L_ReSize := .F.
// * LOCAL L_ReSize_Old := L_ReSize
// * IF L_ReSize_New # NIL
// *    L_ReSize := L_ReSize_New
// * ENDIF
// * RETURN L_ReSize_Old
// *
// * #pragma BEGINDUMP
// *
// * #include "hbapi.h"
// *
// * HB_FUNC( WVW_SIZE_READY )
// * {
// *    BOOL bIsReady;
// *    static BOOL s_bIsReady = FALSE;
// *    bIsReady = s_bIsReady;
// *    if (ISLOG(1))
// *    {
// *       s_bIsReady = hb_parl(1);
// *    }
// *    hb_retl(bIsReady);
// * }
// * #pragma ENDDUMP
// *
// * #IFDEF _TESTE
// * **************
// * PROC LOGAR_WVW(C_STR)
// * **************
// * #INCLUDE "fileio.ch"
// * LOCAL N_Handle := FOPEN("p:\temp\wvw.log",FO_READWRITE)
// * IF N_Handle==-1
// *    N_Handle := FCREATE("p:\temp\wvw.log")
// * ENDIF
// * FSEEK(N_Handle,0,FS_END)
// * FWRITE(N_Handle,C_STR+CHR(13)+CHR(10))
// * FCLOSE(N_Handle)
// * #ENDIF
// *
// ******************************** FIM DO JANELA ********************************
