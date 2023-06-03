/* encoding: cp850 */

//
//                                Aspec - Informatica
//                            Direitos Autorais Reservado
//                        Variantes especificas da classe janela
//
#INCLUDE "xx.ch"
#INCLUDE "inkey.ch"
#INCLUDE "janela.ch"          // para usar os pseudo métodos do objeto janela
#INCLUDE "setcurs.ch"
#INCLUDE "cua.ch"
#INCLUDE "common.ch"
#INCLUDE "mousecua.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "hbgtinfo.ch"
#INCLUDE "gtnap.ch"

/*---------------------------------------------------------------------------*/

FUNCTION Setup_nap(C_Title, N_Lines, N_Cols, B_Init)
    NAP_INIT(C_Title, N_Lines, N_Cols, B_Init)
    RETURN (.T.)

/*---------------------------------------------------------------------------*/

FUNCTION DIRET_BMPS(C_DIRET_NEW)
    STATIC C_DIRET_BMPS := ""
    LOCAL C_DIRET_ANT := C_DIRET_BMPS
    IF C_DIRET_NEW # NIL
        C_DIRET_BMPS := C_DIRET_NEW
    ENDIF
    RETURN C_DIRET_ANT

/*---------------------------------------------------------------------------*/













***********************
STAT FUNCTION Perguntar (C_Cabec_x,C_SubCabec,VC_Menu,;
                         VC_TxtBotoes,N_Default)
***********************
*
LOCAL VX_Janela , N_CursorAnt , N_Opcao, N_Cont, N_Col, N_Row, N_Ret, N_OptLen
LOCAL N_DeslocaCabecalho := 4

* Tirar cópia do vetor, pois será alterado aqui dentro...
LOCAL VC_Menu_Aux := ACLONE(VC_Menu)
//LOCAL N_PaintRefresh_Old
*
//
// FRAN: Not required this code
//
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       N_PaintRefresh_Old := WVW_SetPaintRefresh(_REPAINT_DEFAULT)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
*
DEFAULT N_Default TO 1
*
N_CursorAnt := SET(_SET_CURSOR,SC_NONE)          // salvar modo do cursor
*
FOR N_Cont := 1 TO LEN(VC_Menu_Aux)
    VC_Menu_Aux[N_Cont] := " "+VC_Menu_Aux[N_Cont]+" "
NEXT
*
VX_Janela := MontarJanela(C_Cabec_x,C_SubCabec,VC_Menu_Aux,VC_TxtBotoes)
IF SOB_MODO_GRAFICO()
   ADDIMAGEM VX_Janela ARQUIVO DIRET_BMPS()+"b99903.abm" ;      //"pergunta.abm"
       COORDENADAS 00,00,02,04 AJUDA "B19123"
ENDIF
*
NAP_LOG("PERGUNTAR ANTES ATIVE!!!!!!!!!!!!!!!!!!!!!!!")

Ative(VX_Janela)
*
NAP_LOG("PERGUNTAR DESPUES ATIVE !!!!!!!!!!!!!!!!!!!!!!!")

IF SOB_MODO_GRAFICO()
    NAP_LOG("PERGUNTAR MENU")

    N_Row := Lin1Livre(VX_Janela)
    N_Col := Col1Livre(VX_Janela)+N_DeslocaCabecalho

//    @ ROW(),COL() SAY VC_Menu[N_Cont]+SPACE(02) // espaçamento entre botoes

    FOR N_Cont := 1 TO LEN(VC_Menu)
        *
        N_OptLen := LEN(VC_Menu[N_Cont])

        NAP_BUTTON(N_WindowNum, N_Row, N_Col, N_Row, N_Col + N_OptLen, {|| VC_Menu[N_Cont]}, NIL, .T., .F.)


        //NAP_CUALIB_BUTTON(VC_Menu[N_Cont], {||.T.}, 1000 + N_Cont, N_Row, N_Col, N_Row, N_Col + N_OptLen, .T.)


        N_Col += N_OptLen + 2
        NAP_LOG(VC_Menu[N_Cont] + " Row:" + hb_ntos(N_Row) + " Col:" + hb_ntos(N_Col))
        //C_TeclaAtalho := XUpper(Left(Troca(VC_Menu[N_Cont]," [",""),1))
    NEXT

    NAP_CUALIB_DEFAULT_BUTTON(N_Default)

    N_Opcao := NAP_WINDOW_MODAL(N_WindowNum)

    IF N_Opcao >= NAP_MODAL_BUTTON_AUTOCLOSE .AND. N_Opcao <= NAP_MODAL_BUTTON_AUTOCLOSE + NAP_MAX_BUTTONS
        N_Opcao -= NAP_MODAL_BUTTON_AUTOCLOSE
    ELSE
        N_Opcao := 0   // NO OPTION SELECTED
    ENDIF

    Destrua VX_Janela
ELSE // NOT SOB_MODO_GRAFICO
    * montar o menu horizontal
    N_Opcao := MenuHorizontal(VX_Janela,VC_Menu_Aux,N_Default)
    Destrua VX_Janela
ENDIF

*
SET(_SET_CURSOR,N_CursorAnt)   // restaurar modo do cursor
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       WVW_SetPaintRefresh(N_PaintRefresh_Old)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
*
RETURN N_Opcao
*
******************************
STATIC FUNCTION MenuHorizontal ( VX_Janela , VC_Menu , N_Default )
*
LOCAL N_Cont, N_Row, N_Col, C_Teclas := ""
LOCAL C_TeclaAtalho
LOCAL V_RegiaoOpcoes := {}
LOCAL N_DeslocaCabecalho
IF SOB_MODO_GRAFICO()   // reservar 4 colunas para mostrar a imagem à esquerda
    N_DeslocaCabecalho := 4
ELSE
   N_DeslocaCabecalho := 0
ENDIF
*
DispBegin()
*
* posicionar cursor
SETPOS(Lin1Livre(VX_Janela),Col1Livre(VX_Janela)+N_DeslocaCabecalho)
*
N_Row := ROW()   // supor inicialmente que o "default" o item 1
N_Col := COL()
*
FOR N_Cont := 1 TO LEN(VC_Menu)
    *
    C_TeclaAtalho := XUpper(Left(Troca(VC_Menu[N_Cont]," [",""),1))
    *
    AADD(V_RegiaoOpcoes,{C_TeclaAtalho,ROW(),COL(),;
                                       ROW(),COL()+LEN(VC_Menu[N_Cont])-1})
    *

    IF SOB_MODO_GRAFICO()

        //
        // FRAN: TODO
        //

    ENDIF

    // #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
    //    IF SOB_MODO_GRAFICO()
    //       * Coordenadas para a WVW_PAINT (3 parï¿½metro da AddGuiObjects)
    //       * - Teve ser ser acrescentado o "-1" abaixo, na coluna inicial.
    //       * - Teve ser ser acrescentado o "+1" abaixo, na coluna final.
    //       AddGuiObject(VX_Janela,;
    //                    DesenhaBotao(VX_Janela,ROW(),COL(),LEN(VC_Menu[N_Cont])),;
    //                    {ROW(),COL()-1,ROW(),COL()+LEN(VC_Menu[N_Cont])-1+1})
    //       AddGuiObject(VX_Janela,;
    //                    DesenhaAtalhoBotao(VX_Janela,ROW(),COL(),LEN(VC_Menu[N_Cont])),;
    //                    {ROW(),COL()-1,ROW(),COL()+LEN(VC_Menu[N_Cont])-1+1})
    //    ENDIF
    // #elif defined(__PLATFORM__LINUX)
    //    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
    // #else
    //    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
    // #endif
    *
    IF N_Cont==N_Default
       N_Row := ROW()
       N_Col := COL()
    ENDIF
    @ ROW(),COL() SAY VC_Menu[N_Cont]+SPACE(02) // espaçamento entre botoes
    *
    C_Teclas += C_TeclaAtalho
NEXT
*
DispEnd()
*
N_Default := MENUTO(VC_Menu,N_Row,N_Col,C_Teclas,N_Default,V_RegiaoOpcoes)
*
RETURN N_Default
*
********************
STAT FUNCTION MENUTO( VC_Menu, N_Row, N_Col, C_Teclas, N_Default, V_RegiaoOpcoes )
********************
LOCAL N_Tecla
LOCAL C_CorInten
LOCAL N_mRow, N_mCol, N_PosBotao
*
IF SOB_MODO_GRAFICO()
    C_CorInten := "N/BG*"  // reverso azul
ELSE
    C_CorInten := SUBSPOS(SetColor(), ",", 2)
ENDIF
*

IF SOB_MODO_GRAFICO()

    //
    // FRAN: TODO
    //
ELSE
    @ N_Row, N_Col SAY VC_Menu[N_Default] Color C_CorInten
    WHILE N_Tecla # K_ENTER .AND. N_Tecla # K_ESC
    N_Tecla := INKEYX(0)

    // * Em qual pï¿½gina de cï¿½digo vem a tecla
    // IF Version()=="Harbour 3.2.0dev (r1703241902)"
    //     * Jï¿½ vem na PC850
    // ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // PENDENTE_LINUX
    //     IF SOB_MODO_GRAFICO()
    //         * Vem na ANSI.
    //         IF N_Tecla >= 128 .AND. N_Tecla <= 255  // Faixa diferente entre ANSI e OEM
    //             N_Tecla := ASC(HB_ANSItoOEM(CHR(N_Tecla)))  // Converter de ANSI para OEM
    //         ENDIF
    //     ELSE
    //         * Jï¿½ vem na PC850
    //     ENDIF
    // ENDIF ERRO
    *
    IF N_Tecla == K_LEFT
        IF N_Default # 1
            * tirar a cor intensa do botao anterior
            @ N_Row, Col()-Len(VC_Menu[N_Default]) SAY VC_Menu[N_Default]
            * colocar a cor intensa no novo botao
            N_Default--
            @ N_Row, Col()-Len(VC_Menu[N_Default+1])-Len(VC_Menu[N_Default])-2 ;
            SAY VC_Menu[N_Default] Color C_CorInten
        ENDIF
    ELSEIF N_Tecla == K_RIGHT
        IF N_Default # Len(VC_Menu)
            * tirar a cor intensa do botao anterior
            @ N_Row, Col()-Len(VC_Menu[N_Default]) ;
            SAY VC_Menu[N_Default]+SPACE(02)
            * colocar a cor intensa no novo botao
            N_Default++
            @ N_Row, Col() SAY VC_Menu[N_Default] Color C_CorInten
        ENDIF
    ELSEIF N_Tecla == K_ESC
        N_Default := 0
    ELSEIF XUpper(Chr(N_Tecla)) $ C_Teclas
        N_Default := At(XUpper(Chr(N_Tecla)),C_Teclas)
        N_Tecla   := K_ENTER
    ELSEIF N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
        N_mRow := mRow()
        N_mCol := mCol()
        N_PosBotao := ASCAN(V_RegiaoOpcoes,{|V_Botao| ;
                            N_MRow >= V_Botao[2]   .AND. ;
                            N_MRow <= V_Botao[4]   .AND. ;
                            N_MCol >= V_Botao[3] .AND. ;
                            N_MCol <= V_Botao[5] })
        IF N_PosBotao # 0
            KEYBOARD REPL(CHR(K_LEFT),LEN(VC_Menu)-1)+; // ir para a primeira opcao
                    REPL(CHR(K_RIGHT),N_PosBotao-1)+;  // deslocar atï¿½ a selecionada
                    CHR(K_ENTER)                       // teclar ENTER
        ENDIF
    ENDIF
    ENDDO
ENDIF   // IF SOB_MODO_GRAFICO()


RETURN N_Default
*
*********************
STAT FUNCTION SUBSPOS( C_String, C_Delim, N_Pos )
*********************
*
* -> Retira numa string a posiï¿½ï¿½o desejada de acordo com os delimitadores
*
LOCAL I, N_Posatu, C_Strret := ""
N_Posatu := 1
FOR I := 1 TO LEN(C_String)
   IF Substr(C_String,I,1) == C_Delim
      N_Posatu++
   ELSEIF N_Posatu == N_Pos
      C_Strret += Substr(C_String,I,1)
   ENDIF
NEXT
RETURN C_Strret
*

// **********************
// STAT FUNCTION Informar ( C_Cabec_x, C_SubCabec, VC_TxtBotoes ,;
//                          L_Parar, N_Segundos, C_ArqImagem, L_TipoMsgAguarde, N_ProgressBar )
// **********************

// OutStd("INFORMAR() function called")

// RETURN 0


**********************
STAT FUNCTION Informar ( C_Cabec_x, C_SubCabec, VC_TxtBotoes ,;
                         L_Parar, N_Segundos, C_ArqImagem, L_TipoMsgAguarde, N_ProgressBar )
**********************
LOCAL N_Tecla, VX_Janela , N_CursorAnt
LOCAL N_PaintRefresh_Old
*
DEFAULT L_Parar TO .T.
DEFAULT N_Segundos TO 0
DEFAULT L_TipoMsgAguarde TO .F.

N_CursorAnt := SET(_SET_CURSOR,SC_NONE)          // salvar modo do cursor

VX_Janela := MontarJanela(C_Cabec_x,C_SubCabec,,VC_TxtBotoes)
IF SOB_MODO_GRAFICO()
   ADDIMAGEM VX_Janela ARQUIVO DIRET_BMPS()+C_ArqImagem ;
       COORDENADAS 00,01,01,04 AJUDA "B19125"
ENDIF
*
IF L_TipoMsgAguarde
   SetJanTipoMsgAguarde(VX_Janela,L_TipoMsgAguarde)
   SetProgressBar(VX_Janela,N_ProgressBar)
ENDIF

Ative(VX_Janela)

IF SOB_MODO_GRAFICO()
    NAP_WINDOW_MODAL(N_WindowNum)
    Destrua VX_Janela
ELSE
    IF L_Parar .AND. N_Segundos == 0
    *
    * esperar a teclagem de ENTER ou ESC
    *
    N_Tecla := Inkey_(.T.)             // igual ao INKEY(0) com SET KEY"s
    *
    DO WHILE N_Tecla # K_ENTER .AND. N_Tecla # K_ESC ;
        .AND. .NOT. CLICOU_NA_JANELA(VX_Janela,N_Tecla)
        N_Tecla := Inkey_(.T.)
    ENDDO
    Destrua VX_Janela
    ELSEIF L_Parar .AND. N_Segundos # 0
    * SET KEYs nao funcionam.
    * Janela fecha sozinha após algum tempo.
    *
    InkeyX(N_Segundos)
    Destrua VX_Janela
    ELSE
    SETPOS(ROW(),COL()-1)       // retroceder cursor em 1 posiï¿½ï¿½o
    ENDIF
ENDIF
*
SET(_SET_CURSOR,N_CursorAnt)          // restaurar modo do cursor
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO() .AND. N_Segundos == 0
//       WVW_SetPaintRefresh(N_PaintRefresh_Old)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//     // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//     #erro "Código não adaptado para esta plataforma"
// #endif
*
IF L_Parar
   RETURN NIL
ENDIF
RETURN VX_Janela
*






**************************
STAT FUNC CLICOU_NA_JANELA (VX_Janela,N_Tecla)
**************************
LOCAL L_Clicou := .F.
LOCAL N_mRow, N_mCol, N_RegiaoMouse, N_Keyboard
*
IF N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
   N_mRow := mRow()
   N_mCol := mCol()
   N_RegiaoMouse := RegiaoJanela_(VX_Janela,N_mRow,N_mCol,;
                                  Lin1Livre(VX_Janela),;
                                  Col1Livre(VX_Janela),;
                                  Lin2Livre(VX_Janela),;
                                  Col2Livre(VX_Janela),;
                                  @N_Keyboard)
  //#INCLUDE "mousecua.ch"
  IF N_RegiaoMouse # FORA_DA_JANELA
     L_Clicou := .T.
  ENDIF
ENDIF
*                      IF N_RegiaoMouse == AREA_UTIL
RETURN L_Clicou
*
****************************
STATIC FUNCTION MontarJanela (C_Cabec_x, C_SubCabec, VC_Menu , VC_TxtBotoes )
****************************
*
LOCAL N_Largura , N_LargAux , N_Altura, VC_Subtitulo, VX_Janela
LOCAL N_LinIni, N_ColIni
LOCAL N_DeslocaCabecalho
*
DEFAULT VC_Menu      TO {}
DEFAULT VC_TxtBotoes TO {}
*
VC_Subtitulo = StrToVet_(C_SubCabec)
*
* calcular largura do box
*
N_Largura := 0                       // procurar maior título
AEVAL(VC_Subtitulo,{|C_Elemento|;
                   N_Largura := MAX(N_Largura,LEN(C_Elemento))} )
*
* + 1 = espaço à esquerda do botao (acrescentado na TecFunc)
* + 1 = espaço à direita do botao (acrescentado na TecFunc)
* + 2 = espaço entre botoes
*
N_LargAux  := 0                      // largura das teclas de funções
AEVAL(VC_TxtBotoes,{|C_Elemento|;
                    N_LargAux := N_LargAux + LEN(C_Elemento) + 1 + 1 + 2 })
N_Largura := MAX(N_LargAux,N_Largura)
*
* + 2 = espaço entre opçoes
*
N_LargAux  := 0                      // largura do menu
AEVAL(VC_Menu  ,{|C_Elemento|;
                  N_LargAux := N_LargAux + LEN(C_Elemento) + 2 })
N_Largura := MAX(N_LargAux,N_Largura)
*
* + 2 = colunas (do box)
* + 2 = espaçamentos laterais (um de cada lado)
* + 2 = reservado para a SCROLLBAR VERTICAL
*
N_Largura := N_Largura + 2 + 2 + 2
*
N_DeslocaCabecalho := 0

//
// FRAN!!! At the moment, measure synchro in TEXT and GTNAP versions
//
// IF SOB_MODO_GRAFICO()   // reservar 4 colunas para mostrar a imagem à esquerda
//    N_DeslocaCabecalho := 4
//    N_Largura := N_Largura + N_DeslocaCabecalho
// ELSE
//    N_DeslocaCabecalho := 0
// ENDIF
*
* definir altura do box
*
* + 2 = traços superior e inferior (do box)
* + 1 = espaçamento inferior
* NÃO foi reservado espaço SCROLLBAR HORIZONTAL / AREA DE MENSAGEM
*
N_Altura := 2+;       // linhas do box
            IIF(EMPTY(VC_Menu),0,2)+;
            IIF(EMPTY(VC_TxtBotoes),0,2)
IF SOB_MODO_GRAFICO()  // reservar um mínimo de 3 linhas para exibir imagem
   N_Altura := N_Altura  + MAX(LEN(VC_Subtitulo),3)
ELSE
   N_Altura := N_Altura  + LEN(VC_Subtitulo)
ENDIF
*
* testar os limites da janela, já que são determinados automaticamente
*
N_Altura  := MIN(N_Altura ,MAXROW()+1)
N_Largura := MIN(N_Largura,MAXCOL()+1)
*
N_LinIni := ROUND((MAXROW()+1-N_Altura)/2,0)
N_ColIni := ROUND((MAXCOL()+1-N_Largura)/2,0)
*

LOG_PRINT("MontarJanela N_LinIni: " + hb_ntos(N_LinIni) + " N_Altura: " + hb_ntos(N_Altura) + " MAXROW: " + hb_ntos(MAXROW()) + " MAXCOL: " + hb_ntos(MAXCOL()))
@ N_LinIni,N_ColIni,N_LinIni+N_Altura,N_ColIni+N_Largura ;
  Jane VX_Janela TITULO C_Cabec_x SubTitulo C_SubCabec Tecl VC_TxtBotoes ;
  DESLOCACAB N_DeslocaCabecalho AJUDA "T05735"
*
AJUSTA_BOTOES(VX_Janela)  // ajusta Lin2Livre à quantidade de botões de função
*
RETURN VX_Janela
*
****************
PROCEDURE ALARME (C_CDMENS,C_SubCabec,N_SEGUNDOS)
****************
LOCAL C_COR_ANT
*

IF C_SubCabec # NIL

IF SOB_MODO_GRAFICO()

    MOSTRAR(C_CDMENS,C_SubCabec,{GL_ENTER+"=OK"},,.F.,N_SEGUNDOS,;
        "Importante","erro.abm")

ELSE

TONE(750,0)
*
   //IF .NOT. SOB_MODO_GRAFICO()
      C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_ERRO))
   //ENDIF
   *
   ALARME_CDMENS_ATIVO(C_CDMENS) // será usado pela ajuda ao usuário
   *
   MOSTRAR(C_CDMENS,C_SubCabec,{GL_ENTER+"=OK"},,.F.,N_SEGUNDOS,;
           "Importante","erro.abm")
   *
   ALARME_CDMENS_ATIVO("")
   *
   LOGA_AJMENST(GetCdTelaTopo(),GetCdGET_ou_Menu_Topo(),C_CdMens,C_SubCabec)  // LOGAR conteúdo de telas
   *

   //IF .NOT. SOB_MODO_GRAFICO()
      SETCOLOR(C_COR_ANT)
   //ENDIF
ENDIF
ENDIF


*
************************
FUNC ALARME_CDMENS_ATIVO(C_CDMENS_NEW)
************************
STAT C_CDMENS_ATIVO := ""
LOCAL C_CDMENS_OLD
*
C_CDMENS_OLD := C_CDMENS_ATIVO
IF C_CDMENS_NEW # NIL
   * setar o novo
   C_CDMENS_ATIVO := C_CDMENS_NEW
ENDIF

RETURN C_CDMENS_OLD  // retornar o estado anterior
*
*****************
PROCEDURE ADVERTE (C_CDMENS, C_SubCabec,N_SEGUNDOS)
    *****************
    LOCAL C_COR_ANT
    *
    TONE(750,0)
    *
    IF C_SubCabec # NIL
       IF .NOT. SOB_MODO_GRAFICO()
          C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_ADVERTENCIA))
       ENDIF
       *
    //   ALARME_CDMENS_ATIVO(C_CDMENS) // será usado pela ajuda ao usuário
       *
       MOSTRAR(C_CDMENS,C_SubCabec,{GL_ENTER+"=OK"},,.F.,N_SEGUNDOS,;
               "Advertência","adverten.abm")
       *
    //   ALARME_CDMENS_ATIVO("")
       *
    //   LOGA_AJMENST(GetCdTelaTopo(),GetCdGET_ou_Menu_Topo(),C_CdMens,C_SubCabec)  // LOGAR conteúdo de telas
       *
       IF .NOT. SOB_MODO_GRAFICO()
          SETCOLOR(C_COR_ANT)
       ENDIF
    ENDIF
    *

// ****************
// FUNCTION MOSTRAR ( C_CDMENS, C_SubCabec , V_TECLAS , L_PARAR, L_MUDA_COR,;
//                    N_SEGUNDOS, C_Cabec_x, C_ArqImagem )
// ****************
//     @ 22, 0 SAY ""
//     OutStd("MOSTRAR() function called")


//     RETURN NIL

****************
FUNCTION MOSTRAR ( C_CDMENS, C_SubCabec , V_TECLAS , L_PARAR, L_MUDA_COR,;
                   N_SEGUNDOS, C_Cabec_x, C_ArqImagem )
****************
LOCAL C_COR_ANT, V_JAN
DEFAULT L_MUDA_COR TO .T.
DEFAULT C_Cabec_x     TO "Informação"
DEFAULT C_ArqImagem TO "informac.abm"
*
ASSUME LEFT(C_CDMENS,1) == "M"
ASSUME LEN(TROCA(C_CDMENS,"M0123456789?",""))==0
ASSUME LEN(C_CDMENS)==6
C_SubCabec := C_SubCabec+";;("+C_CDMENS+")"
*
IF CABEC_TESTE_AUTOMATICO()
   C_Cabec_x +=" ("+C_CDMENS+")"
ENDIF
*
IF L_MUDA_COR .AND. .NOT. SOB_MODO_GRAFICO()
   C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_OK))
ENDIF
V_JAN := INFORMAR(C_Cabec_x,C_SubCabec,V_TECLAS,L_PARAR,N_SEGUNDOS,C_ArqImagem)
IF L_MUDA_COR .AND. .NOT. SOB_MODO_GRAFICO()
   SETCOLOR(C_COR_ANT)
ENDIF
*
RETURN V_JAN
*
*******************
FUNCTION MSGAGUARDE ( C_CDMENS, C_Cabec_x, C_SubCabec , L_ComBotaoEsc, N_ProgressBar )
*******************
LOCAL C_COR_ANT, V_JAN
LOCAL V_TECLAS
LOCAL C_TXT1   := ""
LOCAL C_TXT2   := ""
LOCAL C_BARRA1 := ""
LOCAL C_BARRA2 := ""
LOCAL C_TEXTO  := ";"
*
DEFAULT C_Cabec_x        TO "Aguarde..."
DEFAULT C_SubCabec       TO "Aguarde... "
DEFAULT L_ComBotaoEsc    TO .T.
DEFAULT N_ProgressBar    TO 0
*
IF N_ProgressBar > 2
   ? MEMVAR->A_JANELA_NAO_TEM_MAIS_QUE_2_PROGRESSBAR
ENDIF
*
ASSUME LEFT(C_CDMENS,1) == "M"
ASSUME LEN(TROCA(C_CDMENS,"M0123456789?",""))==0
ASSUME LEN(C_CDMENS)==6
IF N_ProgressBar > 0    ///  So se for ProgressBar nï¿½o aceita Multilinhas no subtitulo
   *
   C_SubCabec := IF( AT( ";", C_SubCabec ) > 0, LEFT(C_SubCabec, AT( ";", C_SubCabec )-1 ), C_SubCabec )
   C_SubCabec := IF( LEN(C_SubCabec) > 53, LEFT(C_SubCabec, 53), C_SubCabec )
   *
   IF .NOT. SOB_MODO_GRAFICO()
      C_BARRA1 := "[" + PADR(CHR(176), 49, CHR(176)) + "]"
      C_TXT1   := PADR( " ", 49 ) + STR(0,3)+"%"
      IF N_ProgressBar == 2
         C_BARRA2 := "[" + PADR(CHR(176), 49, CHR(176)) + "]"
         C_TXT2   := PADR( " ", 49 ) + STR(0,3)+"%"
      ENDIF
      *
      C_TEXTO := ";;"+IF(N_ProgressBar == 1, ";", "") + C_BARRA1 + ";" + C_TXT1 + ;
                     IF(N_ProgressBar == 2, ";;", ";") + C_BARRA2 + ";" + C_TXT2
   ELSE
      C_TEXTO := ";"+SPACE(50)
   ENDIF
ENDIF
*
C_SubCabec := C_SubCabec + C_TEXTO + ";("+C_CDMENS+")"
*
IF CABEC_TESTE_AUTOMATICO()
   C_Cabec_x +=" ("+C_CDMENS+")"
ENDIF
*
IF L_ComBotaoEsc
   V_TECLAS := {"Esc=sair"}
ENDIF
*
IF .NOT. SOB_MODO_GRAFICO()
   C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_OK))
ENDIF

V_JAN := INFORMAR(C_Cabec_x,C_SubCabec,V_TECLAS,.F.,NIL,"b99902.abm",.T.,N_ProgressBar)   //"informac.abm"
*
IF .NOT. SOB_MODO_GRAFICO()
   SETCOLOR(C_COR_ANT)
ENDIF
*
RETURN V_JAN
*
*********************
PROC FECHARMSGAGUARDE ( V_JAN )
*********************
IF .NOT. GetJanTipoMsgAguarde(V_JAN)
   ? MEMVAR->ERRO_FECHAMENTO_SEM_DESTRUAJAN
ENDIF
*
SetJanTipoMsgAguarde(V_JAN,.F.)  // tirar o atributo, para nï¿½o gerar log na DESTRUA...
SetProgressBar(V_JAN,NIL)  // Ao destruir uma MsgAguarde, setar NIL para o parï¿½metro de N_ProgressBar
*
DESTRUA V_JAN
*
*****************
FUNCTION CONFIRME (C_SubCabec,N_DEFAULT)
*****************
LOCAL N_OPCAO
DEFAULT N_DEFAULT TO 1
N_OPCAO := PERGUN(C_SubCabec,{"Sim","Não"},N_DEFAULT,,"Confirmação")
RETURN (N_OPCAO==1)
*

***********
FUNC PERGUN ( C_SubCabec, V_OPCOES, N_DEFAULT, L_PODE_ZERO, C_Cabec_x )
***********
LOCAL N_OPCAO
LOCAL C_COR_ANT
IF .NOT. SOB_MODO_GRAFICO()
   C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_ADVERTENCIA))
ENDIF
*
NAP_LOG("PERGUN!!!!!!!!!!!!!!!!!!!!!!!")

DEFAULT N_DEFAULT   TO 1
DEFAULT L_PODE_ZERO TO .T.
DEFAULT C_Cabec_x   TO "Informação"
*
* a confirma não aceita o ESC
*
N_OPCAO   := PERGUNTAR(C_Cabec_x,C_SubCabec,V_OPCOES,,N_DEFAULT)
DO WHILE N_OPCAO == 0 .AND. .NOT. L_PODE_ZERO
   N_OPCAO := PERGUNTAR(C_Cabec_x,C_SubCabec,V_OPCOES,,N_DEFAULT)
ENDDO
IF .NOT. SOB_MODO_GRAFICO()
   SETCOLOR(C_COR_ANT)
ENDIF
*
RETURN N_OPCAO
*
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    **********************
//    STAT FUNC DesenhaBotao(VX_Janela,N_Lin,N_Col,N_Largura)
//    **********************
//    RETURN {|| Wvw_DrawBoxRaised(N_WindowNum,N_Lin,N_Col,N_Lin,N_Col+N_Largura-1)}
//    *
//    ****************************
//    STAT FUNC DesenhaAtalhoBotao(VX_Janela,N_Lin,N_Col)
//    ****************************
//    LOCAL N_DeslocaVertical := 0
//    LOCAL N_AlturaEmPixels  := 1
//    LOCAL N_TelaHeight := TelaPrincipalHeight()
//    *
//    * Subir o sublinhado, para "descolar" da margem inferior do botï¿½o.
//    *
//    IF N_TelaHeight >= 1024 .OR. ;  // resoluï¿½ï¿½o VERTICAL
//       N_TelaHeight >=  960
//       N_DeslocaVertical := -1
//       N_AlturaEmPixels  :=  2
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
//    RETURN {|| Wvw_DrawLine(N_WindowNum,;
//                            N_Lin,N_Col+1,N_Lin,N_Col+1,;   // saltar o branco
//                            0,2,2,;   // 0=horizontal, 2=plain e 2=bottom
//                            NIL,N_AlturaEmPixels,NIL,;
//                            {N_DeslocaVertical,0,N_DeslocaVertical,0}) }
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
// #endif
*
*
*
*

//    FUNCTION SetDefaultWindowSize(N_QtLin,N_QtCol)
//     LOCAL N_HeightMin   // altura  mínima que, com certeza, cabe na menor resolução
//     LOCAL N_WidthMin    // largura mínima que, com certeza, cabe na menor resolução
//     LOCAL N_HeightMax   // altura  máxima que ultrapassa a maior resolução
//     LOCAL N_WidthMax    // largura máxima que ultrapassa a maior resolução
//     LOCAL N_MaxMaxRow
//     LOCAL N_MaxMaxCol
//     LOCAL C_Fonte
//     *
//     LOCAL L_Verbose := .F.  // Só colocar para .T. se for debugar esta rotina
//     *
//     * Resoluções mais comuns:
//     * N_ScreenHeight   N_ScreenWidth
//     *    >= 1024         >= 1680
//     *    >= 1024         >= 1440
//     *    >= 1024         >= 1280
//     *    >=  960         >= 1280
//     *    >=  864         >= 1152
//     *    >=  768         >= 1024
//     *    >=  600         >= 1024   (NetBooks)
//     *    >=  600         >=  800   (SuperVGA)
//     *
//     IF WVW_GetScreenHeight() <= 600
//        * Testes práticos mostraram que, em resolução vertical baixa (altura),
//        * o fonte "Courier New" apresenta um serrilhado que torna a leitura das letras
//        * difícil. Nesta situação, o "Lucida Console" tem aparência bem melhor.
//        C_Fonte := "Lucida Console"
//     ELSE
//        * Já em resolução vertical alta, o "Courier New" não apresenta mais
//        * o serrilhado, e tem aparência melhor.
//        * A desvantagem do "Lucida Console", neste caso, é que fica muito largo,
//        * com aparência pesada e feia.
//        C_Fonte := "Courier New"
//     ENDIF
//     *
//     * - Os valores abaixo foram obtidos por via prática, num computador com
//     *   Windows 7 e resolução 600 x 800 (SuperVGA)
//     * - Isto é importante, porque a SetMode() só consegue funcionar se o
//     *   fonte escolhido efetivamente couber na resolução e tamanhos especificados
//     *
//     * Usar o valores mínimos para permitir que a SetMode() mude a quantidade de
//     * linhas para 35 e colunas para 110, até mesmo na resolução 600 X 800
//     N_HeightMin := 6
//     N_WidthMin  := 5
//     *
//     Wvw_SetFont(,C_Fonte,N_HeightMin,N_WidthMin)
//     Wvw_PBSetFont(,C_Fonte,N_HeightMin,N_WidthMin)
//     WVW_SetDefLineSpacing( 4 )  // todo o sistema (novas janelas)
//     WVW_SetLineSpacing( 0, 4 )  // tela "default" do GTWVW
//     SetMode(N_QtLin,N_QtCol)
//     IF L_Verbose
//        ALERT("Com height mínimo "+LTRIM(STR(N_HeightMin))+" e width mínimo "+;
//              LTRIM(STR(N_WidthMin))+;
//              " cabem "+LTRIM(STR(WVW_MAXMAXROW())) +" rows e "+;
//              LTRIM(STR(WVW_MAXMAXCOL()))+" cols")
//     ENDIF
//     ASSUME MAXROW() == N_QtLin-1
//     ASSUME MAXCOL() == N_QtCol-1
//     IF L_Verbose
//        ALERT("Tela mudada para "+LTRIM(STR(N_QtLin))+" linhas "+;
//              LTRIM(STR(N_QtCol))+" colunas")
//     ENDIF
//     *
//     * Usar os valores máximos para ir reduzindo o tamanho até que
//     * caiba na resolução da tela (a maior tela até agora foi a 1024 x 1860)
//     N_HeightMax := 23+1    // em teste real, só coube 23
//     N_WidthMax  := 15+1    // em teste real, só coube 15
//     *
//     * Setar a maior altura de fonte possível, que ainda caiba 35 linhas.
//     * Se WVW_MAXMAXROW() continuar com o mesmo valor, é porque não coube
//     * na tela, sendo necessário reduzir ainda mais a altura do fonte.
//     *
//     N_MaxMaxRow := WVW_MAXMAXROW()
//     Wvw_SetFont(,C_Fonte,N_HeightMax)
//     Wvw_PBSetFont(,C_Fonte,N_HeightMax)
//     IF L_Verbose
//        ALERT("Com height "+LTRIM(STR(N_HeightMax))+;
//              " cabe "+LTRIM(STR(WVW_MAXMAXROW()))+" rows" )
//     ENDIF
//     DO WHILE WVW_MAXMAXROW() == N_MaxMaxRow .AND. ;
//              N_HeightMax > N_HeightMin
//        N_HeightMax--
//        Wvw_SetFont(,C_Fonte,N_HeightMax)
//        Wvw_PBSetFont(,C_Fonte,N_HeightMax)
//        IF L_Verbose
//           ALERT("Com height "+LTRIM(STR(N_HeightMax))+;
//                 " cabe "+LTRIM(STR(WVW_MAXMAXROW()))+" rows" )
//        ENDIF
//     ENDDO
//     IF L_Verbose
//        ALERT("O height final foi "+LTRIM(STR(N_HeightMax)))
//     ENDIF
//     *
//     * Setar a maior altura de fonte possível, que ainda caiba 110 colunas.
//     * Se WVW_MAXMAXCOL() continuar com o mesmo valor, é porque não coube
//     * na tela, sendo necessário reduzir ainda mais a largura do fonte.
//     *
//     N_MaxMaxCol := WVW_MAXMAXCOL()
//     Wvw_SetFont(,C_Fonte,N_HeightMax,N_WidthMax)
//     Wvw_PBSetFont(,C_Fonte,N_HeightMax,N_WidthMax)
//     IF L_Verbose
//        ALERT("Com width "+LTRIM(STR(N_WidthMax))+;
//              " cabe "+LTRIM(STR(WVW_MAXMAXCOL()))+" cols" )
//     ENDIF
//     DO WHILE WVW_MAXMAXCOL() == N_MaxMaxCol .AND. ;
//              N_WidthMax > N_WidthMin
//        N_WidthMax--
//        Wvw_SetFont(,C_Fonte,N_HeightMax,N_WidthMax)
//        Wvw_PBSetFont(,C_Fonte,N_HeightMax,N_WidthMax)
//        IF L_Verbose
//           ALERT("Com width "+LTRIM(STR(N_WidthMax))+;
//                 " cabe "+LTRIM(STR(WVW_MAXMAXCOL()))+" cols")
//        ENDIF
//     ENDDO
//     IF L_Verbose
//        ALERT("O width final foi "+LTRIM(STR(N_WidthMax)))
//     ENDIF
//     ASSUME WVW_MAXMAXROW() >= N_QtLin-1
//     ASSUME WVW_MAXMAXCOL() >= N_QtCol-1
//     *
//     /*
//     * USAR ESTE TRECHO DE CÓDIGO SÓ EM AMBIENTE DE TESTE E NO CASO DE
//     * MUDANÇA DE FONTE, OU MUDANÇA DE TAMANHO MÍNIMO DE RESULUÇÃO.
//     #INCLUDE "intercep.ch"
//     *
//     DBCREATE_T(ASPECTMP()+"testfont.dbf",;
//                {{"HEIGHT"   ,"N", 3,0},;
//                 {"WIDTH"    ,"N", 3,0},;
//                 {"HEIGHT_OK","L", 1,0},;
//                 {"WIDTH_OK" ,"L", 1,0},;
//                 {"MAXMAXROW","N", 3,0},;
//                 {"MAXMAXCOL","N", 3,0}})
//     *
//     USE_T A19276 TABLE (ASPECTMP()+"testfont.dbf") CANINSERT EXCLUSIVE FINALUSE
//     PRIVATE N_HEIGHT
//     FOR N_HEIGHT := 5 TO 35
//        PRIVATE N_WIDTH
//        PRIVATE N_MaxMaxRow_OLD := WVW_MAXMAXROW()
//        FOR N_WIDTH := 4 TO 25
//            PRIVATE N_MaxMaxCol_OLD := WVW_MAXMAXCOL()
//            *
//            Wvw_SetFont(,C_Fonte,N_HEIGHT,N_WIDTH)
//            WVW_SetDefLineSpacing( 4 )  // todo o sistema (novas janelas)
//            WVW_SetLineSpacing( 0, 4 )  // tela "default" do GTWVW
//            SetMode(N_QtLin,N_QtCol)
//            *
//            ASSUME MAXROW() == N_QtLin-1
//            ASSUME MAXCOL() == N_QtCol-1
//            *
//            TESTFONT->(APP_REG())
//            ALIAS TESTFONT REPL HEIGHT     WITH N_HEIGHT
//            ALIAS TESTFONT REPL WIDTH      WITH N_WIDTH
//            IF WVW_MAXMAXROW() # N_MaxMaxRow_OLD
//               ALIAS TESTFONT REPL HEIGHT_OK WITH .T.
//            ENDIF
//            IF WVW_MAXMAXCOL() # N_MaxMaxCol_OLD
//               ALIAS TESTFONT REPL WIDTH_OK WITH .T.
//            ENDIF
//            ALIAS TESTFONT REPL MAXMAXROW  WITH WVW_MAXMAXROW()
//            ALIAS TESTFONT REPL MAXMAXCOL  WITH WVW_MAXMAXCOL()
//            UNLOCK
//        NEXT
//     NEXT
//     CLOSE TESTFONT
//     */

//  RETURN NIL

//    *******************
//    FUNC ChangeFontSize(N_Way)
//    *******************
//       LOCAL aFontInfo   := wvw_getfontinfo()
//       LOCAL C_Fonte     := aFontInfo[1]
//       LOCAL N_HeightOld := aFontInfo[2]
//       LOCAL N_WidthOld  := aFontInfo[3]
//       LOCAL N_HeightNew
//       LOCAL N_WidthNew
//       LOCAL N_MaxMaxRow
//       LOCAL N_MaxMaxCol
//       LOCAL L_Mudou := .F.
//       LOCAL L_Verbose := .F.  // Sï¿½ colocar para .T. se for debugar esta rotina
//       *
//       * No fonte "Lucida Console" a alteraï¿½ï¿½o de 1 Height e 2 Width normalmente
//       * jï¿½ gera novo tamanho de fonte onde tanto a largura como a altura mudam,
//       * causando mudanï¿½a no tamanho da tela (efeito de "encolhimento" ou "expansï¿½o").
//       * Quando se mudou o fonte predominante para "Courier New", foi preciso
//       * aumentar a variaï¿½ï¿½o para 2 Height e 3 Width, para que a mudanï¿½a
//       * de tamanho de fonte causasse tanto mudanï¿½a na largura como na altura.
//       * Com isto, o salto na mudanï¿½a de tamanho do fonte (e tela) passarï¿½ a ser maior,
//       * ao se usar a presente opï¿½ï¿½o.
//       *
//       IF L_Verbose
//          ALERT("O height atual ï¿½ "+LTRIM(STR(N_HeightOld))+;
//                " e width atual ï¿½ "+LTRIM(STR(N_WidthOld)))
//       ENDIF
//       *
//       IF N_Way == +1
//          N_HeightNew := N_HeightOld + 3
//          N_WidthNew  := N_WidthOld + 2
//       ELSEIF N_Way == -1
//          N_HeightNew := N_HeightOld - 3
//          N_WidthNew  := N_WidthOld - 2
//       ENDIF ERRO
//       *
//       IF N_HeightNew < 9 .OR. N_WidthNew < 6
//          * Letra ficou tï¿½o pequena que torna quase impossï¿½vel a leitura
//          IF L_Verbose
//             ALERT("Nï¿½o se tentarï¿½ mudar para height "+LTRIM(STR(N_HeightNew))+;
//                   " e width "+LTRIM(STR(N_WidthNew)))
//          ENDIF
//       ELSE
//          IF L_Verbose
//             ALERT("Se tentarï¿½ mudar para height "+LTRIM(STR(N_HeightNew))+;
//                           " e width "+LTRIM(STR(N_WidthNew)))
//          ENDIF
//          *
//          N_MaxMaxRow := WVW_MAXMAXROW()
//          N_MaxMaxCol := WVW_MAXMAXCOL()
//          Wvw_SetFont(,C_Fonte,N_HeightNew,N_WidthNew)
//          Wvw_PBSetFont(,C_Fonte,N_HeightNew,N_WidthNew)
//          *
//          IF WVW_MAXMAXROW() == N_MaxMaxRow .OR. ;
//             WVW_MAXMAXCOL() == N_MaxMaxCol
//             IF L_Verbose
//                ALERT("A mudanï¿½a Nï¿½O foi realizada com sucesso")
//             ENDIF
//             Wvw_SetFont(,C_Fonte,N_HeightOld,N_WidthOld)
//             Wvw_PBSetFont(,C_Fonte,N_HeightOld,N_WidthOld)
//          ELSE
//             IF L_Verbose
//                ALERT("A mudanï¿½a foi realizada com sucesso")
//             ENDIF
//             L_Mudou := .T.
//          ENDIF
//       ENDIF
//       *
//    RETURN L_Mudou
//    *

************************
FUNC TelaPrincipalHeight    // Altura APROXIMADO, em pixels, da tela principal
************************
LOCAL N_TelaPrincipalHeight := NAP_HEIGHT()
RETURN N_TelaPrincipalHeight

***********************
FUNC TelaPrincipalWidth     // Largura APROXIMADA, em pixels, da tela principal
***********************
LOCAL N_TelaPrincipalWidth := NAP_WIDTH()
RETURN N_TelaPrincipalWidth

***************************
FUNCTION GETCLIPBOARD_ASPEC
***************************
* NOTA: Os seguintes acentos em cï¿½digo diferente na PC850 e na PC437: "ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½Ú§"
*
LOCAL C_RETORNO
*
IF SOB_MODO_GRAFICO()

    //
    // FRAN: TODO
    //
//    #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//       * COMO ESTAVA ANTES:
//       *   - Nï¿½o dï¿½ certo usar o comando abaixo para pegar o texto do "clipboard":
//       *        C_RETORNO := HB_GTINFO(HB_GTI_CLIPBOARDDATA)
//       *   - Se for usado, parte dos caracteres acentuados vem trocados
//       *     pela letra equivalente sem o acento (ex: "ï¿½" vem como "A").
//       *   - Isto ocorre em todos os acentos da PC850 que sï¿½o diferentes na PC437
//       *     ("ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½Ú§").
//       *   - Tentou-se resolver esta questï¿½o usando WVW_GetClipboard(),
//       *     mas ocorreu o mesmo problema.
//       *
//       * COMO RESOLVER O PROBLEMA:
//       *   - Mudar a pï¿½gina de cï¿½digo da GTWVW "de volta" para a original (255-OEM para 1252-ANSI)
//       *   - Ler o texto do "clipboard" (vem na pï¿½gina de cï¿½digo do Windows)
//       *   - Voltar a pï¿½gina de cï¿½digo da GTWVW para a 255-OEM.
//       *   - NOTA: Por alguns milissegundos, a pï¿½gina de cï¿½digo da GTWVW fica errada, frente
//       *           aos PRGs e DBFs do aplicativo, mas a tela existente continua
//       *           exibindo os acentos corretamente. Ou seja, visualmente nï¿½o fica errado,
//       *           nem por milissegundos...
//       *
//       * ATENï¿½ï¿½O: Foi colocado um "zero" fixo no primeiro parï¿½metro, pois NESTE EXE
//       *          parece que o "clipboard" do Windows se vincula ï¿½ janela inicial da GTWVW,
//       *          mesmo que se esteja posicionado em uma tela subsequente.
//       * GRAVE: Mas, em EXE gerado sem a CUA, teve-se se informar a janela "do topo", ao invï¿½s
//       *        janela inicial. Nï¿½o tivemos tempo de investigar o motivo desta diferenï¿½a
//       *        de comportamento...
//       #DEFINE N_WindowNum_Janela_Principal  0
//       WVW_SetCodePage(N_WindowNum_Janela_Principal,1252)  // 1252-ANSI
//       IF Version()=="Harbour 3.2.0dev (r1703241902)"
//          C_RETORNO := HB_AnsiToOEM(HB_GTINFO(HB_GTI_CLIPBOARDDATA))  // modo grï¿½fico
//       ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // PENDENTE_LINUX
//          C_RETORNO := HB_GTINFO(HB_GTI_CLIPBOARDDATA)  // modo grï¿½fico
//       ENDIF
//       WVW_SetCodePage(N_WindowNum_Janela_Principal,255)   //  255-OEM
//       #UNDEF N_WindowNum_Janela_Principal
//    #elif defined(__PLATFORM__LINUX)
//       // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//    #else
//       #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
//    #endif
ELSE
   * Tudo funciona normalmente (acentuaï¿½ï¿½o vem correta do "clipboard")
   C_RETORNO := HB_GTINFO(HB_GTI_CLIPBOARDDATA)  // modo texto
ENDIF
*
RETURN C_RETORNO
*
// ***********************
PROC SETCLIPBOARD_ASPEC(C_Texto)
***********************
* NOTA: Os seguintes acentos em cï¿½digo diferente na PC850 e na PC437: "ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½Ú§"
*
* Caso o texto contenha o CHR(141)=="ï¿½", desconsiderar esta tecla,
* pois a mesma tem significado especial para o Clipper (soft carriage return - junto com o CHR(10)),
* sendo comum sua existï¿½ncia em textos (remover antes de copiar para o "clipboard")
*
C_Texto := STRTRAN(C_Texto,CHR(141)," ")  // CHR(141)=="ï¿½"
*
IF SOB_MODO_GRAFICO()

    //
    // FRAN: TODO
    //
//    #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//       * COMO ESTAVA ANTES:
//       *   - Nï¿½o dï¿½ certo usar o comando abaixo para copiar o texto para o "clipboard":
//       *        HB_GTINFO(HB_GTI_CLIPBOARDDATA,C_Texto)
//       *   - Se for usado, parte dos caracteres acentuados vï¿½o errados
//       *     (vï¿½o como caracteres de borda de boxes !). Isto ocorre em todos os
//       *     acentos da PC850 que sï¿½o diferentes na PC437 ("ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½Ú§").
//       *   - A saï¿½da temporï¿½ria foi excluir do textos os referidos caracteres acentuados.
//       *       C_Texto := TROCA(C_Texto,"ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½ï¿½Ú§",;
//       *                                "AAAAaEIOOOoU ")
//       *   - Tentou-se resolver esta questï¿½o usando WVW_SetClipboard() e a
//       *     WVW_PasteFromClipboard(), mas ocorreu o mesmo problema.
//       *
//       * COMO RESOLVER O PROBLEMA:
//       *   - Mudar a pï¿½gina de cï¿½digo da GTWVW "de volta" para a original (255-OEM para 1252-ANSI)
//       *   - Colar no "clipboard" um texto jï¿½ na pï¿½gina de cï¿½digo do Windows
//       *   - Voltar a pï¿½gina de cï¿½digo da GTWVW para a 255-OEM.
//       *   - NOTA: Por alguns milissegundos, a pï¿½gina de cï¿½digo da GTWVW fica errada, frente
//       *           aos PRGs e DBFs do aplicativo, mas a tela existente continua
//       *           exibindo os acentos corretamente. Ou seja, visualmente nï¿½o fica errado,
//       *           nem por milissegundos...
//       *
//       * ATENï¿½ï¿½O: Foi colocado um "zero" fixo no primeiro parï¿½metro, pois NESTE EXE
//       *          parece que o "clipboard" do Windows se vincula ï¿½ janela inicial da GTWVW,
//       *          mesmo que se esteja posicionado em uma tela subsequente.
//       * GRAVE: Mas, em EXE gerado sem a CUA, teve-se se informar a janela "do topo", ao invï¿½s
//       *        janela inicial. Nï¿½o tivemos tempo de investigar o motivo desta diferenï¿½a
//       *        de comportamento...
//       #DEFINE N_WindowNum_Janela_Principal  0
//       WVW_SetCodePage(N_WindowNum_Janela_Principal,1252)  // 1252-ANSI
//       IF Version()=="Harbour 3.2.0dev (r1703241902)"
//          HB_GTINFO(HB_GTI_CLIPBOARDDATA,HB_OEMToAnsi(C_Texto))  // modo grï¿½fico
//       ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // ADAPTACAO_LINUX
//          HB_GTINFO(HB_GTI_CLIPBOARDDATA,C_Texto)  // modo grï¿½fico
//       ENDIF
//       WVW_SetCodePage(N_WindowNum_Janela_Principal,255)   //  255-OEM
//       #UNDEF N_WindowNum_Janela_Principal
//    #elif defined(__PLATFORM__LINUX)
//       // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
//    #else
//       #erro "Cï¿½digo nï¿½o adaptado para esta plataforma"
//    #endif
ELSE
   * Tudo funciona normalmente (acentuaï¿½ï¿½o vai correta para o "clipboard")
   HB_GTINFO(HB_GTI_CLIPBOARDDATA,C_Texto)  // modo texto
ENDIF
// *
// *************** FIM DO outros.prg

PROCEDURE LOG_PRINT(C_Text)
    IF SOB_MODO_GRAFICO()
        NAP_LOG(C_Text)
    ELSE
        NAP_LOG(C_Text)
    ENDIF

    RETURN




