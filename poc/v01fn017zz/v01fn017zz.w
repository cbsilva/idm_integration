&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emsfnd           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
{include/i-prgvrs.i V01FN017ZZ 2.00.00.038 } /*** "010038" ***/

{include/i_framevers.i}
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/*Vari†veis definidas para atualizar valores para programa de complemento RH*/
DEFINE NEW GLOBAL SHARED VARIABLE v-row-parent
    AS ROWID
    NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE v_ind_tip_usuar_cor
    AS CHARACTER
    FORMAT "X(13)"
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS "Super", "Super","Admin", "Admin","Supervisor", "Supervisor","Comum", "Comum"
    /*l_super*/ /*l_super*/ /*l_administrador*/ /*l_administrador*/ /*l_supervisor*/ /*l_supervisor*/ /*l_comum*/ /*l_comum*/
    BGCOLOR 8 
    LABEL "Tipo Usu†rio"
    COLUMN-LABEL "Tipo Usu†rio"
    NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE v_cod_usuar_corren
    AS CHARACTER
    FORMAT "x(12)":U
    LABEL "Usuˇrio Corrente"
    COLUMN-LABEL "Usuˇrio Corrente"
    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_usuar_mestre
    AS RECID
    FORMAT ">>>>>>9"
    INITIAL ?
    NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE v_log_exec_atualiza AS LOGICAL   NO-UNDO.

DEFINE                   VARIABLE c-cod-usuario       AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE v_cod_usuar
    AS CHARACTER
    FORMAT "x(12)"
    LABEL "Usu†rio"
    COLUMN-LABEL "Usu†rio"
    NO-UNDO.

DEFINE BUFFER b_usuar_mestre FOR usuar_mestre.

DEFINE VARIABLE v_cod_usuar_dest
    AS CHARACTER
    FORMAT "x(12)"
    LABEL "Usu†rio Destino"
    NO-UNDO.
DEFINE VARIABLE v_log_layout_impr_cop
    AS LOGICAL
    FORMAT "Sim/N∆o"
    INITIAL NO
    VIEW-AS TOGGLE-BOX
    LABEL "Layouts Padr‰es"
    NO-UNDO.
DEFINE VARIABLE v_log_grp_novo_usuar
    AS LOGICAL
    FORMAT "Sim/N∆o"
    INITIAL NO
    VIEW-AS TOGGLE-BOX
    LABEL "Grupos do Usu†rio"
    NO-UNDO.
DEFINE VARIABLE v_log_impr_cop
    AS LOGICAL
    FORMAT "Sim/N∆o"
    INITIAL NO
    VIEW-AS TOGGLE-BOX
    LABEL "Impressoras"
    NO-UNDO.

DEFINE VARIABLE c-fra1                AS CHARACTER FORMAT "x(8)".

DEFINE BUTTON bt_ok
    LABEL "OK"
    SIZE 10 BY 1
    AUTO-GO.
DEFINE BUTTON bt_zoo_245507
    LABEL "Zoom"
    IMAGE-UP FILE "image/im-zoo"
    IMAGE-INSENSITIVE FILE "image/ii-zoo"
    SIZE 4 BY .88.
DEFINE BUTTON bt_zoo_245534
    LABEL "Zoom"
    IMAGE-UP FILE "image/im-zoo"
    IMAGE-INSENSITIVE FILE "image/ii-zoo"
    SIZE 4 BY .88.
DEFINE BUTTON bt_can
    LABEL "Cancela"
    SIZE 10 BY 1
    AUTO-ENDKEY.
DEFINE BUTTON bt_hel2
    LABEL "Ajuda"
    SIZE 10 BY 1.

DEFINE RECTANGLE rt_cxtf
    SIZE 1 BY 1
    FGCOLOR 0 EDGE-PIXELS 2.
DEFINE RECTANGLE rt_key
    SIZE 1 BY 1
    EDGE-PIXELS 2.
DEFINE RECTANGLE rt_mold
    SIZE 1 BY 1
    EDGE-PIXELS 2.
DEFINE RECTANGLE rt_rgf
    SIZE 1 BY 1
    EDGE-PIXELS 2.
DEFINE RECTANGLE rt_001
    SIZE 1 BY 1
    EDGE-PIXELS 2.
DEFINE RECTANGLE rt_003
    SIZE 1 BY 1
    EDGE-PIXELS 2.
DEFINE RECTANGLE rt_004
    SIZE 1 BY 1
    EDGE-PIXELS 2.
DEFINE RECTANGLE rt_cxcf
    SIZE 1 BY 1
    FGCOLOR 1 EDGE-PIXELS 2.

DEFINE FRAME f_dlg_03_usuar_mestre_copia
    rt_004
    AT ROW 02.83 COL 01.86 BGCOLOR 8 
    rt_003
    AT ROW 04.50 COL 03.29
    c-fra1 VIEW-AS TEXT NO-LABELS
    AT ROW 04.20 COL 05.29 BGCOLOR 8 
    rt_cxcf
    AT ROW 07.96 COL 02.00 BGCOLOR 7 
    rt_001
    AT ROW 01.38 COL 01.86 BGCOLOR 8 
    usuar_mestre.cod_usuario
    AT ROW 01.58 COL 18.00 COLON-ALIGNED LABEL "Usu†rio"
    VIEW-AS FILL-IN
    SIZE-CHARS 13.14 BY .88
    FGCOLOR ? BGCOLOR 15 FONT 2
    bt_zoo_245507
    AT ROW 01.58 COL 33.14
    usuar_mestre.nom_usuario
    AT ROW 01.58 COL 38.00 NO-LABELS
    VIEW-AS FILL-IN
    SIZE-CHARS 33.14 BY .88
    FGCOLOR ? BGCOLOR 15 FONT 2
    v_cod_usuar_dest
    AT ROW 03.08 COL 18.00 COLON-ALIGNED LABEL "Usu†rio Destino"
    HELP "C¢digo do Usu†rio Destino"
    VIEW-AS FILL-IN
    SIZE-CHARS 13.14 BY .88
    FGCOLOR ? BGCOLOR 15 FONT 2
    bt_zoo_245534
    AT ROW 03.08 COL 33.14
    b_usuar_mestre.nom_usuario
    AT ROW 03.13 COL 38.00 NO-LABELS
    VIEW-AS FILL-IN
    SIZE-CHARS 33.14 BY .88
    FGCOLOR ? BGCOLOR 15 FONT 2
    v_log_grp_novo_usuar
    AT ROW 05.00 COL 11.00 LABEL "Grupos do Usu†rio"
    HELP "Copia Grupos de Usu†rios do Usu†rio"
    VIEW-AS TOGGLE-BOX
    v_log_impr_cop
    AT ROW 05.00 COL 45.00 LABEL "Impressoras"
    HELP "Copia Impressoras do Usu†rio"
    VIEW-AS TOGGLE-BOX
    v_log_layout_impr_cop
    AT ROW 06.00 COL 45.00 LABEL "Layouts Padr‰es"
    HELP "Copia Layouts Padr‰es do Usu†rio"
    VIEW-AS TOGGLE-BOX
    bt_ok
    AT ROW 08.21 COL 03.00 FONT ?
    HELP "OK"
    bt_can
    AT ROW 08.21 COL 14.00 FONT ?
    HELP "Cancela"
    bt_hel2
    AT ROW 08.21 COL 61.57 FONT ?
    HELP "Ajuda"
    WITH 1 DOWN SIDE-LABELS NO-VALIDATE KEEP-TAB-ORDER THREE-D
    SIZE-CHARS 74.00 BY 09.79 DEFAULT-BUTTON bt_ok
    VIEW-AS DIALOG-BOX
    FONT 1 FGCOLOR ? BGCOLOR 8
    TITLE "Copia Relaá‰es do Usu†rio".


{utp/ut0666.i}
{utp/ut-gerdoc.i}

DEFINE VARIABLE hFluig      AS HANDLE  NO-UNDO.
DEFINE VARIABLE lFluig      AS LOGICAL NO-UNDO.
DEFINE VARIABLE h-bofn017   AS HANDLE  NO-UNDO.

DEFINE BUFFER busuar_mestre FOR usuar_mestre.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES usuar_mestre
&Scoped-define FIRST-EXTERNAL-TABLE usuar_mestre


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR usuar_mestre.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-mold bt_compl_uhr btCaminhoPDF ~
btGrupoSeguranca btConfigPDF btMultiIdioma bt_copy_usuar bt_perfil_usuar ~
btPrefUsuar btSessoesProgress 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validarAlteracaoSenha V-table-Win 
FUNCTION validarAlteracaoSenha RETURNS LOGICAL
    ( INPUT p-log-enable AS LOGICAL /* habilita ou nao conforme hierarquia */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btAlteraSenha 
     LABEL "Alterar Senha" 
     SIZE 20 BY 1.13.

DEFINE BUTTON btCaminhoPDF 
     LABEL "Caminho Leitor PDF" 
     SIZE 20 BY 1.13.

DEFINE BUTTON btConfigPDF 
     LABEL "Configuraá∆o PDF" 
     SIZE 20 BY 1.13.

DEFINE BUTTON btGrupoSeguranca 
     LABEL "Grupo Seguranáa" 
     SIZE 20 BY 1.13.

DEFINE BUTTON btMultiIdioma 
     LABEL "Multi-Idioma" 
     SIZE 20 BY 1.13.

DEFINE BUTTON btPrefUsuar      
     LABEL &IF "{&frame_version}" = "1" &THEN "Atalhos de Execuá∆o" &ELSE "Prefer. Acesso" &ENDIF
     SIZE 20 BY 1.13.

DEFINE BUTTON btSessoesProgress 
     LABEL "Quantidade Sess‰es DI" 
     SIZE 20 BY 1.13 TOOLTIP "Configurador Controle de Sess‰es do DI por usu†rio".

DEFINE BUTTON bt_compl_uhr 
     LABEL "Compl RH" 
     SIZE 20 BY 1.13.

DEFINE BUTTON bt_copy_usuar 
     LABEL "Copiar Relac." 
     SIZE 20 BY 1.13 TOOLTIP "Copiar Relacionamentos do Usu†rio".

DEFINE BUTTON bt_perfil_usuar 
     LABEL "Perfil Usu†rio" 
     SIZE 20 BY 1.13 TOOLTIP "Copiar Relacionamentos do Usu†rio".

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 9.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     bt_compl_uhr AT ROW 1.5 COL 11 WIDGET-ID 4
     btCaminhoPDF AT ROW 1.5 COL 40 WIDGET-ID 12
     btGrupoSeguranca AT ROW 2.75 COL 11 WIDGET-ID 6
     btConfigPDF AT ROW 2.75 COL 40 WIDGET-ID 14
     btMultiIdioma AT ROW 4 COL 11 WIDGET-ID 8
     bt_copy_usuar AT ROW 4 COL 40 WIDGET-ID 16
     btAlteraSenha AT ROW 5.25 COL 11 WIDGET-ID 10
     bt_perfil_usuar AT ROW 5.25 COL 40 WIDGET-ID 24
     btPrefUsuar AT ROW 6.54 COL 11 HELP &IF "{&frame_version}" = "1" &THEN "Atalhos de Execuá∆o" &ELSE "Preferencias de acesso." &ENDIF WIDGET-ID 22
     btSessoesProgress AT ROW 6.54 COL 40 WIDGET-ID 26
     rt-mold AT ROW 1 COL 1
     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: emsfnd.usuar_mestre
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 9.79
         WIDTH              = 75.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btAlteraSenha IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btAlteraSenha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAlteraSenha V-table-Win
ON CHOOSE OF btAlteraSenha IN FRAME f-main /* Alterar Senha */
DO:
    IF  c-cod-usuario = "" 
    OR  c-cod-usuario = ? THEN
        RETURN.

    FIND b_usuar_mestre NO-LOCK
        WHERE b_usuar_mestre.cod_usuario = c-cod-usuario NO-ERROR.
    IF  NOT AVAILABLE b_usuar_mestre THEN
        RETURN.
                
    ASSIGN 
        v_rec_usuar_mestre = RECID(b_usuar_mestre).

    /*  Foi removida a exibicao da mensagem 34074 pois quando o usuario for 'Extreno' 
        o botao e desabilitado e com isso, a condicao nunca sera executada
    */

    IF  v_cod_usuar_corren  <> c-cod-usuario
    AND v_ind_tip_usuar_cor <> "Super" /*l_super*/ THEN 
    DO:
        /* Somente super usuarios podem alterar senha de outro usuario. */
        RUN utp/ut-msgs.p (INPUT "show",
            INPUT 4760,
            INPUT SUBSTITUTE ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_3207*/.
        RETURN.
    END /* if */.
    
    RUN sec/sec000zb.p (INPUT v_rec_usuar_mestre) /*prg_fnc_trocar_senha_outro_usuario*/.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCaminhoPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCaminhoPDF V-table-Win
ON CHOOSE OF btCaminhoPDF IN FRAME f-main /* Caminho Leitor PDF */
DO:
    RUN sec/sec000zm.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConfigPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigPDF V-table-Win
ON CHOOSE OF btConfigPDF IN FRAME f-main /* Configuraá∆o PDF */
DO:
    IF c-cod-usuario <> "" THEN 
    DO:
        RUN sec/sec000zj.w (INPUT c-cod-usuario) /*prg_fnc_param_extens_ems_caminho_pdf*/.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGrupoSeguranca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGrupoSeguranca V-table-Win
ON CHOOSE OF btGrupoSeguranca IN FRAME f-main /* Grupo Seguranáa */
DO:
    IF c-cod-usuario <> "" THEN 
    DO:
        FIND FIRST b_usuar_mestre 
            WHERE b_usuar_mestre.cod_usuario = c-cod-usuario NO-LOCK NO-ERROR.

        IF AVAILABLE b_usuar_mestre THEN 
        DO:
            RUN sec/sec000zf.p(INPUT b_usuar_mestre.cod_usuario, INPUT b_usuar_mestre.nom_usuario).

            IF v_log_exec_atualiza = YES THEN 
            DO:
                IF SEARCH("men/mer010aa.w") <> ? THEN
                    RUN men/mer010aa.w.
                ELSE IF SEARCH("men/mer010aa.r") <> ? THEN
                        RUN men/mer010aa.r.      
            
                v_log_exec_atualiza = NO.
            END.
        END /* if */.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMultiIdioma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMultiIdioma V-table-Win
ON CHOOSE OF btMultiIdioma IN FRAME f-main /* Multi-Idioma */
DO:
    RUN sec/sec000zg.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrefUsuar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrefUsuar V-table-Win
ON CHOOSE OF btPrefUsuar IN FRAME f-main /* Prefer. Acesso */
DO:
    /* --- Solicita as preferencias de acessodo usuario - */
    RUN sec/sec000ad.w ( INPUT c-cod-usuario ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btSessoesProgress
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSessoesProgress V-table-Win
ON CHOOSE OF btSessoesProgress IN FRAME f-main /* Sess‰es Progress */
DO:
    IF  c-cod-usuario = "" OR  c-cod-usuario = ? THEN DO:
        RETURN.
    END.
    
    FIND b_usuar_mestre NO-LOCK WHERE b_usuar_mestre.cod_usuario = c-cod-usuario NO-ERROR.
    IF  NOT AVAILABLE b_usuar_mestre THEN DO:
        RETURN.
    END.

    ASSIGN v_rec_usuar_mestre = RECID(b_usuar_mestre).
    
    RUN sec/sec000ae.p (INPUT v_rec_usuar_mestre).
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME bt_compl_uhr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_compl_uhr V-table-Win
ON CHOOSE OF bt_compl_uhr IN FRAME f-main /* Compl RH */
DO:
    IF c-cod-usuario <> "" THEN 
    DO:
        FIND FIRST b_usuar_mestre 
            WHERE b_usuar_mestre.cod_usuario = c-cod-usuario NO-LOCK NO-ERROR.
        IF AVAILABLE b_usuar_mestre THEN 
        DO:

            ASSIGN 
                v_rec_usuar_mestre = RECID(b_usuar_mestre).

            IF SEARCH("prghur/fpp/fp9100.r") = ? AND SEARCH("prghur/fpp/fp9100.w") = ? THEN 
            DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT 4,
                                   INPUT SUBSTITUTE ("prghur/fpp/fp9100.w")).
                RETURN NO-APPLY.
            END.
            ASSIGN 
                v-row-parent = ROWID(b_usuar_mestre).
            RUN prghur/fpp/fp9100.w /*prg_upd_usuar_aplicat_hur*/.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_copy_usuar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_copy_usuar V-table-Win
ON CHOOSE OF bt_copy_usuar IN FRAME f-main /* Copiar Relac. */
DO:

    RUN pi_usuar_mestre_copia /*pi_usuar_mestre_copia*/.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_perfil_usuar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_perfil_usuar V-table-Win
ON CHOOSE OF bt_perfil_usuar IN FRAME f-main /* Perfil Usu†rio */
DO:
    IF  c-cod-usuario = "" 
    OR  c-cod-usuario = ? THEN
        RETURN.

    FIND b_usuar_mestre NO-LOCK
        WHERE b_usuar_mestre.cod_usuario = c-cod-usuario NO-ERROR.
    IF  NOT AVAILABLE b_usuar_mestre THEN
        RETURN.
                
    ASSIGN 
        v_rec_usuar_mestre = RECID(b_usuar_mestre).


    RUN sec/sec000zk.p (INPUT v_rec_usuar_mestre).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


ON  CHOOSE OF bt_zoo_245507 IN FRAME f_dlg_03_usuar_mestre_copia DO:

    /* fn_generic_zoom */
    IF  SEARCH("sec/sec000ka.r") = ? AND SEARCH("sec/sec000ka.p") = ? THEN 
    DO:
        /* Inicio -- Projeto Internacional */
        {utp/ut-liter.i "Programa_execut†vel_n∆o_foi_encontrado" *}
        MESSAGE RETURN-VALUE + ":" /*l_programa_nao_encontrado*/  "sec/sec000ka.p"
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
    ELSE
        RUN sec/sec000ka.p /*prg_sea_usuar_mestre*/.

    IF  v_rec_usuar_mestre <> ?
    THEN DO:
        FIND usuar_mestre WHERE RECID(usuar_mestre) = v_rec_usuar_mestre NO-LOCK NO-ERROR.
        ASSIGN usuar_mestre.cod_usuario:screen-value IN FRAME f_dlg_03_usuar_mestre_copia =
               STRING(usuar_mestre.cod_usuario).
        DISPLAY usuar_mestre.nom_usuario
                WITH FRAME f_dlg_03_usuar_mestre_copia.
        APPLY "entry" TO usuar_mestre.cod_usuario IN FRAME f_dlg_03_usuar_mestre_copia.
    END.

END. /* ON  CHOOSE OF bt_zoo_245507 IN FRAME f_dlg_03_usuar_mestre_copia */

ON  CHOOSE OF bt_zoo_245534 IN FRAME f_dlg_03_usuar_mestre_copia DO:

    /* fn_generic_zoom_variable */
    IF  SEARCH("sec/sec000ka.r") = ? AND SEARCH("sec/sec000ka.p") = ? THEN DO:
        /* Inicio -- Projeto Internacional */
        {utp/ut-liter.i "Programa_execut†vel_n∆o_foi_encontrado" *}
        MESSAGE RETURN-VALUE + ":" /*l_programa_nao_encontrado*/  "sec/sec000ka.p"
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
    ELSE
        RUN sec/sec000ka.p /*prg_sea_usuar_mestre*/.

    IF  v_rec_usuar_mestre <> ?
    THEN DO:
        FIND b_usuar_mestre WHERE RECID(b_usuar_mestre) = v_rec_usuar_mestre NO-LOCK NO-ERROR.
        ASSIGN v_cod_usuar_dest:screen-value IN FRAME f_dlg_03_usuar_mestre_copia =
               STRING(b_usuar_mestre.cod_usuario).
        DISPLAY b_usuar_mestre.nom_usuario
                WITH FRAME f_dlg_03_usuar_mestre_copia.
        APPLY "entry" TO v_cod_usuar_dest IN FRAME f_dlg_03_usuar_mestre_copia.
    END /* if */.

END. /* ON  CHOOSE OF bt_zoo_245534 IN FRAME f_dlg_03_usuar_mestre_copia */

ON CHOOSE OF bt_ok IN FRAME f_dlg_03_usuar_mestre_copia DO:
    RUN pi_choose_ok.
END.

ON VALUE-CHANGED OF v_log_impr_cop IN FRAME f_dlg_03_usuar_mestre_copia
DO:

    IF  v_log_impr_cop:checked IN FRAME f_dlg_03_usuar_mestre_copia = NO THEN
        ASSIGN v_log_layout_impr_cop:checked IN FRAME f_dlg_03_usuar_mestre_copia = NO
               v_log_layout_impr_cop:sensitive IN FRAME f_dlg_03_usuar_mestre_copia = NO.
    ELSE
        v_log_layout_impr_cop:sensitive IN FRAME f_dlg_03_usuar_mestre_copia = YES.    

END.


ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_usuar_mestre_copia DO:

    /*exibir a ajuda */
    RUN men/men900za.p (INPUT SELF:frame,
                        INPUT THIS-PROCEDURE:HANDLE) /*prg_fnc_chamar_help_context*/.

END.
/* ***************************  Main Block  *************************** */

IF SEARCH("fluig/fluigAPI.r") <> ? OR SEARCH("fluig/fluigAPI.p") <> ? THEN DO:                
    RUN fluig/fluigAPI.p PERSISTENT SET hFluig.
    
    IF VALID-HANDLE(hFluig) THEN DO:
        RUN integrationActive IN hFluig( OUTPUT lFluig ).
    END.
    DELETE PROCEDURE hFluig NO-ERROR.
END.    

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF         

/************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "usuar_mestre"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "usuar_mestre"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-botoes V-table-Win 
PROCEDURE habilita-botoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-log-enable        AS LOGICAL NO-UNDO. /* variavel que diz se pode alterar com base na hierarquia */    
    DEFINE INPUT PARAMETER p-log-enable-grupos AS LOGICAL NO-UNDO. /* variavel que diz se pode alterar grupos de seguranca da aba geral */

        
    &IF "{&product_version}" >= "12.1.M" &THEN
    FIND FIRST usuar_mestre_aux
        WHERE  usuar_mestre_aux.cod_usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
         
        IF AVAIL usuar_mestre_aux THEN DO:
            IF usuar_mestre_aux.log_inativ = TRUE THEN DO:
                ASSIGN p-log-enable        = FALSE
                       p-log-enable-grupos = FALSE.
            END.
        END. 
    &ENDIF
        
    ASSIGN btAlteraSenha:SENSITIVE IN FRAME {&FRAME-NAME}    = validarAlteracaoSenha(p-log-enable).

    ASSIGN bt_compl_uhr:SENSITIVE IN FRAME {&FRAME-NAME}     = p-log-enable
           btMultiIdioma:SENSITIVE IN FRAME {&FRAME-NAME}    = p-log-enable
           btPrefUsuar:SENSITIVE IN FRAME {&FRAME-NAME}      = p-log-enable
           bt_perfil_usuar:SENSITIVE IN FRAME {&FRAME-NAME}  = p-log-enable
           btCaminhoPDF:SENSITIVE IN FRAME {&FRAME-NAME}     = p-log-enable
           btConfigPDF:SENSITIVE IN FRAME {&FRAME-NAME}      = p-log-enable
           bt_copy_usuar:SENSITIVE IN FRAME {&FRAME-NAME}    = p-log-enable
           btSessoesProgress:SENSITIVE IN FRAME {&FRAME-NAME} = p-log-enable
           btGrupoSeguranca:SENSITIVE IN FRAME {&FRAME-NAME} = p-log-enable-grupos.

    &IF  "{&emsfnd_dbtype}":U <> "oracle" OR "{&product_version}" < "12.1.P" OR "{&frame_version}" = "1" &THEN
        btSessoesProgress:VISIBLE IN FRAME {&FRAME-NAME} = FALSE.
    &ENDIF
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}

    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
        RETURN 'ADM-ERROR':U.

    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF VALID-HANDLE(h-bofn017) THEN
        DELETE PROCEDURE h-bofn017.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

    /* Code placed here will execute AFTER standard behavior.    */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    DISABLE {&ADM-MODIFY-FIELDS} WITH FRAME {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    
    /* Dispatch standard ADM method.                             */

    RUN dispatch IN This-procedure ( INPUT 'display-fields':U ) .

    ASSIGN c-cod-usuario = usuar_mestre.cod_usuario.

    RUN new-state("habilita-botoes-geral":U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    IF adm-new-record = YES THEN
        ENABLE {&ADM-MODIFY-FIELDS} WITH FRAME {&frame-name}.
    &endif
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    IF NOT VALID-HANDLE(h-bofn017) THEN
        RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN bt_compl_uhr:SENSITIVE IN FRAME {&FRAME-NAME} = CONNECTED('dthrpyc').
        
    &IF "{&product_version}" >= "12.1.M" &THEN
    FIND FIRST usuar_mestre_aux
        WHERE  usuar_mestre_aux.cod_usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
         
        IF AVAIL usuar_mestre_aux THEN DO:
            IF usuar_mestre_aux.log_inativ = TRUE THEN DO:
                bt_compl_uhr:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
            END.
        END.
    &ENDIF
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER v-row-parent-externo AS ROWID NO-UNDO.

    ASSIGN v-row-parent = v-row-parent-externo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
      Purpose:Validar a viewer     
      Parameters:  <none>
      Notes: N∆o fazer assign aqui. Nesta procedure
      devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
      ainda n∆o foi criado.       
    ------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */

/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */

      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_choose_ok V-table-Win 
PROCEDURE pi_choose_ok :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /************************** Buffer Definition Begin *************************/

    DEFINE BUFFER b_imprsor_usuar
        FOR imprsor_usuar.
    DEFINE BUFFER b_layout_impres_padr_copy
        FOR layout_impres_padr.
    DEFINE BUFFER b_usuar_grp_usuar_copy
        FOR usuar_grp_usuar.
    DEFINE BUFFER b_usuar_mestre_corren
        FOR usuar_mestre.

    /*************************** Buffer Definition End **************************/

    ASSIGN INPUT FRAME f_dlg_03_usuar_mestre_copia v_cod_usuar_dest
           INPUT FRAME f_dlg_03_usuar_mestre_copia v_log_grp_novo_usuar
           INPUT FRAME f_dlg_03_usuar_mestre_copia v_log_impr_cop
           INPUT FRAME f_dlg_03_usuar_mestre_copia v_log_layout_impr_cop.
    FIND usuar_mestre NO-LOCK
         WHERE usuar_mestre.cod_usuario = INPUT FRAME f_dlg_03_usuar_mestre_copia
               usuar_mestre.cod_usuario /*cl_dlg_03_usuar_mestre_copia of usuar_mestre*/ NO-ERROR.
    FIND b_usuar_mestre NO-LOCK
         WHERE b_usuar_mestre.cod_usuario = v_cod_usuar_dest 
              /*cl_v_cod_usuar_dest of b_usuar_mestre*/ NO-ERROR.
    IF  NOT AVAILABLE usuar_mestre OR NOT AVAILABLE b_usuar_mestre
    THEN DO:
        /* Usu†rio &1 Inexistente ! */
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 4823,
                           INPUT SUBSTITUTE ("")) /*msg_2584*/.
        RETURN NO-APPLY.
    END /* if */.

    /* Verifica tipo do Usu†rio */
    FIND b_usuar_mestre_corren NO-LOCK
        WHERE b_usuar_mestre_corren.cod_usuario = v_cod_usuar_corren
        USE-INDEX srmstr_id /*cl_usuar_corren of b_usuar_mestre_corren*/ NO-ERROR.
    IF  b_usuar_mestre_corren.ind_tip_usuar = "Supervisor" /*l_supervisor*/ 
    THEN DO:
        IF  usuar_mestre.ind_tip_usuar <> "Comum" /*l_comum*/ 
        THEN DO:
            /* Vocà n∆o pode copiar relacionamentos de um usu†rio tipo &1 ! */
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 15415,
                               INPUT SUBSTITUTE ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                               usuar_mestre.ind_tip_usuar, usuar_mestre.cod_usuario)) /*msg_5607*/.
            RETURN NO-APPLY.
        END /* if */.
    END /* if */.

    IF  b_usuar_mestre_corren.ind_tip_usuar = "Admin" /*l_administrador*/ 
    THEN DO:
        IF  usuar_mestre.ind_tip_usuar <> "Comum" /*l_comum*/  
        AND usuar_mestre.ind_tip_usuar <> "Supervisor" /*l_supervisor*/ 
        THEN DO:
            /* Vocà n∆o pode copiar relacionamentos de um usu†rio tipo &1 ! */
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 15415,
                               INPUT SUBSTITUTE ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                               usuar_mestre.ind_tip_usuar, usuar_mestre.cod_usuario)) /*msg_5607*/.
            RETURN NO-APPLY.
        END /* if */.
    END /* if */.
    
    DEFINE VARIABLE sobrescrever_layouts AS LOGICAL NO-UNDO.
    IF v_log_layout_impr_cop THEN DO:
        /* Inicio -- Projeto Internacional */
        DEFINE VARIABLE c-lbl-liter-sobrescrever-layouts-padroes-j AS CHARACTER NO-UNDO.
        {utp/ut-liter.i "Sobrescrever_layouts_padr‰es_j†_definidos?" *}
        ASSIGN c-lbl-liter-sobrescrever-layouts-padroes-j = TRIM(RETURN-VALUE).
        DEFINE VARIABLE c-lbl-liter-deseja-substituir-os-layouts-p AS CHARACTER NO-UNDO.
        {utp/ut-liter.i "Deseja_substituir_os_layouts_padr‰es_j†_definidos_no_usu†rio_destino_pelos_layouts_padr‰es_definidos_para_os_mesmos_programas_no_usu†rio_origem?" *}
        ASSIGN c-lbl-liter-deseja-substituir-os-layouts-p = TRIM(RETURN-VALUE).
        RUN utp/ut-msgs.p("show":U,
                          27100,
                          c-lbl-liter-sobrescrever-layouts-padroes-j + "~~" +
                          c-lbl-liter-deseja-substituir-os-layouts-p).
    END.
    
    ASSIGN sobrescrever_layouts = (RETURN-VALUE = "yes":U).

    IF  SESSION:SET-WAIT-STATE('general') THEN .
    
    ASSIGN v_cod_usuar = usuar_mestre.cod_usuario.

    IF  v_log_grp_novo_usuar = YES
    THEN DO:
        grp_usuar:
        FOR EACH usuar_grp_usuar NO-LOCK
            WHERE usuar_grp_usuar.cod_usuario = v_cod_usuar /*cl_v_cod_usuar of usuar_grp_usuar*/:
            FIND b_usuar_grp_usuar_copy EXCLUSIVE-LOCK
                WHERE b_usuar_grp_usuar_copy.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar
                AND b_usuar_grp_usuar_copy.cod_usuario = v_cod_usuar_dest
                /*cl_usuar_grp_usuar of b_usuar_grp_usuar_copy*/ NO-ERROR.
            IF  NOT AVAILABLE b_usuar_grp_usuar_copy
            THEN DO:
                CREATE b_usuar_grp_usuar_copy.
                ASSIGN b_usuar_grp_usuar_copy.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar
                       b_usuar_grp_usuar_copy.cod_usuario   = v_cod_usuar_dest.
            END /* if */.
        END /* for grp_usuar */.

        IF v_log_integr_gerdoc = YES THEN DO:
            /* ÷nicio Integraá∆o com Gerenciador de Documentos */

             RUN utp/utapi022.p (INPUT "A",
                                 INPUT b_usuar_mestre.cod_usuario,
                                 INPUT b_usuar_mestre.nom_usuario,
                                 INPUT "N",
                                 INPUT "",
                                 INPUT b_usuar_mestre.cod_e_mail_local,
                                 INPUT b_usuar_mestre.dat_valid_senha,
                                 OUTPUT table tt-erro).

             FIND FIRST tt-erro NO-LOCK NO-ERROR.
             IF AVAILABLE tt-erro THEN DO:
                RUN utp/ut0666.w (INPUT table tt-erro).
             END. 

        /* Fim Integracao com Gerenciador de Documentos */
        END.
    END /* if */.

    IF  v_log_impr_cop = YES
    THEN DO:
        imprsor_usuar:
        FOR EACH imprsor_usuar NO-LOCK
            WHERE imprsor_usuar.cod_usuario = v_cod_usuar /*cl_v_cod_usuar of imprsor_usuar*/:

            FIND b_imprsor_usuar EXCLUSIVE-LOCK
                WHERE b_imprsor_usuar.cod_usuario = v_cod_usuar_dest
                AND b_imprsor_usuar.nom_impressora = imprsor_usuar.nom_impressora
                /*cl_v_cod_usuar_dest of b_imprsor_usuar*/ NO-ERROR.
            IF  NOT AVAILABLE b_imprsor_usuar
            THEN DO:
                CREATE b_imprsor_usuar.
                ASSIGN b_imprsor_usuar.nom_impressora    = imprsor_usuar.nom_impressora
                       b_imprsor_usuar.cod_usuario       = v_cod_usuar_dest
                       b_imprsor_usuar.nom_disposit_so   = imprsor_usuar.nom_disposit_so
                       b_imprsor_usuar.log_imprsor_princ = imprsor_usuar.log_imprsor_princ.
            END /* if */.
        END /* for imprsor_usuar */.
    END /* if */.

    IF  v_log_layout_impr_cop = YES
    THEN DO:
        layout_usuar:
        FOR EACH layout_impres_padr NO-LOCK
            WHERE layout_impres_padr.cod_usuario = v_cod_usuar /*cl_v_cod_usuar of layout_impres_padr*/:
            FIND b_imprsor_usuar EXCLUSIVE-LOCK
                WHERE b_imprsor_usuar.nom_impressora = layout_impres_padr.nom_impressora
                AND b_imprsor_usuar.cod_usuario    = v_cod_usuar_dest
                /*cl_layout_padr_usuar_dest of b_imprsor_usuar*/ NO-ERROR.
            IF  AVAILABLE b_imprsor_usuar
            THEN DO:
                FIND b_layout_impres_padr_copy EXCLUSIVE-LOCK
                    WHERE b_layout_impres_padr_copy.cod_usuario = v_cod_usuar_dest
                    AND b_layout_impres_padr_copy.cod_proced  = layout_impres_padr.cod_proced
                    /*cl_v_cod_usuar_dest of b_layout_impres_padr_copy*/ NO-ERROR.
                IF NOT AVAILABLE b_layout_impres_padr_copy THEN
                    CREATE b_layout_impres_padr_copy.
                IF NEW b_layout_impres_padr_copy OR sobrescrever_layouts THEN DO:
                    ASSIGN b_layout_impres_padr_copy.nom_impressora    = layout_impres_padr.nom_impressora
                           b_layout_impres_padr_copy.cod_usuario       = v_cod_usuar_dest
                           b_layout_impres_padr_copy.cod_proced        = layout_impres_padr.cod_proced
                           b_layout_impres_padr_copy.cod_layout_impres = layout_impres_padr.cod_layout_impres.

                END /* if */.
            END /* if */.
        END /* for layout_usuar */.
    END /* if */.

    IF  SESSION:SET-WAIT-STATE('') THEN .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_usuar_mestre_copia V-table-Win 
PROCEDURE pi_usuar_mestre_copia :
/*------------------------------------------------------------------------------
      Purpose:     copiada da versao anterior da manutencao de usuarios visa copiar
                    o relacionamento entre as tabelas grupo de usuarios, usuario mestre
                    e impressoras
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* adjust size of objects in this frame */
    ASSIGN bt_can:width-chars   IN FRAME f_dlg_03_usuar_mestre_copia = 10.00
           bt_can:height-chars  IN FRAME f_dlg_03_usuar_mestre_copia = 01.00
           bt_hel2:width-chars  IN FRAME f_dlg_03_usuar_mestre_copia = 10.00
           bt_hel2:height-chars IN FRAME f_dlg_03_usuar_mestre_copia = 01.00
           bt_ok:width-chars    IN FRAME f_dlg_03_usuar_mestre_copia = 10.00
           bt_ok:height-chars   IN FRAME f_dlg_03_usuar_mestre_copia = 01.00
           rt_001:width-chars   IN FRAME f_dlg_03_usuar_mestre_copia = 71.00
           rt_001:height-chars  IN FRAME f_dlg_03_usuar_mestre_copia = 01.29
           rt_003:width-chars   IN FRAME f_dlg_03_usuar_mestre_copia = 68.00
           rt_003:height-chars  IN FRAME f_dlg_03_usuar_mestre_copia = 02.75
           rt_004:width-chars   IN FRAME f_dlg_03_usuar_mestre_copia = 71.00
           rt_004:height-chars  IN FRAME f_dlg_03_usuar_mestre_copia = 04.79
           rt_cxcf:width-chars  IN FRAME f_dlg_03_usuar_mestre_copia = 70.57
           rt_cxcf:height-chars IN FRAME f_dlg_03_usuar_mestre_copia = 01.42.
    /* set private-data for the help system */
    ASSIGN bt_zoo_245507:private-data              IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000009431":U
           usuar_mestre.cod_usuario:private-data   IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000013111":U
           usuar_mestre.nom_usuario:private-data   IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000004724":U
           bt_zoo_245534:private-data              IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000009431":U
           v_cod_usuar_dest:private-data           IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000008264":U
           b_usuar_mestre.nom_usuario:private-data IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000004724":U
           v_log_grp_novo_usuar:private-data       IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000008264":U
           v_log_impr_cop:private-data             IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000008264":U
           v_log_layout_impr_cop:private-data      IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000008264":U
           bt_ok:private-data                      IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000010721":U
           bt_can:private-data                     IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000011050":U
           bt_hel2:private-data                    IN FRAME f_dlg_03_usuar_mestre_copia = "HLP=000011326":U
           FRAME f_dlg_03_usuar_mestre_copia:private-data                               = "HLP=000008264".
    /* enable function buttons */
    ASSIGN bt_zoo_245507:sensitive IN FRAME f_dlg_03_usuar_mestre_copia = YES
           bt_zoo_245534:sensitive IN FRAME f_dlg_03_usuar_mestre_copia = YES.    

    ON 'leave':U OF v_cod_usuar_dest DO:
        FIND FIRST usuar_mestre NO-LOCK 
             WHERE usuar_mestre.cod_usuario = v_cod_usuar_dest:Screen-value IN FRAME f_dlg_03_usuar_mestre_copia NO-ERROR.
        IF  AVAILABLE usuar_mestre THEN 
            DISPLAY usuar_mestre.nom_usuario @ b_usuar_mestre.nom_usuario WITH FRAME f_dlg_03_usuar_mestre_copia.
        ELSE 
            DISPLAY ""                       @ b_usuar_mestre.nom_usuario WITH FRAME f_dlg_03_usuar_mestre_copia.
    END.

    /* THCZD8 - cleiton.bonomini */

    ON 'leave':U OF usuar_mestre.cod_usuario DO:
        FIND FIRST usuar_mestre NO-LOCK 
             WHERE usuar_mestre.cod_usuario = usuar_mestre.cod_usuario:Screen-value IN FRAME f_dlg_03_usuar_mestre_copia NO-ERROR.
        IF  AVAILABLE usuar_mestre THEN 
            DISPLAY usuar_mestre.nom_usuario @ usuar_mestre.nom_usuario WITH FRAME f_dlg_03_usuar_mestre_copia.
        ELSE 
            DISPLAY ""                       @ usuar_mestre.nom_usuario WITH FRAME f_dlg_03_usuar_mestre_copia.
    END.

    {utp/ut-liter.i "Usu†rio" * R}
    ASSIGN usuar_mestre.cod_usuario:LABEL = TRIM(RETURN-VALUE).

    {utp/ut-liter.i "Usu†rio_Destino" * R}
    ASSIGN v_cod_usuar_dest:LABEL = TRIM(RETURN-VALUE). 
    
    {utp/ut-liter.i "Copiar" * R}
    ASSIGN c-fra1:SCREEN-VALUE IN FRAME f_dlg_03_usuar_mestre_copia = TRIM(RETURN-VALUE).

    {include/i_fclfrm.i f_dlg_03_usuar_mestre_copia }
    
    FIND usuar_mestre 
        WHERE usuar_mestre.cod_usuario = c-cod-usuario NO-LOCK NO-ERROR.

    ASSIGN b_usuar_mestre.nom_usuario:screen-value IN FRAME f_dlg_03_usuar_mestre_copia = ""
           v_cod_usuar_dest:screen-value IN FRAME f_dlg_03_usuar_mestre_copia           = ""
           v_log_grp_novo_usuar  = NO
           v_log_impr_cop        = NO
           v_log_layout_impr_cop = NO.
    VIEW FRAME f_dlg_03_usuar_mestre_copia.
    DISPLAY bt_can
            bt_hel2
            bt_ok
            usuar_mestre.cod_usuario
            usuar_mestre.nom_usuario
            v_log_grp_novo_usuar
            v_log_impr_cop
            v_log_layout_impr_cop
            WITH FRAME f_dlg_03_usuar_mestre_copia.
    ENABLE usuar_mestre.cod_usuario
           v_cod_usuar_dest
           v_log_grp_novo_usuar WHEN NOT lFluig
           v_log_impr_cop
           WITH FRAME f_dlg_03_usuar_mestre_copia.
    DISABLE v_log_layout_impr_cop
            WITH FRAME f_dlg_03_usuar_mestre_copia.
    UPDATE bt_can
           bt_hel2
           bt_ok
           WITH FRAME f_dlg_03_usuar_mestre_copia.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

    CASE p-state:
        /* Object instance CASEs can go here to replace standard behavior
           or add new cases. */
        {src/adm/template/vstates.i}
    END CASE.
    RUN pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validarAlteracaoSenha V-table-Win 
FUNCTION validarAlteracaoSenha RETURNS LOGICAL
    ( INPUT p-log-enable AS LOGICAL /* habilita ou nao conforme hierarquia */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  somente podera alterar senha se nao existir senha externa ativa
        Notes:  jaison antoniazzi - 20/10/2008, conforme FO 1.876.717
    ------------------------------------------------------------------------------*/
    
    &SCOPED-DEFINE CT_USUARIO_ACESSO_EXTERNO "Externo"
    &SCOPED-DEFINE CT_TIPO_USUARIO_COMUM "Comum"

    DEFINE VARIABLE tipoacesso        AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lUsuarioNaoHumano AS LOGICAL   NO-UNDO INITIAL FALSE.
    DEFINE VARIABLE lResult           AS LOGICAL   NO-UNDO INITIAL FALSE.
    /*
    verificar se o usuario selecionado no programa tem login externo
    jaison antoniazzi
    */
    FIND FIRST busuar_mestre 
        WHERE busuar_mestre.cod_usuario = c-cod-usuario 
        NO-LOCK NO-ERROR.
    IF  AVAILABLE busuar_mestre THEN
        tipoacesso = busuar_mestre.ind_tip_aces_usuar.

    RUN isNotHumanUser IN h-bofn017 (INPUT  tipoacesso,
                                     OUTPUT lUsuarioNaoHumano).
    
    ASSIGN lResult = FALSE.

    IF  v_ind_tip_usuar_cor        <> "Super":U                  
    OR  ENCODE(v_cod_usuar_corren) <> v_cod_usuar_corren_criptog 
    OR  tipoacesso                 <> "Interno":U                
    OR  NOT CAN-FIND( FIRST usuar_mestre NO-LOCK
                      WHERE usuar_mestre.cod_usuario   = v_cod_usuar_corren 
                        AND usuar_mestre.ind_tip_usuar = v_ind_tip_usuar_cor )THEN DO: 
    IF  p-log-enable
        AND tipoacesso <> "Externo":U THEN DO:
            ASSIGN lResult = TRUE.
        END.
    END.
    ELSE DO:
         IF  ENCODE(v_cod_usuar_corren) = v_cod_usuar_corren_criptog 
         AND CAN-FIND( FIRST usuar_mestre NO-LOCK
                       WHERE usuar_mestre.cod_usuario   = v_cod_usuar_corren 
                         AND usuar_mestre.ind_tip_usuar = v_ind_tip_usuar_cor ) THEN DO:
            ASSIGN lResult = TRUE.
         END.
    END.

    &IF "{&product_version}" >= "12.1.M" &THEN   
    FIND FIRST usuar_mestre_aux WHERE usuar_mestre_aux.cod_usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
    IF AVAILABLE usuar_mestre_aux THEN DO:
        IF usuar_mestre_aux.log_inativ = TRUE THEN
            ASSIGN lResult = FALSE.
    END.
    &ENDIF

    RETURN lResult.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

