/*------------------------------------------------------------------------
    File        : ESSEC008.p
    Purpose     : Programa para manutená∆o de usu†rios
    Description : Manutená∆o de usu†rios (SEC000AA)
    Author(s)   : Cleberson Silva - 4Make
    Created     : 13/09/2020
    Notes       :
 ------------------------------------------------------------------------*/
 {include/i-prgvrs.i ESSEC008 2.00.00.001}
  
{METHOD/dbotterr.i}

/* definicao de temp-tables */
DEFINE TEMP-TABLE ttUsuarios NO-UNDO
   FIELD cod-usuario          AS CHAR FORMAT "x(12)"
   FIELD nome-usuario         AS CHAR FORMAT "x(50)"
   FIELD cod-senha            AS CHAR FORMAT "x(16)"
   FIELD ind-tip-usuar        AS CHAR FORMAT "x(13)"
   FIELD ind-tip-acesso       AS CHAR FORMAT "x(13)"
   FIELD dat-inic-valid       AS DATE FORMAT "99/99/9999"
   FIELD dat-fim-valid        AS DATE FORMAT "99/99/9999"
   FIELD dat-valid-senha      AS DATE FORMAT "99/99/9999"
   FIELD num-dias-valid-senha AS INTEGER.


DEFINE TEMP-TABLE tt-usuar_mestre NO-UNDO LIKE usuar_mestre
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE ttGruposUsuarios NO-UNDO
    FIELD cod-usuario    AS CHAR
    FIELD cod-grupo      AS CHAR
    FIELD acao-registro  AS CHAR. /*criar, remover*/


DEFINE TEMP-TABLE tt-erros NO-UNDO
    FIELD cod-erro  AS INTE
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.




/* definicao de variaveis*/
DEFINE VARIABLE h-bofn017 AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-id      AS INTEGER     NO-UNDO.


DEFINE DATASET fndUsuarios FOR ttUsuarios.
DEFINE DATASET fndGrupos   FOR ttGruposUsuarios.

/* Recebimento de parametros */
DEFINE INPUT PARAMETER DATASET FOR fndUsuarios.
DEFINE INPUT PARAMETER DATASET FOR fndGrupos.

DEFINE OUTPUT PARAMETER retorno AS CHARACTER NO-UNDO.


/* definicao de buffer */
DEFINE BUFFER b-usuar_mestre FOR usuar_mestre.


/* limpa tabelas */

EMPTY TEMP-TABLE ttUsuarios.
EMPTY TEMP-TABLE tt-usuar_mestre.

FOR FIRST ttUsuarios NO-LOCK:

    /* descomentar este bloco depois de criar a tela de parametros
    FIND FIRST param-integr NO-LOCK NO-ERROR.
    IF NOT AVAIL param-integr THEN
    DO:
        ASSIGN retorno = "Usu†rio de integraá∆o n∆o parametrizado no programa ESSEC008A".
        LEAVE.
    END.
    */


    RUN btb/btapi910ze.p   (INPUT "han1alv",   /*USUARIO*/
                            INPUT "",          /*SENHA*/
                            INPUT "1",         /*EMPRESA*/
                            OUTPUT TABLE tt-erros). /*RETORNO DE ERROSl*/


    IF CAN-FIND(FIRST tt-erros NO-LOCK) THEN
    DO:
        FOR EACH tt-erros.

            IF retorno = "" THEN
                ASSIGN retorno = STRING(tt-erros.cod-erro) + " - " + tt-erros.desc-erro.
            ELSE 
                ASSIGN retorno = retorno + CHR(13) + STRING(tt-erros.cod-erro) + " - " + tt-erros.desc-erro.
        END.
        
    END.

    
    
    /*recupera o id do ultimo usuario criado*/
    FIND LAST usuar_mestre NO-LOCK NO-ERROR.
    IF AVAIL usuar_mestre THEN
        ASSIGN i-id = usuar_mestre.idi_dtsul + 1.

    
    FIND FIRST usuar_mestre NO-LOCK
         WHERE usuar_mestre.cod_usuario = ttUsuarios.cod-usuario NO-ERROR.
    IF NOT AVAIL usuar_mestre THEN
    DO:
        CREATE tt-usuar_mestre.
        ASSIGN tt-usuar_mestre.cod_usuario          = ttUsuarios.cod-usuario
               tt-usuar_mestre.nom_usuario          = ttUsuarios.nome-usuario
               tt-usuar_mestre.ind_tip_usuar        = "1" /*--comun--*/
               tt-usuar_mestre.ind_tip_aces_usuar   = "1" /*--interno--*/
               tt-usuar_mestre.dat_inic_valid       = TODAY
               tt-usuar_mestre.dat_fim_valid        = TODAY + 30
               tt-usuar_mestre.dat_valid_senha      = TODAY
               tt-usuar_mestre.num_dias_valid_senha = 90
               tt-usuar_mestre.idi_dtsul            = i-id.


        IF NOT VALID-HANDLE(h-bofn017) THEN
            RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.

        RUN emptyRowErrors IN h-bofn017.
        RUN openQueryStatic IN h-bofn017 (INPUT "Main":U) NO-ERROR.
        RUN setRecord IN h-bofn017(INPUT TABLE tt-usuar_mestre).
        RUN createRecord IN h-bofn017.
        RUN getRowErrors IN h-bofn017(OUTPUT TABLE RowErrors).

        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
        DO:
            FOR EACH RowErrors NO-LOCK:
                ASSIGN retorno = RowErrors.errorDescription.
            END.
            RETURN.
        END.
        
        /*-- vincula usuario a empresa*/
        IF NOT CAN-FIND(FIRST segur_empres_usuar WHERE segur_empres_usuar.cod_usuario = ttUsuarios.cod-usuario)
        THEN DO:
            CREATE segur_empres_usuar.
            ASSIGN segur_empres_usuar.cod_usuario = ttUsuarios.cod-usuario
                   segur_empres_usuar.cod_empresa = "001".

            RELEASE segur_empres_usuar.
        END.

        /*-- vincula os grupos dos usuarios --*/
        RUN pi-vincula-grupos (INPUT ttUsuarios.cod-usuario).

        IF VALID-HANDLE(h-bofn017) THEN
            DELETE PROCEDURE h-bofn017.

        ASSIGN retorno = SUBSTITUTE("Usuario &1 registrado com sucesso.", ttUsuarios.cod-usuario).
    END.
    DO:

        CREATE tt-usuar_mestre. 
        BUFFER-COPY usuar_mestre EXCEPT nom_usuario TO tt-usuar_mestre.
        ASSIGN tt-usuar_mestre.nom_usuario = ttUsuarios.nome-usuario.


        IF NOT VALID-HANDLE(h-bofn017) THEN                                       
            RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.                          
                                                                                  
        RUN emptyRowErrors IN h-bofn017.                                          
        RUN openQueryStatic IN h-bofn017 (INPUT "Main":U) NO-ERROR.               
        RUN setRecord IN h-bofn017(INPUT TABLE tt-usuar_mestre).                  
        RUN createRecord IN h-bofn017.                                            
        RUN getRowErrors IN h-bofn017(OUTPUT TABLE RowErrors).                    
                                                                                  
        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN                                 
        DO:                                                                       
            FOR EACH RowErrors NO-LOCK:                                           
                ASSIGN retorno = RowErrors.errorDescription.                      
            END.                                                                  
            RETURN.                                                               
        END.    

        IF VALID-HANDLE(h-bofn017) THEN    
            DELETE PROCEDURE h-bofn017. 

        /*-- vincula os grupos dos usuarios --*/                 
        RUN pi-vincula-grupos (INPUT ttUsuarios.cod-usuario).  

        ASSIGN retorno = SUBSTITUTE("Usuario &1 alterado com sucesso.", ttUsuarios.cod-usuario).

    END.
END.



PROCEDURE pi-vincula-grupos:
    DEFINE INPUT PARAMETER p-usuario AS CHAR NO-UNDO.

    DEFINE BUFFER b_usuar_grp_usuar FOR usuar_grp_usuar.

    /*-- vincula os grupos dos usuarios --*/                                                               
    FOR EACH ttGruposUsuarios NO-LOCK WHERE ttGruposUsuarios.cod-usuario = p-usuario: 
                                                                                                           
        IF NOT CAN-FIND(FIRST usuar_grp_usuar NO-LOCK                                                              
                        WHERE usuar_grp_usuar.cod_grp_usuar = ttGruposUsuarios.cod-grupo                        
                          AND usuar_grp_usuar.cod_usuario   = ttGruposUsuarios.cod-usuario) THEN                     
        DO:                                                                                                
             CREATE usuar_grp_usuar.                                                                       
             ASSIGN usuar_grp_usuar.cod_grp_usuar =  ttGruposUsuarios.cod-grupo                             
                    usuar_grp_usuar.cod_usuario   =  ttGruposUsuarios.cod-usuario.                               
                                                                                                           
             RELEASE usuar_grp_usuar.                                                                      
        END. 
        ELSE
        DO:
            IF ttGruposUsuarios.acao-registro = "remover" THEN
            DO:
                FIND FIRST b_usuar_grp_usuar EXCLUSIVE-LOCK                                          
                     WHERE b_usuar_grp_usuar.cod_grp_usuar = ttGruposUsuarios.cod-grupo    
                       AND b_usuar_grp_usuar.cod_usuario   = ttGruposUsuarios.cod-usuario NO-ERROR.
                IF AVAIL b_usuar_grp_usuar THEN
                    DELETE b_usuar_grp_usuar.

                RELEASE b_usuar_grp_usuar.
            END.
        END.
    END.

END PROCEDURE.
                                                                                              
