/*------------------------------------------------------------------------
    File        : createAccount.p
    Description : Programa para checar as informaá‰es do usuario
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
 
 /* ***************************  Definitions  ************************** */
 
{include/i-prgvrs.i createAccount 2.00.00.001}
{METHOD/dbotterr.i}

//{include/ttGetUser.i}
{include/ttUserData.i}
{include/ttGenerics.i}
{include/ttResponse.i "'createAccountResponse'"}

/* global variable definitions */

/* local variable definitions */
DEFINE VARIABLE h-bofn017   AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-id        AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod-grupo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mensagem  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-position  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-length    AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-inativo   AS CHARACTER   NO-UNDO.


/* buffer definitions */
DEFINE BUFFER b-usuar_mestre FOR usuar_mestre.
DEFINE BUFFER b_usuar_grp_usuar FOR usuar_grp_usuar.

/*-- dataset definitions --*/
DEFINE DATASET dsCreateUser 
    NAMESPACE-URI "webservice.idm.bosch.com:types" 
    XML-NODE-NAME "createAccount"
    FOR ttUserData.

DEFINE DATASET dsRetorno
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "createAccountResponse"
    XML-NODE-TYPE "HIDDEN"
    SERIALIZE-HIDDEN
    FOR ttRetorno.                                


/* ***************************  Main Block  ************************** */
DEFINE INPUT  PARAMETER DATASET FOR dsCreateUser.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetorno.


RUN piCreateUser(INPUT TABLE ttUserData).


CREATE ttRetorno.
ASSIGN ttRetorno.logUser = IF RETURN-VALUE = "OK":U THEN TRUE ELSE FALSE.


PROCEDURE piCreateUser:
/*------------------------------------------------------------------
 Purpose: Create user in sec000aa.w
--------------------------------------------------------------------*/
    DEFINE INPUT PARAM TABLE FOR ttUserData.

    LOG-MANAGER:WRITE-MESSAGE(">>> CRIANDO USUARIO <<<") NO-ERROR.

    FIND FIRST ttUserData NO-LOCK NO-ERROR.


    //limpa temp-table
    EMPTY TEMP-TABLE tt-usuar_mestre.
    EMPTY TEMP-TABLE ttRetorno.

    /* Checks if the user parameter exists */
    RUN piLogin.
    IF RETURN-VALUE = "NOK":U OR ttUserData.accountId = "?" THEN 
        RETURN "NOK":U.
    
    
    
    /*recupera o id do ultimo usuario criado*/
    FIND LAST b-usuar_mestre NO-LOCK NO-ERROR.
    IF AVAIL b-usuar_mestre THEN ASSIGN i-id = b-usuar_mestre.idi_dtsul + 1.
    

    IF NOT CAN-FIND(FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = ttUserData.accountId NO-LOCK) THEN
    DO:
        createUser: 
        DO TRANS ON ERROR UNDO, LEAVE:
            DO ON QUIT UNDO createUser, LEAVE:
                DO ON ERROR UNDO, LEAVE:
                
                    CREATE tt-usuar_mestre.
                    ASSIGN tt-usuar_mestre.cod_usuario          = ttUserData.accountId
                           tt-usuar_mestre.nom_usuario          = ttUserData.firstName + " " + ttUserData.lastname
                           tt-usuar_mestre.cod_senha            = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM(ttUserData.accountId))))
                           tt-usuar_mestre.cod_senha_framework  = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM(ttUserData.accountId))))
                           tt-usuar_mestre.ind_tip_usuar        = "comum" 
                           tt-usuar_mestre.ind_tip_aces_usuar   = "interno"
                           tt-usuar_mestre.dat_inic_valid       = TODAY
                           tt-usuar_mestre.dat_fim_valid        = TODAY + 30
                           tt-usuar_mestre.dat_valid_senha      = TODAY - 1
                           tt-usuar_mestre.num_dias_valid_senha = 90
                           tt-usuar_mestre.idi_dtsul            = i-id
                           tt-usuar_mestre.nom_dir_spool        = es-param-integ.nom-dir-spool
                           tt-usuar_mestre.nom_subdir_spool     = ttUserData.accountId
                           tt-usuar_mestre.cod_servid_exec      = es-param-integ.cod-servid-exec
                           tt-usuar_mestre.nom_subdir_spool_rpw = ttUserData.accountId
                           tt-usuar_mestre.cod_e_mail_local     = ttUserData.email
                           tt-usuar_mestre.cod_dialet           = es-param-integ.cod-dialet.
                    
                    
                    IF NOT VALID-HANDLE(h-bofn017) THEN
                        RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.

                    RUN emptyRowErrors  IN h-bofn017.
                    RUN openQueryStatic IN h-bofn017 (INPUT "Main":U) NO-ERROR.
                    RUN setRecord       IN h-bofn017(INPUT TABLE tt-usuar_mestre).
                    RUN createRecord    IN h-bofn017.
                    RUN getRowErrors    IN h-bofn017(OUTPUT TABLE RowErrors).

                    IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
                    DO:
                        FOR EACH RowErrors NO-LOCK:
                            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("ErrorCode &1 ErrorMessage &2", RowErrors.ErrorNumber, RowErrors.errorDescription)).
                            ASSIGN c-mensagem = c-mensagem + SUBSTITUTE("ErrorCode &1 ErrorMessage &2", RowErrors.ErrorNumber, RowErrors.errorDescription).
                        END.
                        UNDO, LEAVE.
                    END.

                    /*-- vincula usuario a empresa*/
                    IF NOT CAN-FIND(FIRST segur_empres_usuar WHERE segur_empres_usuar.cod_usuario = ttUserData.accountId)
                    THEN DO:
                        CREATE segur_empres_usuar.
                        ASSIGN segur_empres_usuar.cod_usuario = ttUserData.accountId
                               segur_empres_usuar.cod_empresa = es-param-integ.ep-codigo.

                        RELEASE segur_empres_usuar.
                    END.
                END.
            END.
        END.

        IF c-mensagem <> "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("##### ERRO AO CRIAR USUARIO")) NO-ERROR.
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("##### &1",c-mensagem )) NO-ERROR.
            RETURN "NOK":U.
                
        END.

        RETURN "OK":U.
    END.

END PROCEDURE.





PROCEDURE piLogin:
/*------------------------------------------------------------------
 Purpose: Checks if the user parameter exists. 
 The user is necessary to manipulare the information inside de ERP
-------------------------------------------------------------------*/

    FIND FIRST es-param-integ NO-LOCK NO-ERROR.
    IF NOT AVAIL es-param-integ THEN
    DO:
        LOG-MANAGER:WRITE-MESSAGE("Usu†rio de integraá∆o n∆o parametrizado no programa ESSEC008B").
        RETURN "NOK":U.
    END.

    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("Instancia Datasul &1",es-param-integ.idi-dtsul-inst)) NO-ERROR.

    // valida o usuario de integraao
    RUN btb/btapi910ze.p   (INPUT es-param-integ.cod-usuario,        /*USUARIO*/
                            INPUT "",                                /*SENHA*/  
                            INPUT es-param-integ.idi-dtsul-inst,     /*DATASUL INSTANCIA*/  
                            OUTPUT TABLE tt-erros).             

    IF CAN-FIND(FIRST tt-erros NO-LOCK) THEN
    DO:
        FOR EACH tt-erros NO-LOCK:

            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("ErrorCode &1 ErrorMessage &2",tt-erros.cod-erro, tt-erros.desc-erro)).
            
        END.
        RETURN "NOK":U.        
    END.

    RETURN "OK":U.

END PROCEDURE.







  
