/*------------------------------------------------------------------------
    File        : checkDeleteAccount.p
    Description : Programa para checar as informa‡äes do usuario
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
 
 /* ***************************  Definitions  ************************** */
 
{include/i-prgvrs.i checkDeleteAccount 2.00.00.001}
{METHOD/dbotterr.i}

{include/ttGetUser.i}
{include/ttResponse.i "'checkDeleteAccountResponse'"}

/* global variable definitions */

/* local variable definitions */
DEFINE VARIABLE lRetOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lRetorno AS LOGICAL INITIAL FALSE NO-UNDO.


/*-- dataset definitions --*/
DEFINE DATASET dsDeleteUser 
    NAMESPACE-URI "webservice.idm.bosch.com:types" 
    XML-NODE-NAME "checkDeleteAccount"
    FOR ttUser.

DEFINE DATASET dsRetorno
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "checkDeleteAccountResponse"
    XML-NODE-TYPE "HIDDEN"
    SERIALIZE-HIDDEN
    FOR ttRetorno.                                


/* ***************************  Main Block  ************************** */
DEFINE INPUT  PARAMETER DATASET FOR dsDeleteUser.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetorno.



RUN piValidUserAccount(INPUT TABLE ttUser).

LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE(">>> VALOR DE RETURN-VALUE &1",RETURN-VALUE)) NO-ERROR.


CREATE ttRetorno.
ASSIGN ttRetorno.logUser = IF RETURN-VALUE = "OK":U THEN TRUE ELSE FALSE.



PROCEDURE piValidUserAccount:
/*---------------------------------------------------------------
 Purpose: Checks if the user sent exists in the database
---------------------------------------------------------------*/
    DEFINE INPUT  PARAM TABLE FOR  ttUser.


    FIND FIRST ttUser NO-LOCK NO-ERROR.
    IF NOT AVAIL ttUser THEN
    DO:
        RETURN "NOK":U.
    END.
    ELSE
    DO:
        /*******************************************
          As validacoes foram separadas, por que 
          o AppServer, estava dando falso positivo
          de forma intermitente - CPAS
        ********************************************/

        IF ttUser.accountId = "?"  THEN
            RETURN "NOK":U.


        IF ttUser.accountId = ""  THEN
            RETURN "NOK":U.


        IF NOT CAN-FIND(FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = ttUser.accountId NO-LOCK) THEN
            RETURN "NOK":U.


        RETURN "OK":U.
    END.

END PROCEDURE.







  
