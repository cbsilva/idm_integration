/*------------------------------------------------------------------------
    File        : checkCreateAccount.p
    Purpose     : Programa verifica se a conta do usuario pode ser 
                  criada
    Description : Manuten‡Æo de usu rios (SEC000AA)
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
{METHOD/dbotterr.i}
{include/i-prgvrs.i checkCreateAccount 2.00.00.001}

/*-- temp-tables definitions --*/
{esp/essec008a.i ttUser "'userData'"}
{esp/essec008b.i ttGroups "'Roles'"}

DEFINE TEMP-TABLE ttRetorno NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    FIELD logUser AS LOGICAL XML-NODE-NAME "checkCreateAccountResponse"
    //XML-NODE-TYPE "TEXT"
    .



/*-- dataset definitions --*/
DEFINE DATASET dsUser 
    NAMESPACE-URI "webservice.idm.bosch.com:types" 
    XML-NODE-NAME "checkCreateAccount"
    XML-NODE-TYPE "HIDDEN"
    FOR ttUser.

DEFINE DATASET dsRetorno
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "checkCreateAccountResponse"
    SERIALIZE-HIDDEN
    FOR ttRetorno.                                


/*-- variable definitions --*/
DEFINE VARIABLE lRetorno AS LOGICAL INITIAL FALSE NO-UNDO.


DEFINE INPUT  PARAMETER DATASET FOR dsUser.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetorno.


FIND FIRST ttUser NO-LOCK NO-ERROR.
IF NOT AVAIL ttUSer THEN
DO:
    CREATE ttRetorno.
    ASSIGN ttRetorno.logUser = FALSE.
    //ASSIGN pResponse = FALSE.
    RETURN "NOK":U.
END.
ELSE
DO:
    RUN piValidUserAccount(INPUT ttUser.accountId, OUTPUT lRetorno).
    CREATE ttRetorno.
    ASSIGN ttRetorno.logUser = lRetorno.
END.

/*-- procedures --*/  
{esp/essec008.i} 







  
