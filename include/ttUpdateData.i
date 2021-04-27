 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/
DEFINE TEMP-TABLE ttUserUpdate NO-UNDO
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "UserData"
    FIELD accountId  AS CHARACTER FORMAT "x(12)"
    INDEX idxUser IS PRIMARY accountId.

