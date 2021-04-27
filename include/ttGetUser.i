 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/

DEFINE TEMP-TABLE ttUser NO-UNDO
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    FIELD accountId  AS CHARACTER XML-NODE-TYPE "HIDDEN"
    INDEX idxUser IS PRIMARY accountId.





