 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/

DEFINE TEMP-TABLE ttUser NO-UNDO
    FIELD accountId  AS CHARACTER XML-NODE-TYPE "HIDDEN"
    INDEX idxUser IS PRIMARY accountId.


DEFINE TEMP-TABLE ttErrorMessage NO-UNDO
    SERIALIZE-NAME 'faultcode'
    FIELD idmFault          AS CHARACTER ///SERIALIZE-NAME "xmlns:urn" XML-NODE-TYPE "ATTRIBUTE"
    FIELD errorCode         AS CHARACTER XML-NODE-NAME "code"
    FIELD ErrorDescription  AS CHARACTER XML-NODE-NAME "detail".



DEFINE TEMP-TABLE ttRetorno NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    FIELD logUser AS LOGICAL XML-NODE-NAME "checkCreateAccountResponse".
