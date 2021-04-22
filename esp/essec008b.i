
 DEFINE TEMP-TABLE {1} NO-UNDO SERIALIZE-NAME {2}
     FIELD accountId AS CHAR XML-NODE-TYPE "hidden"
     FIELD groupCode AS CHAR XML-NODE-NAME "name"
     INDEX idxGroupCode IS PRIMARY accountId groupCode.
