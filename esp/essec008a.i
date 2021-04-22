

DEFINE TEMP-TABLE {1} NO-UNDO SERIALIZE-NAME {2}
    FIELD accountId  AS CHAR
    FIELD firstName  AS CHAR
    FIELD lastName   AS CHAR
    FIELD email      AS CHAR
    FIELD department AS CHAR
    INDEX idxUser IS PRIMARY accountId.

