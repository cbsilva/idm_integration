{METHOD/dbotterr.i}
{fnbo/bofn017.i ttUsuarioMestre}

DEFINE VARIABLE h-bofn017          AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-idi-dtsul-instan AS INTEGER     NO-UNDO.


IF NOT VALID-HANDLE(h-bofn017) THEN
    RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.

EMPTY TEMP-TABLE ttUsuarioMestre.

RUN newRecord   IN h-bofn017.
RUN getRecord   IN h-bofn017(OUTPUT TABLE ttUsuarioMestre).
RUN openQueryStatic IN h-bofn017 (INPUT "Main":U) NO-ERROR.

FIND LAST usuar_mestre NO-LOCK NO-ERROR.
ASSIGN i-idi-dtsul-instan = IF AVAIL usuar_mestre THEN usuar_mestre.idi_dtsul + 1 ELSE 1.


FIND FIRST ttUsuarioMestre NO-ERROR.
IF AVAIL ttUsuarioMestre THEN
DO:
       
    ASSIGN ttUsuarioMestre.idi_dtsul             = i-idi-dtsul-instan
           ttUsuarioMestre.cod_usuario           = "testeqg3"
           ttUsuarioMestre.nom_usuario           = "teste via qg"
           ttUsuarioMestre.cod_senha             = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM("teste@123"))))                   
           ttUsuarioMestre.cod_senha_framework   = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM("teste@123"))))                   
           ttUsuarioMestre.ind_tip_usuar         = "comum"                                                                      
           ttUsuarioMestre.ind_tip_aces_usuar    = "interno"                                                                    
           ttUsuarioMestre.dat_inic_valid        = TODAY                  
           ttUsuarioMestre.dat_fim_valid         = TODAY + 30             
           ttUsuarioMestre.dat_valid_senha       = TODAY - 1              
           ttUsuarioMestre.num_dias_valid_senha  = 90                     
           ttUsuarioMestre.nom_dir_spool         = "c:\temp"
           ttUsuarioMestre.nom_subdir_spool      = ""
           ttUsuarioMestre.cod_servid_exec       = ""
           ttUsuarioMestre.nom_subdir_spool_rpw  = ""
           ttUsuarioMestre.cod_e_mail_local      = ""
           ttUsuarioMestre.cod_dialet            = "PT".


    //RUN setRecord    IN h-bofn017(INPUT TABLE ttUsuarioMestre).
    //RUN createRecord IN h-bofn017.


    RUN validateCreate IN h-bofn017(INPUT TABLE ttUsuarioMestre, OUTPUT TABLE RowErrors, OUTPUT ttUsuarioMestre.r-rowid).


    //RUN getRowErrors IN h-bofn017(OUTPUT TABLE RowErrors).

    FOR EACH RowErrors.
        DISP RowErrors.ErrorNumber 
             RowErrors.ErrorDescription FORMAT "x(70)" WITH WIDTH 333 1 COL.
    END.


    RUN createUsuarMestreExt IN h-bofn017.



END.


IF VALID-HANDLE(h-bofn017) THEN DELETE PROCEDURE h-bofn017.
