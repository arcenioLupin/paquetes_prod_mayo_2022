create or replace PACKAGE       venta.PKG_SWEB_CRED_SOLI_REPORTES AS

/*-----------------------------------------------------------------------------
    Nombre : SP_LIST_CRED_SOLI_VC_COD_OPERS
    Proposito : Lista de codigos de operaciones por cliente.
    Referencias : PKG_SWEB_CRED_SOLI_REPO_VC.SP_LIST_CRED_SOLI_VC_COD_OPERS
    Parametros : p_cod_clie
    Log de Cambios
    Fecha        Autor          Descripcion
    27/02/2020   jquintanilla   REQ CU-18 Creación
  ----------------------------------------------------------------------------*/    
PROCEDURE SP_LIST_CRED_SOLI_VC_COD_OPERS(
    p_cod_clie          IN vve_cred_soli.cod_clie%type,
    p_ret_cursor        OUT SYS_REFCURSOR,
    p_ret_cantidad      OUT NUMBER,
    p_ret_esta          OUT NUMBER,
    p_ret_mens          OUT VARCHAR2
);

/*-----------------------------------------------------------------------------
    Nombre : SP_LIST_CRED_SOLI_VC_OPERS
    Proposito : Lista de informacion de todas las operaciones por cliente.
    Referencias : 
    Parametros : p_cod_clie, 
               p_cod_oper 
    Log de Cambios
    Fecha        Autor          Descripcion
    18/02/2020   jquintanilla    REQ CU-19     Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE SP_LIST_CRED_SOLI_VC_OPERS(
    p_cod_clie          IN VARCHAR2,
    p_cod_oper          IN VARCHAR2,
    p_ret_cursor        OUT SYS_REFCURSOR,
    p_ret_cantidad      OUT NUMBER,
    p_ret_esta          OUT NUMBER,
    p_ret_mens          OUT VARCHAR2
);

/*-----------------------------------------------------------------------------
    Nombre : SP_lIST_AMORTIZACION_X_OPERA
    Proposito : Lista las amortizaciones por la lista de operaciones realizadas por el cliente.
    Referencias : 
    Parametros : p_cod_oper 
    Log de Cambios
    Fecha        Autor          Descripcion
    12/03/2020   ebarboza    REQ CU-19     Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE SP_lIST_AMORTIZACION_X_OPERA(
    p_cod_sociedad   VARCHAR2,
    p_cod_ref1              arlcml.cod_oper%TYPE,
    p_num_refer             arlcml.cod_oper%TYPE,
    p_ret_cursor        OUT SYS_REFCURSOR,
    p_ret_esta          OUT NUMBER,
    p_ret_mens          OUT VARCHAR2
);

/*-----------------------------------------------------------------------------
    Nombre : SP_LIST_CRED_SOLI_VC_GARAN
    Proposito : Lista informacion de las Solicitudes Credito, Reporte VistaCliente.
    Referencias : 
    Parametros : p_cod_clie, 
               p_cod_oper 
    Log de Cambios
    Fecha        Autor          Descripcion
    18/02/2020   jquintanilla    REQ CU-19     Creacion
  ----------------------------------------------------------------------------*/    
PROCEDURE SP_LIST_CRED_SOLI_VC_GARAN(
    p_cod_clie          IN vve_cred_soli.cod_clie%type,
    p_cod_oper          IN vve_cred_soli.cod_oper_rel%type,
    p_ret_cursor        OUT SYS_REFCURSOR,
    p_ret_cantidad      OUT NUMBER,
    p_ret_esta          OUT NUMBER,
    p_ret_mens          OUT VARCHAR2
);

/*-----------------------------------------------------------------------------
    Nombre : SP_LIST_CRED_SOLI_CO
    Proposito : Lista de informacion de los creditos otorgados por cliente.
    Referencias : 
    Parametros : p_cod_region, 
                 p_cod_area_vta,
                 p_cod_tipo_oper,
                 p_fec_factu_inicio,
                 p_fec_factu_fin
    Log de Cambios
    Fecha        Autor          Descripcion
    18/02/2020   jquintanilla    REQ CU-19     Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE SP_LIST_CRED_SOLI_CO(
    p_cod_region        IN vve_mae_zona.cod_zona%type,
    p_cod_area_vta      IN gen_area_vta.cod_area_vta%type,
    p_cod_tipo_oper     IN vve_cred_soli.tip_soli_cred%type,
    p_fec_factu_inicio  IN VARCHAR2,
    p_fec_factu_fin     IN VARCHAR2,
    p_op_aprobados      IN VARCHAR2,
    p_cliente           IN VARCHAR2,-- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>
    p_ruc_cliente       IN VARCHAR2, -- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>
    p_fec_ope_inicio    IN VARCHAR2,-- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>
    p_fec_ope_fin       IN VARCHAR2, -- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>    
    p_ret_cursor        OUT SYS_REFCURSOR,
    p_ret_cantidad      OUT NUMBER,
    p_ret_esta          OUT NUMBER,
    p_ret_mens          OUT VARCHAR2
);

/*-----------------------------------------------------------------------------
    Nombre : fn_obtiene_usu_estado
    Proposito : Devuelve el usuario que genero el estado ingresado en la solicitud.
    Referencias : 
    Parametros : p_cod_soli_cred, 
                 p_cod_estado
    Log de Cambios
    Fecha        Autor          Descripcion
    01/07/2021   avilca         Creacion
  ----------------------------------------------------------------------------*/
   FUNCTION fn_usu_estado (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_cod_estado         IN vve_cred_soli.cod_estado%TYPE
    ) RETURN VARCHAR2;

/*-----------------------------------------------------------------------------
    Nombre : fn_obtiene_fec_estado
    Proposito : Devuelve la fecha en que se genero el estado ingresado en la solicitud.
    Referencias : 
    Parametros : p_cod_soli_cred, 
                 p_cod_estado
    Log de Cambios
    Fecha        Autor          Descripcion
    01/07/2021   avilca         Creacion
  ----------------------------------------------------------------------------*/
    FUNCTION fn_fec_estado (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_cod_estado         IN vve_cred_soli.cod_estado%TYPE
    )RETURN VARCHAR2 ;   

/*-----------------------------------------------------------------------------
    Nombre : fn_usu_aprob
    Proposito : Devuelve el usuario que aprobó la solicitud en un nivel n.
    Referencias : 
    Parametros : p_cod_soli_cred, 
                 p_ind_nivel
    Log de Cambios
    Fecha        Autor          Descripcion
    08/07/2021   avilca         Creacion
  ----------------------------------------------------------------------------*/
    FUNCTION fn_usu_aprob (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_ind_nivel         IN vve_cred_soli_apro.ind_nivel%TYPE
    ) RETURN VARCHAR2; 

/*-----------------------------------------------------------------------------
    Nombre : fn_fec_aprob
    Proposito : Devuelve la fecha en que se aprobó la solicitud en el nivel n.
    Referencias : 
    Parametros : p_cod_soli_cred, 
                 p_ind_nivel
    Log de Cambios
    Fecha        Autor          Descripcion
    01/07/2021   avilca         Creacion
  ----------------------------------------------------------------------------*/
    FUNCTION fn_fec_aprob (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_ind_nivel         IN vve_cred_soli_apro.ind_nivel%TYPE
    )RETURN VARCHAR2;  	
    
    
/*-----------------------------------------------------------------------------
    Nombre : fn_usu_aprob_soli
    Proposito : Devuelve usuario que solicitó aprobación de la solicitud en el mínimo nivel.
    Referencias : 
    Parametros : p_cod_soli_cre
    Log de Cambios
    Fecha        Autor          Descripcion
    13/10/2021   avilca         Creacion
  ----------------------------------------------------------------------------*/    
    FUNCTION fn_usu_aprob_soli (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE
    ) RETURN VARCHAR2;
    
/*-----------------------------------------------------------------------------
    Nombre : fn_fec_aprob_soli
    Proposito : Devuelve la fecha en que se aprobó la solicitud en el mínimo nivel.
    Referencias : 
    Parametros : p_cod_soli_cred 
    Log de Cambios
    Fecha        Autor          Descripcion
    13/10/2021   avilca         Creacion
  ----------------------------------------------------------------------------*/    
    FUNCTION fn_fec_aprob_soli (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE
    )RETURN VARCHAR2;     

/*-----------------------------------------------------------------------------
    Nombre : fn_ratio_cobertura
    Proposito : Devuelve el ratio de cobertura del presente año
    Referencias : PKG_SWEB_CRED_SOLI_GARANTIA.sp_list_cobergara_fc
    Parametros : p_cod_soli_cred
    Log de Cambios
    Fecha        Autor          Descripcion
    24/02/2020   jquintanilla    REQ CU-19     Creacion
  ----------------------------------------------------------------------------*/
FUNCTION fn_ratio_cobertura(
    p_cod_soli_cred     IN VARCHAR2
) RETURN NUMBER;

/*-----------------------------------------------------------------------------
    Nombre : SP_LIST_CRED_SOLI_VO
    Proposito : Lista informacion de las Solicitudes Credito, Reporte VistaOperacion.
    Referencias : PKG_SWEB_CRED_SOLI_REPO_VO.SP_LIST_CRED_SOLI_VO
    Parametros :
    Log de Cambios
    Fecha        Autor          Descripcion
    06/03/2019   jaltamirano    req-87567     Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE SP_LIST_CRED_SOLI_VO
  (
    p_cod_cred_soli IN vve_cred_soli.cod_soli_cred%type,
    p_cod_oper_rel  IN vve_cred_soli.cod_oper_rel%type,
    p_cod_usua_sid  IN sistemas.usuarios.co_usuario%TYPE,
    p_cod_usua_web  IN sistemas.sis_mae_usuario.cod_id_usuario%TYPE,     
    p_ret_cursor    OUT SYS_REFCURSOR,
    p_ret_cantidad  OUT NUMBER,
    p_ret_esta      OUT NUMBER,
    p_ret_mens      OUT VARCHAR2
  );

  
END PKG_SWEB_CRED_SOLI_REPORTES;
