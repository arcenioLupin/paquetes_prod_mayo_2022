create or replace PACKAGE BODY       ventas.pkg_sweb_cred_soli_reportes AS

    PROCEDURE sp_list_cred_soli_vc_cod_opers (
        p_cod_clie       IN vve_cred_soli.cod_clie%TYPE,
        p_ret_cursor     OUT SYS_REFCURSOR,
        p_ret_cantidad   OUT NUMBER,
        p_ret_esta       OUT NUMBER,
        p_ret_mens       OUT VARCHAR2
    ) AS
    BEGIN
        OPEN p_ret_cursor FOR SELECT DISTINCT
                                  sc.cod_oper_rel   AS cod_oper
                              FROM
                                  vve_cred_soli sc
                                  INNER JOIN arlcop ac ON sc.cod_oper_rel = ac.cod_oper
                              WHERE
                                  sc.cod_clie = p_cod_clie
                                  AND ac.estado = 'A'
                                  AND sc.cod_oper_rel IS NOT NULL;

        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';
    END;

    PROCEDURE sp_list_cred_soli_vc_opers (
        p_cod_clie       IN VARCHAR2,
        p_cod_oper       IN VARCHAR2,
        p_ret_cursor     OUT SYS_REFCURSOR,
        p_ret_cantidad   OUT NUMBER,
        p_ret_esta       OUT NUMBER,
        p_ret_mens       OUT VARCHAR2
    ) AS

        v_cod_solid_cred       VARCHAR2(20);
        v_cod_cia              vve_cred_soli.cod_empr%TYPE;
        v_nro_operacion        vve_cred_soli.cod_oper_rel%TYPE;
        v_val_mon_fin          vve_cred_soli.val_mon_fin%TYPE;
        v_tipo_operacion       VARCHAR2(20);
        v_fecha_otorgamiento   VARCHAR2(20);
        v_fecha_vencimiento    VARCHAR2(20);
        v_plazo_dias           VARCHAR2(20);
        v_tea_porc             VARCHAR2(20);
        v_val_porc_tea_sigv    vve_cred_soli.val_porc_tea_sigv%TYPE;
        v_tea                  arlcop.tea%TYPE;
        v_asesor_comercial     VARCHAR(100);
        v_region               VARCHAR(100);
        v_saldo_original       NUMBER;
    BEGIN
        v_cod_solid_cred := NULL;
        v_cod_cia := NULL;
        v_tipo_operacion := NULL;
        v_fecha_otorgamiento := NULL;
        v_tea_porc := NULL;
        v_saldo_original := NULL;

    --Obtener codSoliCred y codCia
        BEGIN
            SELECT
                sc.cod_soli_cred,
                sc.cod_empr,
                sc.tip_soli_cred,
                sc.cod_oper_rel,
                sc.val_mon_fin
            INTO
                v_cod_solid_cred,
                v_cod_cia,
                v_tipo_operacion,
                v_nro_operacion,
                v_val_mon_fin
            FROM
                vve_cred_soli sc
            WHERE
                sc.cod_clie = TO_CHAR(p_cod_clie)
                AND sc.cod_oper_rel = TO_CHAR(p_cod_oper);

        EXCEPTION
            WHEN no_data_found THEN
                p_ret_esta := 1;
                p_ret_mens := 'La consulta se realizó de manera exitosa';
        END;

        IF ( v_cod_solid_cred IS NOT NULL ) THEN
            BEGIN
                SELECT DISTINCT
                    ( no_cia )
                INTO v_cod_cia
                FROM
                    arlcop
                WHERE
                    cod_oper = TO_CHAR(p_cod_oper)
                    AND no_cliente = TO_CHAR(p_cod_clie);

            EXCEPTION
                WHEN no_data_found THEN
                    v_cod_cia := NULL;
            END;
        END IF;

    --TIPO_OPERACION

        IF ( v_cod_solid_cred IS NOT NULL AND v_tipo_operacion IS NOT NULL ) THEN
            BEGIN
                SELECT
                    m.descripcion
                INTO v_tipo_operacion
                FROM
                    vve_tabla_maes m,
                    vve_cred_soli cs
                WHERE
                    cs.cod_clie = TO_CHAR(p_cod_clie)
                    AND cs.cod_oper_rel = v_nro_operacion -- IN (v_nro_operacion)
                    AND m.cod_grupo = 86
                    AND m.cod_tipo = cs.tip_soli_cred;

            EXCEPTION
                WHEN no_data_found THEN
                    v_tipo_operacion := NULL;
            END;
        ELSE
            BEGIN
                SELECT
                    m.descripcion
                INTO v_tipo_operacion
                FROM
                    vve_tabla_maes m
                WHERE
                    m.cod_grupo = 86
                    AND m.cod_tipo IN (
                        SELECT
                            DECODE(o.modal_cred,'F','TC01','M','TC03','P','TC05','R','TC07')
                        FROM
                            arlcop o
                        WHERE
                            o.no_cliente = TO_CHAR(p_cod_clie)
                            AND cod_oper = TO_CHAR(v_nro_operacion) -- IN (v_nro_operacion) 
                    );

            EXCEPTION
                WHEN no_data_found THEN
                    v_tipo_operacion := NULL;
            END;
        END IF;

    --FECHA_OTORGAMIENTO

        IF ( v_cod_solid_cred IS NOT NULL ) THEN
            BEGIN
                SELECT
                    DECODE(fec_apro_inte,NULL,'--',TO_CHAR(fec_apro_inte,'DD/MM/YYYY') )
                INTO v_fecha_otorgamiento
                FROM
                    vve_cred_soli
                WHERE
                    cod_clie = TO_CHAR(p_cod_clie)
                    AND cod_oper_rel = TO_CHAR(v_nro_operacion);

            EXCEPTION
                WHEN no_data_found THEN
                    v_fecha_otorgamiento := NULL;
            END;

        END IF;

        IF ( v_fecha_otorgamiento = '--' ) THEN
            BEGIN
                SELECT
                    DECODE(fecha_aut_ope,NULL,'--',TO_CHAR(fecha_aut_ope,'DD/MM/YYYY') )
                INTO v_fecha_otorgamiento
                FROM
                    arlcop
                WHERE
                    no_cliente = TO_CHAR(p_cod_clie)
                    AND cod_oper = TO_CHAR(v_nro_operacion);

            EXCEPTION
                WHEN no_data_found THEN
                    v_fecha_otorgamiento := NULL;
            END;
        END IF;
    --v_fecha_otorgamiento := NVL(v_fecha_otorgamiento,'--'); 

    --FECHA_VENCIMIENTO
      --<I Req. 87567 E2.1 ID## avilca 18/12/2020>
        BEGIN
            SELECT
                TO_CHAR(max(f_vence),'DD/MM/YYYY') fec_vencimiento
            INTO v_fecha_vencimiento
            FROM
                arlcml
            WHERE
                no_cliente = TO_CHAR(p_cod_clie)
                AND cod_oper = TO_CHAR(v_nro_operacion)
                AND no_cia = v_cod_cia ;

        EXCEPTION
            WHEN no_data_found THEN
                v_fecha_vencimiento := NULL;
        END;
    --<F Req. 87567 E2.1 ID## avilca 18/12/2020>
    --PLAZO DIAS

        BEGIN
            SELECT
                q.plazo_dias_op || ' días'
            INTO v_plazo_dias
            FROM
                (
                    SELECT
                        ( CASE nvl(ind_per_gra,'N')
                            WHEN 'S'   THEN ( no_cuotas + 1 ) * fre_pago_dias
                            WHEN 'N'   THEN no_cuotas * fre_pago_dias
                        END ) plazo_dias_op
                    FROM
                        arlcop
                    WHERE
                        cod_oper = v_nro_operacion--<nro_operacion> 
                        AND NOT EXISTS (
                            SELECT
                                1
                            FROM
                                vve_cred_soli
                            WHERE
                                cod_clie = p_cod_clie
                                AND cod_oper_rel = cod_oper
                        )
                    UNION
                    SELECT
                        no_cuotas * fre_pago_dias AS plazo_dias_op
                    FROM
                        arlcop
                    WHERE
                        cod_oper = v_nro_operacion--<nro_operacion>  
                        AND EXISTS (
                            SELECT
                                1
                            FROM
                                vve_cred_soli
                            WHERE
                                cod_clie = p_cod_clie
                                AND cod_oper_rel = cod_oper
                        )
                ) q;

        EXCEPTION
            WHEN no_data_found THEN
                v_plazo_dias := NULL;
        END;

    --TEA

       /* BEGIN
            SELECT
                sc.val_porc_tea_sigv,
                o.tea
            INTO
                v_val_porc_tea_sigv,
                v_tea
            FROM
                vve_cred_soli sc,
                arlcop o
            WHERE
                sc.cod_oper_rel = o.cod_oper
                AND sc.cod_clie = o.no_cliente
                AND sc.cod_oper_rel = v_nro_operacion
                AND sc.cod_clie = p_cod_clie;
*/

          BEGIN
            SELECT
                sc.val_porc_tea_sigv
            INTO
                v_val_porc_tea_sigv
            FROM
                vve_cred_soli sc,
                arlcop o
            WHERE
                sc.cod_oper_rel = o.cod_oper
                AND sc.cod_clie = o.no_cliente
                AND sc.cod_oper_rel = v_nro_operacion
                AND sc.cod_clie = p_cod_clie;
        EXCEPTION
            WHEN no_data_found THEN
                v_val_porc_tea_sigv := NULL;
                v_tea := NULL;
        END;

       -- IF ( v_val_porc_tea_sigv IS NOT NULL AND v_tea IS NOT NULL ) THEN
        IF ( v_val_porc_tea_sigv IS NOT NULL ) THEN
            BEGIN
                v_tea_porc := 0.00;
               -- v_tea_porc := round( (v_val_porc_tea_sigv / v_tea),2);
                v_tea_porc := round( (v_val_porc_tea_sigv),2);
            EXCEPTION
                WHEN no_data_found THEN
                    v_tea_porc := NULL;
            END;
        ELSE
            v_tea_porc := '0.00';
        END IF;

    --ASESOR COMERCIAL & REGION powered by *Lucía*

        BEGIN
            SELECT
                cod_pers_soli,
                (
                    SELECT DISTINCT
                        des_zona
                    FROM
                        sis_mae_usuario u,
                        sis_mae_usuario_filial uf,
                        vve_mae_zona_filial zf,
                        vve_mae_zona z
                    WHERE
                        u.txt_usuario = s.cod_pers_soli
                        AND u.cod_id_usuario = uf.cod_id_usuario
                        AND uf.cod_filial = zf.cod_filial
                        AND zf.cod_zona = z.cod_zona
                )
            INTO
                v_asesor_comercial,
                v_region
            FROM
                vve_cred_soli s
            WHERE
                cod_soli_cred = v_cod_solid_cred;

        EXCEPTION
            WHEN no_data_found THEN
                v_asesor_comercial := NULL;
                v_region := NULL;
        END;

    --SALDO ORIGINAL

        BEGIN
            SELECT
                total_financiar
            INTO v_saldo_original
            FROM
                arlcop
            WHERE
                cod_oper = v_nro_operacion
                AND ROWNUM = 1;

        EXCEPTION
            WHEN no_data_found THEN
                v_saldo_original := NULL;
        END;

        OPEN p_ret_cursor FOR SELECT
                                 v_cod_cia              AS nro_cia,
                                 v_nro_operacion        AS nro_operacion,
                                 v_tipo_operacion       AS tipo_operacion,
                                 v_fecha_otorgamiento   AS fec_otorgamiento,
                                 v_fecha_vencimiento    AS fec_vencimiento,
                                 v_plazo_dias           AS plazo_dias,
                                 v_tea_porc             AS tea,
                                 (
                                     SELECT
                                         fn_ratio_cobertura(v_cod_solid_cred)
                                     FROM
                                         dual
                                 ) AS ratio_cobertura,
                                 v_asesor_comercial     AS asesor_comercial,
                                 v_region               AS region,
                                 v_saldo_original       AS saldo_original,
                                 v_val_mon_fin          AS val_mon_fin
                             FROM
                                 dual;

        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';
    EXCEPTION
        WHEN OTHERS THEN
            p_ret_esta :=-1;
            p_ret_mens := 'SP_LIST_CRED_SOLI_VC_OPERS:' || sqlerrm;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR','SP_LIST_CRED_SOLI_VC_OPERS',NULL,'Error en la consulta',p_ret_mens,
            NULL);
    END sp_list_cred_soli_vc_opers;


/*-----------------------------------------------------------------------------
    Nombre : SP_lIST_AMORTIZACION_X_OPERA
    Proposito : Lista las amortizaciones por la lista de operaciones realizadas por el cliente.
    Referencias : 
    Parametros : p_cod_oper 
    Log de Cambios
    Fecha        Autor          Descripcion
    12/03/2020   ebarboza    REQ CU-19     Creacion
  ----------------------------------------------------------------------------*/

    PROCEDURE SP_lIST_AMORTIZACION_X_OPERA (
        p_cod_sociedad   VARCHAR2,
        p_cod_ref1       arlcml.cod_oper%TYPE,
        p_num_refer      arlcml.cod_oper%TYPE,
        p_ret_cursor     OUT SYS_REFCURSOR,
        p_ret_esta       OUT NUMBER,
        p_ret_mens       OUT VARCHAR2
    ) AS
    BEGIN
        OPEN p_ret_cursor FOR SELECT
                                  nvl(amortizacion,0) AS AMORTIZACION
                              FROM
                                  arlcml
                              WHERE
                                  no_cia = (SELECT COD_CIA FROM  gen_mae_sociedad WHERE cod_sociedad= p_cod_sociedad AND ROWNUM <=1)
                                  AND cod_oper = p_cod_ref1
                                  AND no_letra = p_num_refer;
        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';


    END SP_lIST_AMORTIZACION_X_OPERA;

    PROCEDURE sp_list_cred_soli_vc_garan (
        p_cod_clie       IN vve_cred_soli.cod_clie%TYPE,
        p_cod_oper       IN vve_cred_soli.cod_oper_rel%TYPE,
        p_ret_cursor     OUT SYS_REFCURSOR,
        p_ret_cantidad   OUT NUMBER,
        p_ret_esta       OUT NUMBER,
        p_ret_mens       OUT VARCHAR2
    ) AS
    BEGIN
        OPEN p_ret_cursor FOR SELECT
                                  s.cod_oper_rel     AS nro_operacion,
                                  g.cod_garantia     AS nro_garantia,
                                  DECODE(ind_tipo_garantia,'M','Mobiliaria','H','Hipotecaria') AS tipo_garantia,
                                  ( CASE nvl(sg.ind_gara_adic,'N')
                                      WHEN 'N'   THEN s.nro_poli_seg
                                      WHEN 'S'   THEN 'N/A'
                                  END ) AS nro_poli,
                                  ( CASE nvl(sg.ind_gara_adic,'N')
                                      WHEN 'N'   THEN s.cod_esta_poli
                                      WHEN 'S'   THEN 'N/A'
                                  END ) AS cod_est_poli,
                                  DECODE(sg.ind_gara_adic,NULL, (
                                      SELECT
                                          descripcion
                                      FROM
                                          vve_tabla_maes m
                                      WHERE
                                          m.cod_grupo = 109
                                          AND cod_tipo = s.cod_esta_poli
                                  ),NULL,NULL) AS est_poli,
                                  'USD' AS divisa,
                                  nvl(g.val_const_gar,0) * 0.8 AS val_comercial,
                                  g.val_realiz_gar   AS val_realiz,
                                  g.fec_fab_const    AS fec_const
                              FROM
                                  vve_cred_maes_gara g,
                                  vve_cred_soli_gara sg,
                                  vve_cred_soli s
                              WHERE
                                  s.cod_clie = p_cod_clie
                                  AND s.cod_soli_cred = sg.cod_soli_cred
                                  AND sg.cod_gara = g.cod_garantia
                                  AND s.cod_oper_rel = p_cod_oper
                                   AND sg.ind_inactivo='N' or sg.ind_inactivo is null
                              ORDER BY
                                  3,
                                  2;

        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';
    EXCEPTION
        WHEN OTHERS THEN
            p_ret_esta :=-1;
            p_ret_mens := 'SP_LIST_CRED_SOLI_VC_GARAN:' || sqlerrm;
    END sp_list_cred_soli_vc_garan;

     PROCEDURE sp_list_cred_soli_co (
        p_cod_region         IN vve_mae_zona.cod_zona%TYPE,
        p_cod_area_vta       IN gen_area_vta.cod_area_vta%TYPE,
        p_cod_tipo_oper      IN vve_cred_soli.tip_soli_cred%TYPE,
        p_fec_factu_inicio   IN VARCHAR2,
        p_fec_factu_fin      IN VARCHAR2,
        p_op_aprobados       IN VARCHAR2,
        p_cliente            IN VARCHAR2, -- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>
        p_ruc_cliente        IN VARCHAR2,  -- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>
        p_fec_ope_inicio     IN VARCHAR2,-- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>
        p_fec_ope_fin        IN VARCHAR2, -- <Req. 87567 E2.1 ID:12 avilca 15/09/2020>
        p_ret_cursor         OUT SYS_REFCURSOR,
        p_ret_cantidad       OUT NUMBER,
        p_ret_esta           OUT NUMBER,
        p_ret_mens           OUT VARCHAR2
    ) AS
    BEGIN
        OPEN p_ret_cursor FOR SELECT DISTINCT
                                  to_number(cs.cod_soli_cred) AS nro_solicitud,
                                  (
                                      SELECT
                                          descripcion
                                      FROM
                                          vve_tabla_maes
                                      WHERE
                                          cod_tipo = cs.cod_estado
                                  ) AS estado_solicitud,
                                  gav.des_area_vta       AS area_venta,
                                  cs.cod_clie            AS cod_cliente,
                                  gp.nom_perso           AS nom_cliente,
                                  CASE
                                      WHEN gp.cod_tipo_perso = 'J' THEN 'Jurídica'
                                      ELSE 'Natural'
                                  END AS tipo_persona,
                                  pv.num_ficha_vta_veh   AS nro_ficha_venta,
                                   (
                                  SELECT SUM(pf.can_veh_fin)  FROM vve_cred_soli_prof pf
                                  WHERE pf.cod_soli_cred = cs.cod_soli_cred
                                    AND pf.ind_inactivo = 'N' )  AS nro_unidades,
                                  
                                --  sp.can_veh_fin         AS nro_unidades,
                                       (
                                  SELECT SUM(pf.val_vta_tot_fin)  FROM vve_cred_soli_prof pf
                                  WHERE pf.cod_soli_cred = cs.cod_soli_cred
                                    AND pf.ind_inactivo = 'N' )  AS total_venta,
                                
                                --  sp.val_vta_tot_fin     AS total_venta,
                                  (
                                      SELECT
                                          descripcion
                                      FROM
                                          vve_tabla_maes
                                      WHERE
                                          cod_grupo = '86'
                                          AND cod_tipo = cs.tip_soli_cred
                                  ) AS tipo_operacion,
                                  cs.val_ci              AS cuota_inicial,
                                  --cs.val_mon_fin         AS monto_financiado,
                                  vcs.val_mon_fin        AS monto_financiado,
                                  cs.can_plaz_mes        AS nro_meses,
                                  zo.des_zona            AS region,
                                  TO_CHAR(cs.fec_soli_cred,'dd/mm/yyyy') AS fecha,
                                  (
                                      SELECT
                                          CASE
                                              WHEN veh.des_agru_veh_seg = '' THEN 'N/A'
                                              ELSE veh.des_agru_veh_seg
                                          END
                                      FROM
                                          vve_cred_maes_gara cmg
                                          INNER JOIN vve_cred_agru_veh_seg veh ON cmg.cod_tipo_veh = veh.cod_agru_veh_seg
                                          INNER JOIN vve_cred_soli_gara csg ON csg.cod_gara = cmg.cod_garantia
                                      WHERE
                                          csg.cod_soli_cred = cs.cod_soli_cred
                                          AND ROWNUM = 1
                                  ) AS segmento,
                                  (
                                      SELECT
                                          f.nom_filial
                                      FROM
                                          gen_filiales f,
                                          vve_proforma_veh p,
                                          vve_cred_soli_prof sp,
                                          vve_cred_soli s
                                      WHERE
                                          s.cod_soli_cred = sp.cod_soli_cred
                                          AND sp.num_prof_veh = p.num_prof_veh
                                          AND p.cod_filial = f.cod_filial
                                          AND p.cod_sucursal = f.cod_sucursal
                                          AND s.cod_soli_cred = cs.cod_soli_cred
                                          AND ROWNUM = 1
                                  ) AS sucursal,
                                  cs.cod_oper_rel        AS op_cronograma,
            --(
             --SELECT
                                  CASE
                                      WHEN arl.estado = 'A' THEN 'APROBADO'
                                      ELSE 'PENDIENTE'
                                  END AS estado_op,
                                  cs.val_porc_ci         AS cuota_inicial_porcentaje,
                                  nvl2(cs.can_dias_venc_1ra_letr,cs.can_dias_venc_1ra_letr,'0') AS vencimiento_primera_letra,
                                  nvl2(cs.val_porc_tea_sigv,cs.val_porc_tea_sigv,'0') AS tea_sin_igv,
                                  TO_CHAR(add_months(cs.fec_venc_1ra_let, (cs.can_plaz_mes - 1) ),'dd/mm/yyyy') AS vencimiento_ultima_letra
                                 ,
                                  (
                                      SELECT
                                          --SUM(cmg.val_realiz_gar)
                                          DECODE(SUM(cmg.val_realiz_gar),NULL,0,SUM(cmg.val_realiz_gar)) -- MBARDALES 14/04/2021
                                      FROM
                                          vve_cred_maes_gara cmg
                                          INNER JOIN vve_cred_soli_gara csg ON csg.cod_gara = cmg.cod_garantia
             --INNER JOIN vve_cred_soli crs ON crs.cod_soli_cred = csg.cod_soli_cred
                                      WHERE
                                          csg.cod_soli_cred = cs.cod_soli_cred -- crs.cod_oper_rel = cs.cod_oper_rel
             --AND crs.cod_soli_cred = cs.cod_soli_cred
                                          AND csg.ind_gara_adic = 'S'
                                          AND nvl(csg.ind_inactivo,'N') = 'N'
                                  ) AS garantias_adicionales,
                                  (
                                      SELECT
                                          fn_ratio_cobertura(cs.cod_soli_cred)
                                      FROM
                                          dual
                                  ) AS ratio_cobertura,
                                  CASE
                                      WHEN cs.val_porc_gast_admi > 0 THEN TO_CHAR(cs.val_porc_gast_admi)
                                                                          || '%'
                                      ELSE '0'
                                  END AS gastos_administrativos,
                                  (
                                      SELECT
                                          CASE
                                              WHEN descripcion = 'Divemotor' THEN 'SI'
                                              ELSE 'NO'
                                          END
                                      FROM
                                          vve_tabla_maes
                                      WHERE
                                          cod_grupo = '90'
                                          AND cod_tipo = cs.ind_tipo_segu
                                  ) AS seguro_divemotor,
                                  TO_CHAR(arl.fecha,'dd/mm/yyyy') fecha_op, -- <Req. 87567 E2.1 ID:12 avilca 16/09/2020>
                                  TO_CHAR(aml.f_aceptada,'dd/mm/yyyy') fecha_aprob_op, -- <Req. 87567 E2.1 ID:12 avilca 16/09/2020>
                                  fn_usu_estado(cs.cod_soli_cred,'ES01') usuario_est_reg,
                                  fn_fec_estado(cs.cod_soli_cred,'ES01') fecha_est_reg,
                                  fn_usu_estado(cs.cod_soli_cred,'ES02') usuario_est_vig,
                                  fn_fec_estado(cs.cod_soli_cred,'ES02') fecha_est_vig,
                                  fn_usu_estado(cs.cod_soli_cred,'ES03') usuario_est_eval,
                                  fn_fec_estado(cs.cod_soli_cred,'ES03') fecha_est_eval,
                                  fn_usu_estado(cs.cod_soli_cred,'ES04') usuario_est_aprob,
                                  fn_fec_estado(cs.cod_soli_cred,'ES04') fecha_est_aprob,
                                  gp.num_ruc,
                                  t.cod_familia_veh,
                                  t.des_familia_veh,
                                  t.cod_tipo_veh,
                                  t.des_tipo_veh,
                                  t.cod_marca,
                                  t.nom_marca,
                                  (
                                      SELECT SUM(mg.val_realiz_gar)
                                        FROM vve_cred_maes_gara mg
                                        INNER JOIN vve_cred_soli_gara sg ON sg.cod_gara = mg.cod_garantia
                                        INNER JOIN vve_cred_soli sc ON sg.cod_soli_cred = sc.cod_soli_cred
                                        WHERE mg.ind_tipo_garantia = 'M' AND mg.ind_tipo_bien = 'A'
                                        AND sg.cod_soli_cred = cs.cod_soli_cred
                                  )suma_val_realiz,
                                  fn_usu_aprob(cs.cod_soli_cred,1) usuario_aprob_nivel1,
                                  fn_fec_aprob(cs.cod_soli_cred,1) fecha_aprob_nivel1,
                                  fn_usu_aprob(cs.cod_soli_cred,2) usuario_aprob_nivel2,
                                  fn_fec_aprob(cs.cod_soli_cred,2) fecha_aprob_nivel2,
                                  fn_usu_aprob(cs.cod_soli_cred,3) usuario_aprob_nivel3,
                                  fn_fec_aprob(cs.cod_soli_cred,3) fecha_aprob_nivel3,
                                  fn_usu_aprob(cs.cod_soli_cred,4) usuario_aprob_nivel4,
                                  fn_fec_aprob(cs.cod_soli_cred,4) fecha_aprob_nivel4,
                                  fn_usu_aprob(cs.cod_soli_cred,5) usuario_aprob_nivel5,
                                  fn_fec_aprob(cs.cod_soli_cred,5) fecha_aprob_nivel5,
                                  fn_usu_aprob(cs.cod_soli_cred,6) usuario_aprob_nivel6,
                                  fn_fec_aprob(cs.cod_soli_cred,6) fecha_aprob_nivel6,
								  --<---I AVILCA 13/10/2021--->
                                  fn_usu_aprob_soli(cs.cod_soli_cred) usuario_aprob_nivel_min,
                                  fn_fec_aprob_soli(cs.cod_soli_cred) fecha_aprob_nivel_min,
                                  --<---F AVILCA 13/10/2021--->
                                 (
                                       SELECT Max(csa.ind_nivel)
                                        FROM vve_cred_soli_apro csa
                                        WHERE csa.cod_soli_cred = cs.cod_soli_cred
                                   )niveles_aprob,
                                   (
                                   SELECT csm.val_gast_adm FROM vve_cred_simu csm
                                   WHERE csm.cod_soli_cred = cs.cod_soli_cred AND csm.ind_inactivo = 'N'
                                  ) val_gast_adm

                              FROM
                                  gen_persona gp
                                  INNER JOIN vve_cred_soli cs ON gp.cod_perso = cs.cod_clie
                                  INNER JOIN gen_area_vta gav ON gav.cod_area_vta = cs.cod_area_vta
                                  INNER JOIN vve_cred_soli_prof sp ON cs.cod_soli_cred = sp.cod_soli_cred
                                  INNER JOIN vve_ficha_vta_veh fv ON fv.cod_clie = cs.cod_clie
                                  INNER JOIN vve_mae_zona_filial zf ON fv.cod_filial = zf.cod_filial
                                  INNER JOIN vve_ficha_vta_proforma_veh pv ON fv.num_ficha_vta_veh = pv.num_ficha_vta_veh AND (pv.ind_inactivo = 'N' OR cs.cod_estado = 'ES06')
                                  INNER JOIN vve_mae_zona zo ON zf.cod_zona = zo.cod_zona
                                  LEFT JOIN arlcop arl ON arl.cod_oper = cs.cod_oper_rel AND arl.no_cia = cs.cod_empr
                                  LEFT JOIN arlcml aml ON arl.cod_oper = aml.cod_oper AND arl.no_cia = aml.no_cia-- <Req. 87567 E2.1 ID:12 avilca 16/09/2020>
                                  LEFT JOIN vve_cred_simu vcs ON cs.cod_soli_cred = vcs.cod_soli_cred AND  vcs.ind_inactivo = 'N'
                                  INNER JOIN ( SELECT
                                             DISTINCT csa.cod_soli_cred,
                                             pvd.cod_familia_veh,
                                             fv.des_familia_veh,
                                             pvd.cod_tipo_veh,
                                             tv.des_tipo_veh,
                                             pvd.cod_marca,
                                             gm.nom_marca
                                            FROM vve_cred_soli csa
                                                INNER JOIN vve_cred_soli_prof csp ON csa.cod_soli_cred = csp.cod_soli_cred
                                                           and csp.fec_crea_reg = (
                                                                                    SELECT Min(fec_crea_reg)
                                                                                    FROM vve_cred_soli_prof
                                                                                      WHERE cod_soli_cred = csa.cod_soli_cred
                                                                                   )
                                                INNER JOIN vve_proforma_veh_det pvd ON pvd.num_prof_veh = csp.num_prof_veh
                                                INNER JOIN vve_familia_veh fv ON fv.cod_familia_veh = pvd.cod_familia_veh
                                                INNER JOIN vve_tipo_veh tv ON tv.cod_tipo_veh = pvd.cod_tipo_veh
                                                INNER JOIN gen_marca gm ON gm.cod_marca = pvd.cod_marca

                                  )t on t.cod_soli_cred = cs.cod_soli_cred
                              WHERE
                                  fv.cod_clie = cs.cod_clie
                                  AND sp.num_prof_veh = pv.num_prof_veh
                                  AND ( p_cod_region IS NULL
                                        OR zo.cod_zona = p_cod_region ) --2 4
                                  AND ( p_cod_area_vta IS NULL
                                        OR gav.cod_area_vta = p_cod_area_vta ) --001 camiones,003 buses
                                  AND ( p_cod_tipo_oper IS NULL
                                        OR cs.tip_soli_cred = p_cod_tipo_oper ) --TC02 TC06
                                  AND ( p_op_aprobados = 'false'
                                        OR ( cs.cod_estado = 'ES04'
                                        AND arl.estado = 'A' ) )
                                 -- <I Req. 87567 E2.1 ID:12 avilca 15/09/2020>
                                  AND ( p_cliente IS NULL
                                        OR gp.nom_perso = TRANSLATE(TRIM(upper(p_cliente)), 'ÁÉÍÓÚ', 'AEIOU') )

                                  AND ( p_ruc_cliente IS NULL
                                        OR gp.num_ruc = p_ruc_cliente )

                                  AND ( ( p_fec_ope_inicio IS NULL
                                          AND p_fec_ope_fin IS NULL )
                                        OR trunc(arl.fecha) BETWEEN TO_DATE(p_fec_ope_inicio,'DD/MM/YYYY') AND TO_DATE(p_fec_ope_fin
                                       ,'DD/MM/YYYY') )
                                  -- < F Req. 87567 E2.1 ID:12 avilca 15/09/2020>
                                  AND ( ( p_fec_factu_inicio IS NULL
                                          AND p_fec_factu_fin IS NULL )
                                        OR trunc(cs.fec_soli_cred) BETWEEN TO_DATE(p_fec_factu_inicio,'DD/MM/YYYY') AND TO_DATE(p_fec_factu_fin
                                       ,'DD/MM/YYYY') );

        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';
    EXCEPTION
        WHEN OTHERS THEN
            p_ret_esta :=-1;
            p_ret_mens := 'SP_LIST_CRED_SOLI_CO:' || sqlerrm;
    END sp_list_cred_soli_co;

	   FUNCTION fn_usu_estado (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_cod_estado         IN vve_cred_soli.cod_estado%TYPE
    ) RETURN VARCHAR2 AS
       v_usuario        VARCHAR2(20);
    BEGIN

         SELECT csm.co_usuario_crea_reg
          INTO  v_usuario
         FROM vve_cred_soli s,vve_cred_soli_modi csm
          WHERE s.cod_soli_cred = csm.cod_soli_cred
          AND s.cod_soli_cred = p_cod_soli_cred
          AND csm.VAL_COLUMNA= p_cod_estado
          AND csm.fec_crea_reg = (select MIN(fec_crea_reg) from vve_cred_soli_modi r
					WHERE r.cod_soli_cred = p_cod_soli_cred
          AND r.VAL_COLUMNA= p_cod_estado

					);


      RETURN v_usuario;
    END fn_usu_estado;

    FUNCTION fn_fec_estado (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_cod_estado         IN vve_cred_soli.cod_estado%TYPE
    )RETURN VARCHAR2 AS
      v_ret_fecha  VARCHAR2(15);
    BEGIN

    SELECT TO_CHAR(csm.fec_crea_reg,'dd/mm/yyyy')
          INTO  v_ret_fecha
         FROM vve_cred_soli s,vve_cred_soli_modi csm
          WHERE s.cod_soli_cred = csm.cod_soli_cred
          AND s.cod_soli_cred = p_cod_soli_cred
          AND csm.VAL_COLUMNA= p_cod_estado
          AND csm.fec_crea_reg = (select MIN(fec_crea_reg) from vve_cred_soli_modi r
          WHERE r.cod_soli_cred = p_cod_soli_cred
          AND r.VAL_COLUMNA= p_cod_estado
          );

      RETURN v_ret_fecha;
    END fn_fec_estado;

	FUNCTION fn_usu_aprob (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_ind_nivel         IN vve_cred_soli_apro.ind_nivel%TYPE
    ) RETURN VARCHAR2 AS
       v_usuario        VARCHAR2(20);
    BEGIN

        SELECT usu.txt_usuario
          INTO v_usuario
         FROM vve_cred_soli_apro csa
          INNER JOIN  sistemas.sis_mae_usuario usu ON csa.cod_id_usua = usu.cod_id_usuario
         WHERE cod_soli_cred = p_cod_soli_cred and ind_nivel = p_ind_nivel
				   AND csa.est_apro = 'EEA01'
				   AND csa.ind_inactivo = 'N'
					 AND usu.ind_inactivo = 'N';


      RETURN v_usuario;
    END fn_usu_aprob;

   --<---I AVILCA 13/10/2021--->
    FUNCTION fn_usu_aprob_soli (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE
    ) RETURN VARCHAR2 AS
       v_usuario                VARCHAR2(30);
       v_contador               NUMBER;
       v_contador_noaprob       NUMBER;			 
    BEGIN
        v_usuario := '';
/*        BEGIN
            SELECT count(*) INTO v_contador_noaprob 
            FROM vve_cred_soli_apro WHERE cod_soli_cred = p_cod_soli_cred
           AND ind_inactivo = 'N';
        EXCEPTION
            WHEN OTHERS THEN
            v_contador_noaprob := 0;
        END;*/

  --      IF v_contador_noaprob >  0 THEN
					BEGIN
							SELECT count(*) INTO v_contador 
							FROM vve_cred_soli_apro WHERE cod_soli_cred = p_cod_soli_cred
							AND ind_inactivo = 'N';
					EXCEPTION
							WHEN OTHERS THEN
							v_contador := 0;
					END;

					IF v_contador > 1 THEN
						SELECT csa.cod_usua_crea_regi
							INTO v_usuario
						FROM vve_cred_soli_apro csa
						WHERE cod_soli_cred = p_cod_soli_cred and ind_nivel = (
									SELECT MAX(IND_NIVEL) FROM vve_cred_soli_apro
									 WHERE cod_soli_cred = p_cod_soli_cred
									AND ind_inactivo = 'N' 
					 )
						AND csa.ind_inactivo = 'N';

					END IF;

					IF v_contador = 1 THEN
						SELECT csa.cod_usua_crea_regi
							INTO v_usuario
						FROM vve_cred_soli_apro csa
						 WHERE cod_soli_cred = p_cod_soli_cred 
						AND csa.ind_inactivo = 'N';

					END IF;
	--			END IF;

      RETURN v_usuario;
    END fn_usu_aprob_soli;
  --<---F AVILCA 13/10/2021--->	

    FUNCTION fn_fec_aprob (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE,
        p_ind_nivel         IN vve_cred_soli_apro.ind_nivel%TYPE
    )RETURN VARCHAR2 AS
      v_ret_fecha  VARCHAR2(15);
    BEGIN

        SELECT TO_CHAR(csa.fec_esta_apro,'dd/mm/yyyy')
          INTO v_ret_fecha
         FROM vve_cred_soli_apro csa
          INNER JOIN  sistemas.sis_mae_usuario usu ON csa.cod_id_usua = usu.cod_id_usuario
         WHERE cod_soli_cred = p_cod_soli_cred and ind_nivel = p_ind_nivel
				   AND csa.est_apro = 'EEA01'
				   AND csa.ind_inactivo = 'N'
					 AND usu.ind_inactivo = 'N';

      RETURN v_ret_fecha;
    END fn_fec_aprob;

	   --<---I AVILCA 13/10/2021--->
    FUNCTION fn_fec_aprob_soli (
        p_cod_soli_cred      IN vve_cred_soli.cod_soli_cred%TYPE
    )RETURN VARCHAR2 AS
      v_ret_fecha  VARCHAR2(15);
      v_contador       NUMBER;
    BEGIN

        BEGIN
            SELECT count(*) INTO v_contador 
            FROM vve_cred_soli_apro WHERE cod_soli_cred = p_cod_soli_cred
            AND ind_inactivo = 'N';
        EXCEPTION
            WHEN OTHERS THEN
            v_contador := 0;
        END;

        IF v_contador > 1 THEN

            SELECT TO_CHAR(csa.fec_crea_regi,'dd/mm/yyyy')
              INTO v_ret_fecha
             FROM vve_cred_soli_apro csa
              WHERE cod_soli_cred = p_cod_soli_cred and ind_nivel  = (
                    SELECT MAX(IND_NIVEL) FROM vve_cred_soli_apro
                     WHERE cod_soli_cred = p_cod_soli_cred
                    AND ind_inactivo = 'N' 
             )
                       AND csa.ind_inactivo = 'N';        
        END IF;

        IF v_contador = 1 THEN

             SELECT TO_CHAR(csa.fec_crea_regi,'dd/mm/yyyy')
              INTO v_ret_fecha
             FROM vve_cred_soli_apro csa
             WHERE cod_soli_cred = p_cod_soli_cred 
               AND csa.ind_inactivo = 'N';         

        END IF;

      RETURN v_ret_fecha;
    END fn_fec_aprob_soli;
    --<---F AVILCA 13/10/2021--->



    FUNCTION fn_ratio_cobertura (
        p_cod_soli_cred IN VARCHAR2
    ) RETURN NUMBER AS

        val_ratio_cobertura   NUMBER;
        val_anio              VARCHAR2(4);
        v_cod_area_vta        VARCHAR2(500);
        v_cod_familia_veh     VARCHAR2(500);
        v_cod_tipo_veh        VARCHAR2(500);
        v_no_cia              VARCHAR2(500);
        v_cant_periodo        NUMBER;
    BEGIN
        SELECT
            TO_CHAR(SYSDATE,'YYYY')
        INTO val_anio
        FROM
            dual;

        SELECT
            cod_cia,
            cod_area_vta,
            cod_familia_veh,
            cod_tipo_veh
        INTO
            v_no_cia,
            v_cod_area_vta,
            v_cod_familia_veh,
            v_cod_tipo_veh
        FROM
            (
                SELECT DISTINCT
                    b.cod_cia,
                    b.cod_area_vta,
                    c.cod_familia_veh,
                    c.cod_tipo_veh
                FROM
                    vve_cred_soli_prof a
                    INNER JOIN vve_proforma_veh b ON a.num_prof_veh = b.num_prof_veh
                                                     AND a.ind_inactivo = 'N'
                                                     AND b.cod_estado_prof IN (
                        'F',
                        'A'
                    )
                    INNER JOIN vve_proforma_veh_det c ON b.num_prof_veh = c.num_prof_veh
                    INNER JOIN vve_ficha_vta_proforma_veh d ON d.num_prof_veh = a.num_prof_veh
                                                               AND d.ind_inactivo = 'N'
                WHERE
                    a.cod_soli_cred = p_cod_soli_cred
                    AND ROWNUM = 1
            );

        SELECT
            COUNT(1)
        INTO v_cant_periodo
        FROM
            (
                SELECT
                    MIN(x.fec_venc) fec_venc,
                    EXTRACT(YEAR FROM x.fec_venc) anio
                FROM
                    vve_cred_simu_lede x
                    INNER JOIN vve_cred_simu s ON s.cod_simu = x.cod_simu
                                                  AND s.ind_inactivo = 'N'
                WHERE
                    s.cod_soli_cred = p_cod_soli_cred
                    AND x.cod_conc_col = 2
                GROUP BY
                    EXTRACT(YEAR FROM x.fec_venc)
            ) a
            INNER JOIN (
                SELECT
                    x.val_mon_conc,
                    x.fec_venc,
                    EXTRACT(YEAR FROM x.fec_venc) anio,
                    x.cod_det_simu,
                    x.cod_nume_letr,
                    x.cod_simu
                FROM
                    vve_cred_simu_lede x
                    INNER JOIN vve_cred_simu s ON s.cod_simu = x.cod_simu
                                                  AND s.ind_inactivo = 'N'
                WHERE
                    s.cod_soli_cred = p_cod_soli_cred
                    AND x.cod_conc_col = 2
            ) b ON a.anio = b.anio
                   AND a.fec_venc = b.fec_venc;

        SELECT
            round(nvl(a.d / n.val_mon_conc,0),2) -- as ratio_cob,n.anio as anio
        INTO val_ratio_cobertura
        FROM
            (
                SELECT
                    r.cod_soli_cred,
                    r.val_can_anos,
                    nvl(r.am,0) + nvl(y.ah,0) d
                FROM
                    (
                        SELECT
                            t.cod_soli_cred,
                            t.val_can_anos,
                            SUM(t.am) AS am
                        FROM
                            (
                                SELECT
                                    sg.cod_gara,
                                    x.val_can_anos,
                                    mg.ind_tipo_garantia,
                                    mg.ind_tipo_bien,
                                    sg.cod_soli_cred,
                                    mg.val_realiz_gar,
                                    mg.val_nro_rango,
                                    x.val_porc_depr,
                                    ( nvl(mg.val_realiz_gar * x.val_porc_depr,0) ) AS am
                                FROM
                                    vve_cred_maes_gara mg
                                    INNER JOIN vve_cred_soli_gara sg ON sg.cod_gara = mg.cod_garantia
                                                                        AND sg.ind_inactivo = 'N'
                                    INNER JOIN vve_mov_avta_fam_tipoveh y ON y.cod_area_vta = '014'
                                                                             AND y.cod_tipo_veh = mg.cod_tipo_veh
                                                                             AND nvl(y.ind_inactivo,'N') = 'N'
                                    INNER JOIN vve_cred_mae_depr x ON x.no_cia = v_no_cia
                                                                      AND x.cod_area_vta = y.cod_area_vta
                                                                      AND x.cod_familia_veh = y.cod_familia_veh
                                                                      AND x.cod_tipo_veh = mg.cod_tipo_veh
                                                                      AND x.val_can_anos < v_cant_periodo
                                WHERE
                                    sg.cod_soli_cred = p_cod_soli_cred
                                    AND mg.ind_tipo_garantia = 'M'
                                    AND ( sg.ind_inactivo IS NULL
                                          OR sg.ind_inactivo <> 'S' )
                                    AND mg.ind_tipo_bien = 'P'
                                UNION
                                SELECT DISTINCT
                                    sg.cod_gara,
                                    e.val_can_anos,
                                    mg.ind_tipo_garantia,
                                    mg.ind_tipo_bien,
                                    a.cod_soli_cred,
                                    c.val_pre_veh,
                                    mg.val_nro_rango,
                                    e.val_porc_depr,
                                    ( nvl(c.val_pre_veh * e.val_porc_depr,0) ) AS am
                                FROM
                                    vve_cred_maes_gara mg
                                    INNER JOIN vve_cred_soli_gara sg ON sg.cod_gara = mg.cod_garantia
                                    INNER JOIN vve_cred_soli_prof a ON sg.cod_soli_cred = a.cod_soli_cred
                                                                       AND a.ind_inactivo = 'N'
                                    INNER JOIN vve_proforma_veh b ON a.num_prof_veh = b.num_prof_veh
                                                                     AND a.ind_inactivo = 'N'
                                                                     AND b.cod_estado_prof IN (
                                        'F',
                                        'A'
                                    )
                                    INNER JOIN vve_proforma_veh_det c ON b.num_prof_veh = c.num_prof_veh
                                    INNER JOIN vve_ficha_vta_proforma_veh d ON d.num_prof_veh = a.num_prof_veh
                                                                               AND d.ind_inactivo = 'N'
                                    INNER JOIN vve_cred_mae_depr e ON e.no_cia = b.cod_cia
                                                                      AND e.cod_familia_veh = c.cod_familia_veh
                                                                      AND e.cod_area_vta = b.cod_area_vta
                                                                      AND e.cod_tipo_veh = c.cod_tipo_veh
                                                                      AND e.val_can_anos < v_cant_periodo
                                WHERE
                                    sg.cod_soli_cred = p_cod_soli_cred
                                    AND mg.ind_tipo_garantia = 'M'
                                    AND ( sg.ind_inactivo IS NULL
                                          OR sg.ind_inactivo <> 'S' )
                                    AND mg.ind_tipo_bien = 'A'
                            ) t
                        GROUP BY
                            t.val_can_anos,
                            t.cod_soli_cred
                    ) r
                    LEFT JOIN (
                        SELECT
                            cod_soli_cred,
                            ah
                        FROM
                            (
                                SELECT
                                    sg.cod_soli_cred,
                                    SUM(nvl(mg.val_realiz_gar,0) ) AS ah
                                FROM
                                    vve_cred_maes_gara mg
                                    INNER JOIN vve_cred_soli_gara sg ON sg.cod_gara = mg.cod_garantia
                                                                        AND sg.ind_inactivo = 'N'
                                WHERE
                                    sg.cod_soli_cred = p_cod_soli_cred
                                    AND mg.ind_tipo_garantia = 'H'
                                    AND ( sg.ind_inactivo IS NULL
                                          OR sg.ind_inactivo <> 'S' )
                                    AND mg.ind_tipo_bien = 'P'
                                GROUP BY
                                    sg.cod_soli_cred
                            ) z
                    ) y ON r.cod_soli_cred = y.cod_soli_cred
            ) a
            INNER JOIN (
                SELECT
                    ( ROWNUM - 1 ) AS val_can_anos,
                    n.anio,
                    n.val_mon_conc
                FROM
                    (
                        SELECT
                            a.anio,
                            b.val_mon_conc
                        FROM
                            (
                                SELECT
                                    MIN(x.fec_venc) fec_venc,
                                    EXTRACT(YEAR FROM x.fec_venc) anio
                                FROM
                                    vve_cred_simu_lede x
                                    INNER JOIN vve_cred_simu s ON s.cod_simu = x.cod_simu
                                                                  AND s.ind_inactivo = 'N'
                                WHERE
                                    s.cod_soli_cred = p_cod_soli_cred
                                    AND x.cod_conc_col = 2
                                GROUP BY
                                    EXTRACT(YEAR FROM x.fec_venc)
                            ) a
                            INNER JOIN (
                                SELECT
                                    x.val_mon_conc,
                                    x.fec_venc,
                                    EXTRACT(YEAR FROM x.fec_venc) anio,
                                    x.cod_det_simu,
                                    x.cod_nume_letr,
                                    x.cod_simu
                                FROM
                                    vve_cred_simu_lede x
                                    INNER JOIN vve_cred_simu s ON s.cod_simu = x.cod_simu
                                                                  AND s.ind_inactivo = 'N'
                                WHERE
                                    s.cod_soli_cred = p_cod_soli_cred
                                    AND x.cod_conc_col = 2
                            ) b ON a.anio = b.anio
                                   AND a.fec_venc = b.fec_venc
                        ORDER BY
                            a.anio ASC
                    ) n
            ) n ON n.val_can_anos = a.val_can_anos
        WHERE
            n.anio = val_anio
        ORDER BY
            n.val_can_anos;

        RETURN val_ratio_cobertura;
		EXCEPTION
			WHEN OTHERS THEN 
				 RETURN NULL;

    END fn_ratio_cobertura;

    PROCEDURE sp_list_cred_soli_vo (
        p_cod_cred_soli   IN vve_cred_soli.cod_soli_cred%TYPE,
        p_cod_oper_rel    IN vve_cred_soli.cod_oper_rel%TYPE,
        p_cod_usua_sid    IN sistemas.usuarios.co_usuario%TYPE,
        p_cod_usua_web    IN sistemas.sis_mae_usuario.cod_id_usuario%TYPE,
        p_ret_cursor      OUT SYS_REFCURSOR,
        p_ret_cantidad    OUT NUMBER,
        p_ret_esta        OUT NUMBER,
        p_ret_mens        OUT VARCHAR2
    ) AS

        v_cod_solid_cred       VARCHAR2(20);
        v_cod_clie             VARCHAR2(20);
        v_tipo_finan           VARCHAR2(50);
        v_val_realiz_gara      FLOAT;
        v_val_realiz_gara_adic FLOAT;
        v_asesor_comercial     VARCHAR2(50);
        v_user_aprob           VARCHAR2(100);
        v_jefe_finanzas        VARCHAR(100);
        v_txt_usuario          sis_mae_usuario.txt_usuario%TYPE;
        v_cod_empresa          VARCHAR(50);
        v_fecha_otorga         VARCHAR2(10);
        v_fecha_venc_ult_let   VARCHAR2(10);--< Req. 87567 E2.1 ID## avilca 16/12/2020>
        v_existe_gar_adic      VARCHAR2(3);
        ve_error EXCEPTION;
    BEGIN
        p_ret_cantidad := 1;
        SELECT
            cod_soli_cred,
            cod_clie,
            cod_empr
        INTO
            v_cod_solid_cred,
            v_cod_clie,
            v_cod_empresa
        FROM
            vve_cred_soli
        WHERE
            cod_oper_rel = p_cod_oper_rel; --4371

    --<I Req. 87567 E2.1 ID## avilca 17/09/2020>       
    -- VERIFICAR SI EXISTEN GARANTIAS ADICIONALES

      SELECT
           CASE count(cod_soli_cred) 
            WHEN  0 THEN 'N0' 
            ELSE 'SI' END INTO v_existe_gar_adic
      FROM vve_cred_soli_gara 
       WHERE cod_soli_cred = v_cod_solid_cred 
       AND ind_gara_adic = 'S';

    --VALOR REALIZACION GARANTIAS ADICIONALES

        BEGIN
            SELECT
                nvl(CAST(SUM(ga.val_realiz_gar) AS FLOAT),0)
            INTO v_val_realiz_gara_adic
            FROM
                vve_cred_soli_gara sg,
                vve_cred_maes_gara ga
            WHERE
                sg.cod_gara = ga.cod_garantia
                AND sg.cod_soli_cred = v_cod_solid_cred
                AND sg.ind_gara_adic = 'S';

        EXCEPTION
            WHEN no_data_found THEN
                v_val_realiz_gara_adic := 0;
        END;        
    --<F Req. 87567 E2.1 ID## avilca 17/09/2020> 
    --TIPO FINANCIAMIENTO

        SELECT
            m.descripcion
        INTO v_tipo_finan
        FROM
            vve_tabla_maes m,
            vve_cred_soli s
        WHERE
            s.cod_clie = v_cod_clie --'50547094'--<cod_cliente> 
            AND s.cod_oper_rel = p_cod_oper_rel --'5534'--<nro_operacion> 
            AND m.cod_grupo = 86
            AND m.cod_tipo = s.tip_soli_cred;

    --VALOR REALIZACION GARANTIAS

        BEGIN
            SELECT
                nvl(CAST(SUM(ga.val_realiz_gar) AS FLOAT),0)
            INTO v_val_realiz_gara
            FROM
                vve_cred_soli_gara sg,
                vve_cred_maes_gara ga
            WHERE
                sg.cod_gara = ga.cod_garantia
                AND sg.cod_soli_cred = v_cod_solid_cred;

        EXCEPTION
            WHEN no_data_found THEN
                v_val_realiz_gara := 0;
        END;

    --ASESOR COMERCIAL

        BEGIN
            SELECT
                sa.cod_usua_ejec
            INTO v_asesor_comercial
            FROM
                vve_cred_soli sol,
                vve_cred_soli_acti sa
            WHERE
                sol.cod_soli_cred = sa.cod_soli_cred
                AND sol.cod_soli_cred = v_cod_solid_cred
                AND sa.cod_acti_cred = 'A1';

        EXCEPTION
            WHEN no_data_found THEN
                v_asesor_comercial := NULL;
        END;

    --JEFE FINANZAS

        BEGIN
            SELECT
                txt_usuario
            INTO v_jefe_finanzas
            FROM
                sis_mae_usuario
            WHERE
                cod_id_usuario IN (
                    SELECT
                        cod_id_usua
                    FROM
                        vve_cred_soli_apro
                    WHERE
                        cod_soli_cred = v_cod_solid_cred
                        AND ind_nivel = (
                            SELECT
                                MIN(ind_nivel)
                            FROM
                                vve_cred_soli_apro
                            WHERE
                                cod_soli_cred = v_cod_solid_cred
                        )
                );

        EXCEPTION
            WHEN no_data_found THEN
                v_jefe_finanzas := NULL;
        END;

    --NIVEL DE AUTONOMIA CREDITICIA

        BEGIN
            SELECT
                txt_usuario
            INTO v_user_aprob
            FROM
                sis_mae_usuario
            WHERE
                cod_id_usuario IN (
                    SELECT
                        cod_id_usua
                    FROM
                        vve_cred_soli_apro
                    WHERE
                        cod_soli_cred = v_cod_solid_cred
                        AND ind_nivel = (
                            SELECT
                                MAX(ind_nivel)
                            FROM
                                vve_cred_soli_apro
                            WHERE
                                cod_soli_cred = v_cod_solid_cred
                        )
                );

        EXCEPTION
            WHEN no_data_found THEN
                v_user_aprob := NULL;
        END;

    --FECHA OTORGAMIENTO

        BEGIN
            SELECT
                TO_CHAR(fec_apro_clie,'DD/MM/YYYY')
            INTO v_fecha_otorga
            FROM
                vve_cred_soli
            WHERE
                cod_empr = v_cod_empresa
                AND cod_oper_rel = p_cod_oper_rel;

        EXCEPTION
            WHEN no_data_found THEN
                v_fecha_otorga := NULL;
        END;

   --<I Req. 87567 E2.1 ID## avilca 16/12/2020>
    --FECHA VENC. ÚLTIMA LETRA

        BEGIN
            SELECT
                TO_CHAR( max(f_vence),'DD/MM/YYYY')
            INTO v_fecha_venc_ult_let
            FROM
                arlcml
            WHERE
                      no_cia = v_cod_empresa
                AND cod_oper = p_cod_oper_rel
                AND no_cliente = v_cod_clie;

        EXCEPTION
            WHEN no_data_found THEN
                v_fecha_venc_ult_let := NULL;
        END;        
   --<F Req. 87567 E2.1 ID## avilca 16/12/2020>

        OPEN p_ret_cursor FOR SELECT

            ----------------------------------------------------------------INFORMACION BASICA
                                 sc.cod_soli_cred,
                                 sc.cod_oper_rel,
                                 sc.cod_empr,
                                 gp.nom_perso           AS nomb_cliente,
                                 (
                                     SELECT
                                         des_zona
                                     FROM
                                         vve_mae_zona_filial f
                                         INNER JOIN vve_mae_zona z ON ( f.cod_zona = z.cod_zona )
                                     WHERE
                                         cod_filial = pv.cod_filial
                                 ) AS region,
                                 f.nom_filial           AS sucursal,
                                 sp.can_veh_fin         AS tota_unid_vendidas,
                                 gav.des_area_vta       AS un,
                                 v_fecha_otorga         AS fec_otorga,
                                 ( pd.val_pre_veh * sp.can_veh_fin ) AS val_tota_venta,
                                 gp.num_ruc, --< Req. 87567 E2.1 ID## avilca 17/09/2020>
            ----------------------------------------------------------------PARAMETROS CREDITO
                                 v_tipo_finan || '' AS tipo_finan,--sc.tip_soli_cred AS TIPO_FINAN,
                                 sc.val_porc_tea_sigv   AS tea,
                                 sc.can_plaz_mes        AS plazo,
                                 sc.val_porc_ci         AS cuota_inicial,
                                 sc.val_gasto_admi      AS comis_admini,
                                 DECODE(sc.ind_gps,'S','SI','NO') AS gps_total,
                                 sc.val_mon_fin         AS monto_finan,
                                 sc.fec_venc_1ra_let    AS fec_venci_1letra,
                                 (
                                     SELECT
                                         m.valor_adic_2
                                     FROM
                                         vve_tabla_maes m
                                     WHERE
                                         m.cod_grupo = '88'
                                         AND m.cod_tipo = sc.cod_peri_cred_soli
                                 ) AS perio_gracia,
                                 (
                                     SELECT
                                         m.descripcion
                                     FROM
                                         vve_tabla_maes m
                                     WHERE
                                         m.cod_grupo = '88'
                                         AND m.cod_tipo = sc.cod_peri_cred_soli
                                 ) AS tipo_perio_gracia,
                                 sc.val_prim_seg        AS segu_total
            --,SALDO_TOTAL_PAGAR
                                ,
                                 v_val_realiz_gara      AS val_realiz_gara
                               , v_val_realiz_gara_adic AS val_realiz_gara_adic--< Req. 87567 E2.1 ID## avilca 17/09/2020>
            --,SALDO_CAPITAL_PAGAR
            --,RATIO_COBER_GARANTIAS
            --,NRO_LETRAS_VENCIDAS
                                ,
                                 v_asesor_comercial     AS asesor_comercial
            --,MONTO_DEUDA_VENCIDA
                                ,
                                 v_jefe_finanzas        AS jefe_finanzas --sc.cod_jefe_vtas_apro
            --,DIAS_ATRASO
                                ,
                                 v_user_aprob           AS nivel_autonomia_credi
            --,SANCION_CREDITO
                                ,
                                 pkg_interfaz_sap_sid.get_cod_clie_sap(sc.cod_clie) AS cod_clie_sap,
                                 v_existe_gar_adic  existe_gara_adic,--< Req. 87567 E2.1 ID## avilca 17/09/2020>
                                 v_fecha_venc_ult_let fec_venc_ult_let --< Req. 87567 E2.1 ID## avilca 16/12/2020>
                             FROM
                                 vve_cred_soli sc
                                 INNER JOIN vve_cred_soli_prof sp ON sc.cod_soli_cred = sp.cod_soli_cred
                                 INNER JOIN vve_proforma_veh_det pd ON sp.num_prof_veh = pd.num_prof_veh
                                 INNER JOIN vve_proforma_veh pv ON pv.num_prof_veh = sp.num_prof_veh
                                 INNER JOIN vve_ficha_vta_proforma_veh vpv ON vpv.num_prof_veh = pv.num_prof_veh
                                 INNER JOIN arcgmc em ON em.no_cia = sc.cod_empr
                                 INNER JOIN gen_persona gp ON gp.cod_perso = sc.cod_clie
                                 INNER JOIN gen_area_vta gav ON gav.cod_area_vta = sc.cod_area_vta
                                 INNER JOIN gen_filiales f ON pv.cod_filial = f.cod_filial
                                                              AND pv.cod_sucursal = f.cod_sucursal

                             WHERE
                                 sc.cod_soli_cred = v_cod_solid_cred;

        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';
    EXCEPTION
        WHEN ve_error THEN
            p_ret_esta := 0;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR','SP_LIST_CRED_SOLI_VO',p_cod_usua_sid,'Error en la consulta',p_ret_mens
           ,NULL);
        WHEN OTHERS THEN
            p_ret_esta :=-1;
            p_ret_mens := 'SP_LIST_CRED_SOLI_VO:' || sqlerrm;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR','SP_LIST_CRED_SOLI_VO',p_cod_usua_sid,'Error en la consulta',p_ret_mens
           ,NULL);
    END sp_list_cred_soli_vo;


    PROCEDURE sp_search_cliente_traza (
        p_cod_clie       IN vve_cred_soli.cod_clie%TYPE,
        p_tipo_docu      IN gen_persona.cod_tipo_docu_iden%TYPE,
        p_num_docu       IN gen_persona.num_docu_iden%TYPE,
        p_ratio          IN VARCHAR2,
        p_cia            IN vve_cred_soli.cod_empr%TYPE,
        p_tipo_gara      IN VARCHAR2,
        p_num_ope        IN arlcop.cod_oper%TYPE,  
        p_cod_soli_cred  IN vve_cred_soli_gest_banc.COD_SOLI_CRED%TYPE,
        p_situ_ope       IN VARCHAR2,
        p_fec_mig_ini    IN  VARCHAR2,
        p_fec_mig_fin    IN  VARCHAR2,
        p_cod_usua_sid   IN sistemas.usuarios.co_usuario%TYPE,
        p_ret_cursor     OUT SYS_REFCURSOR,
        p_ret_esta       OUT NUMBER,
        p_ret_mens       OUT VARCHAR2
    ) AS
        ve_error            EXCEPTION;
        
        BEGIN
        
          OPEN p_ret_cursor FOR
            SELECT  DISTINCT
              ( SELECT  LTRIM(c1.cod_clie_sap,'0')  FROM cxc_mae_clie c1 WHERE c1.cod_clie = a.no_cliente ) as cod_clie_sap,
              a.no_cliente as cod_clie
            FROM arlcop a
            INNER JOIN gen_persona gp  ON  a.no_cliente = gp.cod_perso
            LEFT JOIN vve_cred_soli s  ON  a.no_cia = s.cod_empr   AND a.cod_oper = s.cod_oper_rel 
            LEFT JOIN lxc_oper_ratio r ON  r.no_cia = a.no_cia  AND r.cod_oper = a.cod_oper AND r.nivel = 1
            LEFT JOIN vve_cred_maes_gara u ON EXISTS ( SELECT 1 FROM lxc_oper_soli_gara g1 WHERE g1.no_cia = a.no_cia AND  ( g1.cod_oper = a.cod_oper or g1.cod_soli_cred = s.cod_soli_cred ) AND u.cod_garantia = g1.cod_gara )
            WHERE  a.estado = 'A'
              AND  a.fecha_cre_reg > TO_DATE('31122018','DDMMYYYY') 
              AND ( (p_tipo_docu IS NULL AND p_num_docu IS NULL )
                    OR DECODE(p_tipo_docu,'002',gp.num_ruc,'001',gp.num_docu_iden) = p_num_docu )
              AND ( p_cod_clie IS NULL
                    OR a.No_Cliente = p_cod_clie )         
              AND ( p_num_ope IS NULL
                    OR a.cod_oper = p_num_ope )  
              AND ( p_cod_soli_cred IS NULL
                    OR s.cod_soli_cred = p_cod_soli_cred )  
              AND ( nvl(p_ratio,6) = 6 AND (r.val_ratio IS NULL) 
              OR  ( p_ratio = 1 AND (r.val_ratio < 1)  )      
              OR  ( p_ratio = 2 AND (r.val_ratio >= 1   AND r.val_ratio < 1.2 ) )
              OR  ( p_ratio = 3 AND (r.val_ratio >= 1.2 AND r.val_ratio < 1.3 ) )
              OR  ( p_ratio = 4 AND (r.val_ratio >= 1.3 AND r.val_ratio < 1.5 ) ) 
              OR  ( p_ratio = 5 AND (r.val_ratio >= 1.5 )  ) )
              AND ( p_cia IS NULL
                    OR a.no_cia = p_cia )     
              AND ( p_tipo_gara IS NULL
                    OR u.ind_tipo_garantia = p_tipo_gara ) 
              AND ( p_fec_mig_ini IS NULL 
                    OR ( SELECT TRUNC( MAX(i.fec_usua_cre ) ) FROM  lxc_aud_imp_letras i WHERE  i.no_cia = a.no_cia AND i.cod_oper = a.cod_oper AND i.estado = a.estado AND substr(i.tipo_oper,1,11) = 'F-MIGRACION' ) >= p_fec_mig_ini )
              AND ( p_fec_mig_fin IS NULL 
                    OR ( SELECT TRUNC( MAX(i.fec_usua_cre ) ) FROM  lxc_aud_imp_letras i WHERE  i.no_cia = a.no_cia AND i.cod_oper = a.cod_oper AND i.estado = a.estado AND substr(i.tipo_oper,1,11) = 'F-MIGRACION' ) <= p_fec_mig_fin );
                       
                       
        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';
    EXCEPTION
        WHEN ve_error THEN
            p_ret_esta := 0;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR',
                                                'sp_search_cliente_traza',
                                                p_cod_usua_sid,
                                                'Error en la consulta',
                                                p_ret_mens,
                                                NULL);
        WHEN OTHERS THEN
            p_ret_esta := -1;
            p_ret_mens := 'SP_SEARCH_CLIENTE_TRAZA:' || sqlerrm;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR',
                                                'sp_search_cliente_traza',
                                                p_cod_usua_sid,
                                                'Error en la consulta',
                                                p_ret_mens,
                                                NULL);
    END sp_search_cliente_traza;


    PROCEDURE sp_search_soli_traza (
        p_cod_clie       IN vve_cred_soli.cod_clie%TYPE,
        p_tipo_docu      IN gen_persona.cod_tipo_docu_iden%TYPE,
        p_num_docu       IN gen_persona.num_docu_iden%TYPE,
        p_ratio          IN VARCHAR2,
        p_cia            IN vve_cred_soli.cod_empr%TYPE,
        p_tipo_gara      IN VARCHAR2,
        p_num_ope        IN arlcop.cod_oper%TYPE,  
        p_cod_soli_cred  IN vve_cred_soli_gest_banc.COD_SOLI_CRED%TYPE,
        p_situ_ope       IN VARCHAR2,
        p_fec_mig_ini    IN  VARCHAR2,
        p_fec_mig_fin    IN  VARCHAR2,
        p_cod_usua_sid   IN sistemas.usuarios.co_usuario%TYPE,
        p_ret_cursor     OUT SYS_REFCURSOR,
        p_ret_esta       OUT NUMBER,
        p_ret_mens       OUT VARCHAR2
    ) AS
        ve_error            EXCEPTION;
        
        BEGIN
        
           OPEN p_ret_cursor FOR
                 /*   SELECT 
                    'ET. SAN MIGUEL DE LOS ANGELES S.A' as razon_social,
                    '10105584998' as nro_documento,
                    '24/03/2022' as fecha_ope,
                    'Vigente' as situ_ope,
                    '10463394-7' as nro_ope,
                    '10057477-20' as nro_rel_ope,
                    '00000000000000000224' as cod_soli_cred,
                    'Camión' as gara_mob_ajena,
                    50000 as val_gara_mob_ajena,
                    'Bus Interprovincial' as gara_mob_prop,
                    150000 as val_gara_mob_prop,
                    'Terreno 400 m2' as gara_hipo,
                    400000 as valor_gara_hipo,
                    1.3 as ratio_ini,
                    'Recon. de Deuda' as desc_tipo_cred,
                    'TC01' as cod_tipo_cred,
                    12.71 as tea_sin_igv,
                    150000 as val_vta,
                    15000  as total_fn,
                    24 as plazo,
                    12 as cuotas_pagadas,
                    3 as cuotas_vencidas,
                    35000 as saldo_capital,
                    75000 as deuda_por_vencer,
                    10000 as deuda_vencida,
                    25000 as val_gara_actual,
                    1.4 as ratio_actual,
                    70 as mora_max,
                    15 as mora_prom
                  FROM DUAL  */
            SELECT 
              gp.nom_perso           AS  razon_social,
              decode(gp.cod_tipo_perso,'N', gp.num_docu_iden,'J',gp.num_ruc)     AS  nro_documento,
              to_char(a.fecha, 'dd/mm/rrrr') as fecha_ope,
              t.estado as situ_ope,
              s.cod_area_vta AS cod_area_vta, -- CAMPO1 VARCHAR2(20)
              a.cod_oper as nro_ope,
              pkg_soli_ope_traza.fn_obt_ope_pri(a.no_cia, a.cod_oper ) AS nro_rel_ope,
              pkg_soli_ope_traza.fun_obt_cod_ref_op(a.no_cia, a.cod_oper ) AS nro_ope_prima,   --CAMPO2  arlcop.cod_oper%TYPE
              s.cod_soli_cred AS cod_soli_cred,
              pkg_soli_ope_traza.fu_obt_placa_oper(a.no_cia,a.cod_oper,'A')  as gara_mob_ajena, --placa
              pkg_soli_ope_traza.fu_obt_vin_oper(a.no_cia,a.cod_oper,'A')   AS  gara_mob_ajena_vin,   --CAMPO 3 VARCHAR2(100)
              r.val_gm_ajeno as  val_gara_mob_ajena,
              pkg_soli_ope_traza.fu_obt_placa_oper(a.no_cia,a.cod_oper,'P') AS gara_mob_prop,
              pkg_soli_ope_traza.fu_obt_vin_oper(a.no_cia,a.cod_oper,'P')  AS  gara_mob_prop_vin,  --CAMPO 4 VARCHAR2(100)
              r.val_gm_propio as val_gara_mob_prop,
              NULL AS gara_hipo,
              r.val_g_hipotecaria as valor_gara_hipo,
              r.val_ratio as ratio_ini,
              initcap(pkg_soli_ope_traza.fun_obt_tipo_op(a.no_cia,a.cod_oper))  as desc_tipo_cred,
              s.tip_soli_cred    as cod_tipo_cred,     
              pkg_sweb_cred_soli.fn_tasa_sin_igv(a.tea) as tea_sin_igv,
             (
              SELECT SUM(a1.val_vta_tot_fin) 
              FROM vve_cred_soli_prof a1
              WHERE a1.cod_soli_cred = s.cod_soli_cred
                AND a1.ind_inactivo = 'N' ) as val_vta,
             (
              SELECT SUM( d.val_pre_docu ) 
              FROM arfafe d
              WHERE  EXISTS ( 
                SELECT  1  
                FROM ARLCRD  f
                WHERE f.no_cia = d.no_cia
                  AND f.tipo_docu = d.tipo_doc
                  AND f.no_docu = d.no_factu
                  AND f.cod_oper = a.cod_oper
                  AND f.no_cia = a.no_cia  ) ) as  val_vta2,
               a.monto_fina as  total_fn,
               a.no_cuotas as plazo,
               t.cuotas_pagadas as cuotas_pagadas,
               t.cuotas_vencidas as cuotas_vencidas,
               t.capital as saldo_capital,
               t.total_financiar as deuda_por_vencer,
               t.capital as deuda_vencida,
               t.mon_gara_tot as val_gara_actual,
               t.por_ratio_gar as ratio_actual,
               t.dia_venc_max as mora_max,
               t.dia_venc_pro as mora_prom
            FROM arlcop a
            INNER JOIN gen_persona gp  ON  a.no_cliente = gp.cod_perso
            LEFT JOIN vve_cred_soli s  ON  a.no_cia = s.cod_empr   AND a.cod_oper = s.cod_oper_rel 
            LEFT JOIN lxc_oper_ratio r ON  r.no_cia = a.no_cia  AND r.cod_oper = a.cod_oper AND r.nivel = 1
            LEFT JOIN vve_cred_maes_gara u ON EXISTS ( SELECT 1 FROM lxc_oper_soli_gara g1 WHERE g1.no_cia = a.no_cia AND  ( g1.cod_oper = a.cod_oper or g1.cod_soli_cred = s.cod_soli_cred ) AND u.cod_garantia = g1.cod_gara )
            INNER JOIN lxc_clie_movi_temp t ON t.no_cia = a.no_cia AND t.cod_oper = a.cod_oper
            WHERE  a.estado = 'A'  
              AND (  (p_tipo_docu IS NULL AND p_num_docu IS NULL )
                    OR DECODE(p_tipo_docu,'002',gp.num_ruc,'001',gp.num_docu_iden) = p_num_docu )
              AND ( p_cod_clie IS NULL
                    OR a.No_Cliente = p_cod_clie )         
              AND ( p_num_ope IS NULL
                    OR a.cod_oper = p_num_ope )  
              AND ( p_cod_soli_cred IS NULL
                    OR s.cod_soli_cred = p_cod_soli_cred )
              AND ( NVL(p_situ_ope, 'AMBOS') = 'AMBOS'
                    OR t.estado = p_situ_ope )     
              AND ( ( nvl(p_ratio, 6) = 6 AND  r.val_ratio IS NULL )
              OR  ( p_ratio = 1 AND (r.val_ratio < 1)  )      
              OR  ( p_ratio = 2 AND (r.val_ratio >= 1   AND r.val_ratio < 1.2 ) )
              OR  ( p_ratio = 3 AND (r.val_ratio >= 1.2 AND r.val_ratio < 1.3 ) )
              OR  ( p_ratio = 4 AND (r.val_ratio >= 1.3 AND r.val_ratio < 1.5 ) ) 
              OR  ( p_ratio = 5 AND (r.val_ratio >= 1.5 )  ) )
              AND ( p_cia IS NULL
                    OR a.no_cia = p_cia )     
              AND ( p_tipo_gara IS NULL
                    OR u.ind_tipo_garantia = p_tipo_gara ) 
              AND ( p_fec_mig_ini IS NULL 
                    OR ( SELECT TRUNC( MAX(i.fec_usua_cre ) ) FROM  lxc_aud_imp_letras i WHERE  i.no_cia = a.no_cia AND i.cod_oper = a.cod_oper AND i.estado = a.estado AND substr(i.tipo_oper,1,11) = 'F-MIGRACION' ) >= p_fec_mig_ini )
              AND ( p_fec_mig_fin IS NULL 
                    OR ( SELECT TRUNC( MAX(i.fec_usua_cre ) ) FROM  lxc_aud_imp_letras i WHERE  i.no_cia = a.no_cia AND i.cod_oper = a.cod_oper AND i.estado = a.estado AND substr(i.tipo_oper,1,11) = 'F-MIGRACION' ) <= p_fec_mig_fin );
         
                       
        p_ret_esta := 1;
        p_ret_mens := 'La consulta se realizó de manera exitosa';
    EXCEPTION
        WHEN ve_error THEN
            p_ret_esta := 0;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR',
                                                'sp_search_soli_traza',
                                                p_cod_usua_sid,
                                                'Error en la consulta',
                                                p_ret_mens,
                                                NULL);
        WHEN OTHERS THEN
            p_ret_esta := -1;
            p_ret_mens := 'sp_search_soli_traza:' || sqlerrm;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR',
                                                'sp_search_soli_traza',
                                                p_cod_usua_sid,
                                                'Error en la consulta',
                                                p_ret_mens,
                                                NULL);
    END sp_search_soli_traza;
    
    PROCEDURE sp_inse_clie_movi_temp (
        p_cod_clie            IN                    lxc.lxc_clie_movi_temp.no_cliente%TYPE,
        p_cia                 IN                    lxc.lxc_clie_movi_temp.no_cia%TYPE,
        p_cod_oper            IN                    lxc.lxc_clie_movi_temp.cod_oper%TYPE,
        p_modal_cred          IN                    lxc.lxc_clie_movi_temp.modal_cred%TYPE,
        p_cod_moneda          IN                    lxc.lxc_clie_movi_temp.moneda%TYPE,
        p_val_mon_fin         IN                    lxc.lxc_clie_movi_temp.total_financiar%TYPE,
        p_num_cuotas          IN                    lxc.lxc_clie_movi_temp.no_cuotas%TYPE,
        p_tea_sigv            IN                    lxc.lxc_clie_movi_temp.tea_sigv%TYPE,
        p_capital             IN                    lxc.lxc_clie_movi_temp.capital%TYPE,
        p_fec_ven_ult_let     IN                    VARCHAR2,
        p_estado              IN                    lxc.lxc_clie_movi_temp.estado%TYPE,
        p_fecha               IN                    VARCHAR2,
        p_mon_gara_tot        IN                    lxc.lxc_clie_movi_temp.mon_gara_tot%TYPE,
        p_porc_ratio_gar      IN                    lxc.lxc_clie_movi_temp.por_ratio_gar%TYPE,
        p_int_oper            IN                    lxc.lxc_clie_movi_temp.interes_oper%TYPE,
        p_tot_igv             IN                    lxc.lxc_clie_movi_temp.total_igv%TYPE,
        p_tot_isc             IN                    lxc.lxc_clie_movi_temp.total_isc%TYPE,
        p_tipo_cambio         IN                    lxc.lxc_clie_movi_temp.tipo_cambio%TYPE,
        p_plazo               IN                    lxc.lxc_clie_movi_temp.plazo%TYPE,
        p_dia_venc_max        IN                    lxc.lxc_clie_movi_temp.dia_venc_max%TYPE,
        p_dia_venc_pro        IN                    lxc.lxc_clie_movi_temp.dia_venc_pro%TYPE,
        p_cod_usua_sid        IN                    sistemas.usuarios.co_usuario%TYPE,
        p_cod_usua_web        IN                    sistemas.sis_mae_usuario.cod_id_usuario%TYPE,
        p_ret_esta            OUT                   NUMBER,
        p_ret_mens            OUT                   VARCHAR2
    ) AS

        ve_error                  EXCEPTION;
        v_sql_base                VARCHAR2(4000);
        v_txt_msj                 VARCHAR2(200);
        v_dia_venc_max            NUMBER;        
        v_dia_venc_pro            NUMBER(5,2);
        v_mon_gara_tot            lxc_clie_movi_temp.mon_gara_tot%TYPE;
        v_plazo                   NUMBER;
        v_porc_ratio_gar          lxc_clie_movi_temp.por_ratio_gar%TYPE;
        v_nro_sec                 NUMBER;
        v_monto_inicial           arlcml.monto_inicial%TYPE;  
        v_ret_esta                NUMBER;
        v_ret_mens                VARCHAR2(400);   
        

    BEGIN


      BEGIN 
        
        IF p_estado = 'CERRADO' THEN
          v_plazo        := 0;
          v_mon_gara_tot := 0;
          v_porc_ratio_gar := 0;
        
        END IF;  
/*        BEGIN 
          sp_calcula_clie_sap(p_cia, p_cod_oper, p_capital, v_nro_sec, v_monto_inicial , v_dia_venc_pro,v_dia_venc_max, v_mon_gara_tot, v_porc_ratio_gar,  v_ret_esta ,v_ret_mens );    
          IF v_ret_esta > 0 THEN
            NULL;
          END IF;
           
        END;  */   
        
          
        INSERT INTO lxc_clie_movi_temp (
            no_cliente,
            no_cia,
            cod_oper,
            modal_cred,
            moneda,
            total_financiar,
            no_cuotas,
            tea_sigv,
            capital,
            vcto_ult_let,
            estado,
            fecha,
            mon_gara_tot,
            por_ratio_gar,
            interes_oper,
            total_igv,
            total_isc,
            tipo_cambio,
            plazo,
            dia_venc_max,
            dia_venc_pro     
        ) VALUES (
            p_cod_clie,
            p_cia,
            p_cod_oper,
            p_modal_cred,
            p_cod_moneda,
            p_val_mon_fin,
            p_num_cuotas,
            p_tea_sigv,
            p_capital,         
            to_date(p_fec_ven_ult_let,'DD/MM/YYYY'),
            p_estado,
            to_date(p_fecha,'DD/MM/YYYY'),
            v_mon_gara_tot,
            v_porc_ratio_gar,
            p_int_oper,
            p_tot_igv,
            p_tot_isc,
            p_tipo_cambio,
            p_plazo,
            p_dia_venc_max,
            p_dia_venc_pro        
            );

        EXCEPTION
          WHEN ve_error THEN
            v_txt_msj := 'Error al insertar registro';
            p_ret_esta := -1;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR', 'sp_inse_clie_movi_temp', p_cod_usua_sid, v_txt_msj
            , p_ret_mens);
            ROLLBACK;
        END;
        
        COMMIT;
        
        p_ret_esta := 1;
        p_ret_mens := 'Se registró correctamente el movimiento ';

    EXCEPTION
        WHEN ve_error THEN
            p_ret_esta := 0;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR', 'sp_inse_clie_movi_temp', p_cod_usua_sid, 'Error al insertar movimiento'
            , p_ret_mens);
            ROLLBACK;
        WHEN OTHERS THEN
            p_ret_esta := -1;
            p_ret_mens := p_ret_mens||'sp_inse_clie_movi_temp:' || sqlerrm;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR', 'sp_inse_clie_movi_temp', p_cod_usua_sid, 'Error al insertar movimiento'
            , p_ret_mens);
            ROLLBACK;
    END sp_inse_clie_movi_temp; 

    PROCEDURE sp_del_clie_movi_temp (
        p_cod_clie            IN                    lxc.lxc_clie_movi_temp.no_cliente%TYPE,
        p_ret_esta            OUT                   NUMBER,
        p_ret_mens            OUT                   VARCHAR2
    ) AS

      ve_error EXCEPTION;
      v_sql_base             VARCHAR2(4000);
      v_txt_msj              VARCHAR2(200);

    BEGIN

        BEGIN 
          
          DELETE FROM lxc_clie_movi_temp e
          WHERE e.no_cliente = p_cod_clie;
            --AND e.no_cia = p_cia;
 
        EXCEPTION
          WHEN ve_error THEN
            v_txt_msj := 'Error al eliminar registro';
            p_ret_esta := -1;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR', 'lxc_clie_movi_temp', p_cod_clie, v_txt_msj
            , p_ret_mens);
            ROLLBACK;
        END;
        COMMIT;
        
        p_ret_esta := 1;
        p_ret_mens := 'Se elimino correctamente el movimiento ';

    EXCEPTION
        WHEN ve_error THEN
            p_ret_esta := 0;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR', 'sp_inse_clie_movi_temp', p_cod_clie, 'Error al insertar movimiento'
            , p_ret_mens);
            ROLLBACK;
        WHEN OTHERS THEN
            p_ret_esta := -1;
            p_ret_mens := p_ret_mens||'sp_inse_clie_movi_temp:' || sqlerrm;
            pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR', 'sp_inse_clie_movi_temp', p_cod_clie, 'Error al insertar movimiento'
            , p_ret_mens);
            ROLLBACK;
    END sp_del_clie_movi_temp;
    

    PROCEDURE sp_calcula_clie_sap (
      p_cia              IN    vve_cred_soli.cod_empr%TYPE,
      p_num_ope          IN    arlcop.cod_oper%TYPE,  
      p_saldo            IN    arlcml.saldo%TYPE, 
      p_nro_sec          OUT   arlcml.nro_sec%TYPE,
      p_monto_inicial    OUT   arlcml.monto_inicial%TYPE,
      p_dias_prom        OUT   NUMBER,  
      p_dias_max         OUT   NUMBER,  
      p_mon_gara_tot     OUT   lxc.lxc_clie_movi_temp.mon_gara_tot%TYPE,
      p_porc_ratio_ga    OUT   lxc.lxc_clie_movi_temp.por_ratio_gar%TYPE,           
      p_ret_esta         OUT   NUMBER,
      p_ret_mens         OUT   VARCHAR2
      ) 
      AS 
        CURSOR c_letra (c_cod_oper  VARCHAR2, c_no_cia VARCHAR2 ) IS 
          select ax.nro_sec,
               ax.cuota,
               ax.monto_inicial,
               ax.amortizacion,
               ax.f_vence,
               ROUND(trunc(sysdate) - ax.f_vence, 0) dias
          from arlcml ax
          where ax.cod_oper = c_cod_oper
           and no_cia = c_no_cia
          ORDER BY ax.nro_sec DESC;
         
        v_saldo               NUMBER;
        v_nro_sec             NUMBER;
        v_monto_inicial       arlcml.monto_inicial%TYPE;
        v_dias_prom           NUMBER;
        v_dias_sum            NUMBER;
        v_dias_count          NUMBER;
        v_dias_max            NUMBER;  
        v_mon_gara_tot        lxc.lxc_clie_movi_temp.mon_gara_tot%TYPE; 
        v_porc_ratio_ga       lxc.lxc_clie_movi_temp.por_ratio_gar%TYPE; 
         
    BEGIN
      --Calculamos
      v_saldo            := p_saldo;
      v_nro_sec          := 0;
      v_monto_inicial    := 0;
      v_dias_prom        := 0;
      v_dias_sum         := 0;
      v_dias_count       := 0;
      v_dias_max         := 0; 
      v_mon_gara_tot     := 0;  

      FOR i IN c_letra (p_num_ope, p_cia)  LOOP 
    
        v_saldo := v_saldo - i.cuota;
        IF i.dias > 0 THEN
           --determino el maximo día
           IF i.dias > v_dias_max THEN
              v_dias_max := i.dias;
           END IF; 
           
           --acumulamos prom
           v_dias_count := v_dias_count +1;
           v_dias_sum := v_dias_sum +i.dias ;
           v_dias_prom := v_dias_sum/v_dias_count;
           
        END IF;
        
        IF v_saldo <= 0 THEN
           v_nro_sec := i.nro_sec-1;
           v_monto_inicial := i.monto_inicial; 
           
           EXIT;
        
        END IF; 
  
      END LOOP;
      
      
      p_nro_sec          := v_nro_sec      ;
      p_monto_inicial    := v_monto_inicial;
      p_dias_prom        := v_dias_prom    ;
      p_dias_max         := v_dias_max     ;
      
      --Calculamos el ratio de garantia.
      BEGIN
        SELECT nvl(o.val_gm_ajeno, 0)  + nvl(o.val_gm_propio, 0) + nvl(o.val_g_hipotecaria, 0)
        INTO  v_mon_gara_tot
        FROM  lxc_oper_ratio o
        WHERE o.no_cia = p_cia
          AND o.cod_oper = p_num_ope
          AND o.anio_ratio = to_char(SYSDATE,'RRRR');
      EXCEPTION
        WHEN OTHERS THEN 
          v_mon_gara_tot := 0;     
      END;
      
      IF v_mon_gara_tot = 0 THEN
        v_porc_ratio_ga  := 0; 
      
      ELSE
        IF nvl(p_monto_inicial, 0) > 0 THEN
        
          p_porc_ratio_ga := p_mon_gara_tot/p_monto_inicial;

        ELSE
          p_mon_gara_tot   := 0;  
          p_porc_ratio_ga  := 0; 
          
        END IF;  
      END IF;
      
      p_mon_gara_tot := v_mon_gara_tot;
      
   
  
    EXCEPTION
      WHEN OTHERS THEN
          p_ret_esta := -1;
          p_ret_mens := p_ret_mens||'sp_calcula_clie_sap:' || sqlerrm;
          pkg_sweb_mae_gene.sp_regi_rlog_erro('AUDI_ERROR', 'sp_calcula_clie_sap', p_num_ope, 'Error al calcular datos actuales'
          , p_ret_mens);
          ROLLBACK;  
    END sp_calcula_clie_sap;  
  
END pkg_sweb_cred_soli_reportes;
