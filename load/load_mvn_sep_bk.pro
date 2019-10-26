PRO load_mvn_sep_bk,trange=trange

  ;+
  ; :Description:
  ;   load SEP data and makes them into tplot vars
  ;
  ; ${parameters}
  ;   trange: time range in time_double()
  ; ${keywords}
  ;
  ; ${Related routines}
  ;
  ; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
  ;
  ; $Last modified
  ;-

nan = !values.f_nan
; SEP
; I might change tplot variables used as a quicklook,
; because the latest procedure to load SEP data creates
; a lot of new tplot variables.
status = EXECUTE("mvn_sep_load, trange=trange");EXECUTE("mvn_sep_load, trange=trange, /L2")
store_data, 'mvn_pfdpu*', /delete, verbose=verbose
store_data, 'mvn_SEP' + ['1F', '1R', '2F', '2R'] + '*', /delete, verbose=verbose
store_data, ['APIDS', 'mvn_DPU_TEMP', 'mvn_SEPS_TEMP', 'mvn_pfp_TEMPS', $
  'mvn_SEPS_hkp_VCMD_CNTR', 'mvn_SEPS_hkp_MEM_CHECKSUM',     $
  'mvn_SEPS_svy_ATT', 'mvn_SEPS_svy_COUNTS_TOTAL', 'mvn_SEPS_svy_ALLTID', $
  'mvn_SEPS_QL', 'mvn_SEP*_QUAL_FLAG'], /delete, verbose=verbose

IF status EQ 0 THEN BEGIN
  septn = 'mvn_sep' + ['1_B-O', '2_B-O', '1_A-F', '2_A-F'] + '_Eflux_Energy'
  store_data, septn[2], data={x: trange, y: REFORM(REPLICATE(nan, 4), [2, 2]), v: [4.3399, 6813.39]}, $
    dlim={yrange: [4.3399, 6813.39], ystyle: 1, ylog: 1, zrange: [1., 1.e5], zstyle: 1, zlog: 1, $
    spec: 1, ztitle: 'keV/s/ster/keV'}
  store_data, septn[0], data={x: trange, y: REFORM(REPLICATE(nan, 4), [2, 2]), v: [4.25304, 6677.02]}, $
    dlim={yrange: [4.25304, 6677.02], ystyle: 1, ylog: 1, zrange: [1., 1.e5], zstyle: 1, zlog: 1, $
    spec: 1, ztitle: 'keV/s/ster/keV'}
  store_data, septn[3], data={x: trange, y: REFORM(REPLICATE(nan, 4), [2, 2]), v: [4.06606, 6383.48]}, $
    dlim={yrange: [4.06606, 6383.48], ystyle: 1, ylog: 1, zrange: [1., 1.e5], zstyle: 1, zlog: 1, $
    spec: 1, ztitle: 'keV/s/ster/keV'}
  store_data, septn[1], data={x: trange, y: REFORM(REPLICATE(nan, 4), [2, 2]), v: [4.13003, 6483.91]}, $
    dlim={yrange: [4.13003, 6483.91], ystyle: 1, ylog: 1, zrange: [1., 1.e5], zstyle: 1, zlog: 1, $
    spec: 1, ztitle: 'keV/s/ster/keV'}
  options, septn, bottom=7, top=254
ENDIF

options, 'mvn_sep1_B-O_Eflux_Energy', ytitle='SEP 1F!CIon', ysubtitle='Energy [keV]', /def
options, 'mvn_sep2_B-O_Eflux_Energy', ytitle='SEP 2F!CIon', ysubtitle='Energy [keV]', /def
options, 'mvn_sep1_A-F_Eflux_Energy', ytitle='SEP 1F!Ce!E-!N', ysubtitle='Energy [keV]', /def
options, 'mvn_sep2_A-F_Eflux_Energy', ytitle='SEP 2F!Ce!E-!N', ysubtitle='Energy [keV]', /def

tname = tnames('mvn_sep*', index=n)
septn = 'mvn_sep' + ['1_B-O', '2_B-O', '1_A-F', '2_A-F'] + '_Eflux_Energy'
septn = tnames(septn, index=m)
;options, septn, panel_size=1., ytickformat='mvn_ql_pfp_tplot_exponent', /def

state = 'idx = WHERE('
FOR i=0, N_ELEMENTS(m)-1 DO BEGIN
  state += '(n eq m[' + string(i, '(I0)') + '])'
  IF i NE N_ELEMENTS(m)-1 THEN state += ' OR '
ENDFOR
undefine, i
state += ', nidx, complement=jdx, ncomplement=njdx)'

status = EXECUTE(state)
IF status EQ 1 THEN IF njdx GT 0 THEN store_data, n[jdx], /delete, verbose=verbose
undefine, idx, jdx, nidx, njdx
undefine, state, status
undefine, septn, tname, n, m


END