pro mvn_sta_l2_tplot_c6_kei

common mvn_c6,mvn_c6_ind,mvn_c6_dat

; C6
if size(mvn_c6_dat,/type) eq 8 then begin

  npts = n_elements(mvn_c6_dat.time)
  mode = mvn_c6_dat.mode
  rate = mvn_c6_dat.rate
  iswp = mvn_c6_dat.swp_ind
  ieff = mvn_c6_dat.eff_ind
  iatt = mvn_c6_dat.att_ind
  mlut = mvn_c6_dat.mlut_ind
  twt  = mvn_c6_dat.twt_arr[mlut,*,*]
  nenergy = mvn_c6_dat.nenergy
  nmass = mvn_c6_dat.nmass
  eprom_ver = mvn_c6_dat.eprom_ver

  time = (mvn_c6_dat.time + mvn_c6_dat.end_time)/2.
  data = mvn_c6_dat.data
  energy = reform(mvn_c6_dat.energy[iswp,*,0])
  mass = total(mvn_c6_dat.mass_arr[iswp,*,*],2)/nenergy
  str_element,mvn_c6_dat,'eflux',eflux,success=success
  ;   eflux = mvn_c6_dat.eflux

  cnt_low_nrg=fltarr(npts)
  for i=0l,npts-1 do begin
    ind = where(energy[i,*] le 10.,count)
    if count ge 1 then cnt_low_nrg[i] = total(data[i,ind,*])
  endfor

  ;   this section needed because eflux in the CDFs got screwed up
  bkg = mvn_c6_dat.bkg
  dead = mvn_c6_dat.dead
  gf = reform(mvn_c6_dat.gf[iswp,*,0]*((iatt eq 0)#replicate(1.,nenergy)) +$
    mvn_c6_dat.gf[iswp,*,1]*((iatt eq 1)#replicate(1.,nenergy)) +$
    mvn_c6_dat.gf[iswp,*,2]*((iatt eq 2)#replicate(1.,nenergy)) +$
    mvn_c6_dat.gf[iswp,*,3]*((iatt eq 3)#replicate(1.,nenergy)), npts*nenergy)#replicate(1.,nmass)
  gf = mvn_c6_dat.geom_factor*reform(gf,npts,nenergy,nmass)
  eff = mvn_c6_dat.eff[ieff,*,*]
  dt = mvn_c6_dat.integ_t#replicate(1.,nenergy*nmass)
  eflux2 = (data-bkg)*dead/(gf*eff*dt)
  eflux2 = float(eflux2)
  if success and keyword_set(test) then if max(abs((eflux-eflux2)/(eflux>.01))) gt 0. then print,'Error in CDF c6 eflux ',max(abs((eflux-eflux2)/(eflux>.01)))
  if not success or keyword_set(replace) then eflux = eflux2

  if keyword_set(test) then begin
    store_data,'mvn_sta_c6_gf30_att_kei',data={x:time,y:reform(mvn_c6_dat.gf[iswp,30,*])}
    ylim,'mvn_sta_c6_gf30_att_kei',.01,20,1
    options,'mvn_sta_c6_gf30_att_kei',colors=[cols.blue,cols.green,cols.red,cols.black]
    store_data,'mvn_sta_c6_gf30_kei',data={x:time,y:reform(gf[*,30,1])/mvn_c6_dat.geom_factor}
    ylim,'mvn_sta_c6_gf30_kei',.01,20,1
    store_data,'mvn_sta_c6_iswp_kei',data={x:time,y:iswp}
    store_data,'mvn_sta_c6_iatt_kei',data={x:time,y:[[iatt eq 0],[iatt eq 1],[iatt eq 2],[iatt eq 3]]}
    options,'mvn_sta_c6_iatt_kei',colors=[cols.blue,cols.green,cols.red,cols.black]
    ylim,'mvn_sta_c6_iatt_kei',-1,2,0
  endif


  store_data,'mvn_sta_c6_P1D_E_kei',data={x:time,y:total(data,3),v:energy}
  store_data,'mvn_sta_c6_P1D_M_kei',data={x:time,y:total(data,2),v:mass}
  store_data,'mvn_sta_c6_E_kei',data={x:time,y:total(eflux,3),v:energy}
  store_data,'mvn_sta_c6_M_kei',data={x:time,y:total(eflux,2),v:mass}
  store_data,'mvn_sta_c6_M_twt_kei',data={x:time,y:total(eflux/twt,2),v:mass}
  store_data,'mvn_sta_c6_tot_kei',data={x:time,y:total(total(data,3),2)}
  store_data,'mvn_sta_c6_tot_le_10eV_kei',data={x:time,y:cnt_low_nrg}
  store_data,'mvn_sta_c6_att_kei',data={x:time,y:iatt}
  store_data,'mvn_sta_c6_mode_kei',data={x:time,y:mode}
  store_data,'mvn_sta_c6_rate_kei',data={x:time,y:rate}
  store_data,'mvn_sta_c6_quality_flag_kei',data={x:time,y:mvn_c6_dat.quality_flag}
  options,'mvn_sta_c6_quality_flag_kei',tplot_routine='bitplot',psym = 1,symsize=1
  store_data,'mvn_sta_c6_eprom_ver_kei',data={x:time,y:eprom_ver}
  ylim,'mvn_sta_c6_eprom_ver_kei',min(eprom_ver)-1,max(eprom_ver)+1,0

  ylim,'mvn_sta_c6_tot_kei',0,0,1
  ylim,'mvn_sta_c6_tot_le_10eV_kei',0,0,1
  ylim,'mvn_sta_c6_P1D_E_kei',.1,40000.,1
  ylim,'mvn_sta_c6_P1D_M_kei',.5,100.,1
  ylim,'mvn_sta_c6_E_kei',.1,40000.,1
  ylim,'mvn_sta_c6_M_kei',.5,100.,1
  ylim,'mvn_sta_c6_M_twt_kei',.5,100.,1
  ylim,'mvn_sta_c6_att_kei',-1,4,0
  ylim,'mvn_sta_c6_mode_kei',-1,7,0
  ylim,'mvn_sta_c6_rate_kei',-1,7,0

  zlim,'mvn_sta_c6_P1D_E_kei',1,1.e4,1
  zlim,'mvn_sta_c6_P1D_M_kei',1,1.e4,1
  zlim,'mvn_sta_c6_E_kei',1.e3,1.e9,1
  zlim,'mvn_sta_c6_M_kei',1.e3,1.e9,1
  zlim,'mvn_sta_c6_M_twt_kei',1.e3,1.e9,1

  datagap=7.
  options,'mvn_sta_c6_P1D_E_kei',datagap=datagap
  options,'mvn_sta_c6_P1D_M_kei',datagap=datagap
  options,'mvn_sta_c6_E_kei',datagap=datagap
  options,'mvn_sta_c6_M_kei',datagap=datagap
  options,'mvn_sta_c6_M_twt_kei',datagap=datagap
  options,'mvn_sta_c6_tot_kei',datagap=datagap
  options,'mvn_sta_c6_tot_le_10eV_kei',datagap=datagap
  options,'mvn_sta_c6_att_kei',datagap=datagap

  options,'mvn_sta_c6_P1D_E_kei','spec',1
  options,'mvn_sta_c6_P1D_M_kei','spec',1
  options,'mvn_sta_c6_E_kei','spec',1
  options,'mvn_sta_c6_M_kei','spec',1
  options,'mvn_sta_c6_M_twt_kei','spec',1

  options,'mvn_sta_c6_P1D_E_kei',ytitle='sta!CP1D-c6!C!CEnergy!CeV'
  options,'mvn_sta_c6_P1D_M_kei',ytitle='sta!CP1D-c6!C!CMass!Camu'
  options,'mvn_sta_c6_E_kei',ytitle='sta!Cc6!C!CEnergy!CeV'
  options,'mvn_sta_c6_M_kei',ytitle='sta!Cc6!C!CMass!Camu'
  options,'mvn_sta_c6_M_twt_kei',ytitle='sta!Cc6!C!CMass!Camu'
  options,'mvn_sta_c6_tot_kei',ytitle='sta!Cc6!C!CCounts'
  options,'mvn_sta_c6_tot_le_10eV_kei',ytitle='sta!Cc6!C!C<10eV Cnts'
  options,'mvn_sta_c6_att_kei',ytitle='sta!Cc6!C!CAttenuator'

  options,'mvn_sta_c6_E_kei',ztitle='eflux'
  options,'mvn_sta_c6_M_kei',ztitle='eflux'
  options,'mvn_sta_c6_M_twt_kei',ztitle='eflux/tofbin'
endif

end