function mvn_sta_pick_mass,d,mass_range

;  m_arr = d.mass_arr
;  nbins = d.nbins
;  idx = where(m_arr ge mass_range[0] and m_arr le mass_range[1])
;  ind = array_indices(m_arr,idx)
;
;  data2 = fltarr(d.nenergy,d.nbins)
;  sz = size(ind)
;  for  in=0,sz[2]-1 do begin
;
;    data2 = data2 + d.data[ind[0,in],ind[1,in],ind[2,in]]
;
;  endfor


  m_arr = d.mass_arr
  nbins = d.nbins
  idx = where(m_arr ge mass_range[0] and m_arr le mass_range[1])
  ind = array_indices(m_arr,idx)
  data2 = fltarr(d.nenergy, d.nbins, d.nmass)

   sz = size(ind)
   for  in=0,sz[2]-1 do begin 
    data2[ind[0,in],ind[1,in],ind[2,in]] = d.data[ind[0,in],ind[1,in],ind[2,in]]
   endfor
   
  return,data2

end


pro mvn_sta_slice2d_test,mass_range,apid=apid

     if not keyword_set(apid) then apid = 'd1'
     if ~ keyword_set(mass_range) then mass_range = [12,100]
     
     d = mvn_sta_get(apid)
     data2 = mvn_sta_pick_mass(d,mass_range)
    
;     nd = {DATA_NAME: d.DATA_NAME,$
;           VALID: d.VALID,$
;           PROJECT_NAME: d.PROJECT_NAME,$
;           UNITS_NAME: d.UNITS_NAME,$
;           UNITS_PROCEDURE: d.UNITS_PROCEDURE,$
;           TIME: d.TIME, $
;           END_TIME: d.END_TIME, $
;           INTEG_T: d.INTEG_T, $
;           
;           
;           NBINS: d.NBINS, $
;           NENERGY: d.NENERGY, $
;           DATA: vdf, $
;           ENERGY: d.ENERGY[*,*,0],$
;           THETA: d.THETA[*,*,0],$
;           PHI: d.PHI[*,*,0], $
;           DENERGY: d.DENERGY[*,*,0], $
;           DTHETA: d.DTHETA[*,*,0], $
;           DPHI: d.DPHI[*,*,0], $
;           DOMEGA: d.DOMEGA[*,*,0], $
;           EFF: d.EFF[*,*,0], $
;           CHARGE: d.CHARGE, $
;           SC_POT: d.SC_POT, $
;           MAGF: d.MAGF, $
;           MASS: d.MASS, $
;           NMASS: 1.,$
;           GEOM_FACTOR: d.GEOM_FACTOR, $
;           GF: d.GF[*,*,0], $
;           DEAD: d.DEAD, $
;           BINS: d.BINS[*,*,0]}
           
           
           
           
           
           
   nd ={ PROJECT_NAME:   'MAVEN', $
         SPACECRAFT:     '0', $
         DATA_NAME:      'd1 32e4d16a8m', $
         APID:           'd1', $
         UNITS_NAME:     'counts', $
         UNITS_PROCEDURE:'mvn_sta_convert_units', $
         VALID:           1, $
         QUALITY_FLAG:    7176, $
         TIME:            1.4188824e+09, $
         END_TIME:        1.4188824e+09, $
         DELTA_T:         16.000000, $
         INTEG_T:         0.12156250, $
         MODE:            3, $
         RATE:            5, $
         SWP_IND:         11, $
         MLUT_IND:        5, $
         EFF_IND:         0, $
         ATT_IND:         0, $
         NENERGY:         32, $
         ENERGY:         d.ENERGY, $
         DENERGY:        d.DENERGY, $
         NBINS:          64, $
         BINS:           replicate(1,d.NENERGY, d.NBINS), $
         NDEF:           4, $
         NANODE:         16, $
         THETA:          d.THETA, $
         DTHETA:         d.DTHETA, $
         PHI:            d.PHI, $
         DPHI:           d.DPHI, $
         DOMEGA:         d.DOMEGA, $
         GF:             d.GF, $
         EFF:            d.EFF, $
         GEOM_FACTOR:    0.000195673, $
         DEAD1:          420.000, $
         DEAD2:          660.000, $
         DEAD3:          460.000, $
         NMASS:          8, $
         MASS:           0.0104389, $
         MASS_ARR:       d.MASS_ARR, $
         TOF_ARR:        d.TOF_ARR, $
         TWT_ARR:        d.TWT_ARR, $
         CHARGE:         1.00000, $
         SC_POT:         0.00000, $
         MAGF:           d.MAGF, $
         QUAT_SC:        d.QUAT_SC, $
         QUAT_MSO:       d.QUAT_MSO, $
         BINS_SC:        d.BINS_SC, $
         POS_SC_MSO:     d.POS_SC_MSO, $
         BKG:            d.BKG, $
         DEAD:           d.DEAD, $
         DATA:           data2}
stop
        slice2d_test,nd,xrange=[-500,500]
   ; for in = 0,nbins-1 do begin
   ;   idx = where(m_arr[*,in,*] ge mass_range[0] and m_arr[*,in,*] le mass_range[1])
   ;   ind = array_indices(m_arr[*,in,*],idx)
   ;   stop
   ; endfor
  

    
    
  
   

end