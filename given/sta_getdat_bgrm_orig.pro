PRO sta_getdat_bgrm_orig, dat, dat_new, ratio=ratio
  ;; Remove proton straggling background from STATIC data by data(m>1.6amu) - ratio*data(m<1.6amu)
  ;; Works for c6, d0, d1, ce, and cf data
  ;; 
  ;; Input: dat: structure from function mvn_sta_get_**()
  ;; Keywords: ratio: ratio for proton straggling revomal
  ;;                  default value: 0.05 for c6 data, 0.5 for d0/d1/ce/cf
  ;; Output: dat_new: same structure as dat with background removed
  ;; Mar 2015
  
  apid = dat.apid
  units_name = dat.units_name
  nmass = dat.nmass
  nenergy = dat.nenergy
  ndef = dat.ndef
  nanode = dat.nanode
  mass = dat.mass_arr
  
  case apid of
      'c6': 
      'd0':
      'd1':
      'ce':
      'cf':
      else: begin
           print, 'Not valid apid!'
           return
           end
  endcase
  
  if keyword_set(ratio) eq 0 then begin
    if apid eq 'c6' then ratio = 0.05 else ratio = 0.5
  endif
  
  if units_name ne 'counts' and units_name ne 'eflux' $
    then dat_new = conv_units(dat,'eflux') else dat_new = dat
  data = dat_new.data ;; eflux
  
  wp = where(mass lt 1.6) 
  if wp[0] eq -1 then begin
     print, 'Not removing background'
     return
  endif
  
  if apid eq 'c6' then begin
     data_proton = dblarr(nenergy, nmass)
     data_proton[wp] = data[wp]
     data_proton = cmreplicate(total(data_proton,2),nmass)
  endif else begin
     data_proton = dblarr(nenergy, ndef*nanode, nmass)
     data_proton[wp] = data[wp]
     data_proton = cmreplicate(total(data_proton,3),nmass)
  endelse

  data = (data - data_proton*ratio)>0  ; orginal-factor*proton
  data[wp] = dat_new.data[wp]   ;;add original proton data into proton channels which are affected by proton subtraction done above 
  
  dat_new.data = data
  dat_new = conv_units(dat_new, units_name) ;; same units as the input data
END