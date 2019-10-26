PRO sta_c6_bgrm, counts, eflux, ratio = ratio, overwrite = overwrite
;; Remove proton straggling background from STATIC c6 data by data(m>2.5amu) - ratio*data(m<2.5amu)
;; Input: c6 data from common block. Must load c6 data first.
;; Keywords: ratio: 2-element, the ratios for proton straggling revomal for counts and eflux, 
;;                  default value: [0.02, 0.02]
;;           overwrite: if set, overwrite c6 data in common block
;; Output: counts: npts x nenergy(32) x nmass(64) array, proton straggling removed
;;         eflux: npts x nenergy(32) x nmass(64) array
;; Mar 2015


    if keyword_set(ratio) eq 0 then begin
        ratio = [0.02, 0.02]
    endif else begin
        if n_elements(ratio) eq 1 then ratio = replicate(ratio, 2)
    endelse
    
    ratio_counts = ratio[0]
    ratio_eflux = ratio[1]
    
    common mvn_c6, mvn_c6_ind, mvn_c6_dat
    
    npts = n_elements(mvn_c6_dat.time)
    ;mode = mvn_c6_dat.mode
    ;rate = mvn_c0_dat.rate
    iswp = mvn_c6_dat.swp_ind
    ;ieff = mvn_c0_dat.eff_ind  
    ;iatt = mvn_c0_dat.att_ind
    mlut = mvn_c6_dat.mlut_ind
    nenergy = mvn_c6_dat.nenergy
    nmass = mvn_c6_dat.nmass
    
    ;time = (mvn_c6_dat.time + mvn_c6_dat.end_time)/2.
    ;energy = reform(mvn_c6_dat.energy[iswp,*,0])
    ;mass = total(mvn_c6_dat.mass_arr[mlut,*,*],2)/nenergy
    ;energy = reform(mvn_c6_dat.energy[iswp,*,*])
    mass = mvn_c6_dat.mass_arr[iswp, *, *];; npts x nenergy x nmass
    
    counts = mvn_c6_dat.data
    eflux = mvn_c6_dat.eflux

    counts_proton = dblarr(npts, nenergy, nmass)
    eflux_proton = dblarr(npts, nenergy, nmass)
        
    wp = where(mass lt 2.5)

    if wp[0] ne -1 then begin
       
       eflux_proton[wp] = eflux[wp]
       counts_proton[wp] = counts[wp]
       eflux_proton = cmreplicate(total(eflux_proton, 3), nmass)
       counts_proton = cmreplicate(total(counts_proton, 3), nmass)
       
       eflux  = (eflux - ratio_eflux*eflux_proton)>0.
       counts = (counts - ratio_counts*counts_proton)>0.
       
       eflux[wp] = mvn_c6_dat.eflux[wp]
       counts[wp] = mvn_c6_dat.data[wp]
   
   endif else begin
       print, 'Not removing background.'
   endelse     
    
   if keyword_set(overwrite) then begin
      mvn_c6_dat.data = counts
      mvn_c6_dat.eflux = eflux
      print, 'Data in common block overwritten!'
   endif
END