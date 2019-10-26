pro rmv_proton_contamination,dat,dat_new
   
   
   apid = dat.apid
   units_name = dat.units_name
   nmass = dat.nmass
   nenergy = dat.nenergy
   ndef = dat.ndef
   nanode = dat.nanode
   mass = dat.mass_arr
   
   dat_new = dat
   
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
   
   data_proton = fltarr(nenergy,ndef*nanode,nmass)
   data_oxy = fltarr(nenergy,ndef*nanode,nmass)
   data_heavy = fltarr(nenergy,ndef*nanode,nmass)
   
   idx_proton = where(mass lt 1.6)
   idx_oxy = where(mass gt 10. and mass lt 50.)
   idx_heavy = where(mass ge 10.)

   data_proton[idx_proton] = dat.data[idx_proton]  ;[32e,64n,8m]
   data_oxy[idx_oxy] = dat.data[idx_oxy]  ;[32e,64n,8m]
   data_heavy[idx_heavy] = dat.data[idx_heavy]  ;[32e,64n,8m]

   data_proton = cmreplicate(total(data_proton,3),nmass)
   data_oxy = cmreplicate(total(data_oxy,3),nmass)
   data_heavy = cmreplicate(total(data_heavy,3),nmass)
   
   data = dat.data - data_proton > 0
   ;data = data_proton
   ;data_proton[idx_proton] = 
   
   ;data = data_proton
   data[idx_proton] = dat.data[idx_proton]
   dat_new.data = data
   


end