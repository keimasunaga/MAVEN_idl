pro mvn_sta_convert_units_kei, data, units, scale=scale

cc3d=findgen(256)

if n_params() eq 0 then return

if strupcase(units) eq strupcase(data.units_name) then return

n_m = data.nmass
n_e = data.nenergy						; number of energies
nbins=data.nbins						; number of bins
energy = data.energy          					; in eV                (n_e,nbins,n_m)
gf = data.geom_factor*data.gf*data.eff
dt = data.integ_t
mass = data.mass*data.mass_arr
dead = data.dead						; dead time array usec for STATIC

case strupcase(data.units_name) of 
;'COMPRESSED' :  scale = 1.d						
'COUNTS' :  scale = 1.d							; 1/sec			
'RATE'   :  scale = 1.d*dt							; 1/sec
'CRATE'  :  scale = 1.d*dt						; 1/sec, corrected for dead time rate
'EFLUX'  :  scale = 1.d*dt*gf 						; eV/cm^2-sec-sr-eV
'FLUX'   :  scale = 1.d*dt*gf * energy					; 1/cm^2-sec-sr-eV
'DF'     :  scale = 1.d*dt*gf * energy^2 * 2./mass/mass*1e5	*1e-3	; sec^3/m^6 ; 1/(cm^3-(km/s)^3)
else: begin
        print,'Unknown starting units: ',data.units_name
	return
      end
endcase

; convert to COUNTS
tmp=data.data
tmp = scale * tmp

; take out dead time correction
if strupcase(data.units_name) ne 'COUNTS' and strupcase(data.units_name) ne 'RATE' then tmp = tmp/dead

scale = 0
case strupcase(units) of
'COMPRESSED' :  scale = 1.
'COUNTS' :  scale = 1.d
'RATE'   :  scale = 1.d/(dt)
'CRATE'  :  scale = 1.d/(dt)
'EFLUX'  :  scale = 1.d/(dt * gf)
'FLUX'   :  scale = 1.d/(dt * gf * energy)
'DF'     :  scale = 1.d/(dt * gf * energy^2 * 2./mass/mass*1e5 ) * 1e-3
else: begin
        message,'Undefined units: '+units
        return
      end
endcase

; dead time correct data if not counts or rate
if strupcase(units) ne 'COUNTS' and strupcase(units) ne 'RATE' then tmp=tmp*dead

; scale to new units
data.units_name = units
if find_str_element(data,'ddata') ge 0 then data.ddata = scale * tmp^.5
data.data = scale * tmp

return
end


