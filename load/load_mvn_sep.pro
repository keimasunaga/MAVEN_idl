PRO load_mvn_sep,trange=trange,tplot=tplot

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
status = EXECUTE("mvn_sep_load, trange=trange,/L1")
;calc,'"MVN_SEP1F_ION_EFLUX"="MVN_SEP1F_ION_FLUX"*"MVN_SEP1F_ION_ENERGY"'
;calc,'"MVN_SEP1R_ION_EFLUX"="MVN_SEP1R_ION_FLUX"*"MVN_SEP1R_ION_ENERGY"'
;calc,'"MVN_SEP2F_ION_EFLUX"="MVN_SEP2F_ION_FLUX"*"MVN_SEP2F_ION_ENERGY"'
;calc,'"MVN_SEP2R_ION_EFLUX"="MVN_SEP2R_ION_FLUX"*"MVN_SEP2R_ION_ENERGY"'
;calc,'"MVN_SEP1F_ELEC_EFLUX"="MVN_SEP1F_ELEC_FLUX"*"MVN_SEP1F_ELEC_ENERGY"'
;calc,'"MVN_SEP1R_ELEC_EFLUX"="MVN_SEP1R_ELEC_FLUX"*"MVN_SEP1R_ELEC_ENERGY"'
;calc,'"MVN_SEP2F_ELEC_EFLUX"="MVN_SEP2F_ELEC_FLUX"*"MVN_SEP2F_ELEC_ENERGY"'
;calc,'"MVN_SEP2R_ELEC_EFLUX"="MVN_SEP2R_ELEC_FLUX"*"MVN_SEP2R_ELEC_ENERGY"'

options,'mvn_SEP*ion_flux','spec',1
ylim,'mvn_SEP*ion_flux',10,10000,1
zlim,'mvn_SEP*ion_flux',1,10000,1

options,'mvn_SEP*elec_flux','spec',1
ylim,'mvn_SEP*elec_flux',10,200,1
zlim,'mvn_SEP*elec_flux',1,100,1

options,'mvn_SEP*ion_eflux','spec',1
ylim,'mvn_SEP*ion_eflux',10,6700,1
zlim,'mvn_SEP*ion_eflux',1,100000,1

options,'mvn_SEP*elec_eflux','spec',1
ylim,'mvn_SEP*elec_eflux',10,200,1
zlim,'mvn_SEP*elec_eflux',1,100000,1
if keyword_set(tplot) then tplot,['mvn_SEP*ion_eflux','mvn_SEP*elec_eflux']


END