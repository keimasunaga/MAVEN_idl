PRO load_mvn_sep_bk2,trange=trange,tplot=tplot

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
status = EXECUTE("mvn_sep_load, trange=trange,/L2")
calc,'"MVN_SEP1F_ION_EFLUX"="MVN_SEP1F_ION_FLUX"*"MVN_SEP1F_ION_ENERGY"'
calc,'"MVN_SEP1R_ION_EFLUX"="MVN_SEP1R_ION_FLUX"*"MVN_SEP1R_ION_ENERGY"'
calc,'"MVN_SEP2F_ION_EFLUX"="MVN_SEP2F_ION_FLUX"*"MVN_SEP2F_ION_ENERGY"'
calc,'"MVN_SEP2R_ION_EFLUX"="MVN_SEP2R_ION_FLUX"*"MVN_SEP2R_ION_ENERGY"'
calc,'"MVN_SEP1F_ELEC_EFLUX"="MVN_SEP1F_ELEC_FLUX"*"MVN_SEP1F_ELEC_ENERGY"'
calc,'"MVN_SEP1R_ELEC_EFLUX"="MVN_SEP1R_ELEC_FLUX"*"MVN_SEP1R_ELEC_ENERGY"'
calc,'"MVN_SEP2F_ELEC_EFLUX"="MVN_SEP2F_ELEC_FLUX"*"MVN_SEP2F_ELEC_ENERGY"'
calc,'"MVN_SEP2R_ELEC_EFLUX"="MVN_SEP2R_ELEC_FLUX"*"MVN_SEP2R_ELEC_ENERGY"'

options,'MVN_SEP*ION_FLUX','spec',1
ylim,'MVN_SEP*ION_FLUX',10,10000,1
zlim,'MVN_SEP*ION_FLUX',1,10000,1

options,'MVN_SEP*ELEC_FLUX','spec',1
ylim,'MVN_SEP*ELEC_FLUX',10,200,1
zlim,'MVN_SEP*ELEC_FLUX',1,100,1

options,'MVN_SEP*ION_EFLUX','spec',1
ylim,'MVN_SEP*ION_EFLUX',10,6700,1
zlim,'MVN_SEP*ION_EFLUX',1,100000,1

options,'MVN_SEP*ELEC_EFLUX','spec',1
ylim,'MVN_SEP*ELEC_EFLUX',10,200,1
zlim,'MVN_SEP*ELEC_EFLUX',1,100000,1
if keyword_set(tplot) then tplot,['MVN_SEP*ION_EFLUX','MVN_SEP*ELEC_EFLUX']


END