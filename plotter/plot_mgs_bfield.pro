;+
; :Description:
;    Purpose of this routine is to draw a map of the Martian crustal field
;    observed by MGS in the Martian Mercator map [360x180]
;
; ${parameters}
;
; ${keywords}
; bphi/btheta: plot Bphi/Btheta instead of Br 
; mult: set in the first plot if you plot maltiple maps in the same panel like mult = '2,2'
; add:  set from the second plot if you plot maltiple maps in the same panel
; 
; ${Related routines}
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified May 13, 2014
;-

pro plot_mgs_bfield,bphi=bphi,btheta=btheta,mult=mult,add=add,title=title

  env = init_env()
  SAVE_LOC = env.SAVE_LOC
  
  ;; load crustal field data
  path = SAVE_LOC + '/maven/sav/bfield_map/'
  file = FILE_SEARCH(path, 'b*_360x180_pc.sav')
  filename = file[1]
  if keyword_set(bphi) then filename = file[0]
  if keyword_set(btheta) then filename = file[2] 
  restore,filename
  
  ;; plot a field map
  loadct2,70,file='/Applications/exelis/idl84/resource/colors/colors2.tbl',/reverse
  if ~keyword_set(bphi) and ~keyword_set(btheta) then plotxyz,findgen(360),findgen(180)-90.,br,zrange=[-70,70],mult=mult,add=add,title=title
  if keyword_set(bphi) then plotxyz,findgen(360),findgen(180)-90.,bp,zrange=[-50,50],mult=mult,add=add,title=title
  if keyword_set(btheta) then plotxyz,findgen(360),findgen(180)-90.,bt,zrange=[-50,50],mult=mult,add=add,title=title
  loadct2,39

end