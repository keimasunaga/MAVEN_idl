;+
;
;PROCEDURE:       MVN_STA_EMPLOT
;
;PURPOSE:         Wrapper to plot energy-mass distribution of MAVEN
;                 STATIC data.
;
;INPUTS:          Time intervals when you would like to check. If time
;                 intervals are not input, then you should click on
;                 the tplot window via 'ctime'.
;
;KEYWORDS:
;   Many keywords are based on 'contour4d'. Please see 'contour4d'.
;
;CREATED BY:      Takuya Hara on 2014-01-28.
;
;LAST MODIFICATION:
; $LastChangedBy: hara $
; $LastChangedDate: 2015-03-17 04:11:24 +0900 (Tue, 17 Mar 2015) $
; $LastChangedRevision: 247 $
; $URL: svn+ssh://hara@maja.ssl.berkeley.edu/home/hara/work/svn/mypro/trunk/mars/maven/sta/mvn_sta_emplot.pro $
;
;-

PRO mvn_sta_emplot, data=data,tvar, apid=apid, silent=s, verbose=v, window=window,          $
                    limits=limits, units=units, title=title, xtitle=xtitle,       $
                    ytitle=ytitle, ztitle=ztitle, retrace=retrace, vel=vel,       $
                    ncont=ncont, levels=levels, fill=fill, bw=bw, pure=pure,      $
                    rotate=rotate, label=label, xmargin=xmargin, ymargin=ymargin, $
                    points=points, zrange=zrange, xlin=xlin, ylin=ylin, twt=twt,  $
                    mass=mass, charsize=chsz, _extra=extra, default=def,$
                    add_mass_contour=add_mass_contour

  IF SIZE(tvar, /type) NE 0 THEN time = tvar
  IF ~keyword_set(apid) THEN apid = 'c6'
  IF keyword_set(window) THEN wnum = window ELSE wnum = 0
  IF keyword_set(chsz) THEN fch = 1. $
  ELSE BEGIN
     IF !d.name EQ 'X' THEN fch = 1.3 ELSE fch = 1.
     chsz = 1.
  ENDELSE 
  IF keyword_set(s) THEN silent = s ELSE silent = 0
  IF keyword_set(v) THEN verbose = v ELSE verbose = 0
  verbose -= silent

  if not keyword_set(data) then data = mvn_sta_get(apid, tt=time)
  IF keyword_set(def) THEN BEGIN
     points = 1
     label = 1
     fill = 1
     mass = 1
     twt = 1
     units = 'eflux'
  ENDIF 
  !p.charsize = fch * chsz
  
  
  ;IF (!d.name EQ 'X') OR (!d.name EQ 'WIN') THEN wi, wnum, wsize=[600., 500.]
  contour4d_kei, data, limits=limits, units=units, title=title, xtitle=xtitle,  $
    ytitle=ytitle, ztitle=ztitle, retrace=retrace, vel=vel, bw=bw, $
    ncont=ncont, levels=levels, fill=fill, mass=mass, pure=pure,   $
    rotate=rotate, label=label, xmargin=xmargin, ymargin=ymargin,  $
    points=points, zrange=zrange, xlin=xlin, ylin=ylin, twt=twt, _extra=extra,$
    add_mass_contour=add_mass_contour
  !p.charsize = 1.
  RETURN
END

