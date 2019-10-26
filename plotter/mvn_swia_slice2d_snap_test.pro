;+
; PROCEDURE:
;       mvn_swia_slice2d_snap
; PURPOSE:
;       Plots 2D slice for the times and data types selected by cursor.
;       Hold down the left mouse button and slide for a movie effect.
; CALLING SEQUENCE:
;       mvn_swia_slice2d_snap
; INPUTS:
;       
; KEYWORDS:
;       same as 'slice2d' except...
;       ARCHIVE: Returns archive distribution instead of survey
;       TWINDOW: Specifies tplot window to click (Def. is current window)
;       WINDOW: Specifies window to plot (Def: generates new window)
;       BLINE: Shows magnetic field direction by a black line
; CREATED BY:
;       Yuki Harada on 2014-10-10
;
; $LastChangedBy: haraday $
; $LastChangedDate: 2015-04-23 13:09:09 -0700 (Thu, 23 Apr 2015) $
; $LastChangedRevision: 17407 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/trunk/projects/maven/swia/mvn_swia_slice2d_snap.pro $
;-

pro mvn_swia_slice2d_snap_test, archive=archive, twindow=twindow, window=window, bline=bline, _extra=_extra,xrange=xrange,erange=erange,nosmooth=nosmooth

dsize = get_screen_size()

;- set up windows

;- tplot window (current window)
if keyword_set(twindow) then Twin = twindow else Twin = !d.window

;- slice2d window
if keyword_set(window) then Dwin = window else begin
   window, /free, xsize=dsize[0]/2., ysize=dsize[1]*2./3.,xpos=0., ypos=0.
   Dwin = !d.window
endelse

print, 'Use button 1 to select time; button 3 to quit.'

wset,Twin
;tplot
ctime,t,npoints=1,/silent,vname=vname

ok = 1
while (ok) do begin

   get3d_func = ''
   if strmatch(vname,'*swif*') eq 1 then get3d_func = 'mvn_swia_get_3df'
   if strmatch(vname,'*swic*') eq 1 then get3d_func = 'mvn_swia_get_3dc'

   if get3d_func ne '' then begin
      d = call_function(get3d_func,t,archive=archive)
      if keyword_set(bline) then bdir = d.magf/total(d.magf^2)^.5

      wset,Dwin

      slice2d_test,d, _extra=_extra, sundir=bdir,xrange=xrange,erange=erange,nosmooth=nosmooth

   endif

   wset,Twin
   ctime,t,npoints=1,/silent,vname=vname
   if (data_type(t) eq 5) then ok = 1 else ok = 0
endwhile

end
