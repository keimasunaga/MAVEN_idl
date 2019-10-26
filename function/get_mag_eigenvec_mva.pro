function get_mag_eigenvec_mva,tvar,tcenter=tcenter,dt=dt,trange=trange,hodogram=hodogram
  
 
  get_data,tvar,data=mag
  
  if keyword_set(trange) then begin
    tmin = trange[0]
    tmax = trange[1]
  endif
  
  if keyword_set(tcenter) then begin
    if not keyword_set(dt) then dt=60d
    tmin = tcenter - dt & tmax = tcenter + dt
  endif
  
  idx = WHERE((mag.x GE tmin AND mag.x LE tmax), ndat)
  bx = mag.y[idx,0]  &  by = mag.y[idx,1]  &  bz = mag.y[idx,2]
  time = mag.x[idx]
  dt = time[ndat-1] - time[0]
  
  bx = interpol(bx,time,time,/NaN)
  by = interpol(by,time,time,/NaN)
  bz = interpol(bz,time,time,/NaN)
  
  
  matrix = DBLARR(3, 3)
  matrix[0, 0] = (TOTAL(bx*bx) / ndat) - (TOTAL(bx) / ndat) * (TOTAL(bx) / ndat)
  matrix[1, 1] = (TOTAL(by*by) / ndat) - (TOTAL(by) / ndat) * (TOTAL(by) / ndat)
  matrix[2, 2] = (TOTAL(bz*bz) / ndat) - (TOTAL(bz) / ndat) * (TOTAL(bz) / ndat)
  matrix[0, 1] = (TOTAL(bx*by) / ndat) - (TOTAL(bx) / ndat) * (TOTAL(by) / ndat)
  matrix[1, 2] = (TOTAL(by*bz) / ndat) - (TOTAL(by) / ndat) * (TOTAL(bz) / ndat)
  matrix[2, 0] = (TOTAL(bz*bx) / ndat) - (TOTAL(bz) / ndat) * (TOTAL(bx) / ndat)
  matrix[1, 0] = matrix[0, 1]
  matrix[2, 1] = matrix[1, 2]
  matrix[0, 2] = matrix[2, 0]
  
  evals = EIGENQL(matrix, eigenvectors=evecs, /double)
  ;IF (NOT keyword_set(hodogram)) AND (NOT keyword_set(tplot)) THEN RETURN

  bmso = [ [bx], [by], [bz] ]
  bmva = DBLARR(ndat, 3)
  
  FOR i=0, ndat-1 DO bmva[i, *] = TRANSPOSE(evecs ## bmso[i, *])  
  store_data,'Bijk',data={x:mag.x[idx],y:bmva}
  stop
  if keyword_set(hodogram) then begin
  color=1
  IF NOT keyword_set(color) THEN BEGIN
    !p.multi = [0, 2, 1]
    plot, bmva[*, 0], bmva[*, 1], xtitle='Bi [nT]', ytitle='Bj [nT]'
    plot, bmva[*, 0], bmva[*, 2], xtitle='Bi [nT]', ytitle='Bk [nT]'
    !p.multi = 0
    stop
  ENDIF ELSE BEGIN
    plot, minmax(bmva[*, 0]), minmax(bmva[*, 1]), /nodata, pos=[0.075, 0.275, 0.475, 0.95], $
      xtitle='Bi [nT]', ytitle='Bj [nT]', _extra=extra
    ncol = floor((248.d0 / (ndat-1)) * indgen(ndat-1)) + 7
    FOR i=0, ndat-2 DO $
      oplot, bmva[i:i+1L, 0], bmva[i:i+1L, 1], color=ncol[i]

    plot, minmax(bmva[*, 0]), minmax(bmva[*, 2]), /nodata, pos=[0.575, 0.275, 0.975, 0.95], $
      xtitle='Bi [nT]', ytitle='Bk [nT]', /noerase, _extra=extra
    FOR i=0, ndat-2 DO $
      oplot, bmva[i:i+1L, 0], bmva[i:i+1L, 2], color=ncol[i]

    xtickname = strarr(4)
    FOR i=0, 3 DO xtickname[i] = STRMID(time_string(time[NN(time, time[0]+(dt/3.d0)*i)]), 11, 8)
    xtickname[3] = xtickname[3] + '    '
    xtitle = STRMID(time_string(MEAN(time)), 0, 10)
    draw_color_scale, pos=[0.075, 0.125, 0.975, 0.15], range=[0, 1], /horizontal, $
      _extra={xticks: 3, xtickname: xtickname}, title=xtitle
  ENDELSE
  endif
  
   return,evecs

end