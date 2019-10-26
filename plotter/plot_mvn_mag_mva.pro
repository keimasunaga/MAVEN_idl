pro plot_mvn_mag_mva,tcenter,mag
  
  get_data,'mvn_B_1sec_MAVEN_MSO',data=mag
  ctime,tcenter
  tmin = tcenter - 60d & tmax = tcenter + 60d
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
  
  
  
  
;  IF keyword_set(hodogram) THEN BEGIN
;    IF NOT keyword_set(wnum) THEN wnum = 29 ELSE wnum = wnum
;    IF !d.name NE 'PS'  THEN BEGIN
;      wdelete, wnum
;      wi, wnum, wsize=[800, 475], _extra={title: 'mgs_mag_hodogram'}
;    ENDIF
  
  
  
  
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

  
  
  
  stop

  ;;MVA analysis
  Bxvar = abs(Bxstd/Bxavg)
  Byvar = abs(Bystd/Byavg)
  Bzvar = abs(Bzstd/Bzavg)
  Btvar = Btstd/Btavg
  Bxxavg = avg(Bxyz.y[idx_b-24:idx_b+24,0]*Bxyz.y[idx_b-24:idx_b+24,0],/NAN)
  Byyavg = avg(Bxyz.y[idx_b-24:idx_b+24,1]*Bxyz.y[idx_b-24:idx_b+24,1],/NAN)
  Bzzavg = avg(Bxyz.y[idx_b-24:idx_b+24,2]*Bxyz.y[idx_b-24:idx_b+24,2],/NAN)
  Bxyavg = avg(Bxyz.y[idx_b-24:idx_b+24,0]*Bxyz.y[idx_b-24:idx_b+24,1],/NAN)
  Byzavg = avg(Bxyz.y[idx_b-24:idx_b+24,1]*Bxyz.y[idx_b-24:idx_b+24,2],/NAN)
  Bzxavg = avg(Bxyz.y[idx_b-24:idx_b+24,2]*Bxyz.y[idx_b-24:idx_b+24,0],/NAN)
  Mmtr = [[Bxxavg-Bxavg^2, Bxyavg-Bxavg*Byavg, Bzxavg-Bxavg*Bzavg], $
    [Bxyavg-Byavg*Bxavg, Byyavg-Byavg^2, Byzavg-Byavg*Bzavg], $
    [Bzxavg-Bzavg*Bxavg, Byzavg-Bzavg*Byavg, Bzzavg-Bzavg^2]]
  Lamda = eigenql(Mmtr,eigenvectors=evecs) ;;eigenvalues & eigenvectors
  Bmva = lamda[2]*evecs[*,2]
  if keyword_set(mva) then begin
    Bx=Bmva[0]
    By=Bmva[1]
    Bz=Bmva[2]
  endif


end