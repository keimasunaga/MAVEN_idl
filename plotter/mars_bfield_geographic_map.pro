PRO mars_bfield_geographic_map, silent=silent, ct=ct, iplot=iplot, $
                                xrange=xr, yrange=yr, zrange=zr, $
                                wi=wnum, bt=bt, bp=bp, _extra=extra, $
                                charsize=chrs, erase=erase, wsize=wsize, $
                                xmargin=xmarg, ymargin=ymarg

 @colors_com
 oldct = color_table
 path = '~/work/data/mars/mgs/proxy/'
 file = FILE_SEARCH(path, 'b*_360x180_pc.sav')
 iplot = 1
 IF keyword_set(bt) THEN iplot=2
 IF keyword_set(bp) THEN iplot=0

 restore, file[iplot]
 CASE iplot OF 
    0: BEGIN
       data = bp
       ztit = 'Bp [nT]'
    END 
    1: BEGIN
       data = br
       ztit = 'Br [nT]'
    END 
    2: BEGIN
       data = bt
       ztit = 'Bt [nT]'
    END 
 ENDCASE 

 IF N_ELEMENTS(xr) EQ 2 THEN xr = xr ELSE xr = [0., 360.]
 IF N_ELEMENTS(yr) EQ 2 THEN yr = yr ELSE yr = [-90., 90.]
 IF N_ELEMENTS(zr) EQ 2 THEN zr = zr ELSE zr = [-20., 20.]
 IF N_ELEMENTS(chrs) EQ 1 THEN chrs = chrs ELSE chrs = 1.2
 IF N_ELEMENTS(wnum) EQ 1 THEN wnum = wnum ELSE wnum = 0
 IF N_ELEMENTS(wsize) EQ 2 THEN wsize = wsize ELSE wsize = [780, 460]
 IF N_ELEMENTS(ct) EQ 1 THEN $
    IF (ct LE 43) AND (ct GE 0) THEN ct = ct ELSE ct = 0 $
    ELSE ct = 0
 loadct2, ct, silent=silent
 
 IF keyword_set(erase) OR !d.window EQ -1 THEN BEGIN
    wi, wnum, wsize=wsize
    IF keyword_set(erase) THEN erase
 ENDIF 
 IF !d.name NE 'PS' THEN BEGIN
    IF NOT keyword_set(xmarg) THEN xmarg = [0.1, 0.15]  
    IF NOT keyword_set(ymarg) THEN ymarg = [0.1, 0.05]
 ENDIF 

 xtit = 'East Longitude [deg]'
 ytit = 'Latitude [deg]'
 plotxyz, findgen(360), findgen(180)-90., data, /noisotropic,   $
          xtitle=xtit, ytitle=ytit, ztitle=ztit, _extra=extra,  $
          xticklen=-.02, yticklen=-.02, xrange=xr, yrange=yr,   $
          zrange=zr, xticks=4, yticks=4, xminor=3, yminor=3,    $
          charsize=chrs, xmargin=xmarg, ymargin=ymarg, wi=wnum
 
 loadct2, oldct, silent=silent
END
