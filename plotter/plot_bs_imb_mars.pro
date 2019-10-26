pro plot_bs_imb_mars,trotignon=trotignon,edberg=edberg,xy=xy,yz=yz,xz=xz,cyl=cyl,oplot=oplot


  ; disk
  R_m = 3389.9D
  h = findgen(180)*2*!DTOR
  disk=[[cos(h)],[sin(h)]]

  if keyword_set(trotignon) then begin
  ; paremeters of imb
  X0_imb = 0.64
  e = 0.77
  L = 1.08
  Rss = 1.38
  t = (findgen(230)-115.)*!DTOR
  r = L/(1.+e*cos(t))
  
  Ximb = r*cos(t) + X0_imb
  Yimb = r*sin(t)
  t_term = acos( -X0_imb/(L+e*X0_imb) )
  r_imb = L/(1.+e*cos(t_term))*sin(t_term)
  crcl_imb = circle_v2(0,0,r_imb)
  
  X0_imb_2 = 1.6
  e_2 = 1.009
  L_2 = 0.528
  Rss_2 = 1.44
  t_2 = (findgen(30)+138.5)*!DTOR
  r_2 = L_2/(1.+e_2*cos(t_2))

  Ximb_2 = r_2*cos(t_2) + X0_imb_2
  Yimb_2 = r_2*sin(t_2)
  t_term_2 = acos( -X0_imb_2/(L_2+e_2*X0_imb_2) )
  r_imb_2 = L_2/(1.+e_2*cos(t_term_2))*sin(t_term_2)
  crcl_imb_2 = circle_v2(0,0,r_imb_2)
  

  ; parameters of Bow Shock
  X0_bs = 0.6
  e = 1.026
  L = 2.081
  Rss = 1.63
  t = findgen(360)*!DTOR
  r = L/(1.+e*cos(t))

  Xbs = r*cos(t) + X0_bs
  Ybs = r*sin(t)
  t_term = acos( -X0_bs/(L+e*X0_bs) )
  r_bs = L/(1.+e*cos(t_term))*sin(t_term)
  crcl_bs = circle_v2(0,0,r_bs)

  if keyword_set(xy) then begin
    if not keyword_set(oplot) then plot,[0],/iso,xrange=[3,-3],yrange=[3,-3],xtit='X',ytit='Y',tit=title,charsize=charsize
    oplot,disk[*,0],disk[*,1]
    oplot,Ximb,Yimb,color=80
    oplot,Ximb_2,Yimb_2,color=80
    oplot,Ximb_2,-Yimb_2,color=80
    oplot,Xbs,Ybs,color=230
  endif

  if keyword_set(yz) then begin
    if not keyword_set(oplot) then plot,disk[*,0],disk[*,1],/iso,xrange=[-3,3],yrange=[-3,3],xtit='Y',ytit='Z',tit=title,charsize=charsize
    oplot,crcl_imb[0,*],crcl_imb[1,*],color=80
    oplot,crcl_bs[0,*],crcl_bs[1,*],color=230
  endif

  if keyword_set(xz) then begin
    if not keyword_set(oplot) then plot,disk[*,0],disk[*,1],/iso,xrange=[3,-3],yrange=[-3,3],xtit='X',ytit='Z',tit=title,charsize=charsize
    oplot,disk[*,0],disk[*,1]
    oplot,Ximb,Yimb,color=80
    oplot,Ximb_2,Yimb_2,color=80
    oplot,Ximb_2,-Yimb_2,color=80
    oplot,Xbs,Ybs,color=230
  endif

  if keyword_set(cyl) then begin
    if not keyword_set(oplot) then plot,disk[*,0],disk[*,1],/iso,xrange=[3,-3],yrange=[0,5],xtit='X',ytit='sqrt(Y^2+Z^2)',tit=title,charsize=charsize
    oplot,Ximb,Yimb,color=80
    oplot,Ximb_2,Yimb_2,color=80
    oplot,Xbs,Ybs,color=230
  endif

endif

  if keyword_set(edberg) then begin
    ; paremeters of imb
    X0_imb = 0.86
    e = 0.92
    L = 0.9
    Rss = 1.33
    t = findgen(360)*!DTOR
    r = L/(1.+e*cos(t))

    Ximb = r*cos(t) + X0_imb
    Yimb = r*sin(t)
    t_term = acos( -X0_imb/(L+e*X0_imb) )
    r_imb = L/(1.+e*cos(t_term))*sin(t_term)
    crcl_imb = circle_v2(0,0,r_imb)

    ; parameters of Bow Shock
    X0_bs = 0.55
    e = 1.05
    L = 2.1
    Rss = 1.58
    t = findgen(360)*!DTOR
    r = L/(1.+e*cos(t))

    Xbs = r*cos(t) + X0_bs
    Ybs = r*sin(t)
    t_term = acos( -X0_bs/(L+e*X0_bs) )
    r_bs = L/(1.+e*cos(t_term))*sin(t_term)
    crcl_bs = circle_v2(0,0,r_bs)
    

  if keyword_set(xy) then begin
    if not keyword_set(oplot) then plot,[0],/iso,xrange=[3,-3],yrange=[3,-3],xtit='X',ytit='Y',tit=title,charsize=charsize
    oplot,disk[*,0],disk[*,1]
    oplot,Ximb,Yimb,color=80
    oplot,Xbs,Ybs,color=230
  endif

  if keyword_set(yz) then begin
    if not keyword_set(oplot) then plot,disk[*,0],disk[*,1],/iso,xrange=[-3,3],yrange=[-3,3],xtit='Y',ytit='Z',tit=title,charsize=charsize
    oplot,disk[*,0],disk[*,1]
    oplot,crcl_imb[0,*],crcl_imb[1,*],color=80
    oplot,crcl_bs[0,*],crcl_bs[1,*],color=230
  endif

  if keyword_set(xz) then begin
    if not keyword_set(oplot) then plot,disk[*,0],disk[*,1],/iso,xrange=[3,-3],yrange=[-3,3],xtit='X',ytit='Z',tit=title,charsize=charsize
    oplot,disk[*,0],disk[*,1]
    oplot,Ximb,Yimb,color=80
    oplot,Xbs,Ybs,color=230
  endif

  if keyword_set(cyl) then begin
    if not keyword_set(oplot) then plot,disk[*,0],disk[*,1],/iso,xrange=[3,-3],yrange=[0,5],xtit='X',ytit='sqrt(Y^2+Z^2)',tit=title,charsize=charsize
    oplot,disk[*,0],disk[*,1]
    oplot,Ximb,Yimb,color=80
    oplot,Xbs,Ybs,color=230
  endif

 endif

end