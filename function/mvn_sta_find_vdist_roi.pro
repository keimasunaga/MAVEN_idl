function mvn_sta_find_vdist_roi,Vx,Vy



Vx01 = [Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx02 = [Vx[0],Vx[2],Vx[1],Vx[3],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx03 = [Vx[0],Vx[3],Vx[1],Vx[2],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx04 = [Vx[0],Vx[4],Vx[1],Vx[2],Vx[3],Vx[5],Vx[6],Vx[7]]
Vx05 = [Vx[0],Vx[5],Vx[1],Vx[2],Vx[3],Vx[4],Vx[6],Vx[7]]
Vx06 = [Vx[0],Vx[6],Vx[1],Vx[2],Vx[3],Vx[4],Vx[5],Vx[7]]
Vx07 = [Vx[0],Vx[7],Vx[1],Vx[2],Vx[3],Vx[4],Vx[5],Vx[6]]


;Vx10 = [Vx[1],Vx[0],Vx[2],Vx[3],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx12 = [Vx[1],Vx[2],Vx[0],Vx[3],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx13 = [Vx[1],Vx[3],Vx[0],Vx[2],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx14 = [Vx[1],Vx[4],Vx[0],Vx[2],Vx[3],Vx[5],Vx[6],Vx[7]]
Vx15 = [Vx[1],Vx[5],Vx[0],Vx[2],Vx[3],Vx[4],Vx[6],Vx[7]]
Vx16 = [Vx[1],Vx[6],Vx[0],Vx[2],Vx[3],Vx[4],Vx[5],Vx[7]]
Vx17 = [Vx[1],Vx[7],Vx[0],Vx[2],Vx[3],Vx[4],Vx[5],Vx[6]]



;Vx20 = [Vx[2],Vx[0],Vx[1],Vx[3],Vx[4],Vx[5],Vx[6],Vx[7]]
;Vx21 = [Vx[2],Vx[1],Vx[0],Vx[3],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx23 = [Vx[2],Vx[3],Vx[0],Vx[1],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx24 = [Vx[2],Vx[4],Vx[0],Vx[1],Vx[3],Vx[5],Vx[6],Vx[7]]
Vx25 = [Vx[2],Vx[5],Vx[0],Vx[1],Vx[3],Vx[4],Vx[6],Vx[7]]
Vx26 = [Vx[2],Vx[6],Vx[0],Vx[1],Vx[3],Vx[4],Vx[5],Vx[7]]
Vx27 = [Vx[2],Vx[7],Vx[0],Vx[1],Vx[3],Vx[4],Vx[5],Vx[6]]


;Vx30 = [Vx[3],Vx[0],Vx[1],Vx[2],Vx[4],Vx[5],Vx[6],Vx[7]]
;Vx31 = [Vx[3],Vx[1],Vx[0],Vx[2],Vx[4],Vx[5],Vx[6],Vx[7]]
;Vx32 = [Vx[3],Vx[2],Vx[0],Vx[1],Vx[4],Vx[5],Vx[6],Vx[7]]
Vx34 = [Vx[3],Vx[4],Vx[0],Vx[1],Vx[2],Vx[5],Vx[6],Vx[7]]
Vx35 = [Vx[3],Vx[5],Vx[0],Vx[1],Vx[2],Vx[4],Vx[6],Vx[7]]
Vx36 = [Vx[3],Vx[6],Vx[0],Vx[1],Vx[2],Vx[4],Vx[5],Vx[7]]
Vx37 = [Vx[3],Vx[7],Vx[0],Vx[1],Vx[2],Vx[4],Vx[5],Vx[6]]


;Vx40 = [Vx[4],Vx[0],Vx[1],Vx[2],Vx[3],Vx[5],Vx[6],Vx[7]]
;Vx41 = [Vx[4],Vx[1],Vx[0],Vx[2],Vx[3],Vx[5],Vx[6],Vx[7]]
;Vx42 = [Vx[4],Vx[2],Vx[0],Vx[1],Vx[3],Vx[5],Vx[6],Vx[7]]
;Vx43 = [Vx[4],Vx[3],Vx[0],Vx[1],Vx[2],Vx[5],Vx[6],Vx[7]]
Vx45 = [Vx[4],Vx[5],Vx[0],Vx[1],Vx[2],Vx[3],Vx[6],Vx[7]]
Vx46 = [Vx[4],Vx[6],Vx[0],Vx[1],Vx[2],Vx[3],Vx[5],Vx[7]]
Vx47 = [Vx[4],Vx[7],Vx[0],Vx[1],Vx[2],Vx[3],Vx[5],Vx[6]]


;Vx50 = [Vx[5],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[6],Vx[7]]
;Vx51 = [Vx[5],Vx[1],Vx[0],Vx[2],Vx[3],Vx[4],Vx[6],Vx[7]]
;Vx52 = [Vx[5],Vx[2],Vx[0],Vx[1],Vx[3],Vx[4],Vx[6],Vx[7]]
;Vx53 = [Vx[5],Vx[3],Vx[0],Vx[1],Vx[2],Vx[4],Vx[6],Vx[7]]
;Vx54 = [Vx[5],Vx[4],Vx[0],Vx[1],Vx[2],Vx[3],Vx[6],Vx[7]]
Vx56 = [Vx[5],Vx[6],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[7]]
Vx57 = [Vx[5],Vx[7],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[6]]

;Vx60 = [Vx[6],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[5],Vx[7]]
;Vx61 = [Vx[6],Vx[1],Vx[0],Vx[2],Vx[3],Vx[4],Vx[5],Vx[7]]
;Vx62 = [Vx[6],Vx[2],Vx[0],Vx[1],Vx[3],Vx[4],Vx[5],Vx[7]]
;Vx63 = [Vx[6],Vx[3],Vx[0],Vx[1],Vx[2],Vx[4],Vx[5],Vx[7]]
;Vx64 = [Vx[6],Vx[4],Vx[0],Vx[1],Vx[2],Vx[3],Vx[5],Vx[7]]
;Vx65 = [Vx[6],Vx[5],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[7]]
Vx67 = [Vx[6],Vx[7],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[5]]


;Vx70 = [Vx[7],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[5],Vx[6]]
;Vx71 = [Vx[7],Vx[1],Vx[0],Vx[2],Vx[3],Vx[4],Vx[5],Vx[6]]
;Vx72 = [Vx[7],Vx[2],Vx[0],Vx[1],Vx[3],Vx[4],Vx[5],Vx[6]]
;Vx73 = [Vx[7],Vx[3],Vx[0],Vx[1],Vx[2],Vx[4],Vx[5],Vx[6]]
;Vx74 = [Vx[7],Vx[4],Vx[0],Vx[1],Vx[2],Vx[3],Vx[5],Vx[6]]
;Vx75 = [Vx[7],Vx[5],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[6]]
;Vx76 = [Vx[7],Vx[6],Vx[0],Vx[1],Vx[2],Vx[3],Vx[4],Vx[5]]






Vy01 = [Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy02 = [Vy[0],Vy[2],Vy[1],Vy[3],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy03 = [Vy[0],Vy[3],Vy[1],Vy[2],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy04 = [Vy[0],Vy[4],Vy[1],Vy[2],Vy[3],Vy[5],Vy[6],Vy[7]]
Vy05 = [Vy[0],Vy[5],Vy[1],Vy[2],Vy[3],Vy[4],Vy[6],Vy[7]]
Vy06 = [Vy[0],Vy[6],Vy[1],Vy[2],Vy[3],Vy[4],Vy[5],Vy[7]]
Vy07 = [Vy[0],Vy[7],Vy[1],Vy[2],Vy[3],Vy[4],Vy[5],Vy[6]]


;Vy10 = [Vy[1],Vy[0],Vy[2],Vy[3],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy12 = [Vy[1],Vy[2],Vy[0],Vy[3],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy13 = [Vy[1],Vy[3],Vy[0],Vy[2],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy14 = [Vy[1],Vy[4],Vy[0],Vy[2],Vy[3],Vy[5],Vy[6],Vy[7]]
Vy15 = [Vy[1],Vy[5],Vy[0],Vy[2],Vy[3],Vy[4],Vy[6],Vy[7]]
Vy16 = [Vy[1],Vy[6],Vy[0],Vy[2],Vy[3],Vy[4],Vy[5],Vy[7]]
Vy17 = [Vy[1],Vy[7],Vy[0],Vy[2],Vy[3],Vy[4],Vy[5],Vy[6]]



;Vy20 = [Vy[2],Vy[0],Vy[1],Vy[3],Vy[4],Vy[5],Vy[6],Vy[7]]
;Vy21 = [Vy[2],Vy[1],Vy[0],Vy[3],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy23 = [Vy[2],Vy[3],Vy[0],Vy[1],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy24 = [Vy[2],Vy[4],Vy[0],Vy[1],Vy[3],Vy[5],Vy[6],Vy[7]]
Vy25 = [Vy[2],Vy[5],Vy[0],Vy[1],Vy[3],Vy[4],Vy[6],Vy[7]]
Vy26 = [Vy[2],Vy[6],Vy[0],Vy[1],Vy[3],Vy[4],Vy[5],Vy[7]]
Vy27 = [Vy[2],Vy[7],Vy[0],Vy[1],Vy[3],Vy[4],Vy[5],Vy[6]]


;Vy30 = [Vy[3],Vy[0],Vy[1],Vy[2],Vy[4],Vy[5],Vy[6],Vy[7]]
;Vy31 = [Vy[3],Vy[1],Vy[0],Vy[2],Vy[4],Vy[5],Vy[6],Vy[7]]
;Vy32 = [Vy[3],Vy[2],Vy[0],Vy[1],Vy[4],Vy[5],Vy[6],Vy[7]]
Vy34 = [Vy[3],Vy[4],Vy[0],Vy[1],Vy[2],Vy[5],Vy[6],Vy[7]]
Vy35 = [Vy[3],Vy[5],Vy[0],Vy[1],Vy[2],Vy[4],Vy[6],Vy[7]]
Vy36 = [Vy[3],Vy[6],Vy[0],Vy[1],Vy[2],Vy[4],Vy[5],Vy[7]]
Vy37 = [Vy[3],Vy[7],Vy[0],Vy[1],Vy[2],Vy[4],Vy[5],Vy[6]]


;Vy40 = [Vy[4],Vy[0],Vy[1],Vy[2],Vy[3],Vy[5],Vy[6],Vy[7]]
;Vy41 = [Vy[4],Vy[1],Vy[0],Vy[2],Vy[3],Vy[5],Vy[6],Vy[7]]
;Vy42 = [Vy[4],Vy[2],Vy[0],Vy[1],Vy[3],Vy[5],Vy[6],Vy[7]]
;Vy43 = [Vy[4],Vy[3],Vy[0],Vy[1],Vy[2],Vy[5],Vy[6],Vy[7]]
Vy45 = [Vy[4],Vy[5],Vy[0],Vy[1],Vy[2],Vy[3],Vy[6],Vy[7]]
Vy46 = [Vy[4],Vy[6],Vy[0],Vy[1],Vy[2],Vy[3],Vy[5],Vy[7]]
Vy47 = [Vy[4],Vy[7],Vy[0],Vy[1],Vy[2],Vy[3],Vy[5],Vy[6]]


;Vy50 = [Vy[5],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[6],Vy[7]]
;Vy51 = [Vy[5],Vy[1],Vy[0],Vy[2],Vy[3],Vy[4],Vy[6],Vy[7]]
;Vy52 = [Vy[5],Vy[2],Vy[0],Vy[1],Vy[3],Vy[4],Vy[6],Vy[7]]
;Vy53 = [Vy[5],Vy[3],Vy[0],Vy[1],Vy[2],Vy[4],Vy[6],Vy[7]]
;Vy54 = [Vy[5],Vy[4],Vy[0],Vy[1],Vy[2],Vy[3],Vy[6],Vy[7]]
Vy56 = [Vy[5],Vy[6],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[7]]
Vy57 = [Vy[5],Vy[7],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[6]]

;Vy60 = [Vy[6],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[5],Vy[7]]
;Vy61 = [Vy[6],Vy[1],Vy[0],Vy[2],Vy[3],Vy[4],Vy[5],Vy[7]]
;Vy62 = [Vy[6],Vy[2],Vy[0],Vy[1],Vy[3],Vy[4],Vy[5],Vy[7]]
;Vy63 = [Vy[6],Vy[3],Vy[0],Vy[1],Vy[2],Vy[4],Vy[5],Vy[7]]
;Vy64 = [Vy[6],Vy[4],Vy[0],Vy[1],Vy[2],Vy[3],Vy[5],Vy[7]]
;Vy65 = [Vy[6],Vy[5],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[7]]
Vy67 = [Vy[6],Vy[7],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[5]]


;Vy70 = [Vy[7],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[5],Vy[6]]
;Vy71 = [Vy[7],Vy[1],Vy[0],Vy[2],Vy[3],Vy[4],Vy[5],Vy[6]]
;Vy72 = [Vy[7],Vy[2],Vy[0],Vy[1],Vy[3],Vy[4],Vy[5],Vy[6]]
;Vy73 = [Vy[7],Vy[3],Vy[0],Vy[1],Vy[2],Vy[4],Vy[5],Vy[6]]
;Vy74 = [Vy[7],Vy[4],Vy[0],Vy[1],Vy[2],Vy[3],Vy[5],Vy[6]]
;Vy75 = [Vy[7],Vy[5],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[6]]
;Vy76 = [Vy[7],Vy[6],Vy[0],Vy[1],Vy[2],Vy[3],Vy[4],Vy[5]]


Vx_arr = [[Vx01],[Vx02],[Vx03],[Vx04],[Vx05],[Vx06],[Vx07],$
                 [Vx12],[Vx13],[Vx14],[Vx15],[Vx16],[Vx17],$
                        [Vx23],[Vx24],[Vx25],[Vx26],[Vx27],$
                               [Vx34],[Vx35],[Vx36],[Vx37],$
                                      [Vx45],[Vx46],[Vx47],$
                                             [Vx56],[Vx57],$
                                                    [Vx67]]
                                                    
Vy_arr = [[Vy01],[Vy02],[Vy03],[Vy04],[Vy05],[Vy06],[Vy07],$
                 [Vy12],[Vy13],[Vy14],[Vy15],[Vy16],[Vy17],$
                        [Vy23],[Vy24],[Vy25],[Vy26],[Vy27],$
                               [Vy34],[Vy35],[Vy36],[Vy37],$
                                      [Vy45],[Vy46],[Vy47],$
                                             [Vy56],[Vy57],$
                                                    [Vy67]]                                                    


idx_str = ['01234567','02134567','03124567','04123567','05123467','06123457','07123456',$
                      '12034567','13024567','14023567','15023467','16023457','17023456',$
                                 '23014567','24013567','25013467','26013457','27013456',$
                                            '34012567','35012467','36012457','37012456',$
                                                       '45012367','46012357','47012356',$
                                                                  '56012347','57012346',$
                                                                             '67012345'] 

n = 0
for i=0,27 do begin
if i gt 0 then n = n+1
dat = find_poly_inside(Vx_arr[*,i],Vy_arr[*,i])
if dat.result[0] eq 1 and dat.result[1] eq 1 then goto, endroop
endfor
endroop:
idx_in0 = fix(strmid(idx_str[n],0,1))
idx_in1 = fix(strmid(idx_str[n],1,1))
idx_poly0 = fix(strmid(idx_str[n],2+dat.sort_agl[0],1))
idx_poly1 = fix(strmid(idx_str[n],2+dat.sort_agl[1],1))
idx_poly2 = fix(strmid(idx_str[n],2+dat.sort_agl[2],1))
idx_poly3 = fix(strmid(idx_str[n],2+dat.sort_agl[3],1))
idx_poly4 = fix(strmid(idx_str[n],2+dat.sort_agl[4],1))
idx_poly5 = fix(strmid(idx_str[n],2+dat.sort_agl[5],1))
;idx_vertex = indgen(8)
;idx_poly = where(indgen(8) ne idx_0 and indgen(8) ne idx_1)
;print,idx_in0,idx_in1, idx_poly0, idx_poly1,idx_poly2,idx_poly3,idx_poly4,idx_poly5
;stop
return,[idx_poly0, idx_poly1,idx_poly2,idx_poly3,idx_poly4,idx_poly5]
end