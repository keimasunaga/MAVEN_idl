pro main_mvn_sta_bkg,dat,dat_new

   dat_new = dat
   bg = mvn_sta_bkg_kei(dat)  
   dat_new.data = dat.data-bg

end