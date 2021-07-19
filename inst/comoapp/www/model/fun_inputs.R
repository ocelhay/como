inputs <- function(inp, run, times, startdate, stopdate) {
  inp  <- inp %>% rename(Intervention = intervention, 
                         `Date Start` = date_start, 
                         `Date End` = date_end, Value = value,
                         `Apply to` = apply_to)

  # cap intervention start and end dates with simulation end date
  inp$`Date Start` = pmin(stopdate, as.Date(inp$`Date Start`))
  inp$`Date End` = pmin(stopdate, as.Date(inp$`Date End`))
  
  inp$`Date Start` = pmax(startdate, as.Date(inp$`Date Start`))
  inp$`Date End` = pmax(startdate, as.Date(inp$`Date End`))
  
  inp <- inp %>% arrange(`Date Start`)
  # print(inp)
  tv<-which(inp$`Apply to`==run)
  
  si<-intersect(which(inp$Intervention=="Self-isolation if Symptomatic"),tv)
  scr<-intersect(which(inp$Intervention=="(*Self-isolation) Screening"),tv)
  sd<-intersect(which(inp$Intervention=="Social Distancing"),tv)
  hw<-intersect(which(inp$Intervention=="Handwashing"),tv)
  msk<-intersect(which(inp$Intervention=="Mask Wearing"),tv)
  wah<-intersect(which(inp$Intervention=="Working at Home"),tv)
  sc<-intersect(which(inp$Intervention=="School Closures"),tv)
  scp<-intersect(which(inp$Intervention=="Partial School Closures"),tv)
  # scc<-intersect(which(inp$Intervention=="School Group Code"),tv)
  cte<-intersect(which(inp$Intervention=="Shielding the Elderly"),tv)
  q<-intersect(which(inp$Intervention=="(*Self-isolation) Household Isolation"),tv)
  tb<-intersect(which(inp$Intervention=="International Travel Ban"),tv)
  vc<-intersect(which(inp$Intervention=="Vaccination"),tv)
  vcp<-intersect(which(inp$Intervention=="Partial Vaccination"),tv)
  mt<-intersect(which(inp$Intervention=="Mass Testing"),tv)
  dx<-intersect(which(inp$Intervention=="Dexamethasone"),tv)
  pmod<-intersect(which(inp$Intervention=="Transmissibility"),tv)
  dmod<-intersect(which(inp$Intervention=="Lethality"),tv)
  cmod<-intersect(which(inp$Intervention=="Breakthrough infection probability"),tv)
  
  v<-(format(as.POSIXct(inp$`Date Start`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date Start`<-v2
  
  v<-(format(as.POSIXct(inp$`Date End`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date End`<-v2

  ## transmissibility
  f<-c()
  pmod_vector<-c()
  p_mod<-c()
  if (length(pmod)>=1){
    for (i in 1:length(pmod)){
      f<-c(f,as.numeric(inp$`Date Start`[pmod[i]]-startdate),as.numeric(inp$`Date End`[pmod[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[pmod[i]]>startdate){
          pmod_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[pmod[i]],(f[i+1]-f[i])*20))
          p_mod<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          pmod_vector<-c(rep(inp$`Value`[pmod[i]],(f[i+1])*20))
          p_mod<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          pmod_vector<-c(pmod_vector,rep(inp$`Value`[pmod[i]],20))
          p_mod<-c(p_mod,rep(1,20))
        }else{
          pmod_vector<-c(pmod_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          p_mod<-c(p_mod,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        pmod_vector<-c(pmod_vector,rep(inp$`Value`[pmod[i]],(f[i*2]-f[i*2-1])*20))
        p_mod<-c(p_mod,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(pmod) && f[i*2]<tail(times,1)){
        pmod_vector<-c(pmod_vector,rep(0,(tail(times,1)-f[i*2])*20))
        p_mod<-c(p_mod,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    pmod_vector<-rep(0,tail(times,1)*20)
    p_mod<-rep(0,tail(times,1)*20)
  }
  ## lethality
  f<-c()
  dmod_vector<-c()
  d_mod<-c()
  if (length(dmod)>=1){
    for (i in 1:length(dmod)){
      f<-c(f,as.numeric(inp$`Date Start`[dmod[i]]-startdate),as.numeric(inp$`Date End`[dmod[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[dmod[i]]>startdate){
          dmod_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[dmod[i]],(f[i+1]-f[i])*20))
          d_mod<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          dmod_vector<-c(rep(inp$`Value`[dmod[i]],(f[i+1])*20))
          d_mod<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          dmod_vector<-c(dmod_vector,rep(inp$`Value`[dmod[i]],20))
          d_mod<-c(d_mod,rep(1,20))
        }else{
          dmod_vector<-c(dmod_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          d_mod<-c(d_mod,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        dmod_vector<-c(dmod_vector,rep(inp$`Value`[dmod[i]],(f[i*2]-f[i*2-1])*20))
        d_mod<-c(d_mod,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(dmod) && f[i*2]<tail(times,1)){
        dmod_vector<-c(dmod_vector,rep(0,(tail(times,1)-f[i*2])*20))
        d_mod<-c(d_mod,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    dmod_vector<-rep(0,tail(times,1)*20)
    d_mod<-rep(0,tail(times,1)*20)
  }
  ## cross immunity
  f<-c()
  cmod_vector<-c()
  c_mod<-c()
  if (length(cmod)>=1){
    for (i in 1:length(cmod)){
      f<-c(f,as.numeric(inp$`Date Start`[cmod[i]]-startdate),as.numeric(inp$`Date End`[cmod[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[cmod[i]]>startdate){
          cmod_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[cmod[i]],(f[i+1]-f[i])*20))
          c_mod<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          cmod_vector<-c(rep(inp$`Value`[cmod[i]],(f[i+1])*20))
          c_mod<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          cmod_vector<-c(cmod_vector,rep(inp$`Value`[cmod[i]],20))
          c_mod<-c(c_mod,rep(1,20))
        }else{
          cmod_vector<-c(cmod_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          c_mod<-c(c_mod,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        cmod_vector<-c(cmod_vector,rep(inp$`Value`[cmod[i]],(f[i*2]-f[i*2-1])*20))
        c_mod<-c(c_mod,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(cmod) && f[i*2]<tail(times,1)){
        cmod_vector<-c(cmod_vector,rep(0,(tail(times,1)-f[i*2])*20))
        c_mod<-c(c_mod,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    cmod_vector<-rep(0,tail(times,1)*20)
    c_mod<-rep(0,tail(times,1)*20)
  }
  ##  self isolation
  f<-c()
  si_vector<-c()
  isolation<-c()
  if (length(si)>=1){
    for (i in 1:length(si)){
      f<-c(f,as.numeric(inp$`Date Start`[si[i]]-startdate),as.numeric(inp$`Date End`[si[i]]-startdate))
      # print(f)
      if(i==1){
        if (inp$`Date Start`[si[i]]>startdate){
          si_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[si[i]],(f[i+1]-f[i])*20))
          isolation<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          si_vector<-c(rep(inp$`Value`[si[i]],(f[i+1])*20))
          isolation<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          si_vector<-c(si_vector,rep(inp$`Value`[si[i]],20))
          isolation<-c(isolation,rep(1,20))
        }else{
          si_vector<-c(si_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          isolation<-c(isolation,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        si_vector<-c(si_vector,rep(inp$`Value`[si[i]],(f[i*2]-f[i*2-1])*20))
        isolation<-c(isolation,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(si) && f[i*2]<tail(times,1)){
        si_vector<-c(si_vector,rep(0,(tail(times,1)-f[i*2])*20))
        isolation<-c(isolation,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    si_vector<-rep(0,tail(times,1)*20)
    isolation<-rep(0,tail(times,1)*20)
  }
  ## social distancing
  f<-c()
  sd_vector<-c()
  distancing<-c()
  if (length(sd)>=1){
    for (i in 1:length(sd)){
      
      f<-c(f,as.numeric(inp$`Date Start`[sd[i]]-startdate),as.numeric(inp$`Date End`[sd[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[sd[i]]>startdate){
          sd_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sd[i]],(f[i+1]-f[i])*20))
          distancing<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          sd_vector<-c(rep(inp$`Value`[sd[i]],(f[i+1])*20))
          distancing<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],20))
          distancing<-c(distancing,rep(1,20))
        }else{
          sd_vector<-c(sd_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          distancing<-c(distancing,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],(f[i*2]-f[i*2-1])*20))
        distancing<-c(distancing,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(sd)&& f[i*2]<tail(times,1)){
        sd_vector<-c(sd_vector,rep(0,(tail(times,1)-f[i*2])*20))
        distancing<-c(distancing,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    sd_vector<-rep(0,tail(times,1)*20)
    distancing<-rep(0,tail(times,1)*20)
  }
  ## screening
  f<-c()
  scr_vector<-c()
  screen<-c()
  if (length(scr)>=1){
    for (i in 1:length(scr)){
      f<-c(f,as.numeric(inp$`Date Start`[scr[i]]-startdate),as.numeric(inp$`Date End`[scr[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[scr[i]]>startdate){
          scr_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[scr[i]],(f[i+1]-f[i])*20))
          screen<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          scr_vector<-c(rep(inp$`Value`[scr[i]],(f[i+1])*20))
          screen<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],20))
          screen<-c(screen,rep(1,20))
        }else{
          scr_vector<-c(scr_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          screen<-c(screen,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],(f[i*2]-f[i*2-1])*20))
        screen<-c(screen,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(scr)&& f[i*2]<tail(times,1)){
        scr_vector<-c(scr_vector,rep(0,(tail(times,1)-f[i*2])*20))
        screen<-c(screen,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    scr_vector<-rep(0,tail(times,1)*20)
    screen<-rep(0,tail(times,1)*20)
  }
  ## handwashing
  f<-c()
  hw_vector<-c()
  handwash<-c()
  if (length(hw)>=1){
    for (i in 1:length(hw)){
      
      f<-c(f,as.numeric(inp$`Date Start`[hw[i]]-startdate),as.numeric(inp$`Date End`[hw[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[hw[i]]>startdate){
          hw_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[hw[i]],(f[i+1]-f[i])*20))
          handwash<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          hw_vector<-c(rep(inp$`Value`[hw[i]],(f[i+1])*20))
          handwash<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],20))
          handwash<-c(handwash,rep(1,20))
        }else{
          hw_vector<-c(hw_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          handwash<-c(handwash,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],(f[i*2]-f[i*2-1])*20))
        handwash<-c(handwash,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(hw)&& f[i*2]<tail(times,1)){
        hw_vector<-c(hw_vector,rep(0,(tail(times,1)-f[i*2])*20))
        handwash<-c(handwash,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    hw_vector<-rep(0,tail(times,1)*20)
    handwash<-rep(0,tail(times,1)*20)
  }
  ## masking
  f<-c()
  msk_vector<-c()
  masking<-c()
  if (length(msk)>=1){
    for (i in 1:length(msk)){
      
      f<-c(f,as.numeric(inp$`Date Start`[msk[i]]-startdate),as.numeric(inp$`Date End`[msk[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[msk[i]]>startdate){
          msk_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[msk[i]],(f[i+1]-f[i])*20))
          masking<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          msk_vector<-c(rep(inp$`Value`[msk[i]],(f[i+1])*20))
          masking<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          msk_vector<-c(msk_vector,rep(inp$`Value`[msk[i]],20))
          masking<-c(masking,rep(1,20))
        }else{
          msk_vector<-c(msk_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          masking<-c(masking,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        msk_vector<-c(msk_vector,rep(inp$`Value`[msk[i]],(f[i*2]-f[i*2-1])*20))
        masking<-c(masking,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(msk)&& f[i*2]<tail(times,1)){
        msk_vector<-c(msk_vector,rep(0,(tail(times,1)-f[i*2])*20))
        masking<-c(masking,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    msk_vector<-rep(0,tail(times,1)*20)
    masking<-rep(0,tail(times,1)*20)
  }
  ## dexamethasone
  f<-c()
  dex<-c()
  if (length(dx)>=1){
    for (i in 1:length(dx)){
      f<-c(f,as.numeric(inp$`Date Start`[dx[i]]-startdate),as.numeric(inp$`Date End`[dx[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[dx[i]]>startdate){
          dex<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          dex<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          dex<-c(dex,rep(1,20))
        }else{
          dex<-c(dex,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        dex<-c(dex,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(dx)&& f[i*2]<tail(times,1)){
        dex<-c(dex,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    dex<-rep(0,tail(times,1)*20)
  }
  ## working at home
  f<-c()
  wah_vector<-c()
  workhome<-c()
  if (length(wah)>=1){
    for (i in 1:length(wah)){
      
      f<-c(f,as.numeric(inp$`Date Start`[wah[i]]-startdate),as.numeric(inp$`Date End`[wah[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[wah[i]]>startdate){
          wah_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[wah[i]],(f[i+1]-f[i])*20))
          workhome<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          wah_vector<-c(rep(inp$`Value`[wah[i]],(f[i+1])*20))
          workhome<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],20))
          workhome<-c(workhome,rep(1,20))
        }else{
          wah_vector<-c(wah_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          workhome<-c(workhome,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],(f[i*2]-f[i*2-1])*20))
        workhome<-c(workhome,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(wah)&& f[i*2]<tail(times,1)){
        wah_vector<-c(wah_vector,rep(0,(tail(times,1)-f[i*2])*20))
        workhome<-c(workhome,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    wah_vector<-rep(0,tail(times,1)*20)
    workhome<-rep(0,tail(times,1)*20)
  }
  ## school closure
  f<-c()
  sc_vector<-c()
  schoolclose<-c()
  if (length(sc)>=1){
    for (i in 1:length(sc)){
      f<-c(f,as.numeric(inp$`Date Start`[sc[i]]-startdate),as.numeric(inp$`Date End`[sc[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[sc[i]]>startdate){
          sc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sc[i]],(f[i+1]-f[i])*20))
          schoolclose<-c(rep(0,f[i]*20),rep(inp$Target[sc[i]],(f[i+1]-f[i])*20))
        }
        else{
          sc_vector<-c(rep(inp$`Value`[sc[i]],(f[i+1])*20))
          schoolclose<-c(rep(inp$Target[sc[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],20))
          schoolclose<-c(schoolclose,rep(inp$Target[sc[i]],20))
        }else{
          sc_vector<-c(sc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          schoolclose<-c(schoolclose,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],(f[i*2]-f[i*2-1])*20))
        schoolclose<-c(schoolclose,rep(inp$Target[sc[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(sc)&& f[i*2]<tail(times,1)){
        sc_vector<-c(sc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        schoolclose<-c(schoolclose,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    sc_vector<-rep(0,tail(times,1)*20)
    schoolclose<-rep(0,tail(times,1)*20)
  }
  schoolclose[is.na(schoolclose)]<-1
  ## partial school closure
  f<-c()
  scp_vector<-c()
  schoolclosepartial<-c()
  if (length(scp)>=1){
    for (i in 1:length(scp)){
      f<-c(f,as.numeric(inp$`Date Start`[sc[i]]-startdate),as.numeric(inp$`Date End`[scp[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[scp[i]]>startdate){
          scp_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[scp[i]],(f[i+1]-f[i])*20))
          schoolclosepartial<-c(rep(0,f[i]*20),rep(inp$Target[scp[i]],(f[i+1]-f[i])*20))
        }
        else{
          scp_vector<-c(rep(inp$`Value`[scp[i]],(f[i+1])*20))
          schoolclosepartial<-c(rep(inp$Target[scp[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          scp_vector<-c(scp_vector,rep(inp$`Value`[scp[i]],20))
          schoolclosepartial<-c(schoolclosepartial,rep(inp$Target[scp[i]],20))
        }else{
          scp_vector<-c(scp_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          schoolclosepartial<-c(schoolclosepartial,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        scp_vector<-c(scp_vector,rep(inp$`Value`[scp[i]],(f[i*2]-f[i*2-1])*20))
        schoolclosepartial<-c(schoolclosepartial,rep(inp$Target[scp[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(scp)&& f[i*2]<tail(times,1)){
        scp_vector<-c(scp_vector,rep(0,(tail(times,1)-f[i*2])*20))
        schoolclosepartial<-c(schoolclosepartial,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    scp_vector<-rep(0,tail(times,1)*20)
    schoolclosepartial<-rep(0,tail(times,1)*20)
  }
  schoolclosepartial[is.na(schoolclosepartial)]<-1
  ## cocooning the elderly
  f<-c()
  cte_vector<-c()
  cocoon<-c()
  if (length(cte)>=1){
    for (i in 1:length(cte)){
      
      f<-c(f,as.numeric(inp$`Date Start`[cte[i]]-startdate),as.numeric(inp$`Date End`[cte[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[cte[i]]>startdate){
          cte_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[cte[i]],(f[i+1]-f[i])*20))
          cocoon<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          cte_vector<-c(rep(inp$`Value`[cte[i]],(f[i+1])*20))
          cocoon<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],20))
          cocoon<-c(cocoon,rep(1,20))
        }else{
          cte_vector<-c(cte_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          cocoon<-c(cocoon,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],(f[i*2]-f[i*2-1])*20))
        cocoon<-c(cocoon,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(cte)&& f[i*2]<tail(times,1)){
        cte_vector<-c(cte_vector,rep(0,(tail(times,1)-f[i*2])*20))
        cocoon<-c(cocoon,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    cte_vector<-rep(0,tail(times,1)*20)
    cocoon<-rep(0,tail(times,1)*20)
  }
  ## quarantine
  f<-c()
  q_vector<-c()
  quarantine<-c()
  if (length(q)>=1){
    for (i in 1:length(q)){
      
      f<-c(f,as.numeric(inp$`Date Start`[q[i]]-startdate),as.numeric(inp$`Date End`[q[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[q[i]]>startdate){
          q_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[q[i]],(f[i+1]-f[i])*20))
          quarantine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          q_vector<-c(rep(inp$`Value`[q[i]],(f[i+1])*20))
          quarantine<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          q_vector<-c(q_vector,rep(inp$`Value`[q[i]],20))
          quarantine<-c(quarantine,rep(1,20))
        }else{
          q_vector<-c(q_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          quarantine<-c(quarantine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        q_vector<-c(q_vector,rep(inp$`Value`[q[i]],(f[i*2]-f[i*2-1])*20))
        quarantine<-c(quarantine,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(q)&& f[i*2]<tail(times,1)){
        q_vector<-c(q_vector,rep(0,(tail(times,1)-f[i*2])*20))
        quarantine<-c(quarantine,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    q_vector<-rep(0,tail(times,1)*20)
    quarantine<-rep(0,tail(times,1)*20)
  }
  ## travel ban
  f<-c()
  tb_vector<-c()
  travelban<-c()
  if (length(tb)>=1){
    for (i in 1:length(tb)){
      
      f<-c(f,as.numeric(inp$`Date Start`[tb[i]]-startdate),as.numeric(inp$`Date End`[tb[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[tb[i]]>startdate){
          tb_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[tb[i]],(f[i+1]-f[i])*20))
          travelban<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          tb_vector<-c(rep(inp$`Value`[tb[i]],(f[i+1])*20))
          travelban<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],20))
          travelban<-c(travelban,rep(1,20))
        }else{
          tb_vector<-c(tb_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          travelban<-c(travelban,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],(f[i*2]-f[i*2-1])*20))
        travelban<-c(travelban,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(tb)&& f[i*2]<tail(times,1)){
        tb_vector<-c(tb_vector,rep(0,(tail(times,1)-f[i*2])*20))
        travelban<-c(travelban,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    tb_vector<-rep(0,tail(times,1)*20)
    travelban<-rep(0,tail(times,1)*20)
  }
  ## mass testing
  f<-c()
  mt_vector<-c()
  masstesting<-c()
  testage<-c()
  if (length(mt)>=1){
    for (i in 1:length(mt)){
      f<-c(f,as.numeric(inp$`Date Start`[mt[i]]-startdate),as.numeric(inp$`Date End`[mt[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[mt[i]]>startdate){
          mt_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[mt[i]],(f[i+1]-f[i])*20))
          masstesting<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
          testage<-c(rep(0,f[i]*20),rep(inp$`Target`[mt[i]],(f[i+1]-f[i])*20))
        }
        else{
          mt_vector<-c(rep(inp$`Value`[mt[i]],(f[i+1])*20))
          masstesting<-c(rep(1,(f[i+1])*20))
          testage<-c(rep(inp$`Target`[mt[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          mt_vector<-c(mt_vector,rep(inp$`Value`[mt[i]],20))
          masstesting<-c(masstesting,rep(1,20))
          testage<-c(testage,rep(inp$`Target`[mt[i]],20))
        }else{
          mt_vector<-c(mt_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          masstesting<-c(masstesting,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          testage<-c(testage,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        mt_vector<-c(mt_vector,rep(inp$`Value`[mt[i]],(f[i*2]-f[i*2-1])*20))
        masstesting<-c(masstesting,rep(1,(f[i*2]-f[i*2-1])*20))
        testage<-c(testage,rep(inp$`Target`[mt[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(mt)&& f[i*2]<tail(times,1)){
        mt_vector<-c(mt_vector,rep(0,(tail(times,1)-f[i*2])*20))
        masstesting<-c(masstesting,rep(0,(tail(times,1)-f[i*2])*20))
        testage<-c(testage,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    mt_vector<-rep(0,tail(times,1)*20)
    masstesting<-rep(0,tail(times,1)*20)
    testage<-rep(0,tail(times,1)*20)
  }
  
  ## vaccine
  f<-c()
  vc_vector<-c()
  vaccine<-c()
  vaccineage<-c()
  if (length(vc)>=1){
    for (i in 1:length(vc)){
      f<-c(f,as.numeric(inp$`Date Start`[vc[i]]-startdate),as.numeric(inp$`Date End`[vc[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[vc[i]]>startdate){
          vc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vc[i]],(f[i+1]-f[i])*20))
          vaccine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
          vaccineage<-c(rep(0,f[i]*20),rep(inp$`Target`[vc[i]],(f[i+1]-f[i])*20))
        }
        else{
          vc_vector<-c(rep(inp$`Value`[vc[i]],(f[i+1])*20))
          vaccine<-c(rep(1,(f[i+1])*20))
          vaccineage<-c(rep(inp$`Target`[vc[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],20))
          vaccine<-c(vaccine,rep(1,20))
          vaccineage<-c(vaccineage,rep(inp$`Target`[vc[i]],20))
        }else{
          vc_vector<-c(vc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccine<-c(vaccine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccineage<-c(vaccineage,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],(f[i*2]-f[i*2-1])*20))
        vaccine<-c(vaccine,rep(1,(f[i*2]-f[i*2-1])*20))
        vaccineage<-c(vaccineage,rep(inp$`Target`[vc[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(vc)&& f[i*2]<tail(times,1)){
        vc_vector<-c(vc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        vaccine<-c(vaccine,rep(0,(tail(times,1)-f[i*2])*20))
        vaccineage<-c(vaccineage,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    vc_vector<-rep(0,tail(times,1)*20)
    vaccine<-rep(0,tail(times,1)*20)
    vaccineage<-rep(0,tail(times,1)*20)
  }
  
  ## vaccine partial
  f<-c()
  vcp_vector<-c()
  vaccinep<-c()
  vaccineagepartial<-c()
  if (length(vcp)>=1){
    for (i in 1:length(vcp)){
      f<-c(f,as.numeric(inp$`Date Start`[vcp[i]]-startdate),as.numeric(inp$`Date End`[vcp[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[vcp[i]]>startdate){
          vcp_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vcp[i]],(f[i+1]-f[i])*20))
          vaccinep<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
          vaccineagepartial<-c(rep(0,f[i]*20),rep(inp$`Target`[vcp[i]],(f[i+1]-f[i])*20))
        }
        else{
          vcp_vector<-c(rep(inp$`Value`[vcp[i]],(f[i+1])*20))
          vaccinep<-c(rep(1,(f[i+1])*20))
          vaccineagepartial<-c(rep(inp$`Target`[vcp[i]],(f[i+1])*20))
        }
      }
      else{
        if (f[(i-1)*2+1]-f[(i-1)*2]==1){
          vcp_vector<-c(vcp_vector,rep(inp$`Value`[vcp[i]],20))
          vaccinep<-c(vaccinep,rep(1,20))
          vaccineagepartial<-c(vaccineagepartial,rep(inp$`Target`[vcp[i]],20))
        }else{
          vcp_vector<-c(vcp_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccinep<-c(vaccinep,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
          vaccineagepartial<-c(vaccineagepartial,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        }
        vcp_vector<-c(vcp_vector,rep(inp$`Value`[vcp[i]],(f[i*2]-f[i*2-1])*20))
        vaccinep<-c(vaccinep,rep(1,(f[i*2]-f[i*2-1])*20))
        vaccineagepartial<-c(vaccineagepartial,rep(inp$`Target`[vcp[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(vcp)&& f[i*2]<tail(times,1)){
        vcp_vector<-c(vcp_vector,rep(0,(tail(times,1)-f[i*2])*20))
        vaccinep<-c(vaccinep,rep(0,(tail(times,1)-f[i*2])*20))
        vaccineagepartial<-c(vaccineagepartial,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    vcp_vector<-rep(0,tail(times,1)*20)
    vaccinep<-rep(0,tail(times,1)*20)
    vaccineagepartial<-rep(0,tail(times,1)*20)
  }
  
  dmMax <- with(as.list(parameters), {
    1 / max(pdeath_h,pdeath_ho,pdeath_hc,pdeath_hco,pdeath_icu,pdeath_icuo,pdeath_icuc,
            pdeath_icuco,pdeath_vent,pdeath_ventc,pdeath_vent_hc,pdeath_icu_hc,
            pdeath_icu_hco)
  })
  dmod_vector <- pmin(dmMax, dmod_vector)
  
  cmod_vector <- pmin(100, cmod_vector)
  
  return(list(si_vector=si_vector,sd_vector=sd_vector,scr_vector=scr_vector,hw_vector=hw_vector,msk_vector=msk_vector,
              wah_vector=wah_vector,sc_vector=sc_vector,scp_vector=scp_vector,tb_vector=tb_vector,mt_vector=mt_vector*1000,
              cte_vector=cte_vector,q_vector=q_vector,vc_vector=vc_vector,vcp_vector=vcp_vector,isolation=isolation,
              screen=screen,cocoon=cocoon,schoolclose=schoolclose,schoolclosepartial=schoolclosepartial,
              workhome=workhome,handwash=handwash,masking=masking,
              quarantine=quarantine,vaccine=vaccine,vaccinep=vaccinep,travelban=travelban,distancing=distancing,
              pmod=p_mod,pmod_vector=pmod_vector,dmod=d_mod,dmod_vector=dmod_vector,cmod=c_mod,cmod_vector=cmod_vector,
              masstesting=masstesting,testage=testage,vaccineage=vaccineage,vaccineagepartial=vaccineagepartial,dex=dex))
}
