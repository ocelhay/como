inputs <- function(inp, run, times, startdate, stopdate){
  # cap intervention end dates with simulation end date
  inp$`Date End` = pmin(stopdate, inp$`Date End`)
  
  tab<-which(inp$`Apply to`==run)
  
  si<-intersect(which(inp$Intervention=="Self-isolation if Symptomatic"),tab)
  scr<-intersect(which(inp$Intervention=="Screening (when S.I.)"),tab)
  sd<-intersect(which(inp$Intervention=="Social Distancing"),tab)
  hw<-intersect(which(inp$Intervention=="Handwashing"),tab)
  wah<-intersect(which(inp$Intervention=="Working at Home"),tab)
  sc<-intersect(which(inp$Intervention=="School Closures"),tab)
  cte<-intersect(which(inp$Intervention=="Shielding the Elderly"),tab)
  q<-intersect(which(inp$Intervention=="Household Isolation (when S.I.)"),tab)
  tb<-intersect(which(inp$Intervention=="International Travel Ban"),tab)
  vc<-intersect(which(inp$Intervention=="Vaccination"),tab)
  
  v<-(format(as.POSIXct(inp$`Date Start`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date Start`<-v2
  
  v<-(format(as.POSIXct(inp$`Date End`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date End`<-v2
  
  ##  self isolation
  f<-c()
  si_vector<-c()
  isolation<-c()
  if (length(si)>=1){
    for (i in 1:length(si)){
      f<-c(f,as.numeric(inp$`Date Start`[si[i]]-startdate),as.numeric(inp$`Date End`[si[i]]-startdate))
      
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
        si_vector<-c(si_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        si_vector<-c(si_vector,rep(inp$`Value`[si[i]],(f[i*2]-f[i*2-1])*20))
        isolation<-c(isolation,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        sd_vector<-c(sd_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],(f[i*2]-f[i*2-1])*20))
        distancing<-c(distancing,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        scr_vector<-c(scr_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],(f[i*2]-f[i*2-1])*20))
        screen<-c(screen,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        hw_vector<-c(hw_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],(f[i*2]-f[i*2-1])*20))
        handwash<-c(handwash,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        wah_vector<-c(wah_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],(f[i*2]-f[i*2-1])*20))
        workhome<-c(workhome,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
          schoolclose<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          sc_vector<-c(rep(inp$`Value`[sc[i]],(f[i+1])*20))
          schoolclose<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        sc_vector<-c(sc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],(f[i*2]-f[i*2-1])*20))
        schoolclose<-c(schoolclose,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        schoolclose<-c(schoolclose,rep(1,(f[i*2]-f[i*2-1])*20))
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
        cte_vector<-c(cte_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],(f[i*2]-f[i*2-1])*20))
        cocoon<-c(cocoon,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        q_vector<-c(q_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        q_vector<-c(q_vector,rep(inp$`Value`[q[i]],(f[i*2]-f[i*2-1])*20))
        quarantine<-c(quarantine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
        tb_vector<-c(tb_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],(f[i*2]-f[i*2-1])*20))
        travelban<-c(travelban,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
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
  ## vaccine
  f<-c()
  vc_vector<-c()
  vaccine<-c()
  if (length(vc)>=1){
    for (i in 1:length(vc)){
      
      f<-c(f,as.numeric(inp$`Date Start`[vc[i]]-startdate),as.numeric(inp$`Date End`[vc[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[vc[i]]>startdate){
          vc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vc[i]],(f[i+1]-f[i])*20))
          vaccine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          vc_vector<-c(rep(inp$`Value`[vc[i]],(f[i+1])*20))
          vaccine<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        vc_vector<-c(vc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],(f[i*2]-f[i*2-1])*20))
        vaccine<-c(vaccine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        vaccine<-c(vaccine,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(vc)&& f[i*2]<tail(times,1)){
        vc_vector<-c(vc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        vaccine<-c(vaccine,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    vc_vector<-rep(0,tail(times,1)*20)
    vaccine<-rep(0,tail(times,1)*20)
  }
  
  return(list(si_vector=si_vector,sd_vector=sd_vector,scr_vector=scr_vector,hw_vector=hw_vector,wah_vector=wah_vector,
              sc_vector=sc_vector,tb_vector=tb_vector,cte_vector=cte_vector,q_vector=q_vector,vc_vector=vc_vector,isolation=isolation,
              screen=screen,cocoon=cocoon,schoolclose=schoolclose,workhome=workhome,handwash=handwash,
              quarantine=quarantine,vaccine=vaccine,travelban=travelban,distancing=distancing))
}