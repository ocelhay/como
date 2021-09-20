pop1<-out0[,(Sindex+1)]+out0[,(Eindex+1)]+out0[,(Iindex+1)]+out0[,(CLindex+1)]+out0[,(Rindex+1)]+
  out0[,(Xindex+1)]+out0[,(Vindex+1)]+out0[,(Zindex+1)]+out0[,(EVindex+1)]+out0[,(ERindex+1)]+out0[,(EVRindex+1)]+
  out0[,(QSindex+1)]+out0[,(QEindex+1)]+out0[,(QIindex+1)]+out0[,(QCindex+1)]+out0[,(QRindex+1)]+
  out0[,(QVindex+1)]+out0[,(QEVindex+1)]+out0[,(QERindex+1)]+out0[,(QVRindex+1)]+out0[,(QEVRindex+1)]+
  out0[,(Hindex+1)]+out0[,(HCindex+1)]+out0[,(ICUindex+1)]+out0[,(ICUCindex+1)]+out0[,(ICUCVindex+1)]+
  out0[,(Ventindex+1)]+out0[,(VentCindex+1)]+out0[,(HCICUindex+1)]+out0[,(HCVindex+1)]

tpop1<-rowSums(pop1)
ab_all_ages<-rowSums(out0[,(Abindex+1)])
ab_all_ages/tpop1