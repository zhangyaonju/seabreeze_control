####this is the SpecFluo Processing software
####it include   parts

#read the data in
library(rms)
Sys.setlocale("LC_ALL","English")
Sys.setlocale("LC_TIME", "English")

jet.colors <-   colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(6)

# QE_CC<-read.table("F:/FluoSpec_data/configuration/QE-PRO_0719_CC_average2_OOIIrrad.cal",
#                    skip = 9,sep='\t')
# 
# 
# QE_BF<-read.table("F:/FluoSpec_data/configuration/QE-PRO_0719_BF_average_OOIIrrad.cal",
#                    skip = 9,sep='\t')
# 
# 
# HR_CC<-read.table("F:/FluoSpec_data/configuration/HR2000plus_0719_CC_OOIIrrad.cal",
#                   skip = 9,sep='\t')
# HR_BF<-read.table("F:/FluoSpec_data/configuration/HR2000plus_0719_BF_OOIIrrad.cal",
#                   skip = 9,sep='\t')


QE_CC<-read.csv("F:/FluoSpec_data/configuration/new_QE_CC_1001.csv",header = T)[,1:2]
QE_BF<-read.csv("F:/FluoSpec_data/configuration/new_QE_BF_1001.csv",header = T)[,1:2]
# QE_CC1<-read.csv("F:/FluoSpec_data/configuration/new_QE_CC_829.csv",header = T)[,1:2]
# QE_BF1<-read.csv("F:/FluoSpec_data/configuration/new_QE_BF_827.csv",header = T)[,1:2]
HR_CC<-read.csv("F:/FluoSpec_data/configuration/new_HR_CC_1001.csv",header = T)[,1:2]
HR_BF<-read.csv("F:/FluoSpec_data/configuration/new_HR_BF_1001.csv",header = T)[,1:2]


read_QE_pro<-function(filename){
  #filename<-"F:/FluoSpec_data/test/QE_PRO_Jul_18_16_00_40_2016.csv"
  con<-file(filename,open="r")
  temp_str<-readLines(con, n=1)
  datetime<-as.POSIXlt(strptime(substr(temp_str,28,60),format="%a %b %d %H:%M:%S %Y"))
  readLines(con, n=1)
  readLines(con, n=1)
  temp_str<-readLines(con, n=1)
  ambient_t<-as.numeric(strsplit(temp_str,": ")[[1]][2])
  temp_str<-readLines(con, n=1)
  measurement_t<-as.numeric(strsplit(temp_str,": ")[[1]][2])
  readLines(con, n=1)
  temp_str<-readLines(con, n=1)
  nonlinearity_correction_factor<-as.numeric(strsplit(temp_str,",")[[1]])
  temp_str<-readLines(con, n=1)
  wavelength<-as.numeric(strsplit(temp_str,",")[[1]])
  temp_str<-readLines(con, n=1)
  inte_t1<-as.numeric(strsplit(temp_str,": ")[[1]][2])
  temp_str<-readLines(con, n=1)
  spectrum_dark1<-as.numeric(strsplit(temp_str,",")[[1]])
  #spectrum_dark1<-rep(mean(as.numeric(strsplit(temp_str,",")[[1]])[c(1:4,1041:1044)]),1044)
  temp_str<-readLines(con, n=1)
  inte_t2<-as.numeric(strsplit(temp_str,": ")[[1]][2])
  temp_str<-readLines(con, n=1)
  spectrum_dark2<-as.numeric(strsplit(temp_str,",")[[1]])
  #spectrum_dark2<-rep(mean(as.numeric(strsplit(temp_str,",")[[1]])[c(1:4,1041:1044)]),1044)
  close(con)
  
  spectrum<-as.matrix(read.csv(filename,skip = 12,header = F))
  QE_data<-list(datetime=datetime,ambient_t=ambient_t,measurement_t=measurement_t,
                nonlinearity_correction_factor=nonlinearity_correction_factor,
                wavelength=wavelength,
                inte_t1=inte_t1,spectrum_dark1=spectrum_dark1,
                inte_t2=inte_t2,spectrum_dark2=spectrum_dark2,spectrum=spectrum)
  return(QE_data)
}

read_HR2k<-function(filename){
  #filename<-"F:/FluoSpec_data/QE_PRO_Jul_17_12_10_26_2016.csv"
  con<-file(filename,open="r")
  temp_str<-readLines(con, n=1)
  datetime<-as.POSIXlt(strptime(substr(temp_str,28,60),format="%a %b %d %H:%M:%S %Y"))
  readLines(con, n=1)
  readLines(con, n=1)
  temp_str<-readLines(con, n=1)
  temp_str<-readLines(con, n=1)
  temp_str<-readLines(con, n=1)
  nonlinearity_correction_factor<-as.numeric(strsplit(temp_str,",")[[1]])
  temp_str<-readLines(con, n=1)
  wavelength<-as.numeric(strsplit(temp_str,",")[[1]])
  temp_str<-readLines(con, n=1)
  inte_t1<-as.numeric(strsplit(temp_str,": ")[[1]][2])
  temp_str<-readLines(con, n=1)
  inte_t2<-as.numeric(strsplit(temp_str,": ")[[1]][2])
  close(con)
  
  spectrum<-as.matrix(read.csv(filename,skip = 10,header = F))
  HR_data<-list(datetime=datetime,wavelength=wavelength,
                nonlinearity_correction_factor=nonlinearity_correction_factor,
                inte_t1=inte_t1,inte_t2=inte_t2,spectrum=spectrum)
}

nonlinearity_correction_QE<-function(QE_data){
  obs<-dim(QE_data$spectrum)[1]/2

  dark_measurement_irr<-mean(QE_data$spectrum[c(1,3,5,7,9),c(1:4,1041:1044)])
  dark_measurement_rad<-mean(QE_data$spectrum[c(2,4,6,8,10),c(1:4,1041:1044)])
  edark<-rep(c(dark_measurement_irr,dark_measurement_rad),5*1044)
  dim(edark)<-c(obs*2,dim(QE_data$spectrum)[2])

  ##edark for dark measurement
  QE_data$spectrum_dark1<-QE_data$spectrum_dark1-mean(QE_data$spectrum_dark1[c(1:4,1041:1044)])
  QE_data$spectrum_dark2<-QE_data$spectrum_dark2-mean(QE_data$spectrum_dark2[c(1:4,1041:1044)])
  
  dark_measurement<-rep(c(QE_data$spectrum_dark1,QE_data$spectrum_dark2),obs)
  dim(dark_measurement)<-c(dim(QE_data$spectrum)[2],obs*2)
  dark_measurement<-t(dark_measurement)
 
  dark_corr<-QE_data$spectrum-dark_measurement-edark

  corr_factor<-QE_data$nonlinearity_correction_factor
  nonlinearity_corrected<-dark_corr/(corr_factor[1]+corr_factor[2]*dark_corr+corr_factor[3]*dark_corr^2+
                                       corr_factor[4]*dark_corr^3+corr_factor[5]*dark_corr^4+
                                       corr_factor[6]*dark_corr^5+corr_factor[7]*dark_corr^6+
                                       corr_factor[8]*dark_corr^7)
  return(nonlinearity_corrected)
}


nonlinearity_correction_HR<-function(HR_data){
  obs<-dim(HR_data$spectrum)[1]/2
  dark_measurement_irr<-mean(HR_data$spectrum[c(1,3,5,7,9),1:18])
  dark_measurement_rad<-mean(HR_data$spectrum[c(2,4,6,8,10),1:18])
  dark_measurement<-rep(c(dark_measurement_irr,dark_measurement_rad),5*2048)
  dim(dark_measurement)<-c(obs*2,dim(HR_data$spectrum)[2])
  dark_corr<-HR_data$spectrum-dark_measurement
  corr_factor<-HR_data$nonlinearity_correction_factor
  nonlinearity_corrected<-dark_corr/(corr_factor[1]+corr_factor[2]*dark_corr+corr_factor[3]*dark_corr^2+
                                       corr_factor[4]*dark_corr^3+corr_factor[5]*dark_corr^4+
                                       corr_factor[6]*dark_corr^5+corr_factor[7]*dark_corr^6+
                                       corr_factor[8]*dark_corr^7)
  return(nonlinearity_corrected)
}

irradiance_calc_QE<-function(QE_data,nonlinearity_corrected,QE_CC,QE_BF){
  sr_bf<-0.153721
  sr_cc<-pi
  area_bf<-0.25*pi*1e-6
  area_cc<-1.95*1.95*pi*1e-6
  wavelength_spread_QE<-59.28809/1043
  obs<-dim(QE_data$spectrum)[1]/2
  factor<-rep(c(QE_CC[,2]/sr_cc/(QE_data$inte_t1/1e6)/area_cc/wavelength_spread_QE/1e3,
                QE_BF[,2]/sr_bf/(QE_data$inte_t2/1e6)/area_bf/wavelength_spread_QE/1e3),obs)
  #this factor used to convert count to mW/m2/sr/nm
  dim(factor)<-c(dim(QE_data$spectrum)[2],obs*2)
  factor<-t(factor)
  factor[,c(1:4,1041:1044)]<-NA
  irradiance_flux<-factor*nonlinearity_corrected
  irradiance_flux[,c(1:4,1041:1044)]<-NA

  return(irradiance_flux)
}

irradiance_calc_HR<-function(HR_data,nonlinearity_corrected,HR_CC,HR_BF){
  sr_bf<-0.153721
  sr_cc<-pi
  area_bf<-0.25*pi*1e-6
  area_cc<-1.95*1.95*pi*1e-6
  wavelength_spread_HR<-918.1482/2047
  obs<-dim(HR_data$spectrum)[1]/2
  factor<-rep(c(HR_CC[,2]/sr_cc/(HR_data$inte_t1/1e6)/area_cc/wavelength_spread_HR/1e3,
                HR_BF[,2]/sr_bf/(HR_data$inte_t2/1e6)/area_bf/wavelength_spread_HR/1e3),obs)
  #this factor used to convert count to mW/m2/sr/nm
  dim(factor)<-c(dim(HR_data$spectrum)[2],obs*2)
  factor<-t(factor)
  factor[,c(1:18)]<-NA
  irradiance_flux<-factor*nonlinearity_corrected
  irradiance_flux[,c(1:18)]<-NA

  return(irradiance_flux)
}

reflectance<-function(irradiance_flux){
  obs<-dim(irradiance_flux)[1]/2
  irr_i<-1:obs*2-1
  rad_i<-1:obs*2
  irr<-irradiance_flux[irr_i,]
  rad<-irradiance_flux[rad_i,]
  
  reflectance<-rad/irr
  return(reflectance)
}

sif_retrival<-function(irradiance,radiance){
  ####wavelength number for 759.0321-764.0709
  ##########################   463     551
  rin<-irradiance[463:551]
  rout<-radiance[463:551]
  x=1:89
  rin[is.na(rin)]<-0
  if((sum(rin==0)==89)||(sum(rout==0)==89)){
    return(NA)
  }
  res<-ols(rout~x*rin+rin+x)
  re_coeff<-coefficients(res)
  rio<-re_coeff[4]*x+re_coeff[3]
  sif1<-re_coeff[2]*x+re_coeff[1]
  rsq<-(cor.test(rout,(rio*rin+sif1))$estimate)^2
#   res2<-ols(rout~x^2+x*rin+x+rin)
#   re_coeff2<-coefficients(res2)
#   #
#   #plot(rout,rio*rin+sif)
#   sif2<-re_coeff2[3]*x*x+re_coeff2[2]*x+re_coeff2[1]
  return(c(sif1,rsq))
}

generate_dark_measurement<-function(coeff,inte_t){
  dark<-coeff$V1+coeff$V2*inte_t/1e6
  return(dark)
}





dir<-"F:/FluoSpec_data/0930/data/"
filename_QE<-list.files(dir,pattern="QE_PRO",full.names = T)
filename_HR<-list.files(dir,pattern="HR2000PLUS",full.names = T)

raw_qe_dat<-substr(basename(filename_QE),8,27)
mod_qe_dat<-gsub("__","_0",raw_qe_dat)
iso_qe_time<-strptime(mod_qe_dat,format="%b_%d_%H_%M_%S_%Y")
qe_doy<-iso_qe_time$yday
qe_hour<-iso_qe_time$hour+floor((iso_qe_time$min)/15)/4
qe_time<-qe_doy+qe_hour/24

raw_hr_dat<-substr(basename(filename_HR),12,31)
mod_hr_dat<-gsub("__","_0",raw_hr_dat)
iso_hr_time<-strptime(mod_hr_dat,format="%b_%d_%H_%M_%S_%Y")
hr_doy<-iso_hr_time$yday
hr_hour<-iso_hr_time$hour+floor((iso_hr_time$min)/15)/4
hr_time<-hr_doy+hr_hour/24

dir.create(paste(dir,"/graph",sep=''))
m20_coeff<-read.csv("F:/FluoSpec_data/configuration/QE_PRO_M20_dark_coeff.csv")
m15_coeff<-read.csv("F:/FluoSpec_data/configuration/QE_PRO_M15_dark_coeff.csv")
m10_coeff<-read.csv("F:/FluoSpec_data/configuration/QE_PRO_M10_dark_coeff.csv")
diurnal_sif<-as.data.frame(array(NA,dim=c(length(filename_QE),23)))
names(diurnal_sif)<-c("date","SIF1","SIF2","SIF3","SIF4","SIF5",
                      "RSQ1","RSQ2","RSQ3","RSQ4","RSQ5",
                      "temp_ambient","temp_measure",
                      "irr_750_QE1","irr_750_QE2","irr_750_QE3","irr_750_QE4","irr_750_QE5",
                      "irr_750_HR1","irr_750_HR2","irr_750_HR3","irr_750_HR4","irr_750_HR5")

# filename_QE<-list.files("F:/FluoSpec_data/test/",pattern="QE_PRO",full.names = T)
# filename_HR<-list.files("F:/FluoSpec_data/test/",pattern="HR2000PLUS",full.names = T)

for (f in 1:length(filename_QE)){
  #f=5
  
  qe_data<-read_QE_pro(filename_QE[f])
  date_t<-substr(basename(filename_QE[f]),8,27)
  if (median(qe_data$spectrum_dark1)>2500)
    next
#   this is used to generate the dark meansurement when internal shuter does not work  
#   if (qe_data$measurement_t< -19&qe_data$measurement_t> -20){
#     qe_data$spectrum_dark1<-generate_dark_measurement(m20_coeff,qe_data$inte_t1)
#     qe_data$spectrum_dark2<-generate_dark_measurement(m20_coeff,qe_data$inte_t2)
#   }else if(qe_data$measurement_t< -14&qe_data$measurement_t> -15){
#     qe_data$spectrum_dark1<-generate_dark_measurement(m15_coeff,qe_data$inte_t1)
#     qe_data$spectrum_dark2<-generate_dark_measurement(m15_coeff,qe_data$inte_t2)
#   }else if(qe_data$measurement_t< -9&qe_data$measurement_t> -11){
#     qe_data$spectrum_dark1<-generate_dark_measurement(m10_coeff,qe_data$inte_t1)
#     qe_data$spectrum_dark2<-generate_dark_measurement(m10_coeff,qe_data$inte_t2)
#   }else{
#     next
#   }
  
  nonlinearity_corrected_QE<-nonlinearity_correction_QE(qe_data)
  irradiance_flux_QE<-irradiance_calc_QE(qe_data,nonlinearity_corrected_QE,QE_CC,QE_BF)
  reflectance_QE<-reflectance(irradiance_flux_QE)
  #sif_retrival(irradiance_flux[1,],irradiance_flux[2,])
  
  sif<-array(NA,dim=c(90,5))
  for (i in 1:5){
    sif[,i]<-sif_retrival(irradiance_flux_QE[i*2-1,],irradiance_flux_QE[i*2,])
  }
  
  hr_data<-read_HR2k(filename_HR[which(hr_time==qe_time[f])[1]])  #
  nonlinearity_corrected_HR<-nonlinearity_correction_HR(hr_data)
  irradiance_flux_HR<-irradiance_calc_HR(hr_data,nonlinearity_corrected_HR,HR_CC,HR_BF)
  reflectance_HR<-reflectance(irradiance_flux_HR)
  
  diurnal_sif[f,1]<-qe_time[f]
  diurnal_sif[f,2:6]<-sif[18,]
  diurnal_sif[f,7:11]<-sif[90,]
  diurnal_sif[f,12]<-qe_data$ambient_t
  diurnal_sif[f,13]<-qe_data$measurement_t
  diurnal_sif[f,14:18]<-irradiance_flux_QE[c(1,3,5,7,9),314]
  diurnal_sif[f,19:23]<-irradiance_flux_HR[c(1,3,5,7,9),1225]
  
  pdf(paste(dir,"/graph/",date_t,".pdf",sep=""),width = 14, height=24)
  #Graph<-function(){
  par(fig=c(0,1,0.8,1),mar=c(4,5,4,5)+0.5)
  plot(NA,ylim=c(0,max(irradiance_flux_QE,na.rm = T)*1.1),xlim=c(min(qe_data$wavelength),max(qe_data$wavelength)),
       main=qe_data$datetime,
       ylab=expression("[mW m"^-2~"sr"^-1~"nm"^-1~"]"),
       xlab="wavelength (nm)",cex.lab=1.25)
  for (i in 1:10){
    lines(qe_data$wavelength,irradiance_flux_QE[i,],col=jet.colors[ceiling(i/2)],lty=i%%2+1)
  }
  legend("bottomright",legend=c("irradiance","radiance"),lty=c(2,1),y.intersp=1,bty="n")

  
  par(fig=c(0,1,0.6,0.8),mar=c(4,5,4,5)+0.5,new=T)
  plot(NA,ylim=c(0,max(irradiance_flux_HR[,500:1500],na.rm = T)*1.1),xlim=c(min(hr_data$wavelength),max(hr_data$wavelength)),
       main=hr_data$datetime,
       ylab=expression("[mW m"^-2~"sr"^-1~"nm"^-1~"]"),
       xlab="wavelength (nm)",cex.lab=1.25)
  for (i in 1:10){
    lines(hr_data$wavelength,irradiance_flux_HR[i,],col=jet.colors[ceiling(i/2)],lty=i%%2+1)
  }
  
  
  
  par(fig=c(0,1,0.4,0.6),mar=c(4,5,4,5)+0.5,new=T)
  plot(NA,ylim=c(0,1),xlim=c(min(qe_data$wavelength),max(qe_data$wavelength)),
       main=qe_data$datetime,
       ylab=expression("reflectance"),
       xlab="wavelength (nm)",cex.lab=1.25)
  for (i in 1:5){
    lines(qe_data$wavelength,reflectance_QE[i,],col=jet.colors[i],lty=1)
  }
  legend(770,40,legend=c("reflectance"),lty=c(1),y.intersp=0.3,bty="n")
  
  
  par(fig=c(0,1,0.2,0.4),mar=c(4,5,4,5)+0.5,new=T)
  plot(NA,ylim=c(0,1),xlim=c(min(hr_data$wavelength),max(hr_data$wavelength)),
       main=hr_data$datetime,
       ylab=expression("reflectance"),
       xlab="wavelength (nm)",cex.lab=1.25)
  for (i in 1:5){
    lines(hr_data$wavelength,reflectance_HR[i,],col=jet.colors[i],lty=1)
  }
  legend(200,21,legend=c("reflectance"),lty=c(1),y.intersp=0.3,bty="n")
  
  par(fig=c(0,1,0,0.2),mar=c(4,5,4,5)+0.5,new=T)
  plot(NA,ylim=c(0,5),xlim=c(qe_data$wavelength[463],qe_data$wavelength[551]),
       main="SIF",
       ylab=expression("[mW m"^-2~"sr"^-1~"nm"^-1~"]"),
       xlab="wavelength (nm)",cex.lab=1.25)
  for (i in 1:5){
    lines(qe_data$wavelength[463:551],sif[1:89,i],col=jet.colors[i],lty=1)
  }
  legend(770,21,legend=c("SIF"),lty=c(1),y.intersp=0.3,bty="n")
  

  
  dev.off()
  
}

write.csv(diurnal_sif,paste("F:/FluoSpec_data/",floor(max(qe_time)),"sif.csv",sep=""))

# 
# a<-qe_data$spectrum[1,]/qe_data$inte_t1*QE_CC[,2]
# b<-qe_data$spectrum[2,]/qe_data$inte_t2*QE_BF[,2]
# 
# 
# a2<-nonlinearity_corrected_HR[1,]/hr_data$inte_t1*HR_CC[,2]
# b2<-nonlinearity_corrected_HR[2,]/hr_data$inte_t2*HR_BF[,2]
# plot(hr_data$wavelength,b2/a2,ylim=c(0,0.005),type="l")
# plot(hr_data$wavelength,hr_data$spectrum[2,])
# plot(qe_data$wavelength,b/a,ylim=c(0,0.002),type="l")
