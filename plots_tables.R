
# helper functions --------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
 indexes = which(!x %in% boxplot.stats(x)$out == T)
 return(indexes)
}


create_df = function(iters,threads,code){
  path = getwd()
  if(code == "OpenMP"){
    file = paste("results_",iters,"_",threads,".csv",sep="")
    full_path = paste(path,"/",file,sep="") 
    print(full_path)
    df = read.csv(full_path)
  }else if(code == "CUDA"){
    file = paste("results_cuda_",iters,"k",".csv",sep="")
    full_path = paste(path,"/",file,sep="") 
    print(full_path)
    df = read.csv(full_path, skip=1)
    
  }else{
    file = paste("results_serial_",iters,"k",".csv",sep="")
    full_path = paste(path,"/",file,sep="") 
    df = read.csv(full_path)
  }
  print(head(df))
  colnames(df) = c("pi", "error", "time")
  df$iters = iters
  df$threads = threads
  indexes = remove_outliers(df$time)
  df_clean = df[indexes, ]
  print(head(df_clean))
  print(dim(df_clean))
  return(df_clean)
}


plot_func = function(data, iters, threads, tool){
  mu_t = round(mean(data$time), 12) 
  sd_t =round(sd(data$time), 12)
  #plot histogram with 40 bins
  par(bg="whitesmoke")
  hist(data$time,
       prob=T,
       breaks=30,
       col="lightsteelblue1",
       main=paste("Histogram for time with ", tool ,"\n iters: ", 
                  iters,"--threads: ",threads,"--samples: 5000", sep=""),
       xlab="time", 
       col.main="blue",
       panel.first=grid(25,25))
  # density
# lines(density(data$time), # density plot
#  lwd = 1, # thickness of line
#  col = "purple")
  # mean line
  abline(v = mu_t,
         col = "green4",
         lwd = 4, lty=3)
  # mean + sd
  abline(v = mu_t + sd_t,
         col = "tomato",
         lwd = 4, lty=3)
  # mean - sd 
  abline(v = mu_t - sd_t,
         col = "tomato",
         lwd = 4, lty=3)
}

# summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
#                       conf.interval=.95, .drop=TRUE) {
#     library(plyr)
# 
#     # New version of length which can handle NA's: if na.rm==T, don't count them
#     length2 <- function (x, na.rm=FALSE) {
#         if (na.rm) sum(!is.na(x))
#         else       length(x)
#     }
# 
#     # This does the summary. For each group's data frame, return a vector with
#     # N, mean, and sd
#     datac <- ddply(data, groupvars, .drop=.drop,
#       .fun = function(xx, col) {
#         c(N    = length2(xx[[col]], na.rm=na.rm),
#           mean = mean   (xx[[col]], na.rm=na.rm),
#           sd   = sd     (xx[[col]], na.rm=na.rm)
#         )
#       },
#       measurevar
#     )
# 
#     # Rename the "mean" column    
#     datac <- rename(datac, c("mean" = measurevar))
# 
#     datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
# library(ggplot2)
#     # Confidence interval multiplier for standard error
#     # Calculate t-statistic for confidence interval: 
#     # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#     ciMult <- qt(conf.interval/2 + .5, datac$N-1)
#     datac$ci <- datac$se * ciMult
# 
#     return(datac)
# }
# 
# 
# tgc=summarySE(df_24_48_96_total[,3:5], 
#               measurevar="time", groupvars=c("iters","threads"))
# tgc
# 
# tgc2 = tgc
# tgc2$iters = as.factor(tgc2$itecornsilk2rs)
# tgc2$threads = as.factor(tgc2$threads)



# Standard error of the mean
# ggplot(tgc2, aes(x=threads, y=time, colour=iters, group= iters)) + 
#     geom_line() +
#     geom_point()
# 
# 
# tgc2 = tgc
# tgc2$iters = as.factor(tgc2$iters)
# tgc2$threads = as.factor(tgc2$threads)



# -------------------------------------------------------------------------

openmp = "OpenMP"

# 24k iterations ----------------------------------------------------------

par(mar=rep(2,4))
par(mfrow=c(3,2))
df_24_1 = create_df(24,1,openmp)
plot_func(df_24_1, 24000000, 1, openmp)

df_24_6 = create_df(24,6,openmp)
plot_func(df_24_6,24000000, 6, openmp)

df_24_12 = create_df(24,12,openmp)
plot_func(df_24_12,24000000, 12, openmp)

df_24_24 = create_df(24,24,openmp)
plot_func(df_24_24,24000000, 24, openmp)

df_24_48 = create_df(24,48,openmp)
plot_func(df_24_48,24000000, 48, openmp)
par(mai=c(0,0,0,0))
plot.new()
# legend
legend(x = "center", # location of legend within plot area
        c("Density plot", "Mean", "Sd"),
        col = c("lightsteelblue1", "green4", "tomato"),
        lwd = c(2, 2, 2), lty=c(1,3,3),cex=.9)



df_24_total = rbind(df_24_1, df_24_6, df_24_12, df_24_24, df_24_48)
# nrow(df_24_1)
# nrow(df_24_total)/5

head(df_24_total)


# 48k iterations ----------------------------------------------------------

par(mar=rep(2,4))
par(mfrow=c(3,2))
df_48_1 = create_df(48,1,openmp)
plot_func(df_48_1, 48000000, 1, openmp)

df_48_6 = create_df(48,6,openmp)
plot_func(df_48_6,48000000, 6, openmp)

df_48_12 = create_df(48,12,openmp)
plot_func(df_48_12,48000000, 12, openmp)

df_48_24 = create_df(48,24,openmp)
plot_func(df_48_24,48000000, 24, openmp)

df_48_48 = create_df(48,48,openmp)
plot_func(df_48_48,48000000, 48, openmp)
par(mai=c(0,0,0,0))
plot.new()
# legend
legend(x = "center", # location of legend within plot area
        c("Density plot", "Mean", "Sd"),
        col = c("lightsteelblue1", "green4", "tomato"),
        lwd = c(2, 2, 2), lty=c(1,3,3),cex=.9)


df_48_total = rbind(df_48_1, df_48_6, df_48_12, df_48_24, df_48_48)

# 96k iterations ----------------------------------------------------------

par(mar=rep(2,4))
par(mfrow=c(3,2))
df_96_1 = create_df(96,1,openmp)
plot_func(df_96_1, 96000000, 1, openmp)

df_96_6 = create_df(96,6,openmp)
plot_func(df_96_6,96000000, 6, openmp)

df_96_12 = create_df(96,12,openmp)
plot_func(df_96_12,96000000, 12, openmp)

df_96_24 = create_df(96,24,openmp)
plot_func(df_96_24,96000000, 24, openmp)

df_96_48 = create_df(96,48,openmp)
plot_func(df_96_48,96000000, 48, openmp)
par(mai=c(0,0,0,0))
plot.new()
# legend
legend(x = "center", # location of legend within plot area
        c("Density plot", "Mean", "Sd"),
        col = c("lightsteelblue1", "green4", "tomato"),
        lwd = c(2, 2, 2), lty=c(1,3,3),cex=.9)



df_96_total = rbind(df_96_1, df_96_6, df_96_12, df_96_24, df_96_48)



# total df ----------------------------------------------------------------
df_24_48_96_total = rbind(df_24_total, df_48_total, df_96_total)


# -------------------------------------------------------------------------




# my_list= list(df_24_1, df_24_6, df_24_12, df_24_24, df_24_48,
#               df_48_1, df_48_6, df_48_12, df_48_24, df_48_48)
# 
# for(i in 1:length(my_list)){
#   print(i)
#   print(paste("mean: ",mean(my_list[[i]]$time)," sd: ", sd(my_list[[i]]$time), sep=""))
# }



library(ggplot2)
library(dplyr)


tgc = df_24_48_96_total%>%
  select(-c(pi,error))%>%
  group_by(threads,iters)%>%
  summarise_at(vars(time),list(name = mean))

tgc

tgc2 = tgc
tgc2$iters = as.factor(tgc2$iters)
tgc2$threads = as.factor(tgc2$threads)


ggplot(tgc2,aes(x=threads, y=name, colour=iters, group= iters)) + 
  geom_line() +
  geom_point() +
  ylab("mean times") +
  labs(title = "Mean times per iteration and threads with OpenMP",
       subtitle = "")+
  theme(
    plot.title = element_text(color = "blue", size = 14, face = "bold", hjust = 0.5))




# CUDA results ------------------------------------------------------------

cuda = "CUDA"

par(mar=rep(2,4))
par(mfrow=c(2,2))
df_24_cuda = create_df(24,1024,cuda)
head(df_24_cuda)
plot_func(df_24_cuda, 24000000, 1024, cuda)

df_48_cuda = create_df(48,1024,cuda)
head(df_48_cuda)
plot_func(df_48_cuda, 48000000, 1024, cuda)

df_96_cuda = create_df(96,1024,cuda)
head(df_96_cuda)
plot_func(df_96_cuda, 96000000, 1024, cuda)
par(mai=c(0,0,0,0))
plot.new()
# legend
legend(x = "center", # location of legend within plot area
        c("Density plot", "Mean", "Sd"),
        col = c("lightsteelblue1", "green4", "tomato"),
        lwd = c(2, 2, 2), lty=c(1,3,3),cex=.9)



df_cuda_total = rbind(df_24_cuda, df_48_cuda, df_96_cuda)

tgc_cuda = df_cuda_total%>%
  select(-c(pi,error))%>%
  group_by(iters)%>%
  summarise_at(vars(time),list(name = mean))

tgc_cuda

tgc2_cuda = tgc_cuda
tgc2_cuda$iters = as.factor(tgc2_cuda$iters)
tgc2_cuda$name = round(tgc2_cuda$name,5)



ggplot(tgc2_cuda,aes(x=iters, y=name, colour=iters, group= 1, fill=iters)) + 
  geom_bar(stat = "identity") +
  ylab("mean times") +
  labs(title = "Mean times per iteration for 1024 threads with CUDA",
       subtitle = "")+
  geom_text(aes(x=iters,y=name,label=name)
            , col="black",size=3.5,position = position_stack(vjust = 1.08))+
  theme(
    plot.title = element_text(color = "blue", size = 14, face = "bold", hjust = 0.5))


# OpenMP [0.00371, 0.00740, 0.0148]

# CUDA [0.00333, 0.00661, 0.0132]



# serial results ----------------------------------------------------------

serial = "serial"

par(mar=rep(2,4))
par(mfrow=c(2,2))
df_24_serial = create_df(24,0,serial)
head(df_24_serial)
plot_func(df_24_serial, 24000000, 0, serial)

df_48_serial = create_df(48,0,serial)
head(df_48_serial)
plot_func(df_48_serial, 48000000, 0, serial)

df_96_serial = create_df(96,0,serial)
head(df_96_serial)
plot_func(df_96_serial, 96000000, 0, serial)
par(mai=c(0,0,0,0))
plot.new()
# legend
legend(x = "center", # location of legend within plot area
        c("Density plot", "Mean", "Sd"),
        col = c("lightsteelblue1", "green4", "tomato"),
        lwd = c(2, 2, 2), lty=c(1,3,3),cex=.9)



df_serial_total = rbind(df_24_serial, df_48_serial, df_96_serial)

head(df_serial_total)

tgc_serial = df_serial_total%>%
  select(-c(pi,error))%>%
  group_by(iters)%>%
  summarise_at(vars(time),list(name = mean))

tgc_serial

tgc

tgc_cuda


tgc_serial_rep =rep(tgc_serial$name,5) ; tgc_serial_rep

speedup = data.frame(abs( (tgc$name-tgc_serial_rep)/tgc_serial_rep *100 ))
colnames(speedup) = "speedup_openmp" 
speedup$speedup_openmp = sapply(speedup$speedup_openmp,function(s){round(s,5)})
speedup$iterations = tgc$iters 
speedup$threads = tgc$threads ; head(speedup)



speedup2 = speedup
speedup2$iterations = as.factor(speedup2$iterations)
speedup2$threads = as.factor(speedup2$threads)


library(scales)

ggplot(speedup2,aes(x=threads, y=speedup_openmp, colour=iterations, fill= iterations)) + 
  geom_bar(stat = 'identity',position='dodge', alpha = 0.8) +
  ylab("Speedup percentage") +
  xlab("threads") +
  labs(title = "Speedup with OpenMP compared to serial code",
       subtitle = "")+
  theme(
    plot.title = element_text(color = "blue", size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(x=threads,y=speedup_openmp,label=paste(speedup_openmp,"%")), 
            position = position_dodge2(width=0.9, preserve = "total", padding=1), col="black",size=3.5,)+
   scale_y_continuous(labels = scales::percent_format(scale = 1))+
  coord_flip()


# -------------------------------------------------------------------------



speedup_cuda = data.frame(abs( (tgc_cuda$name-tgc_serial$name)/tgc_serial$name *100 ))
colnames(speedup_cuda) = "speedup_cuda" 
speedup_cuda$speedup_cuda = sapply(speedup_cuda$speedup_cuda,function(s){round(s,5)})
speedup_cuda$iterations = c(24,48,96)



speedup2_cuda = speedup_cuda ; head(speedup2_cuda)
speedup2_cuda$iterations = as.factor(speedup2_cuda$iterations)


library(scales)

ggplot(speedup2_cuda,aes(x=iterations, y=speedup_cuda, colour=iterations, fill= iterations)) + 
  geom_bar(stat = 'identity',position='dodge',alpha=0.8) +
  ylab("Speeup percentage") +
  xlab("iterations") +
  labs(title = "Speedup with CUDA compared to serial code",
       subtitle = "")+
  theme(
    plot.title = element_text(color = "blue", size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(x=iterations,y=speedup_cuda,label=paste(speedup_cuda,"%")), 
            position = position_dodge2(width=0.9, preserve = "total", padding=4), col="black",size=3.5,)+
   scale_y_continuous(labels = scales::percent_format(scale = 1))+
  coord_flip()

S