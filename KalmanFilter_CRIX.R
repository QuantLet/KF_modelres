# ------------------------------------------------------------------------------
# Project:     KalmanFilter_CRIX
# ------------------------------------------------------------------------------
# Quantlet:    KF_modelres
# ------------------------------------------------------------------------------
# Description: Use the CRIX data as an example to predict market return by
#              Kalman filter
# ------------------------------------------------------------------------------
# Keywords:    plot, Kalman Filter, predict, estimation, returns, CRIX, gif
# ------------------------------------------------------------------------------
# See also:    https://gist.github.com/mathew-hall/2ca753c68a594e2c37b1
# ------------------------------------------------------------------------------
# Author:      Ruting Rainy Wang
# ------------------------------------------------------------------------------

rm(list = ls(all = TRUE))

# load package
libraries = c("FKF","quantmod","ggplot2","rjson","car","magick","gganimate","hrbrthemes","gifski","dplyr","viridis",
              "ggplot2","rjson","car","magick","gganimate")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# people can change the wdir
wdir = "/Users/wkhaerdle/Documents/Project/Metis/"
setwd(wdir)
dir.create("Results")
save = paste0(wdir, '/', "Results/")
file = "http://data.thecrix.de/data/crix.csv"
start = '2021-02-09'
end = '2017-01-02'


crix = read.csv(file = file, header = TRUE)
crix = as.data.frame(crix)
crix = crix[which(crix$date <= start),]
crix = crix[which(crix$date >= end),]
count = nrow(crix)
price = crix$price
return = c(0, diff(log(crix$price))) #observations

#Allocate space:
xhat = rep(0,count) #a posteri estimate at each step
P = rep(0,count)  #a posteri error estimate
xhatminus=rep(0,count) #a priori estimate
Pminus=rep(0,count) #a priori error estimate
K=rep(0,count) #gain

#estimate of measurement variance
R = 1**2

#initialise guesses: assume true_value=0, error is 1.0
xhat[1] <- return[1] 
P[1] <- 1
Q_Select = c(0.2) # could change to a range
for (Q in Q_Select){ 
  for (k in 2:count){
    #time update
    xhatminus[k] <- xhat[k-1]
    Pminus[k] <- P[k-1] + Q
    
    #measurement update
    K[k] = Pminus[k] / (Pminus[k] + R)
    xhat[k] = xhatminus[k] + K[k] * (return[k] - xhatminus[k])
    P[k] = (1-K[k]) * Pminus[k]
  }
  
  png(paste0(save,'CRIXPrice_Kalman_',Q,'.png'),width=1000,height=600,units="px",bg = "transparent")
  
  x= as.Date(crix$date)
  plot(x,return,col="blue",type="l",xlab = "Date",ylab ="Return",lwd = 1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  lines(x,xhatminus, col = "red",type="l",lwd = 1.5)
  
  dev.off()
}

# movie of parameter interactions
gif_plot = data.frame(date = crix$date, xhatminus = xhatminus, return = return)
gif1 <-  gif_plot %>% 
  ggplot(aes(x=xhatminus, y=return))  +
  geom_line(colour = 'grey') +
  geom_point() +
  theme_ipsum() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  transition_reveal(as.Date(date)) +
  ggtitle("Date: {frame_along}")

#save .gif
animate(gif1, renderer = gifski_renderer(paste0(save,'CRIXPrice_Kalman_',Q,'.gif')), bg = "transparent",
        nframes = 100)

