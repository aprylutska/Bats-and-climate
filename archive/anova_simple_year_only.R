rm(list = ls()) # Reset R`s brain
library(lubridate)    #створення нової колонки з даними про день від початку року
df <- read.csv("Homolsha_all_years.csv")

df$date <- as.Date(df$Data, "%d.%m.%Y") # convert date to R Data format
df$Place <- as.factor(df$Place)
df$Species <- as.factor(df$Species)
df$sex <- as.factor(df$sex)
df$age <- as.factor(df$age)
df$Year <-as.factor (df$Year)

subset_df_clim$day <- yday(subset_df_clim$date)

df2 <- subset(
  df, # обєкт з якого відбираються рядки
  Species %in% c ("Myotis daubentonii") & 
    age == "sad" #види та вікова група які потрібні
) 

df2$Ra <- sub(",", ".", df2$Ra) #заміна ком на крапки в промірах передпліччя
df2$W <- sub(",", ".", df2$W)

df$Ra.num <- as.numeric(df$Ra)

df$Ra[is.na(df$Ra.num)]

?sub
subset_df_clim$Ra.dot <- sub(",", ".", subset_df_clim$Ra) #заміна ком на крапки в промірах передпліччя

df2$Data <- as.Date(df2$Data, "%d.%m.%Y") # convert date to R Data format
df$Place <- as.factor(df$Place)
df$Species <- as.factor(df$Species)
df$sex <- as.factor(df$sex)
df$age <- as.factor(df$age)
df2$Year <- year(df2$Data) #виокремлення року
df2$Year <-as.factor (df2$Year)
df2$Ra <- as.numeric(df2$Ra)
df2$W <- as.numeric(df2$W)

subset_df_clim$Ra.num <- as.numeric(subset_df_clim$Ra.dot)

subset_df_clim$Ra[is.na(subset_df_clim$Ra.num)]

subset_df_clim <- subset(df_clim, 
                  Species == 'Nyctalus noctula' & age == "sad")

subset_df_clim$year <-as.factor (subset_df_clim$year)

library (min)

min(df1$Ra,na.rm = TRUE) <- NULL   #видалення порожніх комірок та аутлаєра(помилкове значення) 

subset_df_clim <-  subset_df_clim[! (subset_df_clim$Ra.num == 24.4),]


library(lubridate)    #створення нової колонки з даними про день від початку року 
df2$day <- yday(df2$Data)

plot(subset_df_clim$day, subset_df_clim$Ra, #діаграма розсіяння, точки
     xlab = "Day from beginning of year", ylab = "Forearm lenght",
     main = "I")
                                                                                
ggplot(data = df2, aes(day, W, colour = Year)) +
  geom_point()


plot(df1$day, df1$Ra, 
     col=c("orange", "blue", "green", "brown")[as.numeric(df1$Year)])

mod <- lm(Ra ~ Year, data=df2)

mod                                                    
summary (mod)                                            
plot(mod)

summary(mod)$sigma


nsim=5000

bsim <- sim (mod, n.sim=nsim)

newdat <- expand.grid(day=seq(170, 200, length=100),
                      year=levels(subset_df_clim$year))

X <- model.matrix(~year + day + year:day, 
                  data=newdat)

newdat$fit <- X%*%coef(mod)

fitmat <- matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim) fitmat[,i] <- X%*%bsim@coef[i,]
newdat$lower <- apply(fitmat, 1, quantile, prob=0.025)
newdat$upper <- apply(fitmat, 1, quantile, prob=0.975)

head(newdat)

plot(subset_df_clim$day, subset_df_clim$Ra.num, 
     col=c("orange", "blue", "green", "brown")[as.numeric(subset_df_clim$year)])

index <- newdat$year == "2008"
lines(newdat$day[index],newdat$fit[index], lwd=2, col="orange")
lines(newdat$day[index],newdat$lower[index], lwd=1, col="orange")
lines(newdat$day[index],newdat$upper[index], lwd=1, col="orange")

index <- newdat$year == "2011"
lines(newdat$day[index],newdat$fit[index], lwd=2, col="blue")
lines(newdat$day[index],newdat$lower[index], lwd=1, col="blue")
lines(newdat$day[index],newdat$upper[index], lwd=1, col="blue")

index <- newdat$year == "2014"
lines(newdat$day[index],newdat$fit[index], lwd=2, col="green")
lines(newdat$day[index],newdat$lower[index], lwd=1, col="green")
lines(newdat$day[index],newdat$upper[index], lwd=1, col="green")

index <- newdat$year == "2019"
lines(newdat$day[index],newdat$fit[index], lwd=2, col="brown")
lines(newdat$day[index],newdat$lower[index], lwd=1, col="brown")
lines(newdat$day[index],newdat$upper[index], lwd=1, col="brown")

mod1 <- lmer (Ra.num ~ PositiveMeanTemp + year +(1|Data), data= subset_df_clim) #модель перевірки впливу погоди, температури

summary (mod1)
