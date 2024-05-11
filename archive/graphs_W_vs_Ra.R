library (ggplot2)
library(lubridate)    #створення нової колонки з даними про день від початку року 
df <- read.csv("Homolsha_all_years.csv")

#?sub 
df$Ra <- sub(",", ".", df$Ra) #заміна ком на крапки в промірах передпліччя
df$W <- sub(",", ".", df$W)

df$date <- as.Date(df$Data, "%d.%m.%Y") # convert date to R Data format
df$Place <- as.factor(df$Place)
df$Species <- as.factor(df$Species)
df$sex <- as.factor(df$sex)
df$age <- as.factor(df$age)
df$Year <- year(df$date) #виокремлення року
df$Year <-as.factor (df$Year)


df$Ra <-as.numeric (df$Ra) #присвоєння промірам нумеричного типу даних
df$W <-as.numeric (df$W)

table (df$Species,df$age)

ggplot(data = df, aes(Ra, W )) +
  geom_point()

#виокремлюю сабсет з тільки двома видами, що цікавлять
df1 <- subset(
  df, # обєкт з якого відбираються рядки
  Species %in% c ("Nyctalus noctula", "Myotis daubentonii") & 
               age == "sad" #види та вікова група які потрібні
  ) 


?subset

table(df1$Species)

#графік скаттерплот для двох видів разом залежності передпліччя від ваги
ggplot(data = df1, aes(Ra, W, colour = Species )) +
  geom_point(alpha = 0.5) + 
  geom_smooth(
    # aes(colour = Species), 
    method = "lm"
    ) +
  facet_wrap(.~Year) +
  theme_bw()

ggsave("MDOCvsNNOC_sad.jpg", units = "mm", width = 120,
       height = 80, dpi = 300)

#скаттерплот залежності передпліччя від ваги для даубентонів з лінією регресії
ggplot(data = subset(
df1, df1$Species == "Myotis daubentonii"),
aes(Ra, W )) +
  geom_point(colour = "blue", alpha = 0.5) + 
  geom_smooth(
    # aes(colour = Species), 
    colour = "blue",
    method = "lm"
  ) +
  labs (x= "forearm lenght", y = "body weight",
        title = "Myotis daubentonii") +
  facet_wrap(.~Year) +
  theme_bw()

ggsave("MDAU_body_condition.jpg", units = "mm", width = 120,
       height = 80, dpi = 300)

#скаттерплот залежності передпліччя від ваги для рижиків з лінією регресії
ggplot(data = subset(
  df1, df1$Species == "Nyctalus noctula"),
  aes(Ra, W )) +
  geom_point(colour = "salmon", alpha = 0.5) + 
  geom_smooth(
    # aes(colour = Species),
    colour = "red",
    method = "lm"
  ) +
  labs (x= "forearm lenght", y = "body weight" ) +
  facet_wrap(.~Year) +
  theme_bw()


ggsave("NNOC_body_condition.jpg", units = "mm", width = 120,
       height = 80, dpi = 300)


#скаттерплот залежності передпліччя від ваги для рижиків в одному вікні за усі роки
ggplot(data = subset(
  df, df$Species == "Nyctalus noctula"),
  aes(Ra, W )) +
  geom_point(colour = "salmon", alpha = 0.5) + 
  geom_smooth(
    aes(colour = Year),
    #colour = Year,
    method = "lm"
  ) +
  labs (x= "forearm lenght", y = "body weight", 
        title = "Nyctalus noctula" ) +
  #facet_wrap(.~Year) +
  theme_bw()

ggsave("NNOC_body_condition_allyears.jpg", units = "mm", width = 160,
       height = 120, dpi = 300)

#скаттерплот залежності передпліччя від ваги для даубентонів в одному вікні за усі роки
ggplot(data = subset(
  df, df$Species == "Myotis daubentonii"),
  aes(Ra, W )) +
  geom_point(colour = "blue", alpha = 0.5) + 
  geom_smooth(
    aes(colour = Year),
    #colour = Year,
    method = "lm"
  ) +
  labs (x= "forearm lenght", y = "body weight", 
        title = "Myotis daubentonii" ) +
  #facet_wrap(.~Year) +
  theme_bw()

ggsave("MDAU_body_condition_allyears.jpg", units = "mm", width = 160,
       height = 120, dpi = 300)


#аналіз того самого для натузиків 

df <- read.csv("Homolsha_all_years.csv")

#?sub 
df$Ra <- sub(",", ".", df$Ra) #заміна ком на крапки в промірах передпліччя
df$W <- sub(",", ".", df$W)

df$date <- as.Date(df$Data, "%d.%m.%Y") # convert date to R Data format
df$Place <- as.factor(df$Place)
df$Species <- as.factor(df$Species)
df$sex <- as.factor(df$sex)
df$age <- as.factor(df$age)
df$Year <- year(df$date) #виокремлення року
df$Year <-as.factor (df$Year)

df$Ra <-as.numeric (df$Ra)
df$W <-as.numeric (df$W)


df <- subset(
  df, # обєкт з якого відбираються рядки
  Species %in% c ("Pipistrellus nathusii") & 
    age == "sad" #види та вікова група які потрібні
) 

ggplot(data = subset(
  df, df$Species == "Pipistrellus nathusii" & df$Ra > 30  ),
  aes(Ra, W )) +
  geom_point(colour = "blue", alpha = 0.5) + 
  geom_smooth(
    aes(colour = Year),
    #colour = Year,
    method = "lm"
  ) +
  labs (x= "forearm lenght", y = "body weight", 
        title = "Pipistrellus nathusii" ) +
  #facet_wrap(.~Year) +
  theme_bw()

ggsave("PNAT_body_condition_allyears.jpg", units = "mm", width = 160,
       height = 120, dpi = 300)



