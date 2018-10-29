# Introducción a R
# Comandos básicos
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

install.packages("maps")
library(maps)
install.packages("readr")
library(readr)
library(dplyr)

# Análisis exploratorio.
ozone <- read_csv("hourly_44201_2014.csv", na=c("", "NA"), col_types="ccccinnccDtDtnccccccccD")
names(ozone)
names(ozone) <- make.names(names(ozone))
names(ozone)
nrow(ozone)
ncol(ozone)
str(ozone)
head(ozone[, c(6:7,10)])
tail(ozone[, c(6:7,10)])
head(table(ozone$Time.Local))
dim(ozone)
summary(ozone)
ozone$State.Code
ozone$State.Name
ozone$Time.Local[1:20]

# Uso de dplyr.
filter(ozone, strptime(Time.Local, "%H:%M:%S") <= strptime("13:14:00", "%H:%M:%S")) %>%
  select(State.Code, County.Code, Date.Local, Time.Local, Sample.Measurement)

filter(ozone, strptime(Time.Local, "%H:%M:%S") == strptime("03:00:00", "%H:%M:%S")) %>%
  select(State.Code, County.Code, Date.Local, Time.Local, Sample.Measurement)

# Tres errores difíciles de rastrear.
filter(ozone, strptime(Time.Local, "%H:%M:%S") == strptime("03:00:00", "%H:%M:%S")) %>%
  select(State.Code, County.Code, Date.Local, Time.Local, Sample.Measurement)
filter(ozone, Time.Local == as.Date("03:00:00", "%H:%M:%S")) %>%
  select(State.Code, County.Code, Date.Local, Time.Local, Sample.Measurement)
filter(ozone, Time.Local == "03:00:00") %>%
  select(State.Code, County.Code, Date.Local, Time.Local, Sample.Measurement)
#

filter(ozone, State.Code == "36" & County.Code == "033" & Date.Local == "2014-09-30") %>%
  select(Date.Local, Time.Local, Sample.Measurement) %>%
  as.data.frame()
select(ozone, State.Name) %>% unique %>% nrow
select(ozone, State.Code) %>% unique %>% nrow
unique(ozone$State.Name)
unique(ozone$State.Code)
summary(ozone$Sample.Measurement)
quantile(ozone$Sample.Measurement, seq(0, 1, 0.1))
par(las=2, mar=c(10, 4, 2, 2), cex.axis=0.8)
boxplot(Sample.Measurement ~ State.Code, ozone, range=0, ylab="Ozone level (ppm)")

# Datos representados sobre mapas.
map("state")
abline(v=-100, lwd=3, col="red")
text(-120, 30, "West")
text(-75, 30, "East")
ozone$region <- factor(ifelse(ozone$Longitude < -100, "west", "east"))
group_by(ozone, region) %>%
  summarize(mean=mean(Sample.Measurement, na.rm=TRUE), median=median(Sample.Measurement, na.rm=TRUE))
boxplot(Sample.Measurement ~ region, ozone, range=0)

filter(ozone, State.Name != "Puerto Rico"
       & State.Name != "Georgia"
       & State.Name != "Hawaii") %>%
    group_by(region) %>%
    summarize(mean = mean(Sample.Measurement, na.rm = TRUE),
              median = median(Sample.Measurement, na.rm = TRUE))

# Distribución normal para el valor 30 en el vector x.
x <- c(25, 20, 15, 5, 30, 7, 5, 10, 12, 40, 30, 30, 10, 25, 10, 20, 10, 10, 25, 5)
pnorm(30, mean = mean(x), sd = sd(x), lower.tail = FALSE)

# Midamos el tiempo que tarda el proceso anterior.
system.time(pnorm(30, mean = mean(x), sd = sd(x), lower.tail = FALSE))

# Salvar la sesión en un archivo.
savehistory("~/projects/Modelos de datos/EjemplosR/0-intro_2.R")
