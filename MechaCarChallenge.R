#library(tidyr)
#library(dplyr)

# =====================================
# MPG Analysis
# =====================================
# input data.
mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=T, stringsAsFactors = F)
#head(mpg_table)
#nrow(mpg_table)
# rename.
mpg_table = rename(mpg_table,
                   length = vehicle.length,
                   weight = vehicle.weight,
                   s_angle = spoiler.angle,
                   clearance = ground.clearance)
#head(mpg_table)

# correlations
cor(mpg_table)

# all factors in the multiple linear regression
lm(mpg ~ length + weight + s_angle + clearance + AWD, data=mpg_table) %>%
  summary()
# only length and clearance
lm(mpg ~ length + clearance, data=mpg_table) %>%
  summary()

# AWD only
awd_mpg_table = subset(mpg_table, AWD == 1)
lm(mpg ~ length + weight + s_angle + clearance, data=awd_mpg_table) %>%
  summary()
# only length and clearance
lm(mpg ~ length + clearance, data=awd_mpg_table) %>%
  summary()

# non-AWD / FWD only
fwd_mpg_table = subset(mpg_table, AWD == 0)
lm(mpg ~ length + weight + s_angle + clearance, data=fwd_mpg_table) %>%
  summary()
# only length and clearance
lm(mpg ~ length + clearance, data=awd_mpg_table) %>%
  summary()

# =====================================
# Coil Analysis
# =====================================
library(ggplot2)
coil_table = read.csv(file='Suspension_Coil.csv',check.names=T, stringsAsFactors = F)
#head(coil_table)
#nrow(coil_table)
summary(coil_table)
# check whether duplicate
table(coil_table$VehicleID) %>% sort(decreasing=TRUE)
# check distribution among lots
table(coil_table$Manufacturing_Lot) %>% sort(decreasing=TRUE)
# variance
var(coil_table$PSI)
# sd
sd(coil_table$PSI)
ggplot(coil_table,aes(y=PSI)) + geom_boxplot()

# =====================================
# Coil T-Test
# =====================================
t.test(coil_table$PSI, mu=1500)

