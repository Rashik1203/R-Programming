rm(list = ls())

data <- read_excel("F:/BUP Reading Materials/all/MDPI/ARDL AND NARDL.xlsx")
install.packages("devtools")
install.packages("urca")
library("urca")
library("devtools")
names(data)
## zivot and Andrew test with label data

dz.intercept_gdp=ur.za(data$LGDP,model = "intercept")
summary(dz.intercept_gdp)
dz.trend_gdp=ur.za(data$LGDP,model = "trend")
summary(dz.label_gdp)
dz.both_gdp=ur.za(data$LGDP,model = "both")
summary(dz.both_gdp)
plot(dz.both_gdp)

####LREM########
dz.intercept_LREM=ur.za(data$LREM,model = "intercept")
summary(dz.intercept_LREM)
dz.trend_LREM=ur.za(data$LREM,model = "trend")
summary(dz.label_LREM)
dz.both_LREM=ur.za(data$LREM,model = "both")
summary(dz.both_LREM)

#########LODA#######

dz.intercept_LODA=ur.za(data$LODA,model = "intercept")
summary(dz.intercept_LODA)
dz.trend_LODA=ur.za(data$LODA,model = "trend")
summary(dz.label_LODA)
dz.both_LODA=ur.za(data$LODA,model = "both")
summary(dz.both_LODA)


#######LEXP###########
dz.intercept_LEXP=ur.za(data$LEXP,model = "intercept")
summary(dz.intercept_LEXP)
plot(dz.intercept_LEXP)
break_point <-dz.intercept_LEXP@breakpoint
cat("Break Point:", break_point, "\n")

# Print the Zivot-Andrews test results
print(za_result)
dz.trend_LEXP=ur.za(data$LEXP,model = "trend")
summary(dz.label_LEXP)
dz.both_LEXP=ur.za(data$LEXP,model = "both")
summary(dz.both_LEXP)

########################################## zivot and Andrew test with first difference data#####################################################################

dz.intercept_gdp =ur.za(diff(data$LGDP),model="intercept")
summary(dz.intercept_gdp )
dz.trend_gdp=ur.za(diff(data$LGDP),model = "trend")
summary(dz.label)
dz.both_gdp=ur.za(diff(data$LGDP),model = "both")
summary(dz.both_gdp)


####LREM########
dz.intercept_LREM=ur.za(diff(data$LREM),model = "intercept")
summary(dz.intercept_LREM)
dz.trend_LREM=ur.za(diff(data$LREM),model = "trend")
summary(dz.label_LREM)
dz.both_LREM=ur.za(diff(data$LREM),model = "both")
summary(dz.both_LREM)

#########LODA#######

dz.intercept_LODA=ur.za(diff(data$LODA),model = "intercept")
summary(dz.intercept_LODA)
dz.trend_LODA=ur.za(diff(data$ODA),model = "trend")
summary(dz.label_LODA)
dz.both_LODA=ur.za(diff(data$ODA),model = "both")
summary(dz.both_LODA)


#######LEXP###########
dz.intercept_LEXP=ur.za(diff(data$LEXP),model = "intercept")
summary(dz.intercept_LEXP)
dz.trend_LEXP=ur.za(diff(data$LEXP),model = "trend")
summary(dz.label_LEXP)
dz.both_LEXP=ur.za(diff(data$LEXP),model = "both")
summary(dz.both_LEXP)