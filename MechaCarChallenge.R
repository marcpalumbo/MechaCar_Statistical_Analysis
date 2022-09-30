?read.csv
library(dplyr)
# delvierable one

MechaCar_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
?lm()

vehicle_length_regression <- lm(mpg ~ .,data=MechaCar_table)
vehicle_length_regression


summary(vehicle_length_regression)

#deliverable 2

SuspensionCoil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

total_summary <-SuspensionCoil_table %>% summarize(mean=mean(PSI),median=median(PSI),variance=var(PSI),sd=sd(PSI))

lot_summary<- SuspensionCoil_table %>% group_by(Manufacturing_Lot)%>%summarize(mean=mean(PSI),median=median(PSI),variance=var(PSI),sd=sd(PSI))


# deliverable 3 

#creating sample for t test. Arbitrarily picked 30 for sample size


sample_pop <- SuspensionCoil_table %>% sample_n(30)
t.test(log10(sample_pop$PSI),mu=mean(log10(SuspensionCoil_table$PSI)))
?subset()


#creating lot dataframes for t tests on each lot 

#lot1
lot_1 <-subset(SuspensionCoil_table,Manufacturing_Lot == 'Lot1')
#lot2
lot_2 <-subset(SuspensionCoil_table,Manufacturing_Lot == 'Lot2')
#lot3
lot_3 <-subset(SuspensionCoil_table,Manufacturing_Lot == 'Lot3')


# t test using subset argument in t.test for lot 1 
t.test(log10(SuspensionCoil_table$PSI), mu=mean(log10(SuspensionCoil_table$PSI)),,,,,,,subset(SuspensionCoil_table,Manufacturing_Lot == 'Lot_1'))

#t_test using subset function outside of t.test and using the t.test function with less syntax for comparison - lot 1 

t.test(lot_1$PSI,mu=mean(log10(SuspensionCoil_table$PSI)))

#t_test using subset function outside of t.test and using the t.test function with less syntax for comparison - lot 

t.test(lot_2$PSI,mu=mean(log10(SuspensionCoil_table$PSI)))
#t_test using subset function outside of t.test and using the t.test function with less syntax for comparison - lot 3
t.test(lot_3$PSI,mu=mean(log10(SuspensionCoil_table$PSI)))



