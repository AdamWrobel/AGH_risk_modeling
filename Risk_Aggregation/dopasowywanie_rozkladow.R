## two market risk drivers: equity index (WIG) and IR (PLN_IR_1Y) ##

library(ghyp)


## Eqity (WIG) ##
EQ_WIG <- read.csv('EQ_WIG.csv')
EQ_WIG <- EQ_WIG %>% group_by(year_month = substr(Date,1,7)) %>% 
  filter(substr(Date,9,10) == max(substr(Date,9,10))) %>% data.frame
plot(EQ_WIG$EQ_WIG, type = 'l')

## IR (PLN_IR_1R) ##
IR_PLN <-read.csv('IR_PLN.csv') %>% mutate(year_month = substr(Date,1,7))
plot(IR_PLN$YR1, type = 'l')
matplot(IR_PLN[,2:4], type = 'l') # for other tenors

## bind both
risk_drivers <- IR_PLN %>% left_join(EQ_WIG, by = 'year_month') %>% select(-contains('Date'))

# transofrm data
risk_drivers_trans <- risk_drivers %>% 
  mutate(WIG_log = log(EQ_WIG),
         YR1_log = log(YR1)) %>% 
  mutate(WIG_log_return = WIG_log - lag(WIG_log)) %>%
  mutate(YR1_log_return = YR1_log - lag(YR1_log)) %>%
  filter(is.na(WIG_log_return) ==F) %>%
  mutate(YR1_log_return_N = scale(YR1_log_return),WIG_log_return_N = scale(WIG_log_return))

# plot data
risk_drivers_trans %>% select(YR1_log_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return PDF')
risk_drivers_trans %>% select(WIG_log_return) %>% as.matrix %>% density %>% plot(main = 'EQ_WIG log return PDF',col = 'red')
risk_drivers_trans %>% select(YR1_log_return_N) %>% as.matrix %>% density %>% plot(main = 'Normalized log return PDF')
risk_drivers_trans %>% select(WIG_log_return_N) %>% as.matrix %>% density %>% lines(col = 'red')
grid()
legend('topright', c('YR1_normalized','EQ_WIG_normalized'), col = c('red','black'), lty=1)


##########################
## fit NIG distribution ## 
##########################

YR1_NIG <- fit.NIGuv(risk_drivers_trans$YR1_log_return, silent = T)
EQ_WIG_NIG <- fit.NIGuv(risk_drivers_trans$WIG_log_return, silent = T)


###############################
## simulate from distibution ## 
###############################

risk_drivers_trans %>% select(YR1_log_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return probability density function',lwd = 2)
qghyp(seq(from=0, to =1, by = 0.001),YR1_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topright',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2),lwd = 2, col = c('black','cadetblue3'))
Sys.sleep(3)
risk_drivers_trans %>% select(WIG_log_return) %>% as.matrix %>% density %>% plot(main = 'EQ WIG log return probability density function',lwd = 2, ylim = c(0,7))
qghyp(seq(from=0, to =1, by = 0.001),EQ_WIG_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topleft',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2), lwd = 2, col = c('black','cadetblue3'))



