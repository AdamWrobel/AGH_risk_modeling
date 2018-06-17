## two market risk drivers: equity index and IR ##

combined_shorter <- combined %>% filter(as.numeric(substr(Date,1,4)) >= 2000)
SP_plot <- ggplot(combined_shorter, aes(x = Date, y = SP500)) + 
  geom_line() + ylab("Indeks S&P500") +xlab("Data")
png('SP500_ts.png', width = 1920 ,height = 1080,res = 300)
print(SP_plot)
dev.off()


png('SP_ts_long.png', width = 1920*1.5, height = 1080,res = 300)
print(SP_plot)
dev.off()

IR_plot <- ggplot(combined_shorter, aes(x = Date, y = Long.Interest.Rate)) + 
  geom_line() + ylab("IR") +xlab("Data")

library(gridExtra)
gA=ggplot_gtable(ggplot_build(SP_plot))
gB=ggplot_gtable(ggplot_build(IR_plot))
maxWidth = grid::unit.pmax(gA$widths[2:3], gB$widths[2:3])
gA$widths[2:3] <- as.list(maxWidth)
gB$widths[2:3] <- as.list(maxWidth)

png('IR&SP500.png', width = 1920, height = 1080,res = 300)
grid.arrange(gA, gB)
dev.off()

SP_d <-
ggplot(combined_shorter, aes(x = SP500_return)) + 
  geom_histogram(aes(y = ..density..), fill = 'coral1', colour = 'black') +
  geom_line(aes(x = SP500_return), adjust = 1, colour = 'dodgerblue', lwd = 1.5, stat="density") +
  xlab('S&P500 quarterly returns')

IR_d <-
ggplot(combined_shorter, aes(x = IR_return)) + 
  geom_histogram(aes(y = ..density..), fill = 'coral1', colour = 'black') +
  geom_line(aes(x = IR_return), adjust = 0.85, colour = 'dodgerblue', lwd = 1.5, stat="density") +
  xlab('IR quarterly returns')

png('SP500_density.png', width = 1920, height = 1080,res = 300)
grid.arrange(SP_d,IR_d, ncol = 2)
dev.off()
      
  



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
