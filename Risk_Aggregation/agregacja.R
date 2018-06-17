
library(dplyr)
library(CDVine)

##########################################
## Intrduction to the concept of Copula ## 
##########################################

sample_size = 15000

# sampling three indpendent uniformly distibutied variables/vectors
uniform_distibutions <- matrix(runif(3*sample_size),sample_size,3) 
hist(uniform_distibutions[,1])

# converting them into normally distibuted with use of inverse cumulative function techniqe (analitical transformation)
normal_distibutions <- qnorm(uniform_distibutions)
plot(density(normal_distibutions[,1]))

# defining dependency parameter
linear_correlation =0.4

# inputing it into correlation matrix as in multivariat distibution
cor_matrix <- matrix(linear_correlation, ncol = 3, nrow = 3)
diag(cor_matrix) <- 1
cor_matrix

# cholesky decomposition - techniqe for conversion of correlation matrix
chol <- chol(cor_matrix)

# incorporating dependency between normal vectors
n_cor <- normal_distibutions %*% chol

# convering normal vectors with dependency into uniform vector
u_cor <- pnorm(n_cor %>% as.matrix)

# convering uniform vectors with dependency into specifc marginal distibution
normal_with_dependency <- qnorm(u_cor) # normal
student_t_with_dependency <- qt(u_cor,df=6) # student-t with 4-degree of freedom

# check their linear correlation
cor(normal_with_dependency)
cor(student_t_with_dependency)

# check kendall tau independence from margins - reduce number of simulations
#cor(student_t_with_dependency, method = 'kendall')


##########################
## Copula Vizualization ##
##########################

plot(normal_distibutions[,1],normal_distibutions[,2], main = 'no dependency')
plot(n_cor[,1],n_cor[,2], main = 'multivariat normal distibution')
plot(normal_with_dependency[,1],normal_with_dependency[,2], main = 'gaussian copula with normal margins') 
plot(student_t_with_dependency[,1],student_t_with_dependency[,2], main = 'gaussian copula with student t margins') 



###########################################
## Dependency between those risk drivers ##
###########################################
## we want to capture dependency not just in body, but also in tails and linear correlation will not capture this ##

# plot dependency
plot(risk_drivers_trans$YR1_log_return_N, risk_drivers_trans$WIG_log_return_N, pch = 19,cex = 1.2,
     xlim = c(-4,4), ylim = c(-4,4), xlab = 'YR1 log return - normalized', ylab = 'EQ_WIG log return - normalized')
grid()

plot(risk_drivers_trans$YR1_log_return_N, risk_drivers_trans$YR10_log_return_N, pch = 19,cex = 1.2,
     xlab = 'YR1 log return - normalized', ylab = 'EQ_WIG log return - normalized')
grid()

# tranformation into uniform vectors - since in that implementation copulas are fitted on uniformly distibuted margins
# we are using previously perfomred distibution fitting
uniform_IR_1 <- risk_drivers_trans %>% select(YR1_log_return) %>% pghyp(YR1_NIG)
uniform_WIG <- risk_drivers_trans %>% select(WIG_log_return) %>% pghyp(EQ_WIG_NIG)
hist(uniform_IR_1) # since we are transforming empirical data - it will be as good as our distibution fitting
hist(uniform_WIG)

# selecting most appropriate copula based on AIC criterion
(fitted_copula <- BiCopSelect(u1 = uniform_IR_1,u2 = uniform_WIG))

# since rotated Gumbel copula was choosen we will simulated from this type of copula to get intuition on its shape
simulated_rotated_Gumbel_copula <- BiCopSim(10000,family = fitted_copula$family, par= -3)
plot(qnorm(simulated_rotated_Gumbel_copula))

# simulating copula based of choosen copula and its parameter
simulated_from_fitted_copula<- BiCopSim(2000,family = fitted_copula$family, par= fitted_copula$par, par2 =fitted_copula$par2)
simulated_IR <- simulated_from_fitted_copula[,1] %>% qghyp(YR1_NIG)
simulated_WIG <- simulated_from_fitted_copula[,2] %>% qghyp(EQ_WIG_NIG)


# ploting both empirical data and margins simulated from fitted copula
plot(simulated_IR,simulated_WIG, xlab = 'log returns of IR month to month', ylab = 'log returns of equity index WIG month to month')
points(risk_drivers_trans$YR1_log_return, risk_drivers_trans$WIG_log_return, col = 'red', pch = 19)
grid()
legend('bottomleft', c('empirical data', 'simulated from fitted copula'), col = c('red','black'), pch = c(19,1))



# same steps to build framwork for relation between IR_1Y and IR_10Y
risk_drivers_trans <- risk_drivers %>% 
  mutate(WIG_log = log(EQ_WIG)) %>% 
  mutate(WIG_log_return = WIG_log - lag(WIG_log)) %>%
  mutate(YR1_log_return = log(YR1) - lag(log(YR1))) %>%
  mutate(YR10_log_return = log(YR10) - lag(log(YR10))) %>%
  filter(is.na(YR1_log_return) ==F) %>%
  mutate(YR1_log_return_N = scale(YR1_log_return),
         YR10_log_return_N = scale(YR10_log_return))

YR10_NIG <- fit.NIGuv(risk_drivers_trans$YR10_log_return, silent = T)
uniform_IR_10 <- risk_drivers_trans %>% select(YR10_log_return) %>% pghyp(YR10_NIG)
(fitted_copula <- BiCopSelect(u1 = uniform_IR_1,u2 = uniform_IR_10))
simulated_from_fitted_copula<- BiCopSim(1000,family = fitted_copula$family, par= fitted_copula$par, par2 =fitted_copula$par2)
simulated_IR <- simulated_from_fitted_copula[,1] %>% qghyp(YR1_NIG)
simulated_IR10 <- simulated_from_fitted_copula[,2] %>% qghyp(YR10_NIG)

plot(simulated_IR,simulated_IR10, xlab = 'log returns of IR (1Y) month to month', ylab = 'log returns of IR (10Y) month to month')
points(risk_drivers_trans$YR1_log_return, risk_drivers_trans$YR10_log_return, col = 'red', pch = 19)
grid()
legend('bottomleft', c('empirical data', 'simulated from fitted copula'), col = c('red','black'), pch = c(19,1))


# 3D density plot
library(rgl); library(MASS)
open3d()
mfrow3d(1, 2)
den3d_emp <- kde2d(risk_drivers_trans$YR1_log_return, risk_drivers_trans$YR10_log_return)
persp3d(den3d_emp,col="lightblue", box = T, ticktype = 'detailed',xlim=c(-0.2,0.2),ylim =c(-0.2,0.2), zlim = c(0,100))
next3d()
den3d_sim <- kde2d(simulated_IR, simulated_IR10)
persp3d(den3d_sim,col="chartreuse2",add = F,xlim=c(-0.2,0.2),ylim =c(-0.2,0.2))
