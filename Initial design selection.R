Historycal_sales$PriceReg
log_PriceReg <- log(Historycal_sales$PriceReg)

#Estimate parameters of the log-normal distribution (mean and standard deviation of log-transformed data)
meanlog_R <- mean(log_PriceReg)  # mean of log-transformed data
meanlog_R<- sd(log_PriceReg)  # standard deviation of log-transformed data

##Do that similar prosidere for user prise and net price

meanlog_R <- 1.928923  # register_price
meanlog_R <- 0.3100553

# Function to generate log-normal data within a specified range
generate_lognormal <- function(n, meanlog, sdlog, min_val, max_val) {
  rg <- rlnorm(n, meanlog, sdlog)
  rg <- rg[rg >= min_val & rg <= max_val]  # Keep only values within the range
  
  while(length(rg) < n) {  # If fewer values, generate more until the required number is reached
    new_rg <- rlnorm(n - length(rg), meanlog, sdlog)
    rg <- c(rg, new_rg[new_rg >= min_val & new_rg <= max_val])
  }
  
  return(rg)
}

# Generate 6 random values within the range 3 and 450
rg <- generate_lognormal(6, meanlog_R, sdlog_R, 1, 5)
rg<-exp(rg)
rg



meanlog_u <- 1.666769 #user_price
sdlog_u <-  0.4130416

# Function to generate log-normal data within a specified range
generate_lognormal <- function(n, meanlog, sdlog, min_val, max_val) {
  up <- rlnorm(n, meanlog, sdlog)
  up <- up[up >= min_val & up <= max_val]  # Keep only values within the range
  
  while(length(up) < n) {  # If fewer values, generate more until the required number is reached
    new_up <- rlnorm(n - length(up), meanlog, sdlog)
    up<- c(up, new_up[new_up >= min_val & new_up <= max_val])
  }
  
  return(up)
}

# Generate 6 random values within the range 3 and 450
up <- generate_lognormal(6, meanlog_u, sdlog_u, 1, 5)
up<-exp(up)



meanlog_N <- 1.569878 #net_price
sdlog_N <-  0.374344

# Function to generate log-normal data within a specified range
generate_lognormal <- function(n, meanlog, sdlog, min_val, max_val) {
  np <- rlnorm(n, meanlog, sdlog)
  np <- np[np >= min_val & np <= max_val]  # Keep only values within the range
  
  while(length(np) < n) {  # If fewer values, generate more until the required number is reached
    new_np <- rlnorm(n - length(np), meanlog, sdlog)
    np<- c(np, new_np[new_np >= min_val & new_np <= max_val])
  }
  
  return(np)
}

# Generate 6 random values within the range 3 and 450
np <- generate_lognormal(6, meanlog_N, sdlog_N, 1, 6)
np
np<-exp(np)

ic<-sample(1:150,6)
m<-sample(c(1, 2), size = 6, replace = TRUE)

d<-matrix(c(m,rg,ic,up,np),byrow = F,nrow=6)
d