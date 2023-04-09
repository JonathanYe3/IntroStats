# Part G

## i)

#' @param m number of draws in the sample
disk_draw<-function(m){
      radial_component<-runif(m, 0, 1)
      angular_component<-runif(m, 0, 2*pi)
      x<-matrix(0,m,1)
      1
      y<-matrix(0,m,1)
      for (i in 1:m) {
            x[i]<-sqrt(radial_component[i])*cos(angular_component[i])
            y[i]<-sqrt(radial_component[i])*sin(angular_component[i])
      }
      disk_draw_mat<-cbind(x,y)
      return(disk_draw_mat)
}

## ii)

#' @param a 2 column matrix, preferably output from disk_draw
MLE_theta_both_coords<-function(A)
{
      z<-matrix(0,nrow(A),1)
      for (i in 1: nrow(A)){
            z[i]=A[i,1]^2 + A[i,2]^2
      }
      MLE<-sqrt(max(z))
      return(MLE)
}

#' @param vector a vector, i.e. either the x column or y column from disk_draw
#' @example 
#' test <- draw_disk(10)
#' MOM_x <- MOM_theta_one_coord(test[,1])
#' MOM_y <- MOM_theta_one_coord(test[,2])
MOM_theta_one_coord <- function(vector){
      
      n <- length(vector)
      MOM <- 2 * sqrt((1/n) * sum(vector^2))
      return(MOM)
}

## iii)

#' @param k number of iterations (10,000)
#' @param m sample size (10)
simulate <- function(k, m){
      W <- matrix(0, k, 3)
      
      for (i in 1:k){
            sample <- disk_draw(m)
            W[i,1] <- MLE_theta_both_coords(sample)
            W[i,2] <- MOM_theta_one_coord(sample[,1])
            W[i,3] <- MOM_theta_one_coord(sample[,2])
      }
      return(W)
}

# Part h
W <- simulate(10000, 10)

bias <- function(vector, theta_true){
      bias <- (sum(vector-theta_true))/length(vector)
      return(bias)
}

## MLE 
bias_MLE <- bias(W[,1],1)
var_MLE <- var(W[,1])
MSE_MLE <- var_MLE + bias_MLE^2
      
## MOM X
bias_MOM_X <- bias(W[,2],1)
var_MOM_X <- var(W[,2])
MSE_MOM_X <- var_MOM_X + bias_MOM_X^2

## MOM Y
bias_MOM_Y <- bias(W[,3],1)
var_MOM_Y <- var(W[,3])
MSE_MOM_Y <- var_MOM_Y + bias_MOM_Y^2

# Part g
correlation_coeff <- cor(W[,2], W[,3])


