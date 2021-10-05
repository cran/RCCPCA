#' Retaind component criterion
#' @export
#' @import stats
#' @import graphics
RCC_PCA<-function(x)
{
  V=ncol(x)
  m=0
  M=0
  RESULTS<-matrix(0, nrow= V, ncol = 1)
  SM<-matrix(0, nrow= V, ncol = 1)

  Analysis=prcomp(x)
  U=Analysis$rotation
  Lambda=(Analysis$sdev)^2
  maxmse=mean(x^2)


  for (i in 1:V)
  {
    Transform=x%*%U[,1:i]
    recon=Transform%*%t(U[,1:i])
    m=mean((x-recon)^2)
    SM[i,1]<-m
  }
  SM=SM

  for (i in 1:V)
  {
    sum(Lambda[1:i])/sum(Lambda)
    Transform=x%*%U[,1:i]
    recon=Transform%*%t(U[,1:i])
    M=mean((x-recon)^2)
    RESULTS[i,1]<-2-((maxmse-M)/maxmse)-(sum(Lambda[1:i])/sum(Lambda))+(i*2/V)
  }
  RESULTS=RESULTS
  plot(seq(1:V),RESULTS[,1], type = "o",main ="RCC" ,xlab = "Number of Principal Components", ylab = "value")
  return(RESULTS)
}
