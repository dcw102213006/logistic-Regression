# need to import library(ROCR)
require(ROCR)
DPP = function(pred, y, yval, fig=TRUE, h=0, b=50) {
  ROCRpred = prediction(pred, y)
  A = as.numeric(performance(ROCRpred, "auc")@y.values)
  
  if( fig ) {
    str = sprintf("Distribution of Predicted Probabilities\nAUC = %.4f", A)
    
    if(h > 0) {
      hist(pred[y != yval], b, col=rgb(0,1,0,1/4), xlim=c(0,1), ylim=c(0, h), 
           main=str, xlab="Y = {0:Green | 1:Red}") }
    else {
      hist(pred[y != yval], b, col=rgb(0,1,0,1/4), xlim=c(0,1),
           main=str, xlab="Y = {0:Green | 1:Red}") }
    
    hist(pred[y == yval], breaks=b, col=rgb(1,0,0,1/4), add=T)
  }
  
  A
}
