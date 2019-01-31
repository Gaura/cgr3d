require(plotly)

plot3dseq <- function(seq, split = 1/2){
      startPos <- c(50,25*sqrt(3),25*sqrt(3))
      positions <- matrix(rep(0, nchar(seq)), nrow = nchar(seq), ncol = 3)
      for(i in 2:nrow(positions)){
            positions[i,]  <- (positions[i-1,] + vertices[[substr(seq, i-1, i-1)]])*split
      }

      p <- plot_ly(type="scatter3d", x = positions[,1], y = positions[,2], z = positions[,3],
                   marker = (list (size = 3))
                   )
      return(p)
}
