install.packages("tidyverse")
install.packages("knitr")

library(tidyverse)
library(knitr)



##day 1

bi1 <-100
demand <- runif(1, min = 80, max=130)
production1 <- 100
ei1 <- bi1 + production1 - demand
ei1


##day 2
runs <- 259
pro_less <- production1 + 100
generate.path <- function(days)
{
 { days <- 259
   bi = ei1
   demand1 <- runif(days, min = 80, max = 130) }
 { production2 <-100
    if(demand1<50) 
    {
      pro_less
    } 
   else {production2}
   }
  ei = bi + production1 - demand1;
  ei
} 


mc.closing <- replicate(runs,generate.path())
mc.closing

hist(mc.closing)
plot(density(mc.closing),main= "Density Plot", col="magenta", lwd=2) 
ggplot(mc.closing)



