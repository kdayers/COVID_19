library(ggplot2)
library(tidyverse)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(drv~cyl)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=drv,y=cyl))

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))+
  geom_point(mapping=aes(x=displ,y=hwy,color=drv),
             show.legend=FALSE)

ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(color=class))+
  geom_smooth(data = filter(mpg, class == "subcompact"))

