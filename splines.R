
library(tidyverse)
library(splines)
library(gapminder)

gpmdr <- gapminder::gapminder |> 
  group_by(country, continent) |> 
  filter(year == max(year)) |> 
  ungroup()

gpmdr_scatter <- gpmdr |> 
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point() + 
  theme_light(base_family = "Avenir Next Condensed")
  

gpmdr_scatter + geom_smooth(method = "lm", formula = y ~ x)
gpmdr_scatter + geom_smooth(method = "lm", formula = y ~ log(x))
gpmdr_scatter + geom_smooth(method = "lm", formula = y ~ x + I(x^2))
gpmdr_scatter + geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3))
  
