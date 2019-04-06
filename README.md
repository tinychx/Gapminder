# Gapminder
The data we can find within Gapminder range from the number of HIV cases, life expectancy, infant malnutrition, the number of children a woman has had, CO2 emissions, the age of first marriage, income per capita , gross domestic product, energy expenditure, energy consumption.The tool allows us to easily correlate any of the indicators and through a temporary animation have the possibility of analyzing the temporal evolution. In addition, we can choose the countries we want to study. For example, we can analyze the relationship between the age of first marriage and the number of children per woman.
library(gapminder)
library(dplyr)
# Library packages
# install.packages(c("gapminder", "plotly"))
library(gapminder)
library(plotly)

# About gapminder data
gapminder_dim <- dim(gapminder)
gapminder_years <- unique(gapminder$year)
gapminder_n_countries <- length(unique(gapminder$country))
## Warning: package 'HIV' was built under R version
##
## Attaching package: 'HIV'
## The following objects are masked from 'package:stats':
##
## filter, lag
## The following objects are masked from 'package:base':
##
## intersect, setdiff, setequal, union
data(gapminder)
gapminder <-
gapminder %>% mutate(lgdpPercap = log10(gdpPercap),
lPop = log10(pop))
gm1952 <- gapminder %>% filter(year==1952) %>%
filter(continent=="Africa" | continent=="Europe" | continent=="Asia") %>%
select(-year,-gdpPercap,-pop)
2. Fit a model for life expectancy that includes your log-GDP and log-population variables and interactions
between these variables and continent.
Solution:
gfit <- lm(lifeExp ~ continent*(lgdpPercap + lPop), data=gm1952)
round(summary(gfit)$coefficients,4)
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) 29.3523 11.3993 2.5749 0.0114
## continentAsia -26.9694 20.2790 -1.3299 0.1864
## continentEurope -31.2649 21.1971 -1.4750 0.1432
## lgdpPercap 5.3098 2.5995 2.0426 0.0436
## lPop -0.9654 1.2735 -0.7581 0.4501
## continentAsia:lgdpPercap 5.6722 3.6725 1.5445 0.1254
## continentEurope:lgdpPercap 16.0610 4.8152 3.3355 0.0012
## continentAsia:lPop 2.2790 1.9740 1.1545 0.2509
## continentEurope:lPop -0.8473 2.2176 -0.3821 0.7032
gfitRed <- lm(lifeExp ~ continent*lgdpPercap, data=gm1952)
anova(gfitRed,gfit)
## Analysis of Variance Table
##
## Model 1: lifeExp ~ continent * lgdpPercap
## Model 2: lifeExp ~ continent * (lgdpPercap + lPop)
## Res.Df RSS Df Sum of Sq F Pr(>F)
## 1 109 3544.7
## 2 106 3468.5 3 76.244 0.7767 0.5095
We retain the null hypothesis that population does not affect life expectancy.
4. In light of part (3) we drop population from the model and use the reduced model. Write the fitted
model, ˆ f(X), for life expectency for countries in Europe. Interpret the slope of this equation.
round(summary(gfitRed)$coefficients,3)
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) 23.053 7.779 2.964 0.004
## continentAsia -7.220 10.153 -0.711 0.479
## continentEurope -34.999 16.660 -2.101 0.038
## lgdpPercap 5.382 2.590 2.078 0.040
## continentAsia:lgdpPercap 4.196 3.288 1.276 0.205
## continentEurope:lgdpPercap 15.350 4.757 3.227 0.002
## Warning: package 'ggplot2' was built under R version 3.4.4
gAug <- data.frame(gm1952,fitted=fitted(gfitRed),residuals=residuals(gfitRed))
ggplot(gAug,aes(x=fitted,y=residuals)) +
geom_point() + geom_smooth()
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
cooks = cooks.distance(gfitRed))
filter(gmAug, abs(studRes)>3) # None
## [1] country continent lifeExp lgdpPercap lPop studRes
## [7] hats cooks
## <0 rows> (or 0-length row.names)
filter(gmAug, hats>3*6/115) # Bosnia and Herzegovina (low GDP), Kuwait (high GDP)
## country continent lifeExp lgdpPercap lPop studRes
## 1 Bosnia and Herzegovina Europe 53.820 2.988351 6.44576 0.7803655
## 2 Kuwait Asia 55.565 5.034959 5.20412 -2.0634119
## hats cooks
## 1 0.2694790 0.03757492
## 2 0.4635861 0.59547221
filter(gmAug, cooks > 1) # None.
## [1] country continent lifeExp lgdpPercap lPop studRes
## [7] hats cooks
## <0 rows> (or 0-length row.names)
# app.R
# Library packages
# install.packages(c("gapminder", "plotly", "shiny"))
library(gapminder)
library(plotly)
library(shiny)
library(dplyr)

# Globar variables
bubble_radius <- sqrt(gapminder$pop / pi)
unique_year <- unique(gapminder$year)
range_gdpPercap <- log10(range(gapminder$gdpPercap) + c(-200, 20000))
range_lifeExp <- range(gapminder$lifeExp) + c(-30, 30)

# Define UI for application
ui <- fluidPage(
   # Application title
   titlePanel("R Gapminder Replica"),
   # Sidebar panel
   sidebarLayout(
     # Silder input
     sidebarPanel(
       sliderInput(
         "year",
         "Year:",
         min = min(gapminder$year),
         max = max(gapminder$year),
         value = min(gapminder$year),
         step = unique_year[2] - unique_year[1],
         sep = "",
         animate = TRUE
       )
     ),
     # Plotly rendering
     mainPanel(
       plotlyOutput("gapminder_bubble")
     )
   )
   
)

# Define server logic
server <- function(input, output) {
  # reactive filtering
  reactive_gapminder <- reactive(
    gapminder %>%
      filter(year == input$year)
  )
  output$gapminder_bubble <- renderPlotly({
    reactive_gapminder() %>% 
       plot_ly(x = ~gdpPercap, y = ~lifeExp,
               size = ~pop, type = "scatter", mode = "markers",
               color = ~continent, text = ~country, hoverinfo = "text",
               sizes = c(min(bubble_radius), max(bubble_radius))) %>%
       layout(xaxis = list(type = "log",
                           range = range_gdpPercap),
              yaxis = list(range = range_lifeExp))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
