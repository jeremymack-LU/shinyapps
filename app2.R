# Load packages ----
pacman::p_load(shiny, shinydashboard, tidyverse, lubridate, patchwork)

# Load climate data ----
## Read in climate data
url <- "https://raw.githubusercontent.com/jeremymack-LU/shinyapps/master/data/abe_climate.csv"
df <- read_csv(url, col_types=cols(tavg=col_double()))

df[df$date==as.Date("1954-05-20"),6] <- NA

# Calculate daily mean temperature (dmt) and day of year (doy)
df <- df %>% 
  mutate(dmt=(tmax+tmin)/2,
         doy=yday(date),
         tmin=tmin* 1.8 + 32,
         tmax=tmax* 1.8 + 32,
         dmt=dmt* 1.8 + 32)

# Summarize temperature data ----
# Calculate monthly means by year
monthly.means <- df %>%
  group_by(month(date), year(date)) %>%
  summarize(avg=mean(dmt, na.rm=TRUE))
colnames(monthly.means)[1:2] <- c("month","year")

# Calculate the average daytime mean by month
monthly.avg <- monthly.means %>%
  group_by(month) %>%
  summarize(avg=mean(avg, na.rm=TRUE))

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="Lehigh Valley Climate"),
  dashboardSidebar(
    selectInput(
      "years",
      "Select year(s):",
      seq(1948,2021,1),
      multiple=TRUE
    )

    ),
  dashboardBody(
    plotOutput("plot", brush = "plot_brush")
  )
)

server <- function(input, output, session) {
  labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  df.sub <- reactive(
    monthly.means %>%
      filter(year%in%input$years)
  )
  
  output$plot <- renderPlot(
    ggplot() +
      geom_boxplot(aes(month,avg,group=month),monthly.means) +
      geom_point(aes(month,avg,color=as.factor(year)),df.sub(),size=3) +
      scale_x_continuous(breaks=seq(1,12,1),
                         labels=labels,
                         expand=c(0.02,0)) +
      labs(x="Month",
           y="Temperature (°F)") +
      theme_dark()
  )
  
}

# Run the app
shinyApp(ui, server)
