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

# What was the highest monthly mean?
monthly.max <- monthly.means %>%
  group_by(month) %>%
  arrange(avg) %>%
  slice(n()) %>%
  rename(year.max=year,
         avg.max=avg)

# What was the lowest monthly mean?
monthly.min <- monthly.means %>%
  group_by(month) %>%
  arrange(desc(avg)) %>%
  slice(n()) %>%
  rename(year.min=year,
         avg.min=avg)

# Merge data
monthly.sum <- monthly.avg %>%
  left_join(monthly.max, by="month") %>%
  left_join(monthly.min, by="month") %>%
  relocate(avg.max, .after=avg) %>%
  relocate(avg.min, .after=year.max) %>%
  mutate(month=as.integer(month),
         year.max=as.integer(year.max),
         year.min=as.integer(year.min)) %>%
  rename(Month=month,
         Average=avg,
         Max=avg.max,
         'Max Year'=year.max,
         Min=avg.min,
         'Min Year'=year.min)

# Hottest day on record for that month
monthly.max.rec <- df %>%
  group_by(month(date)) %>%
  drop_na(tmax) %>%
  arrange(tmax) %>%
  slice(n()) %>%
  as_tibble() %>%
  select(date,tmax) %>%
  mutate(Month=month(date)) %>%
  relocate(date, .after=tmax) %>%
  rename('Record daily max.'=tmax,
         'Date of record max.'=date)

# Coolest day on record for that month
monthly.min.rec <- df %>%
  group_by(month(date)) %>%
  drop_na(tmin) %>%
  arrange(desc(tmin)) %>%
  slice(n()) %>%
  as_tibble() %>%
  select(date,tmin) %>%
  mutate(Month=month(date)) %>%
  relocate(date, .after=tmin)%>%
  rename('Record daily min.'=tmin,
         'Date of record min.'=date)

# Merge data
monthly.sum <- monthly.sum %>%
  left_join(monthly.max.rec, by="Month") %>%
  left_join(monthly.min.rec, by="Month") %>%
  mutate(`Date of record min.`=as.character(`Date of record min.`),
         `Date of record max.`=as.character(`Date of record max.`))

# Summarize precipitation data ----
# Calculate monthly sums by year
prcp.sums <- df %>%
  group_by(month(date), year(date)) %>%
  summarize(total_in=sum(prcp, na.rm=TRUE)/25.4)
names(prcp.sums)[1:2] <- c("month","year")

# Calculate the average sum by month
prcp.avg <- prcp.sums %>%
  group_by(month) %>%
  summarize(avg=mean(total_in,na.rm=TRUE))

# What was the highest monthly mean?
prcp.max <- prcp.sums %>%
  group_by(month) %>%
  arrange(total_in) %>%
  slice(n()) %>%
  rename(year.max=year,
         sum.max=total_in)

# What was the lowest monthly mean?
prcp.min <- prcp.sums %>%
  group_by(month) %>%
  arrange(desc(total_in)) %>%
  slice(n()) %>%
  rename(year.min=year,
         sum.min=total_in)

# Merge data
prcp.sum <- prcp.avg %>%
  left_join(prcp.max, by="month") %>%
  left_join(prcp.min, by="month") %>%
  relocate(sum.max, .after=avg) %>%
  relocate(sum.min, .after=year.max) %>%
  mutate(month=as.integer(month),
         year.max=as.integer(year.max),
         year.min=as.integer(year.min)) %>%
  rename(Month=month,
         Average=avg,
         Max=sum.max,
         'Max Year'=year.max,
         Min=sum.min,
         'Min Year'=year.min)

# Most rain on a single day in each month
prcp.max.rec <- df %>%
  group_by(month(date)) %>%
  drop_na(prcp) %>%
  arrange(prcp) %>%
  slice(n()) %>%
  as_tibble() %>%
  select(date,prcp) %>%
  mutate(Month=month(date),
         prcp=prcp/25.4) %>%
  relocate(date, .after=prcp) %>%
  rename('Record daily precip.'=prcp,
         'Date of record precip.'=date)

prcp.sum <- prcp.sum %>%
  left_join(prcp.max.rec, by="Month") %>%
  mutate(`Date of record precip.`=as.character(`Date of record precip.`))

# Create the Shiny App ----
# User Interface (ui)
ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="Lehigh Valley Climate"),
  dashboardSidebar(
    selectizeInput(inputId='data',
                   label='Select climate variable:',
                   choices=c(" ","Precipitation","Temperature"),
                   options=list(
                     placeholder='',
                     onInitialize = I('function() { this.setValue(" "); }'))
    )),
  dashboardBody(
    fluidRow(
      column(9,plotOutput("plot", brush = "plot_brush")),
      column(3,plotOutput("legend"))
    ),
    fluidRow(
      column(9,align="center",tableOutput("Data"))
    )
  )
)

# Server function (server)
server <- function(input, output, session) {
  labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  output$plot <- renderPlot({
    plot <- {
      if (input$data==" ")
        ggplot() + theme_void()
      else if(input$data=="Temperature")
        ggplot(monthly.sum, aes(x=Month)) +
          geom_crossbar(aes(ymin=Min,ymax=Max,y=Average),
                        width=0.5,
                        size=0.25,
                        fill="lightgray") +
          geom_point(aes(y=`Record daily max.`),
                     shape=21,
                     fill="#d1ce08") +
          geom_point(aes(y=`Record daily min.`),
                     shape=21,
                     fill="blue") +
          scale_x_continuous(breaks=seq(1,12,1),
                             labels=labels,
                             expand=c(0.02,0)) +
          labs(x="Month",
               y="Temperature (°F)") +
          theme_dark()
      
      else ggplot(prcp.sum, aes(x=Month)) +
        geom_crossbar(aes(ymin=Min,ymax=Max,y=Average), 
                      width=0.75,
                      size=0.25,
                      fill="lightgray") +
        geom_point(aes(y=`Record daily precip.`),
                   shape=21,
                   size=3,
                   fill="blue") +
        scale_x_continuous(breaks=seq(1,12,1),
                           labels=labels,
                           expand=c(0.02,0)) +
        scale_y_continuous(expand=c(0.02,0)) +
        labs(x="Month",
             y="Total precipitation (in)") +
        theme_dark()
    }
    plot
  }, res=96)
  
  output$legend <- renderPlot({
    leg <- {
      if (input$data==" ")
        ggplot() + theme_void()
      else if(input$data=="Temperature")
        ggplot() +
        annotate(geom="rect", xmin=1.5, xmax=3, ymin=0, ymax=4, 
                 color="black", fill="lightgray", size=0.25) +
        annotate(geom="text",
                 label="Maximum daily\nmean temperature",
                 x=3.1, hjust=0, y=4, vjust=1, size=3) +
        annotate(geom="text",
                 label="Minimum daily\nmean temperature",
                 x=3.1, hjust=0, y=0, vjust=0, size=3) +
        annotate(geom="text",
                 label="Average daily\nmean temperature",
                 x=3.1, hjust=0, y=2, vjust=0.5, size=3) +
        annotate("segment", x=1.5, xend=3, y=2, yend=2) +
        annotate("point", x=2,y=-0.5,shape=21,fill="blue") +
        annotate(geom="text",
                 label="Record daily minimum temperature",
                 x=2.1, hjust=0, y=-0.5, vjust=0.5, size=3) +
        annotate("point", x=2,y=4.5,shape=21,fill="#d1ce08") +
        annotate(geom="text",
                 label="Record daily maximum temperature",
                 x=2.1, hjust=0, y=4.5, vjust=0.5, size=3) +
        scale_x_continuous(limits=c(1.45,4.5)) +
        theme_void() +
        theme(text=element_text(color="#22211d"),
              plot.background=element_rect(fill="white", color=NA),
              panel.background=element_rect(fill="white", color=NA),
              legend.background=element_rect(fill="white", color=NA))
      else ggplot() +
        annotate(geom="rect", xmin=1.5, xmax=3, ymin=0, ymax=4, 
                 color="black", fill="lightgray", size=0.25) +
        annotate(geom="text",
                 label="Maximum monthly\nprecipitation",
                 x=3.1, hjust=0, y=4, vjust=1, size=3) +
        annotate(geom="text",
                 label="Minimum monthly\nprecipitation",
                 x=3.1, hjust=0, y=0, vjust=0, size=3) +
        annotate(geom="text",
                 label="Average monthly\nprecipitation",
                 x=3.1, hjust=0, y=2, vjust=0.5, size=3) +
        annotate("segment", x=1.5, xend=3, y=2, yend=2) +
        annotate("point", x=2,y=4.5,shape=21,fill="blue") +
        annotate(geom="text",
                 label="Record daily precipitation",
                 x=2.1, hjust=0, y=4.5, vjust=0.5, size=3) +
        scale_x_continuous(limits=c(1.45,4.5)) +
        theme_void() +
        theme(text=element_text(color="#22211d"),
              plot.background=element_rect(fill="white", color=NA),
              panel.background=element_rect(fill="white", color=NA),
              legend.background=element_rect(fill="white", color=NA))
      
    }
    leg
  }, res=96)
  
  output$Data <- renderTable({
    if(input$data==" ")
      "To plot data, please select a climate variable"
    else if(input$data=="Temperature")
      brushedPoints(monthly.sum, input$plot_brush)
    else 
      brushedPoints(prcp.sum, input$plot_brush)
  })
}

# Run the app
shinyApp(ui, server)
