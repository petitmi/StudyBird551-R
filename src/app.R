library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(wordcloud)

app <- Dash$new(external_stylesheets = dbcThemes$QUARTZ)
years <- c()
mark <- list()

for (i in 2012:2022) {
years <- c(years, toString(i))}

for (year in years) {
mark[[year]] <- list(
label = year,
style = list(color = "white"))}


# Define the layout of the app
app$layout(dbcContainer(list(    
    dccLocation(id='url',refresh=F),
    dbcRow(list(htmlDiv(list( 
            htmlH3("BillBoard Top100 Hot 🔥🔥Songs🔥🔥 Analysis", className="display-4"),
            htmlH5("Study Bird 551"),
            htmlHr())),
        dbcRow(list(dbcCol(dbcNav(list(
            dbcNavItem(dbcNavLink("Artist", id="page-artist",href="artist")),
            dbcNavItem(dbcNavLink("Lyrics", id="page-lyrics",href="lyrics")),
            dbcNavItem(dbcNavLink("Tracks", id="page-tracks",href="tracks"))),vertical = TRUE)
        ),dbcCol(width=10,
            list(dccRangeSlider(id='year-slider',min=2012,max=2022,value=list(2012, 2022),step=1,marks=mark),
            htmlDiv(id="page-content"),
            dccGraph(id='chart1'),
            htmlDiv(id="describe1"),
            dccGraph(id='chart2'),
            htmlDiv(id="describe2"),
            dccGraph(id='chart3'),
            htmlDiv(id="describe3"))))))),
        dbcRow(list(htmlHr(),
            htmlP("Data from BillBoard, Spotify, Musixmatch and Genius.")))),fluid=T))
app %>% add_callback(
    output=list(output("page-content", 'children'),
    output("chart1", 'figure'),
    output("describe1", 'children'),
    output("chart2", 'figure'),
    output("describe2", 'children'),
    output("chart3", 'figure'),
    output("describe3", 'children')),
    params=list(input('url', 'pathname'),
    input("year-slider", "value")),
    function(pathname,year) {
        if (pathname=='/artist') {
            return(generate_page_content("Artist Analysis", year))
        } else if (pathname=='/lyrics') {
            return(generate_page_content("Lyrics Analysis", year))
        } else if (pathname=='/tracks') {
            return(generate_page_content("Tracks Analysis", year))
        } else {
            return(generate_page_content("Artist Analysis", year))}})

generate_page_content <- function(page_title, year) {
    
    start_year <- year[1]
    end_year <- year[2]
    if (page_title=="Artist Analysis"){
        df <- read.csv('./data/processed/artist_process_data.csv')
        df <- filter(df,Year<=2022&Year>=2012)
        df1 <- df[!duplicated(df[,4]),]
        df1$year <- format(df1$first_year, format="%Y")
        df2 <- filter(df1,popularity>60)
        df3 <- df[!duplicated(df[,11]),]
        a <- wordcloud(words=df1$Artist,freq=df1$count,min.freq=3,max.words=200,colors=brewer.pal(8, "Dark2"),scale=c(2, .25))
        b <- wordcloud(words=df1$Artist,freq=df1$popularity,min.freq=80,max.words=200,colors=brewer.pal(8, "Dark2"),scale=c(1.5, .15))
        df4 <- read.csv("./data/processed/genres.csv")
        
        plot1<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        plot2<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        plot3<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        
         chart_describ=dccMarkdown("
        - The **size** and **color** represents the combination of Artist's popularity and song's quality.
        - The singer in the **upper right** corner has the strongest overall strength.
        - We filtered out singers whose popularity was less than **60**.
        ")
        chart1_describ=dccMarkdown("
        - The chart on the **left** represents the **distribution of singers by year of debut**.
        - The chart on the **right** represents the **percentage of the year of debut of the top artists**.
        - The **range** of debut years is every **10** years.
        ")
        chart2_describ=dccMarkdown("
        - Represented **the type of singer** with the **highest** number of top. 
        ")
        
        
    } else if (page_title=="Lyrics Analysis"){
        
        df <- data.frame(x = rnorm(100), y = rnorm(100))
        plot1<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        plot2<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        plot3<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        
        chart_describ=dccMarkdown("
        - Divide the top 100 songs into 10 groups; 
        - The X-axis represents the year; 
        - The Y-axis is the frequency of the keyword 'love'; 
        - From the figure, we can see the frequency of 'love' and the trend of their frequency every year.
        ")
    
        chart1_describ=dccMarkdown("
        - Divide the top 100 songs into 4 groups; 
        - The X-axis represents the year; 
        - The Y-axis is the lyrics length; 
        - From the figure, From the figure, we can compare each year's lyrics length in the four rank groups.
        ")
        
        chart2_describ=dccMarkdown("
            - Chart on the left is a pie chart representing the proportion of positive, negative and neutral lyrics in the selected year. 
            - Placing the mouse on the pie chart can show the specific number of songs; 
            - The chart on the right is the boxplot which displays ranges within sentiment scores for the selected year; 
            - sentiment score ranges from -1 to 1. (-1 is the most negative, 1 is the most positive, and 0 is neutral).
            ")
        
    } else if (page_title=="Tracks Analysis"){
        
        df <- data.frame(x = rnorm(100), y = rnorm(100))
        plot1<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        plot2<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        plot3<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        
        chart_describ=dccMarkdown("
        
        ")
    
        chart1_describ=dccMarkdown("
       
        ")
        
        chart2_describ=dccMarkdown("
          
            ")
    }
    
    
    return(list(htmlDiv(htmlH4(sprintf("%s between %s and %s:", page_title, start_year, end_year))
                ),ggplotly(plot1),htmlDiv(chart_describ),ggplotly(plot2),htmlDiv(chart1_describ),ggplotly(plot3),htmlDiv(chart2_describ)))
}
# Run the app
app$run_server(debug = T)
