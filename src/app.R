library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(wordcloud2)
library(webshot)
library(htmlwidgets)
library(base64enc)

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
        
        plot1<-ggplot(data = df2, aes(x = count, y = popularity)) + 
                    geom_point() + 
                    labs(title="Artist's Song Quality vs Popularity",
                         x="Number of times on top", 
                         y = "Artits popularity")+
                    theme(plot.title = element_text(hjust = 0.5))
        
        plot2<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        plot3<-ggplot(data = df, aes(x = x, y = y)) + geom_point()
        
        gglpotly(plot1)
        
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
        
        options(dplyr.summarise.inform = FALSE)
        df <- read_excel('./data/processed/lyrics_dataset.xlsx')
        df <- df %>% filter(Year >= start_year & Year <= end_year)
        cutRank4<-cut(df$Rank, breaks = 4,labels=c('1-25','26-50','51-75','76-100'))
        df$rank_bin <-cutRank4

        df_bin <- df %>% 
        select(c('rank_bin','Year','Word_Count','Sentiment_Polarity','Frequency_love'))%>% group_by(rank_bin,Year)%>%
        summarise(across(c('Word_Count', 'Sentiment_Polarity','Frequency_love'), mean))%>% as.data.frame()
        source <- df %>% count(Sentiment)%>% as.data.frame()
        cutRank10<-cut(df$Rank, breaks = 10,labels=c('1-10','11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100'))
        df$rank_bin10 <-cutRank10
        df_bin10 <- df %>% 
        select(c('rank_bin10','Year','Word_Count','Sentiment_Polarity','Frequency_love'))%>% group_by(rank_bin10,Year)%>%
        summarise(across(c('Word_Count', 'Sentiment_Polarity','Frequency_love'), mean))%>% as.data.frame()
        if(toString(start_year)==toString(end_year)){
            plot1<-ggplot(df_bin10, aes(x=Year, y=Frequency_love, fill=rank_bin10)) +
              geom_bar(stat="identity", position="stack") +
              scale_x_discrete(name="Year") +
              scale_y_continuous(name="Frequency") +
              ggtitle(sprintf("Frequency of 'love' Analysis")) +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5), 
              legend.position="bottom", 
              legend.title=element_blank(), 
              legend.direction = "horizontal")

        }else{
            plot1<-ggplot(df_bin10, aes(x = Year, y = Frequency_love, fill = factor(rank_bin10))) + 
        geom_area(stat = "identity", alpha = 0.8) + 
        scale_fill_discrete(name = "rank_bin10") +
        labs(title = sprintf("Frequency of 'love' Analysis"), x = "Year", y = "Frequency") + 
        theme(plot.title = element_text(hjust = 0.5))
        }
        
        
        
        plot2<-ggplot(df_bin10, aes(x=factor(Year), y=Word_Count, fill=rank_bin10)) +
          geom_bar(stat="identity", position="stack") +facet_grid(~rank_bin10)+
          scale_x_discrete(name="Year") +
          scale_y_continuous(name="Lyrics Length") +
          ggtitle(sprintf("Lyrics Length Anaylisis")) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5), 
                axis.text.x = element_text(size=5,angle = 90, vjust = 0.5, hjust=1),
                legend.position="right", 
                legend.title=element_blank(), 
                legend.direction = "horizontal")
        
        
        p1<-ggplot(source, aes(x=Sentiment, y=n, fill=Sentiment))+
            geom_bar(width = 1, stat = "identity",color="white")
        
        p2<-ggplot(df, aes(x=factor(Year), y=Sentiment_Polarity, fill=factor(Year))) +
            geom_boxplot()+
            scale_x_discrete(name="Year") +
            scale_y_continuous(name="Sentiment Score")+
            ggtitle(paste('Boxplot of Sentiment Score Analysis'))

        plot3<-subplot(p1, p2, nrows = 1)%>%layout(legend=list(title=list(text='')))


        df$'word_cloud'<- df$'Clean_Lyrics'
        remove_words <- c('verse', 'chorus', 'oh', 'want', 'yeah', 'wan', 'na', 'got', 'might', 'feat')
        for (word in remove_words) {
              df$'word_cloud'<- gsub(paste0("\\b", word, "\\b"), '', df$'word_cloud', perl=TRUE)}
        text <- paste(df$'word_cloud', collapse = " ")
        my_words <- strsplit(tolower(text), "\\W+")[[1]]
        word_freq <- table(my_words)
        word_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
        names(word_df) <- c("word", "frequency")
        word_df <- word_df[order(-word_df$frequency),]
        pic<-wordcloud2(word_df, size = 1, color = "random-dark")
        saveWidget(pic, "tmp.html", selfcontained = F)
        webshot("tmp.html", "wordcloud.png", delay = 3, vwidth = 900, vheight = 200)
        img <- readBin("wordcloud.png", "raw", file.info("wordcloud.png")$size)
        img_base64 <- base64encode(img)
        md_code <- paste0("![](data:image/png;base64,", img_base64, ")")

        
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
        
        chart2_describ=dccMarkdown(list("- Chart on the left is a bar chart displaying the number of positive, negative and neutral lyrics in the selected year.", 
            "- The chart on the right is the boxplot which displays ranges within sentiment scores for the selected year;", 
            "- Sentiment score ranges from -1 to 1. (-1 is the most negative, 1 is the most positive, and 0 is neutral).",md_code))
        
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
