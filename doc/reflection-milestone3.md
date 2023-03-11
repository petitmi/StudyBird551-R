# Reflection for Study Bird 551 Project(R)
**Topic:** Billboard Top100 Hot🔥🔥Songs🔥🔥Analysis  
**Group Members:** Nyx, Tia, Yuhong


## Implemented  
1. **The objectives and directions:** For the hottest songs, represented by Billboard top100 chart, we wanted to understand their characteristics and also how they have changed over the years.
2. **The indicators represented the characteristics and their calibers:** There are three objects of characteristics, each aspects contain several dimensions. ([README.md](https://github.com/petitmi/Study-Bird-551/readme.md))

3. **The various visualization of characteristics:**
   - Basic bar charts, line charts, pie chars
   - Complex box plots, wordcloud plots, stack plots
   - Compound multi-axies and multi-dimension plots
4. **User interaction and structure**
 <table>
    <thead>
        <tr>
            <th colspan="2"  text-align= "left">Title</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="2">Side bar: Tab selection</td>
            <td>Range selection</td>
        </tr>
        <tr>
            <td>Main page container:  Iframe plots</td>
        </tr>
        <tr>
            <td colspan="2">Footer</td>
        </tr>
    </tbody>
</table>

## Unimplemented
1. Each analysis object selected the dimensions that were considered most important and did not cover more and more complete dimensions. Improvements could be made by adding selection components.
2. More detailed features were not implemented: for example, the analysis of lyrics displayed by search keywords.


## Other reflections:

Same design and functionality as Milestone 2, so there are no issues with analysis ideas, data processing, aesthetics and consistency, or collaboration. The main differences and problems arose during the implementation of R language. Here are some difficulties we encountered:

1. Converting syntax from Python to R is time-consuming, and most dash methods have different names and usage in R. For example, the callback in R needs to contain functions.
2. Pie charts are not available in ggplotly for visualization, so we had to change the type of plots for one part of our visualization.
3. Subtitles are also not available, so we removed this information.
4. It takes much longer for R to generate a word cloud picture, which can take up to 10 minutes.
5. In the lyrics tab, the year cannot be transformed into a factor.

For the suggestion of adding components for users to customize the keywords they want to analyze in the first plot in the lyrics tab, we plan to:
1. Identify five keywords to include in the additional frequency columns, where the five keywords are "love," "freedom," "ocean," "robot," and "happy." These five keywords were chosen from five different aspects, namely classic themes, politics, nature, technology, and emotions.
2. Use Python to parse the lyrics and count how many times each keyword appears in each song.
3. Add five new columns to store the frequency value of each keyword.
4. Use Altair to create an area chart with the year on the x-axis and the total number of words on the y-axis. This will provide an overview of how the lyrics change over time.
5. Add a dropdown menu to the chart that allows the user to select a keyword to view its frequency over time. This can be achieved using the method alt.binding_select().
