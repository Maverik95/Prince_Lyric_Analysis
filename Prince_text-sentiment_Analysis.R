# Libraries needed
library(dplyr) #data manipulation
 
library(ggplot2) #for visualizations

library(gridExtra)

library(tidytext)  #text mining

library(wordcloud2) #creative visualizations

# This project is centered on the artiste 'prince'. We shall perform a text sentiment analysis on his lyrics
# Import the data from the working directory
prince_orig <- read.csv("prince_raw_data.csv", stringsAsFactors = FALSE)
# Setting strings as Characters by making stringsAsFactors = FALSE

# Use the names() to see all the columns contained in the dataset
names(prince_orig)
       
# We are only interested in a few columns such as text, song, year, peak, album, us.pop and u.s.r.b. These are the essentials. 
# We can also change the names of some columns using the select function.
prince_ess <- prince_orig %>%
+ select(c(lyric=text, song, year, album, US_Pop= US.Pop, US_R_B=US.R.B, peak))

# Take a glimpse of the new data
glimpse(prince_ess[139, ])

# Check how many observations are there
> dim(prince_ess)

# We go into cleaning the data by first getting rid of pesky contradictions such as won't and can't 
# by using the gsub() to make appropriate substitutions. A function will be created 
# to make this substitutions and the function will be applied to the lyric column
> fix.contradictions <- function(doc) {
+ doc <- gsub("won't", "will not", doc)
+ doc <- gsub("can't", "can not", doc)
+ doc <- gsub("n't", "not", doc)
+ doc <- gsub("'ll", "will", doc)
+ doc <- gsub("'re", "are", doc)
+ doc <- gsub("'ve", "have", doc)
+ doc <- gsub("'m", "am", doc)
+ doc <- gsub("'d", "would", doc)
+ doc <- gsub("'s", "", doc)
+ return(doc)
+ }

# Apply the function on lyric column and expand contractions
prince_ess$lyric <- sapply(prince_ess$lyric, fix.contradictions)

# There are also special characters that mess up the dataset. We will create a function to remove these special characters
removespec <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# Apply the function
prince_ess$lyric <- sapply(prince_ess$lyric, removespec)

# Convert the lyric column tolower using the tolower() function. This makes for constitency
prince_ess$lyric <- sapply(prince_ess$lyric, tolower)

# Let's examine the structure of the cleaned lyrics
str(prince_ess[139, ]$lyric, nchar.max = 300)

# Get facts about the full dataset
summary(prince_ess)

# Create the decade column by grouping the year column using 
# the mutate() function from dplyr and also using the ifelse() function
prince_ess_mut <- prince_ess %>%
+ mutate(decade = ifelse(year %in% 1978:1979, "1970s", ifelse(year %in% 1980:1989, "1980s", 
ifelse(year %in% 1990:1999, "1990s", ifelse(year %in% 2000:2009, "2000s", ifelse(year %in% 2010:2015, "2010s", "NA"))))))

# You can also create a column for chart_level which represents whether a song 
# peaked in the top 10 or top 100. Both are mutually exclusive
prince_ess_muta <- prince_ess_mut %>%
+ mutate(chart_level= ifelse(peak %in% 1:10, "Top 10", ifelse(peak %in% 11:100, "Top 100", "Uncharted")))

# Check the summary of the changed prince data
summary(prince_ess_muta)

# Take a glimpse of the dataset                        
glimpse(prince_ess_muta)

# Create a binary field called charted showing two factors the charted songs and the uncharted songs
prince_cond <- prince_ess_muta %>%
+ mutate(charted = ifelse(peak %in% 1:100, "Charted", "Uncharted"))

glimpse(prince_cond)

# Save the new dataset to .csv for use later 
write.csv(prince_cond, file= "prince_cond.csv")

> # Define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")
theme_lyrics <- function(){
  + theme(plot.title = element_text(hjust = 0.5),
          +       axis.text.x = element_blank(), 
          +       axis.ticks = element_blank(),
          +       panel.grid.major = element_blank(),
          +       panel.grid.minor = element_blank(),
          +       legend.position = "none")
  + }

# Before getting into text mining, we need to understand some trends like finding out how many songs he really released per decade
# Use filter(), group_by() and summarise() to manipulate the data. We then use ggplot() for visualizations
decade_songs <- prince_cond %>%
+ filter(decade != "NA") %>%
+ group_by(decade, charted) %>%
+ summarise(number_of_songs = n()) %>%
+ ggplot() +
+ geom_bar(aes(x= decade, y= number_of_songs, fill= charted), stat= "identity")+
+ theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.minor = element_blank())+
+ ggtitle("Number of Released songs per Decade")+
+ labs(x= NULL, y="Song Count")

# The graph shows he was most active in the 1990s, however the bar chart shows he had more charted songs in the 1980s
# Now you can create a similar graph with chart_level. 
# Group_by() both decade and chart_level so you can see the trend. 
# In this graph we only want songs that have their peak>0 and then we using n() to count the number of songs within the summarise() function
charted_songs_over_time <- prince_cond %>%
+ filter(peak>0) %>%
+ group_by(decade, chart_level)%>%
+ summarise(number_of_songs = n()) %>%
+ ggplot()+
+ geom_bar(aes(x=decade, y= number_of_songs, fill= chart_level), stat = "identity")+
+ theme(plot.title = element_text(hjust = 0.8))+
+ ggtitle("Charted Prince Songs over Decades")+
+ labs(x="Decades", y="Songs Count")
charted_songs_over_time
# Insights
# The graph shows interesting visualizations. 
# Notice that of all the Prince's charted songs majority reached top 10. 
# In the last graph we inferred that the 1990s was his most prolific year. However the 1980s was the year he had the most charted songs. 

charted_songs_overall <- prince_cond %>%
  + group_by(decade, chart_level)%>%
  + summarise(number_of_songs = n()) %>%
  + ggplot()+
  + geom_bar(aes(x=decade, y= number_of_songs, fill= chart_level), stat = "identity")+
  + theme(plot.title = element_text(hjust = 0.8))+
  + ggtitle("Charted Prince Songs over Decades")+
  + labs(x="Decades", y="Songs Count")
charted_songs_overall
# Below is a visualization of the songs that hit No 1. on the charts

# Load the knitr library
library(knitr) # For dynamic reporting

# install the kableExtra package
install.packages("kableExtra")

# Load the kableExtra library
library(kableExtra)

# Install the formattable library for working with color_tiles
install.packages("formattable")

# Load the formattable library
library(formattable) # for the color_tile function

# Select Prince's No 1 songs along with the years they were released
prince_peak1 <- prince_cond %>%
+ filter(peak == "1") %>%
+ select(year, song, peak) %>%
+ arrange(year) %>%
+ mutate(year= color_tile("lightblue", "lightgreen" )(year)) %>%
+ mutate(peak= color_tile("lightgreen", "lightblue" )(peak)) %>%
+ kable("html", escape = FALSE, align = "c", caption = "Prince's no 1 songs") %>%
+ kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), full_width = FALSE)

# Prince's No 1 songs
prince_peak1

# Below is a list of superfluous words that need to be removed manually
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", "theres", "bridge", "fe0f", "yeah", "baby",   
"alright", "wanna", "gonna", "chorus", "verse","whoa", "gotta", "make", "miscellaneous", "2",  
"4", "ooh", "uurh", "pheromone", "poompoom", "3121", "matic", " ai ", " ca ", " la ", 
"hey", " na ", " da ", " uh ", " tin ", "  ll", "transcription","repeats")

# From the tidytext framework you need to perform tokenization and transform it to a tidy structure.  
To do this, use tidytext's unnest_tokens() function. 
unnest_tokens() requires at least two arguments: the output column name that will be 
created as the text is unnested into it ("word", in this case), and the input column that holds the current text (i.e. lyrics).

# You can then take the prince dataset and pipe it into unnest_tokens() 
# and then remove stop words. Stop words are overly common words that dont add meaning to our analysis. 
# There are different lists to choose from, but here you'll use the lexicon called stop_words from the tidytext package. 
# Use sample() to show a random list of these stop words and head() to limit to 30 words
stop_words_prince <- sample(stop_words$word, 30)
stop_words_prince

# After you tokenize lyrics into words, you use dplyr's anti_join() to remove stop words. 
# Then remove undesirable words by using dplyr's filter(). Distinct() is used to rid off duplicate records.
# Lastly you can remove all words with fewer than four characters because most of them are interjections such as "yea, is and hey"
# Unnest, remove stopwords, undesirable and short words
prince_cond_filt <- prince_cond %>%
+ unnest_tokens(word, lyric) %>%
+ anti_join(stop_words) %>%
+ distinct() %>%
+ filter(!word %in% undesirable_words) %>%
+ filter(nchar(word) > 3)
               
# Examine the class of your new tidy structure
class(prince_cond_filt) # Dataframe

# Check the dimension of the dataframe
dim(prince_cond_filt)

# Take the glimpse of the dataframe
glimpse(prince_cond_filt)

# Create a table that count the number of words per song and then also contains a column showing the song's chart level
full_word_count <- prince_cond %>%
+ unnest_tokens(word, lyric) %>%
+ group_by(song, chart_level) %>%
+ summarise(num_words=n()) %>%
+ arrange(desc(num_words))

head(full_word_count)

# Make a histogram plot of the songs and the number of songs per song
full_word_count_plot <- full_word_count %>%
+ ggplot()+
+ geom_histogram(aes(x= num_words, fill= chart_level))+
+ labs(x="word count")+
+ ggtitle("Word Count Distributiion")+
+ theme(plot.title= element_text(hjust=0.5), panel.grid.minor.y = element_blank())

# View the graph
full_word_count_plot

# From the graph we can infer that Prince's songs that had between 200-300 songs 
# had the most possibility charting at the top 10 and top 100.

# The graph is also right skewed. We should be skeptical about data outliers. 
# So out of curiosity lets check songs that made the top 10 and had words above 800
full_word_count %>%
+ filter(chart_level == "Top 10" & num_words > 800) %>%
+ left_join(prince_orig, by="song") %>%
+ select(c(song, "Word Count"=num_words, "Peak Position"= peak, "US Pop"= US.Pop, "US R&B"= US.R.B, Canada= CA, Ireland= IR)) %>%
+ kable("html", escape = FALSE, caption = "Top 10 charted Prince songs with above 800 words")%>%
+ kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))

# Set up a function that fixes contractions
fix_contractions <- function(doc){
+ doc <- gsub("'m", " am", doc)
+ doc <- gsub("'ll", " will", doc)
+ doc <- gsub("n't", " not", doc)
+ doc <- gsub("'re", " are", doc)
+ doc <- gsub("'ve", " have", doc)
+ doc <- gsub("'d", " would", doc)
+ return(doc)
+ }

# Run the fix_contractions through the data frame to fix the contractions
prince_cond_filt$word <- sapply(prince_cond_filt$word, fix_contractions)

# Plot a chart showing the top 10 most frequently used words in Prince's songs
prince_cond_filt %>%
+ count(word, sort= TRUE) %>%
+ top_n(10) %>%
+ ungroup() %>%
+ mutate(word= reorder(word, n)) %>%
+ ggplot()+
+ geom_col(aes(x=word, y=n), fill=my_colors[4])+
+ theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())+
+ xlab("")+
+ ylab("word count")+
+ ggtitle("Most Frequently used words in Prince songs")+
+ coord_flip()

freq_used_words

# Unnest the words, filter words above three letters and then plot the top 10 most frequently used words in Prince's songs
freq_used_words <- prince_cond_filt %>%
+ unnest_tokens(wordss, word) %>%
+ filter(nchar(wordss) > 3) %>%
+ count(wordss, sort = TRUE) %>%
+ top_n(10) %>%
+ ungroup() %>%
+ mutate(wordss = reorder(wordss, n)) %>%
+ ggplot()+
+ geom_col(aes(wordss, n), fill=my_colors[4])+
+ theme(plot.title = element_text(hjust = 0.8), panel.grid.major = element_blank())+
+ xlab("")+
+ ylab("Word Count")+
+ ggtitle("Most frequently used words in Prince's Songs")+
+ coord_flip()

# Visualize the chart
freq_used_words	

# Remove stop words and undesirable words from the prince_cond_filt dataframe
prince_cond_filt <- prince_cond_filt %>%
+ unnest_tokens(wordss, word) %>%
+ filter(!wordss %in% stop_words) %>%
+ filter(!wordss %in% undesirable_words) %>%
+ filter(nchar(wordss) > 3)

# Glimpse the dataframe
head(prince_cond_filt, 10)

# Create a table containing the frequency of all words in the prince dataset
prince_cond_wordcount <- prince_cond_filt %>%
+ count(wordss, sort= TRUE)

# Do a wordcloud on the top 300 words in the unnested prince dataset
wordcloud2(prince_cond_wordcount[1:300, ], size = .5)

# Create a table showing the most used words grouped by chart_levels
popular_words <- prince_cond_filt %>%
+ group_by(chart_level) %>%
+ count(wordss, chart_level, sort = TRUE) %>%
+ slice(seq_len(8)) %>%
+ ungroup() %>%
+ arrange(chart_level, n) %>%
+ mutate(row=row_number())

# Check the top 10 rows of the popular_words dataframe
> head(popular_words, 10)

# Visualize the graph

popular_words %>%
+ ggplot(aes(row, n, fill= chart_level)) +
+ geom_col(show.legend = NULL)+
+ xlab("NULL")+
+ ylab("Song Count")+
+ ggtitle("Popular words by Chart Level")+
+ theme_lyrics()+
+ facet_wrap(~chart_level, scales = "free")+
+ scale_x_continuous(# This handles replacement of row
+ breaks= popular_words$row,
+ labels= popular_words$wordss)+
+ coord_flip()

> # The top words across the chart look very, very similar. 
This doesn't look good for our hopes of predicting whether a song will succeed based on lyrics. 
But you've only touched the surface of what is possible with text mining, and NLP and predictive modelling.
