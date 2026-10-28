### Working with Strings
### Created by: Wei Shen Lim
### Created on: 2026-10-27
##########################################################################

## Load Libraries
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(janeaustenr)

########################## What is a String? ##################################
words <- "This is a string"
words

#Several strings in a vector
words_vector <- c("Apples", "Bananas", "Oranges")
words_vector

############################## Manipulation ###################################
#Paste words together
paste("High temp", "Low pH")

#Add a dash in between the words
paste("High temp", "Low pH", sep = "-")

#Remove the space in between the words
paste0("High temp", "Low pH")

#Working with vectors
shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")

#Know how long a string is
str_length(shapes)

#Extract specific characters
seq_data <- c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4) #extract the 2nd to 4th

#Modify strings
str_sub(seq_data, start = 3, end = 3) <- "A" #add an A in the 3rd position
seq_data

#Duplicate strings
str_dup(seq_data, times = c(2,3)) #times is the number of times to duplicate each string

################################ Whitespace ###################################
#Deal with extra white spaces
badtreatments <- c("High", " High", "High ", "Low", "Low")
badtreatments

#Remove white space
str_trim(badtreatments) #this removes both

#Can also just remove from one side or the other
str_trim(badtreatments, side = "left") #this removes left

#Add white space to either side
str_pad(badtreatments, 5, side = "right") #add a white space to the right side after the 5th character

#Add a character instead of a white space
str_pad(badtreatments, 5, side = "right", pad = "1") #add a 1 to the right side after the 5th character

############################# Locale Sensitive ################################
#Make everything upper case
x <- "I love R!"
str_to_upper(x)

#Make everything lower case
str_to_lower(x)

#Make it title case
str_to_title(x)

################################ Pattern Matching #############################
#View a specific pattern in a vector of strings
data <- c("AAA", "TATA", "CTAG", "GCTT")

#Find all the strings with an A
str_view(data, pattern = "A")

#Detect a specific pattern
str_detect(data, pattern = "A")
str_detect(data, pattern = "AT")
str_locate(data, pattern = "AT")

########################### regex: Regular Expressions ########################
## Metacharacters
vals <- c("a.b", "b.c", "c.d")

#Replace all the "." with a space
str_replace(vals, "\\.", " ")

#Replace all instances
vals <- c("a.b.c", "b.c.d", "c.d.e")
str_replace(vals, "\\.", " ")     #only replaces the first instance (default)
str_replace_all(vals, "\\.", " ") #replace all instances

## Sequences
#Only keep strings with digits
val2 <- c("test 123", "test 456", "test")
str_subset(val2, "\\d")

## Character Class
#Count the number of lowercase vowels in each string
str_count(val2, "[aeiou]")

#Count any digit
str_count(val2, "[0-9]")

## Quantifiers
#Example: Find the Phone Numbers
strings <- c("550-153-7578",
             "banana",
             "435.114.7586",
             "home: 672-442-6739")

#Make a regex that finds the phone number
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

#Which strings contain phone numbers?
str_detect(strings, phone)

#Subset only the strings with phone numbers
test <- str_subset(strings, phone)
test

############################# Think, Pair, Share ##############################
#Replace all the "." with "-" and extract only the numbers (leaving the letters behind)
#Remove any extra white space. You can use a sequence of pipes.
test |> 
  str_replace_all(pattern = "\\.", replacement = "-") |> #replace periods with -
  str_replace_all(pattern = "[a-zA-Z]|\\:", replacement = "") |>  #remove everything we don't want
  str_trim() #trim the white space

################################ tidytext #####################################
## Load data and explore it
head(austen_books()) 
tail(austen_books())

#Clean and add a column for line and chapter
original_books <- austen_books() |> #get all of Jane Austen's books
  group_by(book) |> 
  mutate(line = row_number(), # find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", #count the chapters (stars with the word chapter followed by a digit or roman numeral)
                                                 ignore_case = TRUE)))) |> #ignore lower or uppercase
  ungroup() #ungroup it so we have a dataframe again
head(original_books) #don't try to view the entire thing... it's >73000 lines...

#Unnest data to that it's only one word per row
tidy_books <- original_books |> 
  unnest_tokens(output = word, input = text) #add a column named word, with the input as the text column
head(tidy_books) #there are now >725,000 rows

#Remove stopwords
head(get_stopwords()) #see an example of all the stopwords
cleaned_books <- tidy_books |> 
  anti_join(get_stopwords()) #dataframe without the stopwords
head(cleaned_books)

#Count the most common words across all her books
cleaned_books |> 
  count(word, sort = TRUE)

#Sentiment Analysis (how many positive and negative words)
sent_word_counts <- tidy_books |> 
  inner_join(get_sentiments()) |> #only keep positive or negative words
  count(word, sentiment, sort = TRUE) #count them
head(sent_word_counts)[1:3, ]

#Plot it
sent_word_counts |> 
  filter(n > 150) |> #take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) |> #add a column where if the word is negative make the goun negative
  mutate(word = reorder(word, n)) |> #sort it so it goes from largest to smallest
  ggplot(aes(x = n, y = word, fill = sentiment)) +
  geom_col() +
  labs(y = "Contribution to sentiment")

################################ Wordcloud ####################################
#Make a word cloud
words <- cleaned_books |> 
  count(word) |> #count all the words
  arrange(desc(n)) |> #sort the words
  slice(1:100) #take the top 100

wordcloud(words = words$word, freq = words$n, colors = brewer.pal(8, "Dark2"))
