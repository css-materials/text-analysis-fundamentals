# Text Mining with R
# Chapter 1.3 Tidying the works of Jane Austen
# https://www.tidytextmining.com/tidytext.html#tidyausten

library(janeaustenr)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

# the janeaustenr package provides austen texts in a one-row-per-line format, 
# where a line is analogous to a literal printed line in a physical book
# https://github.com/juliasilge/janeaustenr

# use austen_books() to return a tidy df 
# use utate() to annotate a linenumber and chapter
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

# restructure this df into the one-token-per-row format
tidy_books <- original_books %>%
  unnest_tokens(word, text)

# remove stop words with anti_join()
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)

# use count() to find most common words in all books
tidy_books %>%
  count(word, sort = TRUE)

# filter, reorder and plot them
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# see the rest of Chapter 1 for more examples and more anayses
# the code below comes from Chapter 3 and focuses on tf and tf-idf

###################################################################################

# Text Mining with R
# Chapter 3,1 Term frequency in Jane Austen’s novels
# https://www.tidytextmining.com/tfidf.html#term-frequency-in-jane-austens-novels

# Analyze TF in Jane Austen books 
# https://www.tidytextmining.com/tfidf.html#the-bind_tf_idf-function

# TF is simply the term frequency or a count of how frequent a word occurs in a document

# count all words
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)
book_words

# summarize total words grouped by each book
total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
total_words

# join these data by book
book_words <- left_join(book_words, total_words)
book_words
# this book_words data frame has one word for each word-book combination
# n: number of times that word is used in that book
# total: total words in that book

# look at the n/total for each book
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
# many words occur rarely, few words occur very frequently
# zipf's law (see book for code and examples) and https://simple.wikipedia.org/wiki/Zipf%27s_law


# Analyze TF-IDF in Jane Austen books 
# https://www.tidytextmining.com/tfidf.html#the-bind_tf_idf-function

# TF is frequency count of words per document 

# IDF measures how important a word is to a document by:
# decreasing the weight for commonly used words and 
# increasing the weight for words that are rarely used in corpus of documents
# for a theoretical explanation of tf-idf: https://web.stanford.edu/~jurafsky/slp3/6.pdf


# calculate tf-idf with bind_tf_idf() function
book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)
book_tf_idf
# notice that idf and thus tf-idf are zero for the most common words

# now look at terms with high tf-idf by rearranging the previous results 
book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
# notice those are proper nouns important to each of these novels:
# none of them appear in all novels, and each is characteristic of a particular one


# visualize the results 
book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)