install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("qdap")
install.packages("ggplot2")
install.packages("scales")
install.packages("tm")

library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(qdap)
library(ggplot2)
library(scales)
library(tm)

setwd("~/json")
business_json = stream_in(file("business.json"))
saveRDS(business_json, "business.Rds")
business = readRDS("business.Rds")
business_df = as.data.frame(business)
review_df = read.csv(file.choose(), nrows = 1000000)

#Analysing the data
## Looking at review pattern
attach(review_df)
cum_reviews = subset(review_df, select=c(date,review_id))
cum_reviews= group_by(cum_reviews,date)
cum_reviews= summarise_all(cum_reviews,funs("reviews" = n()))
cum_reviews= mutate(cum_reviews,dates = as.Date(date), cumulative_count = cumsum(reviews))
cum_reviews= arrange(cum_reviews,cum_reviews$dates)

ggplot(cum_reviews, aes(x = dates, y = cumulative_count)) +
    geom_line(size = 1, color='blue') +
    scale_y_continuous(labels = comma) +
    labs(x = "Year of Review", y = "Cumulative Review Count") + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(size = 8, angle = 45), axis.text.y = element_text(size = 8))

#Filtering Inactive Users
ratings_users = subset(review_df, select=c(user_id, stars))
ratings_users = group_by(ratings_users, user_id)
ratings_users = summarise_all(ratings_users, funs("count" = n(), "mean" = mean, "median" = median))
ratings_users = arrange(ratings_users, ratings_users$count)
ratings_users = ungroup(ratings_users)


inactive_user_proportion <- function(criteria){
  proportion = rep(0, as.numeric(criteria))
  for (i in 1:as.numeric(criteria)){
    proportion[i] = (nrow(filter(ratings_users, count <= i))*100) / nrow(ratings_users)
  }
  plot(proportion, type = "b", xlim = c(1,i), xlab = "Frequency of Reviews", ylab = "Percentage of users", col = "red")
}
inactive_user_proportion(6)

inactive_users = filter(ratings_users, ratings_users$count == 1)
review_df_clean = review_df[!(review_df$user_id %in% inactive_users$user_id),]

round(nrow(review_df_clean) / nrow(review_df), 3)

# Merging the Business and Review data
review_business = merge(review_df_clean, business_df, by = "business_id")
review_business_trim = select(review_business, -starts_with("hour"), -starts_with("attribute"), -contains("votes"), -contains("type"))

#Getting insights about the reviews by categories
category_count = group_by(review_business_trim, as.character(categories))
category_count = summarise(category_count, Count = n() )
category_count = arrange(category_count, desc(category_count$Count) )

#Zooming in states
genre_count = subset(review_business_trim,select = c(state,categories))
genre_count = filter(genre_count, str_detect(genre_count$categories, "Restaurant"))
genre_count = unnest(genre_count,categories)
genre_count = group_by(genre_count, state)
genre_count = count(genre_count, categories)
genre_count = arrange(genre_count, desc(genre_count$n))

## Remove non-essential categories AND filtering the 90th percentile 
state_restaurant_count = group_by(genre_count, state)
state_restaurant_count = filter(state_restaurant_count, categories != "Restaurants" || categories != "Nightlife" || categories != "Bars")
state_restaurant_count = filter(state_restaurant_count, n > quantile(n, 0.9)) 

## Visualise Share of Reviews by State
state_share = subset(state_restaurant_count,select=c(state, n))
state_share = group_by(state_share, state)
state_share = summarise_all(state_share, funs("count" = sum(n)))
state_share = arrange(state_share, desc(state_share$count))
state_share = mutate(state_share, proportion = round(count / sum(count), 2))

plot_state_share = state_share %>% 
  ggplot(aes(x = reorder(state, -proportion), y = proportion, fill = state)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) + 
  ggtitle("Proportion of Reviews by State") +
  labs(x = "State", y = "Proportion of Reviews") +
  theme( axis.text.x = element_text(face = "bold", size = 8, angle = 45), axis.text.y = element_text(face = "bold", size = 8))
plot_state_share

## Visualising cuisine review counts in Arizona and Nevada
plot_aznv_cuisine <- state_restaurant_count %>% 
   filter(state == "AZ" | state == "NV") %>%   
   ggplot(aes(x = reorder(categories, -n), y = n, fill = categories)) + 
   geom_bar(stat = "identity") +
   facet_grid(state ~., scales = "free") +
   scale_y_continuous(labels = comma) + 
   ggtitle("Review Count by Cuisine") +
   labs(x = "Cuisine", y = "No. of Reviews") +
   theme(legend.position = "none", axis.text.x = element_text(face = "bold", size = 7, angle = 90), 
        axis.text.y = element_text(face = "bold", size = 8))

plot_aznv_cuisine

#Creating The Corpus
## Filtering Pizza reviews from Nevada and Arizona
aznv_pizza = filter(review_business_trim, state == "AZ" | state == "NV")
aznv_pizza = filter(aznv_pizza, str_detect(categories, "Pizza"))

## Sentiment lexicon from Bing Liu (cs.uic.edu/~liub/FBS/sentiment-analysis.html)
pos_words = scan("positive-words.txt", what='character', comment.char=';')
neg_words = scan("negative-words.txt", what='character', comment.char=';')

# function
score.sentiment = function(text_vector, pos.words, neg.words, .progress='none')
{    
  require(plyr)
  require(stringr)
  
  scores <- ldply(text_vector, function(text_vector, pos.words, neg.words) {
    
    text_vector = gsub('[[:punct:]]', '', text_vector)
    text_vector = gsub('[[:cntrl:]]', '', text_vector)
    text_vector = gsub('\\d+', '', text_vector)
    text_vector = tolower(text_vector)
    
    word.list = str_split(text_vector, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    pos_matches = !is.na(pos.matches)
    neg_matches = !is.na(neg.matches)
    score = sum(pos_matches) - sum(neg_matches)
    
  }, pos.words, neg.words, .progress=.progress)
  
  scores_final = data.frame(sentiment_score = scores, text = text_vector)
  return(scores_final)
}


# Score sentiment of review texts
Pizza_sentiment = score.sentiment(aznv_pizza$text, pos_words, neg_words)

hist(scale(as.numeric(as.matrix(Pizza_sentiment))), breaks = 100, main = "Sentiment Distribution", xlab = "Sentiment Score")
hist(aznv_pizza$stars.x, main = "Ratings Distribution for All Pizza Reviews", xlab = "Ratings")

cor.test(aznv_pizza$stars.x, Pizza_sentiment[,1])

# Arranging Sentiment Scores with Star Ratings
Pizza_starsentiment = cbind(Pizza_sentiment, aznv_pizza$stars.x)
Pizza_starsentiment$text = as.character(Pizza_starsentiment$text)
colnames(Pizza_starsentiment) = c("sentiment_score", "text_review", "star_rating")

# Considering only extreme star ratings
star_end_pizza = filter(Pizza_starsentiment, star_rating == 1 | star_rating == 5)

attach(star_end_pizza)
hist(scale(star_end_pizza$sentiment_score), breaks = 100, main = "Sentiment Distribution for Extreme Ratings", xlab = "Sentiment Score")
cor.test(star_rating, sentiment_score)