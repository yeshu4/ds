#Loading the packages
library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)

#Loading the files
books <- read.csv("C:/Users/intellipaat/Desktop/book/books.csv")
ratings <- read.csv("C:/Users/intellipaat/Desktop/book/ratings.csv")
book_tags <- read.csv("C:/Users/intellipaat/Desktop/book/book_tags.csv")
tags <- read.csv("C:/Users/intellipaat/Desktop/book/tags.csv")

#Having a glance at the data
View(ratings)
View(books)
View(book_tags)
View(tags)

-------------------------------------------------------------
#Data cleaning

#Finding duplicate ratings
  
ratings %>% group_by(user_id, book_id) %>% mutate(N=n()) -> ratings
table(ratings$N)
ratings %>% filter(N>1) -> duplicate_ratings  

#Remove duplicate ratings
ratings %>% filter(N==1) -> ratings

#Removing users who rated fewer than 3 books

ratings %>% group_by(user_id) %>% mutate(Ratings_Given = n()) -> ratings
ratings %>% filter(Ratings_Given>2) -> ratings
----------------------------------------------------------
#Selecting a sample from the entire data-set
  
set.seed(1)
user_fraction <- 0.02
users <- unique(ratings$user_id)
sample_users <- sample(users, round(user_fraction * length(users)))  
ratings %>% filter(user_id %in% sample_users) -> ratings

------------------------------------------------
#Making a distribution of ratings 
  
  ratings %>% 
  ggplot(aes(x = rating, fill = factor(rating))) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE)

#Number of ratings per book

ratings %>% 
  group_by(book_id) %>% 
  summarize(number_of_ratings_per_book = n()) %>% 
  ggplot(aes(number_of_ratings_per_book)) + 
  geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0,40))

---------------------------------
#Finding the count of different genres
  
  
genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Cookbooks", "Crime", "Fantasy", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion", "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))


available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]

#plotting the percentage of each genre

tmp <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n / sumN) %>%
  arrange(-percentage) %>%
  left_join(tags, by = "tag_id")  


tmp %>% 
  ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = 'YlOrRd') + labs(y = 'Percentage', x = 'Genre')



------------------------------------------
#Finding the top 10 books with highest ratings
  
  books %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(title, ratings_count, average_rating) 

#Finding the 10 most popular books

books  %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(title, ratings_count, average_rating) 
------------------------------
  #Re-structuring our data to build collaborative filtering
  
dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)
ratingmat[,-1] -> ratingmat
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]
dim(ratingmat)
  
------------------------------------------------------
#converting the rating matrix into a real rating matrix
  
ratingmat0 <- ratingmat  
dim(ratingmat0)
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings
-----------------------------
#Splitting the data into train & test
  
sample(x=c(T,F),size=nrow(real_ratings),replace = T, prob = c(0.9,0.2)) ->split_book
real_ratings[split_book,]->recc_train
real_ratings[!split_book,]->recc_test
---------------------------------------
#Building the ubcf model
Recommender(data = recc_train,method="UBCF")->recc_model_ubcf
n_recommended_ubcf<-6

#Recommending books
predict(object=recc_model_ubcf,newdata=recc_test,n=n_recommended_ubcf)->recc_predicted_ubcf
--------------------------------------------------
#Recommeding books for user number-1
recc_predicted_ubcf@items[[1]]->user1_book_numbers
recc_predicted_ubcf@itemLabels[user1_book_numbers]

books %>% filter(id==6343) %>% select(original_title,authors)
books %>% filter(id==7482) %>% select(original_title,authors)
books %>% filter(id==2750) %>% select(original_title,authors)
-------------------------------------------------------
#Recommending books for user number-5
  
recc_predicted_ubcf@items[[5]]->user5_book_numbers
recc_predicted_ubcf@itemLabels[user5_book_numbers]

books %>% filter(id==4624) %>% select(original_title,authors)
books %>% filter(id==6867) %>% select(original_title,authors)
books %>% filter(id==7326) %>% select(original_title,authors)

  