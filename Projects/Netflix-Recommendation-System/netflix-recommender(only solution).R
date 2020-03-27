#To load the datasets from their respective file paths(locations)


data1 = read.csv('C:\\Users\\Intellipaat-Team\\Desktop\\DESKTOP\\rcmt\\netflix-prize-data\\combined_data_1.txt', header = FALSE,sep = ",", stringsAsFactors = FALSE)

data1 = data1[,-3]

colnames(data1) = c('Cust_Id', 'Rating')

sum(is.na(data1))

str(data1)

#----------------------------------------------------------------------------------------------------------------------------------

data2 = read.csv('C:\\Users\\Intellipaat-Team\\Desktop\\DESKTOP\\rcmt\\netflix-prize-data\\combined_data_2.txt', header = FALSE,sep = ",", stringsAsFactors = FALSE)

data2 = data2[,-3]

colnames(data2) = c('Cust_Id', 'Rating')

sum(is.na(data2))

str(data2)

#-----------------------------------------------------------------------------------------------------------------------------------------------

data3 = read.csv('C:\\Users\\Intellipaat-Team\\Desktop\\DESKTOP\\rcmt\\netflix-prize-data\\combined_data_3.txt', header = FALSE,sep = ",", stringsAsFactors = FALSE)

data3 = data3[,-3]

colnames(data3) = c('Cust_Id', 'Rating')

sum(is.na(data3))

str(data3)

#--------------------------------------------------------------------------------------------------------------------------------------------

data4 = read.csv('C:\\Users\\Intellipaat-Team\\Desktop\\DESKTOP\\rcmt\\netflix-prize-data\\combined_data_4.txt', header = FALSE,sep = ",", stringsAsFactors = FALSE)

data4 = data4[,-3]

colnames(data4) = c('Cust_Id', 'Rating')

sum(is.na(data4))

str(data4)


#----------------------------------------------------------------------------------------------------------------------------------------------
# To combine all datasets into one
#data = rbind(data1,data2,data3,data4)

#To find the distribution of different ratings in the datset
library(dplyr)

data = data1 %>% group_by(Rating) %>% summarise(count = n())

data = data[-6,]

# get movie count by counting nan values
movie_count = sum(is.na(data1))

# get customer count
cust_count = data1 %>% summarize(n_distinct(Cust_Id)) - movie_count

cust_count = as.integer(cust_count)

# get rating count
rating_count = nrow(data1) - movie_count

#---------------------------------------------------------------------------------------------------------------------------------------------

#To plot the distribution of the ratings in as a bar plot
library(ggplot2)

# To disable the scientific notation
options(scipen = 999)


ggplot(data, aes(factor(Rating), y = count, fill=factor(Rating)))+ 
    geom_bar(stat = "identity")+
    geom_text(aes(label= sprintf("%d%%",round(count/sum(count) * 100))), vjust=-0.5)+
    labs(x = 'Ratings', y = 'Rating Count', fill = 'Ratings')+
    labs(title = sprintf("Total pool: %d movies, %d customers, %d ratings given\n", movie_count,cust_count,rating_count))+
    theme_bw()
#----------------------------------------------------------------------------------------------------------------------
#To load the preprocessed datset 'netflix_dataset' from its respective location

#To load only first 500000 rows for faster computation

data = read.csv('C:\\Users\\Intellipaat-Team\\Desktop\\DESKTOP\\rcmt\\netflix-prize-data\\netflix_data.csv', stringsAsFactors = FALSE, nrows = 500000)


#To load the movie_titles dataset from its respective location
movies = read.csv('C:\\Users\\Intellipaat-Team\\Desktop\\DESKTOP\\rcmt\\netflix-prize-data\\movie_titles.csv',encoding = "ISO-8859-1", header = FALSE, stringsAsFactors = FALSE)

colnames(movies) = c('Movie_Id', 'Year', 'Name')

str(movies)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#To create a list of all the movies rated less often(only include top 30% rated movies)

library(dplyr)

df_movie_summary = data %>% group_by(Movie_Id) %>% summarise(count = n(),mean(Rating))


movie_benchmark = round(quantile(df_movie_summary$count,0.7, names = FALSE),0)

drop_movie_list = df_movie_summary[df_movie_summary$count < movie_benchmark,1]

#To create a dataframe of all the movies rated less often
drop_movie_list = as.data.frame(drop_movie_list)


#To create a list of all the inactive users(users who rate less often)
df_movie_summary1 = data %>% group_by(Cust_Id) %>% summarise(count = n(),mean(Cust_Id))


cust_benchmark = round(quantile(df_movie_summary1$count,0.7, names = FALSE),0)

drop_cust_list = df_movie_summary1[df_movie_summary1$count < cust_benchmark,1]

#To create a dataframe of the users who rate less often
drop_cust_list = as.data.frame(drop_cust_list)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------

#To remove all the movies rated less often and users who rate less often
library(dplyr)

data = anti_join(data, drop_movie_list, by = "Movie_Id")
data = anti_join(data, drop_cust_list, by = "Cust_Id")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


library(reshape2)

#Create ratings matrix for 'ratings' matrix with Rows = userId, Columns = movieId

ratingmat <- dcast(data, Cust_Id~Movie_Id, value.var = "Rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds


library(recommenderlab)

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list



#Obtain recommendations
recom_result <- matrix(0,10)

for (i in c(1:10)){
    recom_result[i] <- movies[as.integer(recom_list[[1]][i]),3]
}


##Print top 10 recommendations
recom_result

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#To calculate the evaluation metrics for the SVD model using thhe 3-fold cross-validation
evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation",train = 0.8, k=3, given=-1, goodRating = 5) #k=3 meaning a 3-fold cross validation.

#To calculate evaluation metrics for top 1,3,5, and 10 recommendations
evaluation_results <- evaluate(evaluation_scheme, method="SVD", n=c(1,3,5,10))

#To print the evaluation results
eval_results <- getConfusionMatrix(evaluation_results)[[1]]

