#install.packages("foreach")
#install.packages("dplyr")
#install.packages(c("httr", "devtools"))
#install.packages("kableExtra")
library(kableExtra)
library(tidyverse)
library(dplyr)
library(plyr)
library(httr)
library(gh)
library(repurrrsive)
library(jsonlite)
library(doParallel)
library(foreach)
library(lubridate)
library(ggplot2)
library(purrr)


rm(list=ls())
options(encoding="UTF-8")

my_token = "60a2fb831c0fdf8e5068fa30d01bb451f33b5fc3" # github personal token
Sys.setenv(GITHUB_TOKEN = my_token)

user_repos_list = gh("/users/agentzh/repos",.limit = Inf)
repo <- dir.create(file.path("repo"), recursive = TRUE)
path <- "./repo/"
files <- dir(path, pattern = "*.json")

# Iterate through the json_array elements and save them in a file
for(i in 1:length(user_repos_list)) {
  Export_json = toJSON(user_repos_list[i])
  write(Export_json,paste(path, "repos_", user_repos_list[[i]][3], ".json", sep=""))
}

#Load Repos to df

files = dir(path, pattern = "*.json")
files = files %>% file.path(path,.)
repo_df <- files %>%
  map_df(~fromJSON(., flatten = TRUE))

# Github users Followers Details

followers_list = gh("/users/agentzh/followers",.limit = Inf)
follower <- dir.create(file.path("follower"), recursive = TRUE)

followers_path <- "./follower/"
followers_files <- dir(path, pattern = "*.json")

# Iterate through the json_array elements and save them in a file
for(i in 1:length(followers_list)) {
  Export_json = toJSON(followers_list[i],pretty = TRUE)
  write(Export_json,paste(followers_path,"followers_",followers_list[[i]][1],".json",sep=""))
}

#Read followers json file to dataframe
followers_files = dir(followers_path, pattern = "*.json")
followers_files = followers_files %>% file.path(followers_path,.)

followers_df = followers_files %>% map_df(.,~fromJSON(., flatten = TRUE))

#Issues of User's each repos
issue <- dir.create(file.path("issue"), recursive = TRUE)
issues_path <- "./issue/"
for(i in 1:length(user_repos_list)){
  if (user_repos_list[[i]]$has_issues) {
    issue_url = paste("/repos/agentzh/",user_repos_list[[i]]$name,"/issues",sep="")
    issue_list = gh(issue_url, .limit = Inf, state= "all")
    Export_json = toJSON(issue_list,pretty = TRUE)
    write(Export_json,paste(issues_path,"issue_",user_repos_list[[i]][3],".json",sep=""))
  }
}

#Reading Issues
issues_files <- dir(issues_path, pattern = "*.json")
issues_files = issues_files %>% file.path(issues_path,.)
issues = list()
for(i in 1:length(issues_files)) {
  df = fromJSON(issues_files[[i]], flatten = TRUE)
  if (!df[1,1]==""){
    issues[[i]] = df
  }
}

removed_issues = bind_rows(issues)

issues_df = as_tibble(removed_issues)

#Describing user

user = gh("/users/agentzh")
json_text = toJSON(user)

users_df_new = fromJSON(json_text, flatten = TRUE)

table_user_df <- ldply (users_df_new, data.frame)

df_agentzh <- tibble(user_name = user$name, user_login = user$login, 
                     user_id = user$id, public_repos = user$public_repos,
                     followers = user$followers, 
                     date_created = as.Date(user$created_at))
kable(head(df_agentzh))

#Describing followers

df_agentzh_followers = followers_df %>% select(login, id, repos_url,url)
kable(head(df_agentzh_followers))

#Describing repos
user_repos_df = repo_df %>% select(name,language,size, forks_count, stargazers_count,
                                   watchers_count,open_issues_count)

kable(head(user_repos_df))

# Describing Issues 

issues_df %>% mutate(created_at = ymd_hms(created_at))

user_issues_df = merge(x = repo_df, y = issues_df, by.x="url",by.y="repository_url",all.x = TRUE, sort=F)

user_issues_df = user_issues_df %>% 
  select(name,number,open_issues_count,open_issues,created_at.y,updated_at.y) %>% 
  mutate(issue_created_at = ymd_hms(created_at.y),
         issue_updated_at = ymd_hms(updated_at.y)) %>% 
  select(name,number,open_issues_count,open_issues,issue_created_at,issue_updated_at)

# date difference is in days
user_issues_df = user_issues_df %>% 
  mutate(date_diff = difftime(issue_updated_at, issue_created_at, tz = "EST", units="days")) %>% 
  mutate(avg_duration_to_close_issue = mean(date_diff, na.rm=TRUE))
 
kable(head(user_issues_df))

##Visualization

#Plot 1
#Distribution of Languages based on repos

plot_1 <- ggplot(repo_df, aes(x = "", fill = as.character(language))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text()) + 
  labs(fill = "Languages", 
       x = NULL, 
       y = NULL, 
       title = "Spread of Languages across the repositories")

plot_1 + coord_polar(theta = "y", start = 0)

#Plot 2
ggplot(data = repo_df, aes(x = as.character(name), y = size)) +
  coord_flip() +
  geom_bar(stat="identity") + 
  labs(title="Bar Graph", 
       subtitle="User Repository VS Size",
       x="User_name",
       y="Size")

#Plot 3

number_of_issues <- issues_df %>%
  group_by(as.integer(user.id),as.character(user.login)) %>% 
  summarize(number_issues = count(as.integer(user.id)))

ggplot(data = number_of_issues, aes(x = number_of_issues$number_issues$freq)) +
  stat_bin(bins=30) +
  labs(title="Bar Graph", 
       subtitle="Count of Issues VS Users",
       x="User",
       y="Count")

