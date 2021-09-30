source('functions/livestreamSetup.R')
letters
# If you put the command in a parenthetical, it assigns it and prints it at the same time
(vector<-c(1,2,6))
vector[2]

vector[c(2,2)]
vector[c(3,2,1)]
string_vector<-c("one","two","three")
class(string_vector)
DF<-data.frame(first_column = string_vector, second_column = string_vector)
# A list is a tree of stuff
?list
test_list<-list("dog on tree")
str(test_list)
test_list[1][[1]]
test_list[2]<-list("dog on rock")

test_list[[1]][2]<-"dog on rock"

test_list[[3]]<-DF




test_list<-list("me")
test_list[[1]][2]<-"you"
test_list[[2]]<-"them"
test_list[[3]]<-DF




