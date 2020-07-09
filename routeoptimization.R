# Clearing Environmental Variables 
rm(list = ls(all=TRUE))
library(RMySQL)
library(Hmisc)
setwd("C:/Users/906601/Desktop/Project_Code/db")
Connection = dbConnect(RMySQL::MySQL(), dbname = "spring_clean", user = "root", password = "Royal@601") 

Stops_Info = dbGetQuery(Connection, "Select * from stops_info_db")
Travel_time = dbGetQuery(Connection,"Select * from travel_time_matrix_db")
Parameters_Info = dbGetQuery(Connection, "Select * from parameters_info_db")

write.csv(Stops_Info, file = "C:/Users/906601/Desktop/Project_Code/db/Stops_Info.csv"
          , row.names = F)
write.csv(Travel_time, file = "C:/Users/906601/Desktop/Project_Code/db/Travel_time.csv"
          , row.names = F)


#--------------------Is Data Missing ? ------------------------
#                      Yes
#         Travel_time rows are supposed to be ---> 38025 
#         But we have ---> 36270
#          So, 38025 - 36270 = 1755 Entries are missing in the data (i.e, 9 Stops data is missing)
#             Out of these I can fill 1755 - 9*9 = 1674 data points from the existing data(i.e,
    #                 To and Fro travel time between two stops)
#           For example,  A-->B distance is given. But B-->A  distance is missing. 
#                 Like Wise we can compute 1674 entries in the data.

#         Remaining entries in the data are the distances between the missing 9 stops.
#       for all the combinations of stops data is missing i.e, 
#                          9 c 2 = 36 
#           From above again we can fill other 36 (Same concept if A-->B is given, compute B-->A)
#                       Other 9 values are distance from that point to itself .i.e, 0 (A-->A)






#---------------------------------Getting Missing Data-------------------------------------------

## Take Unique values from Travel_time first column
To_stops <- Travel_time$FROM_STOP_ID
To_stops_data <- unique(To_stops, incomparables = FALSE)
To_stops_data


Total_Stops <- c(Stops_Info$STOP_ID)
Depots <- c('DEP10001','DEP10022','DEP25005','DEP35024','DEP45024', 'DEP50002')
Only_Stops <- setdiff(Total_Stops,Depots)

Total_Stops
Only_Stops

Missing_data <- setdiff(Total_Stops,To_stops_data)
Missing_data

rm(To_stops_data,To_stops)

#------------------     Understanding the data  ------------------------
# Here, I am trying to analysing if the distances from A-->B & B-->A are equal ?
#  If not equal  
    # How much are they differing by ?
      #  Analyse the spread of the differences

data_present <- setdiff(Total_Stops, Missing_data)

difference <- data.frame(NULL)
data_pres<-data_present


for (i in data_present){
  for (j in data_pres)
  {
    temp_1 <- Travel_time$TRAVEL_TIME[which((Travel_time$FROM_STOP_ID == i)
                                            & (Travel_time$TO_STOP_ID == j))]
    temp_2 <- Travel_time$TRAVEL_TIME[which((Travel_time$FROM_STOP_ID == j)
                                            & (Travel_time$TO_STOP_ID == i))]
    
    diff <- abs(temp_2 - temp_1)
    #if (diff >= 30){
    #  difference<- rbind(difference,c(j, i, diff))
    #}
    difference<-rbind(difference,diff)
  }
  data_pres<- data_pres[!data_pres %in% c(j)]
}

View(difference)
colnames(difference)<- c("data_present")
write.csv(difference, file = "C:/Users/906601/Desktop/Project_Code/db/diff.csv"
          , row.names = T)

diff <- read.csv(file = "diff.csv")

summary(diff)
plot(diff,xlab = "Combinations of Stops", ylab = "Difference")

rm(data_present,difference,data_pres,i,j,temp_1,temp_2,diff)

# -----------------------------From the plot-----------------------------------

#       I had 76.67% of my data within a difference of 0-10
#             21.8% of my data with a difference of 11-15
#             1.28% of my data with a difference of 16-21
#             0.25% of my data with a difference of 25-45

#     So generating a random number on the above probabilities and adding/subtracting 
#                        it to the original time
set.seed(123)
A <- 0:10
B <- 11:15
C <- 16:21
D <- 25:45
L <- sapply(list(A, B, C, D), length)

random_diff <- sample(c(A, B, C, D),
            size = 2000,
            prob = rep(c(0.7667, 0.2180, 0.0128, 0.0025) / L, L),
            replace = TRUE)
rm(A,B,C,D,L)

#------------------ Calculating distance between the Missing data----------------

# Here I am trying to calculate the distance between a missing point (A) another missing point (B)
#     So I have used properties of triangle here to solve that problem
#     If i have to calculate distance between two points (B, C)
#         here i have distance between a point(A) to each of the points B(a) & C (b)
#           If I consider that three points can form a triangle,
#               distance between B & C would fall in the range of
#                   a - b < BC < a + b


library(permute)

combinations<- combn(Missing_data,2,NULL,FALSE)
vector_df <- data.frame("FROM_STOP_ID" = character(0),"TO_STOP_ID" = character(0),"Travel_time"= numeric(0),
                        stringsAsFactors=FALSE)


index<- 1
for (i in combinations) {
  j <- i[1]
  k<- i[2]
  prev_min <- 0
  prev_max <- 1000
  for (t in Total_Stops){
    if (!(t %in% Missing_data)){
      temp_travel_j <- Travel_time$TRAVEL_TIME[which((Travel_time$FROM_STOP_ID == t)
                                                     & (Travel_time$TO_STOP_ID == j))]
      temp_travel_k <- Travel_time$TRAVEL_TIME[which((Travel_time$FROM_STOP_ID == t)
                                                     & (Travel_time$TO_STOP_ID == k))]
      
      
      minus <- temp_travel_j - temp_travel_k
      minus <- abs(minus)
      add <- temp_travel_j + temp_travel_k
      
      if ((minus > prev_min)& (minus< prev_max)){
        prev_min = minus
      }
      if ((add < prev_max)&(add > prev_min)){
        prev_max = add
      }
    }
  }
  
  x <- sample(prev_min:prev_max,1) 
  
  vector_df<- rbind(vector_df, data.frame(j, k, x))
  colnames(vector_df)<- c("k","j","y")
  y <- x + as.numeric(random_diff[index])
  index <- index +1
  vector_df<- rbind(vector_df, data.frame(k, j, y))
  colnames(vector_df)<- c("j","k","x")
}

colnames(vector_df)<- c("FROM_STOP_ID","TO_STOP_ID","TRAVEL_TIME")

View(vector_df)

Travel_time <-rbind(Travel_time, vector_df)
View(Travel_time)

rm(combinations,vector_df,i,j,k,t,temp_travel_j,temp_travel_k,minus,add,prev_max, prev_min,x,y)


#----------------------------Filling Missing Data--------------------------------

# Here I am trying to fill the next 1683 entries .i.e.,
#     1674 entries from the data (Using the logic A-->B ~= B-->A)
#         + 9 entries (Distance from a missing point to itself = '0')

vector_df_2 <- data.frame("FROM_STOP_ID" = character(0),"TO_STOP_ID" = character(0),"Travel_time"= numeric(0),
                          stringsAsFactors=FALSE)

for (i in Missing_data) {
    for (j in Total_Stops) {
    if(!(j==i)){
      if(!(j %in% Missing_data)){
        temp_Travel_time <- Travel_time$TRAVEL_TIME[which((Travel_time$FROM_STOP_ID == j)
                                                          & (Travel_time$TO_STOP_ID == i))]
        temp_Travel_time <- temp_Travel_time + random_diff[index] 
        vector_df_2<- rbind(vector_df_2, data.frame(i,j, temp_Travel_time))
        index <- index +1
      }
    } else{
      temp_Travel_time <- 0
      vector_df_2<- rbind(vector_df_2, data.frame(i,j, temp_Travel_time))
    }
  }
}

colnames(vector_df_2)<- c("FROM_STOP_ID","TO_STOP_ID","TRAVEL_TIME")
View(vector_df_2)

Travel_time <- rbind(Travel_time, vector_df_2)
View(Travel_time)


rm(vector_df_2, i,j,index,random_diff,temp_Travel_time)

# ------------------------- Results from the Data-------------------------
#     I have 6 Depots on the whole
#         In which two Depots are overlapping with other two Depots
#           I have come to above conclusion calculating distances between the Depots
#               Distance between DEP10001 and DEP45024 = 0
#                                 DEP50002 and DEP35024 = 0
#                                   DEP10022 and DEP25005 = 32

# From the above Data, I have decided to cluster the data i.e., assign a Stop to a particular depot
#       learning the time taken to travel the distance from Depot to Stop + Service time at that stop

# Note : I have considered only 4 Clusters (Depots) Since two are overlapping with two other Stops.


#--------------------Assigning Each stop to a Depot based on above approach--------------------

DEP10001<-NULL
DEP35024<-NULL
DEP10022<-NULL
DEP25005<-NULL

table_to_view<-NULL

for (i in Only_Stops)
{
  Service_time <- 0;
  Service_time<- Stops_Info$TIME_TO_COMPLETE_WORK[which(Stops_Info$STOP_ID==i)];
  x<-500;
  Total_time<-0;
  allocated_depot<-NULL;
  for (j in Depots)
  {
    TimetoTravel = 0;
    TimetoTravel <- Travel_time$TRAVEL_TIME[which((Travel_time$FROM_STOP_ID == j)
                                                  & (Travel_time$TO_STOP_ID == i))]
    Total_time <- Service_time + TimetoTravel;
    
    if (Total_time < x)
    {
      x<-Total_time;
      allocated_depot<-j;
    }
  }
  table_to_view <- rbind(table_to_view,c(i,allocated_depot,x));
  if (allocated_depot=='DEP45024'||allocated_depot=='DEP10001')
  {
    DEP10001 <- rbind(DEP10001,i);
  }else if (allocated_depot=='DEP35024'||allocated_depot=='DEP50002')
  {
    DEP35024 <- rbind(DEP35024,i);
  }else if (allocated_depot=='DEP10022')
  {
    DEP10022 <- rbind(DEP10022,i);
  }else if (allocated_depot=='DEP25005')
  {
    DEP25005 <- rbind(DEP25005,i);
  }
}



table_to_view <- as.data.frame(table_to_view)
colnames(table_to_view) <- c("STOP_ID","ALLOCATED_TO","Total_time")
View(table_to_view)



write.csv(table_to_view, file = "C:/Users/906601/Desktop/Project_Code/db/Allocation_table.csv"
          , row.names = F)

rm(i, j, x, Total_time, Service_time,TimetoTravel,allocated_depot, table_to_view)

View(DEP10001)
View(DEP35024)
View(DEP10022)
View(DEP25005)

#-------Functions being used in the Code to find Travel time/ Service time /Total Route time---------
#     Route_Travel_Time (parameter = route) --> To Calculate total travel time of a route 
#     Route_Service_Time (parameter = route) --> To Calculate Service  time of a route
#     Total_Route_Time (parameter = route) --> To calculate Total Route Time i.e.,
#                                                  Total Travel Time + Total Service time
#

#------------------------Function for getting Travel Time----------------------------

Route_Travel_Time <- function(Route_info){
  # formatize data
  Route_info <- as.character(Route_info)
  stops_in_routes <- strsplit(x = as.character(Route_info), split = "->")[[1]]
  route_travel_time <- 0
  for(i in 1:(length(stops_in_routes)-1))
  {
    route_travel_time <- route_travel_time + Travel_time$TRAVEL_TIME[which((Travel_time$FROM_STOP_ID == stops_in_routes[i])
                                                                           & (Travel_time$TO_STOP_ID == stops_in_routes[i+1]))]
  }
  return(route_travel_time)
}

#-----------------------Function for getting Service Time----------------------------

Route_Service_Time <- function(Route_info){
  # formatize data
  Route_info <- as.character(Route_info)
  stops_in_routes <- strsplit(x = as.character(Route_info), split = "->")[[1]]
  Service_time <- 0
  for(i in 1:length(stops_in_routes)){
    Service_time <- Service_time + Stops_Info$TIME_TO_COMPLETE_WORK[which(Stops_Info$STOP_ID==stops_in_routes[i])];
  }
  return(Service_time)
}

#----------Function for the Total Route Time = Total Travel Time + Total Service time------------

Total_Route_Time <- function(Route_info){
  Total_time <- Route_Travel_Time(Route_info) + Route_Service_Time(Route_info)
  return(Total_time)
  
}

# ----------------------Penalties Awarded according to the constraints---------------------------
#   Constraints given in the problem are 
#           Maximum time (travel and job execution time) threshold per route. (660mins - 11hours)
#           Maximum stops threshold per route. (5/6/7)
#           Minimum stops threshold per route. (3/4/5)
#           Time Window (early or late) (30 min or 60 min) : Allowable time window and large 
#                                         violations window, if reaches early or late.
          


#-----------------------------------Functions Explained---------------------------------
#     penalty_Maxtime (route, total_route_time ): Calculating penalty if it is exceeding the 
#                                                         maximum allowed time 660 min
#     
#     penalty_OverStops(route, min_stops, max_stops, total_route_time)    : Calculating penalty 
#                                                 if it exceeds allowed stops to be Serviced.
#     penalty_Window(route, Window) : Calculate penalty if it coming early or late



##----------------------Penalty function for Maximum time allowed---------------------
#       Total travel and job execution must not exceed 660mins for any route. 
#       Apply penalty if exceeds, 
#           1 - 60 mins - exceeded minutes + 10% of total route time
#           61 - 120 mins - 2 times the exceeded minutes + 20% of total route time
#           121 - 180 mins - 3 times the exceeded minutes + 30% of total route time
#           181 - 240 mins - 4 times the exceeded minutes + 40% of total route time

           
penalty_Maxtime <- function(route, time_route)
{
  penalty <- 0;
  route_len <- length(strsplit(x = as.character(route), 
                               split = "->")[[1]]) - 2
  
  if(time_route<660){
    exceeded_minutes <- 0
  } else{
    exceeded_minutes <- Total_Route_Time(route) - 660
  }
  if(exceeded_minutes!=0){
    if ((exceeded_minutes <61)){
      penalty <- exceeded_minutes + (0.10 * time_route);
    } else if (exceeded_minutes <121){
      penalty <- (2*exceeded_minutes) + (0.20 * time_route)
    } else if (exceeded_minutes <181){
      penalty <- (3*exceeded_minutes) + (0.30 * time_route)
    } else if (exceeded_minutes <241) {
      penalty <- (4*exceeded_minutes) + (0.40 * time_route)
    } else{
      penalty <- (5*exceeded_minutes) + (0.50 * time_route)
    }
  }
  return(penalty)
}

#penalty_Maxtime(route,time_route)
#route <- c("DEP35024->STP68855->STP62829->STP66273->STP69307->STP69424->STP69927->STP70398->DEP35024")

#time_route <- Total_Route_Time(route)


#---------------------Penalty ----------------------------
#       If any route has more/less than the stops threshold
#             1 stop - 10% of total route time
#             2 stops - 20% of total route time
#             3 stops - 30% of total route time...

penalty_OverStops <- function(route, min_stops, max_stops, time_route)
{
  penalty <- 0
  exceeded_Routes<-0
  route_len <- length(strsplit(x = as.character(route), 
                               split = "->")[[1]]) - 2
  if (route_len< min_stops){
    exceeded_Routes <- min_stops - route_len
  } else if(route_len > max_stops){
    exceeded_Routes <- route_len - max_stops
  }
  
  if(exceeded_Routes==1){
    penalty <- (0.10 * time_route)
  } else if(exceeded_Routes==2){
    penalty <- (0.20 * time_route)
  }else if (exceeded_Routes==2){
    penalty <- (0.30 * time_route)
  }
  return(penalty)
}
# Penalty function testing
#penalty_OverStops(route,3,5,time_route)
#route <- c("DEP35024->STP68855->STP62829->STP66273->STP69307->STP69424->STP69927->STP70398->DEP35024")
#Missing_data
#time_route <- Total_Route_Time(route)



#     If a vehicle reaches a customer before/after small time window (30 mins) - no penalty
#           with in small and large time window - 10% of total route time
#             beyond large time window 
#           1st 60mins - exceeded minutes + 10% of total route time
#           2nd 60mins - 2 times the exceeded minutes + 20% of total route time



penalty_Window <- function(route, Window_intr){
  penalty<-0
  time_route<- Total_Route_Time(route)
  Route_info <- as.character(route)
  stops_in_routes <- strsplit(x = as.character(route), split = "->")[[1]]
  stops_considered_travel <- stops_in_routes[2:(length(stops_in_routes)-1)]
  stops_considered_travel<-concatenate(stops_considered_travel)
  
  if(!(length(stops_in_routes)<=3)){
    stops_considered_service <- stops_in_routes[2:(length(stops_in_routes)-2)]
    stops_considered_service<-concatenate(stops_considered_service)
    interval <- as.integer(Route_Travel_Time(stops_considered_travel)+Route_Service_Time(stops_considered_service))
  } else {
    interval <- 0
    penalty<- 0
  }
  
  wind_allowed = 180 + (2* Window_intr)
  
  if(interval<wind_allowed){
    exceeded_minutes <- 0
  } else{
    exceeded_minutes <- interval - wind_allowed
  }
  if(exceeded_minutes!=0){
    if ((exceeded_minutes <61))
    {
      penalty <- exceeded_minutes + (0.10 * time_route);
    } else if (exceeded_minutes <121){
      penalty <- (2*exceeded_minutes) + (0.20 * time_route)
    } 
  }
  return(penalty)
}


#---------------This is the function to calculte the whole cost involved in processing a population-----
#                   Function for calculating Total cost for a population

cost <- function(Population, min_stops, max_stops, Window_intr) {
  population_cost <- 0
  for (i in (1:length(Population))) {
    time_route <- as.numeric(Total_Route_Time(Route = Population[i]))
    penalty_1 <- penalty_Maxtime(route = Population[i], time_route)
    penalty_2 <- penalty_OverStops (route = Population[i], min_stops, max_stops,time_route)
    penalty_3 <- penalty_Window(route = Population[i], Window_intr)
    population_cost <- as.numeric(population_cost + time_route + penalty_1 + penalty_2 + penalty_3)
  }
  return(population_cost)
}




#-----------------Maximum Route Cost in a Population---------------------

#         Calculated to generate children which would cost less than the 
#                    maximum in the population along with penalty
#
max_RouteCost<- function(Population, min_stops, max_stops, Window_intr)
{ 
  max_RouteCost_value<-0
  for (i in (1:length(Population))) {
    route_Cost <- 0
    travel_time_route <- as.numeric(Total_Route_Time(Route = Population[i]))
    penalty_1 <- penalty_Maxtime(route = Population[i], time_route)
    penalty_2 <- penalty_OverStops (route = Population[i], min_stops, max_stops,time_route)
    penalty_3 <- penalty_Window(route = Population[i], Window_intr)
    
    route_Cost <- as.numeric(penalty_1 + penalty_2 + penalty_3 + travel_time_route)
    if(route_Cost>max_RouteCost_value){
      max_RouteCost_value<-route_Cost
      max_i <- i
      
    }
  }
  return(max_RouteCost_value)
}

#-------------------------Function for concatenating the routes ---------------------
#     In my code, I have split the data into a character vector, this function helps to 
#         concatenate it back to the following format :    "DEP->A->B->C->DEP"
concatenate <- function(Stop_id) {
  for (i in 1:length(Stop_id)) {
    if (i == 1) {
      route = Stop_id[i]
    } else {
      route = paste(route, Stop_id[i], sep = "->")
    }
  }
  return(route)
}


#----------------------Generating random initial population--------------------------
# Here, I are trying to generate random route plan in a particular cluster(my function's parameter)
#                   without any constraints involved.


Create_InitialPopulation <- function(DEP_table, DEP_ID) {
  Input_stops <- DEP_table
  inp_data <- as.data.frame(Input_stops)
  colnames(inp_data) <- c("Input_stops")

  Init_Population<-NULL
  
  set.seed(456)
  while(nrow(inp_data)!=0){
    route_gen<-NULL
    random_number <- sample(3:7, 1)
    if (nrow(inp_data)<=random_number)
    {
      random_number <- nrow(inp_data)
    }
    ran_route<-NULL
    for (j in (1:random_number)){
      random_2 <- sample(1:nrow(inp_data), 1)
      ran_route<-c(ran_route,as.character(inp_data$Input_stops[random_2]))
      inp_data<- data.frame(inp_data[-c(random_2),])
      colnames(inp_data) <- c("Input_stops")
    }
    
    route_gen <- as.character(c(DEP_ID,ran_route,DEP_ID))
    route <- concatenate(route_gen)
    
    if (length(Init_Population) == 0) {
      Init_Population = route
    } else {
      Init_Population = c(Init_Population,route)
    }
  }
  return(Init_Population)
}

#-----------------------------CrossOver Function----------------------------
# Here in Crossover I am randomly taking two parents from a population
#     breaking each parent into two halfs - first half and second half
#       Now I am joining first parent first half and second parent second half for Child 1
#         and joining second parent first half and first parent second half for child 2
CrossOver = function(parent_1, parent_2) {
  parent_1 = as.character(parent_1)
  parent_2 = as.character(parent_2)
  stops_parent1 <- strsplit(x = parent_1, split = "->")[[1]]
  stops_parent2 <- strsplit(x = parent_2, split = "->")[[1]]
  
  if ((length(stops_parent1) %% 2) == 0) {
    first_parent_1 = stops_parent1[seq(1,length(stops_parent1)/2,1)]
    second_parent_1 = stops_parent1[seq((length(stops_parent1)/2 + 1),
                                        length(stops_parent1),1)]
  } else {
    
    first_parent_1 = stops_parent1[seq(1,floor(length(stops_parent1)/2),1)]
    second_parent_1 = stops_parent1[seq(ceiling(length(stops_parent1)/2),
                                        length(stops_parent1),1)]
  }
  
  if ((length(stops_parent2) %% 2) == 0) {
    first_parent_2 = stops_parent2[seq(1,length(stops_parent2)/2,1)]
    second_parent_2 = stops_parent2[seq((length(stops_parent2)/2 + 1),
                                        length(stops_parent2),1)]
  } else {
    
    first_parent_2 = stops_parent2[seq(1,floor(length(stops_parent2)/2),1)]
    second_parent_2 = stops_parent2[seq(ceiling(length(stops_parent2)/2),
                                        length(stops_parent2),1)]
  }
  
  
  new_parent_1 = c(first_parent_1, second_parent_2)
  new_parent_2 = c(first_parent_2, second_parent_1)
  
  final_parent_1 = concatenate(new_parent_1)
  final_parent_2 = concatenate(new_parent_2)
  return(c(final_parent_1,final_parent_2))
}


#----------------------------------------Mutation-------------------------------------------
# Here in Mutation, I am switching a random stop in a route with another stop from the route
#         to be precise interchanging two positions of stops within a route.

Mutation = function(route) {
  route = as.character(route)
  result_mutate = route
  stops_in_route <- strsplit(x = route, split = "->")[[1]]
  if (length(stops_in_route) > 3) {
    interchange_positions = sample(c(2:(length(stops_in_route)-1)),2,replace = FALSE)
    temp = stops_in_route[interchange_positions[1]]
    stops_in_route[interchange_positions[1]] = stops_in_route[interchange_positions[2]]
    stops_in_route[interchange_positions[2]] = temp
    result_mutate = concatenate(stops_in_route)
  }
  return(result_mutate)
}


#-------------------------------Genetic Algorithm-------------------------------

# Here the algorithm generates its children from the above CrossOver and Mutation functions
#       It also considers the childs only if atleast one child generates its cost 
#                 less than the maximum cost of a route in the current population

Genetic_algorithm = function(Init_Population, trials, min_stops, max_stops, Window_intr, CrossOver_prob, Mutation_prob) {
  cost_involved = c()
  trails_new = c()
  
  max_route_cost<- max_RouteCost(Init_Population, min_stops, max_stops, Window_intr)
  for (trials in 1:trials) {
    crossRandom = runif(1,0,1)
    if (crossRandom < CrossOver_prob) {
      parent_routes = sample(x = c(1:length(Init_Population)),size = 2,
                             replace = F)
      child_routes = CrossOver(Init_Population[parent_routes[1]],
                               Init_Population[parent_routes[2]])
      mutateRandom = runif(n = 1,min = 0,max = 1)
      if (mutateRandom < Mutation_prob) {
        child_routes[1] = Mutation(child_routes[1])
        child_routes[2] = Mutation(child_routes[2])
      }
      if ((Total_Route_Time(child_routes[1]) <= max_route_cost) | 
          (Total_Route_Time(child_routes[2]) <= max_route_cost )) {
        new_Population = c(Init_Population[-parent_routes],child_routes)
        old_Cost = cost(Init_Population, min_stops, max_stops, Window_intr)
        new_Cost = cost(new_Population, min_stops, max_stops, Window_intr)
        
        if (new_Cost < old_Cost) {
          
          cat("Reproduction Cycle : ", trials,
              "  | New Population Cost - ", new_Cost ,"\n")
          cost_involved[length(cost_involved)+1] = c(new_Cost)  #Add the improved cost values to a vector
          trails_new[length(trails_new)+1] = c(trials)
          Init_Population = new_Population
          }
        }
      }
    }
plot(trails_new,cost_involved,xlab="Number of Trials", ylab="Total time", xlim=c(0,5000),
       ylim=c(2500,6000),type='o',col="red",main="Cost over time")
  return(Init_Population)
}






# ------------------------------- Main Function ;) -------------------------------------------
#     Here I am initiating the creation of initial random population on all the clusters

InitialPopulation_DEP35024 <- Create_InitialPopulation(DEP35024, "DEP35024")
InitialPopulation_DEP35024

InitialPopulation_DEP10001 <- Create_InitialPopulation(DEP10001, "DEP10001")
InitialPopulation_DEP10001

InitialPopulation_DEP10022 <- Create_InitialPopulation(DEP10022, "DEP10022")
InitialPopulation_DEP10022

InitialPopulation_DEP25005 <- Create_InitialPopulation(DEP25005, "DEP25005")
InitialPopulation_DEP25005

#-----------------------Understanding the Parameters_db-------------------------
# Here we have in each entry how would the cost vary with the penalties we include.
# So I am manually entering the constraints 
# Min_Stops, Max_Stops, Window

Window_in_1 <- 30
Window_in_2 <- 60


Min_Stops_1 <- 3
Max_Stops_1 <- 5


Min_Stops_2 <- 4
Max_Stops_2 <- 6

Min_Stops_3 <- 5
Max_Stops_3 <- 7

#---------------Cost Values of Random Initial Population generated------------------------------
# This Section has Min_Stops = 3 and Max_Stops = 5, Window_limit = 30 min
cost(InitialPopulation_DEP35024, Min_Stops_1, Max_Stops_1, Window_in_1)
cost(InitialPopulation_DEP10022, Min_Stops_1, Max_Stops_1, Window_in_1)
cost(InitialPopulation_DEP10001, Min_Stops_1, Max_Stops_1, Window_in_1)
cost(InitialPopulation_DEP25005, Min_Stops_1, Max_Stops_1, Window_in_1)

#-------------Running Genetic Algorithms-------------------

New_Population_35024<-Genetic_algorithm(InitialPopulation_DEP35024,5000, Min_Stops_1, Max_Stops_1, Window_in_2,  0.6,0.4)
New_Population_10001<-Genetic_algorithm(InitialPopulation_DEP10001,3000, Min_Stops_3, Max_Stops_3, Window_in_2, 0.6,0.4)
New_Population_10022<-Genetic_algorithm(InitialPopulation_DEP10022,3000, Min_Stops_3, Max_Stops_3, Window_in_2, 0.6,0.4)
New_Population_25005<-Genetic_algorithm(InitialPopulation_DEP25005,3000, Min_Stops_3, Max_Stops_3, Window_in_2, 0.6,0.4)




dbDisconnect(Connection)

View(Parameters_Info)


