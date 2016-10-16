---
title: "OPR 624 Project Proposal"
author: "Dinesh Hemnani, Helen Hoffman"
output: word_document
always_allow_html: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Indego Bike Share Transport Optimization 

### Introduction

Indego Bike Share program is a new public transportation program deviced by the City of Philadelphia managed by the Office of Transportation.
The program has over a hundred stations with hundreds of bikes available for thousands of paetrons across the city of Philadelphia.  

* The program works like this.

* Sign Up for the program.

* Remove a bike from the station that's the most convenient for you.

* Ride to your destination.

* Return the bike to the closest location.

A few of the stations are shown below.  

```{r, echo=FALSE, warning=FALSE, message=FALSE}

x <- data.frame(x = c(-75.16757,-75.22399,-75.21323,-75.20799,-75.18699,-75.17939,-75.17350,-75.13979,-75.16735,-75.19311),
                y = c(39.97263,39.97841,39.95866,39.96674,39.97522,39.97274,39.98062,39.95295,39.94391,39.94681),
                n = c(1,2,3,4,5,6,7,8,9,10))

library(leaflet)
library(sp)
m <- leaflet(x) %>% addTiles() %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n))
m

```

### Problem Statement

All the stations have a fixed capacity of bikes it can give out and bikes it can take back. This makes it difficult for the paetrons to remove bikes and return bikes.  

As the program has a membership fee, patrons would not be happy to be riding far away from their destination to return the bike, nor would the be happy to see an empty station.  

This makes it necessary for the office of transportation to maintain an optimum number of bikes at each station, specially before the rush hour when the program expects maximum number of paetrons to be using the program.  

### Possible Solutions

#### Touching each station through the city - Traveling salesman problem

The traveling salesman problem is very famous for finding optimal way of visiting maximum number of places in the shortest possible distance. The solution to the traveling salesman problem can be applied to the stations, but will take a lot of time to find the solution using brute force. Number of possible permutations are (n-1)!/2.  

In the map above there are 10 stations, the number of possible solutions are 362,880 possible solutions. As the number of stations increase, the time required to calculate increases.  

The number of possible solutions of a hundred stations is 9.332622e+155.  

A few possible solutions are  

__OPTION 1__
```{r, echo=FALSE, message=FALSE, warning=FALSE}
a <- Line(as.matrix(x[,1:2]))
m2 <- leaflet(a) %>% addTiles() %>% addPolylines() %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n))
m2
```

  
__OPTION 2__
```{r, echo=FALSE, message=FALSE, warning=FALSE}
x1 <- x[order(x = x$x, decreasing = T),]

a <- Line(as.matrix(x1[,1:2]))
m2 <- leaflet(a) %>% addTiles() %>% addPolylines() %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n))
m2
```
  
  
__Option 3__
```{r,echo=FALSE, message=FALSE, warning=FALSE}
x2 <- x[order(x = x$y, decreasing = T),]

a <- Line(as.matrix(x2[,1:2]))
m2 <- leaflet(a) %>% addTiles() %>% addPolylines() %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n))
m2
```
  
  
#### Nearest Neighbour Algorithm, a greedy heuristics approach

This algorithm uses the nearest neighbour approach to find the shortest path. According to the algorithm, the salesman will find the closest next station to his current station to find the shortest path. This does reduce the number of possible solutions as at each station, the number of possible options reduce and the number of possible options keep reducing as keep reaching my next city.  


Using the algorithm the solution is

```{r, echo=FALSE, message=FALSE, warning=FALSE}

library(RANN)
nn <- nn2(x)

for (i in 1:nrow(x)){
  for(j in 2:ncol(nn$nn.idx)-1){
  if (nn$nn.idx[i,j] %in% nn$nn.idx[1:i-1,j]) {
    nn$nn.idx[i,j] <- nn$nn.idx[i,j+1]    
  }
  }
}
 
x3 <- x[c(nn$nn.idx[,2],1),]
a <- Line(as.matrix(x3[,1:2]))
m3 <- leaflet(a) %>% addTiles() %>% addPolylines() %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n))
m3
```
  
  
This solution is not best, but is very quick to calculate, and feasible, making it an attractive approach. But the optimal solution is far from this.

#### Proposed Solution

The solution that we are proposing is a combination of nearest neighbourhood at different layers. The algorithm works in the following way.

* Find the closest set of stations that need to be balanced, making a group of them.

* Find groups of closest groups that need to be balanced.

* Keep increasing the hierarchy till all stations are completed.

Below is an illustration of how the algorithm works.

##### Step 1 - Find the closest set of stations that need to be balanced

I am assuming, for simplicity that all stations need balancing, for this proposal.

On grouping the stations together, we get
```{r, echo=FALSE, warning=FALSE, message=FALSE}
m4 <- leaflet(x) %>% addTiles() %>% addRectangles(lng1 = x[2,1], lat1 = x[2,2], lng2 = x[4,1], lat2 = x[3,2]) %>% addRectangles(lng1 = x[5,1], lat1 = x[7,2], lng2 = x[1,1], lat2 = x[1,2]) %>% addRectangles(lng1 = x[10,1], lat1 = x[9,2], lng2 = x[8,1], lat2 = x[8,2] ) %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n))
m4
```

##### Step 2 - Balance each station of a group 
```{r, echo=FALSE, warning=FALSE, message=FALSE}
a <- Line(x[2:4,1:2])
b <- Line(x[c(1,5,6,7),1:2])
c <- Line(x[8:10,1:2])
m5 <- leaflet(x) %>% addTiles() %>% addRectangles(lng1 = x[2,1], lat1 = x[2,2], lng2 = x[4,1], lat2 = x[3,2]) %>% addRectangles(lng1 = x[5,1], lat1 = x[7,2], lng2 = x[1,1], lat2 = x[1,2]) %>% addRectangles(lng1 = x[10,1], lat1 = x[9,2], lng2 = x[8,1], lat2 = x[8,2] ) %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n)) %>% addPolylines(lng = a@coords[,1], lat = a@coords[,2]) %>% addPolylines(lng = b@coords[,1], lat = b@coords[,2]) %>% addPolylines(lng = c@coords[,1], lat = c@coords[,2])
m5
```

##### Step 3 - Balance the groups 
```{r, echo=FALSE, warning=FALSE, message=FALSE}
a <- Line(x[2:4,1:2])
b <- Line(x[c(5,6,7,1),1:2])
c <- Line(x[8:10,1:2])
d <- Line(x[c(4,5),1:2])
e <- Line(x[c(1,8),1:2])
m6 <- leaflet(x) %>% addTiles() %>% addRectangles(lng1 = x[2,1], lat1 = x[2,2], lng2 = x[4,1], lat2 = x[3,2]) %>% addRectangles(lng1 = x[5,1], lat1 = x[7,2], lng2 = x[1,1], lat2 = x[1,2]) %>% addRectangles(lng1 = x[10,1], lat1 = x[9,2], lng2 = x[8,1], lat2 = x[8,2] ) %>% addMarkers(lng = x$x, lat = x$y, popup = as.character(x$n)) %>% addPolylines(lng = a@coords[,1], lat = a@coords[,2]) %>% addPolylines(lng = b@coords[,1], lat = b@coords[,2]) %>% addPolylines(lng = c@coords[,1], lat = c@coords[,2]) %>% addPolylines(lng = d@coords[,1], lat = d@coords[,2], color = "red") %>% addPolylines(lng = e@coords[,1], lat = e@coords[,2], color = "red")
m6
```

#### Some points about the solution

Although this might not be the optimal solution, it can be calculated in a few iterations and can be implemented immediately. The solution uses a combination of linear and heuristic approaches and depends on the data provided to the model.

As some stations need not require balancing, these can be excluded in the decision making process, reducing the complexity of the model.

As the city of Philadelphia provides an API to collect data about the usage, we can have the model implemented in real time to check at when should we balance.


