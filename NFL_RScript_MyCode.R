# NFL Big Data Bowl

# 1 Set WD and Import Packages
setwd("~/Spring Semester Syracuse 2019/Big-Data-Bowl-master/Data")
library(tidyverse)

# 2 Read in Files
temp = list.files(pattern="tracking_gameId_201709")
temp = list.files(pattern="tracking_gameId_201710")
myfiles = lapply(temp, read_csv)
tracking.example <- do.call('rbind', myfiles)
games.sum <- read_csv('games.csv') 
plays.sum <- read_csv('plays.csv')
players.sum <- read_csv('players.csv')

# 3 Filter and Join Files - WR Routes on Passing Plays
tracking.example <- tracking.example %>% inner_join(games.sum) 
tracking.example <- tracking.example %>% inner_join(plays.sum) 
tracking.example <- tracking.example %>% filter(!is.na(PassResult)) 
tracking.example <- tracking.example %>% inner_join(players.sum)
CsPass <- tracking.example %>% filter(PositionAbbr == 'C')
WRsPass <- tracking.example %>% filter(PositionAbbr == 'WR' | PositionAbbr == 'TE' | PositionAbbr == 'RB')

# 4 Determine Snap Direction
C_y = CsPass[,c(3,6,7,13,14)]
C_y = C_y %>% filter(event == 'ball_snap')
C_y$event = NULL
WRsPass = merge(WRsPass, C_y, by = c('gameId','playId'))
WRsPass = WRsPass[,c(3:14,1:2,15:70)]
names(WRsPass)[3] = 'y'
names(WRsPass)[69] = 'C_y'
names(WRsPass)[6] = 'dir'
names(WRsPass)[70] = 'C_dir'

WRsPass = WRsPass[order(WRsPass$frame.id),]
WRsPass = WRsPass[order(WRsPass$gameId),]
WRsPass = WRsPass[order(WRsPass$displayName),]
WRsPass = WRsPass[order(WRsPass$playId),]

# 5 Determine Side of the Ball each receiver was

WRsPass$ball_side = ifelse(WRsPass$event == 'ball_snap' & 
                             WRsPass$C_dir >= 0 & WRsPass$C_dir <= 180 &
                             WRsPass$C_y > WRsPass$y, 'left', NA)
WRsPass$ball_side = ifelse(WRsPass$event == 'ball_snap' & 
                             WRsPass$C_dir >= 0 & WRsPass$C_dir <= 180 &
                             WRsPass$C_y < WRsPass$y, 'right', WRsPass$ball_side)
WRsPass$ball_side = ifelse(WRsPass$event == 'ball_snap' & 
                             WRsPass$C_dir > 180 & WRsPass$C_dir <= 360 &
                             WRsPass$C_y < WRsPass$y, 'left', WRsPass$ball_side)
WRsPass$ball_side = ifelse(WRsPass$event == 'ball_snap' & 
                             WRsPass$C_dir > 180 & WRsPass$C_dir <= 360 &
                             WRsPass$C_y > WRsPass$y, 'right', WRsPass$ball_side)

WRsPass = WRsPass %>% fill(ball_side)

# 7 Split Data By Route (Start and End Point)

library(data.table)

data = split(WRsPass, list(WRsPass$playId, WRsPass$displayName), drop = TRUE)

data = data[sapply(data, function(x) all('ball_snap' %in% x$event))]

for (i in 1:length(data)){
  data[[i]] = data[[i]][(which(data[[i]][[7]]=='ball_snap')):
                        (which(data[[i]][[7]]=='pass_outcome_incomplete' |
                         data[[i]][[7]]=='pass_outcome_caught' |
                         data[[i]][[7]]=='pass_outcome_touchdown' |
                         data[[i]][[7]]=='pass_outcome_interception' |
                         data[[i]][[7]]=='qb_sack' |
                         data[[i]][[7]]=='qb_strip_sack' |
                         data[[i]][[7]]=='run' |
                         data[[i]][[7]]=='first_contact' |
                         data[[i]][[7]]=='pass_arrived' |
                         data[[i]][[7]]=='out_of_bounds' |
                         data[[i]][[7]]=='tackle' |
                         data[[i]][[7]]=='qb_spike')),]
}

for (i in 1:length(data)){
  data[[i]][[2]] = abs(data[[i]][[2]] - data[[i]][[2]][[1]])
  data[[i]][[3]] = data[[i]][[3]] - data[[i]][[3]][[1]]
}

##################################################################################################
# 8 Merge Dataframes together
for(i in 1:length(data)){
  data[[i]]$route_num = i
}

for(i in 1:length(data)){
  data[[i]] = data[[i]][!duplicated(data[[i]]$frame.id),]
}

merged = do.call("rbind", data)
merged$row_num = 1:nrow(merged)

# 9 Plot All Routes
library(ggplot2)
ggplot(graph, aes(x = y, y = x)) +
  geom_point() +
  ggtitle("Example ") +
  xlab('X') +
  ylab('Y') + 
  guides(color=FALSE)

# 10 Choose Break Point
library(segmented)

# 10.1 - Route Num
ysi = {}
count = 0

for (i in 1:length(data)){
  for (j in 1:nrow(data[[i]])){
    count = count + 1
    ysi[count] = i
  }
}

# 10.2 - Frame Num
ysj = {}
count = 0

for (i in 1:length(data)){
  for (j in 1:nrow(data[[i]])){
    count = count + 1
    ysj[count] = (data[[i]]$frame.id[j] - data[[i]]$frame.id[1]) + 1
  }
}

# 10.3 - y-coordinate
ysy = {}
count = 0

for (i in 1:length(data)){
  for (j in 1:nrow(data[[i]])){
    count = count + 1
    ysy[count] = data[[i]]$y[j]
  }
}
  
# 10.4 - x-coordinate
ysx = {}
count = 0

for (i in 1:length(data)){
  for (j in 1:nrow(data[[i]])){
    count = count + 1
    ysx[count] = data[[i]]$x[j]
  }
}

# 10.5 Create Dataframe
ys = data.frame(i = ysi, j = ysj, y = ysy, x = ysx)
rm(ysi)
rm(ysj)
rm(ysx)
rm(ysy)

# 11 Euclidian Distance
dist2d <- function(a,b,c) {
  pa = a - b
  ba = b - c
  t = as.vector((pa %*% ba) / (ba %*% ba))
  d = (pa - t * ba)
  dist = sqrt(sum(d^2))
}

# 11 - Distance from Start Point
dist_start = {}
count = 0
#dist_start <- data.frame(i = numeric(), j = numeric(), k = numeric(), d = numeric())

for(i in 1:length(data)){
  tbl_x = data[[i]]$x
  tbl_y = data[[i]]$y
  for(j in 1:length(tbl_x)){
    for(k in 1:j){
    a1 <- c(tbl_y[k],tbl_x[k]) # every other point between b and c
    b1 <- c(tbl_y[j],tbl_x[j]) # line point 1
    c1 <- c(tbl_y[1],tbl_x[1]) # line point 2 - Start Route
    d1 <- dist2d(a1,b1,c1)
    tmp_dist = d1
    count = count + 1
    dist_start[[count]] = tmp_dist
    }
  }
}

#dist_start = sub(NaN, 0, dist_start)
#dist_start = as.numeric(dist_start)

dist_start_k = {}
count = 0

for(i in 1:length(data)){
  num = nrow(data[[i]])
  for(j in 1:num){
    for(k in 1:j){
      count = count + 1
      dist_start_k[[count]] = k
    }
  }
}

dist_start_j = {}
count = 0

for(i in 1:length(data)){
  num = nrow(data[[i]])
  for(j in 1:num){
    for(k in 1:j){
      count = count + 1
      dist_start_j[[count]] = j
    }
  }
}

dist_start_i = {}
count = 0

for(i in 1:length(data)){
  num = nrow(data[[i]])
  for(j in 1:num){
    for(k in 1:j){
      count = count + 1
      dist_start_i[[count]] = i
    }
  }
}

dist_start_df = data.frame(i = dist_start_i, j = dist_start_j, k = dist_start_k, d = dist_start)
dist_start_df$i = sub(NaN, 0, dist_start_df$i)
dist_start_df$i = as.numeric(dist_start_df$i)

rm(dist_start_i)
rm(dist_start_j)
rm(dist_start_k)
rm(dist_start)

# 12 - Distance From End Point
dist_end <- {}
count = 0

for(i in 1:length(data)){
  tbl_x = data[[i]]$x
  tbl_y = data[[i]]$y
  for(j in 1:length(tbl_x)){
    for(k in 1:length(tbl_x)){
      a1 <- c(tbl_y[k],tbl_x[k]) # every other point between b and c
      b1 <- c(tbl_y[j],tbl_x[j]) # line point 1
      c1 <- c(tbl_y[length(tbl_y)],tbl_x[length(tbl_x)]) # line point 2 - Start Route
      d1 <- dist2d(a1,b1,c1)
      tmp_dist = d1
      count = count + 1
      dist_end[[count]] = tmp_dist
    }
  }
}

#dist_end = sub(NaN, 0, dist_end)
#dist_end = as.numeric(dist_end)

dist_end_k = {}
count = 0

for(i in 1:length(data)){
  num = nrow(data[[i]])
  for(j in 1:num){
    for(k in 1:nrow(data[[i]])){
      count = count + 1
      dist_end_k[[count]] = k
    }
  }
}

dist_end_j = {}
count = 0

for(i in 1:length(data)){
  num = nrow(data[[i]])
  for(j in 1:num){
    for(k in 1:nrow(data[[i]])){
      count = count + 1
      dist_end_j[[count]] = j
    }
  }
}

dist_end_i = {}
count = 0

for(i in 1:length(data)){
  num = nrow(data[[i]])
  for(j in 1:num){
    for(k in 1:nrow(data[[i]])){
      count = count + 1
      dist_end_i[[count]] = i
    }
  }
}

dist_end_df = data.frame(i = dist_end_i, j = dist_end_j, k = dist_end_k, d = dist_end)
dist_end_df$i = sub(NaN, 0, dist_end_df$i)
dist_end_df$i = as.numeric(dist_end_df$i)

rm(dist_end_i)
rm(dist_end_j)
rm(dist_end_k)
rm(dist_end)

# 13 Complete Distances
dist_end_df = anti_join(dist_end_df, dist_start_df, by = c('i','j','k'))

# 14 Minimize Euclidian Distance
dist_start_df = aggregate(dist_start_df$d, by=list(dist_start_df$i, dist_start_df$j), FUN=sum)
dist_end_df = aggregate(dist_end_df$d, by=list(dist_end_df$i, dist_end_df$j), FUN=sum)

dist = merge(dist_start_df, dist_end_df, by = c('Group.1', 'Group.2'), all = T)
dist$x.x = sub(NaN, 0 , dist$x.x)
dist$x.y = sub(NaN, 0 , dist$x.y)
dist$x.x = as.numeric(dist$x.x)
dist$x.y = as.numeric(dist$x.y)
#dist$x.y = ifelse(is.na(dist$x.y), 0, dist$x.y)
dist$final = dist$x.x + dist$x.y
mins = aggregate(final ~ Group.1, dist, function(x) min(x))
tmp = dist[,c(1,2,5)]
mins = merge(mins, tmp, by = c('Group.1','final'))
mins = merge(mins, ys, by.x = c('Group.1', 'Group.2'), by.y = c('i','j'))
mins = mins[!duplicated(mins$Group.1), ]
mins = mins[order(mins$Group.1),]

maxs = {}

for(i in 1:length(data)){
  tmp_max = max(data[[i]][[3]])
  maxs = append(maxs, tmp_max)
}

mins_2 = {}

for(i in 1:length(data)){
  tmp_mins = min(data[[i]][[3]])
  mins_2 = append(mins_2, tmp_mins)
}

mins$maxs = maxs
mins$error = ifelse(mins$y<mins$maxs, FALSE, TRUE)
mins$y = ifelse(mins$error == TRUE, mins$y-.01, mins$y)
mins$mins = mins_2
mins$error = ifelse(mins$y>mins$mins, FALSE, TRUE)
mins$y = ifelse(mins$error == TRUE, (mins$y + .01), mins$y)

# 15 Find Branch Length
branch_length = {}

for (i in 1:length(data)){
  tmp_branch_length = sqrt((mins$y[i])^2+(mins$x[i])^2)
  branch_length = append(branch_length, tmp_branch_length)
}

# 16 Run Segmented Regression Using Minimzed Break Point
segmented.mods = {}
brokens = {}

for (i in 1:length(data)){
  lin.mod <- lm(x~y, data = data[[i]])
  try(
    segmented.mod <- segmented(lin.mod, 
                               seg.Z = ~y, 
                               psi = mins$y[i], 
                               control = seg.control(
                                 n.boot = 30, 
                                 h = 0.001))
    )
  broken = broken.line(segmented.mod)$fit
  brokens = append(brokens, broken)
  segmented.mods = append(segmented.mods, segmented.mod)
}

segmented.mods.list = split(segmented.mods, rep(1:ceiling(length(segmented.mods)/23), each=23)[1:length(segmented.mods)])

# 17 Find Branch Angle
branch_angle = {}

for (i in 1:length(data)){
  angle = ifelse(sum(data[[i]]$y)>0,
      atan(segmented.mods.list[[i]]$coefficients[2]+segmented.mods.list[[i]]$coefficients[3])*180/pi,
      ifelse(atan(segmented.mods.list[[i]]$coefficients[2]<0),
        180-(-atan(segmented.mods.list[[i]]$coefficients[2])*180/pi),
        (-atan(segmented.mods.list[[i]]$coefficients[2])*180/pi)-90))
  branch_angle = append(branch_angle, angle)
}

# 18 Find Avg Speed
avg_speed = {}

for (i in 1:length(data)){
  tmp_avg_speed = mean(data[[i]]$s)
  avg_speed = append(avg_speed, tmp_avg_speed)
}

# 19 Find End Point Coordinates
last_x = {}
last_y = {}

for (i in 1:length(data)){
  tmp_last_x = data[[i]]$x[nrow(data[[i]])]
  last_x = append(last_x, tmp_last_x)
  tmp_last_y = data[[i]]$y[nrow(data[[i]])]
  last_y = append(last_y, tmp_last_y)
}

# 20 Determine What Side the Ball Was On
ball_side = {}

for (i in 1:length(data)){
  tmp_ball_side = data[[i]]$ball_side[1]
  ball_side = append(ball_side, tmp_ball_side)
}

# 21 Create New Dataframe
model_df <- data.frame(matrix(unlist(branch_length), nrow=length(data), byrow=T))
names(model_df) = 'branch_length'
model_df$branch_angle = branch_angle
model_df$avg_speed = avg_speed
model_df$last_x = last_x
model_df$last_y = last_y
model_df$ball_side = ball_side
model_df$ball_side = ifelse(model_df$ball_side=='left', 0, 1)

# 22 Plot Example Route
dat2 = data.frame(x = brokens[merged$row_num[which(merged$route_num==323)]], y = data[[323]]$y)

library(ggplot2)
ggplot(data[[323]], aes(x = y, y = x)) +
  ggtitle("Example Route -135 Degree Branch Angle") +
  geom_point() +
  geom_line(data = dat2, color = 'blue')

# 23 Cluster Routes
model_df$route_num = 1:nrow(model_df)
data_l = model_df %>% filter(ball_side == 0)

# 24 Hierarchical Cluster
hc = hclust(d = dist(data_l[-7], method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

clusters = cbind(data_l, y_hc)
total = merge(merged, clusters, by = 'route_num')
total$y_hc = as.factor(total$y_hc)

###################################################################################################
# Show Certain Routes Graphically

total <- read.csv('ClustersForGraphing.csv')

graph <- total %>% filter(route_num == 6554) # 1 - Slant
graph <- total %>% filter(route_num == 24255) # 2 - Short Go
graph <- total %>% filter(route_num == 1212) # 3 - Out
graph <- total %>% filter(route_num == 30411) # 4 - Wheel
graph <- total %>% filter(route_num == 17049) # 5 - Long Go
graph <- total %>% filter(route_num == 13440) # 6 - Post
graph <- total %>% filter(route_num == 14169) # 7 - In
graph <- total %>% filter(route_num == 15143) # 8 - Curl
graph <- total %>% filter(route_num == 11128) # 9 - Corner
graph <- total %>% filter(route_num == 2456) # 10 - Flat
graph <- total %>% filter(route_num == 7056) # 11 - Drag
graph <- total %>% filter(route_num == 15515) # 12 - Comeback
graph$route_num <- as.factor(graph$route_num)

library(ggplot2)
ggplot(graph
       , aes(x = y, y = x, color = route_num)) +
  geom_point() +
  ggtitle("12 - Comeback Route") +
  xlab('X') +
  ylab('Y') +
  guides(color=FALSE)

