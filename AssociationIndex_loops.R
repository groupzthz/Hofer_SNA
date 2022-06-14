# Main data structure
library("data.table")
# Time manipulation
library("lubridate")
# Data manipulation and SNA
library("plyr")
library("tidyverse")
library("asnipe")
library("igraph")
library("sna")
library("here")


list.files()


plots_path_out = 'C:\\Users\\matthew\\Desktop\\Hofer_SNA\\plots\\'
data_path_out = 'C:\\Users\\matthew\\Desktop\\Hofer_SNA\\processed data\\'

# Loading the data for Pen D
dat <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~read.csv2(., na.strings=c("", "NA")))


dat <- dat %>% 
  mutate(time2 = as_date(TimeStamp))

dat.1 <-  dat %>% 
  filter(time2  <= (min(time2)+5))

dat.2 <-  dat %>% 
  filter(time2  > (min(time2)+5))

write.csv(dat.1,paste(data_path_out, 'rawPenD.1.csv'))
write.csv(dat.2,paste(data_path_out, 'rawPenD.2.csv'))


dat.pen.e <- 
  list.files(pattern = "*\\.csv") %>% 
  map_df(~read.csv2(., na.strings = c("", "NA")))
         
dat.pen.e <- dat.pen.e %>%
  mutate(time1 = as_date(TimeStamp))


dat.e.1 <- dat.pen.e %>% 
  filter(time1 <=(min(time1) + 5))

dat.e.2 <- dat.pen.e %>% 
  filter(time1 > (min(time1) + 5))

write.csv(dat.e.1,paste(data_path_out, 'rawPenE.1.csv'))
write.csv(dat.e.2,paste(data_path_out, 'rawPenE.2.csv'))

dat.e.1 <- read.csv("rawPenE.1.csv")
dat.e.2 <- read.csv("rawPenE.2.csv")

dat.e.1 <- dat.e.1[, -1]
dat.e.2 <- dat.e.2[, -1]

dat.pen.f <- 
  list.files(pattern = "*\\.csv") %>% 
  map_df(~read.csv2(., na.strings = c("", "NA")))

dat.pen.f <- dat.pen.f %>%
  mutate(time1 = as_date(TimeStamp))


dat.f.1 <- dat.pen.f %>% 
  filter(time1 <=(min(time1) + 5))

dat.f.2 <- dat.pen.f %>% 
  filter(time1 > (min(time1) + 5))

write.csv(dat.f.1,paste(data_path_out, 'rawPenF.1.csv'))
write.csv(dat.f.2,paste(data_path_out, 'rawPenF.2.csv'))

###need to take off the first column if uploading from saved csv file###
dat.1 <- read.csv("processed data/rawPenD.1.csv")
dat.2 <- read.csv("processed data/rawPenD.2.csv")

dat.1 <- dat.1[,-1]
dat.2 <- dat.2[,-1]

summary(dat.1)

dat.e.1 <- read.csv("processed data/rawPenE.1.csv")
dat.e.2 <- read.csv("processed data/rawPenE.2.csv")

dat.e.1 <- dat.e.1[, -1]
dat.e.2 <- dat.e.2[, -1]


dat.f.1 <- read.csv("processed data/rawPenF.1.csv")
dat.f.2 <- read.csv("processed data/rawPenF.2.csv")


dat.f.1 <- dat.f.1[, -1]
dat.f.2 <- dat.f.2[, -1]

####doing multiple time periods within a pen#####
d.dat.d <- list(dat.1, dat.2)
d.dat.e <- list(dat.e.1, dat.e.2)
d.dat.f <- list(dat.f.1, dat.f.2)

m.list.d = lapply(d.dat.d, function(x){
  n1 = pivot_longer(x, cols = c(2:16), names_to = "bird.id", values_to = "ant.id", values_drop_na = TRUE)
  n1 <- n1 %>% 
    group_by(TimeStamp) %>% 
    mutate(id = row_number())
  n2 = pivot_wider(n1, names_from = "id", values_from = "bird.id")
  bird.id = unique(gather(n2[,4:ncol(n2)])$value)
  bird.id = bird.id[is.na(bird.id) == F]
  m1 = apply(n2[,4:ncol(n2)], 1, function(x) match(bird.id, x))
  m1[is.na(m1)] = 0
  m1[m1>0] = 1
  rownames(m1) = bird.id
  colnames(m1) = paste('group', 1:ncol(m1), sep = "_")
  m1
  
})
  
m.list.e = lapply(d.dat.e, function(x){
  n1 = pivot_longer(x, cols = c(2:16), names_to = "bird.id", values_to = "ant.id", values_drop_na = TRUE)
  n1 <- n1 %>% 
    group_by(TimeStamp) %>% 
    mutate(id = row_number())
  n2 = pivot_wider(n1, names_from = "id", values_from = "bird.id")
  bird.id = unique(gather(n2[,4:ncol(n2)])$value)
  bird.id = bird.id[is.na(bird.id) == F]
  m1 = apply(n2[,4:ncol(n2)], 1, function(x) match(bird.id, x))
  m1[is.na(m1)] = 0
  m1[m1>0] = 1
  rownames(m1) = bird.id
  colnames(m1) = paste('group', 1:ncol(m1), sep = "_")
  m1
  
})


m.list.f = lapply(d.dat.f, function(x){
  n1 = pivot_longer(x, cols = c(2:16), names_to = "bird.id", values_to = "ant.id", values_drop_na = TRUE)
  n1 <- n1 %>% 
    group_by(TimeStamp) %>% 
    mutate(id = row_number())
  n2 = pivot_wider(n1, names_from = "id", values_from = "bird.id")
  bird.id = unique(gather(n2[,4:ncol(n2)])$value)
  bird.id = bird.id[is.na(bird.id) == F]
  m1 = apply(n2[,4:ncol(n2)], 1, function(x) match(bird.id, x))
  m1[is.na(m1)] = 0
  m1[m1>0] = 1
  rownames(m1) = bird.id
  colnames(m1) = paste('group', 1:ncol(m1), sep = "_")
  m1
  
})

###get network with asnipe package using simple association###

adjs.d = lapply(m.list.d, function(x) get_network(t(x), data_format = "GBI", association_index = "SRI"))

adjs.e = lapply(m.list.e, function(x) get_network(t(x), data_format = "GBI", association_index = "SRI"))

adjs.f = lapply(m.list.f, function(x) get_network(t(x), data_format = "GBI", association_index = "SRI"))

adjs

###making graphs between different time periods###

gs.d = lapply(adjs.d, function(x) graph_from_adjacency_matrix(x, "undirected", weighted = T))

gs.e = lapply(adjs.e, function(x) graph_from_adjacency_matrix(x, "undirected", weighted = T))

gs.f = lapply(adjs.f, function(x) graph_from_adjacency_matrix(x, "undirected", weighted = T))

time.period = c("1-5 days", "20-25 days")
default = par()
par(mfrow = c(1,2))

for(i in 1:2){
  plot(gs.d[[i]], edge.width = E(gs.d[[i]])$weight*200, vertex.label = "", vertex.color = "gold1", vertex.size = 10, edge.color = "gray10", main = paste(time.period[i]))
}

for(i in 1:2){
  plot(gs.f[[i]], edge.width = E(gs.f[[i]])$weight*200, vertex.label = "", vertex.color = "gold1", vertex.size = 10, edge.color = "gray10", main = paste(time.period[i]))
}

###making clusters and looking at modularity using cluster_fast_greedy- could use something else, but apparently works well with small samples###

coms = lapply(gs.d, function(x) cluster_fast_greedy(x))
coms = lapply(gs.e, function(x) cluster_fast_greedy(x))
coms = lapply(gs.f, function(x) cluster_fast_greedy(x))

mods = sapply(coms, modularity)

###changing color because node order changes- need to look into this more###
###also do we want the vertex label (HenID) in graph? would need to change cex###

com.colors = list(c("blue", "yellow", "green", "red"), c( "green",  "blue", "red", "yellow"))

png(filename = paste(plots_path_out, 'penD.modularity.png'), width = 1024, height = 768, pointsize = 12)

set.seed(2)
par(mfrow = c(1,2))


for(i in 1:2){
  l = layout_with_fr(gs.d[[i]])
  V(gs.d[[i]])$color = com.colors[[i]][membership(coms[[i]])]
  plot(gs.d[[i]], layout = l, edge.width = E(gs.d[[i]])$weight*200,  vertex.label = "", vertex.size = 10, edge.color = "gray10")
  title(paste(time.period[i], ":Modularity =", round(mods[[i]], 2)),  cex.main = 2.25)
}

dev.off()

png(filename = paste(plots_path_out, 'penE.modularity.png'), width = 1024, height = 768, pointsize = 12)

set.seed(2)
par(mfrow = c(1,2))


for(i in 1:2){
  l = layout_with_fr(gs.e[[i]])
  V(gs.e[[i]])$color = com.colors[[i]][membership(coms[[i]])]
  plot(gs.e[[i]], layout = l, edge.width = E(gs.e[[i]])$weight*200,  vertex.label = "", vertex.size = 10, edge.color = "gray10")
  title(paste(time.period[i], ":Modularity =", round(mods[[i]], 2)),  cex.main = 2.25)
}

dev.off()

png(filename = paste(plots_path_out, 'penF.modularity.png'), width = 1024, height = 768, pointsize = 12)

set.seed(2)
par(mfrow = c(1,2))


for(i in 1:2){
  l = layout_with_fr(gs.f[[i]])
  V(gs.f[[i]])$color = com.colors[[i]][membership(coms[[i]])]
  plot(gs.f[[i]], layout = l, edge.width = E(gs.f[[i]])$weight*200,  vertex.label = "", vertex.size = 10, edge.color = "gray10")
  title(paste(time.period[i], ":Modularity =", round(mods[[i]], 2)),  cex.main = 2.25)
}

dev.off()

####testing modularity against null model overall with 1000 swaps###
gbi=t(m.list.d[[1]])
gbi=t(m.list.e[[1]])
gbi=t(m.list.f[[1]]) 

swap.m=list() 

times=1000

for (k in 1:times){
  swap.m[[k]]=network_swap(gbi, swaps=1000)$Association_index }

swap.f=lapply(swap.m, function(x) graph_from_adjacency_matrix(x, "undirected", weighted=T)) 
mod.swap=sapply(swap.f, function(x) modularity(cluster_fast_greedy(x)))

hist(mod.swap, xlim=c(min(mod.swap), mods[[1]]))
abline(v=mods[[1]], col="red", lty=2, lwd=2) 
p=(length(which(mod.swap>=mods[[1]]))+1)/(times+1)
p


###try serial method###

gbi2 = t(m.list.d[[1]])
gbi2 = t(m.list.e[[1]])
gbi2 = t(m.list.f[[1]])
assoc2=get_network(gbi2)

net.perm=network_permutation(gbi2, permutations=10000, returns=1, association_matrix = assoc2)

swap.g2=apply(net.perm, 1, function(x) graph_from_adjacency_matrix(x,"undirected", weighted=T))
mod.swap2=sapply(swap.g2, function(x) modularity(cluster_fast_greedy(x))) 
hist(mod.swap2,xlim=c(min(mod.swap2), mods[[1]]), main="Serial Method") 
abline(v=mods[[1]], col="red", lty=2, lwd=2) 
p=(length(which(mod.swap2>=mods[[1]]))+1)/100001
p

###putting variables together for results

pen <-  c("D", "E", "F")
global_p <- c("0.059", "0.0079", "0.002")
serial_p <- c("<0.001", "<0.001", "<0.001")

pen.global.serial <- cbind(pen, global_p, serial_p)

pen.global.serial <- as.data.frame(pen.global.serial)

###check across time points with mantel test - are matrices the same or different###
###using ecodist, but maybe vegan###

library(ecodist)

#restrict comparison to individuals that were seen in two time periods - don't really need to do this I think as we only have the same 15###
###good practice though###

id12=rownames(adjs[[1]])[rownames(adjs[[1]])%in%rownames(adjs[[2]])] #get IDs of birds that were present in both networks
ids.m1=match(id12,rownames(adjs[[1]])) #get row/columns of those individuals in matrix 1 
ids.m2=match(id12,rownames(adjs[[2]])) #get row/colums of those individuals in matrix 2
m12=adjs[[1]][ids.m1,ids.m1] #matrix 1 of association indices of only returning individuals
m21=adjs[[2]][ids.m2,ids.m2] #matrix 2 of association indices of only returning individuals
m12=m12[order(rownames(m12)),order(rownames(m12))] #reorder the rows/columns by alphanumeric order
m21=m21[order(rownames(m21)),order(rownames(m21))] #reorder the rows/columns by alphanumeric order
mantel12=mantel(as.dist(m12)~as.dist(m21)) 
mantel12


#restrict comparison to individuals that were seen in two time periods 
id12=rownames(adjs.f[[1]])[rownames(adjs.f[[1]])%in%rownames(adjs.f[[2]])] #get IDs of birds that were present in both networks
ids.m1=match(id12,rownames(adjs.f[[1]])) #get row/columns of those individuals in matrix 1 
ids.m2=match(id12,rownames(adjs.f[[2]])) #get row/colums of those individuals in matrix 2
m12=adjs.f[[1]][ids.m1,ids.m1] #matrix 1 of association indices of only returning individuals
m21=adjs.f[[2]][ids.m2,ids.m2] #matrix 2 of association indices of only returning individuals
m12=m12[order(rownames(m12)),order(rownames(m12))] #reorder the rows/columns by alphanumeric order
m21=m21[order(rownames(m21)),order(rownames(m21))] #reorder the rows/columns by alphanumeric order
mantel12=mantel(as.dist(m12)~as.dist(m21)) 
mantel12


#restrict comparison to individuals that were seen in two time periods
id12=rownames(adjs.f[[1]])[rownames(adjs.f[[1]])%in%rownames(adjs.f[[2]])] #get IDs of birds that were present in both networks
ids.m1=match(id12,rownames(adjs.f[[1]])) #get row/columns of those individuals in matrix 1 
ids.m2=match(id12,rownames(adjs.f[[2]])) #get row/colums of those individuals in matrix 2
m12=adjs.f[[1]][ids.m1,ids.m1] #matrix 1 of association indices of only returning individuals
m21=adjs.f[[2]][ids.m2,ids.m2] #matrix 2 of association indices of only returning individuals
m12=m12[order(rownames(m12)),order(rownames(m12))] #reorder the rows/columns by alphanumeric order
m21=m21[order(rownames(m21)),order(rownames(m21))] #reorder the rows/columns by alphanumeric order
mantel12=mantel(as.dist(m12)~as.dist(m21)) 
mantel12


###Potenial way to get pearson correlation
install.packages("vcd")
library(vcd)


df1 <- read.csv2("exampleData.csv", na.strings=c("", "NA"))

df1.1 <- df1[,2:16]

empty_m <- matrix(ncol = length(df1.1),
                  nrow = length(df1.1),
                  dimnames = list(names(df1.1),
                                  names(df1.1)))


calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m ,df1.1)



####this works use this if can't figure out other way
dat.1.test <- dat.1.test[,2:16]


datd.1 <- dat.1[,2:16]
datd.2 <- dat.2[,2:16]

date.1 <- dat.e.1[,2:16]
date.2 <- dat.e.2[,2:16]

datf.1 <- dat.f.1[,2:16]
datf.2 <- dat.f.2[,2:16]
newdata=as.data.frame(matrix(ncol=ncol(df1.1), nrow=ncol(df1.1)))

for (c in 1:ncol(df1.1)){
  print(c)

  d <- df1.1[,c]
  
  for(k in 1:ncol(df1.1)){
  print(k)
  e <- df1.1[,k]
  
  g <- sum(ifelse(d==e, 1,0), na.rm=T)
  h <- length(d[!is.na(d)])
  i <- length(e[!is.na(e)])
  
  ai <- g/(h + i - g)
 abs(ai)
 
 newdata[c,k] =  abs(ai)
 }
}


n <- c(names(df1.1))

names(newdata) <- n
rownames(newdata) <- n

get.ai <- function(y) {
  col.y <- ncol(y)
  V <- as.data.frame(matrix(ncol = col.y, nrow = col.y))
  for(i in 1:col.y){
    print(i)
    for(j in 1:col.y){
      print(j)
      g <- sum(ifelse(y[i] == y[j], 1,0), na.rm=T)
      h <- length(y[i][!is.na(y[i])])
      p <- length(y[j][!is.na(y[j])])
      ai <- g/(h + p - g)
      
      V[i,j] <- abs(ai)
      
      n <- c(names(y))
      names(V) <- n
      rownames(V) <- n
    }
  }
  return(V)
}

ai.d1.test <- get.ai(dat.1.test)

datpenD.1 <- get.ai(datd.1)
datpenD.2 <- get.ai(datd.2)

write.csv(datpenD.1,paste(path_out, 'aiPenD.1.csv'))
write.csv(datpenD.2,paste(path_out, 'aiPenD.2.csv'))


datpenE.1 <- get.ai(date.1)
datpenE.2 <- get.ai(date.2)

write.csv(datpenE.1, paste(path_out, 'aiPenE.1.csv'))
write.csv(datpenE.2, paste(path_out, 'aiPenE.2.csv'))


datpenF.1 <- get.ai(datf.1)
datpenF.2 <- get.ai(datf.2)

write.csv(datpenF.1, paste(path_out, 'aiPenF.1.csv'))
write.csv(datpenF.2, paste(path_out, 'aiPenF.2.csv'))
as.data.frame(try)
df1.2 <- df1.1 %>% 
  select(1:2)


aiPenD.1 <- read.csv2(" aiPenD.1.csv", sep = ",")
aiPenD.2 <- read.csv2(" aiPenD.2.csv", sep = ",")

list.files()
aiPenD.1 <- as.matrix(aiPenD.1)
x <- aiPenD.1[,1]

rownames(aiPenD.1) <- x
aiPenD.1 <- aiPenD.1[, -1]

aiPenD.2 <- as.matrix(aiPenD.2)

x.2 <- aiPenD.2[,1]
rownames(aiPenD.2) <- x.2
aiPenD.2 <- aiPenD.2[, -1]

aiPenD.1 <- aiPenD.1[order(rownames(aiPenD.1)), order(rownames(aiPenD.1))]
aiPenD.2 <- aiPenD.2[order(rownames(aiPenD.2)), order(rownames(aiPenD.2))]
mand <- mantel(as.dist(aiPenD.1) ~ as.dist(aiPenD.2))
mand
####try to get network using asnipe###

install.packages("asnipe")
install.packages("sna")
install.packages("igraph")
install.packages("randomcoloR")

library("sna")
library("asnipe")
library("igraph")
library("randomcoloR")

aiPenD.1 <- aiPenD.1[,2:16]
netd.1 <- get_network(aiPend.1)

diag(ai.d1.test) <- 0
assoc.g <- graph_from_adjacency_matrix(as.matrix(ai.d1.test), "undirected", weighted = T)
assoc.g


plot(assoc.g, edge.width = E(assoc.g)$weight*200)

assoc.g <- graph_from_adjacency_matrix(as.matrix(aiPenD.1), "undirected", weighted = T)
assoc.g

plot(assoc.g, edge.width = E(assoc.g)$weight*100)

diag(aiPenD.1) <- 0

com = fastgreedy.community(assoc.g)
node.colors = membership(com)
plot(assoc.g, edge.width=E(assoc.g)$weight*10, vertex.label = "", vertex.size = 5, vertex.color = node.colors)


net_graph <- graph.adjacency(as.matrix(aiPenD.1), mode = "directed", weighted = T, diag = F)

deg <- graph.strength(net_graph)

deg_binary <- degree(aiPenD.1)

x <- names(aiPenD.1)
de.1 <- data.frame(cbind(x,deg, deg_binary))


random_networks <- network_permutation()

community <- leading.eigenvector.community(net_graph)

communities <- list()

for(i in 1:max(community$membership)) {
  communities[[i]] <- which(community$membership)
}

n <- 15
palette <- distinctColorPalette(n)


plot(net_graph, vertex.color = palette, vertex.size = 3, vertex.label = NA, mark.groups = communities)


###new method###

library(tidyverse)
install.packages("lsr")
library(lsr)
?table

f = function(x,y) {
  b = df1.1 %>% select(x,y) %>% table(useNA = "ifany") 
  b = as.data.frame(b)
  g = sum(ifelse(b$x == b$y, 1, 0), na.rm = T)
  data.frame(x, y, g)}
  h = length(b.t$x[!is.na(b.t$x)])
  i = length(b.t$y[!is.na(b.t$y)])
  ai = g/(h +i - g)
  data.frame(x, y, g, h, i, ai)
}

df_comb = data.frame(t(combn(sort(names(df1.1)), 2)), stringsAsFactors = F)

df_res = map2_df(df_comb$X1, df_comb$X2, f)



col.y <- ncol(df1.2)
V <- matrix(ncol = col.y, nrow = col.y)
for(i in 1:ncol(df1.2)){
  for(j in 1:ncol(df1.2)){
    g <- sum(ifelse(i == j, 1, 0), na.rm = T)
    h <- length(i[!is.na(i)])
    p <- length(j[!is.na(j)])
    ai <- g/(h + p -g)
    V[i,j] <- ai
  }
}
try <- get.ai(df1.1)


