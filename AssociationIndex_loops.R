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


sessionInfo()

###checking which files I have
list.files()

#paths for plots and data after cleaning
plots_path_out = 'C:\\Users\\matthew\\Desktop\\Hofer_SNA\\plots\\'
data_path_out = 'C:\\Users\\matthew\\Desktop\\Hofer_SNA\\processed data\\'

# Loading the data for all pens - starting with Pen D

dat <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~read.csv2(., na.strings=c("", "NA")))

#change to date

dat <- dat %>% 
  mutate(time2 = as_date(TimeStamp))


#dividing into time points

dat.1 <-  dat %>% 
  filter(time2  <= (min(time2)+5))

dat.2 <-  dat %>% 
  filter(time2  > (min(time2)+5))

#save to processed data

write.csv(dat.1,paste(data_path_out, 'rawPenD.1.csv'))
write.csv(dat.2,paste(data_path_out, 'rawPenD.2.csv'))

#Pen E
dat.pen.e <- 
  list.files(pattern = "*\\.csv") %>% 
  map_df(~read.csv2(., na.strings = c("", "NA")))
         
dat.pen.e <- dat.pen.e %>%
  mutate(time1 = as_date(TimeStamp))


dat.e.1 <- dat.pen.e %>% 
  filter(time1 <=(min(time1) + 5))

dat.e.2 <- dat.pen.e %>% 
  filter(time1 > (min(time1) + 5))

#save to processed data

write.csv(dat.e.1,paste(data_path_out, 'rawPenE.1.csv'))
write.csv(dat.e.2,paste(data_path_out, 'rawPenE.2.csv'))
 
#restarted and loading data-load from processed data
dat.e.1 <- read.csv("rawPenE.1.csv")
dat.e.2 <- read.csv("rawPenE.2.csv")


#Pen F
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

names(dat.1)
#d.name will be used below when we need to get rid of extra individuals from pre-transition (we only want the 15 birds post-transition)
d.name <- names(dat.1[, -1])

dat.e.1 <- read.csv("processed data/rawPenE.1.csv")
dat.e.2 <- read.csv("processed data/rawPenE.2.csv")

dat.e.1 <- dat.e.1[, -1]
dat.e.2 <- dat.e.2[, -1]
names(dat.e.1)
e.name <- names(dat.e.1[, -1])

dat.f.1 <- read.csv("processed data/rawPenF.1.csv")
dat.f.2 <- read.csv("processed data/rawPenF.2.csv")


dat.f.1 <- dat.f.1[, -1]
dat.f.2 <- dat.f.2[, -1]
names(dat.f.1)
f.name <- names(dat.f.1[, -1])

####lumping multiple time periods within a pen and doing functions to make faster#####
d.dat.d <- list(dat.1, dat.2)
d.dat.e <- list(dat.e.1, dat.e.2)
d.dat.f <- list(dat.f.1, dat.f.2)

#takes lists and creates a individual by group matrix, also known as GBI
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
#Pen D

adjs.d = lapply(m.list.d, function(x) get_network(t(x), data_format = "GBI", association_index = "SRI"))
adjs.d[1]
#Pen E
adjs.e = lapply(m.list.e, function(x) get_network(t(x), data_format = "GBI", association_index = "SRI"))

#Pen F
adjs.f = lapply(m.list.f, function(x) get_network(t(x), data_format = "GBI", association_index = "SRI"))


###making SNA graphs between different time periods###

gs.d = lapply(adjs.d, function(x) graph_from_adjacency_matrix(x, "undirected", weighted = T))

gs.e = lapply(adjs.e, function(x) graph_from_adjacency_matrix(x, "undirected", weighted = T))

gs.f = lapply(adjs.f, function(x) graph_from_adjacency_matrix(x, "undirected", weighted = T))

#actual node graphs
time.period = c("1-5 days", "20-25 days")
default = par()
par(mfrow = c(1,2))

for(i in 1:2){
  plot(gs.d[[i]], edge.width = E(gs.d[[i]])$weight*200, vertex.label = "", vertex.color = "gold1", vertex.size = 10, edge.color = "gray10", main = paste(time.period[i]))
}

for(i in 1:2){
  plot(gs.e[[i]], edge.width = E(gs.e[[i]])$weight*200, vertex.label = "", vertex.color = "gold1", vertex.size = 10, edge.color = "gray10", main = paste(time.period[i]))
}

for(i in 1:2){
  plot(gs.f[[i]], edge.width = E(gs.f[[i]])$weight*200, vertex.label = "", vertex.color = "gold1", vertex.size = 10, edge.color = "gray10", main = paste(time.period[i]))
}

###making clusters and looking at modularity using cluster_fast_greedy- could use something else, but apparently works well with small samples###

coms.d = lapply(gs.d, function(x) cluster_fast_greedy(x))
coms.e = lapply(gs.e, function(x) cluster_fast_greedy(x))
coms.f = lapply(gs.f, function(x) cluster_fast_greedy(x))

mods.d = sapply(coms.d, modularity)
mods.e = sapply(coms.e, modularity)
mods.f = sapply(coms.f, modularity)

###changing color because node order changes- need to look into this more###
###also do we want the vertex label (HenID) in graph? would need to change cex maybe###

com.colors = list(c("blue", "yellow", "green", "red"), c( "green",  "blue", "red", "yellow"))

###save files to plots folder
png(filename = paste(plots_path_out, 'penD.modularity.png'), width = 1024, height = 768, pointsize = 12)

#set seed for reproducability
set.seed(2)
par(mfrow = c(1,2))


for(i in 1:2){
  l = layout_with_fr(gs.d[[i]])
  V(gs.d[[i]])$color = com.colors[[i]][membership(coms.d[[i]])]
  plot(gs.d[[i]], layout = l, edge.width = E(gs.d[[i]])$weight*200, vertex.size = 15, edge.color = "gray10")
  title(paste(time.period[i], ":Modularity =", round(mods.d[[i]], 2)),  cex.main = 2.25)
}

dev.off()

png(filename = paste(plots_path_out, 'penE.modularity.png'), width = 1024, height = 768, pointsize = 12)

set.seed(2)
par(mfrow = c(1,2))


for(i in 1:2){
  l = layout_with_fr(gs.e[[i]])
  V(gs.e[[i]])$color = com.colors[[i]][membership(coms.e[[i]])]
  plot(gs.e[[i]], layout = l, edge.width = E(gs.e[[i]])$weight*200,  vertex.label = "", vertex.size = 10, edge.color = "gray10")
  title(paste(time.period[i], ":Modularity =", round(mods.e[[i]], 2)),  cex.main = 2.25)
}

dev.off()

png(filename = paste(plots_path_out, 'penF.modularity.png'), width = 1024, height = 768, pointsize = 12)

set.seed(2)
par(mfrow = c(1,2))
for(i in 1:2){
  l = layout_with_fr(gs.f[[i]])
  V(gs.f[[i]])$color = com.colors[[i]][membership(coms.f[[i]])]
  plot(gs.f[[i]], layout = l, edge.width = E(gs.f[[i]])$weight*200,  vertex.label = "", vertex.size = 10, edge.color = "gray10")
  title(paste(time.period[i], ":Modularity =", round(mods.f[[i]], 2)),  cex.main = 2.25)
}

dev.off()


com=fastgreedy.community(g) #community detection method
node.colors=membership(com) #assign node color based on community membership
set.seed(2)
plot(g, edge.width=E(g)$weight*10, vertex.label="", vertex.size=5, vertex.color=node.colors)

####testing modularity against null model overall with 1000 swaps - do 1 at a time###
#start with gbi for d and then do the swaps, repeat gbi with e than f
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
#same as above- do one pen and then the next

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
edge.attributes(gs.d[[1]], index = E(gs.d[[1]]))
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

id12=rownames(adjs.d[[1]])[rownames(adjs.d[[1]])%in%rownames(adjs.d[[2]])] #get IDs of birds that were present in both networks
ids.m1=match(id12,rownames(adjs.d[[1]])) #get row/columns of those individuals in matrix 1 
ids.m2=match(id12,rownames(adjs.d[[2]])) #get row/colums of those individuals in matrix 2
m12=adjs.d[[1]][ids.m1,ids.m1] #matrix 1 of association indices of only returning individuals
m21=adjs.d[[2]][ids.m2,ids.m2] #matrix 2 of association indices of only returning individuals
m12=m12[order(rownames(m12)),order(rownames(m12))] #reorder the rows/columns by alphanumeric order
m21=m21[order(rownames(m21)),order(rownames(m21))] #reorder the rows/columns by alphanumeric order
mantel12=mantel(as.dist(m12)~as.dist(m21)) 
mantel12


#restrict comparison to individuals that were seen in two time periods 
id12=rownames(adjs.e[[1]])[rownames(adjs.e[[1]])%in%rownames(adjs.e[[2]])] #get IDs of birds that were present in both networks
ids.m1=match(id12,rownames(adjs.e[[1]])) #get row/columns of those individuals in matrix 1 
ids.m2=match(id12,rownames(adjs.e[[2]])) #get row/colums of those individuals in matrix 2
m12=adjs.e[[1]][ids.m1,ids.m1] #matrix 1 of association indices of only returning individuals
m21=adjs.e[[2]][ids.m2,ids.m2] #matrix 2 of association indices of only returning individuals
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


###Potenial way to get pearson correlation###
#DO NOT USE THIS-JUST PRACTICE WITH SRI AND MATRICES

####this works use this if can't figure out other way
#PRACTICE WITH GBI
dat.1.test <- dat.1.test[,2:16]


datd.1 <- dat.1[,2:16]
datd.2 <- dat.2[,2:16]

date.1 <- dat.e.1[,2:16]
date.2 <- dat.e.2[,2:16]

datf.1 <- dat.f.1[,2:16]
datf.2 <- dat.f.2[,2:16]

#create null matrix to fill with associations
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

##make into function

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

###Do not use###
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




####Try with ANTS r package and see if we get similar results####
install.packages("devtools")
devtools::install_github('SebastianSosa/ANTs')

library(devtools)
install_github("SebastianSosa/ANTs")
Sys.setenv(LANG = "eng")
library(ANTs)

####converting dat.1 for pen D into gbi ####

summary(dat.1)

  n1 = pivot_longer(dat.1, cols = c(2:16), names_to = "bird.id", values_to = "ant.id", values_drop_na = TRUE)
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

m2 <- t(m1)

row.names(m2) = 1:nrow(m2)
colnames(m2) = 1:ncol(m2)

df = ANTs::gbi.to.df(m2)

pre.net = perm.ds.grp(df, scan = 'scan', nperm = 1000, progress = TRUE, index = 'sri')



save.image(file = "pre.net.RData")

load(file = "pre.net.RData")

test <- met.strength(pre.net[[1]])
test


RR1 <- list()
for(i in 1:1000) {
  RR1[[i]] <- erdos.renyi.game(15,105,type=c("gnm"),directed=FALSE,loops=FALSE)
}

x <- edge.attributes(RR1[[1]], index = E(RR1[[1]]))




####new associations pre-transition###

#Pen D

###the names are not the same from pre-transition to post-transition. Also there are 5 fewer birds
d.name <- d.name %>% 
  substring(.,2)

dat <- read.csv2("MarinaDataFormat1.csv", header = T, sep = ";")

summary(dat)
names(dat)

dat <- dat %>% 
  mutate(pen = as.factor(ï..Pen),
         date = dmy(Date),
         fa = as.factor(Focal.Animal),
         time = hms(Time),
         aa = as.factor(Association))

dat.d.pre <- dat %>% 
  filter(pen == "D")

dat.d.pre.edge <- dat.d.pre %>% 
  select(fa, aa)

summary(dat.d.pre.edge)

dat.d.pre.edge <- dat.d.pre.edge %>% 
  filter(aa != "individual not found")

dat.d.pre.edge <- dat.d.pre.edge %>% 
  filter(aa != "")

dat.d.pre.edge <- dat.d.pre.edge %>% 
 mutate(comb = paste(fa, aa, sep = "_"))

dat.d.pre.edge <- dat.d.pre.edge %>% 
  filter(comb != "XC_XC")

dat.d.pre.edge <- dat.d.pre.edge %>% 
  select(fa, aa)

dat.d.pre.edge <- dat.d.pre.edge %>% 
  filter(aa %in% d.name)

dat.d.pre.edge <- dat.d.pre.edge %>% 
  filter(fa %in% d.name)

d.d.edge <- as.matrix(dat.d.pre.edge)

dat.d.g <- graph.data.frame(d.d.edge, directed = FALSE)
a.dg <- as_adjacency_matrix(dat.d.g, 
                             type = c("both"), 
                             names = T, 
                             edges = T)

g.adj.d <- graph_from_adjacency_matrix(a.dg, "undirected", weighted = T)
set.seed(123)
plot(g.adj.d, edge.width = E(g.adj.d)$weight*0.012, vertex.color = "gold1", vertex.size = 10, edge.color = "gray10")
title("Pen D Pre-transition",  cex.main = 2.25)

try <- as.matrix(a.dg)
###Pen E pre-transition####
e.name <- names(dat.e.1[, -1])

e.name <- e.name %>% 
  substring(., 2)
e.name
dat.e.pre <- dat %>% 
  filter(pen == "E")

dat.e.pre.edge <- dat.e.pre %>% 
  select(fa, aa)

dat.e.pre.edge <- dat.e.pre.edge %>% 
  filter(aa != "individual not found")

dat.e.pre.edge <- dat.e.pre.edge %>% 
  filter(aa != "")

dat.e.pre.edge <- dat.e.pre.edge %>% 
  filter(aa %in% e.name)
dat.e.pre.edge <- dat.e.pre.edge %>% 
  filter(fa %in% e.name)

d.e.edge <- as.matrix(dat.e.pre.edge)

dat.e.g <- graph.data.frame(d.e.edge, directed = FALSE)
a.eg <- as_adjacency_matrix(dat.e.g, 
                            type = c("both"), 
                            names = T, 
                            edges = T)

g.adj.e <- graph_from_adjacency_matrix(a.eg, "undirected", weighted = T)
set.seed(123)
plot(g.adj.e, edge.width = E(g.adj.e)$weight*0.012, vertex.color = "gold1", vertex.size = 10, edge.color = "gray10")
title("Pen E Pre-transition",  cex.main = 2.25)


###Pen F pre-transition

f.name <- names(dat.f.1[, -1])

f.name <- f.name %>% 
  substring(., 2)

dat.f.pre <- dat %>% 
  filter(pen == "F")

dat.f.pre.edge <- dat.f.pre %>% 
  select(fa, aa)

dat.f.pre.edge <- dat.f.pre.edge %>% 
  filter(aa != "individual not found")

dat.f.pre.edge <- dat.f.pre.edge %>% 
  filter(aa != "")

dat.f.pre.edge <- dat.f.pre.edge %>% 
  filter(aa %in% f.name)
dat.f.pre.edge <- dat.f.pre.edge %>% 
  filter(fa %in% f.name)

d.f.edge <- as.matrix(dat.f.pre.edge)

dat.f.g <- graph.data.frame(d.f.edge, directed = FALSE)
plot(dat.f.g)
a.fg <- as_adjacency_matrix(dat.f.g, 
                            type = c("both"), 
                            names = T, 
                            edges = T)



g.adj.f <- graph_from_adjacency_matrix(a.fg, "undirected", weighted = T)
set.seed(123)
plot(g.adj.f, edge.width = E(g.adj.f)$weight*0.012, vertex.color = "gold1", vertex.size = 10, edge.color = "gray10")
title("Pen F Pre-transition",  cex.main = 2.25)


coms.d = cluster_fast_greedy(g.adj.d)
coms.e = cluster_fast_greedy(g.adj.e)
coms.f = cluster_fast_greedy(g.adj.f)

coms = lapply(gs.d, function(x) cluster_fast_greedy(x))
coms = lapply(gs.e, function(x) cluster_fast_greedy(x))
coms = lapply(gs.f, function(x) cluster_fast_greedy(x))

mods = sapply(coms, modularity)
mod.d = modularity(coms.d)
mod.d
node.colors=membership(coms) #assign node color based on community membership
set.seed(123)
plot(g.adj.d, edge.width=E(g.adj.d)$weight*0.012, vertex.size=10, vertex.color=node.colors, edge.color = "gray10")


l=layout_with_fr(g.adj.d)
V(g.adj.d)$color=node.colors
plot(g.adj.d, layout=l, edge.width=E(g.adj.d)$weight*0.012, vertex.size=10,edge.color="gray10", main=paste("Pen D Pre-transition", ": Modularity=", round(modularity(coms), 2)))

node.colors=membership(coms.e) #assign node color based on community membership
set.seed(123)
plot(g.adj.d, edge.width=E(g.adj.d)$weight*0.012, vertex.size=10, vertex.color=node.colors, edge.color = "gray10")


l=layout_with_fr(g.adj.e)
V(g.adj.e)$color=node.colors
plot(g.adj.e, layout=l, edge.width=E(g.adj.e)$weight*0.012, vertex.size=10,edge.color="gray10", main=paste("Pen E Pre-transition", ": Modularity=", round(modularity(coms.e), 2)))


node.colors=membership(coms.f) #assign node color based on community membership
set.seed(123)
l=layout_with_fr(g.adj.f)
V(g.adj.f)$color=node.colors
plot(g.adj.f, layout=l, edge.width=E(g.adj.f)$weight*0.012, vertex.size=10,edge.color="gray10", main=paste("Pen E Pre-transition", ": Modularity=", round(modularity(coms.e), 2)))

###testing modularity
df = ANTs::gbi.to.df(gbi)

pre.net = perm.ds.grp(df, scan = 'scan', nperm = 1000, progress = TRUE, index = 'sri')

pre.net



gbi=t(try)
gbi=t(m.list.e[[1]])
gbi=t(m.list.f[[1]]) 

swap.m=list() 

times=1000

for (k in 1:times){
  swap.m[[k]]=network_swap(gbi, swaps=1000)$Association_index }

swap.g=lapply(swap.m, function(x) graph_from_adjacency_matrix(x, "undirected", weighted=T)) 
mod.swap=sapply(swap.g, function(x) modularity(cluster_fast_greedy(x)))

hist(mod.swap, xlim = c(min(mod.swap), mod.d))
abline(v=mod.d, col="red", lty=2, lwd=2) 
p=(length(which(mod.swap>=mod.d[[1]]))+1)/(times+1)
p

?erdos.renyi.game
x <- names(dat.1)
x <- x[-16]

get.edge.ids(gs.d[[1]], vp = x)
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
edge.attributes(gs.d[[1]], index = E(gs.d[[1]]))
###putting variables together for results

pen <-  c("D", "E", "F")
global_p <- c("0.059", "0.0079", "0.002")
serial_p <- c("<0.001", "<0.001", "<0.001")

pen.global.serial <- cbind(pen, global_p, serial_p)

pen.global.serial <- as.data.frame(pen.global.serial)


adjs.d = lapply(m.list.d, function(x) get_network(t(x), data_format = "GBI", association_index = "SRI"))
?get_network()

id12=rownames(adjs[[1]])[rownames(adjs[[1]])%in%rownames(adjs[[2]])] #get IDs of birds that were present in both networks
ids.m1=match(id12,rownames(adjs[[1]])) #get row/columns of those individuals in matrix 1 
ids.m2=match(id12,rownames(adjs[[2]])) #get row/colums of those individuals in matrix 2
m12=adjs[[1]][ids.m1,ids.m1] #matrix 1 of association indices of only returning individuals
m21=adjs[[2]][ids.m2,ids.m2] #matrix 2 of association indices of only returning individuals
m12=m12[order(rownames(m12)),order(rownames(m12))] #reorder the rows/columns by alphanumeric order
m21=m21[order(rownames(m21)),order(rownames(m21))] #reorder the rows/columns by alphanumeric order

mantel12=mantel(as.dist(m12)~as.dist(m21)) 
mantel12


###try with a different format
library(stringr)
dat <- read.csv2("processed data/MarinaDataFormat2.csv", header = T, sep = ";")
summary(dat)
names(dat)

dat.1 <- dat %>% 
 select(!contains("behaviour"))

summary(dat.1)

dat.1 <- dat.1 %>% 
  mutate(pen = as.factor(ï..Pen),
         date = dmy(Date),
         fa = as.factor(Focal_Animal))

names(dat.1) = gsub(pattern = "Association.", replacement = "", x = names(dat.1))
names(dat.1)

dat.1 <- dat.1 %>% 
  select(-"ï..Pen", -"Date", -"Focal_Animal", -"individual.not.found")

names(dat.1)
dat.1 <- dat.1 %>% 
  relocate(fa)

dat.1.d <- dat.1 %>% 
  filter(pen == "D")

dat.1.d.try <- dat.1.d %>% 
  group_by(fa) %>% 
  summarise(across(BY:TU, sum))

x <- dat.1.d.try$fa

dat.1.d.d <- data.frame(dat.1.d.try[,-1], row.names = x)

dat.1.d.d <- dat.1.d.d[order(rownames(dat.1.d.d)),order(rownames(dat.1.d.d))]
dat.1.d.d <- dat.1.d.d[,order(colnames(dat.1.d.d))]
d.name
<<<<<<< HEAD

d.name <- d.name[! d.name %in% c('time2')]

d.name <- d.name %>% 
  substring(., 2)
=======
names(dat.1.d.d)
d.name <- d.name[! d.name %in% c('ime2')]
d.name <- d.name[! d.name %in% c('date')]
d.name <- d.name[! d.name %in% c('pen')]
>>>>>>> f5660d92632184d964797f01426aad1392dffbfc

dat.d.pre.edge <- dat.1.d.d %>% 
  select(all_of(d.name))
dat.d.pre.edge <- dat.d.pre.edge[,order(colnames(dat.d.pre.edge))]

dat.d.pre.edge <- dat.d.pre.edge %>% 
  filter(row.names(dat.d.pre.edge) %in% d.name)
dat.d.pre.edge <- as.matrix(dat.d.pre.edge)

g.d = graph_from_adjacency_matrix(dat.d.pre.edge, mode = "undirected", weighted = T)

set.seed(123)
plot(g.d, edge.width = E(g.d)$weight, layout=layout_with_fr(g.d))

coms.d = cluster_fast_greedy(g.d)

node.colors=membership(coms.d) #assign node color based on community membership

set.seed(123)
par(mfrow = c(1,1))

l=layout_with_fr(g.d)

V(g.d)$color=node.colors

plot(g.d, layout=l, edge.width=E(g.d)$weight, vertex.size=20,edge.color="gray10", main=paste("Pen D Pre-transition", ": Modularity=", round(modularity(coms.d), 2)))


####Pen E

dat.1.e <- dat.1 %>% 
  filter(pen == "E")

dat.1.e.try <- dat.1.e %>% 
  group_by(fa) %>% 
  summarise(across(BY:TU, sum))

x <- dat.1.e.try$fa

dat.1.e.e <- data.frame(dat.1.d.try[,-1], row.names = x)

dat.1.e.e <- dat.1.e.e[order(rownames(dat.1.e.e)),order(colnames(dat.1.e.e))]
dat.1.e.e <- dat.1.e.e[,order(colnames(dat.1.e.e))]
e.name

e.name <- e.name[! e.name %in% c('time1')]
e.name <- e.name %>% 
  substring(., 2)

dat.e.pre.edge <- dat.1.e.e %>% 
  select(all_of(e.name))

dat.e.pre.edge <- dat.e.pre.edge %>% 
  filter(row.names(dat.e.pre.edge) %in% e.name)
dat.e.pre.edge <- as.matrix(dat.e.pre.edge)
diag(dat.e.pre.edge) <- 0

g.e = graph_from_adjacency_matrix(dat.e.pre.edge, mode = "undirected", weighted = T)

set.seed(123)
plot(g.e, edge.width = E(g.e)$weight, layout=layout_with_fr(g.e))

coms.e = cluster_fast_greedy(g.e)

node.colors=membership(coms.e) #assign node color based on community membership

set.seed(123)

l=layout_with_fr(g.e)
V(g.e)$color=node.colors
plot(g.e, layout=l, edge.width=E(g.e)$weight, vertex.size=20,edge.color="gray10", main=paste("Pen E Pre-transition", ": Modularity=", round(modularity(coms.e), 2)))


####Pen F

dat.1.f <- dat.1 %>% 
  filter(pen == "F")

dat.1.f.try <- dat.1.f %>% 
  group_by(fa) %>% 
  summarise(across(BY:TU, sum))
x <- dat.1.f.try$fa
dat.1.f.f <- data.frame(dat.1.f.try[,-1], row.names = x)



dat.1.f.f <- dat.1.f.f[order(rownames(dat.1.f.f)),order(colnames(dat.1.f.f))]
f.name
f.name <- f.name[! f.name %in% c('time1')]
f.name <- f.name[! f.name %in% c('imeStamp')]
f.name <- f.name %>% 
  substring(., 2)

dat.f.pre.edge <- dat.1.f.f %>% 
  select(all_of(f.name))

dat.f.pre.edge <- dat.f.pre.edge %>% 
  filter(row.names(dat.f.pre.edge) %in% f.name)
dat.f.pre.edge <- as.matrix(dat.f.pre.edge)
diag(dat.f.pre.edge) <- 0
g.f = graph_from_adjacency_matrix(dat.f.pre.edge, mode = "undirected", weighted = T)

set.seed(123)
plot(g.f, edge.width = E(g.f)$weight, layout=layout_with_fr(g.f))

coms.f = cluster_fast_greedy(g.f)

node.colors=membership(coms.f) #assign node color based on community membership

set.seed(123)

l=layout_with_fr(g.f)
V(g.f)$color=node.colors
plot(g.f, layout=l, edge.width=E(g.f)$weight, vertex.size=20,edge.color="gray10", main=paste("Pen F Pre-transition", ": Modularity=", round(modularity(coms.f), 2)))


#Pen D
swap.m=list() 

times=1000

for (k in 1:times){
  swap.m[[k]]=network_swap(dat.d.pre.edge, swaps=1000)$Association_index }

swap.f=lapply(swap.m, function(x) graph_from_adjacency_matrix(x, "undirected", weighted=T)) 
mod.swap=sapply(swap.f, function(x) modularity(cluster_fast_greedy(x)))

hist(mod.swap, xlim=c(min(mod.swap), modularity(coms.d)))
abline(v=modularity(coms.d), col="red", lty=2, lwd=2) 
p=(length(which(mod.swap>=modularity(coms.d)))+1)/(times+1)
p


#Pen E
swap.e=list() 

times=1000

for (k in 1:times){
  swap.e[[k]]=network_swap(dat.e.pre.edge, swaps=1000)$Association_index }

swap.e.1=lapply(swap.e, function(x) graph_from_adjacency_matrix(x, "undirected", weighted=T)) 
mod.e.swap=sapply(swap.e.1, function(x) modularity(cluster_spinglass(x)))
mod.e.swap=sapply(swap.e.1, function(x) modularity(cluster_fast_greedy(x)))

cluster_spinglass(x)

try <- map(swap.e.1, safe.mod.swap)
try.1 <- try[! try %in% c("Error in file")]
Pen.e.swap <- unlist(try.1, use.names=FALSE)

hist(Pen.e.swap, xlim=c(min(Pen.e.swap), modularity(coms.e)))
abline(v=modularity(coms.e), col="red", lty=2, lwd=2) 
p=(length(which(Pen.e.swap>=modularity(coms.e)))+1)/(times+1)
p


#Pen F
swap.f=list() 

times=1000

for (k in 1:times){
  swap.f[[k]]=network_swap(dat.f.pre.edge, swaps=1000)$Association_index }


swap.f.1=lapply(swap.f, function(x) graph_from_adjacency_matrix(x, "undirected", weighted=T)) 
mod.swap <- function(x){
  modularity(cluster_fast_greedy(x))
}
mod.swap=possibly(sapply(swap.f, function(x) modularity(cluster_fast_greedy(x))), otherwise = "Error in file")


safe.mod.swap <- possibly(mod.swap, otherwise = "Error in file")

try <- map(swap.f.1, safe.mod.swap)
str(try, max.level = 1)
try.1 <- try[! try %in% c("Error in file")]
Pen.f.swap <- unlist(try.1, use.names=FALSE)
hist(Pen.f.swap, xlim=c(min(Pen.f.swap), modularity(coms.f)))
abline(v=modularity(coms.f), col="red", lty=2, lwd=2) 
p=(length(which(Pen.f.swap>=modularity(coms.f)))+1)/(times+1)
p


#restrict comparison to individuals that were seen in two time periods - don't really need to do this I think as we only have the same 15###
###good practice though###

###need to get rid of the first letter of adj datasets###
names(adjs.d)[-1] <- sub("D.", "", names(adjs.d)[-1], fixed = TRUE)

rownames(adjs.d[[1]]) <- sub("D", "", rownames(adjs.d[[1]]))
rownames(adjs.d[[2]]) <- sub("D", "", rownames(adjs.d[[2]])) 
colnames(adjs.d[[1]]) <- sub("D", "", colnames(adjs.d[[1]]))
colnames(adjs.d[[2]]) <- sub("D", "", colnames(adjs.d[[2]])) 

colnames(adjs.d[[1]])
colnames(m13)
id12=rownames(adjs.d[[1]])[rownames(adjs.d[[1]])%in%rownames(adjs.d[[2]])] #get IDs of birds that were present in both networks
ids.m1=match(id12,rownames(adjs.d[[1]])) #get row/columns of those individuals in matrix 1 
ids.m2=match(id12,rownames(adjs.d[[2]])) #get row/colums of those individuals in matrix 2
m12=adjs.d[[1]][ids.m1,ids.m1] #matrix 1 of association indices of only returning individuals
m21=adjs.d[[2]][ids.m2,ids.m2] #matrix 2 of association indices of only returning individuals
m12=m12[order(rownames(m12)),order(rownames(m12))] #reorder the rows/columns by alphanumeric order
m21=m21[order(rownames(m21)),order(rownames(m21))] #reorder the rows/columns by alphanumeric order

id13=rownames(dat.d.pre.edge)[rownames(dat.d.pre.edge)%in%rownames(adjs.d[[1]])]
ids.m3=match(id13,rownames(dat.d.pre.edge)) #get row/columns of those individuals in matrix 1 
ids.m4=match(id13,rownames(adjs.d[[1]])) #get row/colums of those individuals in matrix 2
m13=dat.d.pre.edge[ids.m3,ids.m3] #matrix 1 of association indices of only returning individuals
m31=adjs.d[[1]][ids.m4,ids.m4] #matrix 2 of association indices of only returning individuals
m13=m13[order(rownames(m13)),order(rownames(m13))] #reorder the rows/columns by alphanumeric order
m31=m31[order(rownames(m31)),order(rownames(m31))] #reorder the rows/columns by alphanumeric order

library(ecodist)
mantel12=mantel(as.dist(m12)~as.dist(m21)) 
mantel12
mantel13=mantel(as.dist(m13)~as.dist(m31)) 
mantel13
mantel23=mantel(as.dist(m21)~as.dist(m13)) 
mantel23


<<<<<<< HEAD

###need to get rid of the first letter of adj datasets###
names(adjs.e)[-1] <- sub("E.", "", names(adjs.d)[-1], fixed = TRUE)

rownames(adjs.e[[1]]) <- sub("E", "", rownames(adjs.e[[1]]))
rownames(adjs.e[[2]]) <- sub("E", "", rownames(adjs.e[[2]])) 
colnames(adjs.e[[1]]) <- sub("E", "", colnames(adjs.e[[1]]))
colnames(adjs.e[[2]]) <- sub("E", "", colnames(adjs.e[[2]])) 

colnames(adjs.e[[1]])
colnames(m23)
id.e.12=rownames(adjs.e[[1]])[rownames(adjs.e[[1]])%in%rownames(adjs.e[[2]])] #get IDs of birds that were present in both networks
ids.e.m1=match(id.e.12,rownames(adjs.e[[1]])) #get row/columns of those individuals in matrix 1 
ids.e.m2=match(id.e.12,rownames(adjs.e[[2]])) #get row/colums of those individuals in matrix 2
m.e.12=adjs.e[[1]][ids.e.m1,ids.e.m1] #matrix 1 of association indices of only returning individuals
m.e.21=adjs.e[[2]][ids.e.m2,ids.e.m2] #matrix 2 of association indices of only returning individuals
m.e.12=m.e.12[order(rownames(m.e.12)),order(rownames(m.e.12))] #reorder the rows/columns by alphanumeric order
m.e.21=m.e.21[order(rownames(m.e.21)),order(rownames(m.e.21))] #reorder the rows/columns by alphanumeric order

id.e.13=rownames(dat.e.pre.edge)[rownames(dat.e.pre.edge)%in%rownames(adjs.e[[1]])]
ids.e.m3=match(id.e.13,rownames(dat.e.pre.edge)) #get row/columns of those individuals in matrix 1 
ids.e.m4=match(id.e.13,rownames(adjs.e[[1]])) #get row/colums of those individuals in matrix 2
m.e.13=dat.e.pre.edge[ids.e.m3,ids.e.m3] #matrix 1 of association indices of only returning individuals
m.e.31=adjs.e[[1]][ids.e.m4,ids.e.m4] #matrix 2 of association indices of only returning individuals
m.e.13=m.e.13[order(rownames(m.e.13)),order(rownames(m.e.13))] #reorder the rows/columns by alphanumeric order
m.e.31=m.e.31[order(rownames(m.e.31)),order(rownames(m.e.31))] #reorder the rows/columns by alphanumeric order

library(ecodist)
rownames(m.e.12)
rownames(m.e.13)
rownames(m.e.21)
mantel.e.12=mantel(as.dist(m.e.12)~as.dist(m.e.21)) 
mantel.e.12
mantel.e.13=mantel(as.dist(m.e.12)~as.dist(m.e.13)) 
mantel.e.13
mantel.e.23=mantel(as.dist(m.e.21)~as.dist(m.e.13)) 
mantel.e.23



###need to get rid of the first letter of adj datasets###
names(adjs.e)[-1] <- sub("E.", "", names(adjs.d)[-1], fixed = TRUE)

rownames(adjs.f[[1]]) <- sub("F", "", rownames(adjs.f[[1]]))
rownames(adjs.f[[2]]) <- sub("F", "", rownames(adjs.f[[2]])) 
colnames(adjs.f[[1]]) <- sub("F", "", colnames(adjs.f[[1]]))
colnames(adjs.f[[2]]) <- sub("F", "", colnames(adjs.f[[2]])) 

colnames(adjs.f[[2]])

id.f.12=rownames(adjs.f[[1]])[rownames(adjs.f[[1]])%in%rownames(adjs.f[[2]])] #get IDs of birds that were present in both networks
ids.f.m1=match(id.f.12,rownames(adjs.f[[1]])) #get row/columns of those individuals in matrix 1 
ids.f.m2=match(id.f.12,rownames(adjs.f[[2]])) #get row/colums of those individuals in matrix 2
m.f.12=adjs.f[[1]][ids.f.m1,ids.f.m1] #matrix 1 of association indices of only returning individuals
m.f.21=adjs.f[[2]][ids.f.m2,ids.f.m2] #matrix 2 of association indices of only returning individuals
m.f.12=m.f.12[order(rownames(m.f.12)),order(rownames(m.f.12))] #reorder the rows/columns by alphanumeric order
m.f.21=m.f.21[order(rownames(m.f.21)),order(rownames(m.f.21))] #reorder the rows/columns by alphanumeric order

id.f.13=rownames(dat.f.pre.edge)[rownames(dat.f.pre.edge)%in%rownames(adjs.f[[1]])]
ids.f.m3=match(id.f.13,rownames(dat.f.pre.edge)) #get row/columns of those individuals in matrix 1 
ids.f.m4=match(id.f.13,rownames(adjs.f[[1]])) #get row/colums of those individuals in matrix 2
m.f.13=dat.f.pre.edge[ids.f.m3,ids.f.m3] #matrix 1 of association indices of only returning individuals
m.f.31=adjs.f[[1]][ids.f.m4,ids.f.m4] #matrix 2 of association indices of only returning individuals
m.f.13=m.f.13[order(rownames(m.f.13)),order(rownames(m.f.13))] #reorder the rows/columns by alphanumeric order
m.f.31=m.f.31[order(rownames(m.f.31)),order(rownames(m.f.31))] #reorder the rows/columns by alphanumeric order

library(ecodist)
rownames(m.f.12)
rownames(m.f.13)
rownames(m.f.21)
rownames(m.f.31)
mantel.f.12=mantel(as.dist(m.f.12)~as.dist(m.f.21)) 
mantel.f.12
mantel.f.13=mantel(as.dist(m.f.12)~as.dist(m.f.13)) 
mantel.f.13
mantel.f.23=mantel(as.dist(m.f.21)~as.dist(m.f.13)) 
mantel.f.23
=======
####get strength of ties for each network for each individual
d.1 <- network(adjs.d[[1]],
               directed = F,
               ignore.eval = F, 
               names.eval = "weight")

d.2 <- network(adjs.d[[2]],
               directed = F,
               ignore.eval = F, 
               names.eval = "weight")

gplot(d.1, gmode="graph", 
      edge.col="darkgrey", 
      edge.lwd=d.1 %e% "weight")

gplot(d.2, gmode="graph", 
      edge.col="darkgrey", 
      edge.lwd=d.1 %e% "weight")
connectedness(d.1)
connectedness(d.2)

d1.deg<-sna::degree(d.1,  cmode="outdegree",ignore.eval=T)
d2.deg<-sna::degree(d.2,  cmode="outdegree",ignore.eval=T)

d1.wdeg<-sna::degree(d.1,  cmode="outdegree",ignore.eval=F)
d2.wdeg<-sna::degree(d.2,  cmode="outdegree",ignore.eval=F)

plot(d1.wdeg, d2.wdeg, pch = 16,
     xlab = "Weighted degree in time period 1",
     ylab = "Weighted degree in time period 2")

cor.test(d1.wdeg, d2.wdeg, method = "spearman")


e.1 <- network(adjs.e[[1]],
               directed = F,
               ignore.eval = F, 
               names.eval = "weight")

e.2 <- network(adjs.e[[2]],
               directed = F,
               ignore.eval = F, 
               names.eval = "weight")

connectedness(e.1)
connectedness(e.2)

e1.wdeg<-sna::degree(e.1,  cmode="outdegree",ignore.eval=F)
e2.wdeg<-sna::degree(e.2,  cmode="outdegree",ignore.eval=F)

plot(jitter(e1.wdeg), jitter(e2.wdeg), pch = 16,
     xlab = "Weighted degree in time period 1",
     ylab = "Weighted degree in time period 2")

cor.test(e1.wdeg, e2.wdeg, method = "spearman")

f.1 <- network(adjs.f[[1]],
               directed = F,
               ignore.eval = F, 
               names.eval = "weight")

f.2 <- network(adjs.f[[2]],
               directed = F,
               ignore.eval = F, 
               names.eval = "weight")

connectedness(f.1)
connectedness(f.2)

f1.wdeg<-sna::degree(f.1,  cmode="outdegree",ignore.eval=F)
f2.wdeg<-sna::degree(f.2,  cmode="outdegree",ignore.eval=F)

plot(jitter(f1.wdeg), jitter(f2.wdeg), pch = 16,
     xlab = "Weighted degree in time period 1",
     ylab = "Weighted degree in time period 2")

cor.test(f1.wdeg, f2.wdeg, method = "spearman")


connectedness(dat.d.pre.edge)

d.pre <- network(dat.d.pre.edge,
               directed = F,
               ignore.eval = F, 
               names.eval = "weight")

dpre.wdeg<-sna::degree(d.pre,  cmode="outdegree",ignore.eval=F)

plot(jitter(dpre.wdeg), jitter(d1.wdeg), pch = 16,
     xlab = "Weighted degree pretransition",
     ylab = "Weighted degree in time period 1")


cor.test(dpre.wdeg, d1.wdeg, method = "spearman")


degree=igraph::degree
betweenness=igraph::betweenness
closeness=igraph::closeness
compon

degree(gs.d[[2]])
gs.d1 <- graph.strength(gs.d[[1]])
gs.d2 <- graph.strength(gs.d[[2]])
gs.pre <- graph.strength(g.d)

cor.test(gs.d1, gs.d2, method = "spearman")
cor.test(gs.pre, gs.d1, method = "spearman")

edge_density(gs.d[[1]])
hist(strength(gs.d[[1]]))
hist(strength(gs.d[[2]]))
hist(strength(g.d))

hist(degree(gs.d[[1]]))
hist(degree(gs.d[[2]]))
hist(degree(g.d))
bd.1 = betweenness(gs.d[[1]], normalized=T)
bd.2 = betweenness(gs.d[[2]], normalized=T)
bd.pre = betweenness(g.d, normalized = T)

cd.1 = closeness(gs.d[[1]], normalized = T)
cd.2 = closeness(gs.d[[2]], normalized = T)
cd.pre = closeness(g.d, normalized = T)

ecd.1 = eigen_centrality(gs.d[[1]])$vector
ecd.2 = eigen_centrality(gs.d[[2]])$vector
ecd.pre = eigen_centrality(g.d)$vector

names = V(gs.d[[2]])$name
names.pre = V(g.d)$name

d.time1 = data.frame(nodes = as.factor(names), strength = gs.d1, betweenness = bd.1, closeness = cd.1, e_cent = ecd.1, timepoint = 2)
summary(d)
d.time1$nodes <- d.time1$nodes %>% 
  substring(.,2)

d.timepre =  data.frame(nodes = as.factor(names.pre), strength = gs.pre, betweenness = bd.pre, closeness = cd.pre, e_cent = ecd.pre, timepoint = 1)

d.time2 = data.frame(nodes = as.factor(names), strength = gs.d2, betweenness = bd.2, closeness = cd.2, e_cent = ecd.2, timepoint = 3)
summary(d)
d.time2$nodes <- d.time2$nodes %>% 
  substring(.,2)

d.all <- rbind(d.timepre, d.time1, d.time2)
summary(d.all)
d.all$timepoint <- as.factor(d.all$timepoint)
require(lme4)
d.m1 <- lmer(scale(strength) ~ timepoint + (1|nodes), data = d.all)
summary(d.m1)
tidy(d.m1, effects = c("fixed"), conf.int = T, conf.level = 0.95)
em.d1 <- emmeans(d.m1, ~ timepoint)
pairs(em.d1)

d.m2 <- lmer(betweenness ~ timepoint + (1|nodes), data = d.all)
summary(d.m2)
tidy(d.m2, effects = c("fixed"), conf.int = T, conf.level = 0.95)
em.d2 <- emmeans(d.m2, ~ timepoint)
pairs(em.d2)

d.m3 <- lmer(e_cent ~ timepoint + (1|nodes), data = d.all)
summary(d.m3)

tidy(d.m3, effects = c("fixed"), conf.int = T, conf.level = 0.95)

em.d3 <- emmeans(d.m3, ~ timepoint)
pairs(em.d3)

gs.e1 <- graph.strength(gs.e[[1]])
gs.e2 <- graph.strength(gs.e[[2]])
gs.epre <- graph.strength(g.e)

cor.test(gs.d1, gs.d2, method = "spearman")
cor.test(gs.pre, gs.d1, method = "spearman")

edge_density(gs.d[[1]])
hist(strength(gs.d[[1]]))
hist(strength(gs.d[[2]]))
hist(strength(g.d))

hist(degree(gs.d[[1]]))
hist(degree(gs.d[[2]]))
hist(degree(g.d))
be.1 = betweenness(gs.e[[1]], normalized=T)
be.2 = betweenness(gs.e[[2]], normalized=T)
be.pre = betweenness(g.e, normalized = T)

ce.1 = closeness(gs.e[[1]], normalized = T)
ce.2 = closeness(gs.e[[2]], normalized = T)
ce.pre = closeness(g.e, normalized = T)

ece.1 = eigen_centrality(gs.e[[1]])$vector
ece.2 = eigen_centrality(gs.e[[2]])$vector
ece.pre = eigen_centrality(g.e)$vector

names = V(gs.e[[2]])$name
names.pre = V(g.e)$name

e.time1 = data.frame(nodes = as.factor(names), strength = gs.e1, betweenness = be.1, closeness = ce.1, e_cent = ece.1, timepoint = 2)
summary(e.time1)
e.time1$nodes <- e.time1$nodes %>% 
  substring(.,2)

e.timepre =  data.frame(nodes = as.factor(names.pre), strength = gs.epre, betweenness = be.pre, closeness = ce.pre, e_cent = ece.pre, timepoint = 1)

e.time2 = data.frame(nodes = as.factor(names), strength = gs.e2, betweenness = be.2, closeness = ce.2, e_cent = ece.2, timepoint = 3)
summary(d)
e.time2$nodes <- e.time2$nodes %>% 
  substring(.,2)

e.all <- rbind(e.timepre, e.time1, e.time2)
summary(e.all)
e.all$timepoint <- as.factor(e.all$timepoint)

e.m1 <- lmer(scale(strength) ~ timepoint + (1|nodes), data = e.all)
summary(e.m1)
tidy(e.m1, effects = c("fixed"), conf.int = T, conf.level = 0.95)
em.e1 <- emmeans(e.m1, ~ timepoint)
pairs(em.e1)
e.m2 <- lmer(betweenness ~ timepoint + (1|nodes), data = e.all)
summary(e.m2)
tidy(e.m2, effects = c("fixed"), conf.int = T, conf.level = 0.95)
em.e2 <- emmeans(e.m2, ~ timepoint)
pairs(em.e2)
e.m3 <- lmer(e_cent ~ timepoint + (1|nodes), data = e.all)
summary(e.m3)
tidy(e.m3, effects = c("fixed"), conf.int = T, conf.level = 0.95)
em.e3 <- emmeans(e.m3, ~ timepoint)
pairs(em.e3)

gs.f1 <- graph.strength(gs.f[[1]])
gs.f2 <- graph.strength(gs.f[[2]])
gs.fpre <- graph.strength(g.f)

bf.1 = betweenness(gs.f[[1]], normalized=T)
bf.2 = betweenness(gs.f[[2]], normalized=T)
bf.pre = betweenness(g.f, normalized = T)

cf.1 = closeness(gs.f[[1]], normalized = T)
cf.2 = closeness(gs.f[[2]], normalized = T)
cf.pre = closeness(g.f, normalized = T)

ecf.1 = eigen_centrality(gs.f[[1]])$vector
ecf.2 = eigen_centrality(gs.f[[2]])$vector
ecf.pre = eigen_centrality(g.f)$vector

names = V(gs.f[[2]])$name
names.pre = V(g.f)$name

summary

f.time1 = data.frame(nodes = as.factor(names), strength = gs.f1, betweenness = bf.1, closeness = cf.1, e_cent = ecf.1, timepoint = 2)
summary(f.time1)
f.time1$nodes <- f.time1$nodes %>% 
  substring(.,2)

f.timepre =  data.frame(nodes = as.factor(names.pre), strength = gs.fpre, betweenness = bf.pre, closeness = cf.pre, e_cent = ecf.pre, timepoint = 1)

f.time2 = data.frame(nodes = as.factor(names), strength = gs.f2, betweenness = bf.2, closeness = cf.2, e_cent = ecf.2, timepoint = 3)
summary(d)
f.time2$nodes <- f.time2$nodes %>% 
  substring(.,2)

f.all <- rbind(f.timepre, f.time1, f.time2)
summary(e.all)
f.all$timepoint <- as.factor(f.all$timepoint)

f.m1 <- lmer(scale(strength) ~ timepoint + (1|nodes), data = f.all)
summary(f.m1)
fit <- as(f.m1, "merModLmerTest")
tidy(f.m1, effects = c("fixed"), conf.int = T, conf.level = 0.95)
em.f1 <- emmeans(f.m1, ~ timepoint)
pairs(em.f1)
f.m2 <- lmer(betweenness ~ timepoint + (1|nodes), data = f.all)
summary(f.m2)

f.m3 <- lmer(e_cent ~ timepoint + (1|nodes), data = f.all)
summary(f.m3)

install.packages("pbkrtest")
require(pbkrtest)
require(lmerTest)
require(emmeans)
require(tidyverse)
require(broom.mixed)
>>>>>>> f5660d92632184d964797f01426aad1392dffbfc
