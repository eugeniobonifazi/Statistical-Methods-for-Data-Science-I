
# Organization of the data ------------------------------------------------

require(tseries, quietly = TRUE)

ConsDiscr <- c("AAP", "AMZN", "DRI", "BBY", "CMCSA")
Energy <- c("APC", "ANDV", "APA", "BHGE", "COG")
Financial <- c("AMG", "AFL", "ALL", "AXP", "AIG")
ConsStaples <- c("MO", "ADM", "CPB", "CHD", "CLX")
TelecomServ <- c("T", "CTL", "VZ", "FTR", "BCE")
HealCare <- c("ABT", "BAX", "AET", "A", "ALXN")
Indus <- c("PWR", "RTN", "RSG", "RHI", "ROK")
InfoTecn <- c("ACN", "ATVI", "ADBE", "AMD", "AKAM")
Materials <- c("APD", "ALB", "AVY", "BLL", "DWDP")
Utilities <- c("AES", "LNT", "AEE", "AEP", "EIX")

d <- c(ConsDiscr,Energy, Financial, ConsStaples, TelecomServ, HealCare, Indus, InfoTecn, Materials, Utilities)
ddata<-matrix(NA, 1258, 50)
for (i in 1:length(d)){
  ddata[,i] <- suppressWarnings(
    get.hist.quote(instrument=d[i], start="2003-01-01", end="2008-01-01",
                   quote= "Close", provider="yahoo", drop=TRUE)
  )
}
colnames(ddata)<-d

# here the matrix of data 


data_mat<-matrix(NA, 1257, 50)
for (i in 1:50){
  data_mat[,i]=diff(log(ddata[,i]))
}


# Pearson correlation -----------------------------------------------------

library(igraph)
library(manipulate)

# Correlation matrix (Pearson) and bootstrap

cor_mat=cor(data_mat)
colnames(cor_mat)<-d
rownames(cor_mat)<-d

B = 1000
b_vec = rep(NA, B)
for (i in 1:B){
  idx <- sample(1:1257, replace = T)
  R_star = data_mat[idx, ]
  delta = sqrt(1257)*max(abs(cor(R_star)-cor(data_mat)))
  b_vec[i] = delta
}

F_n = ecdf(b_vec) 
plot(F_n, col = "blue", xlab = "Bootstrap vector of Delta", ylab = "ECDF", main = "Ecdf of the bootstrap vector")


# Adjacency Matrix and graph creation with dynamic plot

AdjacencyMatrix_Graph <- function(alpha, matr, epsi){
  
  n = nrow(matr)
  m = ncol(matr)
  ad_mat = matrix(NA, n, m)
  t_alpha = quantile(F_n, 1-(alpha/choose(n, 2)))
  
  
  for (i in 1:n){
    for (j in 1:m){
      
      inf_CI = matr[i, j] - t_alpha*(1/sqrt(1257))
      sup_CI = matr[i, j] + t_alpha*(1/sqrt(1257))
      
      if (i==j){ad_mat[i, j]=0}
      
      else {
        if ((inf_CI<=epsi & sup_CI>=epsi) || (inf_CI<= -epsi & sup_CI >=-epsi)){ad_mat[i, j]=0}
        else {ad_mat[i, j]=1}
        
      }
      
    }
  }
  
  colnames(ad_mat)<-d
  rownames(ad_mat)<-d
  G <- igraph::graph.adjacency(ad_mat, mode = "undirected", diag = F)
  
  igraph::V(G)$color[1:5] <- "dodgerblue3"    # Consumer Discretionary
  igraph::V(G)$color[6:10] <- "gold2"         # Energy
  igraph::V(G)$color[11:15] <- "forestgreen"  # Financials
  igraph::V(G)$color[16:20] <- "lightblue2"   # Consumer Staples
  igraph::V(G)$color[21:25] <- "lightgray"    # Telecommunications Services
  igraph::V(G)$color[26:30] <- "indianred1"   # Health Care
  igraph::V(G)$color[31:35] <- "lightsalmon1" # Industrials
  igraph::V(G)$color[36:40] <- "moccasin"     # Information Technology
  igraph::V(G)$color[41:45] <- "midnightblue" # Materials
  igraph::V(G)$color[46:50] <- "chocolate1"   # Utilities
  
  
  return(plot(G, vertex.size = 10, vertex.label.cex = 0.50, vertex.label.color = "black"))
  
}

manipulate(
  AdjacencyMatrix_Graph(alpha = a, matr = cor_mat, epsi = e), 
  a = slider(.00000001, 0.5, .00000001, "alpha", .00000001), 
  e = slider(.0, 0.8, .0, "epsi", .001)
)




# Spearman correlation -----------------------------------------------------



library(igraph)
library(manipulate)

# Correlation matrix (Spearman) and bootstrap 

corr_mat2 <- cor(data_mat, method = "spearman")
colnames(corr_mat2)<-d
rownames(corr_mat2)<-d

B = 1000
b_vec2 = rep(NA, B)
for (i in 1:B){
  idx <- sample(1:1257, replace = T)
  R_star = data_mat[idx, ]
  delta = sqrt(1257)*max(abs(cor(R_star, method = "spearman")-corr_mat2))
  b_vec2[i] = delta
}

F_n2 = ecdf(b_vec2) 
plot(F_n2, col = "blue", xlab = "Bootstrap vector of Delta", ylab = "ECDF", main = "Ecdf of the bootstrap vector")


# Adjacency Matrix and graph creation with dynamic plot

AdjacencyMatrix_Graph <- function(alpha, matr, epsi){
  
  n = nrow(matr)
  m = ncol(matr)
  ad_mat = matrix(NA, n, m)
  t_alpha = quantile(F_n2, 1-(alpha/choose(n, 2)))
  
  
  for (i in 1:n){
    for (j in 1:m){
      
      inf_CI = matr[i, j] - t_alpha*(1/sqrt(1257))
      sup_CI = matr[i, j] + t_alpha*(1/sqrt(1257))
      
      if (i==j){ad_mat[i, j]=0}
      
      else {
        if ((inf_CI<=epsi & sup_CI>=epsi) || (inf_CI<= -epsi & sup_CI >=-epsi)){ad_mat[i, j]=0}
        else {ad_mat[i, j]=1}
        
      }
      
    }
  }
  
  colnames(ad_mat)<-d
  rownames(ad_mat)<-d
  G <- igraph::graph.adjacency(ad_mat, mode = "undirected", diag = F)
  
  igraph::V(G)$color[1:5] <- "dodgerblue3"    # Consumer Discretionary
  igraph::V(G)$color[6:10] <- "gold2"         # Energy
  igraph::V(G)$color[11:15] <- "forestgreen"  # Financials
  igraph::V(G)$color[16:20] <- "lightblue2"   # Consumer Staples
  igraph::V(G)$color[21:25] <- "lightgray"    # Telecommunications Services
  igraph::V(G)$color[26:30] <- "indianred1"   # Health Care
  igraph::V(G)$color[31:35] <- "lightsalmon1" # Industrials
  igraph::V(G)$color[36:40] <- "moccasin"     # Information Technology
  igraph::V(G)$color[41:45] <- "midnightblue" # Materials
  igraph::V(G)$color[46:50] <- "chocolate1"   # Utilities
  
  
  return(plot(G, vertex.size = 10, vertex.label.cex = 0.50, vertex.label.color = "black"))
  
}


manipulate(
  AdjacencyMatrix_Graph(alpha = a, matr = corr_mat2, epsi = e), 
  a = slider(.00000001, 0.5, .00000001, "alpha", .00000001), 
  e = slider(.0, 0.8, .0, "epsi", .001)
)

