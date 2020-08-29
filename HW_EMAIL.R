load("C:\\Users\\shelb\\OneDrive\\Documents\\MSA2021\\Linear_Algebra\\LeukError.RData")
dim(leuk)

str(leuk)
is.data.frame(leuk)
View(leuk)

leuk[,5001] <- as.numeric(as.factor(leuk[,5001]))


randomColumns = sample(2:5000,2) #sample(x, size) 
randomColumns #taking 2 colums
plot(leuk[,randomColumns])


# covariance
pcaOut = prcomp(leuk, rank =3, scale = F)


c<-summary(pcaOut)
c
c["rotation"] # principal components or loading or eigenvectors
c[[5]] # the scores
c[[6]][1,] # standard deviation
c[[6]][2,] # proportion of variance

library(rgl)
label = as.double(leuk[,5001])
plot3d(x = pcaOut$x[,1], y = pcaOut$x[,2],z= pcaOut$x[,3],size = 8, col = label, xlab = "Principal Component 1", ylab = "Principal Component 2", zlab = "Principal Component 3")
x = pcaOut$x[,1]
y = pcaOut$x[,2]
z= pcaOut$x[,3]


library(RColorBrewer)
library(ggfortify)
display.brewer.all()
palette(brewer.pal(n = 3, name = "Dark2"))

colors = factor(palette())
colors = colors[label]
colors

library(plotly)
graph = plot_ly(x = pcaOut$x[,1], y = pcaOut$x[,2],z= pcaOut$x[,3], type='scatter3d', mode="markers", marker = list(color=colors))
graph <- graph %>% layout(scene = list(xaxis = list(title = 'Principal Component 1'), yaxis = list(title = 'Principal Component 2'), zaxis = list(title = 'Principal Component 3')))
graph

#screeplot
plot(pcaOut$sdev^2, type='b', ylab = 'Eigenvalue (Variance of Each Component)', main = 'Starwars Screeplot')

install.packages(c("FactoMineR", "factoextra"))
library(factoextra)
library(FactoMineR)


# On PCA#1
g<-as.data.frame(pcaOut$x[,1])
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(g, graph = F)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

# On PCA #2
g<-as.data.frame(pcaOut$x[,2])
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(g, graph = F)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

# On PCA #3
g<-as.data.frame(pcaOut$x[,3])
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(g, graph = F)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)



