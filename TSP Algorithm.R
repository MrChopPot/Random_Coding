#install.packages("TSP")
library(TSP)
tsp_df <- ETSP(data.frame(x = c(2,0,4,4,6,7,4), y = c(2,6,5,7,4,2,2)))
shopping_path <- solve_TSP(tsp_df)
shopping_path
tour_length(shopping_path)
plot(tsp_df, shopping_path, cex = 2, ylim = c(1,8))
title(main = "Optimal Route")
text(tsp_df, labels = rownames(tsp_df), cex=0.9, font=2)


#install.packages("igraph")
#library(igraph)
#mat_net <- matrix(c(0,10,13,18,14,18,0,10,0,12,15,17,22,15,13,12,0,8,6,11,9,18,15,8,
                    0,10,15,16,14,17,6,10,0,6,8,18,22,11,15,6,0,10,0,15,9,16,8,10,0),7,7)
#g <- graph_from_adjacency_matrix(mat_net, weighted = T) 
#plot(g, 
     vertex.label.color = "black", 
     edge.color = 'gray77',
     vertex.size = 20,
     edge.arrow.size = 0,
     layout = layout_nicely(g))
#V(g)
#E(g)
#get.shortest.paths(g, 1, 7)[[1]]
#shortest.paths(g, algorithm = "dijkstra")[1,7]

