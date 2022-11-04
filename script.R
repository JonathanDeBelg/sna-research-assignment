library(igraph)
library(network)
data_path <- '/Users/jonathan/Documents/JADS/year-1/social-network-analysis/sna-research-assignment/data/formatted_data.csv'

dataas <- read.csv(file = data_path)
dataas <- dataas[,!names(dataas) %in% c("user_msg", "X", "user_type", "user_msg_count")]
dataas <- dataas[!duplicated(dataas), ]
net <- dataas[,1:2]

eAttr <- data.frame(dataas$count)
EdgeList <- snafun::make_edgelist(net, eAttr)
NodeList <- snafun::make_nodelist(net)

NodeList
head(EdgeList)

surveynet <- igraph::graph_from_data_frame(
            EdgeList, 
            NodeList, 
            directed=FALSE)
surveynet
eAttr
igraph::E(surveynet)$weight <- eAttr
igraph::E(surveynet)$weight <- dataas$count

names <-snafun::extract_vertex_names(surveynet) 
names

plot(surveynet,
           edge.color = igraph::E(surveynet)$weight/15,        
           edge.width = igraph::E(surveynet)$weight/15,
           vertex.border = 'green',
           vertex.size = 20,
           vertex.label = names,
           vertex.label.cex = 2,
           vertex.label.color = 'green')
 
