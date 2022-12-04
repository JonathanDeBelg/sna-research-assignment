library(igraph)
library(network)
library(snafun)
library("readxl")
library(texreg)

data_path <- '/Users/jonathan/Documents/JADS/year-1/social-network-analysis/sna-research-assignment/data/formatted_data.csv'
forum_user_data <- '/Users/jonathan/Documents/JADS/year-1/social-network-analysis/sna-research-assignment/data/formatted_forum_user_data.xlsx'
dr_data <- '/Users/jonathan/Documents/JADS/year-1/social-network-analysis/sna-research-assignment/data/Death ratio.xlsx'

forum_data <- data.frame(readxl::read_xlsx(forum_user_data))
dataas <- read.csv(file = data_path)
dataas <- dataas[,!names(dataas) %in% c("user_msg", "X", "user_type", "user_msg_count")]
dataas <- dataas[!duplicated(dataas), ]

dr_data <- data.frame(readxl::read_xlsx(dr_data))

weights <- dataas[,3]
weights
net <- dataas[,1:2]

eAttr <- data.frame(dataas$count)
nAttr <- as.vector(forum_data[,!names(forum_data) %in% c("Forum", "Eindtotaal")])

noMessages <- nAttr$Number.Of.Messages
mortality <- dr_data$Mortality
avg_diagnose_age <- dr_data$Average.age.of.diagnosis

EdgeList <- snafun::make_edgelist(net)
NodeList <- snafun::make_nodelist(net)

surveynet <- igraph::graph_from_data_frame(
  EdgeList, 
  NodeList, 
  directed=FALSE)

surveynet <- surveynet %>%
  set_vertex_attr("no_messages", value = noMessages)
surveynet <- surveynet %>%
  set_vertex_attr("mortality", value = mortality)
surveynet <- surveynet %>%
  set_vertex_attr("avg_diagnose_age", value = avg_diagnose_age)

igraph::E(surveynet)$weight <- weights

plot(surveynet,
     edge.color = igraph::E(surveynet)/15,        
     edge.width = igraph::E(surveynet)/4,
     vertex.border = 'green',
     vertex.size = 20,
     vertex.label = snafun::extract_vertex_names(surveynet),
     vertex.label.cex = .5,
     vertex.label.color = 'black')

matrix_path <- '/Users/jonathan/Documents/JADS/year-1/social-network-analysis/sna-research-assignment/data/mental_disease_matrix.xlsx'
matrix_table <- read_excel(matrix_path)
com_matrix <- data.matrix(matrix_table)
com_matrix

mental_disease_fora_net <- to_network(surveynet)
mental_disease_fora_net
surveynet

md_model <- ergm::ergm(mental_disease_fora_net ~ edges)
summary(md_model)

adj_weight <- igraph::as_adjacency_matrix(surveynet, attr="weight")
matrix_weight <- as.matrix(adj_weight)

cov_data <- data.frame(
  NodeList,
  diagnose_age = avg_diagnose_age,
  no_messages = noMessages,
  mortality = mortality
)

cov_data

formula2 <- matrix_weight ~ edges + mutual + netcov(com_matrix)
formula2
m2 <- GERGM::gergm(formula2,
                   covariate_data = cov_data,
                   number_of_networks_to_simulate = 60000,
                   thin = 1/100,
                   proposal_variance = 0.05,
                   MCMC_burnin = 1000,
                   seed = 456,
                   convergence_tolerance = 0.5,
                   parallel = TRUE,
                   cores = 5)
m2
GERGM::GOF(m2)
