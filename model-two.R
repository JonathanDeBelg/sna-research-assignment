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

surveynet <- snafun::add_vertex_attributes(surveynet, attr_name = "no_messages", noMessages)
surveynet <- snafun::add_vertex_attributes(surveynet, attr_name = "mortality", mortality)
surveynet <- snafun::add_vertex_attributes(surveynet, attr_name = "avg_diagnose_age", avg_diagnose_age)
surveynet <- snafun::add_edge_attributes(surveynet, attr_name = "weight", weights)

plot(surveynet,
     edge.color = igraph::E(surveynet)/15,        
     edge.width = igraph::E(surveynet)/4,
     vertex.border = 'green',
     vertex.size = 20,
     vertex.label = snafun::extract_vertex_names(surveynet),
     vertex.label.cex = .5,
     vertex.label.color = 'black')

matrix_path <- '/Users/jonathan/Documents/JADS/year-1/social-network-analysis/sna-research-assignment/data/mental_disease_matrix.xlsx'
matrix_table <- readxl::read_excel(matrix_path)
com_matrix <- data.matrix(matrix_table)
com_matrix

mental_disease_fora_net <- snafun::to_network(surveynet)
mental_disease_fora_net

adj_weight <- igraph::as_adjacency_matrix(surveynet, attr="weight")
matrix_weight <- as.matrix(adj_weight)

cov_data <- data.frame(
  NodeList,
  diagnose_age = avg_diagnose_age,
  no_messages = noMessages,
  mortality = mortality
)
subset <- cov_data[2]

subset$diagnose_age <- round(subset$diagnose_age)
class(subset)
rownames(subset)
colnames(matrix_weight) == rownames(matrix_weight)
rownames(subset) <- NodeList
nrow(subset)

matrix_weight
formula2 <- matrix_weight ~ edges + mutual + absdiff("diagnose_age") + netcov(com_matrix)
formula2
m2 <- GERGM::gergm(formula2,
                   covariate_data = subset,
                   number_of_networks_to_simulate = 100000,
                   MCMC_burnin = 10000,
                   thin = 1/10,
                   parallel = TRUE,
                   cores = 6)
summary(m2)
(EstSE <- rbind(t(attributes(m2)$theta.coef),
                t(attributes(m2)$lambda.coef)))


lower = -0.07217179 - 0.04763048*(-qnorm((1 - 0.95)/2))
upper = -0.07217179 + 0.04763048*(-qnorm((1 - 0.95)/2))

lower
upper

GERGM::Trace_Plot(m2)
GERGM::Estimate_Plot(m2)
fit.gof <- GERGM::GOF(m2)
plot(fit.gof)

odds <- exp(-0.07217179)
# The odds forming a tie if they have the same age is about 0.93:1  
# the odds of interacting within the fora 

# If you are interested in betting, then think of this as the odds of forming a 
# business tie being 13.7:1 in the presence of a marriage tie. Sounds like a pretty 
# good bet.


  
