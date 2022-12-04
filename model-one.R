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

filtered_more_then_median <- (subset(dataas, count > 20))
weights <- filtered_more_then_median[,3]
net <- filtered_more_then_median[,1:2]

eAttr <- data.frame(dataas$count)
nAttr <- as.vector(forum_data[,!names(forum_data) %in% c("Forum", "Eindtotaal")])

# newMembers <- nAttr$New.member
# activeMember <- nAttr$Active.member
# formerMember <- nAttr$Former.member
# member <- nAttr$Member
# wellKnownMember <- nAttr$Well.known.member
# wellKnownMember <- nAttr$Well.known.member
noMessages <- nAttr$Number.Of.Messages
mortality <- dr_data$Mortality
avg_diagnose_age <- dr_data$Average.age.of.diagnosis


EdgeList <- snafun::make_edgelist(net)
NodeList <- snafun::make_nodelist(net)

surveynet <- igraph::graph_from_data_frame(
            EdgeList, 
            NodeList, 
            directed=FALSE)

# surveynet <- surveynet %>%
#   set_vertex_attr("new_members", value = newMembers)
# surveynet <- surveynet %>%
#   set_vertex_attr("active_members", value = activeMember)
# surveynet <- surveynet %>%
#   set_vertex_attr("former_members", value = formerMember)
# surveynet <- surveynet %>%
#   set_vertex_attr("members", value = member)
# surveynet <- surveynet %>%
#   set_vertex_attr("well_known_members", value = wellKnownMember)

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
surveynet

matrix_path <- '/Users/jonathan/Documents/JADS/year-1/social-network-analysis/sna-research-assignment/data/mental_disease_matrix.xlsx'
matrix_table <- read_excel(matrix_path)
matrix <- data.matrix(matrix_table)

mental_disease_comorbid_net <- snafun::to_network(matrix)
mental_disease_comorbid_net
mental_disease_fora_net <- to_network(surveynet)
mental_disease_fora_net

#How the networks correlate
snafun::g_correlation(
  mental_disease_comorbid_net, 
  mental_disease_fora_net)

menCor <- sna::qaptest(list(mental_disease_comorbid_net, mental_disease_fora_net), 
                       FUN = snafun::g_correlation, reps = 10000)
summary(menCor)
sna::plot.qaptest(menCor)


# ANSWERS THE PROB QUESTION
# abs_active_members <- snafun::make_matrix_from_vertex_attribute(surveynet, 
#                                                           name = "active_members", measure = "absdiff")
# abs_new_members <- snafun::make_matrix_from_vertex_attribute(surveynet, 
#                                                           name = "new_members", measure = "absdiff")
# abs_well_known <- snafun::make_matrix_from_vertex_attribute(surveynet, 
#                                                           name = "well_known_members", measure = "absdiff")
# abs_former_members <- snafun::make_matrix_from_vertex_attribute(surveynet, 
#                                                           name = "former_members", measure = "absdiff")
# abs_members <- snafun::make_matrix_from_vertex_attribute(surveynet, 
#                                                           name = "members", measure = "absdiff")
abs_no_messages <- snafun::make_matrix_from_vertex_attribute(surveynet, 
                                                         name = "no_messages", measure = "absdiff")
abs_mort <- snafun::make_matrix_from_vertex_attribute(surveynet, 
                                                      name = "mortality", measure = "absdiff")
abs_diagnose_age <- snafun::make_matrix_from_vertex_attribute(surveynet, 
                                                      name = "avg_diagnose_age", measure = "absdiff")

w_all <- sna::netlogit(mental_disease_fora_net, 
                           list(mental_disease_comorbid_net, abs_no_messages, abs_mort, abs_diagnose_age),
                           nullhyp = "qapspp", reps = 1001)
w_all$names <- c(c("Intcpt", "Comorbidity", "abs_no_messages", "abs_mort", "abs_diagnose_age"))
summary(w_all)

w_1 <- sna::netlogit(mental_disease_fora_net, 
                       list(mental_disease_comorbid_net, abs_no_messages, abs_mort),
                       nullhyp = "qapspp", reps = 1001)
w_1$names <- c(c("Intcpt", "Comorbidity", "abs_no_messages", "abs_mort"))
summary(w_1)

w_2 <- sna::netlogit(mental_disease_fora_net, 
                           list(mental_disease_comorbid_net, abs_no_messages),
                           nullhyp = "qapspp", reps = 1001)
w_2$names <- c(c("Intcpt", "Comorbidity", "abs_no_messages"))
summary(w_2)

w_0 <- sna::netlogit(mental_disease_fora_net, 
                     list(mental_disease_comorbid_net),
                     nullhyp = "qapspp", reps = 1001)
w_0$names <- c(c("Intcpt", "Comorbidity"))
summary(w_0)


texreg::screenreg(list(w_all, w_1, w_2, w_0))
w_2



# Diseases were users interact wintin these 2 fora's tend 

# Diseases where users interact within these fora's tend 
# to (do not) do comorbid between these diseases.
