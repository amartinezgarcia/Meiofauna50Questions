########################################################
# 50 questions. Script for the summary analyses
# Martinez et al.
# v. 1.0 (11.10.2022)
# Last update 14.07.2023
# Uploaded 07.02.2025
########################################################


###### Upload packages ---------------------------------

setwd("/Users/amartinez/Dropbox/_Papers/_READY/submitted - Martinez_50questions_Meiofauna/05 Analyses") # choose yours

library(dplyr)
library(ggplot2)


### Voters and panel members
voters <- read.csv2("Answers_short.csv")
factors <- c("Reached_sorted","Country","Gender",
             "Career_sort","Career_group","Continent")

voters[factors] <- lapply(voters[factors],factor); rm(factors)
panel <- voters[ which (voters$Reached_sorted == "Panel"),]


### Questions
questions <- read.csv2("Questions_scored.csv")
  questions <- questions[order(questions$scores, decreasing = TRUE), ]




###### Gender ration  ---------------------------------

gender <- as.data.frame(table(voters$Gender))
    gender$Freq <- (gender$Freq / sum(gender$Freq))*100
    gender


gender.panel <- as.data.frame(table(voters$Gender))
  gender.panel$freq <- (gender.panel$Freq / sum(gender.panel$Freq))*100

gender.panel

rm(gender, gender.panel)






###### FIGURE 1: DESCRIPTIVE GRAPHS -----------------------------------------------------------------


### Expertise -----------------------------------------
 

cb1 <- voters %>%
  ggplot( aes(x=Expertise, fill="#3c1407")) +
  geom_bar(color="#3c1407", alpha=0.6, width = 0.7) +
  scale_fill_manual(values=c("#3c1407")) +
  theme_classic() +
  labs(fill="") +
  coord_flip() +
  theme(legend.position="none")

exp.graph <- as.data.frame(table(voters$Expertise))
exp.graph$fraction = exp.graph$Freq / sum(exp.graph$Freq)


cb2 <- panel %>%
  ggplot( aes(x=Expertise, fill="#3c1407")) +
  geom_bar(color="#3c1407", alpha=0.6, width = 0.7) +
  scale_fill_manual(values=c("#3c1407")) +
  theme_classic() +
  labs(fill="") +
  coord_flip() +
  theme(legend.position="none")

exp.graph <- as.data.frame(table(panel$Expertise))
exp.graph$fraction = 100*(exp.graph$Freq / sum(exp.graph$Freq))


plot(cb1)
plot(cb2)

rm(cb1,cb2, exp.graph)



### Career -----------------------------------------

voters$Career_group <- factor(voters$Career_group, levels = c("researcher","PostDoc",
                                                          "student","job"))

cb3 <- voters %>%
  ggplot( aes(x=Career_group, fill="#3c1407")) +
  geom_bar(color="#3c1407", alpha=0.6,width = 0.7) +
  scale_fill_manual(values=c("#3c1407")) +
  theme_classic() +
  labs(fill="") +
  coord_flip() +
  theme(legend.position="none")

career.graph <- as.data.frame(table(voters$Career_group))
career.graph$fraction = 100*(career.graph$Freq / sum(career.graph$Freq))

cb4 <- panel %>%
  ggplot( aes(x=Career_group, fill="#3c1407")) +
  geom_bar(color="#3c1407", alpha=0.6,width = 0.7) +
  scale_fill_manual(values=c("#3c1407")) +
  theme_classic() +
  labs(fill="") +
  coord_flip() +
  theme(legend.position="none")

career.graph <- as.data.frame(table(panel$Career_group))
career.graph$fraction = 100*(career.graph$Freq / sum(career.graph$Freq))

plot(cb3)
plot(cb4)

rm(cb3,cb4,career.graph)



### Geography -----------------------------------------

TDWG1 <- sf::st_read("/Users/amartinez/Dropbox/_Papers/_READY/submitted - Martinez_50questions_Meiofauna/05 Analyses/map/level1.shp")

tdwg1r <- as.data.frame(table(voters$Continent))
tdwg1r$percentage <-  100*(tdwg1r$Freq / sum(tdwg1r$Freq))

tdwg1p <- sp::merge(TDWG1, tdwg1r,
                    by.x="LEVEL1_COD", by.y="Var1",
                    all=T, duplicateGeoms=TRUE)

ggplot() +
geom_sf(data = tdwg1p, aes(fill = Freq),
        colour = "black", cex=0.005) +
  scale_fill_distiller("voters", type = "seq",
                       direction = 2, palette = "Oranges", na.value="white") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = "transparent"),
        legend.position="bottom")

rm(TDWG1, tdwg1p, tdwg1r)




### Channels -----------------------------------------

reach <- as.data.frame(table(voters$Reached_sorted))
reach$fraction = reach$Freq / sum(reach$Freq)

# Compute the cumulative percentages (top of each rectangle)
reach$ymax = cumsum(reach$fraction)

# Compute the bottom of each rectangle
reach$ymin = c(0, head(reach$ymax, n=-1))

# Make the plot
ggplot(reach, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  scale_fill_manual(values = alpha(c("#009999","#993D00", "#FFE5CB","#E5FFFF",
                                     "#65FFFF"), 0.8)) +
    theme_void()


rm(reach)




###### FIGURE 1: Box plot ----------------------------------------------------------------------

library(ggplot2)
library(dplyr)

questions <- read.csv2("Questions_scored.csv")
questions <- questions[order(questions$scores, decreasing = TRUE), ]



## Questions per panel
q50 <- questions[c(1:50), ]
table(q50$panel)
table(questions$panel)


## Extract top 5 questions
top5 <- Reduce(rbind,
               by(questions,
                  questions["panel"],
                  head,
                  n = 6))

top5$top.panel <- rep("yes",nrow(top5))
top5 <- top5[c("ID","top.panel")]
questions <- merge(questions,top5, by="ID",all=T)
questions[is.na(questions)] <- "no"


## number words
questions$nwords <- lengths(gregexpr("\\W+", questions$question)) + 1

## readability
readibility <-
  quanteda.textstats::textstat_readability(questions$question,
                                           measure = "Flesch",
                                           remove_hyphens = TRUE,
                                           min_sentence_length = 1,
                                           max_sentence_length = 10000,
                                           intermediate = FALSE)
questions$Flesch <- readibility$Flesch


questions$panel <- factor(questions$panel, levels = c("Systematics","Macro",
                                                      "Morpho","Genome",
                                                      "Climate","Pop",
                                                      "Biogeom","Communication"))
unique(questions$panel)

ggplot(questions,
       aes(x=panel, y=scores)) +
  geom_violin(alpha=0.4, colour = "#d2ac8d") +
  geom_jitter(aes(colour = top.panel,
                  size= nwords, stroke=0.5,
                  alpha = 30), width=0.2) +
  scale_color_manual(values = c("grey80","#d2ac8d")) +
  geom_hline(yintercept=2013, linetype="dashed", color = "grey") +
  theme_classic()

rm(q50,top5)

#### modellig votes

library(glmmTMB)


mod.votes <- glm(scores ~ nwords*Flesch*panel, family = "poisson", data = questions)
summary(mod.votes)
car::Anova(mod.votes)







###### FIGURE 2: Analyses of the confounding factors affecting the votes -------------------------

library(vegan)
library(BAT)


###### Demographics --------------------------------------------------------------


### Dataset

voters1 <- voters[complete.cases(voters),]
voters1 <- voters1[ which(voters1$Gender != "V"),]

polls <- voters1[,c(24:141)]


### Distance matrix

polls.gower <- gower(polls)
euc.pco <- labdsv::pco(polls.gower,k=4)
SUM <- sum(euc.pco$eig[euc.pco$eig > 0])
barplot(euc.pco$eig)
(euc.pco$eig[1]+euc.pco$eig[2]+euc.pco$eig[3]+euc.pco$eig[4])/sum(euc.pco$eig[euc.pco$eig > 0])


dist.polls <- vegdist(polls, method="jaccard")


#### Adonis

mod.demo <- dist.polls ~ Birth + Gender + Continent + Expertise
(perm.demo <- vegan::adonis2(mod.demo, voters1, permutations = 999))


mod.areas <- dist.polls ~ Evolution + Systematics + Ecology + Morphology +
  Geochemistry + Microbiology + Molecular + Conservation + Education

(perm.areas <- vegan::adonis2(mod.areas, voters1, permutations = 999))


mod.all <- dist.polls ~ Birth + Gender + Continent + Expertise +
                        Evolution + Systematics + Ecology + Morphology +
                          Geochemistry + Microbiology + Molecular + Conservation + Education

(perm.all <- vegan::adonis2(mod.all, voters1, permutations = 999))

rm(mod.demo,perm.demo,mod.areas,perm.areas, euc.pco,SUM, mod.all, perm.all)



#### RDA demographics (expertise, birth, gender)

demographic <- voters1[c("Expertise", "Birth", "Gender", "Continent")]

rda.demo <- vegan::rda(polls ~ ., data = demographic)
summary(rda.demo)

perc.demo <- round((summary(rda.demo)$cont$importance[2, 1:2]), 2)
 sc_si <- scale(scores(rda.demo, display="sites", choices=c(1,2), scaling=2))
 sc_bp <- scale(scores(rda.demo, display="bp", choices=c(1, 2), scaling=2))

plot(rda.demo,
     scaling = 2, # set scaling type
     type = "none", # this excludes the plotting of any points from the results
     frame = FALSE,
     # set axis limits
     xlim = c(-4,4),
     ylim = c(-4,4),
     xlab = paste0("RDA1 (", perc.demo[1], "%)"),
     ylab = paste0("RDA2 (", perc.demo[2], "%)")
)

points(sc_si, pch = 21, col = "#3c1407", bg = "#d2ac8d", cex = 0.5)
  arrows(0,0, sc_bp[,1], sc_bp[,2], col = "#3c1407",code = 2, angle = 15, lwd = 2)

# add text labels for arrows
text(x = sc_bp[,1] + 0.15, y = sc_bp[,2] + 0.03,
     labels = rownames(sc_bp),
     col = "#3c1407", cex = 1,  font = 0.5)

rm(rda.demo,perc.demo,sc_si,sc_bp)



### RDA research areas background (evolution, ecology...)

areas <- voters1[c("Evolution",
                  "Ecology",
                  "Systematics",
                  "Morphology",
                  "Geochemistry",
                  "Microbiology",
                  "Molecular",
                  "Conservation",
                  "Education")]

rda.areas <- vegan::rda(polls ~ ., data = areas)

perc.areas <- round(100*(summary(rda.areas)$cont$importance[2, 1:2]), 2)
      sc_si1 <- scores(rda.areas, display="sites", choices=c(1,2), scaling=2)
      sc_bp1 <- scores(rda.areas, display="bp", choices=c(1, 2), scaling=2)
      
plot(rda.areas, scaling = 2, type = "none", frame = FALSE,
     xlim = c(-7,14),ylim = c(-7,14),
     xlab = paste0("RDA1 (", perc.areas[1], "%)"),
     ylab = paste0("RDA2 (", perc.areas[2], "%)"))

points(sc_si1, pch = 21, col = "#3c1407", bg = "#d2ac8d", cex = 0.5, alpha = 0.2)


arrows(0,0, sc_bp1[,1], sc_bp1[,2], col = "#3c1407",code = 2,
       angle = 15, lwd = 2)


text(x = sc_bp1[,1] + 0.15, # adjust text coordinate to avoid overlap with arrow tip
     y = sc_bp1[,2] + 0.03,
     labels = rownames(sc_bp1),
     col = "#3c1407", cex = 0.5,  font = 0.5)

rm(rda.areas,perc.areas,member0,sc_bp1,sc_si1)




##### Chord Diagram connecting areas with panels

questions <- read.csv2("Questions_scored.csv")
panel.names <- as.vector(unique(questions$panel))

areas <- voters1[c("Evolution",
                       "Ecology",
                       "Systematics",
                       "Morphology",
                       "Geochemistry",
                       "Microbiology",
                       "Molecular",
                       "Conservation",
                       "Education")]
expertises <- as.vector(colnames(areas))


network <- data.frame()
for (k in 1:length(expertises)){
  vote.expert <-  voters1[, c(k+13, 24:141)]
  vote.expert <- vote.expert[ which (vote.expert[,1] == 1), c(2:ncol(vote.expert)) ]

      cat0 <- data.frame()
      for (i in 1:length(panel.names)){
        panel <- questions[ which (questions$panel == panel.names[i]),]
        cat <- sum(vote.expert[colnames(vote.expert) %in% panel$ID])
        cat <- as.data.frame(cbind(panel.names[i],cat))
        cat0 <- rbind(cat0,cat)
      }
    #member <- cbind(cat0,rep(expertises[k],length(panel.names)))
    cat2 <-as.data.frame(t(cat0), row.names = F)
    member <- cbind(cat2[2,],expertises[k])
    network <- rbind(network,member)
}
rm(member,cat,cat0,cat2)

colnames(network) <- c(panel.names,"exper")
row.names(network) <- network$exper
network <- network[,-9]

network <- as.data.frame(lapply(network,as.numeric))

network <- as.matrix(network)
row.names(network) <-c("aEvolution","aEcology","aSystematics","aMorphology",
                        "aGeochemistry","aMicrobiology","aMolecular","aConservation",
                        "aEducation")


library(circlize)

grid.col = c(aEvolution = "#800000",aEcology = "#767676",
              aSystematics = "#FFA319",aMorphology = "#8A9045",
              aGeochemistry = "#155F83",aMicrobiology = "#C16622",
              aMolecular = "#8F3931",aConservation = "#58593F",
              aEducation = "#350E20", Climate = "black",
              Communication = "black", Macro = "black",
              Biogeom = "black", Systematics = "black",
              Pop = "black", Morpho = "black",
              Genome = "black")


p1 <- chordDiagramFromMatrix(network, transparency = 0.7, big.gap = 5,
             grid.col = grid.col, scale = T)
circos.clear()




exp.tot <- as.data.frame(colSums(areas))
colnames(exp.tot) <- c("Freq")

exp.tot$fraction = exp.tot$Freq / sum(exp.tot$Freq)
exp.tot$Var1 = row.names(exp.tot)

exp.tot$ymax = cumsum(exp.tot$fraction)
exp.tot$ymin = c(0, head(exp.tot$ymax, n=-1))

exp.tot$Var1<- factor(exp.tot$Var1, levels = c("Evolution","Ecology",
                                               "Systematics","Morphology",
                                               "Geochemistry","Microbiology",
                                               "Molecular","Conservation",
                                               "Education"))

ggplot(exp.tot, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1),) +
  geom_rect(stat="identity") +
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("#800000","#767676",
                                     "#FFA319","#8A9045",
                                     "#155F83","#C16622",
                                     "#8F3931","#58593F",
                                     "#350E20")) +
  theme_void()


rm(exp.tot,p1,vote.expert,network,grid.col,i,k,panel.names)










###################################
# Confounding factors
###################################

library(performance)
library(modEvA)
library(lme4)
library(sp)
library(nlme)
library(car)
library(MuMIn)
library(reshape2)
library(dplyr)
library(MASS)


boxplot(questions$score ~ questions$panel)
boxplot(questions$nwords ~ questions$panel)
boxplot(questions$Flesch ~ questions$panel)


plot(questions$scores ~ questions$nwords)
plot(questions$Flesch ~ questions$nwords)
plot(questions$scores ~ questions$Flesch)



model <- glm.nb(scores ~ nwords*Flesch*panel,
                data = questions)

Anova(model)
summary(model)
performance::check_model(model)


library(emmeans) # v. 1.7.0
library(magrittr) # v. 2.0.1

emm1 = emmeans(model, specs = pairwise ~ panel)
emm1$emmeans
emm1$contrasts

