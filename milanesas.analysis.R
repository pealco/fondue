#library(lme4)
library(ggplot2)

import.data = function(filename) {
	buffer  = read.table(filename);
	rawdata = data.frame(
		subject		= factor(buffer$V1),
		experiment	= factor(buffer$V2),
		item	   	= factor(buffer$V3),
		condition	= factor(buffer$V4),
		resp		= factor(buffer$V6),
		acc			= factor(buffer$V7),
		rt			=       (buffer$V8),
		gram        = rep(NA, length(buffer$V1)),
		number      = rep(NA, length(buffer$V1)),
		verbtype    = rep(NA, length(buffer$V1))
	)
	return(rawdata)
} 

summary(data <- import.data("milanesas.txt")) 

#### Recoding the data; expanding the conditions.

cond.gram   = which(data$condition == "c" | data$condition == "d" | data$condition == "g" | data$condition == "h")
cond.ungram = which(data$condition == "a" | data$condition == "b" | data$condition == "e" | data$condition == "f")
cond.sg     = which(data$condition == "a" | data$condition == "c" | data$condition == "e" | data$condition == "g")
cond.pl     = which(data$condition == "b" | data$condition == "d" | data$condition == "f" | data$condition == "h")
cond.verb   = which(data$condition == "a" | data$condition == "b" | data$condition == "c" | data$condition == "d")
cond.aux    = which(data$condition == "e" | data$condition == "f" | data$condition == "g" | data$condition == "h")

data$gram[cond.gram]     = "gram"
data$gram[cond.ungram]   = "ungram"
                    
data$number[cond.sg]     = "a-sg"
data$number[cond.pl]     = "b-pl"
                    
data$verbtype[cond.verb] = "verb"
data$verbtype[cond.aux]  = "aux"

data$gram     = as.factor(data$gram)
data$number   = as.factor(data$number)
data$verbtype = as.factor(data$verbtype)

# Data cleanup
data.relclause.verb = subset(data, rt > 50 & rt < 2000 & experiment == "RELCLAUSE" & verbtype == "verb")

data.relclause.verb$condition = data.relclause.verb$condition[drop=T]
data.relclause.verb$resp      = data.relclause.verb$resp[drop=T]

##### RELCLAUSE VERB barplot

xtab.relclause.verb = xtabs(formula = ~ resp + gram + number, data = data.relclause.verb)
stats.verb = xtab.relclause.verb[2,,]/apply(xtab.relclause.verb,2:3,sum) * 100

stats.verb.melt = melt(stats.verb)

ggplot(stats.verb.melt, aes(gram)) +
geom_bar(aes(y=value, fill=number), position="dodge")  +
scale_fill_grey("Attractor", breaks = c("a-sg", "b-pl"), labels = c("Singular", "Plural")) +
scale_x_discrete("", breaks = c("gram", "ungram"), labels = c("Grammatical", "Ungrammatical")) +
scale_y_continuous("", limits = c(0, 100)) +
coord_cartesian(ylim = c(0,100)) + 
theme_bw() +
opts(legend.position="none")

#opts(panel.background = theme_blank())

##### The models

#lm.relclause.verb   = lmer(resp ~ gram/number + (1|subject) + (1|item), data = data.relclause.verb, family="binomial")
#summary(lm.relclause.verb)
