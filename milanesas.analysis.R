library(lme4)
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


attach(data)

	# Data cleanup

    filt.all            = which(rt > 50 & rt < 2000)
    filt.fillers        = which(rt > 50 & rt < 2000 & experiment == "filler")
    filt.relclause      = which(rt > 50 & rt < 2000 & experiment == "RELCLAUSE")
    filt.relclause.verb = which(rt > 50 & rt < 2000 & experiment == "RELCLAUSE" & verbtype == "verb")
    filt.relclause.aux  = which(rt > 50 & rt < 2000 & experiment == "RELCLAUSE" & verbtype == "aux")
    
    data.all = data[filt.all, ]
    data.fillers = data[filt.fillers, ]
    
    data.relclause = data[filt.relclause,]
    
        data.relclause$condition      = data.relclause$condition[drop=T]
        data.relclause$resp           = data.relclause$resp[drop=T]
    
    data.relclause.verb = data[filt.relclause.verb,]

        data.relclause.verb$condition = data.relclause.verb$condition[drop=T]
        data.relclause.verb$resp      = data.relclause.verb$resp[drop=T]
        
    data.relclause.aux  = data[filt.relclause.aux, ]

        data.relclause.aux$condition  = data.relclause.aux$condition[drop=T]
        data.relclause.aux$resp       = data.relclause.aux$resp[drop=T]

detach(data)

##### RELCLAUSE VERB barplot

pdf ("Agreement results.pdf")
#par(mfrow=c(2,1))

xtab.relclause.verb = xtabs(formula = ~ resp + gram + number, data = data.relclause.verb)    #why formula?  
																							 # for resp, "N"=1 & "Y"=2
stats.verb <-t(xtab.relclause.verb[2,,]/apply(xtab.relclause.verb,2:3,sum))

print ("Stats Main Verb")
print(stats.verb*100)

barplot(t(xtab.relclause.verb[2,,]/apply(xtab.relclause.verb,2:3,sum))*100, ylim=c(0,100), beside=T, bty="n",
    main        = "RC with Main Verb", 
    legend.text = c("Singular attractor",  "Plural attractor"), 
    ylab        = "Proportion 'Y' responses", 
    names       = c("Grammatical",  "Ungrammatical")
		)
axis(2, seq(0, 100, 10))

##### RELCLAUSE AUX barplot

xtab.relclause.aux = xtabs(formula = ~ resp + gram + number,  data = data.relclause.aux)     
stats.aux <- t(xtab.relclause.aux[2,,]/apply(xtab.relclause.aux,2:3,sum))

print ("Stats Auxiliary")
print(stats.aux*100)

barplot(t(xtab.relclause.aux[2,,]/apply(xtab.relclause.aux,2:3,sum))*100, ylim=c(0,100), beside=T, bty="n",
    main        = "RC with Auxiliary", 
    legend.text = c("Singular attractor",  "Plural attractor"), 
    ylab        = "Proportion 'Y' responses", 
    names       = c("Grammatical",  "Ungrammatical")
	)
axis(2, seq(0, 100, 10))

dev.off()

##### Data summary   

length(data.all[,1])/length(data[,1]) #Data remaining after filtering

summary(data.all)


##### The models

lm.relclause        = lmer(resp ~ gram/number + (1|subject) + (1|item), data = data.relclause,      family="binomial")
lm.relclause.verb   = lmer(resp ~ gram/number + (1|subject) + (1|item), data = data.relclause.verb, family="binomial")
lm.relclause.aux    = lmer(resp ~ gram/number + (1|subject) + (1|item), data = data.relclause.aux,  family="binomial")

summary(lm.relclause)
summary(lm.relclause.verb)
summary(lm.relclause.aux)