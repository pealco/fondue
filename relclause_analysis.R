library(ggplot2)

MAX.SD = 2.5

round.to.nearest = function(x, p) {
	d = as.integer(x/p)
	if (x %% p != 0) {
		s = sign(d)
		a = abs(d) + 1
		d = s*a
	}
	return(d*p)
}

get.ci = function(v) {
  return(t.test(v)$conf.int)
}

error.bar = function(x, upper, lower=upper, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

trim.outliers = function(data) {
  means = tapply(data$RT, data$Region[drop=TRUE], mean)
  stds = sqrt(tapply(data$RT, data$Region[drop=TRUE], var))
  mins = means - MAX.SD * (stds)
  mins.full = mins[as.character(data$Region)]
  maxs = means + MAX.SD * (stds)
  maxs.full = maxs[as.character(data$Region)]
  data.cor = data[data$RT >= mins.full & data$RT <= maxs.full,]
  return(data.cor)
}

compute.stats = function(data) {
  means = tapply(data$RT, data$Region [drop=TRUE], mean)
  mins = tapply(data$RT, data$Region [drop=TRUE], min)
  maxs = tapply(data$RT, data$Region [drop=TRUE], max)
  stds = sqrt(tapply(data$RT, data$Region [drop=TRUE], var))
  rtns = sqrt(tapply(data$RT, data$Region [drop=TRUE], length))
  sems = stds/rtns
  seml = means - sems
  semu = means + sems
  cis  = tapply(data$RT, data$Region [drop=TRUE], get.ci)
  cil  = sapply(cis, function(v) {v[1]})
  ciu  = sapply(cis, function(v) {v[2]})
  stats = data.frame(means, mins, maxs, cil, ciu, seml, semu)
  names(stats) = c("Mean", "Min", "Max", "95%tlow", "95%tupp", "-SEM", "+SEM")
  return(stats)
}

##############################################################################
#begin processing
##############################################################################

#Read items file
items = read.table("fondue.itm", sep=":")
names(items) = c("Subject", "Order", "Trial_type", "Item_number","Condition")

#Read data file
fondue = read.table("fondue.dat", quote="'")
names(fondue) = c("Subject", "Trial_type", "Item_number", "Condition","Position", "Word", "Region", "RT")

#Exclude subjects with overall accuracy less than 80%  --> this excludes subject 28

	#for this I'll create fondue.q, which is the dataframe that has only the question lines for each subject. There
	#are 10.399 lines (FIX: it should be 10.400: 40 subjects X 260 sentences). Due to the way Linger stores the 
	#data, for question lines the column "Region", which usually has the region names ("01v1", "det1", etc.) 
	#has only 0 and 1, which represent the accuracy each sentence

#Exclude practice items (they shouldn't count for accuracy purposes)
fondue.p = subset(fondue, Trial_type == "practice")
fondue   = subset(fondue, Trial_type != "practice") 

#Create a dataframe with only questions
fondue.q = subset(fondue, Position == "?")
	
fondue.q$Region = factor(fondue.q$Region)   #this keeps only the 0 and 1 as levels
									         #(and not the other region names)

fondue.q$Accuracy = as.numeric(as.character(fondue.q$Region))       #as.character is necessary before 
																	 #turning it into a numeric variable

accuracy.subj = ddply(fondue.q, .(Subject), summarize, accuracy_mean = mean(Accuracy))   # gives the accuracy score betweenn 0-1

include = accuracy.subj$accuracy_mean >= 0.8      			#this has either T or F, T for the sujects with accuracy equal or 
												#higher than 80% . However, I need the actual subject numbers so...
print("Overall Accuracy")
print(mean(fondue.q$Accuracy))
 
subjects = accuracy.subj$Subject 	#vector with ALL the subjects numbers

include.subj = subjects[include] 				#This is a vector with the numbers of the subjects I want to include 


fondue.exc = subset(fondue, Subject %in% include.subj)   				 #I have to use %in% instead of "==" because 
																		 #the 2 vectors are of different length 
																		 #and the length of the second vector is 
																		 #different than 1						
																													#Get the test items for RELCLAUSE
relclause.items = subset(items, Trial_type == "RELCLAUSE")
relclause = subset(fondue.exc, Trial_type == "RELCLAUSE")

#Add order column
relclause = merge(relclause, relclause.items)

#Exclude the RT for the questions
relclause.nq = subset(relclause, Position != "?")

#Rename/merge the labels "02atti"& "02atta" to "02att"
relclause.nq$Region = as.character(relclause.nq$Region)
relclause.nq[relclause.nq$Region == "02atti" | relclause.nq$Region == "02atta" ,]$Region = "02att"
relclause.nq$Region = factor(relclause.nq$Region)

#Add useful columns to describe conditions: Grammaticality, Number and Animacy
relclause.nq$Gram    = with(relclause.nq, Condition == "c" | Condition == "d" | Condition == "g" | Condition == "h")
relclause.nq$Plural  = with(relclause.nq, Condition == "b" | Condition == "d" | Condition == "f" | Condition == "h")
relclause.nq$Animate = with(relclause.nq, Condition == "e" | Condition == "f" | Condition == "g" | Condition == "h")

										  
#Add gender column to code for the relationship betweem the two NPs
item.to.gmatch = c("gmismatch","gmismatch","gmismatch","gmatch","gmismatch",
					"gmatch","gmismatch","gmismatch",	"gmismatch","gmismatch",
					"gmatch","gmatch","gmismatch","gmismatch","gmatch","gmatch",
					"gmatch","gmismatch","gmismatch","gmismatch","gmismatch",
					"gmismatch","gmatch","gmatch","gmatch","gmatch","gmismatch",
					"gmismatch","gmatch","gmismatch","gmismatch","gmatch","gmatch",
					"gmatch","gmatch","gmatch","gmatch","gmatch","gmismatch",
					"gmatch","gmismatch","gmatch","gmismatch","gmatch","gmismatch",
					"gmismatch","gmatch","gmismatch")
relclause.nq$GMatch = item.to.gmatch[relclause.nq$Item_number]  #this column says whether the 2 NPs
																 #matched/mismatched in gender

gender = c("mf","mf","fm","mm","fm","mm","fm","fm","mf","fm","mm","mm","fm","fm",
			"ff","mm","ff","fm","mf","fm","fm","fm","mm","ff","mm","ff","fm","fm",
			"mm","fm","mf","mm","mm","mm","mm","mm","mm","mm","mf","ff","fm","mm",
			"mf","ff","fm","fm","mm","mf")

relclause.nq$Gender = gender[relclause.nq$Item_number]  #this column say which was the gender for the 2 NPs
														 # (e.g. mf= masculine-feminine)
										  
#Add further useful columns
relclause.nq$Length = nchar(as.character(relclause.nq$Word))

#Exclude sentences with error responses
relclause.q = subset(relclause, Position == "?")
excluded.trials = which(relclause.q$Region == 0)
subj.sno.full = list()

for (i in 1:length(excluded.trials)) {
	t = excluded.trials[i]
	subj.sno.full[[i]] = unlist(c(relclause.q[t,c("Subject", "Item_number")]))
	}
exclude.condl = rep(FALSE, nrow(relclause.nq))

for (ssn in subj.sno.full) {
	subj = ssn[1]
	sno = ssn[2]
	exclude.condl = exclude.condl |
					(relclause.nq$Subject == subj & relclause.nq$Item_number == sno)
	}

relclause.nq = relclause.nq[!exclude.condl,]  #THIS IS THE FINAL DATAFRAME

															 
#Exclude RTs bigger than 2 s
#relclause.nq = relclause.nq[relclause.nq$RT < 2000, ] 


#Pull off subject means
#for (s in unique(relclause.nq$Subject)) {
#  rts = relclause.nq[relclause.nq$Subject == s, "RT"]
#  mean = mean(rts)
#  relclause.nq[relclause.nq$Subject == s,"RT"] = rts - mean
#}

#Regress out nuisance factors
# m = lm(RT ~ Length*Order, data=relclause.nq)
# relclause.nq$RT = resid(m)

#select only the regions I want
regions = c("01det1", "02att", "03que", "04det2", "05suj", "06v1", "07prep", "08det3", "09noun")

#Separate the ANIMACY manipulation & calculate RTs for each condition within each animacy condition
anim.val = list(T, F, c(T, F))
anim.str = c("Animate", "Inanimate", "Pooled")

relclause.c = subset(relclause.nq, Region %in% regions)
relclause.c$Region = relclause.c$Region[drop=T]

#locate each condition

sg.gram   = trim.outliers(subset(relclause.c, Gram == T & Plural == F))	
sg.ungram = trim.outliers(subset(relclause.c, Gram == F & Plural == F))
pl.gram   = trim.outliers(subset(relclause.c, Gram == T & Plural == T))
pl.ungram = trim.outliers(subset(relclause.c, Gram == F & Plural == T))

all = rbind(sg.gram, sg.ungram, pl.gram, pl.ungram)

group.cond = data.frame(GroupedCondition = c("SG Gram", "SG Ungram", "PL Gram", "PL Ungram", "SG Gram", "SG Ungram", "PL Gram", "PL Ungram"), 
                        Condition = c("c", "a", "d", "b", "g", "e", "h", "f"))
                        
gram.cond = data.frame(gram = c("gram", "ungram", "gram", "ungram", "gram", "ungram", "gram", "ungram"), 
                        Condition = c("c", "a", "d", "b", "g", "e", "h", "f"))

number.cond = data.frame(number = c("SG", "SG", "PL", "PL", "SG", "SG", "PL", "PL"), 
                        Condition = c("c", "a", "d", "b", "g", "e", "h", "f"))
                        

all = merge(all, group.cond)
all = merge(all, gram.cond)
all = merge(all, number.cond)

data = ddply(all, .(Region, GroupedCondition, gram, number), summarize, mean_rt = mean(RT), se = sd(RT)/sqrt(length(Subject)))

write.csv(data, "relclause.plottable.csv")

colors = c("PL Gram" = "black", "PL Ungram" = "red", "SG Gram" = "black", "SG Ungram" = "red")
linetypes = c("PL Gram" = "dashed", "PL Ungram" = "dashed", "SG Gram" = "solid", "SG Ungram" = "solid")
shapes = c("PL Gram" = 1, "PL Ungram" = 1, "SG Gram" = 16, "SG Ungram" = 16)
limits = aes(ymax = mean_rt + se, ymin = mean_rt - se)

ggplot(data, aes(x=Region, y=mean_rt, color=GroupedCondition, shape=GroupedCondition)) + 
    geom_line(aes(group=GroupedCondition, linetype=GroupedCondition)) +
    geom_point() +
    geom_errorbar(limits, width=0.2, alpha=I(0.4)) +
    scale_color_manual("Condition", value = colors) + 
    scale_linetype_manual("Condition", value = linetypes) +
    scale_shape_manual("Condition", value=shapes) +
    scale_x_discrete(breaks=regions, labels=c("DET", "NP", "que", "DET", "NP", "V", "PP", "DET", "NP")) +
    labs(x="", y="") +
    theme_bw() +
    opts(legend.position="none")

ggplot(data, aes(x=Region, y=mean_rt, color=GroupedCondition, linetype=GroupedCondition, shape=GroupedCondition)) + 
    geom_line(aes(group=GroupedCondition)) +
    geom_point() +
    geom_errorbar(limits, width=0.2) +
    scale_color_manual("Condition", value = colors) + 
    scale_linetype_manual("Condition", value = linetypes) +
    scale_shape_manual("Condition", value=shapes) +
    scale_x_discrete(breaks=regions, labels=c("DET", "NP", "que", "DET", "NP", "V", "PP", "DET", "NP")) +
    labs(x="", y="") +
    theme_bw()