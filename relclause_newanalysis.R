# updated 3.9.11

library(lme4)

MAX.SD = 2.5;

round.to.nearest = function(x, p) {
	d = as.integer(x/p);
	if (x %% p != 0) {
		s = sign(d);
		a = abs(d) + 1;
		d = s*a;
	}
	return(d*p);
}

get.ci = function(v) {
  return(t.test(v)$conf.int);
}

error.bar = function(x, upper, lower=upper, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

trim.outliers = function(data, by) {
  f = rep("", nrow(data))
  for (v in by) {
    f = paste(f, as.character(v))
  }
  f = as.factor(f)
  means = tapply(data$RT, f, mean);
  stds = sqrt(tapply(data$RT, f, var));
  mins = means - MAX.SD*(stds);
  mins.full = mins[as.character(f)];
  maxs = means + MAX.SD*(stds);
  maxs.full = maxs[as.character(f)];
  data.cor = data[data$RT >= mins.full & data$RT <= maxs.full,];
  return(data.cor);
}

compute.stats = function(data) {
  means = tapply(data$RT, data$Region [drop=TRUE], mean);
  mins = tapply(data$RT, data$Region [drop=TRUE], min);
  maxs = tapply(data$RT, data$Region [drop=TRUE], max);
  stds = sqrt(tapply(data$RT, data$Region [drop=TRUE], var));
  rtns = sqrt(tapply(data$RT, data$Region [drop=TRUE], length));
  sems = stds/rtns;
  seml = means - sems;
  semu = means + sems;
  cis = tapply(data$RT, data$Region [drop=TRUE], get.ci)
  cil = sapply(cis, function(v) {v[1]});
  ciu = sapply(cis, function(v) {v[2]});
  stats = data.frame(means, mins, maxs, cil, ciu, seml, semu);
  names(stats) = c("Mean", "Min", "Max", "95%tlow", "95%tupp", "-SEM", "+SEM");
  return(stats);
}

##############################################################################
# Begin processing
##############################################################################

# Read items file
items = read.table("fondue.itm", sep=":");
names(items) = c("Subject", "Order", "Trial_type", "Item_number","Condition");

# Read data file
fondue = read.table("fondue.txt", quote="'")
names(fondue) = c("Subject", "Trial_type", "Item_number", "Condition","Position", "Word", "Region", "RT");

# Make subject and item factors
items$Subject      = as.factor(paste("S", items$Subject, sep=''))
fondue$Subject     = as.factor(paste("S", fondue$Subject, sep=''))
items$Item_number  = as.factor(paste("I", items$Item_number, sep=''))
fondue$Item_number = as.factor(paste("I", fondue$Item_number, sep=''))

#Exclude subjects with overall accuracy less than 80%  --> this excludes subject 28

  #for this I'll create fondue.q, which is the dataframe that has only the
  #question lines for each subject. There are 10.399 lines (FIX: it should be
  #10.400: 40 subjects X 260 sentences). Due to the way Linger stores the data,
  #for question lines the column "Region", which usually has the region names
  #("01v1", "det1", etc.) has only 0 and 1, which represent the accuracy each
  #sentence

#Exclude practice items (they shouldn't count for accuracy purposes)
fondue.p = fondue[fondue$Trial_type == "practice", ]  	#nrow(fondue.p)=1720, why?
fondue = fondue[fondue$Trial_type != "practice" , ] 

#Create a dataframe with only questions
fondue.q = fondue[fondue$Position == "?",]  #nrow(fondue.q)=10.399 why?
	
fondue.q$Region = factor(fondue.q$Region)   	#this keeps only the 0 and 1 as levels
												#(and not the other region names)

fondue.q$Accuracy = as.numeric(as.character(fondue.q$Region))
									#as.character is necessary before 
									#turning it into a numeric variable

accuracy.subj = tapply(fondue.q$Accuracy, fondue.q$Subject, mean)		#it gives the accuracy score between 0-1
												

include = accuracy.subj >= 0.8      			#this has either T or F, T for the sujects
												#with accuracy equal or higher than 80% .
												#However, I need the actual subject numbers so...
print( "Overall Accuracy")
print (mean (fondue.q$Accuracy))
 
subjects = names(accuracy.subj) 	#vector with ALL the subjects numbers

include.subj = subjects[include] 			#This is a vector with the names of the
											#subjects I want to include 


fondue.exc = fondue[fondue$Subject %in% include.subj, ]
																		#I have to use %in% instead of "==" because 
																		#the 2 vectors are of different length 
																		#and the length of the second vector is 
																		#different than 1						
#Get the test items for RELCLAUSE
relclause.items = items [items$Trial_type == "RELCLAUSE", ]
relclause = fondue.exc [fondue.exc$Trial_type == "RELCLAUSE", ]

#Add order column
relclause$Order = rep(0, nrow(relclause));
ssno = unique(relclause[,c("Subject", "Item_number")]);
for (i in 1:nrow(ssno)) {
  subj = as.character(ssno[i, "Subject"]);
  sno = as.character(ssno[i, "Item_number"]);
  relclause[relclause$Subject==subj & relclause$Item_number==sno, "Order"] <-
	  relclause.items[relclause.items$Subject == subj & relclause.items$Item_number == sno, "Order"];
}

#Exclude the RT for the questions
relclause.nq = relclause [relclause$Position != "?",]

#Rename/merge the labels "02atti"& "02atta" to "02att"
relclause.nq$Region = as.character(relclause.nq$Region)
relclause.nq[relclause.nq$Region == "02atti" | relclause.nq$Region == "02atta" ,]$Region = "02att"
relclause.nq$Region = factor(relclause.nq$Region)
  
#Add useful columns to describe conditions: Grammaticality, Number and Animacy
relclause.nq$Gram <-with(relclause.nq, Condition == "c" | Condition == "d" |
                                       Condition == "g" | Condition == "h");
relclause.nq$Plural <-with(relclause.nq, Condition == "b" | Condition == "d" |
                                         Condition == "f" | Condition == "h");
relclause.nq$Animate <-with(relclause.nq, Condition == "e" | Condition == "f" |
                                          Condition == "g" | Condition == "h");

										  
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
					
inum.index = as.numeric(substr(as.character(relclause.nq$Item_number), 2,
                                nchar(as.character(relclause.nq$Item_number))))
relclause.nq$GMatch = as.factor(item.to.gmatch[inum.index])
																#this column says whether the 2 NPs
																#matched/mismatched in gender

gender = c("mf","mf","fm","mm","fm","mm","fm","fm","mf","fm","mm","mm","fm","fm",
			"ff","mm","ff","fm","mf","fm","fm","fm","mm","ff","mm","ff","fm","fm",
			"mm","fm","mf","mm","mm","mm","mm","mm","mm","mm","mf","ff","fm","mm",
			"mf","ff","fm","fm","mm","mf")

relclause.nq$Gender = as.factor(gender[inum.index])
															#this column say which was the gender for the 2 NPs
															# (e.g. mf= masculine-feminine)
										  
#Add further useful columns
relclause.nq$Length = nchar(as.character(relclause.nq$Word));

#Exclude sentences with error responses
relclause.q = relclause [relclause$Position == "?",]
excluded.trials = which(relclause.q$Region==0);
subj.sno.full = list();


	for (i in 1:length(excluded.trials)) {
		t = excluded.trials[i];
		subj.sno.full[[i]] = as.character(unlist(c(relclause.q[t,c("Subject", "Item_number")])));
		}
	exclude.condl = rep(FALSE, nrow(relclause.nq));

	for (ssn in subj.sno.full) {
		subj = ssn[1];
		sno = ssn[2];
		exclude.condl = exclude.condl |
						(relclause.nq$Subject == subj & relclause.nq$Item_number == sno);
		}

relclause.nq = relclause.nq[!exclude.condl,]

relclause.nq$RT = log(relclause.nq$RT)
relclause.nq = trim.outliers(relclause.nq, list(relclause.nq$Region,
                                                 relclause.nq$Condition)) #THIS IS THE FINAL DATAFRAME
												 												 

##############################################################################
# Calculate stats
##############################################################################

# Calculate ANOVAs for ROI  
models = list()
anovas = list()
for (r in c("05suj", "06v1", "07prep", "08det3", "09noun",
            "10v2", "11v3", "end1")) {
  d = relclause.nq[relclause.nq$Region==r,]
  m1 = lmer(RT ~ 1 + (1|Subject) + (1|Item_number), data=d)
  m2 = lmer(RT ~ Gram+Plural+Animate + (1|Subject) + (1|Item_number), data=d)
  m3 = lmer(RT ~ Gram*Plural+Animate + (1|Subject) + (1|Item_number), data=d)
  m4 = lmer(RT ~ Gram*Plural+Gram*Animate+Plural*Animate + (1|Subject) + (1|Item_number), data=d)
  m5 = lmer(RT ~ Gram*Plural*Animate + (1|Subject) + (1|Item_number), data=d)
  models[[r]] = m5
  anovas[[r]] = anova(m1, m2, m3, m4, m5)
}
		

		
#Exclude RTs bigger than 2 s
#relclause.nq = relclause.nq[relclause.nq$RT < 2000, ] 


##Pull off subject means
#for (s in unique(relclause.nq$Subject)) {
#  rts = relclause.nq[relclause.nq$Subject == s, "RT"];
#  mean = mean(rts);
#  relclause.nq[relclause.nq$Subject == s,"RT"] = rts - mean;
#}

#Regress out nuisance factors
# m = lm(RT ~ Length*Order, data=relclause.nq);
# relclause.nq$RT = resid(m);

#select only the regions I want
regions = c("01det1", "02att", "03que", "04det2", "05suj", "06v1", "07prep", "08det3", "09noun", 
			 "10v2", "11v3","end1");

####################################################################################################
#Write tables for QMPE
#NOTE: for each file I need to MANUALLY modify "regions_number.p": (1) change name of input file and 
		#(2) change the name of the output file

#Save condition levels
#I save it as Condition 2 because I'm collapsing 4 conditions into 2 (collapsing animacy)

#relclause.nq$Condition2 = as.factor(paste(as.character(relclause.nq$Gram), 
#											as.character(relclause.nq$Plural), sep=""))
#condition.levels = levels(relclause.nq$Condition2)
#
##name of output files
#relclause.nq$Condition2 = as.numeric(relclause.nq$Condition2);
#relclause.nq = relclause.nq[order(relclause.nq$Condition2),];
#relclause.nq$RT = relclause.nq$RT + 500;  #this adds a constant of 500 ms to the regressed RTs

#write.table(relclause.nq[relclause.nq$Region=="05suj",c("Condition2", "RT")],"relclause_05suj.txt", 
#		row.names=F, col.names=F)
#write.table(relclause.nq[relclause.nq$Region=="06v1",c("Condition2", "RT")],"relclause_06v1.txt", 
#		row.names=F, col.names=F)
#write.table(relclause.nq[relclause.nq$Region=="07prep",c("Condition2", "RT")],"relclause_07prep.txt", 
#		row.names=F, col.names=F)
#####################################################################################################

##Separate the ANIMACY manipulation & calculate RTs for each condition within each animacy condition
#anim.val = list(T, F, c(T, F));
#anim.str = c("Animate", "Inanimate", "Pooled");
#
#all = NULL

#for (i in 1:3) {
#	print(anim.str[i]);
#	relclause.c = relclause.nq[relclause.nq$Animate %in% anim.val[[i]],];
#	
#	#locate each condition
#
#	sg.gram = trim.outliers(relclause.c [relclause.c$Gram == TRUE & relclause.c$Plural == FALSE, ]);	
#	sg.ungram = trim.outliers(relclause.c [relclause.c$Gram == FALSE & relclause.c$Plural == FALSE, ]);
#	pl.gram = trim.outliers(relclause.c [relclause.c$Gram == TRUE & relclause.c$Plural == TRUE, ]);
#	pl.ungram = trim.outliers(relclause.c [relclause.c$Gram == FALSE & relclause.c$Plural == TRUE, ]);
	
  
#	all.a = rbind(sg.gram, sg.ungram, pl.gram, pl.ungram);
#	if (anim.str[i] %in% c("Animate", "Inanimate")) {
#		if (is.null(all)) {
#			all = all.a
#		} else {
#			all = rbind(all, all.a)
#		}
#	}
#	
#	sg.gram.stat = compute.stats(sg.gram);
#	sg.ungram.stat = compute.stats(sg.ungram);
#	pl.gram.stat = compute.stats(pl.gram);
#	pl.ungram.stat = compute.stats(pl.ungram);
  
#	min.y = min(c(sg.gram.stat[["95%tlow"]], sg.ungram.stat[["95%tlow"]],
#                 pl.gram.stat[["95%tlow"]], pl.ungram.stat[["95%tlow"]]));
#	min.y = round.to.nearest(min.y - abs(min.y)*0.1, 50);
#	max.y = max(c(sg.gram.stat[["95%tupp"]], sg.ungram.stat[["95%tupp"]],
#                 pl.gram.stat[["95%tupp"]], pl.ungram.stat[["95%tupp"]]));
#	max.y = round.to.nearest(max.y + abs(max.y)*0.1, 50);

  

	
	# #Set up plot
	# pdf (paste("Mean_RT_.",anim.str[i],".pdf", sep=""));
	
	# #Set up the scale
	# xlimit = c(1,12);
	# ylimit = c(min.y,max.y);

	# plot(NA,xlim=xlimit,ylim=ylimit,xlab="Regions", ylab="Mean RT", xaxt="n",
    # yaxt="n", main=anim.str[i], type="n");

	# #legend(1, max.y, c("Singular, Gram", "Singular, Ungram",
                    # #"Plural, Gram", "Plural, Ungram"), pch=c(19, 19, 21, 21),
                    # #col=c("black", "red", "black", "red"), cex=1);
					
	# legend(1, max.y, c("Singular, Ungram","Plural, Ungram","Singular, Gram", 
					   # "Plural, Gram"), pch=c(19, 21, 19, 21),
					   # col=c("red", "red", "black", "black"), cex=1)


	# axis(1, at=seq(1, 12, 1), labels=regions, cex.axis=.9, las=2);   
	# axis(2, at=seq(min.y, max.y, by=50));
 
 
	# #plot the y points
	# points (sg.gram.stat[regions,"Mean"], type="b", pch=19, col= "black")
		# error.bar (1:12, sg.gram.stat[regions,"+SEM"],
        # sg.gram.stat[regions,"-SEM"],col= "black")
		
	# points (sg.ungram.stat[regions,"Mean"], type="b", pch=19, col= "red")
		# error.bar (1:12, sg.ungram.stat[regions,"+SEM"],
        # sg.ungram.stat[regions,"-SEM"],col= "red")

	# points (pl.gram.stat[regions,"Mean"], type="b", pch=21, col= "black")
		# error.bar (1:12, pl.gram.stat[regions,"+SEM"],
        # pl.gram.stat[regions,"-SEM"],lty=2, col= "black")

	# points (pl.ungram.stat[regions,"Mean"], type="b", pch=21, col= "red")
		# error.bar (1:12, pl.ungram.stat[regions,"+SEM"],
        # pl.ungram.stat[regions,"-SEM"],lty=2, col= "red")
 
 
 #dev.off();
#}

##Stats for item using Animacy as a factor (three way interactions)
#
#print("ANOVA animacy region 05suj");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="05suj",])));
#
#print("ANOVA animacy region 06v1");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="06v1",])));
##with(all[all$Region=="06v1",], interaction.plot(Gram, Animate, RT))
#
#print("ANOVA animacy region 07prep");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="07prep",])));			   
#
#print("ANOVA animacy region 08det3");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="08det3",])));
#
#print("ANOVA animacy region 09noun");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="09noun",])));
#
#print("ANOVA animacy region 10v2");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="10v2",])));
##with(all[all$Region=="10v2",], interaction.plot(Gram, Animate, RT))
#
#print("ANOVA animacy region 11v3");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="11v3",])));
#
#print("ANOVA animacy region end1");
#print(summary(aov(RT ~ Gram*Plural*Animate, data=all[all$Region=="end1",])));
#

# #Graphs for the three-way analysis

# regions.A = c("01det1", "02att", "03que", "04det2", "05suj", "06v1", "07prep",
             # "08det3", "09noun", "10v2", "11v3", "end1");

# sg.gram.a = all[all$Gram == TRUE & all$Plural == FALSE & all$Animate == TRUE, ]
# sg.gram.i = all[all$Gram == TRUE & all$Plural == FALSE & all$Animate == FALSE, ]
# pl.gram.a = all[all$Gram == TRUE & all$Plural == TRUE & all$Animate == TRUE, ]
# pl.gram.i = all[all$Gram == TRUE & all$Plural == TRUE & all$Animate == FALSE, ]
# sg.ungram.a = all[all$Gram == FALSE & all$Plural == FALSE & all$Animate == TRUE, ]
# sg.ungram.i = all[all$Gram == FALSE & all$Plural == FALSE & all$Animate == FALSE, ]
# pl.ungram.a = all[all$Gram == FALSE & all$Plural == TRUE & all$Animate == TRUE, ]
# pl.ungram.i = all[all$Gram == FALSE & all$Plural == TRUE & all$Animate == FALSE, ]

# sg.gram.a.stat = compute.stats(sg.gram.a);
# sg.gram.i.stat = compute.stats(sg.gram.i);
# sg.ungram.a.stat = compute.stats(sg.ungram.a);
# sg.ungram.i.stat = compute.stats(sg.ungram.i);
# pl.gram.a.stat = compute.stats(pl.gram.a);
# pl.gram.i.stat = compute.stats(pl.gram.i);
# pl.ungram.a.stat = compute.stats(pl.ungram.a);
# pl.ungram.i.stat = compute.stats(pl.ungram.i);

# min.y = min(c(sg.gram.a.stat[["95%tlow"]], sg.gram.i.stat[["95%tlow"]],
			 # sg.ungram.a.stat[["95%tlow"]], sg.ungram.i.stat[["95%tlow"]],
			 # pl.gram.a.stat[["95%tlow"]], pl.gram.i.stat[["95%tlow"]],
			 # pl.ungram.a.stat[["95%tlow"]], pl.ungram.i.stat[["95%tlow"]]));
# min.y = round.to.nearest(min.y - abs(min.y)*0.1, 50);
# max.y = max(c(sg.gram.a.stat[["95%tlow"]], sg.gram.i.stat[["95%tlow"]],
			 # sg.ungram.a.stat[["95%tlow"]], sg.ungram.i.stat[["95%tlow"]],
			 # pl.gram.a.stat[["95%tlow"]], pl.gram.i.stat[["95%tlow"]],
			 # pl.ungram.a.stat[["95%tlow"]], pl.ungram.i.stat[["95%tlow"]]));
# max.y = round.to.nearest(max.y + abs(max.y)*0.1, 50);


# #Set up plot
# pdf (paste("mean_RT.with_animacy.pdf", sep=""));

# #Set up the scale
# xlimit = c(1,12);
# ylimit = c(min.y,max.y);

# plot(NA,xlim=xlimit,ylim=ylimit,xlab="Regions", ylab="Residual RTs", xaxt="n",
     # yaxt="n", main="Three-way", type="n");

# legend(1, max.y, c("Singular, Gram, Animate", "Singular, Gram, Inanimate",
					# "Singular, Ungram, Animate", "Singular, Ungram, Inanimate",
					# "Plural, Gram, Animate", "Plural, Gram, Inanimate",
					# "Plural, Ungram, Animate", "Plural, Ungram, Inanimate"),
					# pch=c(19, 20, 19, 20, 21, 22, 21, 22),
					# col=c("black", "darkgreen", "red", "blue", "black", "darkgreen", "red", "blue"), cex=0.5);

# axis(1, at=seq(1, 12, 1), labels=regions.A, las=2, cex.axis=.8);
# axis(2, at=seq(min.y, max.y, by=50));


# #plot the y points
# points (sg.gram.a.stat[regions.A,"Mean"], type="b", pch=19, col= "black")
	# error.bar (1:12, sg.gram.a.stat[regions.A,"+SEM"],
	# sg.gram.a.stat[regions.A,"-SEM"],col= "black")

# points (sg.gram.i.stat[regions.A,"Mean"], type="b", pch=20, col= "darkgreen")
	# error.bar (1:12, sg.gram.i.stat[regions.A,"+SEM"],
	# sg.gram.i.stat[regions.A,"-SEM"],col= "darkgreen")
	
# points (sg.ungram.a.stat[regions.A,"Mean"], type="b", pch=19, col= "red")
	# error.bar (1:12, sg.ungram.a.stat[regions.A,"+SEM"],
	# sg.ungram.a.stat[regions.A,"-SEM"],col= "red")

# points (sg.ungram.i.stat[regions.A,"Mean"], type="b", pch=20, col= "blue")
	# error.bar (1:12, sg.ungram.i.stat[regions.A,"+SEM"],
	# sg.ungram.i.stat[regions.A,"-SEM"],col= "blue")
	
# points (pl.gram.a.stat[regions.A,"Mean"], type="b", pch=21, col= "black")
	# error.bar (1:12, pl.gram.a.stat[regions.A,"+SEM"],
	# pl.gram.a.stat[regions.A,"-SEM"],lty=2, col= "black")

# points (pl.gram.i.stat[regions.A,"Mean"], type="b", pch=22, col= "darkgreen")
	# error.bar (1:12, pl.gram.i.stat[regions.A,"+SEM"],
	# pl.gram.i.stat[regions.A,"-SEM"],lty=2, col= "darkgreen")
	
# points (pl.ungram.a.stat[regions.A,"Mean"], type="b", pch=21, col= "red")
	# error.bar (1:12, pl.ungram.a.stat[regions.A,"+SEM"],
	# pl.ungram.a.stat[regions.A,"-SEM"],lty=2, col= "red")

# points (pl.ungram.i.stat[regions.A,"Mean"], type="b", pch=22, col= "blue")
	# error.bar (1:12, pl.ungram.i.stat[regions.A,"+SEM"],
	# pl.ungram.i.stat[regions.A,"-SEM"],lty=2, col= "blue")
	
# dev.off()
