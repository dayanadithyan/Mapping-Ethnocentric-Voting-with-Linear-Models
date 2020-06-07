###============== WORKSPACE ==============####

#Determine working directory, assign an object to it
work.d <- getwd()

#Define with a vector, the names of the folders
output.folders <- c("1.Raw.Data","2.Clean.Data", "3.Analysis",
                    "4.Graphs", "5.Tables")

#Check to see if folders exist, if not, create function to create them

# This loop checks goes through the given out.put.folders list and checks to
# see if theyexisit in the working directory.
# If they don't they print "does not exist" and creates them,
# if it does exist it prints "does exist"

for(i in 1:length(output.folders)){
  if(!file.exists(output.folders[i])){
    print(paste(i, "does not exist"))
    dir.create(output.folders[i])
  }
  else  {
    print(paste(i,"does exist"))
  }
}

#Setup the pathways

path.rd <- paste(work.d,"/",output.folders[1], "/", sep="") #rawdata

path.cd <- paste(work.d,"/",output.folders[2], "/", sep="") #cleandata

path.a <- paste(work.d,"/",output.folders[3], "/", sep="") #analysis

path.g <- paste(work.d,"/",output.folders[4], "/", sep="") #graphs

path.t <- paste(work.d,"/",output.folders[5], "/", sep="") #tables


###============= CLEANING DATA ====================###

voter.db <- read.csv("raw.voter.ethnic.data.csv")

setwd(path.rd)

View(voter.db) #view the data

str(voter.db) #check the structure

#sort the data by population size
voter.db <- voter.db[order(voter.db$population, decreasing = "TRUE"),]

View(voter.db) #check the ordered data

write.csv(voter.db, "voterDB.csv")

#Attach a label, ethnic majority or minority to the database

cat.voter <- vector() #create an empty vector to be filled

#SEPARATING MAJORITY AND MINORITY LGAs#
#write an if loop within a for loop
#return those rows (LGAs) with a higher majority ethnic population than minority

for(i in 1:nrow(voter.db)) {
  if(voter.db[i,13] > voter.db[i,14]) {
    #paste into position i of vector cat.voter, label 'Majority'
    cat.voter[i] <- paste0("Majority")
  } else if (voter.db[i,13] < voter.db[i,14]) {
    #paste into position i of vector cat.voter, label 'Minority'
    cat.voter[i] <-paste0("Minority")
  }}

#Refer intro of paper for more info on majority and minority ethnicity
#else if is used instead of simply else to
#account for the odd case of them being equal

#we know the vector is ordered
#add the new collum to the database, these rows will be the labels

voter.db$ethnicity <- cat.voter

#WHICH PARTY HAS THE HIGHEST VOTES IN EACH LGA#
#Write a function to determine which party
#wins each electorate, and attach as
#collum to the data frame

#we know this vector will be ordered
party.win <-vector() #empty vector

#use the which max function, and apply it to the data frame
party.win <- (colnames(voter.db[,8:12])[apply(voter.db[,8:12], 1, which.max)])
#the collumn with the maximum value for each row is returned

#store this in a vector, it will be deleted later
voter.db$temp.win <- party.win

#Write a for loop to paste labels onto the database
winner.name <-vector() #empty vector for names

#check the labels from the collumn we just attached
for(i in 1:nrow(voter.db)) {
  if(voter.db[i,16] == "slpp.votes")
    #categorise as populist
    #paste into position i of vector win.name
    winner.name[i] <- paste0("Populist")
  else {
    #categorise as UNF
    #paste into position i of vector win.name
    winner.name[i] <- paste0("United National Front")
  }}

#add collumn to database
voter.db$winner <- winner.name

#Drop unwanted collums
voter.db$temp.win <- NULL

#CALCULATE VOTER TURNOUT#

#total pollled v total population
#round data to 2 sig.fig for ease of viewing
v.turnout <- signif(voter.db$tot.polled / voter.db$population, 2)

voter.db$voter.turnout <- v.turnout #add this collumn to the database

#Save to clean data folder
#change wd to clean data folder
setwd(path.cd)

View(voter.db) #check the file before Writing

#save the file
clean.voterdb <- write.csv(voter.db, "cleanvoterdb.csv", row.names = FALSE)

#Read the clean file
main.db <- read.csv("cleanvoterdb.csv")

View(main.db) #check the file

gc() #clear memory before analysis

#============ ANALYSIS =================#

#========== HYPOTHESIS TESTING =========#

#order the data by population for consistency
main.db <- main.db[order(main.db$population, decreasing = "TRUE"),]

#prepare a 2x2 contingency table
pop.vot <- table(main.db$winner, main.db$ethnicity)

#change the 0 in the table to 1
#to avoid abnormalities with the Confidence Interval in the Fisher's Test
# i.e - conf.int = infinity
pop.vot[1,2] <- 1

#create a test table doubling the values to see if FT is appropriate
#this is because of the small sample (N=20)
pop.vot.2 <- 2*pop.vot

#Perform hypothesis tests
ft.populist <- fisher.test(pop.vot) #fishers test
ft.populist.test <- fisher.test(pop.vot.2) #trial fisher test

#significance set at < 0.05
#return results
ft.populist
ft.populist.test

#####=========CORRELATION TESTING=========#########

#test correlation
#testing the null hypothesis (ref. Methods in paper)
pop.cor <- cor.test(main.db$sinhala.buddhist, main.db$slpp.votes,
                    #correlating majority population against slpp votes in a LGA
                    #use Pearson's test
                    method = "pearson")
pop.cor #return result

#additional analyis
unf.total <- main.db$unp.votes + main.db$upfa.slfp.votes +
  main.db$jvp.votes + main.db$other.votes
#the united national front is a coalition of parties (ref paper)

#paste this into the database
main.db$unf.total <- unf.total

#run the pearson correlation test
min.cor <- cor.test(main.db$minority, main.db$unf.total, method = "pearson")
min.cor #return result


#Plot the correlations
#We will be producing graphs, so change wd accordingly
setwd(path.g)

#Test each correlation using ggplot2
#Load the libraries
#You must install these packages if you don't have them already **********
library("ggpubr")
#scatterplot libraries
library("ggplot2")
#ggplot libraries
library("gridExtra")
library("jtools")

#using the layered grammar of graphics
pop.plot <- ggscatter(main.db, x = "sinhala.buddhist", y = "slpp.votes",
                      #add a regression line, with the confidence interval, use Pearson's MCC
                      add = "reg.line", conf.int = TRUE,
                      cor.coef = TRUE, cor.method = "pearson",
                      #label the axes
                      xlab = "Buddhist population in LGA",
                      ylab = "Votes received by Populist Party",
                      #set color, firebrick is similar to the official SLPP party colors
                      color = "firebrick1")

png("SinhalaBuddhistSLPP.png") #save to disk
pop.plot #return result
dev.off()

#plot the test correlation as well
min.plot <- ggscatter(main.db, x = "minority", y = "unf.total",
                      #testing correlation between minority population and unf total
                      add = "reg.line", conf.int = TRUE,
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "Minority population in LGA",
                      ylab = "Votes received by National Party",
                      color = "green1")

png("MinorityUNF.png")
min.plot #return result
dev.off()

#paste them onto one page for comparison
grid.arrange(pop.plot, min.plot, nrow = 1)

###TESTING ARBITRARY PLOTS FOR ANALYSIS#######

test.plot <- ggscatter(main.db, x = "minority", y = "slpp.votes",
                       add = "reg.line", conf.int = TRUE,
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "Minority population in LGA",
                       ylab = "Votes received by SLPP",
                       color = "cyan")

test.plot #return result

test.plot.2 <- ggscatter(main.db, x = "sinhala.buddhist", y = "unf.total",
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "Buddhist population in LGA",
                         ylab = "Votes received by UNF",
                         color = "darkorchid")

test.plot.2 #return result

###=============VISUALING BOTH ON SAME PLOT USING BASE R==============###

#CALCULATE regression for populist
pop.reg <- lm(main.db$slpp.votes ~ main.db$sinhala.buddhist, data = main.db)
str(pop.reg) #check structure
summary(pop.reg) #check the fields

#CALCULATE regression for minority
min.reg <- lm(main.db$unf.total ~ main.db$minority, data = main.db)
str(min.reg)
summary(min.reg)

#######==PLOT BLOCK==######
#save the plot
png(filename ="MajorityMinorityVoting.png")
plot(main.db$sinhala.buddhist, main.db$slpp.votes,
     xlim = range(main.db$sinhala.buddhist),
     #this is set as the range because they have the highest population numbers
     ylim = range(unf.total), pch = 19, col = "firebrick1",
     #this is set as the range because the UNF has the higher total votes
     xlab = "Population by ethnicity across LGAs",
     ylab = "Votes received by corresponding party")
#set the axis labels
points(main.db$minority, main.db$unf.total, pch = 19, col = "green1")
#add the minority data
abline(pop.reg, col = "firebrick1")
abline(min.reg, col = "green1")
#set colours
legend(90000, 150000, legend = c("Sinhala-Buddhist-SLPP", "Minority-UNF"),
       pch = 15, col = c("firebrick1", "green1"))
#change plot device from screen to disk
dev.off()
#Are the results the same with half the sample size?
#this will tell us if the results vary depending on the location
#or if it is consistent across the dataset

set.seed(989)
new.budd <- sample(main.db$sinhala.buddhist, 10, replace = TRUE)
new.min <- sample(main.db$minority, 10, replace = TRUE)
new.slpp <-sample(main.db$slpp.votes, 10, replace = TRUE)
new.unf <- sample(main.db$unf.total, 10, replace = TRUE)

random.db <- data.frame(new.budd, new.min, new.slpp, new.unf)

#perform corr.test
new.pop.cor <- cor.test(random.db$new.budd, random.db$new.slpp,
                        method = "pearson")
new.pop.cor

#the correlation is weak
new.min.cor <- cor.test(random.db$new.min, random.db$new.unf,
                        method = "pearson")
new.min.cor

#the correlation is weak 

###========END OF ANALYSIS============#

