########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
########################################################
#Some simple visualizations and statistics of the first (of two) admission exams to Mexico's biggest university

library(ggplot2)
library(directlabels)
library(Hmisc)
library(xtable)

cleanData <- function(area.df) {
  area <- area.df
  colnames(area) <- c("Num","Score","Accepted","Major","Faculty")
  area$Major <- sub(".[0-9]+. +(.)[\n]*", "\\1", area$Major)
  area$Faculty <- sub(".[0-9]+. +(.)[\n]*", "\\1", area$Faculty)
  area$Loc <- ifelse(substr(area$Faculty,1,8)=="FACULTAD",c("C.U."),
                   c(area$Faculty))
  #area.ac<-subset(area,area$Accepted=="A")
  area
}

median_cl_boot <- function(x, conf.int = 0.95, B = 1000, na.rm = TRUE, reps = FALSE) {
    if (na.rm)
        x <- x[!is.na(x)]
    n <- length(x)
    xm <- median(x)
    if (n < 2)
        return(data.frame(y = xm, ymin = NA, ymax = NA))
    resamples <- lapply(1:B, function(i) sample(x, replace=T))
    r.median <- sapply(resamples, median)
    quant <- quantile(unlist(r.median),
                      c((1 - conf.int)/2, (1 + conf.int)/2))
    names(quant) <- NULL
    Median <- median(x)
    data.frame(y = Median,
               ymin = quant[1],
               ymax = quant[2])
}


graphMajors <- function(area.df, title="", filename) {
  area.ac <- cleanData(area.df)
  area.ac<-subset(area.ac,area.ac$Accepted=="A")

 # major <- data.frame(summarize(area.ac$Score, llist(area.ac$Major,
#                    area.ac$Loc), median ))
 # major$area.ac.Major <- factor(major$area.ac.Major)
 # major$area.ac.Major <- with(major, reorder(area.ac.Major,
  #                                           median))

  colnames(area.ac) <- c("Num", "Score", "Accepted", "Major",
                         "Faculty", "Campus")
  area.ac <- ddply(area.ac, .(Major, Campus), transform,
                        median = median(Score))
  area.ac <- ddply(area.ac, .(Major), transform, median = max(median))
  area.ac$Major <- with(area.ac, reorder(Major, median))
#  area.ac$Major<-factor(area.ac$Major, levels = levels(
 #                                     major$area.ac.Major))
  if (length(levels(factor(area.ac$Campus))) < 7) {
    p <- ggplot(aes(y=Score, x=Major, color=Campus, shape=Campus),
                data=area.ac)
  } else {
    p <- ggplot(aes(y=Score, x=Major, color=Campus, group = Campus), data=area.ac)
  }
  p + opts(strip.text.y = theme_text())  +
      coord_flip() +
      geom_jitter(alpha = I(.2),
                  position=position_jitter(width=.15)) +
      stat_summary(size=.75, fun.data = median_cl_boot) +#
      stat_summary(fun.data = median_cl_boot, color=I("black"),
                   geom="point",size=3) +
      ylab("Number of correct answers") + xlab("") +
          opts(title=title) + theme_bw()
  ggsave(file = paste("output/", filename, "_maj.png", sep = ""),
         dpi=72, width=3.3, height=2.05, scale=4)
}

graphCampus <- function(area.df, title="", filename) {
  area.ac<-cleanData(area.df)
  area.ac<-subset(area.ac,area.ac$Accepted=="A")
  colnames(area.ac) <- c("Num", "Score", "Accepted", "Major", "Campus",
                         "Loc")
  area.ac$Campus <- with(area.ac, reorder(factor(Campus), Score,
                                          median))
  ggplot(data = area.ac, aes(x = Campus, y = Score)) +
         geom_jitter(alpha = I(.2),
                  position=position_jitter(width = .15)) +
         stat_summary(fun.data = median_cl_boot, color = I("red"),
                      width = .4, geom = "crossbar") +
         ylab("Number of correct answers") +
         xlab("") +
         coord_flip() +
         opts(title = title) + theme_bw()
  ggsave(file = paste("output/", filename, "_fac.png", sep = ""),
         dpi=72, width=2.5, height=1.5, scale=4)
}

makeTable <- function(area.df, filename) {
  area <- cleanData(area.df)
  area.re <- subset(area,area$Accepted == "R")
  area.ac <- subset(area,area$Accepted == "A")
  min_score <- data.frame(summarize(area.ac$Score, llist(area.ac$Major,
                    area.ac$Faculty), min ))
  yieldR <- summarize(area.re,
                      llist(area.re$Accepted,area.re$Major,
                            area.re$Faculty),
                      nrow)
  yieldA <- summarize(area.ac,
                      llist(area.ac$Accepted,area.ac$Major,
                            area.ac$Faculty),
                      nrow)
  colnames(min_score) <- c("Major","Location","Cutoff.Score")
  min_score$Total.Applied <- yieldA$area.ac + yieldR$area.re
  min_score$Accepted <- yieldA$area.ac
  min_score$Acceptance.Rate <- paste(round((yieldA$area.ac /
                                       yieldR$area.re) * 100,
                                digits = 1),"%", sep = "")
  write.csv(min_score[order(-min_score$Cutoff.Score),],
            file = paste("output/", filename, ".csv", sep = ""),
            row.names = FALSE)
#  print(xtable(min_score[order(-min_score$Minimo),]), type="html",
 #       file = paste("output/", filename, ".html", sep = ""),
  #      include.rownames = FALSE)
}

readFile <- function(filename) {
    filename <- paste("data/", filename, ".csv.bz2", sep="")
    read.csv(bzfile(filename), na.strings=c("N","C"), header=FALSE,
                 colClasses = c("factor","numeric","factor","factor",
                              "factor"))
}

newAnalyzeThis <- function(){
    allareas <- data.frame()
    list(
         analyze = function(filename, title.majors, title.campuses) {
            area <- readFile(filename)
            allareas <<- rbind(allareas, area)
            makeTable(area, filename)
            graphMajors(area, title.majors , filename)
            graphCampus(area, title.campuses, filename)
            return(NULL)
         },
         result = function() {
             cleanData(allareas)
         }
    )
}

########################################################
#Finally, the program
########################################################
filenames <- c("AreaI", "AreaII", "AreaIII", "AreaIV")
title.majors <- c("Median score by major (95% CI). Physical Sciences, Mathematics and Engineering",
                  "Median score by major (95% CI). Biological Sciences and Health",
                  "Median score by major (95% CI). Social Sciences",
                  "Median score by major (95% CI). Humanities and Arts")
title.campuses <- c("Median score by Educational Establishment (95% CI). Physical Sciences, Mathematics and Engineering",
                    "Median score by Educational Establishment (95% CI). Biological Sciences and Health",
                    "Median score by Educational Establishment (95% CI). Social Sciences",
                    "Median score by Educational Establishment (95% CI). Humanities and Arts")

analyzeUNAM <- newAnalyzeThis()
mapply(analyzeUNAM$analyze, filenames, title.majors, title.campuses)
allareas <- analyzeUNAM$result()


########################################################
#Some basic statistics
########################################################


#Number Accepted and Rejected (7690 + 98241)
ddply(allareas,.(Accepted),function(df) nrow(df))
#Average score of those Accepted
ddply(allareas,.(Accepted),function(df) mean(df$Score, na.rm=TRUE))
#Wooow, that's a lot of people with high scores who were rejected,
#maybe instead of appling to med school they should have studied
#some social science, hahaha
ddply(allareas[order(-allareas$Score), ][1:7690,], .(Accepted),
      function(df) nrow(df))
3766 / (3924 + 3766)
ddply(allareas[order(-allareas$Score), ][1:7690,], .(Accepted),
      function(df) mean(df$Score, na.rm=TRUE))


ggplot(allareas, aes(Score, group = Accepted, fill = Accepted,
                     color = Accepted)) +
    geom_density(aes(y = ..count..), alpha =.4, binwidth = 1) +
    xlab("Score") + ylab("Number of Students") +
    annotate("text", x = 75, y = 2200, label = "Rejected",
             color = "#F8766D" ) +
    annotate("text", x = 105, y = 270, label = "Accepted",
             color = "#00BFC4") +
    scale_color_manual(values = c("#00BFC4", "#F8766D")) +
    scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
    theme_bw() +
    opts(title = "UNAM - February Admission Exam, 2009") +
    #facet_wrap(~ Loc, scales = "free") +
    opts(legend.position = "none")
ggsave(file = "output/accepted-vs-rejected.png",
         dpi=72, width=1.5, height=1.5, scale=4)

########################################################
#Compare salaries with the entrace exam score
########################################################
#From Reforma:
#Medicina: $13,364 pesos
#Ingeniería en Sistemas: 12,636 pesos
#Derecho: $10,969 pesos
#Ingeniería Mecatrónica: $10,902 pesos
#Arquitectura: $10,870 pesos
#Ingeniería Civil: $10,821 pesos
#Licenciado en Sistemas: $10,736 pesos
#Mercadotecnia: $10,545 pesos
#Ingeniería Industrial: $9,747 pesos
#Psicología: $9,708 pesos
#Relaciones Internacionales: $9,704 pesos
#Administración: $9,567 pesos
#Comunicación: $9,372 pesos
#Contaduría: $8,151 pesos
allareas.a <- subset(allareas,Accepted == "A")
allareas.cu.a <- allareas.a[grep("FACULTAD",allareas.a$Faculty),]
avscore <- function(str, df){
    mean(df[grep(str, df$Major),]$Score)
}
majors <- c("MEDICO", "INGENIERIA EN COMPUTACION",
            "DERECHO","MECATRONICA", "ARQUITECTO","INGENIERIA CIVIL",
            "INGENIERIA INDUSTRIAL","PSICOLOGIA","RELACIONES INT",
            "ADMINISTRACION","COMUNICACION","CONTADURIA" )
scores <- sapply(majors, avscore, allareas.cu.a)
salaries <- c(13364,12636,10969,10902,10870,10821,9747,9708,9704,9567,9372,8151)
ss <- data.frame(scores, log.salaries = log(salaries))
ggplot(ss, aes(scores, log.salaries, label = rownames(ss))) +
    geom_point() +
    ylab("Number of correct answers") +
    geom_smooth(method = lm) +
    geom_text(hjust=-0.05, angle = -40, size = 4) +
    coord_cartesian(xlim = c(75.5, 115)) +
    opts(title = "Starting Salary vs Entrance Exam Score") +
    theme_bw()
ggsave(file = "output/score_vs_salary.png",
         dpi=72, width=1.5, height=1.5, scale=4)



#############################################################
#The med school students at the UNAM have the highest scores
#and since all students who want to become doctors
#have to take the ENARM, we can compare them to other schools
#############################################################

#If 47% percent of the students passed the ENARM what score would
#be the equivalent in the admission exam
enarm <- read.csv("data/enarm-2009.csv")
enarm$prop <- enarm$Passed / enarm$Students
area.ac<-subset(allareas, allareas$Accepted == "A")

getMinScore <- function(Campus, df) {
    area.ac.F<-subset(df, Major == "MEDICO CIRUJANO" &
                          Faculty == Campus)
    rown <- which(tolower(as.character(enarm$University)) == tolower(Campus))
    print(nrow(area.ac.F) * enarm[rown,]$prop)
    print(enarm[rown,]$prop)
    ordered <- area.ac.F$Score[order(-area.ac.F$Score)]
    min(ordered[1:round(nrow(area.ac.F) * enarm[rown,]$prop,
                        digits=0)])
}
facs <- c("FACULTAD DE MEDICINA",
          "FES IZTACALA",
          "FES ZARAGOZA")
scores <- sapply(facs, getMinScore, area.ac)


area.ac.MC<-subset(area.ac,area.ac$Major=="MEDICO CIRUJANO" &
                   (area.ac$Faculty=="FACULTAD DE MEDICINA" |
                   area.ac$Faculty=="FES IZTACALA"  |
                   area.ac$Faculty=="FES ZARAGOZA"))
colnames(area.ac.MC) <- c("Num", "Correct", "Accepted", "Major",
                          "Campus", "Loc")
ggplot(area.ac.MC, aes(x = Correct, fill = Campus, color = Campus)) +
       geom_density(alpha = 0.4, size=1,
                    aes(y = ..density..))  +
       xlab("Number of correct answers") +
       geom_vline(xintercept = scores[1], linetype = 2,
                  color = "red", alpha = .3) +
       geom_vline(xintercept = scores[2], linetype = 2,
                  color = "green", alpha = .3) +
       geom_vline(xintercept = scores[3], linetype = 2,
                  color = "blue", alpha = .3) +
       opts(title = "Distribution of Med Students Admission Scores") +
       theme_bw()
ggsave(file = "output/density_unam.png",
         dpi=72, width=2.5, height=1.5, scale=4)

#Given the differences in the admission exam scores what percentages
#should pass the ENARM
perPass <- function(campus, area.ac){
    nrow(subset(area.ac,area.ac$Major == "MEDICO CIRUJANO" &
                   area.ac$Faculty == campus &
                   area.ac$Score >= scores[1] )) /
    nrow(subset(area.ac,area.ac$Major == "MEDICO CIRUJANO" &
                   area.ac$Faculty == campus ))
}
med <- c("FES IZTACALA", "FES ZARAGOZA", "FACULTAD DE MEDICINA")
sapply(med, perPass, area.ac)
enarm
scores


#enarm$e <- sqrt((enarm$prop * (1 - enarm$prop)) / enarm$Students)
#enarm$University <- reorder(enarm$University, enarm$prop)
#ggplot(enarm, aes(University, prop)) +
#    scale_y_continuous(formatter = "percent") +
#    geom_linerange(aes(ymax = prop + e, ymin = prop -e), color = "red") +
#    geom_point() +
#    coord_flip() +
#    opts(title = "Percentage of students who passed the ENARM in 20089") +
#    ylab("") +
#    theme_bw()
#ggsave(file = "output/uni_ranked.png",
#         dpi=72, width=1.5, height=1.5, scale=4)

########################################################
#ENARM
########################################################

enarm.all <- read.csv("data/enarm-2009-all.csv")
enarm.all$per <- ifelse(enarm.all$Passed > 0,
                    enarm.all$Passed / enarm.all$Test.Takers, 0)
means <- sapply(enarm.all[,2:8], function(x) mean(x))
wmeans <- sapply(enarm.all[,5:8],
                 function(x) weighted.mean(x, enarm.all$Test.Takers))
means[["per"]];wmeans
ggplot(enarm.all, aes(Medical.Score, English.Score,
                      label = University)) +
    geom_point(aes(size = Test.Takers)) +
    geom_text(hjust=-0.07, angle = -60, size = 3, alpha = .8) +
    geom_smooth(method = lm) +
    theme_bw()
ggsave(file = "output/all-universities.png",
         dpi=72, width=3.3, height=2.3, scale=4)

enarm <- read.csv("data/enarm-2009.csv")
enarm$per <- enarm$Passed / enarm$Students
enarm$error <- sqrt((enarm$per * (1 - enarm$per)) / enarm$Students)



grid.newpage()
pushViewport(viewport(layout =  grid.layout(nrow = 2, ncol = 2)))

subplot <- function(x, y) viewport(layout.pos.row = x,
                                   layout.pos.col = y)

enarm$University <- reorder(enarm$University, enarm$per)
print(ggplot(enarm, aes(University, per)) +
    scale_y_continuous(formatter = "percent") +
    geom_linerange(aes(ymax = per + error, ymin = per - error),
                   color = "red") +
    geom_point() + xlab("") +
    coord_flip() +
    geom_hline(yintercept = means[["per"]], linetype = 2,
               color = "gray") +
    scale_y_continuous(formatter = "percent", limits = c(0, .95)) +
    opts(title = "Percentage of students who passed the ENARM in 2009") +
    ylab("") +
    theme_bw(), vp = subplot(1, 1))
enarm.unam <- subset(enarm.all, University %in%
                     c("UNAM FM-CU",
                       "UNAM FES Zaragoza",
                       "UNAM FES Iztacala",
                       "Univ Panameriacana",
                       "Univ La Salle",
                       "Inst Tecnológico Est Sup Monterrey",
                       "Univ. Aut. De Nuevo León"))
print(ggplot(enarm.all, aes(Medical.Score, English.Score,
                        label = University)) +
    geom_text(data = enarm.unam, aes(Medical.Score, English.Score,
                        label = University),
              hjust=-0.09, angle = 0, size = 3, alpha = 1) +
    geom_point(aes(size = Test.Takers)) +
    geom_smooth(method = lm) +
    opts(title = "Regression of Medical and English Scores") +
    scale_x_continuous(limits = c(52.25, 64)) +
    scale_y_continuous(limits = c(5.31, 8.3)) +
    theme_bw(),
    vp = subplot(1, 2))

enarm$University <- reorder(enarm$University, enarm$English.Score)
print(ggplot(enarm, aes(University, English.Score)) +
    geom_point() +
    coord_flip() + xlab("") +
    geom_hline(yintercept = means[["English.Score"]], linetype = 2,
               color = "gray") +
    opts(title = "Average English Score in the ENARM 2009") +
    ylab("") +
    theme_bw(), vp = subplot(2, 2))

enarm$University <- reorder(enarm$University, enarm$Medical.Score)
print(ggplot(enarm, aes(University, Medical.Score)) +
    geom_point() +
    coord_flip() + xlab("") +
    geom_hline(yintercept = means[["Medical.Score"]], linetype = 2,
               color = "gray") +
    opts(title = "Average Medical Score in the ENARM 2009") +
    ylab("") +
    theme_bw(), vp = subplot(2, 1))

dev.print(png, "output/uni-enarm2009.png", width=800, height=600)

#Fraud in the ENARM?
#Secretaria de Marina, Escula Médico Naval 71 40 - 2007
#4 1 - 2008
40/71
1/4
#Escuela Médico Militar Univ. Ejer y Fza Aerea 116 115 - 2007
#2 0 - 2008

#filenames <- c("AreaI", "AreaII", "AreaIII", "AreaIV")
#filenamesF <- paste("data/", filenames, "Feb2010.csv", sep="")
#filenamesJ <- paste("data/", filenames, "Junio2010.csv", sep="")


#readFiles <- function(files) {
#  unam <- lapply(files, function(x) {
#                               t <- read.csv(x)
#                               names(t) <- c("Num","Score","Accepted","Major","Faculty")
#                               t
#                           })
#  unam <- rbind.fill(unam)
#  unam
#}

#unamF <- readFiles(filenamesF)
#unamF$date <- "Feb"
#unamJ <- readFiles(filenamesJ)
#unamJ$date <- "Jun"
#unam <- rbind(unamF, unamJ)


#unam$score2 <- as.numeric(as.character(unam$Score))
#ddply(subset(unam, Accepted = "A"), .(Faculty),
#      function(df) mean(df$score2, na.rm = TRUE))
#max(unam$score2, na.rm = TRUE)
#fix(unam)
