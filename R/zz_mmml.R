
############## [DEPRECATED - FOR LATER REFERENCE]

# ### version 06.08.2012
# ### function name, variables, and defaults
#
# #' Multiple-Measure Maximum-Likelihood Classification
# #'
# #' Classifies a participant as a user of one of several decision strategies based on choices, confidence ratings, and # response times.
# #' @param data either a data frame or the path to a csv file (e.g., "folder/data.csv")
# #' @param strategy strategy label
# #' @param choice vector of choice predictions for item type 1,2,...
# #' @param time predicted contrasts for response times
# #' @param conf predicted contrasts for confidence judgments
# #' @param id only select a subset of participants (e.g., \code{id=1})
# #' @param output path to csv file to store results
# #' @param sep the csv field separator string
# #' @param maxit maximum number of iterations
# #' @examples
# #' # MM-ML with data by Glockner (2009)
# #' mmml(data = glockner2009, strategy = "TTB",id=1,
# #'      choice = c(10, 10, 10, 10, 10, 10),
# #'      time = c(0.167, 0.167, 0.167, 0.167, 0.167, 0.833),
# #'      conf = c(0.167, 0.167, 0.167, 0.167, 0.167, 0.833))
# #' @author Marc Jekel
# # @import stats4
# #' @export
# #' @references
# #' Glöckner, A. (2009). Investigating intuitive and deliberate processes statistically: The multiple-measure maximum # likelihood strategy classification method. Judgment and Decision Making, 4, 186-199.
# #'
# #' Jekel, M., & Nicklisch, A. (2010). Implementation of the Multiple-Measure Maximum Likelihood strategy classificati# on method in R: Addendum to Glöckner (2009) and practical guide for application. Judgment and Decision Making, 5, 54# -63.
# mmml = function(
#   data,
#   strategy="NONAME",
#   choice = "none",
#   time = "none",
#   conf = "none",
#   id = "all",
#   output,
#   sep = ",",
#   maxit = 10^9,
#   reltol = 10^-15
# ){
#
#   ### set control values for mle
#   controlParameterMLE = list(maxit = maxit, reltol = reltol)
#
#   ### read in data
#   if(is.character(data))
#     data = read.csv(data,sep =  sep,header = T)
#
#   if(id == "all"){
#     partic <- seq_along(unique(data$PARTICIPANT))
#   }else{
#     partic <- id
#   }
#
#   ### define variables and set default values
#
#   guessing = 0  # guessing for choices
#   zerocontrasttime = 0 # zero contrast for decision times
#   zerocontrastconfidence = 0  # zero contrast for confidence judgments
#   counterGUESSING = 0 # needed to check for guessing
#   memory = numeric() # output is stored in the variable memory
#   choice_freq = numeric()
#
#   ### check if there is only guessing for choices
#
#   if(choice[1] != "none"){
#
#     for (i in 1 : length(choice)){
#
#       if(choice[i] == 1/2){
#         counterGUESSING = counterGUESSING + 1
#       }
#     }
#
#     if (counterGUESSING==length(choice)){
#       guessing = 1
#     }
#   }
#
#   ### check if there is a zero contrast for decision times
#
#   if (time[1]!="none"){
#
#     if (sum(abs(time)) == 0 | sd(time)==0){
#       zerocontrasttime = 1
#       time = rep(0,length(time))
#     }
#   }
#
#   ### check if there is a zero contrast for confidence judgments
#
#   if (conf[1]!="none"){
#
#     if (sum(abs(conf)) == 0 |
#         sd(conf)==0){
#       zerocontrastconfidence = 1
#       conf = rep(0,length(conf))
#     }
#   }
#
#   ### determine the number of parameters that need to be
#   ### estimated (needed to calculate BICs)
#
#   numberOFfreeParameters = 7 # full model
#   if(guessing == 1){
#     numberOFfreeParameters=numberOFfreeParameters-1
#   }
#
#   if(choice[1] == "none"){
#     numberOFfreeParameters=numberOFfreeParameters-1
#   }
#
#   if(time[1] == "none"){
#     numberOFfreeParameters=numberOFfreeParameters-3
#   }
#
#   if(conf[1] == "none"){
#     numberOFfreeParameters=numberOFfreeParameters-3
#   }
#
#   if(zerocontrasttime == 1){
#     numberOFfreeParameters=numberOFfreeParameters-1
#   }
#
#   if(zerocontrastconfidence == 1){
#     numberOFfreeParameters=numberOFfreeParameters-1
#   }
#
#   ### change contrasts for decision times and confidence
#   ### judgments --> range of contrasts = 1 and sum of contrasts = 0
#
#   if(time[1] != "none" & zerocontrasttime !=1){
#     time =
#       time-(sum(time)/length(time))
#     rangetime=range(time)[2]-range(time)[1]
#     time = time/rangetime }
#
#   if(conf[1] != "none" & zerocontrastconfidence != 1){
#     conf=conf-
#       (sum(conf)/length(conf))
#     rangeconfidence=range(conf)[2]-
#       range(conf)[1]
#     conf = conf/rangeconfidence }
#
#   ### loop participants
#
#   for(numbpartic in 1 : length(partic)){
#
#     ### define variables
#
#     numberOFtypes = max(data$TYPE[data$PARTICIPANT==partic[numbpartic]])
#     numberOFtasksPerType = numeric()
#
#     for(loopnumberOFtasksPerType in 1 : numberOFtypes){
#       numberOFtasksPerType[loopnumberOFtasksPerType] = NROW(data[data$TYPE
#                                                                 == loopnumberOFtasksPerType & data$PARTICIPANT
#                                                                 == partic[numbpartic],]) }
#
#     logliks = c(0,0,0)
#     numberOFmeasures = 0
#     stratOutput  = numeric()
#     epsilon = numeric()
#     messageerror = ""
#     #errortermneeded = 1 # see *** below --> is it necessary
#     # to add/subtract an error term?
#
#     ### prepare data vector for choices
#
#     if(choice[1] != "none"){
#
#       for(looptype in 1 : numberOFtypes){
#         choice_freq[looptype] =  sum(data$ACHOICES[data$PARTICIPANT ==
#                                                partic[numbpartic] & data$TYPE == looptype])
#       }
#
#       OnEpsilon = ifelse(choice==1/2,0,1)
#       # see the function for choices below --> if
#       # the strategy predicts option A or B, epsilon
#       # needs to be fixed at 0.5
#       FixedEpsilon = ifelse(choice==1/2,1/2,0)
#
#       ### If all choices are (resp. are not) in line # ***
#       ### with the expected choices, it is necessary
#       ### for mle convergence to add
#       ### (resp. subtract) 1 strategy inconsistent
#       ### (resp. consistent) choice_freq. It is further
#       ### necessary that the 1 strategy inconsistent
#       ### (resp. consistent) choice_freq is added to
#       ### (resp. subtracted from) a type of tasks
#       ### that does NOT predict guessing (i.e, fixed
#       ### epsilon = .5).
#
#       #for (looperror in 1 : length(numberOFtasksPerType)){
#       #
#       #  if(choice_freq[looperror]!=numberOFtasksPerType[looperror] &
#       #     choice_freq[looperror]!=0 & choice[looperror] != 1/2){
#       #    errortermneeded = 0
#       #  }
#       #}
#
#       ### new error term command (correction felix)
#
#       checkProp =
#         ifelse(choice_freq == choice,1,0)
#
#       checkProp =
#         ifelse(choice_freq == (numberOFtasksPerType-choice),2,
#                checkProp)
#
#       if((sum(checkProp[choice != .5] == 1) ==
#           sum(choice != .5)) |
#          (sum(checkProp[choice != .5] == 2) ==
#           sum(choice != .5))
#       ){
#
#         errortermneeded = 1
#       }else{
#
#         errortermneeded = 0
#       }
#
#       ###
#
#       if (guessing != 1 & errortermneeded == 1){
#         typeerror = which(choice!=1/2)[1]
#
#         if (choice_freq[typeerror] == 0){
#           choice_freq[typeerror] = 1
#           messageerror =
#             paste("### ERROR TERM: 1 A CHOICE ADDED TO TYPE OF TASKS ###",
#                   typeerror)
#         }else{
#           choice_freq[typeerror] = numberOFtasksPerType[typeerror]-1
#           messageerror =
#             paste("### ERROR TERM: 1 A CHOICE SUBTRACTED FROM TYPE OF TASKS ###",
#                   typeerror)
#         }
#       }
#
#       ### if option B is predicted, choices and expected choices need
#       ### to be inverted
#
#       expectedchoiceINVERTED = ifelse(choice == 1/2,
#                                       numberOFtasksPerType,choice)
#       indexItemsInverted = which(expectedchoiceINVERTED == 0)
#
#       for (loopinversion in 1 : length(indexItemsInverted)){
#         choice_freq[indexItemsInverted[loopinversion]] =
#           numberOFtasksPerType[indexItemsInverted[loopinversion]] -
#           choice_freq[indexItemsInverted[loopinversion]]
#         expectedchoiceINVERTED[indexItemsInverted[loopinversion]] =
#           numberOFtasksPerType[indexItemsInverted[loopinversion]]
#       }
#
#       numberOFmeasures = numberOFmeasures + 1
#     }
#
#     ### prepare data vector for decision times
#
#     if(time[1] != "none"){
#       time_obs = data$DECTIMES[data$PARTICIPANT == partic[numbpartic]]
#       typesTime = data$TYPE[data$PARTICIPANT == partic[numbpartic]]
#       Ttime = time[typesTime]
#       numberOFmeasures = numberOFmeasures + 1
#
#       if(sd(time_obs) == 0){
#
#         time_obs = time_obs + rnorm(length(time_obs))
#
#         messageerror = paste(messageerror,
#                              "### NO VARIANCE IN DECTIMES --> random noise N(0,1) added ###")
#       }
#     }
#
#
#     ### prepare data vector for confidence judgments
#
#     if(conf[1] != "none"){
#       confidence = data$CONFJUDGMENTS[data$PARTICIPANT ==
#                                        partic[numbpartic]]
#       typesConfidence  = data$TYPE[data$PARTICIPANT ==
#                                     partic[numbpartic]]
#       Tconfidence = conf[typesConfidence]
#       numberOFmeasures = numberOFmeasures + 1
#
#       if(sd(confidence) == 0){
#
#         confidence = confidence + rnorm(length(confidence))
#         messageerror = paste(messageerror,
#                              "### NO VARIANCE IN CONFJUDGMENTS --> random noise N(0,1) added ###")
#       }
#     }
#
#     ###################################################################
#     ############### define density functions ##########################
#     ###################################################################
#
#     ### choices
#
#     if(choice[1] != "none"){
#
#       if(guessing!=1){
#         Choice = function(epsil){-sum(dbinom(choice_freq, expectedchoiceINVERTED,
#                                              prob = (((1-epsil) * OnEpsilon) + FixedEpsilon) , log = TRUE))};
#       }else{
#         Choice = function(epsil){-sum(dbinom(choice_freq, expectedchoiceINVERTED,
#                                              prob = 1-epsil , log = TRUE))};      # function for the RANDOM model
#       }
#     }
#
#     ### decision times
#
#     if(zerocontrasttime != 1){
#       Time = function(mu_Time = 8,sigma_Time, R_Time){-sum(dnorm(time_obs,
#                                                                  mean = (mu_Time+(Ttime*abs(R_Time))),
#                                                                  sd = sigma_Time,log = TRUE))};
#     }else{
#       Time = function(mu_Time, sigma_Time){-sum(dnorm(time_obs,
#                                                       mean = mu_Time,sd = sigma_Time,log = TRUE))};
#     }
#
#     ### confidence judgments
#
#     if(zerocontrastconfidence != 1){
#       Confidence = function(mu_Conf, sigma_Conf,R_Conf){-sum(
#         dnorm(confidence,
#               mean = (mu_Conf +(Tconfidence*abs(R_Conf))),sd = sigma_Conf,
#               log = TRUE))};
#     }else{
#       Confidence = function(mu_Conf, sigma_Conf){-sum(dnorm(confidence,
#                                                             mean = mu_Conf ,sd = sigma_Conf,log = TRUE))};
#     }
#
#     ###################################################################
#     ############## do mle #############################################
#     ###################################################################
#
#     ### set starting values for mle
#
#     start_epsilon = 0.5
#
#     if(time[1] != "none"){
#       start_mu_Time = mean(time_obs)
#       start_sigma_Time = sd(time_obs)
#       start_R_Time = sd(tapply(time_obs,typesTime,mean))
#     }
#
#     if(conf[1] != "none"){
#       start_mu_Conf = mean(confidence)
#       start_sigma_Conf = sd(confidence)
#       start_R_Conf = sd(tapply(confidence,typesConfidence,mean))
#     }
#
#     ### mle for choices
#
#     if(choice[1] != "none"){
#
#
#       if(guessing != 1){
#
#         fit1Func <- quote(mle())
#         fit1Func$control <- controlParameterMLE
#         fit1Func$start <- list(epsil= start_epsilon)
#         fit1Func$method <-"BFGS"
#         fit1Func$minuslog <- Choice
#
#         fit1 = eval(fit1Func)
#
#       }else{
#
#         fit1Func <- quote(mle())
#         fit1Func$control <- controlParameterMLE
#         fit1Func$start <- list(epsil= start_epsilon)
#         fit1Func$method <-"BFGS"
#         fit1Func$minuslog <- Choice
#         fit1Func$fixed = list(epsil = 0.5)
#
#         fit1 = eval(fit1Func)
#
#       }
#
#       logliks[1] = logLik(fit1)
#     }
#
#     ### mle for decision times
#
#     if(time[1] != "none"){
#
#       if(zerocontrasttime != 1){
#         startTIME = list(mu_Time = start_mu_Time,
#                          sigma_Time = start_sigma_Time,R_Time = start_R_Time)
#       }else{
#         startTIME = list(mu_Time = start_mu_Time,
#                          sigma_Time = start_sigma_Time)
#       }
#
#       fit2Func <- quote(mle())
#       fit2Func$control <- controlParameterMLE
#       fit2Func$start <- startTIME
#       fit2Func$method <-"BFGS"
#       fit2Func$minuslog <- Time
#
#       fit2 = eval(fit2Func)
#       logliks[2] = logLik(fit2)
#     }
#
#     ### mle for confidence judgments
#
#     if(conf[1] != "none"){
#
#       if(zerocontrastconfidence != 1){
#         startCONFIDENCE = list(mu_Conf=start_mu_Conf,
#                                sigma_Conf=start_sigma_Conf,R_Conf=start_R_Conf)
#       }else{
#         startCONFIDENCE = list(mu_Conf=start_mu_Conf,
#                                sigma_Conf=start_sigma_Conf)
#       }
#
#       fit3Func <- quote(mle())
#       fit3Func$control <- controlParameterMLE
#       fit3Func$start <- startCONFIDENCE
#       fit3Func$method <-"BFGS"
#       fit3Func$minuslog <- Confidence
#
#       fit3 = eval(fit3Func)
#       logliks[3] = logLik(fit3)[1]
#     }
#
#     ###################################################################
#     ######### calculate log-liks, BICs, coefficients, z-scores, #######
#     #########  significance levels, and confidence intervals ##########
#     ###################################################################
#
#     ### log-likelihoods
#
#     logliks = sum(logliks)
#
#     ### BICs
#
#     BICs = (-2*(logliks)+log(numberOFmeasures*numberOFtypes)*
#               numberOFfreeParameters)[1]
#
#     ### choices
#
#     if(choice[1] != "none" & guessing != 1){
#       epsilon = coef(summary(fit1))
#       epsilon = c(epsilon ,coef(summary(fit1))[,1]/coef(summary(fit1))[,2])
#       epsilon = c(epsilon,2*(1-pnorm(abs(epsilon[3]))))
#       epsilon = c(epsilon, epsilon[1]-epsilon[2]*1.96,
#                   epsilon[1]+epsilon[2]*1.96)
#
#       ### set labels if MM-ML is only based on choices
#
#       if(time[1] == "none" & conf[1] ==
#          "none"){
#         names(epsilon) = c("epsilon","Std. Error epsilon",
#                            "z epsilon","P>|z| epsilon","2.5 % CI epsilon",
#                            "97.5 % CI epsilon")
#       }
#     }
#
#     ### decision times
#
#     if(time[1] != "none"){
#       outputSTRATtime = coef(summary(fit2))
#       outputSTRATtime = cbind(outputSTRATtime,
#                               "z"=coef(summary(fit2))[,1]/ coef(summary(fit2))[,2])
#       outputSTRATtime = cbind(outputSTRATtime,
#                               "P>|z|"=2*(1-pnorm(abs(outputSTRATtime[,3]))))
#       outputSTRATtime = cbind(outputSTRATtime,confint(fit2))
#       stratOutput = outputSTRATtime
#     }
#
#     ### confidence judgments
#
#     if(conf[1] != "none"){
#       outputSTRATconfidence = coef(summary(fit3))
#       outputSTRATconfidence = cbind(outputSTRATconfidence,
#                                     "z"=coef(summary(fit3))[,1]/coef(summary(fit3))[,2])
#       outputSTRATconfidence = cbind(outputSTRATconfidence,
#                                     "P>|z|"=2*(1-pnorm(abs(outputSTRATconfidence[,3]))))
#       outputSTRATconfidence = cbind(outputSTRATconfidence,confint(fit3))
#       stratOutput = rbind(stratOutput,outputSTRATconfidence)
#     }
#
#     stratOutput = rbind(epsilon,stratOutput)
#
#     ###################################################################
#     ################ Output ###########################################
#     ###################################################################
#
#
#     if(length(epsilon) > 0){
#       if(round(epsilon[1],6) > .5){
#
#         messageerror = paste(messageerror,
#                              "### NOTE THAT EPSILON IS > .5 ###")
#       }}
#
#     print("###################################################################")
#     print (paste("|","participant =",partic[numbpartic],"| strategy =",strategy,"|"))
#     print (paste("| Log-Likelihood =",round(logliks,2),"| BIC =",round(BICs,2),"|"))
#
#     if(messageerror!=""){
#       print(messageerror)
#     }
#
#     print(stratOutput)
#     print("###################################################################")
#
#     if (is.matrix(stratOutput)==TRUE){
#       memory = rbind(memory,cbind("participant"=partic[numbpartic],
#                                   "strategy name" = strategy,"log-lik"=logliks,
#                                   "BIC"=BICs,stratOutput))
#     }else{
#       memory = rbind(memory,c("participant"=partic[numbpartic],
#                               "strategy name" = strategy,"log-lik"=logliks,
#                               "BIC"=BICs,stratOutput))
#     }
#   } # end participant loop
#
#   ### save data
#
#   if(!missing(output) && !is.null(output) && output != ""){
#     write.csv(memory , file = output,row.names = TRUE)
#   }
#
#   epsilon = ifelse(length(epsilon) == 0, .5, epsilon)
#
#   resulUpd = c(BIC = BICs, epsilon = epsilon[1])
#   return(resulUpd)
#
# } # end function
