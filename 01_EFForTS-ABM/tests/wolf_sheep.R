#set paths

netlogopath <- file.path("/home/ecomod/NetLogo 6.1.1")
netlogoversion <- "6.1.1"


if (file.exists(netlogopath)){
  print('netlogopath exists')
}else{
  stop('Please specify the folder that contains Netlogo')
}

modelpath <- file.path("/home/ecomod/NetLogo 6.1.1/app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
outpath <- file.path("EFForTS-ABM/01_EFForTS-ABM/tests/output")#/EFForTS-ABM/01_EFForTS-ABM/tests/

if (file.exists(modelpath)){
  print('modelpath exists')
}else{
  stop('Please specify the folder that contains the model')
}


if (file.exists(outpath)){
  print('outpath exists')
}else{
  stop('Please specify the folder that contains the outpath')
}



#attach paths, set available memory for JAVA, set NetLogo version
nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname="wolf-sheep",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup",
                            idgo="go",
                            idfinal=NA_character_,
                            idrunnum=NA_character_,
                            runtime=50,
                            evalticks=seq(40,50),
                            metrics=c("count sheep", "count wolves", "count patches with [pcolor = green]"),
                            variables = list('initial-number-sheep' = list(min=50, max=150, qfun="qunif"),
                                             'initial-number-wolves' = list(min=50, max=150, qfun="qunif")),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             "grass-regrowth-time" = 30,
                                             "sheep-gain-from-food" = 4,
                                             "wolf-gain-from-food" = 20,
                                             "sheep-reproduce" = 4,
                                             "wolf-reproduce" = 5,
                                             "show-energy?" = "false"))


#nl@simdesign <- simdesign_eFast(nl=nl, samples=10, nseeds=3)

nl@simdesign <-  simdesign_simple(nl, nseeds=1)




setsim(nl, "simoutput") <- run_nl_all(nl)
