


a=5
no_cores <- detectCores()-1
cl <- makeCluster(no_cores) 
 clusterExport(cl,"a")
parLapply(cl,1:3,fun = function(x){
  a<<-a+5
})
a
clusterExport(cl,"a",envir = environment()) 
a
