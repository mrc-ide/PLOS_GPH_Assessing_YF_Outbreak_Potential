library(didehpc)
options(didehpc.cluster = "fi--didemrchnb")
didehpc_config()
setwd("T:/Keith/Run2022_10_G_IGL_new2")

root <- "contexts"
packages <- c("YellowFeverDynamics")
sources <- c("R/run_function_multi.R")
package_sources <- conan::conan_sources(c("mrc-ide/odin.dust","mrc-ide/YellowFeverDynamics"))
ctx <- context::context_save(root,packages = packages, sources = sources,package_sources = package_sources)
obj <- didehpc::queue_didehpc(ctx)

#Get ID(s) and stop run(s)
obj$task_status()
ids <- obj$task_status()
obj$task_delete(names(ids))
obj$task_status()
