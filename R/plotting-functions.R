#' #' extractSlopes
#' #'
#' #' Grabs slope coefficient data
#' #'
#' #' @param model a model.
#' #' @export extractSlopes
#'
#'
#' ninjaForest = function(model,summary = NULL,save = F,height = 16, width = 16,f_name = test.png, title = ""){
#'   #this chunk sets up summary object
#'   summary = extractData(model) %>%
#'     select(p,lbound,ubound)
#'   names(summary) = c("p","lower","upper")
#'   summary$type = "summary"
#'   summary$setting = "pooled"
#'   summary$year = 1
#'   summary$cluster = "pooled"
#'
#'   dat = model$data %>%
#'     mutate(se = sqrt(v),
#'            lower = logit2prob(y - 1.96*se),
#'            upper = logit2prob(y + 1.96*se)) %>%
#'     mutate(y = logit2prob(y), type = "data", setting = "effects") %>%
#'     dplyr::select(p = y, lower,upper,type,setting, cluster)
#'
#'   dat$author = lapply(dat$cluster, function(x){
#'     x = as.numeric(as.character(x))
#'     f[f$study_id == x, "author"] %>% unlist() %>% .[1]
#'   }) %>% unlist
#'
#'   dat$year = lapply(dat$cluster, function(x){
#'     x = as.numeric(as.character(x))
#'     f[f$study_id == x, "year"] %>% unlist() %>% .[1]
#'   }) %>% unlist
#'   dat = dat[order(dat$year),]
#'   dat$cluster = paste(dat$author,dat$year)
#'   #create empty columns in summary for author and year
#'   fun = dat[,c("cluster","p","lower","upper","type","setting","year")]
#'   dat = rbind(fun,
#'               summary)
#'   dat=dat[order(dat$year),]
#'
#'   test = ggplot(dat, aes(y = reorder(cluster,year), x = p, xmin = lower, xmax = upper)) + geom_point(color = "black") +
#'     geom_point(data = dat[dat$type == "summary",],color = 'black', shape = 18, size = 4.5)+
#'     #add the CI error bars
#'     geom_errorbarh(height=.1)+
#'     #Specify the limits of the x-axis and relabel it to something more meaningful
#'     scale_x_continuous(name='proportion')+
#'     ylab(NULL)+
#'     ggtitle(title)+
#'     geom_vline(xintercept=.5, color='black', linetype='dashed') +
#'     facet_grid(setting~., scales= 'free', space='free')+
#'     theme_classic()
#'   if(save == T){
#'     ggsave(f_name, path = getwd(), height = height, width = height, units = "cm", dpi = 600)
#'
#'   }
#'   test
#' }
