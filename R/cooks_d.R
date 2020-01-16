# cooks_d = function(meta){
#   all_vals = extractData(meta)
#
#   m_call = meta$call
#   model_data = meta$data
#
#   clusters = unique(model_data$cluster)
#
#   dats = lapply(seq_along(clusters), function(c) {
#     model_data[model_data$cluster != clusters[c], ]
#   })
#
#   models = lapply(seq_along(dats), function(x) {
#     meta3(y, v, cluster, data = dats[[x]])
#   })
#
#   get_d = function(modi, hat = all_vals){
#     vals = extractData(modi)
#     ui = vals$estimate
#     u = hat$estimate
#     t = hat$t2 + hat$t2_3
#     v = hat$SE^2
#
#     d = (u - ui)^2 / (v + t)
#
#     d2 <- crossprod(dfb, svb) %*% dfb
#     dfb = u - ui
#
#
#   }
#
#   data.frame(cluster = clusters,cooks.d = unlist(lapply(models, get_d)))
#
#
#   # (estimate - estimate_without)^2 /
#
#
#
# }
