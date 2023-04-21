library(jsonlite)
library(ggplot2)
j <- jsonlite::fromJSON('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf/amewoo_2021_58km_obs1.0_ent0.003_dist0.05_pow0.2.json')
jdf <- as.data.frame(j)

pdf('loss_dict.pdf')
par(mfrow = c(2,2))
for (i in names(jdf)){
  plot(jdf[[i]], ylab = i)
}
dev.off()

#p <- ggplot(jdf, aes(row.names(jdf, jdf))) + geom_point()
#p + facet_wrap(vars(names(jdf)))
