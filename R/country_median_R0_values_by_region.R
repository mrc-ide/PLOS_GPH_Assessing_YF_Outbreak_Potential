regions_subset=unique(subset$region)
R0_median_values=rep(NA,length(regions_subset))
for(i in 1:length(regions_subset)){
  R0_median_values[i]=median(subset$R0[subset$region==regions_subset[i]])
}