

# Statistics behind the differential gene expression -------------------------------------
# Note: this requires the binompval from the RaceID2 package


# amount of reads that are attributed to one gene in cluster 1
cluster1_reads_of_gene<-3
# and sum of all reads in cluster 1 (numbers here are made up)
cluster1_total<-20

# Same for cluster 2 (also made up)
cluster2_reads_of_gene<-5
cluster2_total<-20

# Now: if cluster 2 has an equal amount of gene expression as cluster 1, what is 
# the chance of finding the observed number of transcripts in cluster 2. For this we assume the
# probability of finding it is equal to read count in cluster 1 / total read count 
# all gene cluster 1 (which is true in the limit of infinite observations).
pvalue <- binompval(cluster1_reads_of_gene/cluster1_total,cluster2_total,cluster2_reads_of_gene)
pvalue_reversed <- binompval(cluster2_reads_of_gene/cluster2_total,cluster1_total,cluster1_reads_of_gene)
1-pbinom(cluster2_reads_of_gene,cluster2_total,cluster1_reads_of_gene/cluster1_total,lower.tail=TRUE)
print(pvalue)
print(pvalue_reversed)


# Testing whether multiplication with a factor changes anything -----
for (myfactor in c(1,10,100,1000)) {
  pvalue_test <- binompval((myfactor*cluster1_reads_of_gene)/(myfactor*cluster1_total),
                      myfactor*cluster2_total,
                      myfactor*cluster2_reads_of_gene)
  print(pvalue_test)
  
  
  pvalue_check<-1-pbinom(myfactor*cluster2_reads_of_gene,
                         myfactor*cluster2_total,
                         (myfactor*cluster1_reads_of_gene)/(myfactor*cluster1_total),
                         lower.tail=TRUE)
  print(pvalue_check)
}




# General shape of the binominal distribution -----------
x<-seq(1,cluster2_total)
df_show<-data.frame(
  x=x,
  y=pbinom(x,cluster2_total,cluster1_reads_of_gene/cluster1_total),
  d=dbinom(x,cluster2_total,cluster1_reads_of_gene/cluster1_total)
)

ggplot(data=df_show)+
  geom_line(aes(x=x,y=d),color='red')+
  geom_point(aes(x=x,y=d),color='red')+
  geom_line(aes(x=x,y=y))+
  geom_point(aes(x=x,y=y))








