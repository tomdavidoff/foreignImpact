# plotElasticityNonResident
# use ChatGPT-fuzzy-matched data on elasticities from paxaoi and CHSP
# Tom Davioff

library(data.table)
library(ggplot2)

df <- fread("data/derived/CMA_Supply_Elasticity_and_Non-Resident_Ownership.csv")
setnames(df,c("CMA", "Supply Elasticity", "Non-Resident Ownership (%)"), c("CMA", "elasticity", "NonResident"))
ggplot(df[,.(CMA, elasticity, `NonResident`)],
       aes(x=NonResident, y=elasticity, label=CMA)) +
  geom_point() +
  # make text smaller
  geom_text(size=2, vjust=1.5, hjust=0.5) +
  # ylimit 3
  ylim(0, 3) +
  labs(title="Elasticity of Supply vs Non-Resident Ownership",
       x="Non-Resident Ownership (%)",
       y="Elasticity of Supply") +
  theme_bw()
ggsave("text/elasticityOwnership.png")
print(df)
