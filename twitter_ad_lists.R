library(tidyverse)
library(igraph)
library(ggraph)


# import data, parse, and write to CSV file
files <- list.files('../twitter_advertiser_lists/', pattern = '.txt')
twitter_ads <- data.frame()

for (file_name in files) {
  source_name = gsub('.txt', '', file_name)
  ad_tech_single <- read_csv(paste('../twitter_advertiser_lists/', file_name, sep = '')) %>%
    mutate(source = source_name)
  twitter_ads <- bind_rows(twitter_ads, ad_tech_single)
}

write_csv(twitter_ads, '../twitter_advertiser_lists/twitter_ads.csv')


# summary statistics
advertiser_count <- twitter_ads %>%
  count(advertiser, sort = TRUE)

user_count <- twitter_ads %>%
  count(source, sort = TRUE)

# get list of unique sources, adnets
sources <- unique(twitter_ads$source)
adnets <- unique(twitter_ads$advertiser)

# function to gather unique third-party domain calls
gather_targets <- function(database, site) {
  return(database %>%
           filter(source == site) %>%
           select(advertiser) %>%
           unique() %>%
           unlist() %>%
           paste())
}


# count third-party domain calls and compare each pair of sites for overlaps
ad_intersects <- tibble()

for (site1 in sources) {
  for (site2 in sources) {
    targets1 <- gather_targets(twitter_ads, site1)
    targets2 <- gather_targets(twitter_ads, site2)
    ad_intersects <- rbind(ad_intersects, 
          as_tibble(cbind(site1, site2, 
                as.numeric(length(targets1)), 
                as.numeric(length(targets2)),
                as.numeric(length(intersect(targets1, targets2))))))
  }
}

colnames(ad_intersects) <-
  c('site1', 'site2', 'site1_targets', 'site2_targets', 'intersects')


# calculate share product between two sites
ad_intersects <- as_tibble(ad_intersects) %>%
  mutate(intersects_site1_share = as.numeric(intersects) / as.numeric(site1_targets),
         intersects_site2_share = as.numeric(intersects) / as.numeric(site2_targets),
         share_product = intersects_site1_share * intersects_site2_share)

adnet_similarities <- ad_intersects %>%
  select(site1, site2, share_product) %>%
  spread(site2, share_product)

write_csv(ad_intersects, '../twitter_advertiser_lists/twitter_ad_intersections.csv')

# plot share products for each site pair on a heat map
png(filename = '../twitter_advertiser_lists/twitter_ad_heatmap.png', width=1000, height=800)

h <- ad_intersects %>%
  ggplot(aes(site1, site2)) + 
  geom_tile(aes(fill = share_product),
            colour = "white") + 
  scale_fill_gradient(low = "white",
                      high = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(title="Adnet similarity")) +
  xlab('') +
  ylab('') +
  ggtitle('Similarity of Twitter advertiser targeting')

print(h)
dev.off()


# network graph
set.seed(2017)
a <- grid::arrow(type = 'closed', length = unit(.1, 'inches'))

network <- twitter_ads %>%
  select(source, advertiser) %>%
  group_by(source, advertiser) %>%
  summarize(count = n()) %>%
  #filter(count >= 5) %>%
  graph_from_data_frame()

# save network graph to file
# CHANGE FILE NAME HERE
png(filename = '../twitter_advertiser_lists/twitter_ad_network_graph.png', width=2000, height=1600)

g <- ggraph(network, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = count), show.legend = FALSE, arrow = a) +
  geom_node_point(color = 'lightblue', size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  ggtitle('Twitter ad network graph')

print(g)
dev.off()

