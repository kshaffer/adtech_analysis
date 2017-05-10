library(tidyverse)
library(igraph)
library(ggraph)


# function to extract domains from URL
extract_domain <- function(url) {
  return(gsub('www.', '', unlist(strsplit(unlist(strsplit(as.character(url), '//'))[2], '/'))[1]))
}

# function to extract protocol from URL
extract_protocol <- function(url) {
  return(unlist(strsplit(as.character(url), '://'))[1])
}

# function to extract root domain from full (sub)domain
extract_root_domain <- function(domain) {
  domain_list <- unlist(strsplit(domain, '.', fixed = TRUE))
  return(paste(domain_list[length(domain_list)-1], domain_list[length(domain_list)], sep = '.'))
}


# import data, parse, and write to CSV file
files <- list.files('sources/', pattern = '.txt')
ad_tech_results <- data.frame()

for (file_name in files) {
  source_name = gsub('.txt', '', file_name)
  ad_tech_single <- read_csv(paste('sources/', file_name, sep = ''), col_names = FALSE) %>%
    transmute(url = X1) %>%
    mutate(source = source_name,
           domain = sapply(url, extract_domain),
           root_domain = sapply(domain, extract_root_domain),
           protocol = sapply(url, extract_protocol))
  ad_tech_results <- rbind(ad_tech_results, ad_tech_single)
}

write_csv(ad_tech_results, 'results/all_adtech_urls.csv')


# summary statistics
call_summary_with_source <- ad_tech_results %>%
  group_by(source, root_domain, protocol) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

call_summary <- ad_tech_results %>%
  group_by(root_domain, protocol) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

protocol_summary <- ad_tech_results %>%
  group_by(source, protocol) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

protocol_summary_spread <- ad_tech_results %>%
  group_by(source, protocol) %>%
  summarize(count = n()) %>%
  spread(protocol, count) %>%
  arrange(desc(http))

ad_domain_summary <- ad_tech_results %>%
  group_by(root_domain) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# write to CSV files
write_csv(call_summary_with_source, 'results/source_adnet_tallies.csv')
write_csv(call_summary, 'results/adnet_tallies_with_security.csv')
write_csv(protocol_summary_spread, 'results/source_security_summary.csv')
write_csv(ad_domain_summary, 'results/adnet_tallies.csv')


# get list of unique sources, adnets
sources <- unique(ad_tech_results$source)
adnets <- unique(ad_tech_results$root_domain)

# function to gather unique third-party domain calls
gather_targets <- function(database, site) {
  return(database %>%
           filter(source == site) %>%
           select(root_domain) %>%
           unique() %>%
           unlist() %>%
           paste())
}


# count third-party domain calls and compare each pair of sites for overlaps
ad_intersects <- tibble()

for (site1 in sources) {
  for (site2 in sources) {
    targets1 <- gather_targets(ad_tech_results, site1)
    targets2 <- gather_targets(ad_tech_results, site2)
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

write_csv(ad_intersects, 'results/adnet_intersections.csv')

# plot share products for each site pair on a heat map
png(filename = 'plots/adtech_heatmap.png', width=1000, height=800)

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
  ggtitle('Similarity of ad-tech targeting')

print(h)
dev.off()


# network graph
set.seed(2017)
a <- grid::arrow(type = 'closed', length = unit(.1, 'inches'))

network <- ad_tech_results %>%
  select(source, root_domain) %>%
  group_by(source, root_domain) %>%
  summarize(count = n()) %>%
  #filter(count >= 5) %>%
  graph_from_data_frame()

# save network graph to file
# CHANGE FILE NAME HERE
png(filename = 'plots/adtech_network_graph.png', width=2000, height=1600)

g <- ggraph(network, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = count), show.legend = FALSE, arrow = a) +
  geom_node_point(color = 'lightblue', size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  ggtitle('Ad-tech network graph')

print(g)
dev.off()

