pacman::p_load("tidyverse",
               "readxl",
               "ggplot2",
               "lubridate",
               "stringr")


consolidated <- read_csv("bp-stats-review-2022-consolidated-dataset-narrow-format.csv")

glimpse(consolidated)
variables <- unique(consolidated$Var)
variables

hdi <- read_csv("HDI.csv") %>%
  select(iso3, hdicode, country, hdi_2019)


consolidated_filtered <- consolidated %>%
  select(Country, ISO3166_alpha3, Year, Region, SubRegion, Var, Value) %>%
  filter(Var %in% c("primary_ej_pc", "primary_ej", "renewables_ej", "co2_combust_mtco2", "co2_combust_pc", "ren_power_twh", "co2_combust_pc", "co2_mtco2", "co2_combust_per_ej")) %>%
  rename(iso3 = ISO3166_alpha3) %>%
  left_join(hdi) %>%
  select(!country)



energy_df_indexed <- consolidated_filtered %>%
  group_by(Country) %>%
  filter(Year >= 2015) %>%
  arrange(Year) %>%
  pivot_wider(c(Country, Region, Year, SubRegion, iso3, hdicode, hdi_2019), names_from = Var, values_from = Value) %>%
  mutate(non_renew_ej = primary_ej - renewables_ej,
         non_renew_indexed = (100 * non_renew_ej/ first(non_renew_ej)),
         primary_pc_indexed = (100 * primary_ej_pc/ first(primary_ej_pc)),
         primary_indexed = (100 * primary_ej/ first(primary_ej)),
         renewables_indexed = (100 * renewables_ej/ first(renewables_ej)),
         percent_renewables = renewables_ej/primary_ej,
         percent_renew_indexed = (100 * percent_renewables/first(percent_renewables)))

energy_df_indexed


# Growth in primary energy


growth_primary <- consolidated_filtered %>%
  filter(Var %in% c("primary_ej", "renewables_ej"),
         Country == "Total World") %>%
  pivot_wider(names_from = 'Var', values_from = 'Value') %>%
  mutate(non_renew_ej = primary_ej - renewables_ej) %>%
  pivot_longer(!c(Country, Year, Region, SubRegion, iso3, hdicode, hdi_2019), names_to = "Var", values_to = "Value")

growth_primary$Var <- factor(growth_primary$Var , levels=c("primary_ej", "renewables_ej", "non_renew_ej") )

world_growth_primary <- ggplot(growth_primary %>%
                                 filter(Var %in% c("non_renew_ej", "renewables_ej")),
                               aes(x = Year,
                                   y = Value,
                                   group = Var,
                                   fill = factor(ifelse(Var == "renewables_ej","x", 
                                                        ifelse(Year > 2015 ,"Years since y agreement","z")))))+
  geom_bar(stat= "identity") + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 20),
        panel.background = element_blank())+
  scale_fill_manual(values = c("#26838E", "#482878", "#440154")) +
  labs(title = "Hold your applause",
       subtitle = "Renewable energy is powering the global recovery by covering the world's increasing energy demands, \nbut we're burning no fewer fossiles than we were in 2019") +
  ylab(label = "Global energy consumption, Exajoules\n ") +
  annotate("text", x = 2008, y = 600, label = "Paris Agreement,\nDecember 2015") +
  geom_curve(x = 2012, y = 600,
             xend = 2015, yend = 570,
             curvature = -0.2,
             size = 0.1,
             arrow = arrow(length = unit(0.03, "npc")))


world_growth_primary



# combine percent renewable with per capita indexed since 




labels = c("China", 
           "US", 
           "United Kingdom",
           "Germany", 
           "Denmark",
           "India",
           "Vietnam",
           "Bangladesh",
           "Pakistan",
           "Iraq",
           "Brazil",
           "Australia",
           "New Zealand",
           "France",
           "Saudi Arabia",
           "Venezuela",
           "Russian Federation",
           "Finland",
           "Sweden",
           "Turkmenistan",
           "Oman",
           "Sri Lanka",
           "Lithuania",
           "Croatia",
           "Bulgaria",
           "Israel",
           "Mexico",
           "Qatar",
           "Switzerland",
           "Romania",
           "Italy",
           "South Africa",
           "Argentina",
           "Chile",
           "Austria",
           "Spain",
           "Portugal",
           "Iceland")



# With percent renewables
scatter_percent <- ggplot(energy_df_indexed %>%
                            left_join(hdi)%>%
                            filter(Year == 2021,
                                   # renewables_ej > 0,
                                   # percent_renew_indexed < 10000,
                                   !grepl("Other", Country),
                                   !grepl("Total", Country)),
                          aes(x = percent_renewables,
                              y = primary_pc_indexed,
                              colour = hdi_2019,
                              label = Country,
                              size = primary_ej)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = c("0%","10%", "20%", "30%", "40%")) +
  scale_y_continuous(labels = c("-40%", "-20%", "0", "20%", "40%")) +
  scale_alpha(guide = "none") +
  scale_size(range = c(2, 25), guide = "none") +
  scale_colour_viridis_c(name = "Human\nDevelopment\nIndex") +
  geom_hline(yintercept = 100) +
  geom_text(data=subset(energy_df_indexed, Country %in% labels & Year == 2021),
            colour = "black",
            size = 3)  +
  labs(title = "Fueling development",
       subtitle = "Growth in energy consumption is concentrated in the developing world, where the transition to renewables since the Paris Agreement is less advanced.
       Size of dot indicates total energy consumption in 2021",
       x = "Share of energy consumption from renewable sources, 2021",
       y = "Change in per capita energy consumption since 2015\n ")+
  annotate("text", x = 0.35, y = 120, label = "Increased energy\nconsumption") +
  annotate("text", x = 0.35, y = 80, label = "Decreased energy\nconsumption") +
  annotate("text", x = 0.35, y = 60, label = "Source: BP Statistical Review of World Energy 2022, United Nations", size = 3) +
  theme(plot.title = element_text(face = "bold", size = 20),
        panel.background = element_blank())

scatter_percent
# 
# 
# # With indexed change renewables
# scatter_index <- ggplot(energy_df_indexed %>%
#                     left_join(hdi)%>%
#                     filter(Year == 2021,
#                            # renewables_ej > 0,
#                            percent_renew_indexed < 6000,
#                            !grepl("Total", Country),
#                            !grepl("Other", Country)),
#                   aes(x = percent_renew_indexed,
#                       y = primary_pc_indexed,
#                       colour = hdi_2019,
#                       label = Country,
#                       size = primary_ej)) +
#   geom_point() +
#   scale_size(range = c(1, 15), name="total consumption") +
#   scale_colour_viridis_c() +
#   geom_hline(yintercept = 100) +
#   geom_text(colour = "black", size = 2)  +
#   labs(title = "Fossils are still fuelling human development",
#        subtitle = "Energy consumption growth is concentrated in the developing world,
#        where the transition to renewables is progressing more slowly",
#        x = "% growth in proportion of energy from renewables",
#        y = "% change in per capital energy consumption since 2015")
# 
# scatter_index
# 
# 

# Emissions

consolidated_filtered$Var <- factor(consolidated_filtered$Var , levels=c("co2_combust_pc", "co2_combust_per_ej", "co2_combust_mtco2"))

Emissions <- ggplot(consolidated_filtered %>%
                      filter(Var %in% c("co2_combust_pc", "co2_combust_per_ej", "co2_combust_mtco2"),
                             Country %in% c("Total Africa", "Total Asia Pacific", "Total CIS", "Total Europe", "Total Middle East", "Total North America", "Total S. & Cent. America")) %>%
                      mutate(Country = gsub("Total ", "", Country),
                             Var = gsub("co2_combust_pc", "Emissions per capita", Var),
                             Var = gsub("co2_combust_per_ej", "Emissions per Exajoule", Var),
                             Var = gsub("co2_combust_mtco2", "Total Co2 emissions", Var)),
                    aes(x = Year,
                        y = Value,
                        fill = Country,
                        label = Region)) +
  geom_area(alpha = 0.7) +
  scale_fill_viridis_d(name = "Region") +
  labs(title = "Not out of the woods",
       subtitle = "Although per capita emissions from energy are falling, and energy is growing cleaner, total global\n
       emissions from energy are back to 2019 levels",
       y = "Millions of tonnes",
       x = "") +
  facet_wrap(~ Var,
             scales = "free") +
  theme(plot.title = element_text(face = "bold", size = 20),
        panel.background = element_blank())

Emissions



# How much renewable capacity have countries added? Keep it to since 2015. renewable generation in 2015 and 2021, difference. 

growth_renewables <- energy_df_indexed %>%
  select(Country, Year, renewables_ej) %>%
  filter(Country %in% c("Total World", "China", "India", "Brazil")) %>%
  pivot_wider(names_from = Country, values_from = renewables_ej) %>%
  mutate("Rest of world" = `Total World` - (Brazil + China + India)) %>%
  select(!`Total World`) %>%
  pivot_longer(!Year, names_to = "Country", values_to = "Value")

growth_renewables

drive_the_renewable_bus <- ggplot(growth_renewables,
                                  aes(x = Year,
                                      y = Value,
                                      fill = Country)) +
  geom_col()

drive_the_renewable_bus


growth_renewables_all <- energy_df_indexed %>%
  select(Country, Year, ren_power_twh) %>%
  filter(!grepl("Total", Country),
         !grepl("Other", Country)) %>%
  pivot_wider(names_from = Year, values_from = "ren_power_twh") %>%
  mutate(twh_added = `2021` - `2015`) %>%
  arrange(desc(twh_added)) %>%
  head(10) %>%
  mutate(colour = "colour")

biggest_adders <- ggplot(growth_renewables_all,
                         aes(x = reorder(Country, twh_added),
                             y = twh_added,
                             fill = colour)) +
  geom_col() + 
  scale_fill_manual(values = c("#1F9D89")) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 20),
        panel.background = element_blank())+
  labs(title = "Great leap",
       subtitle = "Between 2015 and 2021, China added 873 TWh of renewable energy generation capacity,\nmore than twice the US's renewable capacity gain in the same period") +
  ylab(label = "Renewable energy capacity increase 2015-2021, TWh\n ") 

biggest_adders



# `How many Terawatt Hours are in a Exajoule? The answer is one Exajoule is equal to 277.78 Terawatt Hours.`Ren_power is generation, renewables_ej is consumption


consolidated_filtered_for_generation <- consolidated %>%
  select(Country, ISO3166_alpha3, Year, Region, SubRegion, Var, Value) %>%
  filter(Var %in% c("wind_twh", "solar_twh", "ren_power_twh")) %>%
  rename(iso3 = ISO3166_alpha3) %>%
  left_join(hdi) %>%
  select(!country)

for_renew_growth <- c( "Vietnam",
                       "Uzbekistan",
                       "India",
                       "Bangladesh",
                       "Brazil",
                       "Morocco",
                       "Pakistan"
)


IEA_test <- c( "China",
               "US",
               "EU",
               "India",
               "Total Europe"
)

these_ones <- c("Morocco", "Iran","Algeria", 
                "Sri Lanka",
                "Philippines")

growth_renewable_gen <- consolidated_filtered_for_generation %>%
  select(!c(iso3, Region, SubRegion, hdicode, hdi_2019)) %>%
  filter(Country %in% these_ones,
         Year >= 2015) %>%
  pivot_wider(names_from = Var, values_from = Value) %>%
  mutate(wind_sol = solar_twh + wind_twh,
         Other = ren_power_twh - wind_sol) %>%
  pivot_longer(!c(Country, Year), names_to = "Var", values_to = "Value") %>%
  group_by(Country, Var) %>%
  arrange(Year) %>%
  mutate(Diff = Value - lag(Value)) %>%
  drop_na()

growth_renewable_gen

growth_renewable_gen$Country <- factor(growth_renewable_gen$Country , levels=c("Azerbaijan", "Algeria", "Sri Lanka","Iran", "Philippines", "Morocco") )

growth_bar <- ggplot(growth_renewable_gen %>%
                       filter(
                         # Var == "ren_power_twh",
                         Var %in% c("solar_twh", "wind_twh", "Other")
                         ,
                         Diff >= 0
                       ) %>%
                       mutate(Var = gsub("solar_twh", "Solar", Var),
                              Var = gsub("wind_twh", "Wind", Var)),
                     aes(x = Year,
                         y = Diff,
                         fill = Var)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Source") +
  facet_wrap(~ Country,
             # scales = "free_y",
             nrow = 1
  ) +
  labs(title = "Meaningful progress",
       subtitle = "Net annual increase in renewable energy generated, 2016-2021") +
  ylab(label = "Terawatt hours") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.title = element_text(face = "bold", size = 20)) +
  geom_hline(yintercept = 0) 

growth_bar


