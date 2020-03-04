library(tidyverse)
library(readr)
library(here)
library(tidytext)
# library(wordcloud)
library(ggwordcloud)

aliases <- read_csv(here("Aliases.csv"))
names(aliases)
str(aliases)

receivers <- read_csv(here("EmailReceivers.csv"))
names(receivers)
str(receivers)

emails <- read_csv(here("Emails.csv"))
names(emails)
str(emails)

persons <- read_csv(here("Persons.csv"))
names(persons)
str(persons)


data("stop_words")

total_words <- emails %>%  
  unnest_tokens(word, ExtractedBodyText) %>%  
  anti_join(stop_words) %>% 
  group_by(word) %>%  
  summarize(n= n())


pal <- wesanderson::wes_palette("BottleRocket2", n = 2)

uni_sw <- data.frame(word = c("13", "date", "05", "04841", "select", "c05739812", "pm", "time"))


libya <- emails %>%  
  mutate(ExtractedBodyText = str_to_lower(ExtractedBodyText)) %>% 
  filter(str_detect(ExtractedBodyText, "libya") == "TRUE") %>%  
  select(ExtractedSubject, ExtractedBodyText) 
libya <- append(libya$ExtractedSubject, libya$ExtractedBodyText)

libya <- data.frame(libya = libya, stringsAsFactors = FALSE) %>% 
  filter(!is.na(libya)) %>%  
  unnest_tokens(word, libya) %>%  
  anti_join(stop_words) %>%  
  count(word, sort = TRUE) %>%  
  filter(!word %in% uni_sw$word) %>%  
  filter(n >= 45) %>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))


libya_plot <- ggplot(data = libya, aes(label = word,
                                       size = n,
                                       angle = angle,
                                       color = n)) +
  geom_text_wordcloud(area_corr = TRUE,
                      eccentricity = .8,
                      shape = "diamond") +
  scale_radius(range = c(2,25), limits = c(0, NA)) + 
  labs(title = "Hillary Clinton's emails with the word 'Liyba' in it") +
  theme_light(base_size = 12, base_family= "serif") + 
  theme(panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid")) +
  scale_color_gradient(low = pal[1], high = pal[2]) 
libya_plot



#wordcloud package
  # with(wordcloud(word,
  #                n,
  #                min.freq = 1,
  #                max.words=200,
  #                random.order=FALSE,
  #                rot.per=0.35, 
  #                colors=brewer.pal(8, "Dark2")))



  
