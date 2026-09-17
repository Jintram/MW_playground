

library(ggplot2)
library(openxlsx)

# Read data
dir_data  <- "/Users/m.wehrens/Documents/TEACHING/Basics_fluorescence_microscopy_2026-01/Admin/"
path_data <- "experience_queryresponse.xlsx"
df_query <- openxlsx::read.xlsx(paste0(dir_data, path_data))
head(df_query)
df_query$Response_fct <- 
    factor(
        df_query$Response,
        levels = c("No experience so far",
                    "Minor experience",
                    "Major experience")
    )

# a function to set all text sizes in a ggplot2 object to "font_size"
set_auto_fontsize <- function(font_size = 14) {
  theme(
    text = element_text(size = font_size),
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size),
    legend.title = element_text(size = font_size),
    legend.text = element_text(size = font_size),
    plot.title = element_text(size = font_size + 2, hjust = 0.5)
  )
}

# Make a bar graph of the responses
p <- ggplot(df_query, aes(x = Response_fct)) +
  geom_bar(position = "dodge") +
  labs(x = NULL,
       y = "Number of Responses",
       title = "With FIJI ..") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "none") +
  set_auto_fontsize() +
  # make sure that y-axis labels are 1, 5, 10, and gridlines every unit
  scale_y_continuous(breaks = seq(0, max(table(df_query$Response_fct)), by = 5),
                     minor_breaks = seq(0, max(table(df_query$Response_fct)), by = 1))
                        
# save the graph
ggsave(filename = paste0(dir_data, "BFM25_experience_responses.pdf"),
       plot = p,
       width = 5,
       height = 10,
       units = "cm",
       device = cairo_pdf())
