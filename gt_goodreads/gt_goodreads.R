# Getting started with {gt} package by Rich Iannone
# Using the Goodreads dataset from Kaggle

# prerequisites 
library(gt)
library(tidyverse)
library(emo)
library(scales)
library(ghibli)

books <- readr::read_csv("books.csv")

# explore
skimr::skim(books)

# process
books2 <- 
  books %>%
  drop_na() %>% 
  select(title, authors, publication_date, publisher,
         average_rating, ratings_count) %>% 
  mutate(publication_date = as.Date(publication_date, "%m/%d/%Y")) %>% 
  filter(ratings_count > 1000000) %>%
  top_n(n = 5, average_rating) %>%
  arrange(desc(average_rating))
  
# image paths
img_path <- c(
  "hp_halfblood.png", "hp_azkaban.png", 
  "hp_phoenix.png", "hp_chamber.png", 
  "fellowship_ring.png"
)

books2$img_path <- img_path

# here comes gt
books_gt <- 
  books2 %>% 
  gt() %>%
  
  # move columns
  cols_move(
    columns = vars(average_rating, ratings_count,
                   publication_date, publisher),
    after = vars(authors)
  ) %>% 
  
  # format ratings count
  fmt_number(
    columns = vars(ratings_count),
    use_seps = T,
    decimals = 0
  ) %>% 
  
  # format published date
  fmt_date(
    columns = vars(publication_date),
    date_style = 6
  ) %>% 
  
  # align column cells
  cols_align(
    align = "center",
    vars(title, authors, average_rating, ratings_count,
         publication_date, publisher, img_path)
  ) %>% 
  
  # add the images
  text_transform(
    cells_body(vars(img_path)),
    fn = function(img_path) {
      
      # change the default height of local_image()
      local_image120 <- function(path) {
        image <- 
          local_image(
            filename = path,
            height = 120
          )
        return(image)
      }
      
      lapply(img_path, local_image120)
    }
  ) %>% 
  
  cols_move(
    columns = vars(img_path),
    after = vars(title)
  ) %>% 
  
  # hide the title column
  cols_hide(
    columns = vars(title)
  ) %>% 
  
  # styling texts with tab_style()
  tab_style(
    style = cell_text(style = "italic", weight = "bold"),
    locations = cells_body(
      columns = vars(average_rating)
    )
  ) %>% 
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = vars(img_path, authors, average_rating,
                     ratings_count, publication_date, publisher)
    )
  ) %>% 
  
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_body(
      columns = vars(authors, ratings_count, 
                     publication_date, publisher)
    )
  ) %>% 
  
  # column names
  cols_label(
    img_path = "Book", authors = "Author/s",
    average_rating = "Average Rating",
    ratings_count = "Ratings Count",
    publication_date = "Date Published",
    publisher = "Publisher"
  ) %>% 
  
  # table title
  tab_header(
    title = md(paste0("5 of the Highest Rated Books on **Goodreads** ", emo::ji("book"))),
    subtitle = "Published books from 1900-2010"
  ) %>% 
  
  # add some footnotes
  tab_footnote(
    footnote = md("*Only books with > 1 million ratings count are inlucded*"),
    locations = cells_column_labels(
      columns = vars(ratings_count)
    )
  ) %>% 
  
  # add some source notes
  tab_source_note(
    source_note = md(
      "Table: Andre De Vera | Data Source: Goodreads-books by Soumik - [kaggle.com](https://www.kaggle.com/jealousleopard/goodreadsbooks)."
    )
  ) %>% 
  
  # setting the theme 
  tab_options(
    table.font.names = "Arial",
    table.background.color = "#F5F5DC",
    heading.background.color = NULL,
    column_labels.background.color = "#E6E6FA",
    footnotes.background.color = "#E6E6FA",
    source_notes.background.color = "#E6E6FA",
  # footnotes.font.size = px(13),
    source_notes.font.size = px(13)
  ) %>% 
  
  # change the average rating cells color
  data_color(
    columns = vars(average_rating),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ghibli::LaputaLight"
      ) %>% as.character(),
      domain = NULL
    )
  )

books_gt

# save as PNG
books_gt %>% 
  gtsave("goodreads5.png")
