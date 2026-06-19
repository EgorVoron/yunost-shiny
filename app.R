# app.R - финальная версия

library(shiny)
library(shinybusy)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(igraph)
library(ggraph)
library(wordcloud2)

# ===== ЗАГРУЗКА ДАННЫХ ПО УМОЛЧАНИЮ =====
load_default_data <- function() {
  poetry <- NULL
  texts <- NULL
  poets <- NULL
  
  if(file.exists("poetry.csv")) {
    poetry <- read.csv("poetry.csv", stringsAsFactors = FALSE)
    if("поэты" %in% names(poetry)) names(poetry)[names(poetry) == "поэты"] <- "poets"
    if("год" %in% names(poetry)) names(poetry)[names(poetry) == "год"] <- "year"
  } else if(file.exists("poetry_by_issue.csv")) {
    poetry <- read.csv("poetry_by_issue.csv", stringsAsFactors = FALSE)
    if("поэты" %in% names(poetry)) names(poetry)[names(poetry) == "поэты"] <- "poets"
    if("год" %in% names(poetry)) names(poetry)[names(poetry) == "год"] <- "year"
  } else if(file.exists("Юность содержания 1955-1974.xlsx - поэзия.csv")) {
    poetry <- read.csv("Юность содержания 1955-1974.xlsx - поэзия.csv", stringsAsFactors = FALSE)
    names(poetry)[names(poetry) == "поэты"] <- "poets"
    names(poetry)[names(poetry) == "год"] <- "year"
  }
  
  if(file.exists("texts.csv")) {
    texts <- read.csv("texts.csv", stringsAsFactors = FALSE)
  } else if(file.exists("тексты_Оля.csv")) {
    texts <- read.csv("тексты_Оля.csv", stringsAsFactors = FALSE)
  } else if(file.exists("Юность содержания 1955-1974.xlsx - тексты_Оля.csv")) {
    texts <- read.csv("Юность содержания 1955-1974.xlsx - тексты_Оля.csv", stringsAsFactors = FALSE)
  }
  
  if(file.exists("poets.csv")) {
    poets <- read.csv("poets.csv", stringsAsFactors = FALSE)
  } else if(file.exists("поэты.csv")) {
    poets <- read.csv("поэты.csv", stringsAsFactors = FALSE)
  } else if(file.exists("Юность содержания 1955-1974.xlsx - поэты.csv")) {
    poets <- read.csv("Юность содержания 1955-1974.xlsx - поэты.csv", stringsAsFactors = FALSE)
  }
  
  list(poetry = poetry, texts = texts, poets = poets)
}

default_data <- load_default_data()

# ===== СТОП-СЛОВА =====
stopwords_full <- c(
  "и", "а", "но", "да", "или", "либо", "то", "однако", "зато", "чтобы", "что", "как",
  "так", "потому", "поэтому", "также", "тоже", "причем", "притом", "если", "когда",
  "где", "куда", "откуда", "пока", "едва", "лишь", "только", "чуть", "ибо",
  "в", "во", "без", "безо", "до", "для", "за", "из", "изо", "к", "ко", "на", "над",
  "надо", "о", "об", "обо", "от", "ото", "по", "под", "подо", "при", "про", "с", "со",
  "у", "через", "сквозь", "между", "перед", "передо", "вокруг", "около", "мимо",
  "вдоль", "поперек", "после", "вслед", "навстречу", "благодаря", "согласно",
  "я", "ты", "он", "она", "оно", "мы", "вы", "они", "меня", "мне", "мной", "тебя",
  "тебе", "тобой", "его", "ему", "им", "ней", "ним", "них", "ими", "нас", "вам", "вас",
  "вами", "мой", "моя", "мое", "мои", "твой", "твоя", "твое", "твои", "наш", "наша",
  "наше", "наши", "ваш", "ваша", "ваше", "ваши", "свой", "своя", "свое", "свои",
  "этот", "эта", "это", "эти", "того", "тому", "тем", "этом", "этим", "этих", "тот","той", "этой", "эту", "свою",
  "та", "те", "тех", "все", "вся", "всех", "всем", "всеми", "весь", "всё", "сам",
  "сама", "само", "сами", "себя", "себе", "собой", "кто", "чего", "чему", "чем", "кем",
  "кому", "какой", "какая", "какое", "какие", "такой", "такая", "такое", "такие",
  "не", "ни", "бы", "б", "же", "ж", "ли", "ль", "ведь", "вон", "вот", "даже", "уже",
  "почти", "совсем", "вдруг", "прямо", "разве", "неужели", "ну", "да", "нет", "ага",
  "ой", "ах", "увы", "чтоб", "иль", "нам", "есть", "хоть", "нем", "него", "ними", "нами", "еще", "ним",
  "быть", "стать", "становиться", "являться", "называться", "было", "была", "были",
  "был", "будет", "будут", "стал", "стала", "стало", "стали", "становится",
  "значит", "означает", "является", "считается", "казалось", "кажется",
  "мочь", "может", "могут", "мог", "могла", "могли", "иметь", "имеет", "имеют",
  "очень", "слишком", "всего", "всегда", "иногда", "никогда", "везде", "повсюду",
  "теперь", "тогда", "там", "тут", "здесь", "сейчас", "потом", "сначала", "наконец",
  "надо", "нужно", "можно", "нельзя", "хорошо", "плохо", "легко", "трудно", "просто",
  "сложно", "быстро", "медленно", "тихо", "громко",
  "один", "одна", "одно", "одни", "два", "две", "двух", "три", "трех", "четыре",
  "пять", "шесть", "семь", "восемь", "девять", "десять", "первый", "второй", "третий",
  "раз", "дважды", "трижды"
)

# ===== ПРЕДПОДСЧЕТ ДЛЯ ОБЛАКА СЛОВ =====
precomputed_freq <- NULL
precomputed_file <- "word_freq.csv"

if(file.exists(precomputed_file) && !is.null(default_data$texts)) {
  precomputed_freq <- read.csv(precomputed_file, stringsAsFactors = FALSE)
  precomputed_freq <- precomputed_freq[!precomputed_freq$word %in% stopwords_full, ]
} else if(!is.null(default_data$texts)) {
  all_text <- paste(default_data$texts$text, collapse = " ")
  all_text <- gsub("\\n", " ", all_text)
  all_text <- gsub("[^а-яА-Я\\s]", " ", all_text)
  all_text <- tolower(all_text)
  all_text <- gsub("\\s+", " ", all_text)
  
  words <- strsplit(all_text, " ")[[1]]
  words <- words[nchar(words) >= 3]
  words <- words[grepl("^[а-я]+$", words)]
  words <- words[!words %in% stopwords_full]
  
  freq <- table(words) |> as.data.frame() |> arrange(desc(Freq))
  names(freq) <- c("word", "freq")
  
  write.csv(freq, precomputed_file, row.names = FALSE)
  precomputed_freq <- freq
}

# ===== ФУНКЦИЯ ОБЛАКА СЛОВ =====
make_wordcloud_fast <- function(min_freq = 20, freq_data = precomputed_freq) {
  if(is.null(freq_data) || nrow(freq_data) == 0) return(NULL)
  
  filtered <- freq_data[freq_data$freq >= min_freq, ]
  if(nrow(filtered) > 100) filtered <- filtered[1:100, ]
  if(nrow(filtered) == 0) return(NULL)
  
  wordcloud2(filtered, size = 0.6, minSize = 8)
}

# ===== ФУНКЦИЯ ДЛЯ ОТРИСОВКИ ГРАФИКА ДЛИТЕЛЬНОСТИ =====
plot_duration <- function(data, min_poems = 20) {
  if(is.null(data) || nrow(data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "Нет данных") +
             theme_void())
  }
  
  poems_col <- if("Всего_стихотворений" %in% names(data)) "Всего_стихотворений" else names(data)[ncol(data)]
  data_filtered <- data |> filter(!!sym(poems_col) >= min_poems)
  
  if(nrow(data_filtered) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                      label = paste("Нет поэтов с >=", min_poems, "стихов")) +
             theme_void())
  }
  
  name_col <- if("Автор" %in% names(data_filtered)) "Автор" else names(data_filtered)[1]
  start_col <- if("Год_первой_публикации" %in% names(data_filtered)) "Год_первой_публикации" else names(data_filtered)[2]
  end_col <- if("Год_последней_публикации" %in% names(data_filtered)) "Год_последней_публикации" else names(data_filtered)[3]
  poems_col <- if("Всего_стихотворений" %in% names(data_filtered)) "Всего_стихотворений" else names(data_filtered)[ncol(data_filtered)]
  
  data_sorted <- data_filtered |>
    arrange(desc(!!sym(poems_col))) |>
    slice_head(n = 60)
  
  data_sorted <- data_sorted |>
    mutate(!!sym(name_col) := factor(!!sym(name_col), 
                                     levels = rev(unique(!!sym(name_col)))))
  
  all_years <- c(data_sorted[[start_col]], data_sorted[[end_col]])
  min_year <- floor(min(all_years, na.rm = TRUE) / 5) * 5 - 5
  max_year <- ceiling(max(all_years, na.rm = TRUE) / 5) * 5 + 5
  
  p <- ggplot(data_sorted) +
    geom_segment(aes(x = !!sym(start_col), 
                     xend = !!sym(end_col),
                     y = !!sym(name_col),
                     yend = !!sym(name_col),
                     color = !!sym(start_col)),
                 linewidth = 7) +
    scale_color_gradient2(
      low = "#3498db", 
      mid = "#2ecc71", 
      high = "#e74c3c",
      midpoint = (min_year + max_year) / 2,
      name = "Год начала"
    ) +
    geom_point(aes(x = !!sym(start_col), y = !!sym(name_col)), 
               size = 3, color = "#2c3e50") +
    geom_point(aes(x = !!sym(end_col), y = !!sym(name_col)), 
               size = 3, color = "#2c3e50") +
    geom_text(aes(x = !!sym(start_col), y = !!sym(name_col), 
                  label = !!sym(start_col)),
              size = 2.5, hjust = 0.5, vjust = -0.5, color = "#2c3e50") +
    geom_text(aes(x = !!sym(end_col), y = !!sym(name_col), 
                  label = !!sym(end_col)),
              size = 2.5, hjust = 0.5, vjust = -0.5, color = "#2c3e50") +
    geom_text(aes(x = (!!sym(start_col) + !!sym(end_col)) / 2, 
                  y = !!sym(name_col),
                  label = paste0(!!sym(end_col) - !!sym(start_col), " лет")),
              size = 2.5, color = "white", fontface = "bold") +
    geom_text(aes(x = max_year + 2, 
                  y = !!sym(name_col),
                  label = paste0(!!sym(poems_col), " ст.")),
              size = 2.5, hjust = 0, color = "#2c3e50") +
    scale_x_continuous(breaks = seq(min_year, max_year, by = 5),
                       limits = c(min_year, max_year + 8)) +
    labs(x = "Годы", 
         y = "Поэт",
         title = "Период сотрудничества с журналом Юность",
         subtitle = paste0("Поэты с >=", min_poems, " стихотворениями. Сортировка по количеству стихов (макс. 60)")) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.height = unit(1.2, "cm"),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor.x = element_blank()
    )
  
  return(p)
}

# ===== ФУНКЦИЯ ДЛЯ ГРАФИКА ПОЭЗИИ =====
plot_poetry_trends <- function(data, year_range = NULL) {
  if(is.null(data) || nrow(data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "Нет данных") +
             theme_void())
  }
  
  plot_data <- data |>
    mutate(
      year = as.numeric(year),
      page_pct = as.numeric(gsub(",", ".", `процент.страниц.с.поэзией`)),
      poems_pct = as.numeric(gsub(",", ".", `процент.стихов.от.всех.произведений`)),
      poets_count = as.numeric(`количество.уникальных.поэтов`)
    ) |>
    filter(!is.na(year), !is.na(page_pct), !is.na(poems_pct), !is.na(poets_count))
  
  if(!is.null(year_range) && length(year_range) == 2) {
    plot_data <- plot_data |> filter(year >= year_range[1] & year <= year_range[2])
  }
  
  if(nrow(plot_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                      label = paste("Нет данных в выбранном диапазоне")) +
             theme_void())
  }
  
  plot_long <- plot_data |>
    select(year, page_pct, poems_pct, poets_count) |>
    pivot_longer(
      cols = c(page_pct, poems_pct, poets_count),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric_label = case_when(
        metric == "page_pct" ~ "Доля страниц с поэзией (%)",
        metric == "poems_pct" ~ "Доля стихов (%)",
        metric == "poets_count" ~ "Количество поэтов"
      )
    )
  
  max_left <- max(plot_data$page_pct, plot_data$poems_pct, na.rm = TRUE)
  max_right <- max(plot_data$poets_count, na.rm = TRUE)
  scale_factor <- if(max_right > 0) max_left / max_right else 1
  
  x_min <- min(plot_data$year, na.rm = TRUE)
  x_max <- max(plot_data$year, na.rm = TRUE)
  
  if(!is.null(year_range) && length(year_range) == 2) {
    x_min <- year_range[1]
    x_max <- year_range[2]
  }
  
  p <- ggplot() +
    geom_line(data = plot_long |> filter(metric %in% c("page_pct", "poems_pct")),
              aes(x = year, y = value, color = metric_label, group = metric),
              linewidth = 1.2) +
    geom_point(data = plot_long |> filter(metric %in% c("page_pct", "poems_pct")),
               aes(x = year, y = value, color = metric_label, group = metric),
               size = 3) +
    geom_line(data = plot_long |> filter(metric == "poets_count"),
              aes(x = year, y = value * scale_factor, 
                  color = metric_label, group = metric),
              linewidth = 1.2, linetype = "dashed") +
    geom_point(data = plot_long |> filter(metric == "poets_count"),
               aes(x = year, y = value * scale_factor, 
                   color = metric_label, group = metric),
               size = 3, shape = 17) +
    scale_y_continuous(
      name = "Доля (%)",
      sec.axis = sec_axis(
        transform = ~ . / scale_factor,
        name = "Количество поэтов"
      ),
      limits = c(0, max_left * 1.15)
    ) +
    scale_x_continuous(
      limits = c(x_min - 0.5, x_max + 0.5),
      breaks = seq(floor(x_min), ceiling(x_max), by = 1)
    ) +
    scale_color_manual(
      values = c("Доля страниц с поэзией (%)" = "#2c3e50",
                 "Доля стихов (%)" = "#e74c3c",
                 "Количество поэтов" = "#27ae60")
    ) +
    labs(
      x = "Год",
      y = "Доля (%)",
      title = "Динамика поэзии в журнале Юность по номерам",
      color = "Показатель"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title.y.right = element_text(color = "#27ae60", size = 10),
      axis.text.y.right = element_text(color = "#27ae60")
    )
  
  return(p)
}

# ===== ФУНКЦИЯ ДЛЯ ОБРЕЗКИ ТАБЛИЦЫ ПОЭТОВ =====
trim_poets_table <- function(data) {
  if(is.null(data) || nrow(data) == 0) return(data)
  
  col_names <- names(data)
  col_names[col_names == "" | is.na(col_names)] <- paste0("col_", which(col_names == "" | is.na(col_names)))
  names(data) <- col_names
  
  wiki_col <- grep("Ссылка.*Википедию|Википедия|wiki|адрес.*википедии", 
                   names(data), ignore.case = TRUE, value = TRUE)
  
  if(length(wiki_col) > 0) {
    wiki_idx <- which(names(data) == wiki_col[1])
    if(length(wiki_idx) > 0 && wiki_idx > 1) {
      data <- data[, 1:min(wiki_idx, ncol(data)), drop = FALSE]
    }
  }
  
  return(data)
}

# ===== ФУНКЦИЯ БЫСТРОГО ГРАФА =====
build_fast_graph <- function(data, min_publications = 11, min_weight = 2) {
  if(is.null(data) || nrow(data) == 0) return(NULL)
  if(!"poets" %in% names(data)) return(NULL)
  
  poets_list <- strsplit(data$poets, ",")
  names(poets_list) <- NULL
  
  all_poets <- unlist(poets_list) |> trimws()
  pub_count <- table(all_poets)
  
  top_poets <- names(pub_count[pub_count >= min_publications])
  if(length(top_poets) < 2) return(NULL)
  
  filtered_rows <- sapply(poets_list, function(x) {
    any(trimws(x) %in% top_poets)
  })
  
  filtered_data <- data[filtered_rows, ]
  
  edges_list <- list()
  for(i in 1:nrow(filtered_data)) {
    poets <- trimws(unlist(strsplit(filtered_data$poets[i], ",")))
    poets <- poets[poets %in% top_poets]
    if(length(poets) >= 2) {
      pairs <- combn(poets, 2, simplify = FALSE)
      edges_list <- c(edges_list, pairs)
    }
  }
  
  if(length(edges_list) == 0) return(NULL)
  
  edge_str <- sapply(edges_list, function(x) paste(sort(x), collapse = "---"))
  edge_counts <- table(edge_str)
  edge_counts <- edge_counts[edge_counts >= min_weight]
  
  if(length(edge_counts) == 0) return(NULL)
  
  edges_df <- do.call(rbind, lapply(names(edge_counts), function(e) {
    nodes <- strsplit(e, "---")[[1]]
    data.frame(from = nodes[1], to = nodes[2], weight = edge_counts[e])
  }))
  
  graph_from_data_frame(edges_df, directed = FALSE)
}

# ===== UI =====
ui <- fluidPage(
  
  titlePanel("Аналитик поэзии журнала Юность"),
  
  tabsetPanel(
    
    # Панель 1: О проекте
    tabPanel(
      title = "О проекте",
      
      fluidRow(
        column(
          width = 10,
          offset = 1,
          
          h2("Аналитик поэзии журнала Юность"),
          p("Приложение для визуализации и анализа поэтического корпуса журнала «Юность» за 1955-1974 годы."),
          hr(),
          
          h4("Возможности"),
          tags$ul(
            tags$li("Динамика поэзии по номерам (1955-1974)"),
            tags$li("Облако слов поэтических произведений"),
            tags$li("Лента времени сотрудничества поэтов с журналом"),
            tags$li("Граф соавторства с настраиваемыми параметрами")
          ),
          hr(),
          
          h3("Команда"),
          h5("Разработчики:"),
          p("Ворончихин Егор"),
          p("Старунова Ольга"),
          h5("Участвовали в подготовке корпуса:"),
          p("Докудовский Никита"),
          p("Курганов Ярослав"),
          p("Линев Никита"),
          hr()        
        )
      )
    ),
    
    # Панель 2: Загрузка данных
    tabPanel(
      title = "Загрузка данных",
      
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4("Загрузите CSV файлы"),
            p("По умолчанию уже загружены данные из папки приложения."),
            fileInput("file_poetry", "Поэзия по номерам", accept = ".csv"),
            fileInput("file_texts", "Тексты стихов", accept = ".csv"),
            fileInput("file_poets", "Информация о поэтах", accept = ".csv"),
            actionButton("load_btn", "Загрузить новые файлы", class = "btn-primary")
          )
        ),
        column(
          width = 8,
          h4("Статус загрузки"),
          verbatimTextOutput("status")
        )
      )
    ),
    
    # Панель 3: Поэзия по номерам
    tabPanel(
      title = "Поэзия по номерам",
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Настройки"),
            uiOutput("year_slider_ui"),
            actionButton("issue_btn", "Показать", class = "btn-primary")
          )
        ),
        column(
          width = 9,
          plotOutput("poetry_trend_plot", height = "500px"),
          br(),
          DTOutput("issue_table")
        )
      )
    ),
    
    # Панель 4: Тексты стихов
    tabPanel(
      title = "Тексты стихов",
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Облако слов"),
            sliderInput("word_min_freq", "Мин. частота:", min = 1, max = 100, value = 20),
            actionButton("wordcloud_btn", "Построить облако слов", class = "btn-primary")
          )
        ),
        column(
          width = 9,
          add_busy_spinner(spin = "fading-circle", position = "top-right"),
          wordcloud2Output("wordcloud", height = "450px"),
          br(),
          DTOutput("poems_table")
        )
      )
    ),
    
    # Панель 5: Поэты
    tabPanel(
      title = "Поэты",
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Длительность сотрудничества"),
            sliderInput("poet_min_poems_duration", "Мин. стихов:", 
                        min = 0, max = 100, value = 20, step = 5),
            actionButton("duration_btn", "Построить график", class = "btn-primary")
          )
        ),
        column(
          width = 9,
          plotOutput("duration_plot", height = "700px"),
          br(),
          DTOutput("poets_table")
        )
      )
    ),
    
    # Панель 6: Сеть авторов
    tabPanel(
      title = "Сеть авторов",
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Настройки сети"),
            uiOutput("pub_slider_ui"),
            sliderInput("graph_weight", "Мин. вес связи:", min = 1, max = 10, value = 2),
            actionButton("graph_btn", "Построить граф", class = "btn-success")
          )
        ),
        column(
          width = 9,
          plotOutput("graph_plot", height = "600px"),
          br(),
          verbatimTextOutput("graph_stats")
        )
      )
    )
  )
)

# ===== СЕРВЕР =====
server <- function(input, output, session) {
  
  poetry_data <- reactiveVal(default_data$poetry)
  texts_data <- reactiveVal(default_data$texts)
  poets_data <- reactiveVal(default_data$poets)
  
  graph_title <- reactiveVal("Поэты с >= 11 публикаций, вес связи >= 2")
  
  output$year_slider_ui <- renderUI({
    req(poetry_data())
    df <- poetry_data()
    if(!is.null(df) && "year" %in% names(df) && nrow(df) > 0) {
      min_year <- min(df$year, na.rm = TRUE)
      max_year <- max(df$year, na.rm = TRUE)
      if(is.finite(min_year) && is.finite(max_year)) {
        sliderInput("issue_years", "Годы:", min = min_year, max = max_year, 
                    value = c(min_year, max_year), step = 1, sep = "")
      } else {
        p("Нет данных по годам")
      }
    } else {
      p("Загрузите файл с данными")
    }
  })
  
  output$pub_slider_ui <- renderUI({
    req(poetry_data())
    df <- poetry_data()
    if(!is.null(df) && "poets" %in% names(df) && nrow(df) > 0) {
      all_poets <- unlist(strsplit(df$poets, ",")) |> trimws()
      if(length(all_poets) > 0) {
        max_pub <- max(table(all_poets))
        sliderInput("min_publications", "Мин. публикаций:", min = 1, max = max_pub, value = min(11, max_pub))
      } else {
        sliderInput("min_publications", "Мин. публикаций:", min = 1, max = 20, value = 11)
      }
    } else {
      sliderInput("min_publications", "Мин. публикаций:", min = 1, max = 20, value = 11)
    }
  })
  
  observe({
    req(texts_data())
    df <- texts_data()
    if(!is.null(df)) {
      if("author" %in% names(df)) updateSelectInput(session, "text_author", choices = c("Все", unique(df$author)))
      if("genre" %in% names(df)) updateSelectInput(session, "text_genre", choices = c("Все", unique(df$genre)))
    }
  })
  
  observe({
    req(poets_data())
    df <- poets_data()
    if(!is.null(df) && "Всего_стихотворений" %in% names(df)) {
      max_poems <- max(df$Всего_стихотворений, na.rm = TRUE)
      if(is.finite(max_poems)) {
        updateSliderInput(session, "poet_min_poems_duration", max = max_poems)
      }
    }
  })
  
  observeEvent(input$load_btn, {
    req(input$file_poetry, input$file_texts, input$file_poets)
    
    poetry <- read.csv(input$file_poetry$datapath, stringsAsFactors = FALSE)
    texts <- read.csv(input$file_texts$datapath, stringsAsFactors = FALSE)
    poets <- read.csv(input$file_poets$datapath, stringsAsFactors = FALSE)
    
    if("поэты" %in% names(poetry)) names(poetry)[names(poetry) == "поэты"] <- "poets"
    if("год" %in% names(poetry)) names(poetry)[names(poetry) == "год"] <- "year"
    
    poetry_data(poetry)
    texts_data(texts)
    poets_data(poets)
    
    output$status <- renderPrint({ 
      cat("Загружено:\n", nrow(poetry), "строк (поэзия)\n", nrow(texts), "строк (тексты)\n", nrow(poets), "строк (поэты)") 
    })
  })
  
  filtered_issue <- reactive({
    req(poetry_data(), input$issue_btn)
    isolate({
      df <- poetry_data()
      req(df, "year" %in% names(df), nrow(df) > 0)
      req(input$issue_years)
      df |> filter(year >= input$issue_years[1], year <= input$issue_years[2])
    })
  }) |> bindEvent(input$issue_btn)
  
  output$issue_table <- renderDT({ 
    req(filtered_issue())
    datatable(filtered_issue(), options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
  })
  
  output$poetry_trend_plot <- renderPlot({
    req(poetry_data(), input$issue_btn)
    isolate({
      df <- poetry_data()
      req(df, nrow(df) > 0)
      plot_poetry_trends(df, year_range = input$issue_years)
    })
  }) |> bindEvent(input$issue_btn)
  
  # Тексты для таблицы
  filtered_texts <- reactive({
    req(texts_data())
    data <- texts_data()
    if(!is.null(data) && nrow(data) > 0) {
      # Показываем все тексты
      data
    }
  })
  
  output$poems_table <- renderDT({ 
    req(filtered_texts())
    cols_to_show <- c("id", "author", "title", "year", "genre", "topic")
    cols_to_show <- cols_to_show[cols_to_show %in% names(filtered_texts())]
    datatable(filtered_texts() |> select(all_of(cols_to_show)), 
              options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  wordcloud_result <- reactive({
    req(input$wordcloud_btn)
    isolate({
      make_wordcloud_fast(min_freq = input$word_min_freq)
    })
  }) |> bindEvent(input$wordcloud_btn)
  
  output$wordcloud <- renderWordcloud2({
    req(wordcloud_result())
    wordcloud_result()
  })
  
  duration_data <- reactive({
    req(poets_data(), input$duration_btn)
    isolate({
      poets_data()
    })
  }) |> bindEvent(input$duration_btn)
  
  output$duration_plot <- renderPlot({
    plot_duration(duration_data(), min_poems = input$poet_min_poems_duration)
  })
  
  filtered_poets <- reactive({
    req(poets_data())
    data <- poets_data()
    if(!is.null(data) && nrow(data) > 0) {
      poems_col <- if("Всего_стихотворений" %in% names(data)) "Всего_стихотворений" else names(data)[ncol(data)]
      
      data <- trim_poets_table(data)
      
      if(poems_col %in% names(data)) {
        data <- data |> filter(!!sym(poems_col) >= input$poet_min_poems_duration)
      }
    }
    data
  })
  
  output$poets_table <- renderDT({ 
    req(filtered_poets())
    datatable(filtered_poets(), 
              options = list(
                scrollX = TRUE, 
                pageLength = 15,
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              ), 
              rownames = FALSE,
              class = 'compact stripe hover row-border')
  })
  
  graph_result <- reactive({
    req(poetry_data(), input$graph_btn)
    isolate({
      df <- poetry_data()
      req(df, nrow(df) > 0, "poets" %in% names(df))
      
      new_title <- paste("Поэты с >=", input$min_publications, "публикаций, вес связи >=", input$graph_weight)
      graph_title(new_title)
      
      build_fast_graph(df, min_publications = input$min_publications, min_weight = input$graph_weight)
    })
  }) |> bindEvent(input$graph_btn)
  
  output$graph_plot <- renderPlot({
    req(graph_result())
    set.seed(123)
    ggraph(graph_result(), layout = "fr") +
      geom_edge_link(aes(width = weight), alpha = 0.5, color = "gray50") +
      geom_node_point(size = 8, color = "steelblue") +
      geom_node_text(aes(label = name), size = 4, repel = TRUE) +
      theme_void() +
      labs(title = graph_title())
  })
  
  output$graph_stats <- renderPrint({
    req(graph_result())
    g <- graph_result()
    cat("Узлов:", vcount(g), "\nСвязей:", ecount(g), "\nПлотность:", round(edge_density(g), 3))
  })
  
  output$status <- renderPrint({ 
    cat("По умолчанию загружено:\n")
    if(!is.null(default_data$poetry)) cat(nrow(default_data$poetry), "строк (поэзия)\n")
    else cat("Файл poetry.csv не найден\n")
    if(!is.null(default_data$texts)) cat(nrow(default_data$texts), "строк (тексты)\n")
    else cat("Файл texts.csv не найден\n")
    if(!is.null(default_data$poets)) cat(nrow(default_data$poets), "строк (поэты)\n")
    else cat("Файл poets.csv не найден\n")
  })
}

shinyApp(ui = ui, server = server)