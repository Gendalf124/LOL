# ====================================================================
# ХАКАТОН: ГОРНОЗАВОДСКИЕ КОРНИ БЕЛОРЕЦКА (ULTIMATE EDITION)
# ====================================================================

library(shiny)
library(visNetwork)
library(DT)
library(ggplot2)
library(dplyr)
library(bslib)
library(plotly)

# Подгружаем внешний файл с описаниями
source("node_descriptions.R", local = TRUE)

# 1. ДАННЫЕ
# --------------------------------------------------------------------
nodes_data <- data.frame(
  id = 1:98,
  label = c(
    "Белорецкий завод", "Тирлянский завод", "Инзерский завод", "Зигазинский завод", 
    "Авзяно-Петровский", "Узянский завод", "Кагинский завод", "Лапыштинский завод",
    "Иван Твердышев", "Яков Твердышев", "Иван Мясников", "Акинфий Демидов",
    "Никита Демидов", "Сергей фон Дервиз", "Дарья Пашкова", "Семён Пашков",
    "Александр Пашков", "Карл Рихтер", "Андрей Вогау", "Петр Коптев",
    "Алексей Управленский", "Пугачевское восстание", "Гражданская война", "ВОВ (Оборонка)",
    "ПАО 'Белорецкий МК'", "ЗАО 'БЗРП'", "Туканский рудник", "Комаровский рудник",
    "Узкоколейная ЖД", "Индустриализация", "Белорецк (город)", "Магнитогорский МК",
    "Евдоким Демидов", "Мария Пашкова", "Константин Белосельский", "Эвакуация 1941",
    "Производство пружин", "Автопром (КАМАЗ)", "РЖД Сектор", "ЧПУ Станки",
    "Сплавы Белой", "Инженерный корпус", "Метизный цех", "Проволочный стан", "Запруда реки", "БЗРП Узел",
    "Аскинский завод", "Династия Ярцевых", "seafile.dtwin.ru",
    "Архангельский завод", "Богоявленский завод", "Нижне-Авзянский завод", "Верхне-Авзянский завод",
    "Ишлинский завод", "Ломовка", "Гора Магнитная", "Река Белая", "Река Инзер", "Николай Демидов",
    "Василий Пашков", "Иван Ершов", "Эрнест Коллинз", "Василий Блюхер", "Михаил Калинин",
    "Водонапорная башня", "Сталепроволочный цех", "Канатный цех", "Группа Мечел", "Чугуновозная дорога",
    "Древесный уголь", "Мартеновский цех", "Доменный цех", "Прокатный стан 150", "Серпуховский завод",
    "Танковая броня", "Аэростатный трос", "Инженер Гауду", "Грум-Гржимайло", "Горное училище",
    "Никольская церковь", "Гора Малиновая", "Арский камень", "Река Нура", "Заповедник",
    "Аркадий Гайдар", "Медвежьи лапы", "Князь Белосельский", "Электростанция", "Метизный союз",
    "Лаборатория БМК", "Алек. Пашков", "Генерал Пашков", "Фирма Харви", "Школа ФЗО",
    "Госпиталь", "Космические сплавы", "Метеостанция", "Музей БМК"
  ),
  group = c(
    rep("Завод", 8), rep("Владелец", 12), "Управляющий", 
    rep("Событие", 4), "Современность", "БЗРП", 
    rep("Инфраструктура", 3), rep("Прочее", 16),
    "Завод", "Владелец", "Инфраструктура",
    rep("Завод", 5), "География", "Ресурс", "Ресурс", "Ресурс", "Владелец",
    "Владелец", "Владелец", "Личность", "Личность", "Личность",
    "Инфраструктура", "Завод", "Завод", "Современность", "Инфраструктура",
    "Ресурс", "Завод", "Завод", "Инфраструктура", "Завод",
    "Продукт", "Продукт", "Личность", "Личность", "Инфраструктура",
    "Инфраструктура", "География", "География", "Ресурс", "География",
    "Личность", "Ресурс", "Владелец", "Инфраструктура", "Прочее",
    "Прочее", "Владелец", "Владелец", "Прочее", "Прочее",
    "Инфраструктура", "Продукт", "Прочее", "Инфраструктура"
  ),
  year_start = c(
    1762, 1801, 1890, 1890, 1755, 1777, 1769, 1895, 
    1740, 1740, 1750, 1730, 1740, 1880, 1780, 1780, 1790, 1870, 1880, 1770, 
    1920, 1773, 1918, 1941, 1993, 1973, 1890, 1760, 1912, 1928, 1762, 1930, 
    1760, 1800, 1860, 1941, 1970, 1975, 1980, 2010, 1760, 1930, 1942, 1912, 1762, 1973,
    1899, 1770, 2024,
    1753, 1755, 1755, 1755, 1805, 1762, 1740, 1700, 1700, 1760,
    1780, 1770, 1890, 1918, 1923, 1916, 1941, 1941, 2003, 1880,
    1762, 1880, 1762, 2000, 1941, 1942, 1941, 1890, 1900, 1876,
    1800, 1762, 1773, 1762, 1978, 1924, 1850, 1860, 1900, 1930,
    1950, 1810, 1800, 1890, 1930, 1800, 1960, 1880, 1900
  ),
  stringsAsFactors = FALSE
)

# 84 исторические связи
edges_data <- data.frame(
  from = c(
    9,11,15,14,18,22,24,26,26,26,25,29,27,36,44,1,48,48,49,49,26, # Старые
    9,9,50,51,5,5,52,53,1,56,57,58,60,61,63,64,65,66,67,68,69,70,
    71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,
    91,92,93,94,95,96,97,98,9,11,15,19,25,26,30,1,31,2,5,6,7
  ), 
  to = c(
    1,1,2,4,3,1,25,38,39,40,26,1,1,24,26,31,47,5,1,26,46, # Старые
    50,51,1,1,52,53,5,5,56,32,1,3,15,5,23,31,31,24,24,25,1,1,
    1,1,25,36,24,24,2,1,1,31,1,22,1,84,31,27,1,1,89,25,
    17,92,1,31,31,25,31,31,11,15,35,14,68,37,1,29,65,29,7,33,7
  ),
  label = c(
    rep("", 84) # Для краткости метки унифицированы
  ),
  stringsAsFactors = FALSE
)

# 2. ИНТЕРФЕЙС (UI)
# --------------------------------------------------------------------
ui <- page_navbar(
  theme = bs_theme(version = 5, bg = "#0F172A", fg = "#F9FAFB", primary = "#EF4444"),
  title = "ИСТОРИЯ БЕЛОРЕЦКА",
  fillable = TRUE,
  
  # Боковая панель
  sidebar = sidebar(
    title = "Навигация",
    bg = "#1E293B",
    sliderInput("year_slider", "Таймлайн (год):", 1730, 2026, 2026, step = 5,
                animate = animationOptions(interval = 800)),
    selectInput("role_filter", "Категория:", choices = c("Все", unique(nodes_data$group))),
    
    hr(),
    # Карточка для динамического описания
    card(
      card_header("Детали объекта", class = "bg-primary text-white"),
      uiOutput("node_info_display"),
      style = "background-color: #0F172A; border: 1px solid #334155;"
    ),
    
    # Мини-статистика
    div(style = "font-size: 0.8em; margin-top: 10px; color: #94A3B8;",
        textOutput("stats_nodes"),
        textOutput("stats_edges"))
  ),

  # ВКЛАДКИ
  nav_panel("ГРАФ СВЯЗЕЙ", 
    card(
      full_screen = TRUE,
      visNetworkOutput("network_plot", height = "calc(100vh - 150px)")
    )
  ),
  
  nav_panel("АНАЛИТИКА",
    layout_column_wrap(
      width = 1/2,
      card(full_screen = TRUE, plotlyOutput("centrality_chart")),
      card(full_screen = TRUE, plotlyOutput("timeline_dynamics")),
      card(full_screen = TRUE, plotlyOutput("group_dist_chart"))
    )
  ),
  
  nav_panel("РЕЕСТР", 
    card(DTOutput("nodes_table"))
  )
)

# 3. СЕРВЕРНАЯ ЛОГИКА
# --------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Реактивный фильтр данных
  filtered_data <- reactive({
    n <- nodes_data %>% filter(year_start <= input$year_slider)
    e <- edges_data %>% filter(from %in% n$id & to %in% n$id)
    
    # Оставляем только те узлы, у которых есть связи
    connected_ids <- unique(c(e$from, e$to))
    n <- n %>% filter(id %in% connected_ids)
    
    if (input$role_filter != "Все") {
      n <- n %>% filter(group == input$role_filter)
      e <- e %>% filter(from %in% n$id & to %in% n$id)
    }
    list(nodes = n, edges = e)
  })

  # Вывод статистики
  output$stats_nodes <- renderText({ paste("Активных объектов:", nrow(filtered_data()$nodes)) })
  output$stats_edges <- renderText({ paste("Активных связей:", nrow(filtered_data()$edges)) })

  # Отрисовка графа (ОБНОВЛЕННАЯ ФИЗИКА)
  output$network_plot <- renderVisNetwork({
    d <- filtered_data()
    visNetwork(d$nodes, d$edges) %>%
      # Мелкие узлы (size = 10)
      visNodes(shape = "dot", size = 10, 
               font = list(color = "white", size = 14, strokeWidth = 2, strokeColor = "black")) %>%
      visEdges(width = 2, color = "#64748B", arrows = "to") %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>%
      # Длинные связи и расталкивание
      visPhysics(
        solver = "forceAtlas2Based", 
        forceAtlas2Based = list(
          gravitationalConstant = -100, 
          springLength = 250,           
          springConstant = 0.04
        ),
        stabilization = FALSE
      ) %>%
      visEvents(selectNode = "function(nodes) { Shiny.setInputValue('current_node_id', nodes.nodes[0]); }",
                deselectNode = "function(nodes) { Shiny.setInputValue('current_node_id', null); }")
  })

  # ДИНАМИЧЕСКИЙ ТЕКСТ ИЗ ВНЕШНЕГО ФАЙЛА
  output$node_info_display <- renderUI({
    res_id <- input$current_node_id
    
    if (is.null(res_id)) {
      return(p("Кликните на узел в графе, чтобы узнать историю.", style = "font-style: italic; color: #64748B;"))
    }
    
    name <- nodes_data$label[nodes_data$id == res_id]
    # Используем функцию из подгруженного node_descriptions.R
    desc <- get_description(res_id) 
    
    tagList(
      h5(name, style = "color: #EF4444;"),
      p(desc, style = "font-size: 0.95em;")
    )
  })

  # ГРАФИК: Центральность
  output$centrality_chart <- renderPlotly({
    d <- filtered_data()
    if(nrow(d$edges) == 0) return(NULL)
    links <- as.data.frame(table(c(d$edges$from, d$edges$to)))
    top_nodes <- links %>%
      rename(id = Var1, freq = Freq) %>%
      mutate(id = as.numeric(as.character(id))) %>%
      left_join(nodes_data, by = "id") %>%
      arrange(desc(freq)) %>% head(10)
    
    p <- ggplot(top_nodes, aes(x = reorder(label, freq), y = freq, fill = freq)) +
      geom_col() + coord_flip() + theme_minimal() +
      scale_fill_gradient(low = "#F87171", high = "#EF4444") +
      labs(title = "ТОП КЛЮЧЕВЫХ УЗЛОВ", x = "", y = "Кол-во связей") +
      theme(text = element_text(color="white"), axis.text = element_text(color="white"))
    ggplotly(p) %>% layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)')
  })

  # ГРАФИК: Динамика
  output$timeline_dynamics <- renderPlotly({
    full_n <- nodes_data %>% arrange(year_start) %>% mutate(cum = row_number())
    p <- ggplot(full_n, aes(x = year_start, y = cum)) +
      geom_line(color = "#3B82F6", linewidth = 1) +
      geom_vline(xintercept = input$year_slider, linetype="dashed", color="red") +
      theme_minimal() + labs(title = "НАКОПИТЕЛЬНЫЙ РОСТ ОБЪЕКТОВ") +
      theme(text = element_text(color="white"), axis.text = element_text(color="white"))
    ggplotly(p) %>% layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)')
  })

  # ГРАФИК: Группы
  output$group_dist_chart <- renderPlotly({
    p <- ggplot(filtered_data()$nodes, aes(x = group, fill = group)) +
      geom_bar() + theme_minimal() + labs(title = "СТРУКТУРА СРЕЗА") +
      theme(text = element_text(color="white"), axis.text = element_text(color="white"), legend.position = "none")
    ggplotly(p) %>% layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)')
  })

  # Таблица данных
  output$nodes_table <- renderDT({
    datatable(filtered_data()$nodes[,c("label", "group", "year_start")], 
              options = list(pageLength = 10, dom = 'ftp', scrollX = TRUE), 
              rownames = FALSE) %>%
      formatStyle(columns = 1:3, color = 'white', backgroundColor = '#1E293B')
  })
}

shinyApp(ui, server)