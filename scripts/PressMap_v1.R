#### Pressing Sonification Using Match Event Data #######
#0. Load packages and Functions -------

#Packages ----
pacman::p_load(tidyverse, StatsBombR, SBpitch, soccermatics, extrafont, ggupset, tibbletime, ggtext, ggrepel, glue, patchwork, cowplot, gtable, grid, magick, here, ggsoccer, janitor, rvest, ggimage)

#Functions ----
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.001 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}




#1. Read RDS containing La Liga event data -----
barca_clean <- read_rds(here("data", "clean", "SB_free_laliga.rds"))

#fix player names (copied from Ryo's tutorial) -----
barca_clean <- barca_clean %>% 
  ## player name
  mutate(player.name = case_when(
    player.name == "Oleguer Presas Renom" ~ "Oleguer",
    player.name == "Xavier Hernández Creus" ~ "Xavi",
    player.name == "Carles Puyol i Saforcada" ~ "Carles Puyol",
    player.name == "Anderson Luís de Souza" ~ "Deco",
    player.name == "Rafael Márquez Álvarez" ~ "Rafa Márquez",
    player.name == "Giovanni van Bronckhorst" ~ "Gio v.Bronckhorst",
    player.name == "Samuel Eto'o Fils" ~ "Samuel Eto'o",
    player.name == "Víctor Valdés Arribas" ~ "Víctor Valdés",
    player.name == "Juliano Haus Belletti" ~ "Juliano Belletti",
    player.name == "Ludovic Giuly" ~ "Ludovic Giuly",
    player.name == "Andrés Iniesta Luján" ~ "Andrés Iniesta",
    player.name == "Ronaldo de Assis Moreira" ~ "Ronaldinho",
    player.name == "Lionel Andrés Messi Cuccittini" ~ "Lionel Messi",
    player.name == "Fernando Navarro i Corbacho" ~ "Fernando Navarro",
    player.name == "Sylvio Mendes Campos Junior" ~ "Sylvinho",
    player.name == "Damià Abella Pérez" ~ "Damià",
    player.name == "Rubén Iván Martínez Andrade" ~ "Ronaldinho",
    player.name == "Ronaldo de Assis Moreira" ~ "Rubén",
    player.name == "Thiago Motta" ~ "Thiago Motta",
    player.name == "Mark van Bommel" ~ "Mark van Bommel",
    player.name == "Henrik Larsson" ~ "Henrik Larsson",
    player.name == "José Edmílson Gomes de Moraes" ~ "Edmílson",
    player.name == "Gabriel Francisco García de la Torre" ~ "Gabri",
    player.name == "Santiago Ezquerro Marín" ~ "Santi Ezquerro",
    player.name == "Maximiliano Gastón López" ~ "Maxi López",
    player.name == "Gianluca Zambrotta" ~ "Gianluca Zambrotta",
    player.name == "Eiður Smári Guðjohnsen" ~ "Eiður Guðjohnsen",
    player.name == "Lilian Thuram" ~ "Lilian Thuram",
    player.name == "Javier Pedro Saviola Fernández" ~ "Javier Saviola",
    player.name == "Gnégnéri Yaya Touré" ~ "Yaya Touré",
    player.name == "Bojan Krkíc Pérez" ~ "Bojan",
    player.name == "Eric-Sylvain Bilal Abidal" ~ "Eric Abidal",
    player.name == "Gabriel Alejandro Milito" ~ "Gabriel Milito",
    player.name == "Giovani dos Santos Ramírez" ~ "Giovani dos Santos",
    player.name == "Víctor Vázquez Solsona" ~ "Víctor Vázquez",
    player.name == "Thierry Henry" ~ "Thierry Henry",
    player.name == "José Manuel Pinto Colorado" ~ "José Manuel Pinto",
    player.name == "Daniel Alves da Silva" ~ "Dani Alves",
    player.name == "Sergio Busquets i Burgos" ~ "Sergio Busquets",
    player.name == "Seydou Kéita" ~ "Seydou Kéita",
    player.name == "José Martín Cáceres Silva" ~ "Martín Cáceres",
    player.name == "Gerard Piqué Bernabéu" ~ "Gerard Piqué",
    player.name == "Aliaksandr Hleb" ~ "Aliaksandr Hleb",
    player.name == "Pedro Eliezer Rodríguez Ledesma" ~ "Pedro",
    player.name == "Sergio Rodríguez García" ~ "Rodri",
    player.name == "Rafael Romero Serrano" ~ "Fali",
    player.name == "José Manuel Rueda Sampedro" ~ "José Manuel Rueda",
    player.name == "Zlatan Ibrahimovic" ~ "Zlatan Ibrahimovic",
    player.name == "Dmytro Chygrynskiy" ~ "Dmytro Chygrynskiy",
    player.name == "Maxwell Scherrer Cabelino Andrade" ~ "Maxwell",
    player.name == "Jeffren Isaac Suárez Bermúdez" ~ "Jeffren",
    player.name == "Víctor Sánchez Mata" ~ "Víctor Sánchez",
    player.name == "Thiago Alcântara do Nascimento" ~ "Thiago Alcântara",
    player.name == "David Villa Sánchez" ~ "David Villa",
    player.name == "Javier Alejandro Mascherano" ~ "Javier Mascherano",
    player.name == "Andreu Fontàs Prat" ~ "Andreu Fontàs",
    player.name == "Ibrahim Afellay" ~ "Ibrahim Afellay",
    player.name == "Manuel Agudo Durán" ~ "Nolito",
    player.name == "Marc Bartra Aregall" ~ "Marc Bartra",
    player.name == "Adriano Correia Claro" ~ "Adriano",
    player.name == "Martín Montoya Torralbo" ~ "Martín Montoya",
    player.name == "Jonathan dos Santos Ramírez" ~ "Jonathan dos Santos",
    player.name == "Francesc Fàbregas i Soler" ~ "Cesc Fàbregas",
    player.name == "Alexis Alejandro Sánchez Sánchez" ~ "Alexis Sánchez",
    player.name == "Juan Isaac Cuenca López" ~ "Isaac Cuenca",
    player.name == "Gerard Deulofeu Lázaro" ~ "Gerard Deulofeu",
    player.name == "Cristian Tello" ~ "Cristian Tello",
    player.name == "Sergi Roberto Carnicer" ~ "Sergi Roberto",
    player.name == "Marc Muniesa Martínez" ~ "Marc Muniesa",
    player.name == "Luis Alberto Suárez Díaz" ~ "Luis Suárez",
    TRUE ~ player.name
  ))
#End fix names ------

#2. find match with highest number of Barcelona presses/counter presses ---------
#remove all unnecessary data & filter down to defensive actions
def_actions <- barca_clean %>% 
  select(1,3,5,12,15:18,21,22,26,66,70,84,114,151,152,188:190) %>% 
  filter(type.name == "Pressure" | counterpress == T | type.name == "Interception" | duel.type.name == "Tackle" | type.name == "Foul Committed" | shot.outcome.name == "Goal" | type.name == "Own Goal For" | type.name == "Half End") %>% 
  mutate(goal_flag = if_else(!is.na(shot.outcome.name) | type.name == "Own Goal For", 1, 0),
         half_end = if_else(type.name == "Half End", 1,0),
         player_label = case_when(
           shot.outcome.name == "Goal" ~ player.name,
           type.name == "Own Goal For" ~  paste("OG"),
           TRUE ~ ""))

#find barca matches with highest pressures & counter presses
barca_press <- def_actions %>% 
  filter(team.name == "Barcelona") %>% 
  filter(type.name == "Pressure" | counterpress == T) %>%
  group_by(match_id) %>% 
  summarise(pressures = n()) %>% 
  arrange(desc(pressures)) %>% 
  head()

barcelona_matches %>% 
  filter(match_id %in% barca_press$match_id) %>% 
  select(match_id, season.season_name, home_team.home_team_name, away_team.away_team_name) 


#3. Prep data for visualization and sonification -------
#decided on Real Betis vs Barcelona in 2018/2019, match_id = 16215
match_press <- def_actions %>% 
  filter(match_id == 16215)

match_details <- barcelona_matches %>% 
  filter(match_id == 16215)

match_press_map <- match_press %>% 
  group_by(minute, team.name, goal_flag, half_end, player_label) %>% 
  summarise(press_location.x = mean(location.x))

#fill gaps
minute <- c(0:max(match_press$minute))
team.name <- unique(match_press_map$team.name)

match_press_map_join <- left_join(crossing(minute, team.name), match_press_map, by = c("minute", "team.name"))

#fix NA's
match_press_map_adjusted <- match_press_map_join %>% 
  arrange(team.name, minute) %>% 
  #use na.approx from zoo package to interpolate between NA values
  mutate(press_location.x = case_when(goal_flag == 1 ~ (lead(press_location.x) + lag(press_location.x))/2,
                                      TRUE ~ press_location.x),
         press_location.x = zoo::na.approx(press_location.x, na.rm = FALSE),
         goal_flag = if_else(is.na(goal_flag),0,goal_flag),
         #create image column to use later for goals
         image = case_when(goal_flag == 1 ~ here("resources", "football_minimal_BG_blue.png"),
                           TRUE ~ "")) %>% 
  arrange(minute)


#Prep for sonification -----
snfctn_ver <- match_press_map_adjusted %>% 
  select(1,2,3,6) %>% 
  arrange(minute, team.name) %>% 
  pivot_wider(names_from = team.name, values_from = c(press_location.x, goal_flag))

#DELIST ENTRIES WITH LISTS
for(z in 2: ncol(snfctn_ver)){
  for (i in 1:nrow(snfctn_ver)){
    snfctn_ver[[i,z]][1] <-  ifelse(is.list(snfctn_ver[i,z]),
                                    ifelse(is.na(unlist(snfctn_ver[i,z])[1]) | unlist(snfctn_ver[i,z]) == c(0,1),
                                           unlist(snfctn_ver[i,z])[2],
                                           unlist(snfctn_ver[i,z])[1]), 
                                    unlist(snfctn_ver[i,z])[1])
  }
}

snfctn_ver <- snfctn_ver %>% 
  rename(Barca_Press_Location = press_location.x_Barcelona,
         Betis_Press_Location = `press_location.x_Real Betis`,
         Barca_Goal = goal_flag_Barcelona,
         Betis_Goal = `goal_flag_Real Betis`) %>% 
  mutate(Barca_Press_Location = if_else(is.na(Barca_Press_Location),60,as.numeric(Barca_Press_Location)),
         Betis_Press_Location = if_else(is.na(Betis_Press_Location),60,as.numeric(Betis_Press_Location)),
         Barca_Goal = if_else(is.na(Barca_Goal),0,as.numeric(Barca_Goal)),
         Betis_Goal = if_else(is.na(Betis_Goal),0,as.numeric(Betis_Goal)))

write_csv(snfctn_ver, here("outputs", "Betis_vs_Barca_Press_Sonification.csv"))

# 4. Plot ------
bgcol <- "#EF4849" #ff6666
barcacol <- "#1105f5" #1105f5
betiscol <- "#05ff8f"
panelcol <- "#202124"
plotfont <- "Open Sans Condensed Medium" 

press_map_plot <- match_press_map_adjusted %>% 
  filter(team.name == "Barcelona") %>% 
  ggplot(aes(x = minute, y = press_location.x)) +
  #SET DEF, MID, ATT 3rd's of the pitch ----  
annotate(geom = "rect", xmin = 0, xmax = max(minute) + 5, ymin = 80, ymax = 120,
         fill = bgcol,
         colour = bgcol,
         alpha = 1) +
  annotate(geom = "rect", xmin = 0, xmax = max(minute) + 5, ymin = 40, ymax = 80,
           fill = bgcol, colour = bgcol, alpha = 0.8) +
  annotate(geom = "rect", xmin = 0, xmax = max(minute) + 5, ymin = 0, ymax = 40,
           fill = bgcol, colour = bgcol, alpha = 0.6) +
  annotate(geom = "text", x = max(minute) + 2, y = 30, label = "Def 3rd", family = plotfont, size = 10, fontface = "italic", hjust = 0, angle = 270, alpha = 0.85, color = "white") +
  annotate(geom = "text", x = max(minute) + 2, y = 70, label = "Mid 3rd", family = plotfont, size = 10, fontface = "italic", hjust = 0, angle = 270, alpha = 0.85, color = "white") +
  annotate(geom = "text", x = max(minute) + 2, y = 110, label = "Att 3rd", family = plotfont, size = 10, fontface = "italic", hjust = 0, angle = 270, alpha = 0.85, color = "white") +
  # END ANNOTATION ----
geom_line(aes(colour = team.name),size = 1, linetype = "solid") +
  scale_colour_manual(values = barcacol) +
  #ADD HT and FT LINES ----
annotate(geom = "line", x = c(47,47), y = c(0, 120), linetype = "dashed", colour = "white") +
  annotate(geom = "line", x = c(max(minute),max(minute)), y = c(0, 120), linetype = "dashed", colour = "white") +
  # HIGHLIGHTS GOALS ----
# geom_segment(data = filter(match_press_map_adjusted, team.name == "Barcelona" & goal_flag != 0), aes(x = minute, xend = minute),y = 0, yend = 120, colour = barcacol, linetype = 1, alpha = 0.5) +
#USE GEOM_IMAGE TO USE FOOTBALL PNG FOR GOALS
geom_image(data = filter(match_press_map_adjusted, team.name == "Barcelona" & goal_flag != 0),
           aes(image = image, x = minute, y = press_location.x), 
           size = 0.025) +
  geom_text(data = filter(match_press_map_adjusted, team.name == "Barcelona" & goal_flag != 0),
            aes(x = minute,
                y = press_location.x,
                label = paste(minute,"' - ",player_label, sep = "")),
            family = plotfont,
            size = 6,
            colour = "white",
            angle = 90,
            nudge_y = 5,
            hjust = 0,
            alpha = 0.85) +
  # AXES, THEMES, LABS ----
scale_x_continuous(breaks = c(seq(0, 45, by = 5), 47, seq(50,90, by = 5), 93),
                   labels = c(seq(0, 45, by = 5), "HT", 
                              seq(50, 90, by = 5), "FT"),
                   limits = c(0, max(minute) + 5),
                   expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0,0))+
  theme(plot.title = element_text(family = plotfont, size = 25, colour = "#ff5c67"),
        plot.subtitle = element_text(size = 15, family = plotfont, colour = "white", face = "italic"),
        plot.caption = element_text(family = plotfont, size = 15, colour = "white"),
        plot.background = element_rect(fill = panelcol),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = plotfont, colour = "white", size = 18, face = "italic"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = plotfont, colour = "white", size = 15, face = "italic"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(size = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  ylab("Area of Pitch")+
  labs(title = "Pressing by Area of Pitch",
       subtitle = paste(match_details$home_team.home_team_name," ",match_details$home_score, "-", match_details$away_score, " ", match_details$away_team.away_team_name, sep = " ", paste("\n",match_details$competition.competition_name," ",match_details$season.season_name,sep = "")),
       caption = "")

press_map_plot

# END PLOT -----

#EXPORT PLOTS -----
ggsave(plot = press_map_plot,
       filename = here("outputs/Betis_vs_Barca_Press_Plot.png"),
       height = 10, width = 15)

#add W22 logo
plot_watch22_logo <- add_logo(
  plot_path = here("outputs/Betis_vs_Barca_Press_Plot.png"),
  logo_path = here("resources/WATCH22_CIRCLE.png"),
  logo_position = "top right",
  logo_scale = 19)

plot_watch22_logo

magick::image_write(
  image = plot_watch22_logo, 
  path = here("outputs/Betis_vs_Barca_Press_Plot_W22.png"))

#add StatsBomb logo

statsbomb_logo <- add_logo(
  plot_path = here("outputs/Betis_vs_Barca_Press_Plot_W22.png"),
  logo_path = here("resources/SB - Icon Lockup - Colour positive.png"),
  logo_position = "bottom right",
  logo_scale = 8)

magick::image_write(
  image = statsbomb_logo, 
  path = here::here("outputs/Betis_vs_Barca_Press_Plot_FINAL.png"))

