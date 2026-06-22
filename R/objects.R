### OBJECTS INSTANCIATION ###
# Objects are saved in data/ via usethis::use_data()
# See data.R for documentation
# Run data-raw construction script to regenerate if needed

# lubaszka_solec <- Bakery(...)  # -> data/lubaszka_solec.rda
# kanapka_szarpana <- Sandwich(...)  # -> data/kanapka_szarpana.rda
# etc.

# -----
# Obect: Bakery
# -----

lubaszka_solec <- Bakery(
  name = "Lubaszka Solec",
  attendance_df = attendance_df
)



# -----
# Objects: products (gathered in a list for shiny)
# -----

# Sandwiches
kanapka_szarpana <- Sandwich(
  name = "Kanapka szarpana",
  price_ref = 13.80, stock = 25, stock_initial = 25
)

kanapka_reuben <- Sandwich(
  name = "Kanapka Reuben",
  price_ref = 9.90, stock = 25, stock_initial = 25
)

kanapka_weganska <- Sandwich(
  name = "Kanapka weganska z pasta curry",
  price_ref = 11.40, stock = 30, stock_initial = 30
)

# Breads
chleb_wiejski <- Bread(
  name = "Chleb wiejski",
  price_ref = 10.80, stock = 15, stock_initial = 15
)

chleb_grycany <- Bread(
  name = "Chleb gryczany",
  price_ref = 12.80, stock = 12, stock_initial = 12
)

chleb_fitness <- Bread(
  name = "Chleb fitness",
  price_ref = 15.60, stock = 10, stock_initial = 10
)

# Pizzas
pizza_margherita <- Pizza(
  name = "Pizza Margherita",
  price_ref = 9.00, stock = 20, stock_initial = 20
)

pizza_salami <- Pizza(
  name = "Pizza Salami",
  price_ref = 9.40, stock = 20, stock_initial = 20
)

pizza_hawaii <- Pizza(
  name = "Pizza Hawaii",
  price_ref = 9.40, stock = 20, stock_initial = 20
)



# Named list for Shiny Input selection

product_catalog <- list(
  # Sandwiches
  "Kanapka szarpana"               = kanapka_szarpana,
  "Kanapka Reuben"                 = kanapka_reuben,
  "Kanapka weganska z pasta curry" = kanapka_weganska,

  # Breads
  "Chleb wiejski"                  = chleb_wiejski,
  "Chleb gryczany"                 = chleb_grycany,
  "Chleb fitness"                  = chleb_fitness,
  # Pizzas
  "Pizza Margherita"               = pizza_margherita,
  "Pizza Salami"                   = pizza_salami,
  "Pizza Hawaii"                   = pizza_hawaii
)
