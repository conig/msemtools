#' get_haiku
#'
#' Generates a haiku
#' @importFrom papertools hash_replace

get_haiku = function(){

  #envir = environment()
  possesive_noun_1 = c("gaze","sneer","sword","blade","smirk")
  adjective_1 = c("wide","slick","red","pale","white","gaunt","iced","dark","small","big","great","slight","bare")
  adjective_2 = c("deathly","corrupt","charming","smirking","knowing","dying","panicked","fearful","tiny")
  adjective_3 = c("discouraged","tremulous","reflective","boggle-eyed")
  ad_verb_3 = c("mournfully","peacefully","painfully","silently")
  ad_verb_2 = c("slightly","softly","bleakly")
  adjective_object_3 = c("heavily","quietly")
  plant_1 = c("tree","plant", "leaf")
  plant_plural_4 = c("jacarandas")
  animal_1 = c("frog","fish","cat","bear","bird","dog","wolf","ant","gnat","toad")
  vowel_actor_1 = c("egg","ant")
  animal_or_plant_1 = c(animal_1, plant_1)
  animal_2 = c("rabbit","flower","doughnut")
  plural_animals_1 = c("ducks", "dogs","bros","snakes","cats","ants")
  plural_animals_2 = c("foxes", "ponies","wizards","zerglings")
  colours_1 = c("black","grey","white","dark","brown")
  colours_2 = c("silver","iron","copper")
  verb_1 = c("sleep","sit","listen","cry","frown")
  verb_2 = c("wailing","bouncing","sliding","smoking","hanging","warming","stinging")
  body_1 = c("eyes","face","ears","neck","legs","leg","chin","breast","chest","feet")
  object_verb_2 = c("falling","rolling")
  plural_objects_1 = c("tears","walls","veils","vests","hats","hands","pipes")
  size_1 = c("Big","Small")
  objects_1 = c("legs","fists","feet","hands","birds","thoughts","dreams","eyes")
  objects_2 = c("pebbles", "peanuts","poppers","doughnuts","talons","sapphires")
  names_1 = c("Jake","John","Joe","Bob","Phil","Jim","James")
  nature_plural_1 = c("leaves","snow","rocks","stones","rain")
  mental_2 = c("concern","regret","insight","wisdom","respite")
  possessed_3 = c("big ##colours_1## ##plural_objects_1##","chickenpox","elephants","coffee stains")
  exclamation_2 = c("O hark!", "Heavens!","Dear lord!","Good grief!","Oh no!","My word!")
  state_1 = c("lost","gone","done","clear")

  one = c("A ninja's swift ##possesive_noun_1##...",
          "A ##adjective_2## ##colours_1## ##animal_or_plant_1##...",
          "A ##verb_2## ##colours_1## ##animal_1##...",
          "A ##adjective_3## ##animal_1##...",
          "A ##adjective_2## thought...",
          "A ##adjective_2## ##colours_1## ##animal_1##...")
  two = c("##colours_2## ##plural_objects_1## ##object_verb_2## softly...",
          "##size_1## ##objects_1## landing ##adjective_object_3##...",
          "##colours_1## ##objects_2## ##verb_2## ##ad_verb_2##...",
          "##adjective_1## ##nature_plural_1##; shrouded in ##mental_2##...",
          "##colours_1## ##nature_plural_1##. ##exclamation_2## My ##adjective_1## ##body_1##.",
          "##colours_2## ##plural_animals_2## ##verb_2## ##plural_animals_1##...",
          "The ##adjective_1## ##animal_1## has funny ##objects_1##...",
          "##adjective_2## ##plant_plural_4## ##verb_1##...")
  three = c("##nature_plural_1## falls ##ad_verb_3##...",
            "##plural_animals_2## ##verb_1## lazily...",
            "##names_1## has ##possessed_3##...",
            "Some ##animal_1## with a leer...",
            "A ##adjective_3## ##animal_1##...",
            "A scary notion...",
            "An ##vowel_actor_1## with ##adjective_1## ##objects_1##...",
            "A little ##colours_1## ##animal_1##?",
            "All is ##state_1##. Katsu!",
            "All is ##state_1##. ##exclamation_2##",
            "A ##adjective_2## ##animal_2##.")

  first = sample(one,1) %>%
    papertools::hash_replace(sample = 1) %>%
    Hmisc::capitalize() %>%
    crayon::white()

  second = sample(two,1) %>%
    papertools::hash_replace(sample = 1) %>%
    Hmisc::capitalize() %>%
    crayon::white()

  third = sample(three,1) %>%
    papertools::hash_replace(sample = 1) %>%
    Hmisc::capitalize() %>%
    crayon::white()

  haiku = paste(first,second,third,
                crayon::red("                            [-_-]~"), sep = "\n")
  #message(haiku)

  class(haiku) = "haiku"

  return(haiku)
}
