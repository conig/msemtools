

get_haiku = function(){

  replace_text = function(string){
    #message(string)

    iteration = 0
    while(grepl("#", string) & iteration < 100){
      string = sub("##adjective_2##",sample(adjective_2,1),string) %>%
        sub("##adjective_1##",sample(adjective_1,1),.) %>%
        sub("##possesive_noun_1##",sample(possesive_noun_1,1),.) %>%
        sub("##adjective_3##",sample(adjective_3,1),.) %>%
        sub("##animal_1##",sample(animal_1,1),.) %>%
        sub("##animal_2##",sample(animal_2,1),.) %>%
        sub("##animal_or_plant_1##",sample(animal_or_plant_1,1),.) %>%
        sub("##colours_1##",sample(colours_1,1),.) %>%
        sub("##colours_2##",sample(colours_2,1),.) %>%
        sub("##object_verb_2##",sample(object_verb_2,1),.) %>%
        sub("##plural_objects_1##",sample(plural_objects_1,1),.) %>%
        sub("##size_1##",sample(size_1,1),.) %>%
        sub("##plural_animals_2##",sample(plural_animals_2,1),.) %>%
        sub("##names_1##",sample(names_1,1),.) %>%
        sub("##adjective_object_3##",sample(adjective_object_3,1),.) %>%
        sub("##nature_plural_1##",sample(nature_plural_1,1),.) %>%
        sub("##mental_2##",sample(mental_2,1),.) %>%
        sub("##body_1##",sample(body_1,1),.) %>%
        sub("##objects_2##",sample(objects_2,1),.) %>%
        sub("##possessed_3##",sample(possessed_3,1),.) %>%
        sub("##verb_1##",sample(verb_1,1),.) %>%
        sub("##ad_verb_3##",sample(ad_verb_3,1),.) %>%
        sub("##ad_verb_2##",sample(ad_verb_2,1),.) %>%
        sub("##exclamation_2##",sample(exclamation_2,1),.) %>%
        sub("##verb_2##",sample(verb_2,1),.) %>%
        sub("##objects_1##",sample(objects_1,1),.)
      iteration = iteration + 1
    }
    if(iteration == 100) stop("haiku broke")

    return(string)
  }

  possesive_noun_1 = c("gaze","sneer","sword","blade","smirk")
  adjective_1 = c("wide","red","pale","white","gaunt","iced","dark")
  adjective_2 = c("deathly","corrupt","charming","smirking","knowing","dying","panicked","fearful")
  adjective_3 = c("discouraged","tremulous","reflective","boggle-eyed")
  ad_verb_3 = c("mournfully","peacefully","painfully","silently")
  ad_verb_2 = c("slightly","softly","bleakly")
  adjective_object_3 = c("heavily","quietly")
  plant_1 = c("tree","plant", "leaf")
  animal_1 = c("frog","fish","cat","bear","bird","dog","wolf","ant","gnat","toad")
  animal_or_plant_1 = c(animal_1, plant_1)
  animal_2 = c("rabbit","flower")
  plural_animals_2 = c("foxes", "ponies","wizards")
  colours_1 = c("black","grey","white","dark","brown")
  colours_2 = c("silver","iron","copper")
  verb_1 = c("sleep","sit","listen","cry","frown")
  verb_2 = c("wailing","bouncing","sliding","smoking","hanging","warming","stinging")
  body_1 = c("eyes","face","ears","neck","legs","leg","chin","breast","chest","feet")
  object_verb_2 = c("falling","rolling")
  plural_objects_1 = c("tears","walls","veils","vests","hats","hands","pipes")
  size_1 = c("Big","Small")
  objects_1 = c("legs","fists","tables")
  objects_2 = c("pebbles", "peanuts","poppers","doughnuts","talons","sapphires")
  names_1 = c("Kate","John","Joe","Bob","Phil")
  nature_plural_1 = c("leaves","snow","rocks","stones","rain")
  mental_2 = c("concern","regret","insight","wisdom","respite")
  possessed_3 = c("big ##colours_1## ##plural_objects_1##","chickenpox","elephants","coffee stains")
  exclamation_2 = c("O hark!", "Heavens!","Dear lord!","Good grief!","Oh no!","My word!")

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
          "##colours_1## ##plural_animals_2## ##verb_2## ##plural_animals_2##...")
  three = c("##nature_plural_1## falls ##ad_verb_3##...",
            "##plural_animals_2## ##verb_1## lazily...",
            "##names_1## has ##possessed_3##...",
            "Some ##animal_1## with a leer...",
            "A ##adjective_3## ##animal_1##...",
            "A scary notion...",
            "An egg with small ##objects_1##...",
            "A little black ##animal_1##?",
            "All is clear. Katsu!")

  first = sample(one,1) %>%
    replace_text %>%
    Hmisc::capitalize()
  second = sample(two,1) %>%
    replace_text %>%
    Hmisc::capitalize()
  third = sample(three,1) %>%
    replace_text %>%
    Hmisc::capitalize()

  haiku = paste(first,second,third,
                "                             [-_-]~", sep = "\n")
  #message(haiku)


  return(haiku)
}
