df_room = data.frame(floor = rep(3:5, each = 3),
                     room  = paste0(rep(3:5, each = 3),
                                    "G-",
                                    rep(LETTERS[1:3], 3)),
                     name  = c("미나", "나연", "다현", 
                               "정연", "지효", "채연",
                               "모모", "쯔위", "사나"),
                     stringsAsFactors = FALSE)
df_list = data.frame(team = c(rep("TWICE", 9),
                              rep("Red Velvet", 5)),
                     No = {set.seed(123)
                       sample(1000:2000, 14)},
                     member = c(c("미나", "나연", "다현", 
                                  "정연", "지효", "채연",
                                  "모모", "쯔위", "사나"),
                                c("아이린", "웬디", "슬기", 
                                  "조이", "예리")),
                     stringsAsFactors = FALSE)
