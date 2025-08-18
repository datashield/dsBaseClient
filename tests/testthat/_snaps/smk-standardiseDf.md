# ask_question displays the correct prompt

    Code
      ask_question_class("my_var")
    Message
      i Would you like to:
      1. Convert `my_var` to a factor in all studies
      2. Convert `my_var` to an integer in all studies
      3. Convert `my_var` to numeric in all studies
      4. Convert `my_var` to a character in all studies
      5. Convert `my_var` to a logical vector in all studies
      6. Cancel `ds.dataFrameFill` operation

# print_all_classes prints the correct message

    Code
      print_all_classes(c("server_1", "server_2", "server_3"), c("numeric", "factor",
        "integer"))
    Message
      * server_1: numeric
      * server_2: factor
      * server_3: integer

# .make_levels_message makes correct message

    Code
      .make_levels_message(level_conflicts)
    Message
      ! Warning: factor variables fac_col2, fac_col3, fac_col6, and fac_col9 do not have the same levels in all studies
      i Would you like to:
      1. Create the missing levels where they are not present
      2. Do nothing
      3. Cancel `ds.dataFrameFill` operation

# .print_var_recode_message prints the correct message

    Code
      .print_var_recode_message(added_cols, "test_df")
    Message
      v The following variables have been added to test_df:
      i server_1 --> col11
      i server_2 --> col11
      i server_3 --> col12
      

# .print_class_recode_message prints the correct message

    Code
      .print_class_recode_message(class_decisions, different_classes, "test_df")
    Message
      v The following classes have been set for all datasources in test_df: 
      i fac_col4 --> factor
      i fac_col5 --> logical

# .print_levels_recode_message prints the correct message

    Code
      .print_levels_recode_message(unique_levs, "test_df")
    Message
      v The following levels have been set for all datasources in test_df: 
      i fac_col2 --> Blue, Green, Red
      i fac_col3 --> No, Yes
      i fac_col6 --> Bird, Cat, Dog
      i fac_col9 --> False, True

# .print_out_messages prints the correct messages

    Code
      .print_out_messages(added_cols, class_decisions, different_classes, unique_levs,
        level_conflicts, "1", "test_df")
    Message
      v The following variables have been added to test_df:
      i server_1 --> col11
      i server_2 --> col11
      i server_3 --> col12
      
      v The following classes have been set for all datasources in test_df: 
      i fac_col4 --> factor
      i fac_col5 --> logical
      
      v The following levels have been set for all datasources in test_df: 
      i fac_col2 --> Blue, Green, Red
      i fac_col3 --> No, Yes
      i fac_col6 --> Bird, Cat, Dog
      i fac_col9 --> False, True

