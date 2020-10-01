require(tidyverse)

# Function to shape the otree sql database such that each participant gets a file:
# As preparation, the app name at the start of each variable should be removed, and
# the database should be only from one app (pre-split)
# The output is one large dataframe with a row for each round
#
# save_path: If given, where to save the data frames to
# filename_str: an extra string to be written into the filename
# drop_prefix: Wether to drop the "session." and "participant." in the variable names
# vars_to_drop: should provide the last part of the name of the variable to be dropped


otree_reshape <- function(dat, save_path='', filename_str='', drop_prefix=TRUE, vars_to_drop=NULL) {


  # First, figure out how many rounds there are:
  n_rounds <- dat %>%
    names() %>%
    str_extract_all('^\\d+') %>%
    unlist() %>%
    unique() %>%
    length()

  var_names <- dat %>%
    names() %>%
    str_extract_all('^1\\..*') %>%
    unlist() %>%
    str_remove('1\\.')

  df_list <- list()
  # Create a dataframe for every subject
  for (i_subj in seq_len(nrow(dat))) {
    cat(str_c('Reshaping participant ', i_subj, '\n'))
    subj_row <- slice(dat, i_subj)
    base_row <- base_df <- subj_row[, str_detect(names(subj_row), '^\\D|^1\\.')]
    names(base_row) <- names(base_df) <- names(base_df) %>%
      str_remove_all('\\d+\\.')
    for (i in 2:n_rounds) {
      base_df <- bind_rows(base_df, base_row)
    }

    # Now fill this base tibble with the actual data. This is where the heavy lifting happens!
    for (i_var in 1:length(var_names)) {
        base_df[, var_names[i_var]] <- t(subj_row[, str_c(1:n_rounds,
          var_names[i_var], sep = '.')])
    }

    if (drop_prefix || !is.null(vars_to_drop)) {
      clean_df <- base_df
      # These would result in two columns with the same name
      keeper_selector <- base_df %>%
        names() %>%
        str_detect('\\.code|\\.payoff|\\.label')

      names(clean_df)[keeper_selector] <- base_df[ , keeper_selector] %>%
        names() %>%
        str_replace_all('\\.', '_')

      names(clean_df) <- names(clean_df) %>%
        str_remove('participant\\.|player\\.|session\\.|subsession\\.|group\\.')

      vars_to_keep_selector <- !(names(clean_df) %in% vars_to_drop)

      if (drop_prefix) {
        df_list[[i_subj]] <- dplyr::select(clean_df, which(vars_to_keep_selector))
      } else {
        df_list[[i_subj]] <- dplyr::select(base_df, which(vars_to_keep_selector))
      }

    } else { # Don't drop prefixes or any variables
      df_list[[i_subj]] <- base_df
    }

    # Manually rename participant_code to participant
    names(df_list[[i_subj]]) <- str_replace(names(df_list[[i_subj]]),
      'participant_code', 'participant')

    if (save_path != '') {
      temp <- df_list[[i_subj]]
      filename <- str_c(str_c('participant',
                              temp$participant[1], filename_str, sep = '_'), '.csv')
      write_delim(df_list[[i_subj]], path = str_c(save_path, '//', filename), delim = ';',
                  quote_escape = 'double')
    }

  }
  df_complete <- bind_rows(df_list)
  if (save_path != '') write_delim(df_complete, path = str_c(save_path, '//all_participants_long_',
                                                           filename_str, '.csv'), delim = ';')
  return(df_complete)
}


# A helper function to easily save plots:
save_kevplot <- function(name = 'temp', location = '..//figures//',
                         h = 7, w = 10, text_size = 20, out = 'png', res = 300) {
    if (out == 'pdf') {
        pdf(str_c(location, name, '.pdf'), height = h, width = w, pointsize = text_size)
    } else {
        png(str_c(location, name, '.png'), height = h, width = w,
            units = 'in', res = res, pointsize = text_size, type = 'cairo')
    }

  print(ggplot2::last_plot())
  dev.off()
}